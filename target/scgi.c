
#define _GNU_SOURCE

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>

#include <ev.h>
#include <pthread.h>

#include "scgi.h"

#define WORKERS 1
#define QUEUE_SIZE 128
#define BUFFER_SIZE 4096

pthread_mutex_t work_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t work_request_available = PTHREAD_COND_INITIALIZER;
pthread_cond_t work_slot_available = PTHREAD_COND_INITIALIZER;
struct scgi_io *work_queue[QUEUE_SIZE];
int work_head = 0;
int work_tail = QUEUE_SIZE - 1;
int work_capacity = QUEUE_SIZE;

char *get_field(struct header* header, const char *key) {
  for(int i = 0, offset = 0; offset < header->length; offset += header->offsets[i] + header->offsets[i+1], i += 2) {
    if(strcmp(&header->fields[offset], key) == 0) return &header->fields[offset + header->offsets[i]];
  }
  
  return NULL;
}

int scgi_start(char *address) {
  if(address == NULL) return scgi_start_tcp("127.0.0.1", 5000);
  if(strncmp(address, "unix:", strlen("unix:")) == 0) return scgi_start_uds(&address[strlen("unix:")]);
  
  for(int i = 0;; i++) {
    if(address[i] == '\0') return scgi_start_tcp(address, 5000);
    if(address[i] != ':') continue;
    
    int port;
    sscanf(&address[i+1], "%d", &port);
    address[i] = '\0';
    return scgi_start_tcp(address, port);
  }
}

int scgi_start_tcp(const char* host, const int port) {
  struct sockaddr_in server;
  server.sin_family = AF_INET;
  server.sin_port = htons(port);
  inet_aton(host, &server.sin_addr);

  int sock = socket(AF_INET, SOCK_STREAM, 0);
  if(sock < 0) { perror("socket"); exit(EXIT_FAILURE); }
  if(bind(sock, (struct sockaddr*)&server, sizeof server) < 0) { perror("bind"); exit(EXIT_FAILURE); }
  if(listen(sock, SOMAXCONN) < 0) { perror("listen"); exit(EXIT_FAILURE); }
  
  printf("Listening on %s:%d\n", host, port);
  
  return sock;
}

int scgi_start_uds(const char* file) {
  struct sockaddr_un server;
  server.sun_family = AF_LOCAL;
  strcpy(server.sun_path, file);

  unlink(file);
  
  int sock = socket(AF_LOCAL, SOCK_STREAM, 0);
  if(sock < 0) { perror("socket"); exit(EXIT_FAILURE); }
  if(bind(sock, (struct sockaddr*)&server, sizeof server) < 0) { perror("bind"); exit(EXIT_FAILURE); }
  if(listen(sock, SOMAXCONN) < 0) { perror("listen"); exit(EXIT_FAILURE); }
  
  printf("Listening on Unix domain socket %s\n", file);
  
  return sock;
}

void scgi_accept(struct ev_loop *loop, struct ev_io *watcher, int revents) {
  struct sockaddr_in client;
  socklen_t client_len = sizeof client;
  int fd = accept(watcher->fd, (struct sockaddr*)&client, &client_len);
  if(fd < 0) perror("accept");
  
  struct scgi_io *w_client = malloc(sizeof *w_client);
  w_client->state = 0;
  w_client->buffer = calloc(BUFFER_SIZE, sizeof *w_client->buffer);
  w_client->cursor = 0;
  w_client->header = malloc(sizeof *w_client->header);
  w_client->header->length = 0;
  w_client->header->fields = NULL;
  w_client->body = malloc(sizeof *w_client->body);
  w_client->body->length = 0;
  w_client->body->content = NULL;
  
  ev_io_init(&w_client->io, scgi_read, fd, EV_READ);  
  ev_io_start(loop, &w_client->io);
}

void scgi_read(struct ev_loop *loop, struct ev_io *watcher, int revents) {
  struct scgi_io *client = (struct scgi_io*)watcher;
  int position = 0;
  size_t bytes_read = read(watcher->fd, client->buffer, BUFFER_SIZE);
  
  // Assumption: This condition never occurs when the entire SCGI has been read.
  if(bytes_read == 0) goto scgi_shutdown_label;
  if(bytes_read == -1) { perror("read"); goto scgi_shutdown_label; }
  
  switch(client->state) {
    default:
      goto scgi_shutdown_label;
      
    case 0:
      while(client->buffer[position] != ':') {
	if(position >= bytes_read) return;
	client->header->length = (client->buffer[position++]-'0') + 10 * client->header->length;
      }
    
      client->cursor = 0;
      client->header->fields = calloc(client->header->length, sizeof *client->header->fields);
      position++;
    
      client->state++;
      
    case 1:
      while(client->cursor < client->header->length) {
	if(position >= bytes_read) return;
	client->header->fields[client->cursor++] = client->buffer[position++];
      }
    
      int entries = 0;
      for(int i = 0; i < client->header->length; i++) if(client->header->fields[i] == '\0') entries++;
      client->header->offsets = calloc(entries, sizeof *client->header->offsets);
    
      for(int entry = 0, position = 0, from = 0; position < client->header->length; entry++, position++) {
	for(from = position; client->header->fields[position] != '\0'; position++);
	client->header->offsets[entry] = 1 + position - from;
      }
    
      if(strcmp(client->header->fields, "CONTENT_LENGTH")) goto scgi_shutdown_label;
      sscanf(&client->header->fields[client->header->offsets[0]], "%d", &client->body->length);
      client->body->content = calloc(client->body->length, sizeof *client->body->content);
      
      client->state++;
      
    case 2:
      if(client->buffer[position++] != ',') goto scgi_shutdown_label;
      client->cursor = 0;
      client->state++;

    case 3:
      while(client->cursor < client->body->length) {
	if(position >= bytes_read) return;
	client->body->content[client->cursor++] = client->buffer[position++];
      }
      
      client->state++;
  }
  
  ev_io_stop(loop, watcher);
  
  pthread_mutex_lock(&work_mutex);
  while(work_capacity == 0) pthread_cond_wait(&work_slot_available, &work_mutex);
  work_tail = (work_tail + 1) % QUEUE_SIZE;
  work_queue[work_tail] = client;
  work_capacity--;
  pthread_cond_signal(&work_request_available);
  pthread_mutex_unlock(&work_mutex);

  return;
  
scgi_shutdown_label:
  ev_io_stop(loop, watcher);
  close(watcher->fd);
  free(client->buffer);
  free(client->header->fields);
  free(client->header);
  free(client->body->content);
  free(client->body);
  free(client);
}

void* scgi_dispatch(void *args) {
  while(1) {
    pthread_mutex_lock(&work_mutex);
    while(work_capacity == QUEUE_SIZE) pthread_cond_wait(&work_request_available, &work_mutex);
    struct scgi_io *client = work_queue[work_head];
    work_head = (work_head + 1) % QUEUE_SIZE;
    work_capacity++;
    pthread_cond_signal(&work_slot_available);
    pthread_mutex_unlock(&work_mutex);
    
    if(strcmp(get_field(client->header, "SCGI"), "1") == 0) {
      const char *call = match(get_field(client->header, "REQUEST_METHOD"), get_field(client->header, "DOCUMENT_URI"));
      char *message = handle_request(client->header, client->body, call);
      send(client->io.fd, message, strlen(message), 0);
      free(message);
    }
    
    close(client->io.fd);
    
    free(client->buffer);
    free(client->header->offsets);
    free(client->header->fields);
    free(client->header);
    free(client->body->content);
    free(client->body);
    free(client);
  }
}

int main(int argc, const char *argv[]) {
  char *address = NULL;
  if(argc > 1) {
    address = calloc(strlen(argv[1]) + 1, sizeof *address);
    strcpy(address, argv[1]);
  }
  
  int socket = scgi_start(address);
  
  pthread_t threads[WORKERS];
  for(long tid = 0; tid < WORKERS; tid++) {
    if(pthread_create(&threads[tid], NULL, scgi_dispatch, (void*)tid)) {
      perror("pthread_create");
      exit(EXIT_FAILURE);
    }
  }
  
  struct ev_io w_accept;
  ev_io_init(&w_accept, scgi_accept, socket, EV_READ);
  struct ev_loop *loop = ev_default_loop(0);
  ev_io_start(loop, &w_accept);
  ev_loop(loop, 0);
  
  pthread_exit(NULL);
}

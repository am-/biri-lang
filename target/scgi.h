#ifndef _SCGI_H
#define _SCGI_H

#include <ev.h>

struct header {
  int length;
  int *offsets;
  char *fields;
};

char *get_field(struct header*, const char*);

struct body {
  int length;
  char *content;
};

struct scgi_io {
  ev_io io;
  int state;
  char *buffer;
  int cursor;
  struct header *header;
  struct body *body;
};

extern const char* match(const char *method, const char *uri);
extern char* handle_request(struct header*, struct body*, const char *call);

int scgi_start(char *address);
int scgi_start_tcp(const char* host, const int port);
int scgi_start_uds(const char* file);

void scgi_accept(struct ev_loop *loop, struct ev_io *watcher, int revents);
void scgi_read(struct ev_loop *loop, struct ev_io *watcher, int revents);
void* scgi_dispatch(void*);

#endif

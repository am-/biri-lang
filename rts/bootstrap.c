
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "data.h"
#include "scgi.h"

char* handle_request(struct header *header, struct body *body, const char *call) {
  char *response = calloc(1024, sizeof *response);
  
  if(call == NULL) {
    sprintf(response, "HTTP/1.1 404 Not found\r\nContent-Type: text/html\r\nContent-Length: 3\r\n\r\n404");
    return response;
  }
  
  sprintf(response, "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: %zu\r\n\r\n%s", strlen(call), call);
  return response;
}

#include <unistd.h>

#include <map_manager.h>
#include <map_internal.h>

int map_read_buffer(int file_descriptor,void *buffer,int nbytes)
  { unsigned char *buffer_ptr;
    int read_count;
    int read_status;

    buffer_ptr=buffer;
    read_count=0;

    while(read_count!=nbytes)
      { read_status=read(file_descriptor,buffer_ptr,nbytes-read_count);
        if (read_status<=0)
          return read_status;
        read_count+=read_status;
        buffer_ptr+=read_status;
      }
    return nbytes;
  }

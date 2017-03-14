#include <unistd.h>

#include <map_manager.h>
#include <map_internal.h>

int map_write_buffer(int file_descriptor,const void *buffer,int nbytes)
  { char *buffer_ptr;
    int write_count;
    int write_status;

    buffer_ptr=(char*)buffer;
    write_count=0;

    while(write_count!=nbytes)
      { write_status=write(file_descriptor,buffer_ptr,nbytes-write_count);
        if (write_status<=0)
          return write_status;
        write_count+=write_status;
        buffer_ptr+=write_status;
      }
    return nbytes;
  }

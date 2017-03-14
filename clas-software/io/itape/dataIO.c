/*
 * dataIO.c
 *
*/


#include <stdio.h>
#include <stdlib.h>
#ifdef Darwin
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif

#include <string.h>
#include <errno.h>

#include <itape.h>


#ifdef USE_ESEND
#include <alarm.h>
#endif

#define WSIZE 4      /* word size on tape (length word, CRC32 word, etc... */

int data_write(int fd,const void* buffer_)
{
  int ret,size;
  void* buffer = (void*)buffer_;

  size = *(int*)buffer;
  if (size % WSIZE)
    {                                /* force size to be multiple of 4 (WSIZE) */
      size = ((size/WSIZE)+1)*WSIZE;
/*
fprintf(stderr,"Change size: %d -> %d\n", *(unsigned long*)buffer,size);
*/
      *(unsigned long*)buffer = size;
    }

  data_addCRC(buffer);

  ret = tape_write(fd,buffer,size + WSIZE);
  if (ret<0)
    {
      fprintf(stderr,"dataIO.write: Data write failure: tape_write() failed\n");
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.write: Data write failure","tape_write() failed\n");
#endif
      return DATAIO_ERROR;
    }

  return DATAIO_OK;
}

static int data_wrongEndian = 0;

static uint32 data_swapEndianInt(uint32 w)
{
  return ((w&0xff)<<24)|((w&0xff00)<<8)|((w&0xff0000)>>8)|((w&0xff000000)>>24);
}

static int makeLength(uint32 tapeLength,uint32* nativeLength,uint32 bufsize)
{
  uint32 lsize,tsize;

  *nativeLength = 0;

  tsize = tapeLength;
  lsize = tsize;
  if (data_wrongEndian)
    {
      lsize = data_swapEndianInt(tsize);
    }

  /* fprintf(stderr,"Lsize: 0x%x 0x%x 0x%x\n",tsize,lsize,data_swapEndianInt(tsize)); */

  if (lsize>bufsize)
    {
      /* bogus size word: maybe we are the wrong-endian? */

      lsize = data_swapEndianInt(tsize);

      if (lsize < bufsize)
	{
	  /* now it's better. Assume swapped-endian and go ahead! */
	  
	  data_wrongEndian = 1;

	  fprintf(stderr,"dataIO.read: NOTICE: Itape is wrong-endian, converting on-the-fly.\n");
	}
      else if (tsize < bufsize)
	{
	  /* Hm... we don't need to do the conversion after all. */
	  
	  data_wrongEndian = 0;

	  /* fprintf(stderr,"dataIO.read: NOTICE: Itape is wrong-endian, converting on-the-fly.\n"); */
	}
      else
	{
	  fprintf(stderr,"dataIO.read: Data Format Error: Record length (%u): is bigger than buffer (%d)\n",lsize,bufsize);
#ifdef USE_ESEND
	  esend(ALARM_ERROR,"dataIO.read: Record length is bigger than buffer","(%d) vs. (%d)\n",lsize,bufsize);
#endif
	  return DATAIO_ERROR;
	}
    }

  if (lsize < WSIZE)
    {
      fprintf(stderr,"dataIO.read: Data Format Error: Invalid Record length (%u): is less than %d\n",lsize,WSIZE);
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read: Record length too small","%d vs. %d\n",lsize,WSIZE);
#endif
      return DATAIO_ERROR;
    }

  *nativeLength = lsize;

  return 0;
}
  
int data_read(int fd,void* buffer,int bufsize)
{
  int ret;
  char *buf = buffer;
  uint32 lsize; /* size in local-endian format */
  uint32 tsize; /* size on tape (maybe endian-swapped) */

  ret = tape_read(fd,&tsize,WSIZE);

  if (ret==TAPEIO_EOT)
    return DATAIO_EOT;

  if (ret<0)
    {
      fprintf(stderr,"dataIO.read: Length word read failure: tape_read() failed\n");
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read: Length word read failure","tape_read() failed\n");
#endif
      return DATAIO_ERROR;
    }
/*
fprintf(stderr,"dataIO.read: %d, 0x%x, %d, size: %d\n",fd,buffer,bufsize,size);
*/
  if (ret==0) return DATAIO_EOF;

  ret = makeLength(tsize,&lsize,bufsize);
  if (ret != 0)
    return ret;

  *((uint32*)buf) = tsize;  /* use the tape size- it will be endian-swapped later */

  ret = tape_read(fd,buf + WSIZE,lsize - WSIZE + WSIZE); /* -WSIZE is the length word, +WSIZE is the CRC32 word */
  if (ret<0)
    {
      fprintf(stderr,"dataIO.read: Data read failure: tape_read() failed\n");
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read: Data read failure","tape_read() failed\n");
#endif
      return DATAIO_ERROR;
    }

  ret = DATAIO_OK;

  if (data_checkCRC1(buffer,lsize))  /* CRC32 is endian-neutral */
    {
      fprintf(stderr,"dataIO.read: Warning: Data failed the CRC check\n");
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read: Data failed the CRC check","");
#endif
      ret = DATAIO_BADCRC;
    }

  if (data_wrongEndian)
    endian_convertItape(buffer,lsize);

  return ret;
}

#define MAXBUF (1024*1024)  /* 1024 Kbytes */

int data_read_alloc(int fd,void **buffer)
{
  int ret,rd;
  char *buf = NULL;
  uint32 tsize;
  uint32 lsize;

  if (buffer==NULL)
    {
      fprintf(stderr,"dataIO.read_alloc: Error: NULL pointer\n");
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read_alloc: NULL buffer pointer","");
#endif
      return DATAIO_ERROR;
    }

  *buffer = NULL;
  rd = 0;

  ret = tape_read(fd,&tsize,WSIZE);
  if (ret<0)
    {
      fprintf(stderr,"dataIO.read_alloc: Length word read failure: tape_read() failed\n");
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read_alloc: Length word read failure","tape_read() failed\n");
#endif
      return DATAIO_ERROR;
    }

  if (ret==0) return DATAIO_EOF;

  rd += ret;

  ret = makeLength(tsize,&lsize,MAXBUF);
  if (ret != 0)
    return ret;

  *buffer = malloc(lsize + WSIZE);
  buf = *buffer;

  if (!buf)
    {
      fprintf(stderr,"dataIO.read_alloc: Error: Cannot allocated %d bytes for data buffer, errno: %d (%s)\n",lsize + WSIZE,errno,strerror(errno));
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read_alloc: Cannot allocated memory","%d bytes, errno: %d (%s)",lsize + WSIZE,errno,strerror(errno));
#endif
      return DATAIO_ERROR;
    }

  *((uint32*)buf) = tsize;

  ret = tape_read(fd,buf + WSIZE,lsize - WSIZE + WSIZE);  /* -WSIZE is length word, +WSIZE is CRC32 word */
  if (ret<0)
    {
      fprintf(stderr,"dataIO.read_alloc: Data read failure: tape_read() failed\n");
#ifdef USE_ESEND
      esend(ALARM_ERROR,"dataIO.read_alloc: Data read failure","tape_read() failed\n");
#endif
      return DATAIO_ERROR;
    }

  rd += ret;

  ret = DATAIO_OK;

  if (data_checkCRC1(buf,lsize))
    {
      fprintf(stderr,"dataIO.read_alloc: Warning: Data failed the CRC check\n");
#ifdef USE_ESEND
      esend(ALARM_WARNING,"dataIO.read_alloc: Data failed the CRC check","");
#endif
      ret = DATAIO_BADCRC;
    }

  if (data_wrongEndian)
    endian_convertItape(buf,lsize);

  return ret;
}

int data_flush(int fd)
{
  return tape_flush(fd);
}

int data_writeFM(int fd)
{
  tape_flush(fd);
  return tape_writeFM(fd);
}

int data_findFM(int fd,int count)
{
  tape_flush(fd);
  return tape_findFM(fd,count);
}

/* end file */

/*
 * Name: tapeIO.c
 *
 * Description : multi-modal read/write library
 *               Supported modes are:
 *                     - "direct": for disk-type devices
 *                     - "stream": for pipes and sockets
 *                     - "fixed-block tape": tape drives in fixed-block mode
 *                     - "variable-block tape": in variable-block mode
 *                     - "buffered-pipe": pipes, with memory buffer
 *
 * Written by Konst Olchanski 11 Apr 1992, 6 Sept 1992,
 *                            26 Apr 1993
 *                            18 Dec 1998: updated
 *
 * $Id: tapeIO.c,v 1.1 1999/04/08 17:43:17 weygand Exp $
 *
*/



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <itape.h>

#define MAX_TIOFILES 50

#define MODE_FREE  0         /* free slot        */
#define MODE_RD    1         /* fixed block read     */
#define MODE_WR    2         /* fixed block write    */
#define MODE_TRANSPARENT 3   /* transparent mode */
#define MODE_VRD   4         /* variable block read  */
#define MODE_VWR   5         /* variable block write */
#define MODE_PRD   6         /* pipe buffered variable blocksize read  */

typedef struct 
{
  int fd;
  int ttype;
  int mode;
  int debug;
  int blockSize;  /* size of the internal buffer */
  int thisBlock;  /* bytes in the current block */
  int inBuffer;   /* unread bytes in the internal buffer */
  int rptr;       /* read pointer */
  int maxnblocks;
  int isTruncated;
  char*buffer;
} tape_info_t;

static tape_info_t tape[MAX_TIOFILES];

static int nFd = 0;

static int getInfo(int fd)
{
  int i;
  for (i=0; i<nFd; i++)
    if (tape[i].mode != MODE_FREE)
      if (tape[i].fd == fd) return i;

/* new tape */

  return -1;
}

int tape_enablePipeIO = 0;

static int setupTape(int fd,int mode)
{
  int i,ttype;

  for (i=0; i<nFd; i++)
    if (tape[i].mode == MODE_FREE) break;

  if (i>=nFd)
    {
      if (nFd>=MAX_TIOFILES)
	{
	  fprintf(stderr,"tapeIO.setupTape: Error: Internal table overflow. Recompile with larger MAX_TIOFILES (current=%d)\n",MAX_TIOFILES);

	  errno = 0;
	  return -1;
	}
      i = nFd;
      nFd++;
    }

  ttype = tape_devType(fd);
  if (ttype < 0)
    {
      fprintf(stderr,"tapeIO.setupTape: Notice: fd %d: Cannot determine device type, assuming STREAM\n",fd);
      ttype = TAPEIO_STREAM;
    }

  if (ttype == TAPEIO_PIPE)
    {
      const char *env = getenv("TAPEIO_ENABLEPIPEIO");

      if ((tape_enablePipeIO==0)&&(env==NULL))
	ttype = TAPEIO_STREAM;
    }

  switch (ttype)
    {
    default:
      fprintf(stderr,"tapeIO.setupTape: Notice: fd %d: Bug: Unknown device type %d, assuming STREAM\n",fd,ttype);
      ttype = TAPEIO_STREAM;

    case TAPEIO_STREAM:  /* !!!!!!!!!!!!!!! careful there is no "break" above !!!!!!!!!!!!!!!! */ 
                         /* !!!!!!!!!!!!!!! "default" falls through here      !!!!!!!!!!!!!!!! */

      tape[i].mode = MODE_TRANSPARENT;
      tape[i].blockSize = 0;
      tape[i].maxnblocks = 0;
      break;

    case TAPEIO_PIPE:

      switch (mode)
	{
	case MODE_WR:
	  tape[i].mode = MODE_TRANSPARENT;
	  tape[i].blockSize = 0;
	  tape[i].maxnblocks = 0;
	  break;

	case MODE_RD:
	  {
	    int blkSize = 0;
	    const char *env = getenv("TAPEIO_ENABLEPIPEIO");
	    if (env!=NULL)
	      blkSize = strtoul(env,NULL,0);
	    if (blkSize < 1024)
	      blkSize = 100*1024;
#ifdef UNDEF
	    /* setup the non-blocking reads */
#ifdef FNONBLK
	    fcntl(fd,F_SETFL,FNONBLK);
#endif
#endif
	    tape[i].mode = MODE_PRD; 
	    tape[i].blockSize = blkSize;
	    tape[i].maxnblocks = 1;
	    fprintf(stderr,"tapeIO.setupTape: fd %d: Buffered pipe with buffer size %d bytes\n",
		    fd,tape[i].blockSize);
	  }
	  break;
	}
      break;

    case TAPEIO_FIXED:
      {
	char *sz = getenv("TAPEIO_FBLOCKSIZE");
	if (sz == NULL)
	  {
	    fprintf(stderr,"tapeIO.setupTape: Notice: The TAPEIO_FBLOCKSIZE environment variable is not set. Exit(1)...\n");
	    exit(1);
	  }

	tape[i].blockSize = (int)strtoul(sz,NULL,0);

	if (tape[i].blockSize == 0)
	  {
	    fprintf(stderr,"tapeIO.setupTape: Error: The TAPEIO_FBLOCKSIZE envirnment variable (equal to (%s)) is zero or not a correct block size. Exit(1)...\n",sz);
	    exit(1);
	  }
	
	sz = getenv("TAPEIO_MAXFBLOCK");
	if (sz == NULL)
	  {
	    fprintf(stderr,"tapeIO.setupTape: Error: The TAPEIO_MAXFBLOCK environment variable is not set. Exit(1)...\n");
	    exit(1);
	  }

	tape[i].maxnblocks = (int)strtoul(sz,NULL,0) / tape[i].blockSize;

	if (tape[i].blockSize == 0)
	  {
	    fprintf(stderr,"tapeIO.setupTape: Error: The TAPEIO_MAXFBLOCK envirnment variable (equal to (%s)) is zero or not a correct block size. Exit(1)...\n",sz);
	    exit(1);
	  }
	
	tape[i].mode = mode;

	fprintf(stderr,"tapeIO.setupTape: fd %d: Tape with block size %d bytes, max transfer size is %d blocks\n",
		fd,tape[i].blockSize,tape[i].maxnblocks);
	
      }
      break;

    case TAPEIO_VARIABLE:
      {
	char *sz = getenv("TAPEIO_MAXVBLOCK");
	if (sz == NULL)
	  {
#ifdef linux
	    sz = "65536";
#else
	    sz = "102400";
#endif
	    fprintf(stderr,"tapeIO.setupTape: Notice: TAPEIO_MAXVBLOCK is not set. Using default: %s bytes\n",sz);
	  }

	tape[i].blockSize = (int)strtoul(sz,NULL,0);
	tape[i].maxnblocks = 1;

	if (mode==MODE_RD)
	  {
	    tape[i].blockSize *= 2;
	  }

	switch (mode)
	  {
	  case MODE_WR: tape[i].mode = MODE_VWR; break;
	  case MODE_RD: tape[i].mode = MODE_VRD; break;
	  }

	fprintf(stderr,"tapeIO.setupTape: fd %d: Tape with variable block size, max tape block size is %d bytes\n",
		fd,tape[i].blockSize);
      }

      break;
    }

  tape[i].fd = fd;
  tape[i].ttype = ttype;

  if (getenv("TAPEIO_DEBUG"))
    tape[i].debug = strtoul(getenv("TAPEIO_DEBUG"),NULL,0);

  tape[i].inBuffer = 0;
  tape[i].rptr = 0;
  tape[i].isTruncated = 0;

  if (tape[i].blockSize > 0)
    {
      tape[i].buffer = malloc(tape[i].blockSize);

      if (!tape[i].buffer)
	{
	  fprintf(stderr,"tapeIO.setupTape: Error: fd %d: cannot allocate %d bytes for internal buffer, errno: %d (%s)\n",fd,tape[i].blockSize,errno,strerror(errno));
	  return -1;
	}
    }
  else tape[i].buffer = NULL;

  return i;
}

int tape_vflush(int p)
{
  int err;

  if (tape[p].inBuffer == 0)
    return 0;

  if (tape[p].inBuffer & 1) /* for some wierd reason the blocksize should be even */
    {
      tape[p].buffer[tape[p].inBuffer] = 0;   /* pad with a zero */
      tape[p].inBuffer ++;
    }

  err = write(tape[p].fd,tape[p].buffer,tape[p].inBuffer);
  if (err<0)
    {
      fprintf(stderr,"tapeIO.vwrite: Error: write() returned %d, errno: %d(%s)\n",err,errno,strerror(errno));
    }

  tape[p].inBuffer = 0;
  
  return err;
}

int tape_vwrite(int p,const char* buffer,int len)
{
  int err = 0;
  int written = 0;

  if (tape[p].inBuffer + len >= tape[p].blockSize)
    {
      err = tape_vflush(p);

      if (err < 0)
	return err;
    }
  
  if (tape[p].inBuffer + len >= tape[p].blockSize)
    {
      /* this event is too big to fit in one block */

      /* make damn sure the buffer was flushed */
      err = tape_vflush(p);
      if (err < 0)
	return err;

      /* keep a number of "full" blocks */
      while (len >= tape[p].blockSize)
	{
	  int wr = 0;
	  int toWrite = tape[p].blockSize;
	  wr = (int)write(tape[p].fd,buffer,toWrite);
	  if (wr<0)
	    {
	      fprintf(stderr,"tapeIO.vwrite: Error: write() returned %d, errno: %d (%s)\n",
		      wr,errno,strerror(errno));
	      return -1;
	    }

	  buffer += toWrite;
	  len -= toWrite;
	  written += toWrite;
	}
    }

  if (len > 0)
    {
      memcpy(&tape[p].buffer[tape[p].inBuffer],buffer,len);
      tape[p].inBuffer += len;
      written += len;
    }

  return written;
}

int tape_vreadBlock(int p)
{
  int retry;

  tape[p].inBuffer = 0;
  tape[p].rptr = 0;

  do /* retry read() requests */
    {
      int rd;
      retry = 0;
      errno = 0;

      tape[p].isTruncated = 0;

      rd = read(tape[p].fd,tape[p].buffer,tape[p].blockSize);
      if (rd > 0)
	{
	  tape[p].thisBlock = rd;
	  tape[p].inBuffer = rd;

	  if (tape[p].debug)
	    fprintf(stderr,"tapeIO.vread: fd=%d, requested=%d, read=%d\n",tape[p].fd,tape[p].blockSize,rd);

	  if (rd == tape[p].blockSize)
	    {
	      tape[p].isTruncated = 1;
	      fprintf(stderr,"tapeIO.vread: Warning: Tape block was truncated to %d bytes, increase TAPEIO_MAXVBLOCK and try again.\n",tape[p].blockSize);
	    }

	  retry = 0;
	}
      else
	{
	  switch (errno)
	    {
	    case 0:
	      switch (tape[p].ttype)
		{
		default:
		case TAPEIO_STREAM:
		  return TAPEIO_EOT;
		case TAPEIO_FIXED:
		case TAPEIO_VARIABLE:
		  return 0;            /* file mark   */
		}
	      return 0;

	    case ENOSPC: return TAPEIO_EOT;   /* end of tape */
	    case ENOMEM:
	      /*
	       * tape block longer than our buffer,
	       * increase the buffer and retry does not work because the retry
	       * will give us the *next* block. This behaviour is not documented
	       * in the man pages (man read, mtio and tps), therefore the
	       * "move the tape back by one block and try to read again"
	       * would be unreliable as well.
	       *
	       * Easiest is to return an error and have somebody else worry about recovering.
	       *
	       * CO 30-Oct-1994.
	       */

	      fprintf(stderr,"tapeIO.vread: Error: The next tape block cannot be read because it is longer than %d bytes, increase TAPEIO_MAXVBLOCK and try again.\n",tape[p].blockSize);
	      return -1;

#if defined(linux)&&defined(EOVERFLOW)
	    case EOVERFLOW:
	      fprintf(stderr,"tapeIO.vread: Error: TAPEIO_MAXVBLOCK=%d is too large for the Linux tape device driver. Set it to 65535 and hope you get no \"block was truncated\" errors. If you do, complain to linux-is-crap@dev.null\n",tape[p].blockSize);
	      return -1;
#endif
	      
	    default:
	      fprintf(stderr,"tapeIO.vread: Error: read() returned %d, errno: %d (%s)\n",rd,errno,strerror(errno));
	      return -1;
	    }
	}
    } while (retry);

/*printf("vread: %d, %d\n",tape[p].blockSize,tape[p].inBuffer);*/
  
  return tape[p].inBuffer;
}

int tape_vread1(int p,char* buffer,int len)
{
  if (tape[p].inBuffer < 2) /* 0 or 1 -
			     * 0 means buffer exhausted,
			     * 1 means the buffer was padded to even bytes */
    {
      int ret = tape_vreadBlock(p);
      if (ret <= 0)
	return ret;
    }
  
  if (len > tape[p].inBuffer)
    {
      if (tape[p].isTruncated)
	{
	  fprintf(stderr,"tapeIO.vread: Error: Attempt to read %d bytes from truncated buffer, buffer contains only %d bytes, blocksize is %d, this block has %d bytes\n",len,tape[p].inBuffer,tape[p].blockSize,tape[p].thisBlock);
	  return -1;
	}

      /* ok. we are reading an event spanning multiple blocks... */
      
      len = tape[p].inBuffer;
    }
  
  memcpy(buffer,&tape[p].buffer[tape[p].rptr],len);
  tape[p].rptr += len;
  tape[p].inBuffer -= len;
  
  return len;
}

int tape_vread(int p,char* buffer,int len)
{
  int toRead = len;
  int gotBytes = 0;
  int count = 0;

  while (toRead > 0)
    {
      int rd = tape_vread1(p,buffer,toRead);

      if (rd < 0)
	return rd; /* report an error */

      if ((gotBytes == 0)&&(rd == 0))
	return 0; /* normal filemark */

      if (rd == 0)
	{
	  fprintf(stderr,"tapeIO.vread: Error: Unexpected EOF while reading %d bytes. Got %d bytes so far, all flushed.\n",len,gotBytes);
	  return -1; /* unexpected filemark */
	}

      toRead -= rd;
      buffer += rd;
      gotBytes += rd;
      count ++;
    }

#ifdef UNDEF
  if (count != 1)
    {
      fprintf(stderr,"tapeIO.vread: Notice: Multi-block read: %d bytes, %d iterations\n",
	      gotBytes,count);
    }
#endif

  return gotBytes;
}

int tape_fflush(int p)
{
  int i = 0;
  int err = 0;
  /* pad the last block with zeros */
  
  for (i=tape[p].inBuffer; i<tape[p].blockSize; i++)
    tape[p].buffer[i] = 0;
  
  /* write the last block onto tape */
  
  err = (int)write(tape[p].fd,tape[p].buffer,tape[p].blockSize);
  if (err<0)
    {
      fprintf(stderr,"tapeIO.fflush: Error: write() returned %d, errno: %d (%s) (buffer flushed anyway)\n",err,errno,strerror(errno));
    }

  return err;
}

int tape_flush(int fd)
{
  int err = 0;
  int p;

  p = getInfo(fd);
  if (p<0) return 0;

  switch (tape[p].mode)
    {
    default:
    case MODE_RD:
      return 0;

    case MODE_WR:
      err = tape_fflush(p);
      break;

    case MODE_PRD:
      tape[p].inBuffer = 0;
      break;

    case MODE_VRD:
      fprintf(stderr,"tapeIO.flush: Notice: The input buffer was flushed: blocksize: %d, %d bytes in the current block, %d unread bytes were flushed\n",tape[p].blockSize,tape[p].thisBlock,tape[p].inBuffer);
      tape[p].inBuffer = 0;
      break;

    case MODE_VWR:
      err = tape_vflush(p);
      break;
    }

  free(tape[p].buffer);
  tape[p].buffer = NULL;
  tape[p].mode = MODE_FREE;
  return err;
}


int tape_fwrite_fillBuf(int p,const void *ptr,int len)
{
  /* first fill the buffer */
	  
  int fill = tape[p].blockSize - tape[p].inBuffer;
	    
  if (len<fill) fill = len;
	    
  memcpy(&tape[p].buffer[tape[p].inBuffer],ptr,fill);
	    
  tape[p].inBuffer += fill;
	  
  /* now write out the buffer */
	  
  if (tape[p].inBuffer == tape[p].blockSize)
    {
      ssize_t err = write(tape[p].fd,tape[p].buffer,tape[p].inBuffer);
      if (err<0)
	{
	  fprintf(stderr,"tapeIO.fwrite: Error: write1 error: write() returned %d, errno: %d (%s)\n",err,errno,strerror(errno));
	  return -1;
	}
      tape[p].inBuffer = 0;
    }
  else
    {
      if (len==0) return fill;
      else 
	{
	  fprintf(stderr,"tapeIO.fwrite: Error: Bug: This branch should never be executed. See source code (w1)\n");
	  /* explanation: buffer is not full, and yet len is not 0 */
	  errno = 0;
	  return -1;
	}
    }

  return fill;
}

int tape_fwrite(int p,const void* buffer,int len)
{
  int err = 0;
  int wr = 0;
  const char *ptr = buffer;

  if (tape[p].inBuffer>0) /* if buffer is empty, do not bother filling it */
    {
      err = tape_fwrite_fillBuf(p,ptr,len);
      if (err > 0)
	{
	  len -= err;
	  ptr += err;
	  wr += err;
	}
    }
  
  /* now write out data (if have enough of it) */
  
  {
    while (len>=tape[p].blockSize)
      {
	int count,ret;
	int n = len/tape[p].blockSize;  /* number of complete blocks to write */
	
	if (n>tape[p].maxnblocks) n = tape[p].maxnblocks;
	
	count = n*tape[p].blockSize;
	
	ret = (int)write(tape[p].fd,ptr,count);
	if (ret<0)
	  {
	    fprintf(stderr,"tapeIO.fwrite: Error: (write2) write() returned %d, errno: %d (%s)\n",ret,errno,strerror(errno));
	    return -1;
	  }
	ptr += ret;
	len -= ret;
	wr += ret;
      }
  }
  
  /* put the rest into the buffer for next write or tape_flush() */
  
  {
    if (len>=tape[p].blockSize)
      {
	fprintf(stderr,"tapeIO.fwrite: Error: Bug: This branch should never be executed. See source code (w2)\n");
	/* explanation: I was supposed to write all the full blocks I have */
	errno = 0;
	return -1;
      }
    
    if (len>0)
      {
	memcpy(tape[p].buffer,ptr,len);
	
	tape[p].inBuffer = len;
	ptr += len;
	len -= len;
	wr += len;
      }
  }
  
  return wr;
}

int tape_write(int fd,const void*buffer,int len)
{
  int p;

  if ((len & 3) != 0)
    {
      fprintf(stderr,"tapeIO.write: Error: Write call with invalid length: %d should be divisible by 4\n",len);
      return (-1);
    }

  p = getInfo(fd);
  if (p<0) 
    {
      p = setupTape(fd,MODE_WR);
      if (p<0) return -1;
    }

  switch (tape[p].mode)
    {
    default:
      fprintf(stderr,"tapeIO.write: Error: Write call with wrong file mode\n");
      return -1;

    case MODE_TRANSPARENT:
      {
	int wr = 0;
	wr = (int)write(fd,buffer,len);
	if (wr<0)
	  {
	    fprintf(stderr,"tapeIO.twrite: Error: (direct) write() returned %d, errno: %d (%s)\n",
		    wr,errno,strerror(errno));
	  }
	return wr;
      }
    case MODE_VWR:
      return tape_vwrite(p,buffer,len);
    case MODE_WR:
      return tape_fwrite(p,buffer,len);
    }
}

static int tape_pread(int fd,int p,void *buffer,int len)
{
  int ret;
  int rd;
  char *ptr = buffer;

  rd = 0;

  while (len>0)
    {
      int toCopy;

      if (tape[p].inBuffer == 0)
	{
	  tape[p].rptr = 0;

	  ret = read(fd,tape[p].buffer,tape[p].blockSize);
	  
	  if (ret > 0)
	    tape[p].inBuffer = ret;
	  else if (ret == 0)
	    return 0;
	  else
	    {
	      if (errno == EAGAIN)
		continue;

	      fprintf(stderr,"tapeIO.pread: Error: read() returned %d, errno: %d (%s)",ret,errno,strerror(errno));
	      return (-1);
	    }
	}

      if (tape[p].inBuffer == 0)
	{
	  fprintf(stderr,"tapeIO.pread: Error: Bug: We should never get here. Aborting...\n");
	  abort();
	}

      toCopy = len;
	
      if (toCopy > tape[p].inBuffer)
	toCopy = tape[p].inBuffer;
      
      memcpy(ptr,&tape[p].buffer[tape[p].rptr],toCopy);
      
      len -= toCopy;
      ptr += toCopy;
      tape[p].inBuffer -= toCopy;
      tape[p].rptr += toCopy;
      rd += toCopy;
    }

  return rd;
}


static int read1(int fd,void*ptr,int sz)
{
  char*p=ptr;
  int rd,ret;
  rd = 0;
  while (sz>0)
    {
      ret = (int)read(fd,p,sz);
      if (ret<0) return ret;
      if (ret==0) return rd;
      rd += ret;
      sz -= ret;
      p += ret;
    }
  return rd;
}

int tape_fread(int p,char* buffer,int len)
{
  int rd = 0;
  char *ptr = buffer;

  if (tape[p].inBuffer>0) /* if buffer not empty, read from it */
    {
      
      /* first read from buffer */
      
      {
	int fill = tape[p].inBuffer;
	
	if (len<fill) fill = len;
	
	memcpy(ptr,&tape[p].buffer[tape[p].rptr],fill);
	
	len -= fill;
	ptr += fill;
	tape[p].inBuffer -= fill;
	tape[p].rptr += fill;
	rd += fill;
      }
      
    }

  /* sanity check */
      
  if (len>0)
    if (tape[p].inBuffer!=0)
      {
	fprintf(stderr,"tapeIO.fread: Error: Bug: This branch should never be executed. See source code (r1)\n");
	/* explanation: if len is non zero then all of the buffer should be read out */
	errno = 0;
	return -1;
      }

  /* now read data from input stream */
  
  {
    if (len>=tape[p].blockSize)
      {
	int n = len/tape[p].blockSize;
	int count = n*tape[p].blockSize;
	int ret = 0;
	
	ret = (int)read(tape[p].fd,ptr,count);
	if (ret<0)
	  {
	    fprintf(stderr,"tapeIO.fread: Error: read() (@data) returned %d, errno: %d (%s)",ret,errno,strerror(errno));
	    return -1;
	  }
	
	if (ret==0)
	  return rd;  /* eof detected */
	
	if (ret!=count)
	  {
	    fprintf(stderr,"tapeIO.fread: Error: Short read() to @data, read(%d) returned %d, errno: %d (%s)",count,ret,errno,strerror(errno));
	  }
	
	ptr += ret;
	len -= ret;
	rd += ret;
      }
  }

  /* if have more to read but it is less than a full block,
   * read it into buffer */
  
  {
    if (len>=tape[p].blockSize)
      {
	fprintf(stderr,"tapeIO.fread: Error: Bug: This branch should never be executed. See source code (r2)\n");
	/* explanation: I was supposed to read all the full blocks I have */
	errno = 0;
	return -1;
      }
    
    if (len>0)
      {
	int ret = 0;
	
	ret = (int)read(tape[p].fd,tape[p].buffer,tape[p].blockSize);
	if (ret<0)
	  {
	    fprintf(stderr,"tapeIO.fread: Error: read(%d) (@buffer) returned %d, errno: %d (%s)\n",tape[p].blockSize,ret,errno,strerror(errno));
	    return -1;
	  }
	
	tape[p].inBuffer = ret;
	tape[p].rptr = 0;
	
	if (ret==0)
	  return rd; /* end of file */
	
	if (len>tape[p].inBuffer)
	  {
	    fprintf(stderr,"tapeIO.fread: Error: Bug: This branch should never be executed. See source code (r3)\n");
	    /* explanation: all the full blocks are expected to be read before */
	    errno = 0;
	    return -1;
	  }
	
	memcpy(ptr,&tape[p].buffer[tape[p].rptr],len);
	
	tape[p].inBuffer -= len;
	tape[p].rptr += len;
	ptr += len;
	rd += len;
	len -= len;
	
	if (len!=0)
	  {
	    fprintf(stderr,"tapeIO.fread: Error: Short read()? to buffer\n");
	  }
      }
  }
      
  return rd;
}

int tape_read(int fd,void*buffer,int len)
{
  int p = getInfo(fd);
  if (p<0) 
    {
      p = setupTape(fd,MODE_RD);
      if (p<0) return -1;
    }
/*
fprintf(stderr,"tapeIO.read: %d, 0x%x, %d\n",fd,buffer,len);
*/
  switch (tape[p].mode)
    {
    default:
      fprintf(stderr,"tapeIO.read: Error: Read call with wrong file mode\n");
      return -1;

    case MODE_TRANSPARENT:
      {
	int rd = read1(fd,buffer,len);
	if (rd<0)
	  {
	    fprintf(stderr,"tapeIO.tread: Error: (direct) read() returned %d, errno: %d (%s)\n",rd,errno,strerror(errno));
	  }
	
	if (rd==0)
	  return TAPEIO_EOT;

	return rd;
      }

    case MODE_PRD:
      {
	int rd = tape_pread(fd,p,buffer,len);

	if (rd==0)
	  return TAPEIO_EOT;
	
	return rd;
      }

    case MODE_VRD:
      return tape_vread(p,buffer,len);

    case MODE_RD:
      return tape_fread(p,buffer,len);

    } /* end switch */
}

/* end file */

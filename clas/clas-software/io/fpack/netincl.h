#ifdef AIX
#ifndef _NETINCL_
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifndef SEEK_CUR
/* on some machines (Alliant, MIPS,...) these macros are not
   defined in stdio.h   */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
/* We keep file pointers in an array in C, rather than passing as ints
 * back to Fortran.  This is implemented for 64-bit Alphas, but it'll
 * work on any machine.
 */
#define NUNITS 40
extern FILE *filepointer[NUNITS];
extern char *buffpointer[NUNITS];
/* includes needed for Exabyte I/O */
#include <sys/tape.h>
#include <string.h>
#include <netdb.h>
#include "rpc/rpc.h"
#include <sys/socket.h>
#include <time.h>
#define MAXSERV 10
#ifndef OPENC
extern CLIENT * clntpointer[MAXSERV];
#endif
 
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   The following definition is instalation dependent. It defines the
   host on which the FP-server for the local APOLLO ring is beig run */
#define  APOSERV   "rc01"
 
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
#define  MAXNAMELEN    257
#define  SENDSIZE   30000
#define  RECESIZE   30000
#define  PERMNUM  0x20805901       /* Prognum of permanent server */
#define  VERSNUM        1
#define  MAINPORT     1601
#define  OPEN           1
#define  CLOSE          2
#define  READ           3
#define  WRITE          4
#define  REWIND         5
#define  LOCK           6
#define  CLOSE_SERVER  10
#define  CHECK_PORT    15
#define  SUBMIT        20
#define  KEY_DELETE    21
#define  KEY_REWRITE   22
#define  KEY_WRITE     23
#define  KEY_READ      24
#define  KEY_BACKSPACE 25
 
struct data_t                      /* input structure for xdr_raw    */
{ int           nbytes;            /* #bytes transmitted             */
  char         *buffer;            /* pointer to the first byte      */
};
 
struct req_t                      /* input structure for xdr_request */
{ int    stream;                  /* stream_id for I/O               */
  int    nbskip;                  /* #bytes to be skipped            */
  int    bufsize;                 /* #bytes to be read/written       */
};
 
struct write_t                    /* input structure for xdr_write   */
{ int           stream;
  int           ibyte;            /* where to start writing          */
  int           nbwrite;          /* #bytes to be written            */
  char         *buffer;
};
 
struct open_t                     /* input structure for xdr_open    */
{ char         *name;
  char         *options;
  int           ispc;             /* code of space units:1=TRK,2=CYL,
                                     anything else='average blksize' */
  int           nr1;              /* primary space                   */
  int           nr2;              /* secondary space                 */
  int           keyunit;          /* zero for non-keyed access,
                                     positive integer for keyed access
                                     files, must be file specific    */
};
 
struct auth_t                     /* input structure for xdr_auth    */
{ char *User, *Password;
  char *Class, *Time_M, *Time_S;
};
#define _NETINCL_
#endif
 
#endif
#ifdef HPUX
#ifndef _NETINCL_
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifndef SEEK_CUR
/* on some machines (Alliant, MIPS,...) these macros are not
   defined in stdio.h   */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
/* We keep file pointers in an array in C, rather than passing as ints
 * back to Fortran.  This is implemented for 64-bit Alphas, but it'll
 * work on any machine.
 */
#define NUNITS 40
extern FILE *filepointer[NUNITS];
extern char *buffpointer[NUNITS];
/* includes needed for Exabyte I/O */
#include <sys/mtio.h>
#include <string.h>
#include <netdb.h>
#include "rpc/rpc.h"
#include <sys/socket.h>
#include <time.h>
#define MAXSERV 10
#ifndef OPENC
extern CLIENT * clntpointer[MAXSERV];
#endif

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   The following definition is instalation dependent. It defines the
   host on which the FP-server for the local APOLLO ring is beig run */
#define  APOSERV   "rc01"

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#define  MAXNAMELEN    257
#define  SENDSIZE   30000
#define  RECESIZE   30000
#define  PERMNUM  0x20805901       /* Prognum of permanent server */
#define  VERSNUM        1
#define  MAINPORT     1601
#define  OPEN           1
#define  CLOSE          2
#define  READ           3
#define  WRITE          4
#define  REWIND         5
#define  LOCK           6
#define  CLOSE_SERVER  10
#define  CHECK_PORT    15
#define  SUBMIT        20
#define  KEY_DELETE    21
#define  KEY_REWRITE   22
#define  KEY_WRITE     23
#define  KEY_READ      24
#define  KEY_BACKSPACE 25

struct data_t                      /* input structure for xdr_raw    */
{ int           nbytes;            /* #bytes transmitted             */
  char         *buffer;            /* pointer to the first byte      */
};

struct req_t                      /* input structure for xdr_request */
{ int    stream;                  /* stream_id for I/O               */
  int    nbskip;                  /* #bytes to be skipped            */
  int    bufsize;                 /* #bytes to be read/written       */
};

struct write_t                    /* input structure for xdr_write   */
{ int           stream;
  int           ibyte;            /* where to start writing          */
  int           nbwrite;          /* #bytes to be written            */
  char         *buffer;
};

struct open_t                     /* input structure for xdr_open    */
{ char         *name;
  char         *options;
  int           ispc;             /* code of space units:1=TRK,2=CYL,
                                     anything else='average blksize' */
  int           nr1;              /* primary space                   */
  int           nr2;              /* secondary space                 */
  int           keyunit;          /* zero for non-keyed access,
                                     positive integer for keyed access
                                     files, must be file specific    */
};

struct auth_t                     /* input structure for xdr_auth    */
{ char *User, *Password;
  char *Class, *Time_M, *Time_S;
};
#define _NETINCL_
#endif

#endif
#ifdef Linux
#ifndef _NETINCL_
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifndef SEEK_CUR
/* on some machines (Alliant, MIPS,...) these macros are not
   defined in stdio.h   */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
/* We keep file pointers in an array in C, rather than passing as ints
 * back to Fortran.  This is implemented for 64-bit Alphas, but it'll
 * work on any machine.
 */
#define NUNITS 40
extern FILE *filepointer[NUNITS];
extern char *buffpointer[NUNITS];
/* includes needed for Exabyte I/O */
#include <sys/mtio.h>
#include <sys/ioctl.h>
#include <string.h>
#include <netdb.h>
#include "rpc/rpc.h"
#include <sys/socket.h>
#include <time.h>
#define MAXSERV 10
#ifndef OPENC
extern CLIENT * clntpointer[MAXSERV];
#endif

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   The following definition is instalation dependent. It defines the
   host on which the FP-server for the local APOLLO ring is beig run */
#define  APOSERV   "rc01"

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#define  MAXNAMELEN    257
#define  SENDSIZE   30000
#define  RECESIZE   30000
#define  PERMNUM  0x20805901       /* Prognum of permanent server */
#define  VERSNUM        1
#define  MAINPORT     1601
#define  OPEN           1
#define  CLOSE          2
#define  READ           3
#define  WRITE          4
#define  REWIND         5
#define  LOCK           6
#define  CLOSE_SERVER  10
#define  CHECK_PORT    15
#define  SUBMIT        20
#define  KEY_DELETE    21
#define  KEY_REWRITE   22
#define  KEY_WRITE     23
#define  KEY_READ      24
#define  KEY_BACKSPACE 25

struct data_t                      /* input structure for xdr_raw    */
{ int           nbytes;            /* #bytes transmitted             */
  char         *buffer;            /* pointer to the first byte      */
};

struct req_t                      /* input structure for xdr_request */
{ int    stream;                  /* stream_id for I/O               */
  int    nbskip;                  /* #bytes to be skipped            */
  int    bufsize;                 /* #bytes to be read/written       */
};

struct write_t                    /* input structure for xdr_write   */
{ int           stream;
  int           ibyte;            /* where to start writing          */
  int           nbwrite;          /* #bytes to be written            */
  char         *buffer;
};

struct open_t                     /* input structure for xdr_open    */
{ char         *name;
  char         *options;
  int           ispc;             /* code of space units:1=TRK,2=CYL,
                                     anything else='average blksize' */
  int           nr1;              /* primary space                   */
  int           nr2;              /* secondary space                 */
  int           keyunit;          /* zero for non-keyed access,
                                     positive integer for keyed access
                                     files, must be file specific    */
};

struct auth_t                     /* input structure for xdr_auth    */
{ char *User, *Password;
  char *Class, *Time_M, *Time_S;
};
#define _NETINCL_
#endif

#endif
#ifdef SunOS
#ifndef _NETINCL_
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifndef SEEK_CUR
/* on some machines (Alliant, MIPS,...) these macros are not
   defined in stdio.h   */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
/* We keep file pointers in an array in C, rather than passing as ints
 * back to Fortran.  This is implemented for 64-bit Alphas, but it'll
 * work on any machine.
 */
#define NUNITS 40
extern FILE *filepointer[NUNITS];
extern char *buffpointer[NUNITS];
/* includes needed for Exabyte I/O */
#include <sys/mtio.h>
#include <sys/ioctl.h>
#include <string.h>
#include <netdb.h>
#include "rpc/rpc.h"
#include <sys/socket.h>
#include <time.h>
#define MAXSERV 10
#ifndef OPENC
extern CLIENT * clntpointer[MAXSERV];
#endif
 
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   The following definition is instalation dependent. It defines the
   host on which the FP-server for the local APOLLO ring is beig run */
#define  APOSERV   "rc01"
 
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
#define  MAXNAMELEN    257
#define  SENDSIZE   30000
#define  RECESIZE   30000
#define  PERMNUM  0x20805901       /* Prognum of permanent server */
#define  VERSNUM        1
#define  MAINPORT     1601
#define  OPEN           1
#define  CLOSE          2
#define  READ           3
#define  WRITE          4
#define  REWIND         5
#define  LOCK           6
#define  CLOSE_SERVER  10
#define  CHECK_PORT    15
#define  SUBMIT        20
#define  KEY_DELETE    21
#define  KEY_REWRITE   22
#define  KEY_WRITE     23
#define  KEY_READ      24
#define  KEY_BACKSPACE 25
 
struct data_t                      /* input structure for xdr_raw    */
{ int           nbytes;            /* #bytes transmitted             */
  char         *buffer;            /* pointer to the first byte      */
};
 
struct req_t                      /* input structure for xdr_request */
{ int    stream;                  /* stream_id for I/O               */
  int    nbskip;                  /* #bytes to be skipped            */
  int    bufsize;                 /* #bytes to be read/written       */
};
 
struct write_t                    /* input structure for xdr_write   */
{ int           stream;
  int           ibyte;            /* where to start writing          */
  int           nbwrite;          /* #bytes to be written            */
  char         *buffer;
};
 
struct open_t                     /* input structure for xdr_open    */
{ char         *name;
  char         *options;
  int           ispc;             /* code of space units:1=TRK,2=CYL,
                                     anything else='average blksize' */
  int           nr1;              /* primary space                   */
  int           nr2;              /* secondary space                 */
  int           keyunit;          /* zero for non-keyed access,
                                     positive integer for keyed access
                                     files, must be file specific    */
};
 
struct auth_t                     /* input structure for xdr_auth    */
{ char *User, *Password;
  char *Class, *Time_M, *Time_S;
};
#define _NETINCL_
#endif
 
#endif

#ifdef IRIX

#ifndef _NETINCL_
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifndef SEEK_CUR
/* on some machines (Alliant, MIPS,...) these macros are not
   defined in stdio.h   */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
/* We keep file pointers in an array in C, rather than passing as ints
 * back to Fortran.  This is implemented for 64-bit Alphas, but it'll
 * work on any machine.
 */
#define NUNITS 40
extern FILE *filepointer[NUNITS];
extern char *buffpointer[NUNITS];
/* includes needed for Exabyte I/O */
#include <sys/mtio.h>
#include <string.h>
#include <netdb.h>
#include "rpc/rpc.h"
#include <sys/socket.h>
#include <time.h>
#define MAXSERV 10
#ifndef OPENC
extern CLIENT * clntpointer[MAXSERV];
#endif

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   The following definition is instalation dependent. It defines the
   host on which the FP-server for the local APOLLO ring is beig run */
#define  APOSERV   "rc01"

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#define  MAXNAMELEN    257
#define  SENDSIZE   30000
#define  RECESIZE   30000
#define  PERMNUM  0x20805901       /* Prognum of permanent server */
#define  VERSNUM        1
#define  MAINPORT     1601
#define  OPEN           1
#define  CLOSE          2
#define  READ           3
#define  WRITE          4
#define  REWIND         5
#define  LOCK           6
#define  CLOSE_SERVER  10
#define  CHECK_PORT    15
#define  SUBMIT        20
#define  KEY_DELETE    21
#define  KEY_REWRITE   22
#define  KEY_WRITE     23
#define  KEY_READ      24
#define  KEY_BACKSPACE 25

struct data_t                      /* input structure for xdr_raw    */
{ int           nbytes;            /* #bytes transmitted             */
  char         *buffer;            /* pointer to the first byte      */
};

struct req_t                      /* input structure for xdr_request */
{ int    stream;                  /* stream_id for I/O               */
  int    nbskip;                  /* #bytes to be skipped            */
  int    bufsize;                 /* #bytes to be read/written       */
};

struct write_t                    /* input structure for xdr_write   */
{ int           stream;
  int           ibyte;            /* where to start writing          */
  int           nbwrite;          /* #bytes to be written            */
  char         *buffer;
};

struct open_t                     /* input structure for xdr_open    */
{ char         *name;
  char         *options;
  int           ispc;             /* code of space units:1=TRK,2=CYL,
                                     anything else='average blksize' */
  int           nr1;              /* primary space                   */
  int           nr2;              /* secondary space                 */
  int           keyunit;          /* zero for non-keyed access,
                                     positive integer for keyed access
                                     files, must be file specific    */
};

struct auth_t                     /* input structure for xdr_auth    */
{ char *User, *Password;
  char *Class, *Time_M, *Time_S;
};
#define _NETINCL_
#endif
#endif

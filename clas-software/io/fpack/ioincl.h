#ifdef AIX
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
#endif
#if defined(HPUX) || defined(IRIX)
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
#endif
#ifdef Linux
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
#endif
#ifdef SunOS
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
#endif

/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
           /* update a record in a remote keyed access file
              ============================================= */
/*KEEP,NETINCL.*/
#include "netincl.h"
/*KEND.*/
 
               /* XDR function for write request
                  ============================== */
int xdr_puts(xdrsp, writep)
XDR *xdrsp;
struct write_t *writep;
{if(!xdr_int(xdrsp, &(writep->stream))) return 0;
 if(!xdr_int(xdrsp, &(writep->ibyte))) return 0;
 if(!xdr_int(xdrsp, &(writep->nbwrite))) return 0;
 if(!xdr_opaque(xdrsp,writep->buffer,
                   (unsigned int)writep->nbwrite)) return 0;
 return 1;
}
 
    /* rewrite last read record in a remote keyed access file
       ====================================================== */
void krewc_(iclptr,streamptr,sizeptr,buffer,retptr)
int      *iclptr;      /* index to pointer to client handle         */
int      *streamptr;   /* pointer to stream_id                       */
int      *sizeptr;     /* pointer to #bytes to be written            */
int      *buffer;      /* pointer to buffer                          */
int      *retptr;      /* pointer to return value (0 means success,  */
                       /*                          1 means failure)  */
 {
   CLIENT  *clptr;
   char            *report = NULL;
   struct write_t   writed;
   enum    clnt_stat    clnt_stat;
/*KEEP,TIMEOUT.*/
#include "timeout.h"
/*KEND.*/
#ifndef Linux
   writed.buffer = (char *) buffer;
#else /* Linux */
   int              i, mask2 = 255*256, mask3 = 255*256*256;
   unsigned int    *ptr, *ptrk;
     /* the buffer must be swapped before sending, but the original
        F-pack buffer should not be changed */
   if( (ptr = (unsigned int *)malloc( *sizeptr )) == (unsigned int *)0 )
     {fprintf(stderr,"krewc: cannot allocate buffer \n");
      *retptr = 1;
      return;
     }
   ptrk = ptr;
   memcpy((char *)ptr, buffer, *sizeptr);
   for(i = 0; i < *sizeptr/4; i++, ptr++)
      *ptr=((*ptr<<24) | ((*ptr<<8)&mask3)) | (((*ptr>>8)&mask2) | (*ptr>>24));
   writed.buffer = (char *) ptrk;
#endif /* Linux */
   writed.stream = *streamptr;
   writed.ibyte  = 0; /* not used */
   writed.nbwrite = *sizeptr;
   clptr = clntpointer[ *iclptr ];
   clnt_stat=clnt_call(clptr, KEY_REWRITE, xdr_puts, &writed,
                       xdr_wrapstring, &report,
                       total_timeout);
#ifdef Linux
   free((char *)ptrk);
#endif /* Linux */
   if(clnt_stat != RPC_SUCCESS)
      {fprintf(stderr,"  clnt_call for REWRITE failed\n");
       clnt_perrno(clnt_stat);
       perror(" RPC");
       clnt_freeres(clptr, xdr_wrapstring, &report);
       close( *((int *)((clptr)->cl_private)) );
       clnt_destroy(clptr);
       *retptr = 3;
       return;
      }
   if(report[0] == 'd')  /*  duplicate key */
      {
      *retptr = 2;
      fprintf(stderr,"SERVER: %s \n", report);
      clnt_freeres(clptr, xdr_wrapstring, &report);
      return;
      }
   if(report[0] == 'p')  /*  error */
      {
      *retptr = 1;
      fprintf(stderr,"SERVER: %s \n", report);
      clnt_freeres(clptr, xdr_wrapstring, &report);
      return;
      }
   clnt_freeres(clptr, xdr_wrapstring, &report);
   *retptr = 0;
 }
 

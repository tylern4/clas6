/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
                    /* rewind request
                       ============== */
/*KEEP,NETINCL.*/
#include "netincl.h"
/*KEND.*/
 
void rewinc_(iclptr,streamptr,retptr)
int      *iclptr;      /* index to pointer to client handle         */
int      *streamptr;   /* pointer to stream_id                      */
int      *retptr;      /* pointer to return value (0 means success  */
                       /* -1 means failure)                         */
 {
   CLIENT *clptr;
   char    *report = NULL;
   enum    clnt_stat  clnt_stat;
/*KEEP,TIMEOUT.*/
#include "timeout.h"
/*KEND.*/
   clptr = clntpointer[ *iclptr ];
   clnt_stat=clnt_call(clptr, REWIND, xdr_int, streamptr,
                       xdr_wrapstring, &report,
                       total_timeout);
   if(clnt_stat != RPC_SUCCESS)
      {fprintf(stderr,"  clnt_call for REWIND failed\n");
       clnt_perrno(clnt_stat);
       perror(" RPC");
       clnt_freeres(clptr, xdr_wrapstring, &report);
       close( *((int *)((clptr)->cl_private)) );
       clnt_destroy(clptr);
       *retptr =  -2;
       return;
      }
   if(report[0] == 'c')
      { *retptr = -1;
        fprintf(stderr,"SERVER: %s \n", report);
        clnt_freeres(clptr, xdr_wrapstring, &report);
        return;
      }
   clnt_freeres(clptr, xdr_wrapstring, &report);
   *retptr = 0;
 }
 

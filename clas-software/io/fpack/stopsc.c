/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
                 /* close private server request
                    ============================ */
 
/*KEEP,NETINCL.*/
#include "netincl.h"
/*KEND.*/
 
void stopsc_(iclptr, retptr)
int      *iclptr;      /* index to pointer to client handle         */
int      *retptr;      /* pointer to return value (0 means success  */
                       /* -1 means failure)                         */
 {
   CLIENT  *clptr;
   enum    clnt_stat   clnt_stat;
/*KEEP,TIMEOUT.*/
#include "timeout.h"
/*KEND.*/
   clptr = clntpointer[ *iclptr ];
   clnt_stat=clnt_call(clptr, CLOSE_SERVER, xdr_void, NULL,
                       xdr_void, NULL,total_timeout);
   if(clnt_stat != RPC_SUCCESS)
      {fprintf(stderr,"  clnt_call for CLOSE_SERVER failed\n");
       clnt_perrno(clnt_stat);
       perror(" RPC");
       close( *((int *)((clptr)->cl_private)) );
       clnt_destroy(clptr);
       clptr = NULL;
       clntpointer[ *iclptr ] = NULL;
       *retptr =  -1;
       return;
      }
   *retptr = 0;
   close( *((int *)((clptr)->cl_private)) );
   clnt_destroy(clptr);
   clptr = NULL;
   clntpointer[ *iclptr ] = NULL;
 /*  fprintf(stderr,"disconnected\n");
   fflush(stderr); */
 }
 

/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
                    /* close file request
                       ==================     */
 
/*KEEP,NETINCL.*/
#include "netincl.h"
/*KEND.*/
 
void closec_(iclptr,streamptr,retptr)
int      *iclptr;      /* index to pointer to client handle         */
int      *streamptr;   /* pointer to stream_id                      */
int      *retptr;      /* pointer to return value (0 means success  */
                       /* -1 means failure)  (on input 1 closes     */
                       /*                          connection)      */
 {
   CLIENT *clptr;      /* pointer to client handle                  */
   char    *report = NULL;
   int      flag;
   enum    clnt_stat  clnt_stat;
/*KEEP,TIMEOUT.*/
#include "timeout.h"
/*KEND.*/
 
   clptr = clntpointer[*iclptr];
   flag = *retptr;
   *retptr = 0;
   clnt_stat=clnt_call(clptr, CLOSE, xdr_int, streamptr,
                       xdr_wrapstring, &report,
                       total_timeout);
   if(clnt_stat != RPC_SUCCESS)
      {fprintf(stderr,"  clnt_call for CLOSE failed\n");
       clnt_perrno(clnt_stat);
       perror(" RPC");
       *retptr =  -1;
      }
   if(report != NULL)
      {if(report[0] == 'c')
         {*retptr = -1;
          fprintf(stderr,"SERVER: %s \n", report);
         }
       clnt_freeres(clptr, xdr_wrapstring, &report);
      }
   if(flag == 1)
      {clnt_stat=clnt_call(clptr, CLOSE_SERVER, xdr_void, NULL,
                           xdr_void, NULL,total_timeout);
       if(clnt_stat != RPC_SUCCESS)
         {fprintf(stderr,"  clnt_call for CLOSE_SERVER failed\n");
          clnt_perrno(clnt_stat);
          close( *((int *)((clptr)->cl_private)) );
          clnt_destroy(clptr);
          clptr = NULL;
          return;
         }
     /* Some RPC implementations (e.g. on IBM MVS ) do not close the
        socket within clnt_destroy(). Socket descriptor is the first
        element of the structure ct_data (pointed to by cl_private)
        defined and used internally in clnt_tcp.c. Hence, ...     */
       close( *((int *)((clptr)->cl_private)) );
       clnt_destroy(clptr);
       clptr=NULL;
  /*     fprintf(stderr,"disconnected\n");
       fflush(stderr); */
 
       clntpointer[ *iclptr ] = NULL;
      }
 }
 
 

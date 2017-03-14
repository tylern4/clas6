/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
               /* read from a remote file
                  ======================= */
/*KEEP,NETINCL.*/
#include "netincl.h"
/*KEND.*/
 
              /* XDR function for read request
                 ============================= */
 int xdr_get(xdrsp, datap)
 XDR *xdrsp;
 struct req_t *datap;
 {
  if(!xdr_int(xdrsp, &(datap->stream))) return 0;
  if(!xdr_int(xdrsp, &(datap->nbskip))) return 0;
  if(!xdr_int(xdrsp, &(datap->bufsize))) return 0;
  return 1;
 }
 
              /* XDR function for raw bytes
                 ========================== */
 int xdr_raw(xdrsp, datap)
 XDR *xdrsp;
 struct data_t *datap;
 {if(!xdr_int(xdrsp, &(datap->nbytes))) return 0;
  if(datap->nbytes < 0) return 1; /* no data follow */
  if(!xdr_opaque(xdrsp,datap->buffer,
                    (unsigned int)datap->nbytes)) return 0;
  return 1;
 }
 
                 /* read bytes request
                    ==================   */
void readc_(iclptr,streamptr,skipptr,sizeptr,buffer,retptr)
int      *iclptr;        /* index to pointer to client handle       */
int      *streamptr;     /* pointer to stream_id                    */
int      *skipptr;       /* pointer to #bytes to be skipped         */
int      *sizeptr;       /* pointer to #bytes to be read            */
int      *buffer;        /* pointer to buffer                       */
int      *retptr;        /* pointer to #bytes read (return value)   */
 
 {
   CLIENT *clptr;
   struct data_t data;
   struct req_t  req;
   enum    clnt_stat     clnt_stat;
/*KEEP,TIMEOUT.*/
#include "timeout.h"
/*KEND.*/
   data.buffer = (char *) buffer;
   req.stream = *streamptr;
   req.nbskip = *skipptr;
   req.bufsize = *sizeptr;
   clptr = clntpointer[ *iclptr ];
 
   clnt_stat=clnt_call(clptr, READ, xdr_get, &req,
                       xdr_raw, &data,
                       total_timeout);
   if(clnt_stat != RPC_SUCCESS)
        {fprintf(stderr,"  clnt_call for READ failed\n");
         clnt_perrno(clnt_stat);
         perror(" RPC");
         close( *((int *)((clptr)->cl_private)) );
         clnt_destroy(clptr);
         *retptr = -1;
         return;
        }
   *retptr = data.nbytes;
 }
 

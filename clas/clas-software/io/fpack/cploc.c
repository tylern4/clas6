/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
#define DUM /* just because of a CMZ bug; if not, it calls the deck
               IBMMVS */
/*KEEP,NETINCL.*/
#include "netincl.h"
/*KEND.*/
 
 
struct lock_t                     /* input structure for xdr_lock    */
{ char         *name;
  int           stream;
  int           mode;
  int           num;              /* for future use                  */
};
 
               /* XDR function for lock request
                  ============================= */
int xdr_lock(xdrsp, lockp)
XDR *xdrsp;
struct lock_t *lockp;
{if(!xdr_wrapstring(xdrsp, &(lockp->name))) return 0;
 if(!xdr_int(xdrsp, &(lockp->stream))) return 0;
 if(!xdr_int(xdrsp, &(lockp->mode))) return 0;
 if(!xdr_int(xdrsp, &(lockp->num))) return 0;
 return 1;
}
 
                 /* lock file request
                    ================= */
 void cploc_(name,mode,iclptr,streamptr,retptr,namelen)
 char     *name;          /* file name                              */
 int      *mode;          /* 0/1 for unlock/lock                    */
 int      *iclptr;        /* index to pointer to client handle      */
 int      *streamptr;     /* pointer to stream_id                   */
 int      *retptr;        /* pointer to return value                */
 short    *namelen;       /* name length                            */
 {
   CLIENT  *clptr;       /* pointer to client handle               */
   char    filename[MAXNAMELEN];
   struct  lock_t  req;
   enum    clnt_stat     clnt_stat;
/*KEEP,TIMEOUT.*/
#include "timeout.h"
/*KEND.*/
 
   clptr = clntpointer[ *iclptr ];
 
   if( *namelen >= MAXNAMELEN )
        {fprintf(stderr,"Filename too long\n");
         return;
        }
   strncpy(filename,name,*namelen);
   filename[*namelen] = '\0';
   req.name = filename;
   req.stream = *streamptr;
   req.mode = *mode;
   req.num = 0;
 
   clnt_stat=clnt_call(clptr, LOCK, xdr_lock, &req,
                       xdr_int, retptr, total_timeout);
   if(clnt_stat != RPC_SUCCESS)
        {fprintf(stderr,"  clnt_call for LOCK failed\n");
         clnt_perrno(clnt_stat);
         perror(" RPC");
         close( *((int *)((clptr)->cl_private)) );
         clnt_destroy(clptr);
         *retptr = -1;
         return;
        }
 
 }

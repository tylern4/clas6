/*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko*/
/*-- Author :    Zbigniew Szkutnik   16/12/94*/
 
        /* read from a remote keyed access file
           ==================================== */
/*KEEP,NETINCL.*/
#include "netincl.h"
/*KEND.*/
 
 struct kread_t          /* input structure for xdr_key     */
 { int    stream;        /* stream_id of keyed file         */
   int    keycode;       /* 0=nokey 1=KEY 2=KEYGE 3=KEYGT   */
   int    keylen;        /* length of the key in bytes      */
   char  *key;           /* pointer to the key              */
   int    nbytes;        /* #bytes to be read               */
 };
 
#ifdef Linux
     /* swap four bytes starting at ptr
        ===============================  */
 static void byte_swap(ptr)
 char *ptr;
 {char ch;
  ch = *ptr;
  *ptr = *(ptr + 3);
  *(ptr + 3) = ch;
  ch = *(ptr + 1);
  *(ptr + 1) = *(ptr + 2);
  *(ptr + 2) = ch;
 }
#endif /* Linux */
              /* XDR function for keyed_read request
                 =================================== */
 int xdr_key(xdrsp, datap)
 XDR *xdrsp;
 struct kread_t *datap;
 {
#ifdef Linux
  char key[40];
  memcpy(key,datap->key,datap->keylen);
  byte_swap(key + 8);
  byte_swap(key + 12);
  byte_swap(key + 16);
#endif /* Linux */
  if(!xdr_int(xdrsp, &(datap->stream))) return 0;
  if(!xdr_int(xdrsp, &(datap->keycode))) return 0;
  if(!xdr_int(xdrsp, &(datap->keylen))) return 0;
#ifndef Linux
  if(!xdr_opaque(xdrsp, datap->key, (unsigned int)datap->keylen)) return 0;
#else /* Linux */
  if(!xdr_opaque(xdrsp, key, (unsigned int)datap->keylen)) return 0;
#endif /* Linux */
  if(!xdr_int(xdrsp, &(datap->nbytes))) return 0;
  return 1;
 }
 
              /* XDR function for raw bytes
                 ========================== */
 int xdr_rawd(xdrsp, datap)
 XDR *xdrsp;
 struct data_t *datap;
 {if(!xdr_int(xdrsp, &(datap->nbytes))) return 0;
  if(datap->nbytes < 0) return 1; /* no data follow */
  if(!xdr_opaque(xdrsp,datap->buffer,
                    (unsigned int)datap->nbytes)) return 0;
  return 1;
 }
 
    /* read a record from a remote keyed access file
       =============================================   */
void kreadc_(iclptr,streamptr,sizeptr,buffer,codeptr,key,
            retptr,lenptr)
int      *iclptr;      /* index to pointer to client handle         */
int      *streamptr;     /* pointer to stream_id                    */
int      *sizeptr;       /* pointer to #bytes to be read            */
int      *codeptr;       /* pointer to key_code;0/1/2/3=no/KEY/KEYGE/
                                                        KEYGT       */
char     *key;           /* key                                     */
short    *lenptr;        /* length of key in bytes                  */
int      *buffer;        /* pointer to buffer                       */
int      *retptr;        /* pointer to #bytes read (return value)   */
 
 {
   CLIENT  *clptr;
   struct data_t data;
   struct kread_t  req;
   enum    clnt_stat     clnt_stat;
/*KEEP,TIMEOUT.*/
#include "timeout.h"
/*KEND.*/
   data.buffer = (char *) buffer;
   req.stream = *streamptr;
   req.keycode = *codeptr;
   req.keylen = (int)*lenptr;
   req.key = key;
   req.nbytes = *sizeptr;
 
   clptr = clntpointer[ *iclptr ];
   clnt_stat=clnt_call(clptr, KEY_READ, xdr_key, &req,
                       xdr_rawd, &data,
                       total_timeout);
   if(clnt_stat != RPC_SUCCESS)
        {fprintf(stderr,"  clnt_call for KEY_READ failed\n");
         clnt_perrno(clnt_stat);
         perror(" RPC");
         close( *((int *)((clptr)->cl_private)) );
         clnt_destroy(clptr);
         *retptr = 3;
         return;
        }
   if( data.nbytes == -1 ) *retptr = 1;
   else
   if( data.nbytes == -2 ) *retptr = 2;
   else
   *retptr = 0;
 }
 

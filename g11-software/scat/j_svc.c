#include <stdio.h>
#include <rpc/rpc.h>
#include "j.h"

static int _r = 1; 
static int *rtn = &_r;

void
evtsvc_inp_1(rqstp, transp)
struct svc_req *rqstp;
SVCXPRT *transp;
{
  union {
    Event msgnoreply_1_arg;
    Event msgwreply_1_arg;
    Event browcreate_1_arg;
    Event browdestroy_1_arg;
    Event browfilter_1_arg;
  } argument;
  char *result;
  bool_t (*xdr_argument)(), (*xdr_result)();
  char *(*local)();
  
  switch (rqstp->rq_proc) {
  case NULLPROC:
    svc_sendreply(transp, xdr_void, NULL);
    return;
    
  case SVCNAME:
    xdr_argument = xdr_void;
    xdr_result = xdr_mstr;
    local = (char *(*)()) svcname_1_svc;
    break;
    
  case MsgNoReply:
    xdr_argument = xdr_Event;
    xdr_result = xdr_int;
    local = (char *(*)()) msgnoreply_1_svc;
    break;
    memset(&argument, 0, sizeof(argument));
    if (!svc_getargs(transp, xdr_argument, &argument)) {
      svcerr_decode(transp);
      return;
    }
    result = (char *)rtn; 
    _r++;
    if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
      svcerr_systemerr(transp);
    }
    result = (*local)(&argument, rqstp);
    if (!svc_freeargs(transp, xdr_argument, &argument)) {
      fprintf(stderr, "unable to free arguments");
      exit(1);
    }
    return;

  case MsgWReply:
    xdr_argument = xdr_Event;
    xdr_result = xdr_Event;
    local = (char *(*)()) msgwreply_1_svc;
    break;
    
  case BrowCreate:
    xdr_argument = xdr_Event;
    xdr_result = xdr_int;
    local = (char *(*)()) browcreate_1_svc;
    break;
    
  case BrowDestroy:
    xdr_argument = xdr_Event;
    xdr_result = xdr_int;
    local = (char *(*)()) browdestroy_1_svc;
    break;
    
  case BrowFilter:
    xdr_argument = xdr_Event;
    xdr_result = xdr_Event;
    local = (char *(*)()) browfilter_1_svc;
    break;
    
  default:
    svcerr_noproc(transp);
    return;
  }
  memset(&argument, 0, sizeof(argument));
  if (!svc_getargs(transp, xdr_argument, &argument)) {
    svcerr_decode(transp);
    return;
  }
  result = (*local)(&argument, rqstp);
  if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
    svcerr_systemerr(transp);
  }
  if (!svc_freeargs(transp, xdr_argument, &argument)) {
    fprintf(stderr, "unable to free arguments");
    exit(1);
  }
  return;
}


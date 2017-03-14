

#include <stdio.h>
#include <sys/types.h>
#include <rpc/rpc.h>
#include <sys/time.h>
#include "j.h"
#include <tcl.h>
#include "svt.h"

extern SVT *svt;
extern Tcl_Interp *itcl_f_interp ;
static int rr=1, *ret=&rr;

int * msgnoreply_1_svc (argp, rqstp)
Event *argp;
struct svc_req *rqstp;
{
  static int  result;
  int         code, len;

  code = Tcl_Eval (itcl_f_interp, argp->s);
  if (code != TCL_OK)
    printf ("itcl_f_interp->result=%s\n", itcl_f_interp->result);
  else
    printf ("---------> %s\n", itcl_f_interp->result);

  rr++;   /* dummy number - really no reply */
  return ((int *)ret);
}
  

mstr * 
svcname_1_svc(argp, rqstp)
void *argp;
struct svc_req *rqstp;
{
  static mstr  result;
  char banner[1024];
  char *un;
  int j, z;

  memset (svt->scratchBuffer, 0x00, SB_SIZE);

  un = getenv ("LOGNAME");
  if (un != 0) {
    sprintf (banner, "%s %s %s %ld %ld\0", un, 
	     svt->pgm_name, svt->uts.nodename, svt->program, svt->vers);
    sprintf (svt->scratchBuffer, "%s _nolists_ 0", banner);
  }
  else {
    sprintf (banner, "%s %s %s %ld %ld\0", "unknown", 
	     "RECSIS", svt->uts.nodename, svt->program, svt->vers);
    sprintf (svt->scratchBuffer, "%s _nolists_ 0", banner);
  }    
  j = strlen (svt->scratchBuffer);
  svt->scratchBuffer[j] = 0;
  Scat_ErrorMsg (svt->scratchBuffer);
  
  result = svt->scratchBuffer;
  return &result;
}

Event * 
msgwreply_1_svc(argp, rqstp)
Event *argp;
struct svc_req *rqstp;
{

  static Event  result;
  int         code, len;

  code = Tcl_Eval (itcl_f_interp, argp->s);
  if (code != TCL_OK)
    printf ("itcl_f_interp->result=%s\n", itcl_f_interp->result);
  else
    printf ("---------> %s\n", itcl_f_interp->result);

  result.s = itcl_f_interp->result;
  
  return(&result);
}

int * 
browcreate_1_svc(argp, rqstp)
mstr *argp;
struct svc_req *rqstp;
{

	static mstr  result;

	/*
	 * insert server code here
	 */

	return(&result);
}

int * 
browdestroy_1_svc(argp, rqstp)
mstr *argp;
struct svc_req *rqstp;
{

	static int  result;

	/*
	 * insert server code here
	 */

	return(&result);
}

Event * 
browfilter_1_svc(argp, rqstp)
mstr *argp;
struct svc_req *rqstp;
{

	static Event  result;

	/*
	 * insert server code here
	 */

	return(&result);
}

void user_exit ()
{
  return;
}

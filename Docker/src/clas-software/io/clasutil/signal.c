/*  signal_     fortran wrapper calls POSIX-compliant signal routine
 *
 *  Follows calling convention in "Unix for Fortran Programmers",
 *    except for status return, which is different for POSIX
 *
 *
 *  Elliott Wolin, W&M, 5-mar-96
 *  Redone to use POSIX signals 5-may-97 EJW (tested on HP and Sun)
 *
 *
 *  inputs:                                        
 *    signum     signal number                     
 *    handler    address of signal handler routine
 *    action:    -1 to declare handler             
 *                0 to restore default             
 *                1 to ignore signal               
 *
 *  outputs:
 *    returns 0 for success, -errno on failure
 *
 */

#define _POSIX_SOURCE_ 1
#define __EXTENSIONS__ 
#define _INCLUDE_POSIX_SOURCE

#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>


int *signal_(int *signum, void (*handler)(), int *action){

  int status;
  struct sigaction act,old_act;


  sigemptyset(&act.sa_mask);
  act.sa_flags=0;


  switch (*action){
  case -1:
    act.sa_handler=handler;
    status=sigaction(*signum,&act,&old_act);
    break;
      
  case 0:
    act.sa_handler=SIG_DFL;
    status=sigaction(*signum,&act,&old_act);
    break;
    
  case 1:
    act.sa_handler=SIG_IGN;
    status=sigaction(*signum,&act,&old_act);
    break;
  }
  
  
/*  return 0 on success, -errno on failure */
  if(status==0)
    return (int*) 0;
  else
    return (int*) -errno;

}

      SUBROUTINE SCAT_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: scat_init.F,v 1.2 1997/07/10 14:20:55 freyberg Exp $
c
c  Documentation for subroutine SCAT_INIT
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters: 
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Dec 20 15:44:54 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "run_control.inc"
#include "reversion.inc"
#include "scat_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: scat_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/07/10 14:20:55 $')
      PARAMETER (CAUTHO = '$Author: freyberg $')
      DATA CRCSID/   
     1'$Id: scat_init.F,v 1.2 1997/07/10 14:20:55 freyberg Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*150  CRMESS
      PARAMETER (CRNAME='SCAT_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LENOCC, INDEXA, SCAT_LOCATEPROGRAM, tcl_eval, 
     1        scat_initialize, scat_programno, scat_versionno
      EXTERNAL LENOCC, INDEXA, tcl_eval, scat_initialize,
     1         scat_locateprogram, scat_programno, scat_versionno
      INTEGER ITCL_ERR
      INTEGER pgmno
      INTEGER verno
      CHARACTER*300 cscat, ctemp
c_end_var
c
c  executable code for routine SCAT_INIT:
c----6----------------------------------------------------------------72
c
c   tell the user where to find the event list in EventStore
c
         write(scat_name,10)jobexe(1:lenocc(jobexe)),nseq,
     1                     jobusrnm(1:lenocc(jobusrnm))
 10      format(a,'_',i4.4,'.',a)
c
         write(crmess,11)scat_name(1:lenocc(scat_name))
 11      format('Event Store name for this job :  ',a)
         call recmes(crname,'i',crmess)
c
c  initialize scat and register the event list with it
c
         itcl_catch = scat_initialize(cscat)
c
         call scat_register(SCAT_NAME(1:lenocc(scat_name)), itcl_err)
c
c  attempt to start the event store......
c
c  first form the EventStore name
c
         pgmno = scat_programno (pgmno)
         write (*,*) 'jjjjjjjjjjjjjjjjjjjjjjj   ', pgmno
         verno = scat_versionno (pgmno)
c
         write(crmess,19)scat_host(1:lenocc(scat_host)) ,
     1                  scat_prog_start(1:lenocc(scat_prog_start)),
     1                  nseq,jobusrnm(1:lenocc(jobusrnm)),
     1                  scat_host(1:lenocc(scat_host)),pgmno,verno
#ifdef HPUX
 19      format('exec remsh ',a,2x,a,' -enum ',2x,I4.4,' -ename ',
#else
 19      format('exec rsh ',a,2x,a,' -enum ',2x,I4.4,' -ename ',
#endif
     1     2x,a,' -pgmhost ',2x,a,' -pgmno ',2x,I10.10,' -pgmver ',
     1     2x,I4.4,' & ')
         call recmes(crname,'i','Starting EventStore via cmd: ')
         call recmes(crname,'i',crmess)
         itcl_err = tcl_eval(crmess(1:lenocc(crmess)))
c
c  Now build the program name to look for.
c 
c         call recmes(crname,'i','Trying to connect to EventStore via:')
c         write(crmess,129)scat_prog_start(1:lenocc(scat_prog_start)),
c     1                  jobusrnm(1:lenocc(jobusrnm))
c 129     format(a,'.',a)
c         write(ctemp(1:),*)'scat_locateprogram(',
c     1             scat_host(1:lenocc(scat_host)),',',
c     1             crmess(1:lenocc(crmess)),',',
c     2             nseq,')'
c         call recmes(crname,'i',ctemp)
c

      RETURN
      END
c
c------------------------------------------------------------------------------





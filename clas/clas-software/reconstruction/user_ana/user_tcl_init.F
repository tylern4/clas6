      SUBROUTINE USER_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: user_tcl_init.F,v 1.10 2008/03/16 04:33:29 fklein Exp $
c
c  Documentation for subroutine USER_TCL_INIT
c
c  Purpose:  USER routine for initializing runtime variables
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Dec  1 17:04:06 EST 1995
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
#include "user_control.inc"
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: user_tcl_init.F,v 1.10 2008/03/16 04:33:29 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_TCL_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, i
      INTEGER ITCL_INT, ITCL_STRING, ITCL_INT_READ, LSTRING, ITCL_DOUBLE
      DATA ITCL_INT, ITCL_DOUBLE, ITCL_STRING, ITCL_INT_READ, LSTRING/1,2,4,129,512/
      Character*32  outbos
c_end_var
c
c  executable code for routine USER_TCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c  The below call LINKS the fortran variable to the tcl variable, to 
c  interactive change the value of `my_variable' either:
c     A) type  set my_variable 10  at the prompt or in your INITializing script
c     B) setup the communication shell and : SendMsg "set my_variable 10"
c
      eventtagfile=' '
      call tcl_linkchar('eventtagfile',eventtagfile,itcl_string,lstring)
      monhistfile='anamonhist'
      call tcl_linkchar('monhistfile',monhistfile,itcl_string,lstring)
      hist21file='hist21'
      call tcl_linkchar('hist21file',hist21file,itcl_string,lstring)
      normfile1='norm.tc.dat'
      call tcl_linkchar('normfile1',normfile1,itcl_string,lstring)
      normfile2='norm.eb.dat'
      call tcl_linkchar('normfile2',normfile2,itcl_string,lstring)
      normfile3='norm.erg.dat'
      call tcl_linkchar('normfile3',normfile3,itcl_string,lstring)
      normfile4='norm.erg2.dat'
      call tcl_linkchar('normfile4',normfile4,itcl_string,lstring)
      tripfile='norm.trip.dat'
      call tcl_linkchar('tripfile',tripfile,itcl_string,lstring)
c
      call histtcl_init
      my_variable=1
      call tcl_linkvar('my_variable',my_variable,itcl_int)
      DO i=1,10
        outbanknames(i)='junk'
        write(outbos,'(A,I1,A)') 'outbanknames(',i,')'
        call tcl_linkchar(outbos(1:15),outbanknames(i),itcl_string,lstring)
      ENDDO
c
        LmySQL = .FALSE.
        call tcl_linkvar('lmysql',lmysql,itcl_int)
        NmySQL = 0
        call tcl_linkvar('nmysql',nmysql,itcl_int)
        runfile=1
        call tcl_linkvar('runfile',runfile,ITCL_int)
        Lnorm=.TRUE.
        call tcl_linkvar('lnorm',lnorm,ITCL_int)
c
      RETURN
      END
c
c------------------------------------------------------------------------------





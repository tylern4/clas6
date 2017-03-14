c      include file USER_CONTROL
c
c_begin_doc
c  RCS ID string
c  $Id: user_control.inc,v 1.9 2008/03/16 04:33:28 fklein Exp $
c
c  Documentation for include file USER_CONTROL
c
c  Purpose:  holds user control variables
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Nov 21 10:11:57 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file USER_CONTROL:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIUSER_CONTROL
      DATA CIUSER_CONTROL/
     1'$rcsId$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in USER_CONTROL common block
c  --------------------------------------------
c
c_end_var
c  the following is an example of a runtime controlable variable
c  look in user_tcl_init.F to see how to link this variable to a
c  tcl variable
c
       INTEGER MY_VARIABLE, runfile
       CHARACTER*512 outbanknames(10), sql_comment, monhistfile
       CHARACTER*132 eventtagfile, hist21file
       CHARACTER*132 normfile1,normfile2,normfile3,normfile4,tripfile
       LOGICAL lmysql,lnorm
       integer LHM,LHST21, LET, Nmysql, ngood_evt
       REAL Ebeam
       common/user_control/MY_VARIABLE,outbanknames,Ebeam,lmysql,lnorm,ngood_evt,
     + runfile,nmysql,monhistfile,eventtagfile,LHM, LET, LHST21, hist21file, 
     + normfile1,normfile2,normfile3,normfile4,tripfile
c----6----------------------------------------------------------------72






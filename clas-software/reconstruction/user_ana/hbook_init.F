      SUBROUTINE HBOOK_INIT(histfile,lhst,hmem)
c
c_begin_doc
c  RCS ID string
c  $Id: hbook_init.F,v 1.5 2008/03/16 04:33:28 fklein Exp $
c
c  Documentation for subroutine HBOOK_INIT
c
c  Purpose:  initialize HBOOK and shared hmem
c  --------
c
c  Input Parameters:  histfile: c*256 : name of output file blank if none
c  --------------     lhst    : I     : logical unit for output file 0 if none
c                     hmem  : c*4   : name of associated shared hmem
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
c  Author:   Arne Freyberger      Created:  Thu Sep 12 08:59:43 EDT 1996
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
#include "user_control.inc"
#include "histtcl.inc"
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
      PARAMETER (CFILE = '$RCSfile: hbook_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:28 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: hbook_init.F,v 1.5 2008/03/16 04:33:28 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*200  CRMESS
      PARAMETER (CRNAME='HBOOK_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer lenocc
      external lenocc

      CHARACTER*(*) histfile
      character*4   hmem
      INTEGER  lhst, istat

c
c  PAW variables
c
      INTEGER HBOOK_MAX
      PARAMETER( HBOOK_MAX = 12800000 )
      REAL H
      COMMON /PAWC/H(HBOOK_MAX)
      INTEGER IQUEST, iret
      COMMON/QUEST/IQUEST(100)
c_end_var
c
c  executable code for routine HBOOK_INIT:
c----6----------------------------------------------------------------72
c
c  call HLIMIT to set the limits for the PAWC/HBOOK
c  
C  DON'T even tried schared memory with this size PAWC!!!!!!!!
c


       CALL HLIMIT(HBOOK_MAX)
c
      iquest(10) = 65000
      call hropen(lhst,'LUN1',histfile(1:lenocc(histfile)),'NQ',8191,IRET)
      call hbset('BSIZE',8176,IRET)
      
      if(LSEB_NTN_DO) then
         call reslun('hbook_small', lhst21, istat)
         if (istat .eq. 1) then
            call hropen(lhst21,'LUN3',hist21file(1:lenocc(hist21file)),'NQ',4096,IRET)
c            call hbset('BSIZE',8176,IRET)
         endif
      endif
c
      if(lmon_hist)then
         call reslun('hbook_init', lhm, istat)
         if (istat .eq. 1) then
            call hropen(lhm,'LUN2',monhistfile(1:lenocc(monhistfile)),'NQ',1024,IRET)
         endif
      endif
      RETURN
      END
c
c------------------------------------------------------------------------------





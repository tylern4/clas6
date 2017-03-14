      SUBROUTINE RENJOB
c
c_begin_doc
c  RCS ID string
c  $Id: renjob.F,v 1.8 2006/01/24 23:05:03 bonus Exp $
c
c  Documentation for subroutine RENJOB
c
c  Purpose:
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:26:49 EST 1995
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
#include "run_control.inc"
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
      PARAMETER ( CFILE = '$RCSfile: renjob.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.8 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2006/01/24 23:05:03 $')
      PARAMETER ( CAUTHO = '$Author: bonus $')
      DATA CRCSID/
     1'$Id: renjob.F,v 1.8 2006/01/24 23:05:03 bonus Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RENJOB')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine RENJOB:
c  -------------------------------------
c
c  do end of tracking stuff
c
      if (ltrk_do) call trk_last
c
      if (lcc_do) call cc_last
c
      IF (LTAGGER_DO) CALL TAG_LAST
c
      if (LREG1_DO) call trk_reg1_last
c
      if (lusr0_do .or. lusr1_do) CALL USER_LAST
c
      if (lgem_do) call gem_erun()
      call hbook_close
c
c  close input and output ET connection
      if (read_et) then
        call close_et(1)
        call ipc_close
      else
        CALL FCLOS
      endif
c
      CALL RESTAT
c
      CALL RECLOS
c
      RETURN
      END
c
c------------------------------------------------------------------------------





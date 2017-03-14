      SUBROUTINE REBGRN
c
c_begin_doc
c  RCS ID string
c  $Id: rebgrn.F,v 1.31 2009/03/31 19:42:16 myurov Exp $
c
c  Documentation for subroutine REBGRN
c
c  Purpose: BEGIN RUN processor, called at the begin of each run
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters: none
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
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:26:09 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c     
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "clasmdl.inc"
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
      PARAMETER ( CFILE = '$RCSfile: rebgrn.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.31 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2009/03/31 19:42:16 $')
      PARAMETER ( CAUTHO = '$Author: myurov $')
      DATA CRCSID/
     1'$Id: rebgrn.F,v 1.31 2009/03/31 19:42:16 myurov Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REBGRN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine REBGRN:
c  -------------------------------------
c

c febuary/march run run number changes in the head bank !!!
      if (nccrun.eq.0) return

c for the online version open histogram file and initialize histograms
c here instead of in rebgjb or recsis_main so we can stop and start
c runs on the fly. gilfoyle 5/15/98

      if (read_et) then
         call hbook_open
         call user_init
      end if
c
c fr mod.
c
      if (LRF_DO) call RF_BRUN(NCCRUN)
c
c
c
      CALL make_runc_bank(NCCRUN)   
	
      call initcl01(NCCRUN)
c tof
      IF (LTOF_DO) THEN
	 CALL SC_BEGIN_RUN(NCCRUN)
         CALL RECMES(CRNAME,'I','TOF/SC package initialized')
      ENDIF
c
c  tracking begin run routines
c
      IF (LTRK_DO) THEN
        CALL DC_BEGIN_RUN(NCCRUN)
      ENDIF
c
c  Forward Calorimeter begin run routines
c
      IF (LEGN_DO) CALL EC_BRUN(NCCRUN)
c
c  Large Angle Calorimeter begin run routines
c
      IF (LEC1_DO) CALL EC1_BRUN(NCCRUN)
c
c  Cerenkov begin run routines
c
      IF (LCC_DO) CALL CC_BRUN(NCCRUN)
c
c
c  IC(DVCS cal.) begin run routines
c
      IF (LIC_DO) CALL IC_BRUN(NCCRUN)
c
c
c  SH(DVCS hodo.) begin run routines SH
c
      IF (LSH_DO) CALL  SH_BRUN(NCCRUN) 
c
c  Tagger begin run routines
c
      IF (LTAGGER_DO) CALL TAG_BRUN(NCCRUN)
c      
c  Start Counter begin run routine     
c 
      if(LST_DO) call ST_BRUN(NCCRUN)
c
c  Call user begin run routine
c
      IF (LUSR0_DO .OR. LUSR1_DO) CALL USER_BRUN(NCCRUN)
c
c  Call builder begin run initialisation
c
      IF (LSEB_DO) CALL BUILDER_BRUN(NCCRUN)      
c
c  Pid initialization
c
      IF (LHBID_DO) THEN
         CALL initTarget(NCCRUN)
         CALL CC01_INIT(NCCRUN)
      endif
c GPID initialization

      IF(LGPID_DO) THEN
         call initgpid(NCCRUN)
      ENDIF
c GEM initialization
      IF(LGEM_DO) THEN
         call GEM_BRUN(NCCRUN)
      ENDIF
c
c  Call the RECSIS begin run stuff  AFTER ALL packages have done their stuff
c
      CALL RECS_BRUN

c  force to write message buffer into logfile
      CALL RECMES(CRNAME,'S',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------


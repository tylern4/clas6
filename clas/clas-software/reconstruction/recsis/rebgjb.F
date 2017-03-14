      SUBROUTINE REBGJB
c
c_begin_doc
c  RCS ID string
c  $Id: rebgjb.F,v 1.12 2006/01/24 23:05:02 bonus Exp $
c
c  Documentation for subroutine REBGJB
c
c  Purpose: BEGIN JOB processor, called ONCE at the beginning of the JOB
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
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:25:56 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c       Nov.1997 -- F.Klein:
c        moved run indexed initialization of TRK params into 'trk_brun.F'
c       Dec.1997 -- J.Manak
c        trk_init call obsolete
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
      PARAMETER ( CFILE = '$RCSfile: rebgjb.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.12 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2006/01/24 23:05:02 $')
      PARAMETER ( CAUTHO = '$Author: bonus $')
      DATA CRCSID/
     1'$Id: rebgjb.F,v 1.12 2006/01/24 23:05:02 bonus Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REBGJB')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer indexa
      external indexa
c_end_var
c
c  executable code for routine REBGJB:
c  -------------------------------------
c 
c  CALL REVINI to fill DBBK bank with RCS info for the RECSIS code
c
      CALL REVINI('RECS',CREVIS,CDATE)
c
c  REJOB_INFO fills DBBK with Job related information (machine/script/who...)
c
c      CALL REJOB_INFO
c
c
c  Call user initialization routine. skip this for the online
c  version. in this case user_init will be called in rebgrn.F 
c  so we can close and open files when runs stop and start and
c  not just at the start of the job. gilfoyle 5/15/98
c
      IF (LUSR0_DO .OR. LUSR1_DO) THEN
         if ( .not. read_et) then
            CALL USER_INIT
            CALL RECMES(CRNAME,'I','USER package initialized')
         end if
      ENDIF
c
      IF (LSEB_DO) THEN
        CALL INIT_BUILDER
        CALL RECMES(CRNAME,'I','Event Builder  package initialized')
      ENDIF
c
      IF (LEGN_DO) THEN
       CALL EC_INIT
       CALL RECMES(CRNAME,'I','EGN package initialized')
      ENDIF
c
      IF (LEC1_DO) THEN
         CALL EC1_INIT
         CALL RECMES(CRNAME,'I','EC1 package initialized')
      ENDIF

c
c STart counter
c
      if (lst_do) then
         call st_init
         call recmes(crname,'I','Start counter package initialized.')
      endif
c
c
      IF (LCC_DO) THEN
         CALL CC_INIT
         CALL RECMES(CRNAME,'I','CC package initialized')
      ENDIF
c
      IF (LTAGGER_DO) THEN
        CALL TAG_INIT
        CALL RECMES(CRNAME,'I','TAG package initialized')
      ENDIF
c
      IF (LREG1_DO) THEN
        CALL TRK_REG1_INIT
        CALL RECMES(CRNAME,'I','REGION1 package initialized')
      ENDIF
c
       IF (LGEM_DO) THEN
        CALL GEM_INIT
        CALL RECMES(CRNAME,'I','BONUS GEM package initialized')
      ENDIF
c
      RETURN
      END
c
c------------------------------------------------------------------------------

      SUBROUTINE EC_EVNT
c     
c     _begin_doc
c     RCS ID string
c     $Id: ec_evnt.F,v 1.8 2008/03/16 04:33:14 fklein Exp $
c     
c     Documentation for subroutine EC_EVNT
c     
c     Purpose: perform the event by event reconstruction of the EGN
c     --------
c     
c     Input Parameters: none
c     ----------------
c     
c     Output Parameters:  none---> BOS bank filled
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   Arne Freyberger      Created:  Tue Nov 28 15:41:35 EST 1995
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
#include "ec_control.inc"
c     BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_evnt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.8 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:14 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1     '$Id: ec_evnt.F,v 1.8 2008/03/16 04:33:14 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_EVNT')
      PARAMETER (CRAUTH='Arne Freyberger')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NSECTOR
      LOGICAL LOK(6)
      logical first
      data first/.TRUE./
c_end_var
c     
c     executable code for routine EC_EVNT:
c---- 6----------------------------------------------------------------72
c     
      CALL EC_INI_BEFOR         !cstep initialization
      CALL EC_STORE_BOS(LOK)
      DO 10 NSECTOR=1,6
         IF (LOK(NSECTOR)) THEN
            CALL ECCAL_STRIP(NSECTOR)
            CALL ECFIT_ANALYZE(NSECTOR)
            CALL HIT_MATCH(NSECTOR)
         ENDIF
 10   CONTINUE
      CALL BDROP(IW,'ECHBECPIECPOECPC')
      CALL EC_FILL_BOS(LOK)
c
      RETURN
      END
c
c------------------------------------------------------------------------------

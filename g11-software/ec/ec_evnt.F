      SUBROUTINE EC_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: ec_evnt.F,v 1.6 2000/12/08 20:01:22 stepanya Exp $
c
c  Documentation for subroutine EC_EVNT
c
c  Purpose: perform the event by event reconstruction of the EGN
c  --------
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters:  none---> BOS bank filled
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Nov 28 15:41:35 EST 1995
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
#include "ec_control.inc"
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
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
      PARAMETER (CFILE = '$RCSfile: ec_evnt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.6 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/12/08 20:01:22 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ec_evnt.F,v 1.6 2000/12/08 20:01:22 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_EVNT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NSECTOR
      LOGICAL LOK(6)
      real threshold(3)
      integer option(4)
      logical first
      data first/.TRUE./
c_end_var
c
c  executable code for routine EC_EVNT:
c----6----------------------------------------------------------------72
c
      IF(anal_opt.EQ.1) THEN

        IF(first) THEN
          print *,'EC: sending parameters ...'
          threshold(1) = EcFitEdge_strip_threshold
          threshold(2) = EcFitEdge_peak_threshold
          threshold(3) = EcFitEdge_hit_threshold
          option(1) = 0
          option(2) = 0
          option(3) = 0
          option(4) = 0
          if(def_ADC) option(1) = 1
          if(def_TDC) option(2) = 1
          if(def_geom) option(3) = 1
          if(def_atten) option(4) = 1
          first = .FALSE.
        ENDIF

        call ecmain_c(iw,threshold,option)
ccc        CALL ECMAIN_F(iw,threshold,option)

      ELSE

        CALL EC_INI_BEFOR         !cstep initialization
        CALL EC_STORE_BOS(LOK)
        DO 10 NSECTOR=1,6
          IF (LOK(NSECTOR)) THEN
            CALL ECCAL_STRIP(NSECTOR)
            CALL ECFIT_ANALYZE(NSECTOR)
            CALL HIT_MATCH(NSECTOR)
          ENDIF
10      CONTINUE
        CALL BDROP(IW,'ECHBECPIECPOECPC')
        CALL EC_FILL_BOS(LOK)

      ENDIF

      RETURN
      END
c
c------------------------------------------------------------------------------

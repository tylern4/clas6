      SUBROUTINE TRK_p2p(isec,plin,plout,p9in,itrk)
c
c_begin_doc
c  RCS ID string
c  $Id: trk_p2p.F,v 1.3 2008/03/16 04:33:27 fklein Exp $
c
c  Documentation for subroutine TRK_p2p
c
c  Purpose:
c  --------
c
c  Input Parameters:  isec     - treck sector number
c  ----------------   plin     - detector plane from where track starts
c                     plout    - detector plane where track ends
c                     p9in     - input {x,y,z,Px/P,Py/P,Pz/P,P,m,Q} at the vert. 
c                     uses only P & Q  
c                     itrk     - linked track ID
c                     sda_trp  - on the plane plin
c
c  Output Parameters: p9in     - changed if plout=1
c  -----------------  sda_trp  - on the plane plout
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Tue Oct 27 12:33:03 EST 1998
c  -------
c
c  Major revisions: F.Klein March 99: change plane definition to get
c  ----------------                   a simpler check whether plane
c                                     was crossed
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "trktcl_var.inc"
#include "sdaswim.inc"
#include "anapatt.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER isec,plin,plout,itrk
      REAL p9in(9)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_p2p.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:27 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: trk_p2p.F,v 1.3 2008/03/16 04:33:27 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_p2p')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      REAL  vmod
      INTEGER NWRITE,i,ist,nstep,ptg_flag,iflag
      REAL vin(9),vout(9),Step,Field(3),xint(9)
      REAL pi, r2deg, tot_length,diff_l, b_old, Sint
      DATA pi,r2deg /3.14159265,57.29578/
      REAL step_min, step_max, defl_angle
      PARAMETER (step_min = 0.1)
      PARAMETER (step_max = 5.)
      PARAMETER (defl_angle = 0.001)
c_end_var
c
c  executable code for routine TRK_p2p:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine filling dcst structure.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c-
      IF(plin.EQ.plout) return
      DO i = 1,3
         vin(i) = sda_trp(i,plin)
         vin(i+3) = sda_trp(i+3,plin)
         vin(i+6) = p9in(i+6)
      ENDDO
      IF(plin.GT.plout)THEN
        DO i = 4,6
          vin(i) = -vin(i)
        ENDDO
        vin(9) = -p9in(9)
      ENDIF
c- 
      Nstep = 10000
      ptg_flag = 0
      tot_length = 0.

c- Get B-field in order to derive the first step size
      step = 0.01
      CALL sda_helix(step,vin, vout,field,ptg_flag,isec)
c- 
      DO ist = 1,nstep

c- Calculate a step size
        IF(vin(9).NE.0.) THEN
          B_old = VMOD(field,3)
          IF(B_old.GT.0.05) THEN
            step = defl_angle*vout(7)/B_old
            IF(step.LT.step_min) then
               step = step_min
            ELSEIF(step.GT.step_max) then
               step = step_max
            ENDIF
          ELSE
            step = step_max
          ENDIF
        ELSE
          step = 10.
        ENDIF
        CALL sda_helix(Step,vin,vout,field,ptg_flag)
        Sint = step
        CALL sda_ipln(isec,vin,vout,plout,Sint,xint,iflag)
        If(iflag.NE.0)Then
          tot_length = tot_length + Sint
            do i=1,3
              sda_trp(i,plout) = xint(i)
              sda_trp(7+i,plout) = Field(i)
            enddo 
          if(plin.gt.plout)then
            do i=4,6
              sda_trp(i,plout) = -xint(i)
            enddo
            if(plout.eq.1) then
              do i = 1,6
                p9in(i) = sda_trp(i,plout)
              enddo
            endif
            diff_l = tot_length - (sda_trp(7,plin)-sda_trp(7,plout)) 
            do i=plin,npln
               if(sda_trp(7,i).lt.999.) then
                  sda_trp(7,i)=sda_trp(7,i)+diff_l
               endif
            enddo
          elseif(plin.lt.plout)then
            do i = 4,6
              sda_trp(i,plout) = xint(i)
            enddo
            diff_l = tot_length - (sda_trp(7,plout)-sda_trp(7,plin)) 
            do i=plout,npln
               if(sda_trp(7,i).lt.999.) then
                  sda_trp(7,i)=sda_trp(7,i)+diff_l
               endif
            enddo
          endif
          RETURN
        ENDIF
        tot_length = tot_length + Step
        Do i = 1,9
           vin(i) = vout(i)
        EndDo
      ENDDO
c- 
      RETURN
      END
c
c------------------------------------------------------------------------------





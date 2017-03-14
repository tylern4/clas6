      SUBROUTINE CC_SCH
c
c_begin_doc
c  RCS ID string
c  $Id: cc_sch.F,v 1.5 2000/12/13 22:38:21 fklein Exp $
c
c  Documentation for subroutine CC_SCH
c
c  Purpose:    To extract data from SCRC bank for inter-package analysis
c  --------
c
c  Input Parameters:   NONE (Info from BOS - banks)
c  ----------------
c
c  Output Parameters:  NONE (ccschb.inc is filled)
c  -----------------
c
c  Author:   Alexander Vlassov    Created:  Fri Aug 16 10:26:41 EDT 1996
c  -------
c
c  Major revisions: Alexander Vlassov 17 April 1998 :
c  ----------------                  New bank for tof (SCRC)
c
c
c                  K.Mikhailov 12 June 1998(New structure of SCRC)
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "ccschb.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_sch.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/12/13 22:38:21 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: cc_sch.F,v 1.5 2000/12/13 22:38:21 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_SCH')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IFIRST,NCOL,NROW,isect
      INTEGER ind, i, k, name_i
      INTEGER MAMIND, mlink
      INTEGER rec_irun,rec_event,rec_trig
      DATA IFIRST/0/
c
c_end_var
c
c  executable code for routine CC_SCH:
c----6----------------------------------------------------------------72
c
      IF(IFIRST.eq.0) then
        name_i = MAMIND(IW,'SCRC')
        if(name_i.eq.0) then
          write(CRMESS,*) ' NO SCRC - bank registered' 
          CALL RECMES(CRNAME,'E',CRMESS)
          return
        end if
        IFIRST = 1
      END IF
c
      jsctrk = 0
c
      do isect = 1,6         ! Sector loop
c
c        ind = MLINK(iw,'SCRC',isect)
        ind = MLINK(iw,'SCR ',isect)
        if(ind.ne.0) then    ! Sector
c
          NCOL  = iw(ind - 5)
          NROW  = iw(ind - 4)
c
c
            if(NROW .gt. msctrk) then
c     ========> too many tracks in SC. Skip the sector and take next.
c
               write(CRMESS,*) 'Too many tracks in SC  !!!',NROW,' >',  msctrk,
     *              ' NCOL,NROW', NCOL,NROW
               CALL RECMES(CRNAME,'W',CRMESS)
c
               go to 5         ! too many tracks
            endif
c
          do i = 1,NROW      ! Nrow loop inside the sector
            k = ind + NCOL*(i-1)
            jsctrk = jsctrk + 1
            jscsec(jsctrk) = isect
            jscpad(jsctrk) = iw(k + 1)     ! cluster id
            jscstat(jsctrk) = iw(k + 10)   ! 12)  status word defined in sc.h
c
            sc_e(jsctrk)     = rw(k + 2)   ! cluster Energy (MeV)
            sc_de(jsctrk)    = rw(k + 11)  ! 3)  error in cluster energy (ns)
            sc_time(jsctrk)  = rw(k + 3)   ! 4)  cluster (energy-weighted) time(ns)
            sc_dtime(jsctrk) = rw(k + 12)  ! 5)  error in cluster time (ns)
            sc_r(1,jsctrk)   = rw(k + 4)   ! 6)  x position in sector coordinate system
            sc_r(2,jsctrk)   = rw(k + 5)   ! 7)  y position in sector coordinate system
            sc_r(3,jsctrk)   = rw(k + 6)   ! 8)  z position in sector coordinate system
            sc_dr(1,jsctrk)  = rw(k + 7)   ! 9)  x error in sector coordinate system
            sc_dr(2,jsctrk)  = rw(k + 8)   ! 10)  y error in sector coordinate system
            sc_dr(3,jsctrk)  = rw(k + 9)   ! 11)  z error in sector coordinate system
          end do             ! Nrow loop inside the sector
        end if               ! Sector
 5      CONTINUE
      end do                 ! Sector loop
c
  100 continue
c     ========> some other actions
c
      RETURN
      END





      SUBROUTINE CC_TBT
c
c_begin_doc
c  RCS ID string
c  $Id: cc_tbt.F,v 1.2 1999/03/09 18:30:54 boiarino Exp $
c
c  Documentation for subroutine CC_TBT
c
c  Purpose:   Extract data from TBTR bank for inter-package analysis
c  --------
c
c  Input Parameters:   NONE (Info from BOS - banks)
c  ----------------
c
c  Output Parameters:  NONE (cctbtb.inc is filled)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  Alexander Vlassov  Created:  Fri Aug  2 10:42:12 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c    Alexander Vlassov  5 December 1997
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
c
#include "bcs.inc"
#include "cctbtb.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_tbt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 18:30:54 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: cc_tbt.F,v 1.2 1999/03/09 18:30:54 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*256  CRMESS
      PARAMETER (CRNAME='CC_TBT')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IFIRST,NCOL,NROW, NCOL1
      INTEGER ind, i, k, name_i, isect, itrkn, name_h, ind_h
      integer irun, ievent, itrig, indn
      integer MAMIND,MLINK
      data IFIRST /0/
c
c_end_var
c
c  executable code for routine CC_TBT:
c----6-------------------------------------------------------------72
c
      call rernev(irun,ievent,itrig)
      IF(IFIRST.eq.0) then
        name_h = MAMIND(IW,'TDPL')
        if(name_h.eq.0) then
          write(CRMESS,*) ' NO TDPL - bank registered' 
          CALL RECMES(CRNAME,'E',CRMESS)
          return
        end if
        name_i = MAMIND(IW,'TBTR')
        if(name_i.eq.0) then
          write(CRMESS,*) ' NO TBTR - bank registered' 
          CALL RECMES(CRNAME,'E',CRMESS)
          return
        end if
        IFIRST = 1
      END IF
      ind   = iw(name_i)
      ind_h = iw(name_h)
c
      jtbtrk = 0
c
    1 CONTINUE   ! Cycle on "hits" in TBTR
c
      IF(ind.NE.0) THEN
c
        NCOL  = iw(ind - 5)
        NROW  = iw(ind - 4)
c
        do i = 1,NROW     ! TBTR loop
c
          k = ind + NCOL*(i-1)
          if(i.ge. mtbtrk) go to 10  ! too many tracks
c
          jtbtrk = jtbtrk + 1
c
          isect = int( 0.01 * iw(k + 9) )
          itrkn = mod( iw(k + 9),100 )
c
          jtbsec(jtbtrk)     = isect
          if(isect.le.0 .or. isect.ge.7) go to 11
c
          tb_vert(1,jtbtrk)  = rw(k +  1)
          tb_vert(2,jtbtrk)  = rw(k +  2)
          tb_vert(3,jtbtrk)  = rw(k +  3)
          tb_mom(1,jtbtrk)   = rw(k +  4)
          tb_mom(2,jtbtrk)   = rw(k +  5)
          tb_mom(3,jtbtrk)   = rw(k +  6)
          tb_charge(jtbtrk)  = rw(k +  7)
c
c Seek for the corresponding points in HDPL bank
c
          ind_h  = MLINK(iw,'TDPL',isect)
          ncol1  = iw(ind_h - 5)
c
          tb_ccl(jtbtrk) = 0.
          tb_scl(jtbtrk) = 0.
          tb_ecl(jtbtrk) = 0.
c
c   Cerenkov plane
c
          indn = ind_h + (itrkn - 1)*10*ncol1 + (4-1)*ncol1
          if(mod(iw(indn + 1),100) .ne.40 ) then
            print *, ' Wrong number for Cerenkov plane !'
            print *,' indn =',indn
            print *, iw(indn + 1),rw(indn+2),rw(indn+3),rw(indn+4),
     &      rw(indn+5),rw(indn+6),rw(indn+7),rw(indn+8)
            return
          else
c
            tb_ccpnt(1,jtbtrk) = rw(indn + 2)
            tb_ccpnt(2,jtbtrk) = rw(indn + 3)
            tb_ccpnt(3,jtbtrk) = rw(indn + 4)
            tb_ccdir(1,jtbtrk) = rw(indn + 5)
            tb_ccdir(2,jtbtrk) = rw(indn + 6)
            tb_ccdir(3,jtbtrk) = rw(indn + 7)
            tb_ccl(jtbtrk)     = rw(indn + 8)
c
          end if
c
c   TOF plane
c
          indn = ind_h + (itrkn - 1)*10*ncol1 + (5-1)*ncol1
          if(mod(iw(indn + 1),100) .ne.41 ) then
            print *, ' Wrong number for TOF plane !'
            print *,' indn =',indn
            print *, iw(indn + 1),rw(indn+2),rw(indn+3),rw(indn+4),
     &      rw(indn+5),rw(indn+6),rw(indn+7),rw(indn+8)
            return
          else
c
            tb_scpnt(1,jtbtrk) = rw(indn + 2)
            tb_scpnt(2,jtbtrk) = rw(indn + 3)
            tb_scpnt(3,jtbtrk) = rw(indn + 4)
            tb_scdir(1,jtbtrk) = rw(indn + 5)
            tb_scdir(2,jtbtrk) = rw(indn + 6)
            tb_scdir(3,jtbtrk) = rw(indn + 7)
            tb_scl(jtbtrk)     = rw(indn + 8)
c
          end if
c
c
c   EC  plane
c
          indn = ind_h + (itrkn - 1)*10*ncol1 + (9-1)*ncol1
          if(mod(iw(indn + 1),100) .ne.45 ) then
            print *, ' Wrong number for TOF plane !'
            print *,' indn =',indn
            print *, iw(indn + 1),rw(indn+2),rw(indn+3),rw(indn+4),
     &      rw(indn+5),rw(indn+6),rw(indn+7),rw(indn+8)
            return
          else
c
            tb_ecpnt(1,jtbtrk) = rw(indn + 2)
            tb_ecpnt(2,jtbtrk) = rw(indn + 3)
            tb_ecpnt(3,jtbtrk) = rw(indn + 4)
            tb_ecdir(1,jtbtrk) = rw(indn + 5)
            tb_ecdir(2,jtbtrk) = rw(indn + 6)
            tb_ecdir(3,jtbtrk) = rw(indn + 7)
            tb_ecl(jtbtrk)     = rw(indn + 8)
c
          end if
c
        end do      ! TBTR loop
c
      ELSE
        IFIRST = 0
        go to 100
c
      END IF
      ind = iw(ind-1)
      go to 1
c
c
   10 continue
c     ========> too many tracks in TBTR
c
      write(CRMESS,*) 'Too many tracks in TBTR  !!!',NROW,
     * ' >', mtbtrk,'  NCOL,NROW', NCOL,NROW
      CALL RECMES(CRNAME,'W',CRMESS)
c
      return
c
   11 continue
c     ========> Wrong sector number in TBTR
c
      write(CRMESS,*) 'Wrong Sector number in TBTR !!!',NROW,
     * ' >', mtbtrk, '  NCOL,NROW', NCOL,NROW
      CALL RECMES(CRNAME,'W',CRMESS)
c
      return
c
  100 continue
c     ========> some other actions
c
      RETURN
      END
c





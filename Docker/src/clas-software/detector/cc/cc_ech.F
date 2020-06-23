      SUBROUTINE CC_ECH
c
c_begin_doc
c  RCS ID string
c  $Id: cc_ech.F,v 1.8 1999/03/09 18:30:51 boiarino Exp $
c
c  Documentation for subroutine CC_ECH
c
c  Purpose:   Extract data from ECHB bank for inter-package analysis
c  --------
c
c  Input Parameters:   NONE (Info from BOS - banks)
c  ----------------
c
c  Output Parameters:  NONE (ccechb.inc is filled)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov    Created:  Fri Aug  2 10:42:12 EDT 1996
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
c
#include "bcs.inc"
#include "ccechb.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_ech.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.8 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 18:30:51 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: cc_ech.F,v 1.8 1999/03/09 18:30:51 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*256  CRMESS
      PARAMETER (CRNAME='CC_ECH')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IFIRST,NCOL,NROW
      INTEGER ind, i, k, name_i
      integer irun,ievent,itrig
      integer MAMIND
      data IFIRST /0/
c
c_end_var
c
c  executable code for routine CC_ECH:
c----6----------------------------------------------------------------72
c
      call rernev(irun,ievent,itrig)
      IF(IFIRST.eq.0) then
        name_i = MAMIND(iw,'ECHB')
        if(name_i.eq.0) then
          write(CRMESS,*) ' NO ECHB - bank registered' 
          CALL RECMES(CRNAME,'E',CRMESS)
          return
        end if
        IFIRST = 1
      END IF
      ind = iw(name_i)
c
      jectrk = 0
c
    1 CONTINUE   ! Cycle on "hits" in ECHB
c
      IF(ind.NE.0) THEN
c
        NCOL  = iw(ind - 5)
        NROW  = iw(ind - 4)
c
        do i = 1,NROW
c
          k = ind + NCOL*(i-1)
          if(i.ge. mectrk) go to 10  ! too many tracks
c
          jectrk = jectrk + 1
          jecsec(jectrk) = int(iw(k + 1)/100)
          jeclay(jectrk) = mod(iw(k + 1),100)
c
          ec_e(jectrk)     = rw(k + 2)
          ec_de(jectrk)    = rw(k + 3)
          ec_time(jectrk)  = rw(k + 4)
          ec_dtime(jectrk) = rw(k + 5)
          ec_ihit(jectrk)  = rw(k + 6)
          ec_dihit(jectrk) = rw(k + 8)
          ec_r(1,jectrk)   = rw(k +10)
          ec_r(2,jectrk)   = rw(k +11)
          ec_r(3,jectrk)   = rw(k +12)
          ec_dr(1,jectrk)  = rw(k +13)
          ec_dr(2,jectrk)  = rw(k +14)
          ec_dr(3,jectrk)  = rw(k +15)
c
        end do
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
c     ========> too many tracks in EC
c
      write(CRMESS,*) 'Too many tracks in EC  !!!',NROW,' >',  mectrk,
     * ' NCOL,NROW', NCOL,NROW
      CALL RECMES(CRNAME,'W',CRMESS)
c
  100 continue
c     ========> some other actions
      RETURN
      END
c





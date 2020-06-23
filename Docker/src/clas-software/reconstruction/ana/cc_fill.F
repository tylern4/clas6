      SUBROUTINE CC_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: cc_fill.F,v 1.7 1999/06/09 19:35:31 stepanya Exp $
c
c  Documentation for subroutine CC_FILL
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov    Created:  Fri Jun 13 17:00:00 EDT 1997
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
#include "bcs.inc"
#include "cc_revt.inc"
#include "cccalb.inc"
#include "ccechb.inc"
#include "ccschb.inc"
#include "cc_rc.inc"
#include "eidcontr.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/06/09 19:35:31 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: cc_fill.F,v 1.7 1999/06/09 19:35:31 stepanya Exp $'   
     2/   
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_FILL')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c
c
      integer ievent,irun,itrig,i,k,j
      integer ncol,nrow,nsec,nhis,ndat_hbt
      real s,a
      integer name_i,ind,nscs,ltrk,ni_trk,ind_trk
      logical pru
*
      integer MAMIND 
      integer np,nelectr,ifirst
      data ifirst /0/
c_end_var
c
c=============================================
c
c  executable code for routine CC_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling CC, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c  First change PAWC directories
      call hcdir('//PAWC/CC',' ')

c  Do your HFILLS below this line
      IF(IFIRST.eq.0) then
        name_i = MAMIND(iw,'EID0')
        if(name_i.eq.0) then
          write(CRMESS,*) ' NO EID0 - bank registered !!!' 
          CALL RECMES(CRNAME,'E',CRMESS)
          return
        end if
        ni_trk = MAMIND(iw,'HBTR')
        if(ni_trk.eq.0 ) then
          CRMESS=' NO HBTR - bank registered !!!'
          CALL RECMES(CRNAME,'E',CRMESS)
          return
        end if
c
        IFIRST = 1
      END IF
c
c=============================================
c
      call hcdir('//PAWC/CC',' ')
      ind = iw(name_i)
c
c    CC raw data information
c
      if( n_hits_CC.ge.1 ) then
c
        do i = 1,n_hits_CC
          NHIS = 100 * n_SECT_CC(i)    ! Sector number
          nsec = n_SECT_CC(i)
          k = n_CC_hit(i)
          if(nsec.eq.6 .and. k.ge.1 .and. k.le.36) then
            s = n_SECT_CC(i)
            call hf1(1001,s,1.)
c ==> 1001
            a = n_CC_hit(i)
            s = I_ADC_CC (i)
            call hf2(NHIS + 1,s,a,1.)
c ==>    1
            s = I_ADC_CC (i) - ccpedes(nsec,k)
            call hf2(NHIS + 2,s,a,1.)
c ==>    2
            call hf2(NHIS + 7,s,a,1.)
c ==>    7
            s = I_TDC_CC (i)
            call hf2(NHIS + 11,s,a,1.)
c ==>   11
c
          end if
        end do
      end if
c
c    CC reconstructed information
c
      if( nrclust.ge.1 ) then
c
        do i = 1,nrclust
c       ================
          NHIS = 100 * nrsect(i)   ! Sector number
c
          s = nrsect(i)
          call hf1(1002,s,1.)
c ==> 1002
          a = 0.1 * nrsegm(i)
          s = 0.1 * nrphe(i)
          call hf2(NHIS + 5,s,a,1.)
c ==>  5
        end do
      end if     
c
c=============================================
c
c    CC eid0 information
c
c=============================================
c
      call eid0(nelectr)
      a = nelectr
      call hf1(1003,s,a)
c ==> 1003
      if(nelectr.ge.1) then
        s = ievent
        call hf1(1005,s,1.)
c ==> 1005
      end if
c
      do i = 1,nelectr
c     ================
c
        NHIS = 100 * jeidsec(i) 
c
        s = jeidsec(i)
        call hf2(21,a,s,1.)
c ==> 21
c
      end do
c
c initial particles ...
c
      np=0
c     Some play with HBTR banks :       (sorry, I don't understand that play
c     ===========================        quite well, but I had to change the
c                                        track Bank  (F.Klein 11.97))
      ind_trk = iw(ni_trk)
      NCOL    = iw(ind_trk - 5)
      NROW    = iw(ind_trk - 4)
      Ndat_hbt= iw(ind_trk)
c      if(ind_trk.ne.0) then
c        print *, ' EVENT with TRACK :',irun,ievent,itrig
c      end if
c
      if(nrclust.ge.1) then
        s = ievent
        call hf1(1004,s,1.)
c ==> 1004
        do i = 1,nrclust
c       ================
c
          NHIS = 100 * nrsect(i)   ! Sector number
          if(ind_trk.ne.0) then
            do j = 0,Ndat_hbt-1,NCOL
              if(iw(ind_trk+8+j)/100.EQ.nrsect(i)) then    !get Sector# of track
                if(rw(ind_trk + 7+j).LT.0.) then	!get charge
                  s = 0.1*nrsegm(i)
                  a = 0.1*nrphe(i)
                  call hf2(NHIS + 3, a, s,1.0)
c ==>  3

                else
                  s = 0.1*nrsegm(i)
                  a = 0.1*nrphe(i)
                  call hf2(NHIS + 4, a, s,1.0)
c ==>  4
                endif
               endif
             enddo
          end if
        end do
      end if
c
      if(nelectr.eq.1 .and. ind_trk.ne.0) then
c
        nscs = jeidsec(1)  ! sector #
c
        ltrk = 0
        do i = 1,NROW
c
          if(iw(ind_trk + 7).LT.0.) then
            ltrk = 1
            s = ievent
            call hf1(1006,s,1.)
c ==> 1006
          end if
        end do
c
      end if
c
c     END of play with TRKL banks :
c     =============================
      call hcdir('//PAWC',' ') !   Return to the initial directory
c
      RETURN
      END
c
c------------------------------------------------------------------------------





      SUBROUTINE CC_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: cc_evnt.F,v 1.9 2002/02/09 03:03:07 stepanya Exp $
c
c  Documentation for subroutine CC_EVNT
c
c  Purpose:
c  --------
c
c  Input Parameters:   Raw data from "cc_revt.inc" 
c  -----------------     (filled by cc_bevt.F)
c 
c
c  Output Parameters:  Reconstructon parameters both in
c  -----------------   common block CC_RC ("cc_rc.inc" file)
c                      and filling BOS bank CCRC
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov     Created:  Fri Jun 21 18:23:45 EDT 1996
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
#include "cc_rc.inc"
#include "cc_hrb.inc"
#include "cccalb.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_evnt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.9 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/02/09 03:03:07 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: cc_evnt.F,v 1.9 2002/02/09 03:03:07 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*200  CRMESS
      PARAMETER (CRNAME='CC_EVNT')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,NWARN
      integer i,j,NCC,INCC,nphe,iped,KCC, nch_nsec
      real a_cal,ANCC,s,Ee,theta,dtheta,riec,rdiec,rxec,drxec
      real thet_cc(18), time2ch
c
      data Ee /1.0/
      data thet_cc /  7.72,  9.44, 11.17, 12.93, 14.69, 16.47, 18.29,
     * 20.15, 22.03, 24.00, 26.04, 28.18, 30.48, 32.94, 35.44, 37.97,
     * 40.51, 43.11/
      data NWRITE,NWARN /0,0/, time2ch /0.05/, nch_nsec /20/ 
c_end_var
c
c  executable code for routine CC_EVNT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 2
        CRMESS='CC_EVNT is called!'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c     BODY :
c
      nrclust = 0
      call bdrop(IW,'CCRC')
      call vzero(nrstat,maxcrec)
      call vzero(nrsect,maxcrec)
      call vzero(nrsegm,maxcrec)
      call vzero( nrphe,maxcrec)
      call vzero(nrtime,maxcrec)
      call vzero(nrthet,maxcrec)
      call vzero( nrphy,maxcrec)
c  ++++   Test of the GOOD event  ++++
      do i = 1, n_hits_CC
        if(n_SECT_CC(i).le.0 .or. n_SECT_CC(i).ge.7) return 
        if(n_CC_hit(i).le.0 .or. n_CC_hit(i).ge.37) return 
      end do
c
      do 100 i = 1, n_hits_CC
c
        nphe = 0
        a_cal = opheamp( n_SECT_CC(i),n_CC_hit(i) )
        iped  = ccpedes( n_SECT_CC(i),n_CC_hit(i) ) + 0.5
        time2ch  = xcct2ch(n_SECT_CC(i),n_CC_hit(i)) 
        if(a_cal.lt.1.) then
          if(NWARN.lt.20) then
            write(CRMESS,*) 'WRONG calibration parameters for sector',
     &      n_SECT_CC(i),'   Segment :',n_CC_hit(i),'  SPE peack :',a_cal
            CALL RECMES(CRNAME,'W',CRMESS)
            NWARN = NWARN + 1
          end if
          a_cal = 10000.
        end if
c
        nphe  = 10 * (I_ADC_CC(i) - iped)/ a_cal + 0.5
        if(nphe.lt.1) go to 100
c
        KCC =  n_CC_hit(i)
        NCC =  (KCC+1)/2
        INCC = 1 - 2*mod(n_CC_hit(i),2)
        if(nrclust.ge.maxcrec) then
          print *, ' MORE then ',maxcrec,
     *    ' HITS during reconstruction !!!'
          go to 100                     ! Enough hits ?
        end if
c
        if(nrclust.eq.0) then
c
          nrclust = 1
          nrsect(nrclust) = n_SECT_CC(i)
          nrsegm(nrclust) = 10 * NCC
          nrsegm_p(nrclust) =10 * NCC  
          nrsegm_m(nrclust) =10 * NCC  
c
          nrphe (nrclust) = nphe
c            
c     *** Time correction ***
          nrtime(nrclust) = 0.5 + nch_nsec *
     &    ( time2ch*I_TDC_CC(i) + acctime(nrsect(nrclust),KCC) )
c     *** Time correction ***
c
          nrphy (nrclust) = INCC
          ANCC = 0.1 * nrsegm(nrclust)
          call cc_ncc_to_thet(ANCC,Ee,theta,dtheta)          
          nrthet(nrclust) = theta + 0.5
          nrdthet(nrclust) = dtheta + 0.5 
          call cc_ncc_to_iec (ANCC,Ee,riec,rdiec)         
          nriec(nrclust) = riec + 0.5
          nrdiec(nrclust) = rdiec + 0.5
          call cc_n2xec(ANCC,rxec,drxec) 
          nrxec(nrclust) = rxec + 0.5
          nrdxec(nrclust) = drxec + 0.5
c
        else
c
          do j = 1, nrclust
c
            
            if(nrsect(j) .eq. n_SECT_CC(i) .and.
     &      abs(nrsegm(j) - 10 * NCC) .le. isgmdiff ) then
c
c   ====>   same cluster
c
              s = ( nrphe(j) * nrsegm(j) + nphe *
     *        10 * NCC ) / (nrphe(j) + nphe) + 0.5
              nrsegm(j) = s     ! weighted CC number
              if(10*NCC.ge.nrsegm_p(j)) nrsegm_p(j) = 10*NCC
              if(10*NCC.le.nrsegm_m(j)) nrsegm_m(j) = 10*NCC
c
              nrphe(j) = nrphe(j) + nphe
              if(I_TDC_CC(i).le.nrtime(j)) then
c     *** Time correction ***
                nrtime(j) = 0.5 + nch_nsec *
     &          ( time2ch*I_TDC_CC(i) + acctime(nrsect(nrclust),KCC) )
c     *** Time correction ***
              end if
c
              if(nrphy (j).ne.INCC) then 
                nrphy (j) = 0
              end if
c
              ANCC = 0.1*nrsegm(j)
              call cc_ncc_to_thet(ANCC,Ee,theta,dtheta)          
              nrthet(j) = theta + 0.5
              nrdthet(j) = dtheta + 0.5 
              call cc_ncc_to_iec (ANCC,Ee,riec,rdiec)         
              nriec(j) = riec + 0.5
              nrdiec(j) = rdiec + 0.5 
              call cc_n2xec(ANCC,rxec,drxec) 
              nrxec(j) = rxec + 0.5
              nrdxec(j) = drxec + 0.5
c
              go to 100
            end if 
c
          end do
c
c ======> new cluster
c
          nrclust = nrclust + 1
          nrsect(nrclust)   = n_SECT_CC(i)
          nrsegm(nrclust)   = 10 * NCC
          nrsegm_p(nrclust) = 10 * NCC  
          nrsegm_m(nrclust) = 10 * NCC
c  
          nrphe (nrclust) = nphe
c            
c            
c     *** Time correction ***
          nrtime(nrclust) = 0.5 + nch_nsec *
     &    ( time2ch*I_TDC_CC(i) + acctime(nrsect(nrclust),KCC) )
c     *** Time correction ***
c
          ANCC = 0.1 * nrsegm(nrclust)
          call cc_ncc_to_thet(ANCC,Ee,theta,dtheta)
          nrthet(nrclust) = theta + 0.5
          nrdthet(nrclust) = dtheta + 0.5 
          call cc_ncc_to_iec (ANCC,Ee,riec,rdiec)         
          nriec(nrclust) = riec + 0.5
          nrdiec(nrclust) = rdiec + 0.5 
          call cc_n2xec(ANCC,rxec,drxec) 
          nrxec(nrclust) = rxec + 0.5
          nrdxec(nrclust) = drxec + 0.5
          nrphy (nrclust) = INCC
c
        end if
c
  100 continue
c
c   ========  *** NO *** Event histogramming  ========
c
      if(nrclust.ge.1) then
c
        call cc_bos_fill
c       ================>  filling BOS bank CCRC
c
      end if
c
      RETURN
      END
c
c-----------------------------------------------------------------------





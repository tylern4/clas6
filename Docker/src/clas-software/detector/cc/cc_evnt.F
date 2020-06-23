      SUBROUTINE CC_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: cc_evnt.F,v 1.12 2007/12/04 21:29:42 vlassov Exp $
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
      PARAMETER (CREVIS = '$Revision: 1.12 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2007/12/04 21:29:42 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_evnt.F,v 1.12 2007/12/04 21:29:42 vlassov Exp $'   
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
      integer i,j,NCC,INCC,nphe,iped,KCC
      real a_cal,ANCC,s,Ee,theta,dtheta,riec,rdiec,rxec,drxec
      real thet_cc(18), ch2nsec, ch_time
      integer nhits, nch
      real adc(max_n_hit_CC),tdc(max_n_hit_CC)
      real aphe(maxcrec),tclust(maxcrec), ta_tot(maxcrec)
      integer itdcmin,itdcmax,iadcmin,iadcmax
      real a1,a2,a3,a4,a5,a6,a7 ! debugging
      integer is ! debugging
c
      data Ee /1.0/
      data thet_cc /  7.72,  9.44, 11.17, 12.93, 14.69, 16.47, 18.29,
     * 20.15, 22.03, 24.00, 26.04, 28.18, 30.48, 32.94, 35.44, 37.97,
     * 40.51, 43.11/
      data NWRITE,NWARN /0,0/, ch2nsec /0.05/
c_end_var
c
c  executable code for routine CC_EVNT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 2
        CRMESS='CC_EVNT is called!'
        CALL RECMES(CRNAME,'I',CRMESS)
c
c       save itdc_min,itdc_max,iddc_min,iddc_max locally
c
        itdcmin = itdc_min
        itdcmax = itdc_max
        iadcmin = iadc_min
        iadcmax = iadc_max
c
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
c
      nhits = 0
      do 100 i = 1, n_hits_CC
c
        nphe = 0
        a_cal = opheamp( n_SECT_CC(i),n_CC_hit(i) )
        iped  = ccpedes( n_SECT_CC(i),n_CC_hit(i) ) + 0.5
        ch2nsec  = abs(xcct2ch(n_SECT_CC(i),n_CC_hit(i))) 
        ch_time = acctime(n_SECT_CC(i),n_CC_hit(i))
c
c  ++++   Test of the GOOD CC hit  ++++
c
        if( n_SECT_CC(i).le.0 .or. n_SECT_CC(i).ge. 7 ) go to 99
        if( n_CC_hit(i) .le.0 .or. n_CC_hit(i) .ge.37 ) go to 99
        if(I_TDC_CC(i) .lt. itdcmin .or.
     &     I_TDC_CC(i) .gt. itdcmax)   go to 99 ! bad TDC time for a hit
        if((I_ADC_CC(i) - iped) .lt. iadcmin .or.
     &     (I_ADC_CC(i) - iped) .gt. iadcmax ) go to 99 ! bad ADC amplitude
c
        if(a_cal.lt.10.) then
          if(NWARN.lt.20) then
            write(CRMESS,*) 'WRONG calibration parameters for sector',
     &      n_SECT_CC(i),'   Segment:',n_CC_hit(i),'  SPE peack:',a_cal
            CALL RECMES(CRNAME,'W',CRMESS)
            NWARN = NWARN + 1
          end if
          a_cal = 200.
        end if
c
c ***** GOOD HIT *****
c
        nhits = nhits + 1
        if(nhits.gt.maxcrec) go to 99  ! too many hits
        adc(nhits) = (I_ADC_CC(i) - iped)/a_cal
        tdc(nhits) = ch2nsec*I_TDC_CC(i) + ch_time
c
        KCC =  n_CC_hit(i)
        NCC =  (KCC+1)/2
        INCC = 1 - 2*mod(n_CC_hit(i),2)
        if(nrclust.ge.maxcrec) then
          print *, ' MORE then ',maxcrec,
     *    ' HITS during reconstruction !!!'
          go to 99                     ! Enough hits ?
        end if
c
        if(nrclust.eq.0) then
c
          nrclust = 1
c
          ta_tot(nrclust) = adc(nhits)*tdc(nhits)  ! Nphe * tdc
          aphe(nrclust) = adc(nhits)               ! N photoelectrons in the cluster
          tclust(nrclust) = tdc(nhits)             ! cluster time (nsec)
c
          nrsect(nrclust)  = n_SECT_CC(i)
          nrsegm(nrclust)  = 10 * NCC
          nrsegm_p(nrclust)= 10 * NCC  
          nrsegm_m(nrclust)= 10 * NCC  
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
          nrphe(nrclust) = 10. * aphe(nrclust) + 0.5 
          nrtime(nrclust) = tclust(nrclust) / ch2nsec + 0.5
c
          go to 99 ! Finished with this hit
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
              s = ( aphe(j) * nrsegm(j) + adc(nhits) *
     &        10 * NCC ) / (aphe(j) + adc(nhits))
              nrsegm(j) = s + 0.5     ! weighted CC segment number
              nch = 0.1*(s + 0.5)
              ch2nsec  = abs(xcct2ch(n_SECT_CC(i),nch))
c
              if(10*NCC.ge.nrsegm_p(j)) nrsegm_p(j) = 10*NCC
              if(10*NCC.le.nrsegm_m(j)) nrsegm_m(j) = 10*NCC
c
              aphe(j) = aphe(j) + adc(nhits) ! Nphe in the cluster
              ta_tot(j) = ta_tot(j) + adc(nhits)*tdc(nhits)  ! Nphe * tdc
              tclust(j) = ta_tot(j)/aphe(j)              ! cluster time (nsec)
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
              nrphe(j) = 10. * aphe(j) + 0.5 
              nrtime(j) = tclust(j) / ch2nsec + 0.5
c
              go to 100     ! finished with this hit
            end if
          end do
c
c ======> new cluster
c
          nrclust = nrclust + 1
c
          ta_tot(nrclust) = adc(nhits)*tdc(nhits)  ! Nphe * tdc
          aphe(nrclust) = adc(nhits)               ! N photoelectrons in the cluster
          tclust(nrclust) = tdc(nhits)             ! cluster time (nsec)
c
          nrsect(nrclust)  = n_SECT_CC(i)
          nrsegm(nrclust)  = 10 * NCC
          nrsegm_p(nrclust)= 10 * NCC  
          nrsegm_m(nrclust)= 10 * NCC  
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
          nrphe(nrclust) = 10. * aphe(nrclust) + 0.5 
          nrtime(nrclust) = tclust(nrclust) / ch2nsec + 0.5
c
        end if    ! nrclust.ne.0
c
   99   continue
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





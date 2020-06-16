      subroutine digi_sc

c_begin_doc

c  Documentation for subroutine digi_sc

c  $Id: digi_sc.F,v 1.5 1998/04/26 09:51:05 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Digitization of the scintillator                          *
c     *                                                                *
c     *    ==>Called by : user_digi                                    *
c     *       Author    M. Guidal				       *	
c     *                                                                *
c     *       Modified  E. Burtin        June 94                       *
c     *          - Several bugs in TDC fixed       		       *
c     *          - Attenuation length implemented                      *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 9-nov-94
c  Second revision:
c  ----------------
c     John Ficenec, Virginia Tech, 15 July/19 August 1996
c  Third revision:
c  ---------------
c     Burin Asavapibhop, U. of Massachusetts, 23-mar-98
c	- get rid of any correction, no timing correlated to DC.
c 	  The start time for GSIM is zero. No cable delays are involved.
c	  TDC = Tflight + Tprop

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: digi_sc.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/04/26 09:51:05 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'DIGI_SC')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     &$Id: digi_sc.F,v 1.5 1998/04/26 09:51:05 holtrop Exp $
     & '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
#include "scsets.inc"
#include "lcgeom.inc"
#include "scgeom.inc"
#include "sccalib.inc"
#include "gsim_trigger.inc"
c_end_inc

c  local variables:
c  ----------------
      integer nhmax
      parameter (nhmax=100)
      real LcL
      parameter (LcL=0.0)
      real xh, yh, zh
      real adc1ih,adc2ih, adc1,adc2, coef1,coef2, length, hits
      real tdc1,tdc2, tdc_min1,tdc_min2, twk1,twk2, tdel1,tdel2
      real tprop1,tprop2, tdc1net,tdc2net, tdc1cts,tdc2cts, rtd1,rtd2
      integer idig,ih,imodul,isect,itra
      integer kdigi,nd,nhits,numbv,numvs
      integer id, ij, ik, k

      dimension numbv(nvol_sc,nhmax),hits(nhits_sc,nhmax),itra(nhmax),
     &         kdigi(ndigi_sc,nhmax),numvs(nvol_sc)

c_end_var
c     The calibration include file has as common sccalib, which contains
c     sccal_values(1-12,1-2,1-48,1-6)=sccal_values(par,end,paddl,sector)
c     The parmeter(par) quantities are for 1 through 12 respectively
c     TD0,TD1,TD2,TW0,TW1,TW2,ADP,ADS,VEF,ATN,TSIG0,TSIG1.
c   1:TD0 will give zero time at the center of each counter
c   2:TD1 is the primary parameter to convert from counts to nsec
c   3:TD2 is the quadratic correction term in the above conversion
c   4:TW0 is the timewalk offset
c   5:TW1 is a time walk parameter
c   6:TW2 is another time walk parameter
c   7:ADP is the ADC pedestal
c   8:ADC is the ADC conversion from counts to NormalMinimumIonizing
c   9:VEF is the effective velocity of propogation in the scintillator
c  10:ATN is the attenuation length in the scintillator
c  11:TSIG0 is the net resolution in nsec
c  12:TSIG1 is another resolution parameter to accomodate other effects
c     the counter end values of 1 or 2 are for L or R, CCW or CW.

c  executable code:
c  ----------------
	
c  loop over the sectors

C      call gphits('SC  ','SCIN')

      DO isect=1,nsect

C         write(6,*)'digi_sc for sector ',isect

         numvs(1)=isect

c  initialize number of words in SC bank
         nd = 0

c  Loop over the scintillators
      Do 20 imodul=1,nscin
         numvs(2)=imodul + nscin*(isect-1)

c  get sc hits
         call gfhits('SC  ','SCIN',nvol_sc,nhits_sc,nhmax,0,numvs,
     &        itra,numbv,hits,nhits)
     
         if (nhits.le.0) go to 20

C         write(6,*)' SC Hit: ',imodul,numvs(2),nhits            

         if (nhits.gt.nhmax) then
            write (chmail,1100) isect, imodul, nhmax, nhits
 1100       FORMAT ('?SCDIGI ERROR   : Hits overflow for scintillator '
     &           ,'no. ',I3,' in sector ',I2,'. Only ',I4,' of ',I4,' hits '
     &           ,'could not be processed.')
            call recmes(crname,'I',chmail)
            nhits=nhmax
         end if

c  initialize values for TDC and ADC calculation
         tdc_min1 = 99999.
         tdc_min2 = 99999.
         adc1 = 0.
         adc2 = 0.

c  time of light propagation in the scintillator; if one of the ganged
c       scintillators then take the average length of the two.  Note
c       that potentially the length can be negative if hit outside the
c       average of the shorter and longer scintillators.  In practice
c       one will delay the signal from the shorter of the two scint.
c       so that the two tubes are in time for a crossing hit.  This
c       can be accomodated with a shift in TD0.  'alsc' is the length
c       of each of the 57 scintillators.

         if(imodul.GE.nddica) then
            id=(imodul+nddica)/2
            ij=2*id-nddica
            ik=ij+1
            length=(alsc(ij)+alsc(ik))/2.0
         else
            length=alsc(imodul)
         end if 


c      print '(6F12.7)', (sccal_values(k,1,imodul,isect),k=1,6)
c      print '(6F12.7)', (sccal_values(k,1,imodul,isect),k=7,12)
c      print '(6F12.7)', (sccal_values(k,2,imodul,isect),k=1,6)
c      print '(6F12.7)', (sccal_values(k,2,imodul,isect),k=7,12)
c      print* ,isect,imodul,nhits,length,ym(imodul)

c  Loop over the NHITS hits in one scintillator.
      do ih=1,nhits
         xh = hits(1,ih)*cossec(isect)+hits(2,ih)*sinsec(isect)
         yh =-hits(1,ih)*sinsec(isect)+hits(2,ih)*cossec(isect)
         zh = hits(3,ih)


c      print* ,ih,xh,yh,zh


c  allow for offset of scintillator center; remember for now there is
c       only one set of geometry which is rotated to get the 6 sectors.
c       When that changes, ym(imodul) will be changed to 
c       ym(imodul,isect) as the include file will be changed.

         yh = yh - ym(imodul)
         tprop1=(length/2.+yh)/sccal_values(9,1,imodul,isect)
         tprop2=(length/2.-yh)/sccal_values(9,2,imodul,isect)

c  for attenuation of light in the scintillator, the length/2. factor
c      cancels because the gain is adjusted so that NMIPs at the center
c      of every counter, irrespective of length, will give the same
c      number of ADC counts.  To display the formula with the length
c      included, replace length by LcL, which is set to zero, and one
c      can replace LcL by length to recover uncalibrated relationship.

         coef1 = exp(-(LcL+yh)/sccal_values(10,1,imodul,isect))
         coef2 = exp(-(LcL-yh)/sccal_values(10,2,imodul,isect))

c      print* ,yh,tprop1,tprop2,coef1,coef2


c  sum the ADC after attenuation, and take the earliest TDC for both ends
c      ADC is a pulse integral converted to counts; thck is thickness.
c      For example there are about 600 counts for a NMIP which represents
c      about 10 MeV of energy deposit, or 0.010 GeV .  For example
c      the value of TD1 is about 50 ps/count or 0.050 ns/count.

         tdc1 = HITS(4,IH) + TPROP1
         tdc2 = HITS(4,IH) + TPROP2
         adc1ih = HITS(5,IH)*COEF1*(1./(dEdxMIP*thck(imodul)))/
     &                       sccal_values(8,1,imodul,isect)
         adc2ih = HITS(5,IH)*COEF2*(1./(dEdxMIP*thck(imodul)))/
     &                       sccal_values(8,2,imodul,isect)
         if(adc1ih.GT.adcmax) adc1ih=adcmax
         if(adc2ih.GT.adcmax) adc2ih=adcmax
         if(adc1ih.LE.adceth) adc1ih=adceth
         if(adc2ih.LE.adceth) adc2ih=adceth

c      print* ,hits(4,ih),hits(5,ih)
c      print* ,tdc1,tdc2,adc1ih,adc2ih


c  apply the time walk, be aware of signs.  They should be the opposite
c        here compared with the reconstruction, since they both use the
c        same calibration file.

         twk1 = -sccal_values(5,1,imodul,isect)/(adc1ih)**
     &          sccal_values(6,1,imodul,isect) - 
     &          sccal_values(4,1,imodul,isect)
         twk2 = -sccal_values(5,2,imodul,isect)/(adc2ih)**
     &          sccal_values(6,2,imodul,isect) -
     &          sccal_values(4,2,imodul,isect)
c         tdc1 = tdc1 + twk1
c         tdc2 = tdc2 + twk2
c  Let's forget about any time walk correction
         tdc1 = tdc1 
         tdc2 = tdc2 


c  the difference between nominal PMT,cable, and start times needs to go
c      here to make sure the proper one survives.  Since we will also
c      time match each scintillator to give the same time as a function
c      of distance from the center; then TD0 will have one term that is
c      -length/2*velocity, and another that is -tdel for the effects
c      that are noted above that combine to give something other than
c      the nominal value of 450 nsec.  Add the tdel piece here, since
c      the other term is already included.
c      Watch out for signs; should have opposite signs as those used
c      in the reconstruction.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         tdel1 = -sccal_values(1,1,imodul,isect) -
     &     (length/2.)/sccal_values(9,1,imodul,isect)
         tdel2 = -sccal_values(1,2,imodul,isect) -
     &     (length/2.)/sccal_values(9,2,imodul,isect)
c         tdc1 = tdc1 + tdel1
c         tdc2 = tdc2 + tdel2
c  Let's forget about any time delay and use these. There is a minus part 
c  because we used the time which respect to the time at mid paddle.
         tdc1 = tdc1 - (length/2.)/sccal_values(9,1,imodul,isect)
         tdc2 = tdc2 - (length/2.)/sccal_values(9,2,imodul,isect)

c      print* ,twk1,twk2,tdel1,tdel2,tdc1,tdc2

c  If signal is below threshold, which is 'discth' here, and
c     which for example might be set to correspond to 30/600=5% NMIP, then
c     the TDC will not be stopped.  Under these circumstances put the 
c     maximum channel reading tdcmax=4096 in as nsec equivalent.

c         if(adc1ih.LT.discth)
c     &   tdc1=tdcmax*sccal_values(2,1,imodul,isect) +
c     &        (tdcmax**2)*sccal_values(3,1,imodul,isect)
c         if(adc2ih.LT.discth)
c     &   tdc2=tdcmax*sccal_values(2,2,imodul,isect) + 
c     &        (tdcmax**2)*sccal_values(3,2,imodul,isect)

c  Use only the slope to convert channel to nanosecond. This 0.05 came from
c  the map database for run #1.     
         if(adc1ih.LT.discth)   tdc1=tdcmax*0.05
         if(adc2ih.LT.discth)   tdc2=tdcmax*0.05
         
         if(tdc1.LT.tdc_min1) tdc_min1 = tdc1
         if(tdc2.LT.tdc_min2) tdc_min2 = tdc2
         adc1 = adc1 + adc1ih
         adc2 = adc2 + adc2ih 

c      print* ,tdc_min1,tdc_min2,adc1,adc2

      enddo


c  put in limit of ADC, both upper limit of ADC and energy threshold
c      if both ends below energy threshold, then no digitization
         if(adc1.LE.adceth.AND.adc2.LE.adceth) go to 20
c         if(adc1.GE.adcmax) adc1=adcovr-sccal_values(7,1,imodul,isect)
c         if(adc2.GE.adcmax) adc2=adcovr-sccal_values(7,2,imodul,isect)

c  Use no pedestal subtraction
         if(adc1.GE.adcmax) adc1=adcovr
         if(adc2.GE.adcmax) adc2=adcovr

c  store TDC in counts
c         tdc1net=(tdc_min1-Ttrig+t0sc)/sccal_values(2,1,imodul,isect)
c         tdc2net=(tdc_min2-Ttrig+t0sc)/sccal_values(2,2,imodul,isect)
c         rtd1=sccal_values(3,1,imodul,isect)/
c     &        sccal_values(2,1,imodul,isect)
c         rtd2=sccal_values(3,2,imodul,isect)/
c     &        sccal_values(2,2,imodul,isect)
c         tdc1cts = tdc1net*(1.-(rtd1*tdc1net)+2.*(rtd1*tdc1net)**2)
c         tdc2cts = tdc2net*(1.-(rtd2*tdc2net)+2.*(rtd2*tdc2net)**2)
c         if(tdc1cts.GE.4096) tdc1cts = tdcmax 
c         if(tdc2cts.GE.4096) tdc2cts = tdcmax
c         KDIGI(2,1) = tdc1cts
c         KDIGI(4,1) = tdc2cts
 
c  Do not use Ttrig and t0sc 
         KDIGI(2,1) = tdc_min1/0.05
         KDIGI(4,1) = tdc_min2/0.05

c  apply poissonian statistics with n photoelectrons per MeV ( to be done )

c  convert to adc reading for either predestal subtracted(apedsub=0)
c                             or pedestal not subtracted(apedsub=1)
c         adc1 = adc1 + apedsub*sccal_values(7,1,imodul,isect)
c         adc2 = adc2 + apedsub*sccal_values(7,2,imodul,isect)

c  No pedestal subtraction
         KDIGI(1,1) = adc1
         KDIGI(3,1) = adc2

         ih = 1
         if (kdigi(1,ih).eq.0) go to 20
            
         call gsdigi(set_sc,det_sc,itra,nhits,numvs,
     1                 kdigi(1,ih), idig)
	 
		
         if (idig.eq.0) then
            write (chmail,1200) imodul, isect
            call recmes(crname,'I',chmail)
         end if

 20   Continue
      ENDDO

      return


 1200 FORMAT ('?SCDIGI ERROR   : Digitization for scintillator '
     1       ,'no. ',I3,' in sector ',I2,' could not be stored.')

      end

c---------------------------------------------------------------------------------

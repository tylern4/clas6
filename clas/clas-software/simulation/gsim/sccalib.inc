c  sccalib.inc

c  $Id: sccalib.inc,v 1.2 1997/04/24 15:38:51 holtrop Exp $

      character*132 cisccalib
      data cisccalib/
     1'$Id: sccalib.inc,v 1.2 1997/04/24 15:38:51 holtrop Exp $'
     1/

c  dEdxMIP is in GeV/cm for Minimum Ionizing Particle
c  discth  is the pre-TDC discriminator threshold in ADC counts equiv.;
c          this will be included in a revised calibration bank later.
c  t0sc    is in ns the accumulation of PMT rise time, cable delay, and
c          and other delays as a single nominal value; variations
c          will be included in the TD0 calibration parameter by tube
c  tdcmax  is the maximum TDC channel count(results when below discth)
c  adceth  is the minimum ADC channel count(if both ends below, no digi)
c  adcmax  is the maximum ADC channel count
c  adcovr  is the ADC overflow channel count
c  apedsub is the indicator specifying if the ADC digitization has the
c          pedestal added or not; is 0.0 if not/1.0 if is ; therefore
c          0.0 means the information in the DIGI bank is pedestal     
c          subtracted, and 1.0 means the info. is not pedestal sub.
c  npaddl  is the number of scintillators of different dimensions
c  nelcch  is the number of geometry/elect. channels after ganging
c  nddica  is the first scintillator to be ganged
c  nsecmx  is the maximum number of sectors to be considered
c  nboscal is the number of entries in the calibration bank per channel
c  the 48 calibration channels will be expanded to 57 scinatillator
c          calibrations for the simulation digitization
c  the information from the 57 scintillators will be combined to 
c          produce 48 electronic channels of information for output
c  the dimensions of sccal_values are the 12 quantities for each
c          tube, the CCW(left)=1 tube or the CW(right)=2 tube, etc.
c          The 12 quantities are in order, TD0, TD1, TD2, TW0, TW1
c          TW2, ADP, ADS, VEF, ATN, TSIG0, TSIG1.

        integer npaddl, nelcch, nddica, nsecmx, nboscal, iadcmax
        real dEdxMIP,discth,t0sc,tdcmax,adcmax,adcovr,apedsub,adceth

        parameter (npaddl = 57)
        parameter (nelcch = 48)
        parameter (nddica = 40)
        parameter (nsecmx =  6)
        parameter (nboscal= 48)
        parameter (iadcmax= 8192)
        parameter (dEdxMIP = 0.001998)
        parameter (discth = 30.0)
        parameter (t0sc   = 450.)
        parameter (tdcmax = 4096.)
        parameter (adcmax = 8192.)
        parameter (adcovr = 16384.)
        parameter (apedsub= 0.0)
        parameter (adceth = 3.0)

        real sccal_values(12,2,npaddl,nsecmx)

        common/ sccalib/ sccal_values

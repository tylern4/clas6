      SUBROUTINE DIGI_TRIG
c
c_begin_doc
c  RCS ID string
c  $Id: digi_trig.F,v 1.3 1998/04/11 03:50:11 holtrop Exp $
c
c  Documentation for subroutine DIGI_TRIG
c
c  Purpose:
c  --------
c  create trigger time.
c   (start time for SC
c    stop time for DC)
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
c  Based heavily (i.e. copied and slightly editted) on DIGI_SC
c   written by  M. Guidal and modified by E. Burtin
c
c  Author:   James Mueller      Created:  Mon Jul 29 10:07:40 EDT 1996
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
c BOS common block  uncomment the next line for BOS include file
#include "gcunit.inc"
#include "scsets.inc"
#include "lcgeom.inc"
#include "scgeom.inc"
#include "gsim_trigger.inc"
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
      PARAMETER (CFILE = '$RCSfile: digi_trig.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/04/11 03:50:11 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/   
     1'$Id: digi_trig.F,v 1.3 1998/04/11 03:50:11 holtrop Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='DIGI_TRIG')
      PARAMETER (CRAUTH='James Mueller')
c
c  Local User defined variables:
c  -----------------------------
      integer nhmax
      parameter (nhmax=100)

c  the following was automatically generated...ejw, W&M, 2-dec-94
      real hits
      integer ih,imodul,isect,itra
      integer length,nd,nhits,numbv,numvs
      real tdc1,tdc2,tdc_min1,tdc_min2,tprop1,tprop2
      real vprop,xh,yh,zh
      real trig_sc,tdel
      dimension numbv(nvol_sc,nhmax),hits(nhits_sc,nhmax),itra(nhmax),
     1         numvs(nvol_sc)
      

      data vprop / 16.0 / 	
      data tdel / 380. /
c_end_var
c
c  executable code for routine DIGI_TRIG:
c----6----------------------------------------------------------------72
c

c save minimum scintillator time for trigger
      trig_sc = 99999.   

c  loop over the sectors
      do isect=1,nsect
         numvs(1)=0

c  initialize number of words in SC bank
         nd = 0

c  Loop over the scintillators
         do 20 imodul=1,nscin
            numvs(2)= imodul + (isect-1)*57

c  get sc hits
            call gfhits('SC  ','SCIN',nvol_sc,nhits_sc,nhmax,0,numvs,
     1           itra,numbv,hits,nhits)

            if (nhits.le.0) go to 20
	
            if (nhits.gt.nhmax) then
               write (chmail,1100) isect, imodul, nhmax, nhits
               call recmes(crname,'I',chmail)
               nhits=nhmax
            end if

c  initialize values for TDC and ADC calculation
            tdc_min1 = 99999.
            tdc_min2 = 99999.

c   Loop over the NHITS hits in one scintillator.
            do ih=1,nhits
               xh = hits(1,ih)*cossec(isect)+hits(2,ih)*sinsec(isect)
               yh =-hits(1,ih)*sinsec(isect)+hits(2,ih)*cossec(isect)
               zh = hits(3,ih)
c  time of light propagation in the scintillator
               length=alsc(imodul)
c  allow for offset of scintillator center
               yh = yh - ym(imodul)
               tprop1=(length/2.+yh)/vprop
               tprop2=(length/2.-yh)/vprop
               tdc1 = HITS(4,IH) + TPROP1
               tdc2 = HITS(4,IH) + TPROP2
c  take the earliest time from each end
               if (tdc1.lt.tdc_min1) tdc_min1 = tdc1
               if (tdc2.lt.tdc_min2) tdc_min2 = tdc2
            enddo

c  keep track of minimum time for trigger information - J.A.M. 19/6/96
            if (tdc_min1.lt.trig_sc) trig_sc = tdc_min1
            if (tdc_min2.lt.trig_sc) trig_sc = tdc_min2

 20      continue
      enddo

      Ttrig = trig_sc + tdel
      return


 1100 FORMAT ('?SCDIGI ERROR   : Hits overflow for scintillator '
     1       ,'no. ',I3,' in sector ',I2,'. Only ',I4,' of ',I4,' hits '
     2       ,'could not be processed.')

      end
c
c------------------------------------------------------------------------------





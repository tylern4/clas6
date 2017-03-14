      SUBROUTINE ECFSP(axis,layer,sector,iterr,ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: ecfsp.F,v 1.19 2008/04/02 03:24:06 fklein Exp $
c     
c     Documentation for subroutine ECFSP
c     
c     Purpose: The name of the subroutine stands for 'EC - fit strips to peaks'.
c     
c     The algorithm has two sections. The first section is used for the first
c     pass through the data to find the peaks from groups of strips; this 
c     section is executed when iterr=1. The second section is executed when
c     iterr=2, and it refines the estimate of the peak locations by making
c     attenuation length corrections to the data and re-calculating the mean
c     and RMS. The subroutine is called multiple times per event from 
c     ecfit_analyze with iterr=1, and is called multiple times per event from
c     ecfph with iterr=2.
c
c     For iterr = 1, the the code does the following: 
c     First, 'groups' are identified: a 'group' is a collection of strips in 
c     each view. The strips in a view have the following characteristics: 1) the 
c     energy deposit is above a threshold value given by the tcl variable 
c     EcFitEdge_strip_threshold, and 2) the strips are immediately adjacent to 
c     each other, or they are not separated by any more strips than is specified
c     by the tcl variable touch_id. If touch_id=1, no missing strips are allowed
c     in the group. If touch_id=2, one missing strip is allowed, if =3, two are 
c     allowed, etc. There are other limits imposed as well: the maximum number 
c     of groups is limited by the parameter Maxpeaks, which is defined in the 
c     file Ec_general.PAR at the present time (Jan 1998). Note that the 
c     'threshold' has energy units, but it is actually not corrected for atten-
c     uation lengths, so the threshold is actually for pulse size, not deposited
c     energy.
c     
c     Next, the group is re-ordered from being sequential in strip number to 
c     being sequential in energy deposit, the first entry being the strip with 
c     the largest energy deposit. The centroid and RMS of each group is then 
c     calculated (with a prescription for calculating the RMS for groups which 
c     have only one strip). At this point the subroutine returns.
c     
c     For iterr = 2, the code does the following: 
c     For each strip in each peak, the energy is corrected for attenuation 
c     length. If layer=9 ('whole' reconstruction), the inner and outer energies 
c     are added together for each strip number, otherwise they are not.
c
c     The inner or outer times for a given view are calculated by averaging all 
c     the strip times (minus transit time in the scintillator) for each strip 
c     in the peak using the square root of the adc value for a weight. The 
c     'whole' time is not calculated in this routine, but is calculated in the
c     routine ecfph, using only the inner and/or outer mean times for each hit.
c
c     The peak energy in a given view and layer is obtained by adding all the 
c     corrected energies in the peak together. The centroid and RMS are 
c     calculated; these are calculated using either of two weights: either 
c     weighted by the energy in the strip, or by the logarithm of the energy in 
c     the strip. The two options are selected by the TCL variable ec_ln_weights. 
c     The second moment, third moment, and fourth moment of the distribution are 
c     calculated, and the subroutine returns.
c
c     Above documentation last updated by W. Brooks in January 1998.
c
c     --------
c     
c     Input Parameters:  (Name   -  Type   - Meaning)
c     ----------------
c     axis   - Integer - U,V or W views
c     layer  - Integer - Whole(9),Inner(10) or Outer(11)
c     sector - Integer - # sector (1:6)
c     iterr  - Integer - 
c     
c     
c     Output Parameters:  (Name - Type - Meaning)
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   St. Stepanyan      Created:  Fri Aug  9 10:50:23 EDT 1996
c     -------
c     
c     Major revisions:
c     ----------------
c     W. Brooks, Jan 1998 - Wrote (above) summary documentation, added in-line 
c        documentation and indenting, added sqrt(adc) weighting to time calculation.
c     Ky Kim, May-11-1999, removed "Timing" part which are now done in "ec_time"
c        function subroutine.
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
c     BOS common block  uncomment the next line for BOS include file
c     #include "bcs.inc"
#include "Ec_general.PAR"
#include "EcStrip.CMN"
#include "ecgeom.inc"
#include "EcCal.CMN"
#include "EcFit_general.CMN"
#include "EcFit_edge.CMN"
#include "ec_rawdata.inc"
#include "ec_control.inc"
#include "ec_strip.inc"
#include "ec_peak.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ecfsp.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.19 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/04/02 03:24:06 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1     '$Id: ecfsp.F,v 1.19 2008/04/02 03:24:06 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ECFSP')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c     
c     Local User defined variables:
c     -----------------------------
c     INTEGER NWRITE
      integer axis,layer,sector,iterr,ierr
*     
*     - working strip arrays
      integer stripID,nghit,highest_adcID
*     
c      integer MAXgroups
c      parameter (MAXgroups= MAXstrips/2+1)
c      real group_E(MAXgroups)
c      integer Ngroup,IG,groupN(MAXgroups),id_g(MAXgroups)
c      integer groupID( MAXstrips,MAXgroups)
      integer IG,id_g(MAXgroups)
*     
      integer id,mth,nth,pth,ith
      integer jth,ist,hitID
      integer member, itest
      real timing_we,etot_allgroup(whole:cover)
      real atl, path, einn_whole, eout_whole, shortest_path, highest_adc
      real dprj,sumEprj,sumEprj2,sumEprj3,sumEprj4,radc
      real dE,cntrd,swid,sum_weights
      logical touch
      REAL myEprj2, myEprj3, myEprj4

c     Step      logical touchs
*     
c     _end_var
c     
c     executable code for routine ECFSP:
c---- 6----------------------------------------------------------------72
*     
*     - initialize below threshold strip # counter
*     
      ith=0
*     
*     - build "groups" out of strips
*     
      swid= Ec_edge_L(axis,layer,sector)/ Maxwstrips
      if(axis.eq.25)etot_allgroup(layer)=0.
      IF ( iterr.EQ.1 ) THEN
*     
*     - form "groups" of strips
         Ngroup=0               !first group
         member=0
         id=-1-touch_id         ! artificial so not touch with any strip
*     
*     -check to see if this strip touches one or more existing groups
* 
         itest = Ec_stripN(axis,layer,sector)
         if (itest .le. 0.OR.itest.gt.Maxwstrips) then
            EcFit_Npeak(axis,layer,sector)= 0 !Event is skipped
            RETURN
         elseif(touch_id.gt.0)then
            DO 5 mth=1,Ec_stripN(axis,layer,sector)
               stripID= Ec_stripID(mth,axis,layer,sector) !id# of strip 
               if (stripid .le. 0.or.stripid.gt.maxwstrips) then
                  write(crmess,999) axis, layer, sector
 999              format('Bad strip-id axis:layer:sector',i3,i3,i3)
                  call recmes(crname,'w',crmess)
               else
                  if(Ec_strip(ENERGY,stripID,axis,layer,sector)
     &                 .GE.EcFitEdge_strip_threshold) then
                     touch=(stripID-id).le.touch_id
                     id=stripID
                     if(touch) then
                        member=member+1
                        strip_efr(Ngroup,stripID)=1. 
                        group_E(Ngroup)= group_E(Ngroup) +
     &                       Ec_strip(ENERGY,stripID,axis,layer,sector)
                     else
                        member=1
                        Ngroup=Ngroup+1
                        If(Ngroup.gt.Maxpeaks) Then
                           EcFit_Npeak(axis,layer,sector)= 0 !Event is skipped
                           RETURN
                        ENDIF
                        group_E(Ngroup)= 
     $                       Ec_strip(ENERGY,stripID,axis,layer,sector)
                     endif
                     groupN(Ngroup)=member
                     groupID(member,Ngroup)=stripID
                  endif
               endif
 5          continue
         elseif(touch_id.eq.0)then
            call group0(axis,layer,sector,iterr,ierr)
         endif
*     
*     - now have "groups" of strips; find ordering according to decreasing size
*     
*     - sort by decreasing size and set minimum size
         IF(Ngroup.gt.0)THEN
            DO mth=1,Ngroup
               etot_allgroup(layer)=etot_allgroup(layer)+ group_E(mth)
            ENDDO
            ierr=0
            call EcGus_sort(Ngroup,group_E,id_g)
            Nghit=0
            DO mth=1,Ngroup
               If(group_E(id_g(mth)).lt.EcFitEdge_peak_threshold)go to 89 !above thre+
               Nghit= Nghit+1   !#
            ENDDO
*     
 89         continue
            Nghit= MIN(Nghit, MAXpeaks)
*     
*     - load "groups" into "peaks" as initial guess
            EcFit_Npeak(axis,layer,sector)= Nghit !total number of peaks
*     
            DO pth= 1,Nghit     !peak made from "pth" sorted group
               IG= id_g(pth)
               sumEprj= 0.
               sumEprj2= 0.
               sum_weights=0.
*     
*     - keep number of strips assigned to peak
*     
               Do nth= 1,groupN(IG)
*     
                  id= groupID(nth,IG)
                  dE=Ec_strip(ENERGY,ID,axis,layer,sector)*strip_efr(ig,ID) 
                  if(ec_ln_weights)then
                     if(dE.gt.0.00001)then
                        dE=log(10000.*dE)
                     else
                        dE=0.
                     endif
                  endif
                  sum_weights=sum_weights+dE
                  dprj= id*swid-swid/2. ! position of the strip
*     
                  sumEprj= sumEprj + dE*dprj
                  sumEprj2= sumEprj2 + dE*dprj*dprj
*     
                  EcFit_peak_strip(nth,pth,axis,layer,sector)=
     &                 groupID(nth,IG) !keep strip id
                  EcFit_stripE(nth,pth,axis,layer,sector)=
     &                 Ec_strip(ENERGY,ID,axis,layer,sector)*strip_efr(ig,ID) !keep strip energy
*     
               EndDo
*     
               EcFit_peak_Nstrip(pth,axis,layer,sector)= groupN(IG) ! # of strips in the group
               EcFit_peak(ENERGY,pth,axis,layer,sector)= group_E(IG) !area
*     -centroid
               cntrd= sumEprj/sum_weights !av. position (first moment)
*     -set RMS width
               if(groupN(IG).gt.1) then
                  EcFit_peak(WIDTH,pth,axis,layer,sector)=
     &                 SQRT(abs(sumEprj2/sum_weights-cntrd**2))
               elseif(groupN(IG).eq.1)then
                  EcFit_peak(WIDTH,pth,axis,layer,sector)=swid/SQRT(12.)
               endif
*     
               EcFit_peak(DIST,pth,axis,layer,sector)= cntrd !position on axis
*     
            ENDDO               !over groups
         ELSE
            EcFit_Npeak(axis,layer,sector)=0
         ENDIF
*     
      ELSEIF ( iterr.EQ.2 ) THEN
*...............................................................
         Do ith=1,EcFit_Npeak(axis,layer,sector)
            IF ( peak_Nhit(ith,axis).GT.0 ) THEN
               shortest_path=1000.
               highest_adc=0.
               highest_adcID=0
               do jth=1,peak_Nhit(ith,axis)
                  hitID=peak_hit(jth,ith,axis)
                  path = peak_path(hitID,axis)
                  if(path.lt.shortest_path)shortest_path=path
               enddo
               do ist=1,EcFit_peak_Nstrip(ith,axis,layer,sector)
                  stripID=EcFit_peak_strip(ist,ith,axis,layer,sector)
                  radc=ec_raw_adcs(stripID,axis,layer,sector)
                  if(radc.gt.highest_adc.and.
     &                 Ec_strip(TIME,stripID,axis,layer,sector).gt.0.and.
     &                 EcCal_Trms(stripID,axis,layer,sector).gt.0.)then
                     highest_adc=radc
                     highest_adcID=stripID
                  endif
               enddo
               do jth=1,peak_Nhit(ith,axis)
                  hitID=peak_hit(jth,ith,axis)
                  Ec_peak(ENERGY,hitID,axis,layer,sector) = 0.0
                  Ec_peak(TIME,hitID,axis,layer,sector) = 0.0
                  path = peak_path(hitID,axis)
                  sumEprj  = 0.
                  sumEprj2 = 0.
                  sumEprj3 = 0.
                  sum_weights=0.
                  timing_we=0.
                  dO ist=1,EcFit_peak_Nstrip(ith,axis,layer,sector)
*     
                     stripID=EcFit_peak_strip(ist,ith,axis,layer,sector)
**                     atL=EcCal_atten(stripID,axis,layer,sector)
*     
                     IF(layer.eq.whole)then
                        einn_whole=0.
                        eout_whole=0.
*     In the following, possible negative energies are eliminated because
*     inner and outer strips are summed together.
                        if(Ec_strip(ENERGY,stripID,axis,inner,sector)
     &                       .gt.0.)then !eliminates occasional negative energies
                           einn_whole=Ec_strip(ENERGY,stripID,axis,
     &                          inner,sector)*
     &                          inner_strips(stripID,axis,sector)*
     &                          exp(path/EcCal_atten(stripID,axis,
     &                          inner,sector))
                        endif
                        if(Ec_strip(ENERGY,stripID,axis,outer,sector)
     &                       .gt.0.)then !eliminates occasional negative energies
                           eout_whole=Ec_strip(ENERGY,stripID,axis,
     &                          outer,sector)*
     &                          outer_strips(stripID,axis,sector)*
     &                          exp(path/EcCal_atten(stripID,axis,
     &                          outer,sector))
                        endif
                        Ec_stripE(hitID,ist,axis,layer,sector)=
     &                       einn_whole+eout_whole 
                     else
                        atL=EcCal_atten(stripID,axis,layer,sector)
                        Ec_stripE(hitID,ist,axis,layer,sector)=
     &                       EcFit_stripE(ist,ith,axis,layer,sector)  
     &                       * exp(path/atL)
                     endif
*     
                     Ec_peak(ENERGY,hitID,axis,layer,sector) = 
     &                    Ec_peak(ENERGY,hitID,axis,layer,sector) +  
     &                    Ec_stripE(hitID,ist,axis,layer,sector)  
*     

c------In the following, bad tdc values have been flagged with the value -999.
c------Don't include these in the weighted time average.

                     if(stripID.eq.highest_adcID.and.layer.ne.whole)then
                        Ec_stripT(hitID,ist,axis,layer,sector)=
     &                       Ec_strip(TIME,stripID,axis,layer,sector) 
     &                      +EcCal_dT1(stripID,axis,layer,sector)*shortest_path**2
     &                      +EcCal_dT2(stripID,axis,layer,sector)*shortest_path**3
     &                      -shortest_path/speed_in_plastic
                        Ec_peak(TIME,hitID,axis,layer,sector) = 
     &                       Ec_peak(TIME,hitID,axis,layer,sector) 
     &                      +Ec_stripT(hitID,ist,axis,layer,sector)
     &                      *sqrt(ec_raw_adcs(stripID,axis,layer,sector))
     &                      /EcCal_Trms(stripID,axis,layer,sector)
                        timing_we = timing_we 
     &                      +sqrt(ec_raw_adcs(stripID,axis,layer,sector))                           
     &                      /EcCal_Trms(stripID,axis,layer,sector)
                     endif
*     
                     dE= Ec_stripE(hitID,ist,axis,layer,sector)
                     if(ec_ln_weights)then
                        if(dE.gt.0.00001)then  
                           dE=log(10000.*Ec_stripE(hitID,ist,axis,
     &                       layer,sector))
                        else
                           dE=0.
                        endif
                     endif
                     sum_weights=sum_weights+dE
                     dprj= stripID*swid-swid/2. ! position of the strip
                     sumEprj = sumEprj  + dE*dprj
                     sumEprj2= sumEprj2 + dE*dprj*dprj
                  enddO
                  time_we(hitID,axis,layer,sector)=timing_we
c
cccc 11/5/07: overflow
                  if(sum_weights.lt.1.e-6) sum_weights=1.e-6
                  if(sum_weights.gt.1.e6) sum_weights=1.e6
                  cntrd= sumEprj/sum_weights
*     -set RMS width
                  if(EcFit_peak_Nstrip(ith,axis,layer,sector).gt.1)then
                     Ec_peak(WIDTH,hitID,axis,layer,sector)=
     &                    SQRT(abs(sumEprj2/sum_weights-cntrd**2))
                  elseif(groupN(IG).eq.1)then
                     Ec_peak(WIDTH,hitID,axis,layer,sector)=
     &                    swid/SQRT(12.)
                  endif
*     
                  sumEprj2 = 0.
                  sumEprj4 = 0.
                  sum_weights=0.
                  dO ist=1,EcFit_peak_Nstrip(ith,axis,layer,sector)
                     stripID=EcFit_peak_strip(ist,ith,axis,layer,sector)
                     dE= Ec_stripE(hitID,ist,axis,layer,sector)
                     if(ec_ln_weights)then
                        if(dE.gt.0.00001)then  
                           dE=log(10000.*Ec_stripE(hitID,ist,axis,
     &                       layer,sector))
                        else
                           dE=0.
                        endif
                     endif
                     sum_weights=sum_weights+dE
*     position of the strip relative to the centre of peak
                     dprj = stripID*swid-swid/2.-cntrd
                     sumEprj2= sumEprj2 + dE*dprj*dprj
                     sumEprj3= sumEprj3 + dE*dprj*dprj*dprj
                     sumEprj4= sumEprj4 + dE*dprj*dprj*dprj*dprj
                  enddO
c-fk: if higher moments are too small numbers scale them, otherwise you'll get
c-fk  underflows (NaN) when doing geometric matching
                  if(sum_weights.lt.1.e-6) sum_weights=1.e-6
                  if(sum_weights.gt.1.e6) sum_weights=1.e6
                  myEprj2=sumEprj2/sum_weights
                  if(abs(myEprj2).lt.1.e-8) myEprj2=1.e-8
                  if(abs(myEprj2).gt.1.e8) myEprj2=sign(myEprj2,1.e8)
                  myEprj3=sumEprj3/sum_weights
                  if(abs(myEprj3).lt.1.e-8) myEprj3=1.e-8
                  if(abs(myEprj3).gt.1.e8) myEprj3=sign(myEprj3,1.e8)
                  myEprj4=sumEprj4/sum_weights
                  if(abs(myEprj4).lt.1.e-8) myEprj4=1.e-8
                  if(abs(myEprj4).gt.1.e8) myEprj4=sign(myEprj4,1.e8)
                  Ec_peak(DIST,hitID,axis,layer,sector)= cntrd !position on axis
                  Ec_peak(mom2,hitID,axis,layer,sector)= myEprj2 !second moment on axis
                  Ec_peak(mom3,hitID,axis,layer,sector)= myEprj3 !third moment on axis
                  Ec_peak(mom4,hitID,axis,layer,sector)= myEprj4 !forth moment on axis
*     
               enddo
            ENDIF
         EndDo
*     
      ENDIF
*     
      RETURN
      END
c
c------------------------------------------------------------------------------
cStep touchs=(stripID-id).le.touch_id
cStep&.and.Ec_strip(ENERGY,stripID,axis,layer,sector).lt.0.1*group_E(Ngroup)





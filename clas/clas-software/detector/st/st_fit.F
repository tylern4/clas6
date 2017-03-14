
      SUBROUTINE St_fit(ok)
c
c_begin_doc
c  RCS ID string
c  $Id: st_fit.F,v 1.22 2008/03/16 04:33:24 fklein Exp $
c
c  Documentation for subroutine st_fit
c
c  Purpose: Get position corrected timing
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     - handle the event that has 2 tracks in one sector :Burin98
c     - handle events with more than 2 tracks per pair (EAP) November-2001
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
c#include "bcs.inc"

#include "wcs.inc"
#include "st_general.PAR"
#include "stconvert.CMN"
#include "stgeom.CMN"
#include "sthit.CMN"
#include "stfit.CMN"
#include "stcal.CMN"
#include "sttcl.CMN"
#include "sttrk.CMN"

c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok
c
c  Local pre-defined variables:
c  ---------------------------
c     RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: st_fit.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.22 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:24 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1     '$Id: st_fit.F,v 1.22 2008/03/16 04:33:24 fklein Exp $
     3     '/
c     Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='st_fit')
      PARAMETER (CRAUTH='Francois Roudot')
      INTEGER NWRITE
c
c     Local st defined variables:
c     ---------------------------
      character*132 crmess
      integer i,j,pair_id,it(2*max_track_per_sector),
     +     sec(2*max_track_per_sector),ih
      real t1,t2, a1,a2, lmin, lmax, itmin, itmax
      real lm(2), itm(2)
      real tprop, vfast,vslow
      real average, w_avg
      integer nami_stgm,ind_stgm,MAMIND
      real L_leg,L_nose,L_total
c     _end_var
c
c     executable code for routine st_fit:
c     -------------------------------------
c
c---- 6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='ST_FIT CALLED'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c--   get geometry info from STG bank

      if(nami_stgm.eq.0) nami_stgm = MAMIND(jw,'STG ')
      ind_stgm = jw(nami_stgm)

      L_leg = rjw(ind_stgm+15)
      L_nose = rjw(ind_stgm+16)
      L_total = L_leg + L_nose

      j = 0                     ! reset track counter

      do pair_id = 1,3          ! loop over all pairs
         if (trk_found(pair_id) .eqv. .true.) then
c--------------------------------------------------------
c--   there are track(s) in this pair
c--   will try to get best timing depending on how many
c--------------------------------------------------------
            if ( (Ntrk_hit_ST(pair_id,1)
     +           + Ntrk_hit_ST(pair_id,2)) .eq. 1) then
c----------------------------------------------------------------
c--   this is the case when there is only one track in each pair
c----------------------------------------------------------------
               j = j + 1

               a1 = StCorrected_data(pair_id, St_ADC1)
               a2 = StCorrected_data(pair_id,St_ADC2)

               ih = StNconvertedHits(pair_id)
               it(ih) = StWhichTrk(ih,pair_id)
               sec(ih) = StWhichSector(ih,pair_id)

               vslow = StCal_values(veff_leg_near,
     +              mod(sec(ih)-1,2)+1, pair_id)
               vfast = StCal_values(veff_leg_far,
     +              mod(sec(ih)-1,2)+1, pair_id)

               if (mod(sec(ih), 2) .ne. 0) then
                  t1 = StCorrected_data(pair_id, St_TDC1)
     +                 - l_st(it(ih), pair_id)/vslow
                  t2 = StCorrected_data(pair_id, St_TDC2)
     +                 - (2.0*L_total - l_st(it(ih), pair_id))/vfast
               else
                  t1 = StCorrected_data(pair_id, St_TDC1)
     +                 - (2.0*L_total - l_st(it(ih), pair_id))/vfast
                  t2 = StCorrected_data(pair_id, St_TDC2)
     +                 - l_st(it(ih), pair_id)/vslow
               endif

               average = (t1 + t2)/2

               if (a1 .gt. 0 .or. a2 .gt. 0) then
                  w_avg = (a1*t1 + a2*t2)/(a1 + a2)
               else
                  w_avg = average
               endif

               if ( nhit_st(pair_id) .eq. 2) then ! first TDCs is missing
                  StFit_pos_indep_time(j) = t2
               else if ( nhit_st(pair_id) .eq. 1) then ! second TDC is missing
                  StFit_pos_indep_time(j) = t1
               else if ( nhit_st(pair_id) .eq. 3) then ! both TDCs are present
                  StFit_pos_indep_time(j) = w_avg
               endif

               StrStatus(j) = 1
               StFit_Position(j) = StCorrected_data(pair_id, St_TDC1)
     +              - StCorrected_data(pair_id, St_TDC2)
               StFit_sector(j) = sec(ih)
               StFit_length(j) = StWhichTrkLength(ih, pair_id)
               TrkNum(j) = StWhichTrkSec(ih, sec(ih))
c---- End single track case ------------------------------------------------------

            else if ( (Ntrk_hit_ST(pair_id, 1) .eq. 1) .and.
     +                (Ntrk_hit_ST(pair_id, 2) .eq. 1) ) then
c---------------------------------------------------------------------------------
c--   this is the case when there is one track in each side (2 tracks in  this pair)
c---------------------------------------------------------------------------------

               lm(1) = 999.
               lm(2) = 999

               do i = 1, StNconvertedHits(pair_id) ! find closest tracks
                  sec(i) = StWhichSector(i,pair_id)
                  it(i) = StWhichTrk(i,pair_id)
                  if ( l_st(it(i), pair_id) .le.
     +                 lm(mod(sec(i)-1,2)+1) ) then
                     lm(mod(sec(i)-1,2)+1) = l_st(it(i),pair_id)
                  endif
               enddo

               do i = 1, StNconvertedHits(pair_id)
                  j = j + 1
                  sec(i) = StWhichSector(i, pair_id)
                  it(i) = StWhichTrk(i, pair_id)
                  vslow = StCal_values(veff_leg_near,
     +                 mod(sec(i)-1,2)+1, pair_id)
                  vfast = StCal_values(veff_leg_far,
     +                 mod(sec(i)-1,2)+1, pair_id)

                  if (mod(sec(i), 2) .ne. 0) then ! first sector in pair

                     if (nhit_st(pair_id) .eq. 1
     +                    .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                        t1 = StCorrected_data(pair_id, St_TDC1) -
     +                       l_st(it(i), pair_id)/vslow
                        StFit_pos_indep_time(j) = t1
                     else if (nhit_st(pair_id) .eq. 2) then ! use the other sector TDC
                        t2 = StCorrected_data(pair_id, St_TDC2) -
     +                       lm(2)/vslow - (2.*L_total-lm(2) -
     +                       l_st(it(i), pair_id))/vfast
                        StFit_pos_indep_time(j) = t2
                     endif

                  else if (mod(sec(i), 2) .eq. 0) then ! second sector in pair

                     if (nhit_st(pair_id) .eq. 2
     +                    .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                        t2 = StCorrected_data(pair_id, St_TDC2) -
     +                       l_st(it(i), pair_id)/vslow
                        StFit_pos_indep_time(j) = t2
                     else if(nhit_st(pair_id) .eq. 1) then ! use the other sector TDC
                        t1 = StCorrected_data(pair_id, St_TDC1) -
     +                       lm(1)/vslow - (2.*L_total-lm(1) -
     +                       l_st(it(i), pair_id))/vfast
                        StFit_pos_indep_time(j) = t1
                     endif

                  endif

                  StrStatus(j) = 2
                  StFit_Position(j) = StCorrected_data(pair_id, St_TDC1)
     +                 - StCorrected_data(pair_id, St_TDC2)
                  StFit_sector(j) = sec(i)
                  StFit_length(j) = StWhichTrkLength(i, pair_id)
                  TrkNum(j) = StWhichTrkSec(i, sec(i))
               enddo
c---- End case one track in each side, two total ------------------------------------------

            else if ( (Ntrk_hit_ST(pair_id, 1) .eq. 2  .and.
     1                 Ntrk_hit_ST(pair_id, 2) .eq. 0) .or.
     2                (Ntrk_hit_ST(pair_id, 2) .eq. 2  .and.
     3                 Ntrk_hit_ST(pair_id, 1) .eq. 0) ) then
c-----------------------------------------------------------------------------------------
c--   this is the case when there are 2 tracks in either side(2 tracks/side, 2 tracks/pair)
c-----------------------------------------------------------------------------------------
               lmin = 999.
               lmax = 0.
               do i = 1, StNconvertedHits(pair_id) ! search for closest track
                  sec(i) = StWhichSector(i, pair_id)
                  it(i) = StWhichTrk(i, pair_id)
                  if (l_st(it(i), pair_id) .le. lmin)
     +                 lmin = l_st(it(i), pair_id)
                  if (l_st(it(i), pair_id) .ge. lmax)
     +                 lmax = l_st(it(i), pair_id)
               enddo

               do i = 1, StNconvertedHits(pair_id)
                  j = j + 1
                  sec(i) = StWhichSector(i, pair_id)
                  it(i) = StWhichTrk(i, pair_id)

                  vslow = StCal_values(veff_leg_near,
     +                 mod(sec(i)-1,2)+1, pair_id)
                  vfast = StCal_values(veff_leg_far,
     +                 mod(sec(i)-1,2)+1, pair_id)

                  if (mod(sec(i),2) .ne. 0) then ! first sector in pair

                     if (nhit_st(pair_id) .eq. 1
     +                    .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                        t1 = StCorrected_data(pair_id, St_TDC1)
     +                       - ( 2.*lmin - l_st(it(i),pair_id) )/vslow
                        StFit_pos_indep_time(j) = t1
                     else if (nhit_st(pair_id) .eq. 2) then ! use the other sector TDC
                        t2 = StCorrected_data(pair_id, St_TDC2)
     +                       - ( 2.*(L_total-lmax) + l_st(it(i),pair_id) )/vfast
                        StFit_pos_indep_time(j) = t2
                     endif

                  else if(mod(sec(i),2) .eq. 0) then ! second sector in pair

                     if (nhit_st(pair_id) .eq. 2
     +                    .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                        t2 = StCorrected_data(pair_id, St_TDC2)
     +                       - (2.*lmin - l_st(it(i),pair_id))/vslow
                        StFit_pos_indep_time(j) = t2
                     else if (nhit_st(pair_id) .eq. 1) then ! use the other sector TDC
                        t1 = StCorrected_data(pair_id, St_TDC1)
     +                       - ( 2.*(L_total-lmax) + l_st(it(i),pair_id) )/vfast
                        StFit_pos_indep_time(j) = t1
                     endif

                  endif

                  StrStatus(j) = 3
                  StFit_Position(j) = StCorrected_data(pair_id, St_TDC1)
     +                 - StCorrected_data(pair_id, St_TDC2)
                  StFit_sector(j) = sec(i)
                  StFit_length(j) = StWhichTrkLength(i, pair_id)
                  TrkNum(j) = StWhichTrkSec(i, sec(i))
               enddo
c-------------------------------------------------------------------------
c--------------------------------------------------------------------------
c     more than 2 tracks per pair_id.
c
c-------------------------------------------------------------------------
            else if ((Ntrk_hit_ST(pair_id,1)
     +              + Ntrk_hit_ST(pair_id,2)) .gt. 2) then
c-------------------------------------------------------------------------
               if ( (Ntrk_hit_ST(pair_id,2) .eq. 0) .or.
     +              (Ntrk_hit_ST(pair_id,1) .eq. 0) ) then
c--------------------------------------------------------------------------
c     all tracks in one side
c--------------------------------------------------------------------------
                  lmin = 999.
                  lmax = 0.
                  do i = 1, StNconvertedHits(pair_id) ! find closest track
                     sec(i) = StWhichSector(i,pair_id)
                     it(i) = StWhichTrk(i,pair_id)
                     if (l_st(it(i),pair_id) .le. lmin) then
                        lmin = l_st(it(i),pair_id)
                     endif
                     if (l_st(it(i),pair_id) .ge. lmax) then
                        lmax = l_st(it(i),pair_id)
                     endif
                  enddo

                  do i = 1, StNconvertedHits(pair_id)
                     j = j + 1
                     sec(i) = StWhichSector(i,pair_id)
                     it(i) = StWhichTrk(i,pair_id)

                     vslow = StCal_values(veff_leg_near,
     +                    mod(sec(i)-1,2)+1,pair_id)
                     vfast = StCal_values(veff_leg_far,
     +                    mod(sec(i)-1,2)+1, pair_id)

                     if (mod(sec(i),2) .ne. 0) then

                        if (nhit_st(pair_id) .eq. 1
     +                       .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                           t1 = StCorrected_data(pair_id, St_TDC1)
     +                          - (2.*lmin - l_st(it(i),pair_id))/vslow
                           StFit_pos_indep_time(j) = t1
                        else if  (nhit_st(pair_id) .eq. 2) then
                           t2 = StCorrected_data(pair_id, St_TDC2)
     +                          - ( 2.*(L_total-lmax) + l_st(it(i),pair_id) )/vfast
                           StFit_pos_indep_time(j) = t2
                        endif

                     else if (mod(sec(i),2) .eq. 0) then

                        if (nhit_st(pair_id) .eq. 2
     +                       .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                           t2 = StCorrected_data(pair_id, St_TDC2)
     +                          - (2.*lmin - l_st(it(i),pair_id))/vslow
                           StFit_pos_indep_time(j) = t2
                        else if (nhit_st(pair_id) .eq. 2) then
                           t1 = StCorrected_data(pair_id, St_TDC1)
     +                          - ( 2.*(L_total-lmax) + l_st(it(i),pair_id) )/vfast
                           StFit_pos_indep_time(j) = t1
                        endif

                     endif

                     StrStatus(j) = 4 ! we will use status 4 for these hits
                     StFit_Position(j) = StCorrected_data(pair_id, St_TDC1)
     1                    - StCorrected_data(pair_id, St_TDC2)
                     StFit_sector(j) = sec(i)
                     TrkNum(j) = StWhichTrkSec(i,sec(i))
                     StFit_length(j) = StWhichTrkLength(i,pair_id)
                  enddo
c---- End case all tracks in one side and total more than 2 -----------------

               else if ( (Ntrk_hit_ST(pair_id,2) .ne. 0) .and.
     +                 (Ntrk_hit_ST(pair_id,1) .ne. 0) ) then
c----------------------------------------------------------------------------
c--   we have tracks in both sides and more than 2 in total
c----------------------------------------------------------------------------
                  lm(1) = 999.
                  lm(2) = 999.

                  do i = 1, StNconvertedHits(pair_id) ! find closest tracks
                     sec(i) = StWhichSector(i,pair_id)
                     it(i) = StWhichTrk(i,pair_id)
                     if ( l_st(it(i), pair_id) .le.
     +                    lm(mod(sec(i)-1,2)+1) ) then
                        lm(mod(sec(i)-1,2)+1) = l_st(it(i),pair_id)
                     endif
                  enddo
 
                  do i = 1, StNconvertedHits(pair_id)
                     j = j + 1
                     sec(i) = StWhichSector(i,pair_id)
                     it(i) = StWhichTrk(i,pair_id)
 
                     vslow = StCal_values(veff_leg_near,
     +                    mod(sec(i)-1,2)+1,pair_id)
                     vfast = StCal_values(veff_leg_far,
     +                    mod(sec(i)-1,2)+1, pair_id)
 
                     if (mod(sec(i),2) .ne. 0) then
 
                        if (nhit_st(pair_id) .eq. 1
     +                       .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                           t1 = StCorrected_data(pair_id,St_TDC1)
     +                          - (2.*lm(1) - l_st(it(i),pair_id))/vslow
                           StFit_pos_indep_time(j) = t1
                        else if (nhit_st(pair_id) .eq. 2) then
                           t2 = StCorrected_data(pair_id, St_TDC2)
     +                          - lm(2)/vslow- (2.*L_total-lm(2)
     +                          - l_st(it(i),pair_id) )/vfast
                           StFit_pos_indep_time(j) = t2
                        endif
 
                     else if (mod(sec(i),2) .eq. 0) then
 
                        if (nhit_st(pair_id) .eq. 2
     +                       .or.nhit_st(pair_id) .eq. 3) then ! this sector TDC is here
                           t2 = StCorrected_data(pair_id,St_TDC2)
     +                          - (2.*lm(2) - l_st(it(i),pair_id))/vslow
                           StFit_pos_indep_time(j) = t2
                        else if (nhit_st(pair_id) .eq. 1) then
                           t1 = StCorrected_data(pair_id, St_TDC1)
     +                          - lm(1)/vslow- (2.*L_total-lm(1)
     +                          - l_st(it(i),pair_id) )/vfast
                           StFit_pos_indep_time(j) = t1
                        endif
 
                     endif
 
                     StrStatus(j) = 5
                     StFit_Position(j) = StCorrected_data(pair_id,St_TDC1)
     1                    - StCorrected_data(pair_id,St_TDC2)
                     StFit_sector(j) = sec(i)
                     StFit_length(j) = StWhichTrkLength(i,pair_id)
                     TrkNum(j) = StWhichTrkSec(i,sec(i))
                  enddo
 
               endif
c---- End case more than 2 tracks per pair -----------------------------
 
            endif               ! all possibilities exploared
         endif                  ! finshed dwith this pair
      enddo                     ! end loop over all pairs
 
      StNfit = j                ! we managed to get timing for that many tracks
 
      RETURN
      END
c---  THE END  ---------------------------------------------------------









      SUBROUTINE Stn_fit(ok)
c
c_begin_doc
c  RCS ID string
c  $Id: st_fit.F,v 1.22 2008/03/16 04:33:24 fklein Exp $
c
c  Documentation for subroutine st_fit
c
c  Purpose: Get position corrected timing  
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     - handle the event that has 2 tracks in one sector :Burin98
c     - handle events with more than 2 tracks per pair (EAP) November-2001
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
c#include "bcs.inc"

#include "wcs.inc"
#include "st_general.PAR"
#include "stconvert.CMN"
#include "stgeom.CMN"
#include "sthit.CMN"
#include "stfit.CMN"
#include "stcal.CMN"
#include "sttcl.CMN"
#include "sttrk.CMN"

c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok
c     
c  Local pre-defined variables:
c  ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: st_fit.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.22 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:24 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1     '$Id: st_fit.F,v 1.22 2008/03/16 04:33:24 fklein Exp $
     3     '/
c     Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='st_fit')
      PARAMETER (CRAUTH='Francois Roudot')
      INTEGER NWRITE
c     
c     Local st defined variables:
c     ---------------------------
      character*132 crmess
      integer i,j,pd_id,it(5*max_track_per_sector),
     +     sec(5*max_track_per_sector),ih, max_i
      real t1,t2, a1,a2, lmin, lmax, itmin, itmax
      real lm(2), itm(2)
      real tprop, vfast,vslow, nosecorr0, nosecorr2, lnose
      real average, w_avg
      integer nami_stgm,ind_stgm,MAMIND
      real L_leg,L_nose,L_total
c     _end_var
c     
c     executable code for routine st_fit:
c     -------------------------------------
c     
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='ST_FIT CALLED'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
c--   get geometry info from STG bank
      
      nami_stgm=MAMIND(jw,'STG ')
      ind_stgm=jw(nami_stgm)

      
      L_leg = rjw(ind_stgm+15) 
      L_nose = rjw(ind_stgm+16)
      L_total = L_leg + L_nose
      
      j = 0                     ! reset track counter
      
      do pd_id = 1,ST_max_pair          ! loop over all pairs
         if (trk_found(pd_id) .eqv. .true.) then 
c--------------------------------------------------------
c--   there are track(s) in this pair 
c--   will try to get best timing depending on how many
c--------------------------------------------------------
            if ( Ntrk_hit_ST(pd_id,1).eq. 1) then 
c----------------------------------------------------------------
c--   this is the case when there is only one track in each paddle       
c----------------------------------------------------------------

               a1 = StCorrected_data(pd_id, St_ADC1)
               
               ih      = StNconvertedHits(pd_id)
               if (ih.gt.0.and.ih.le.5*max_track_per_sector) then
                  it(ih)  = StWhichTrk(ih,pd_id)
                  sec(ih) = StWhichSector(ih,pd_id)
                  
                  if (it(ih).gt.0.and.sec(ih).le.6.and.sec(ih).gt.0) then
                     j = j + 1
                     vslow   = StnCal_values(veff_leg,pd_id)
                     vfast   = StnCal_values(veff_nose,pd_id)
                     nosecorr0   = StnCal_values(veff_nose1,pd_id)
                     nosecorr2   = StnCal_values(veff_nose2,pd_id)
                     if (l_st(it(ih), pd_id) .le. L_leg) then
                        t1      = StCorrected_data(pd_id, St_TDC1)  
     +                       - l_st(it(ih), pd_id)/vslow
                     else 
                        lnose = l_st(it(ih), pd_id) - L_leg
                        t1    = StCorrected_data(pd_id, St_TDC1)  
     +                       - L_leg/vslow
     +                       - lnose/vfast - nosecorr0 - nosecorr2*lnose*lnose
                     endif
                  
                     StFit_pos_indep_time(j) = t1
                     
                     StrStatus(j)      = 1  
                     StFit_Position(j) = l_st(it(ih), pd_id)
                     StFit_Energy(j)   = a1
                     StFit_sector(j)   = sec(ih)
                     StFit_paddle(j)   = pd_id-(sec(ih)-1)*4
                     StFit_length(j)   = StWhichTrkLength(ih, pd_id)
                     TrkNum(j)         = StWhichTrkSec(ih,pd_id)
                     StFit_where(j)    = StWhereHit(ih,pd_id)
                  endif
               endif
c---- End case one track in each paddle,       ------------------------------------------

            else if ( Ntrk_hit_ST(pd_id, 1) .ge. 2) then
c-----------------------------------------------------------------------------------------
c--   this is the case when there are 2 tracks in either side(2 tracks/side, 2 tracks/pair)
c-----------------------------------------------------------------------------------------
               lmin = 999.
               do i = 1, StNconvertedHits(pd_id) ! search for closest track
                  sec(i) = StWhichSector(i, pd_id)
                  it(i) = StWhichTrk(i, pd_id)
                  if (it(i).gt.0.and.l_st(it(i), pd_id) .le. lmin) lmin = l_st(it(i), pd_id)
               enddo 

               max_i = StNconvertedHits(pd_id)  ! limit to size of sec() and it()
               if (max_i.gt.5*max_track_per_sector) max_i = 5*max_track_per_sector

               do i = 1, max_i
                  sec(i) = StWhichSector(i, pd_id)
                  it(i)  = StWhichTrk(i, pd_id)

                  if (it(i).gt.0.and.sec(i).le.6.and.sec(i).gt.0) then
                     j = j + 1
                     vslow  = StnCal_values(veff_leg, pd_id)
                     vfast   = StnCal_values(veff_nose,pd_id)
                     nosecorr0   = StnCal_values(veff_nose1,pd_id)
                     nosecorr2   = StnCal_values(veff_nose2,pd_id)
                     
                     
                     a1 = StCorrected_data(pd_id, St_ADC1)
                     if(l_st(it(i),pd_id).le. L_leg) then
                        t1     = StCorrected_data(pd_id, St_TDC1)
     +                       - ( 2.*lmin - l_st(it(i),pd_id) )/vslow
                     else 
                        if(lmin.le.L_leg) then                        
                           lnose = l_st(it(i),pd_id) - L_leg
                           t1 = StCorrected_data(pd_id, St_TDC1) 
     +                          - lmin/vslow +(L_leg-lmin)/vslow
     +                          + lnose/vfast + nosecorr0 + nosecorr2*lnose*lnose
                        else
                           lnose = lmin - L_leg
                           t1 = StCorrected_data(pd_id, St_TDC1) 
     +                          - L_leg/vslow 
     +                          - lnose/vfast - nosecorr0 - nosecorr2*lnose*lnose
     +                          + (l_st(it(i),pd_id)-lmin)/vfast + nosecorr0 + nosecorr2*(l_st(it(i),pd_id)-lmin)**2                        
                        endif
                     endif
                     StFit_pos_indep_time(j) = t1
                     
                     StrStatus(j)      = 3
                     StFit_Position(j) = l_st(it(i),pd_id)
                     StFit_Energy(j)   = a1
                     StFit_sector(j)   = sec(i)
                     StFit_paddle(j)   = pd_id-(sec(i)-1)*4
                     StFit_length(j)   = StWhichTrkLength(i, pd_id)
                     TrkNum(j)         = StWhichTrkSec(i, pd_id)
                     StFit_where(j)    = StWhereHit(i,pd_id)
                  endif
               enddo 
c-------------------------------------------------------------------------
c---- End case more than 2 tracks per pair -----------------------------
               
            endif               ! all possibilities exploared     
         endif                  ! finshed dwith this pair
      enddo                     ! end loop over all pairs
      
      StNfit = j                ! we managed to get timing for that many tracks
      
      RETURN
      END
c---  THE END  ---------------------------------------------------------



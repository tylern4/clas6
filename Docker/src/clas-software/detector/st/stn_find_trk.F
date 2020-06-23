      SUBROUTINE Stn_find_trk(ok,Trk_Result_BOS_name)
c
c_begin_doc
c  RCS ID string
c  $Id: stn_find_trk.F,v 1.5 2004/10/26 16:50:54 devita Exp $
c
c  Documentation for subroutine st_find_trk
c
c  Purpose:  
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
c     - determine whether the hits from tracking are in the boundary of ST
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
#include "bcs.inc"
#include "wcs.inc"
#include "st_general.PAR"
#include "sthit.CMN"
#include "stgeom.CMN"
#include "sttrk.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok
      character*4 Trk_Result_BOS_name
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: stn_find_trk.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/10/26 16:50:54 $')
      PARAMETER ( CAUTHO = '$Author: devita $')
      DATA CRCSID/
     1     '$Id: stn_find_trk.F,v 1.5 2004/10/26 16:50:54 devita Exp $
     3     '/
c     Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='STFINDTRK')
      PARAMETER (CRAUTH='Francois Roudot')
c     
c     Local st defined variables:
c     ---------------------------
      character*132 crmess
      integer planes_per_track
      parameter(planes_per_track=10)
      integer MLINK		!= BOS routine
      integer idx,i,j,pair_id,itrk(St_max_pair)
      logical trkfound,found
      integer isec,ncol,nd,k,itry
      real  xx1,yy1,zz1,xx2,yy2,zz2,vx,vy,vz,l1,l2
      real leg_z_max,leg_z_min,nose_z_max,nose_z_min,nose_angle
      real pd_y_center,leg_y_max,leg_y_min
      real d_trk_hit,dmin_trk_hit
      integer pd_id,jmin_trk_hit
      integer nami_stgm,ind_stgm
      integer MAMIND
      integer btr_ind, indt, htr_ind, indh
      integer trk_id
      REAL pi
      DATA pi/3.14159265/
      DATA nami_stgm /0/
c     _end_var
c     
c  executable code for routine st_find_trk:
c     -------------------------------------
c     
c     Store in StTrknumber the number of tracks seen by one coupled-paddle
c     
      found=.FALSE.
      
c     read in geometry bank STGN
      
      if(nami_stgm.eq.0) nami_stgm=MAMIND(jw,'STG ')
      ind_stgm=jw(nami_stgm)
      
      leg_y_max=rjw(ind_stgm+4) 
      leg_y_min=rjw(ind_stgm+5) 

      leg_z_max=rjw(ind_stgm+6) 
      leg_z_min=rjw(ind_stgm+7) 
      
      nose_z_max=rjw(ind_stgm+12)
      nose_z_min=rjw(ind_stgm+13)
      nose_angle=rjw(ind_stgm+14)
      
      
      do i=1,St_max_pair
         trk_found(i)=.false.            
         StTrknumber(i)=0
         ntrk_hit_leg(i,1) = 0
         ntrk_hit_nose(i,1) = 0
         do j=1,St_max_hit
            do k=1,ST_max_hit
               StFoundTrk(k,j,i) = 0
            enddo
         enddo  
         itrk(i) = 0            ! count number of tracks hit ST in each sector
      enddo 

      
c--   first let's check if we have any tacks and their number does not exceed
c--   ST_max_hit
      
      if(Trk_Result_BOS_name .eq. 'HDPL') then
         btr_ind=MAMIND(IW,'HBTR')
      else if(Trk_Result_BOS_name .eq. 'TDPL') then
         btr_ind=MAMIND(IW,'TBTR')
      endif
      
      indt=IW(btr_ind)
      
c if HBTR/TBTR banks are found and number of tracks is not zero then ...
      if ( indt.gt.0 .and. IW(indt) .gt. 0 .and. IW(indt-4) .le.St_max_sector*max_track_per_sector) then !(ifbtr)

c loop over CLAS sectors
         do i=1,6               ! (do1)
            idx=MLINK(iw,Trk_Result_BOS_name,i)
            if(idx.gt.0) then   !(if1)
               isec = IW(idx-2)
               ncol = IW(idx-5)
               nd   = IW(idx)-1
c check if number of track in the sector is not zero             
               if(nd.GT.0) then !(if2)
c we will truncate if number of tracks in sector > max_track_per_sector=10
                  if(iw(idx-4)/10 .gt. max_track_per_sector) then
                     nd=planes_per_track*ncol*(max_track_per_sector-1)
c                     write(CRMESS,'(a,i2,a,i2,a)') 
c     +                    'More than ', max_track_per_sector, 
c     +                    ' tracks in sector ', 
c     +                    isec, ', will truncate'
c                     call RECMES(CRNAME,'W',CRMESS)
                  endif
                  ntr = 0
                  do k=0,nd,planes_per_track*ncol !(do2)     
                     ntr = ntr+1 
                     vx = RW(idx+2+k)
                     vy = RW(idx+3+k)
                     vz = RW(idx+4+k)
                     xx1 = RW(idx+ncol+2+k)
                     yy1 = RW(idx+ncol+3+k) 
                     zz1 = RW(idx+ncol+4+k)
                     l1  = RW(idx+ncol+8+k)
                     xx2 = RW(idx+2*ncol+2+k)
                     yy2 = RW(idx+2*ncol+3+k) 
                     zz2 = RW(idx+2*ncol+4+k)
                     l2  = RW(idx+2*ncol+8+k) 
c if the tracking gives 2 hit planes, select the one that has shortest track length
                     if(xx1.ne.1000..and.xx2.ne.1000.) then
                        if(l1.gt.l2) then
                           xx1 = 1000.
                           yy1 = 1000.
                           zz1 = 1000.
                        else
                           xx2 = 1000.
                           yy2 = 1000.
                           zz2 = 1000.
                        endif
                     endif
               
c loop over ST hits to find the closest    
                     dmin_trk_hit=9999.
                     pair_id=0
                     do j=1,StNHits !(do3)
c check if hits is in the same sector of the tracks
                        if(StSecHit(j).eq.i) then
                           pd_id      = StFoundHit(j)-(StSecHit(j)-1)*4
                           pd_y_center= leg_y_min+(pd_id-1+0.5)*(leg_y_max-leg_y_min)/4
                           if(yy1.ne.1000) then
                              d_trk_hit=abs(yy1-pd_y_center)
                           else
                              d_trk_hit=abs(yy2-pd_y_center)
                           endif
c look for closest hit if more than one
                           if(d_trk_hit.lt.dmin_trk_hit) then
                              dmin_trk_hit=d_trk_hit
                              jmin_trk_hit=j
                              pair_id=StFoundHit(j)                              
                           endif
                        endif
                     enddo      !(do3)                   
c if hit in the same sector is found then save corresponding information
                     if(pair_id.ne.0) then
                        StTrknumber(pair_id)=StTrknumber(pair_id)+1
                        itrk(i) = itrk(i)+1
                        StFoundTrk(jmin_trk_hit,StTrknumber(pair_id),pair_id) = StTrknumber(pair_id)
                        StTrkSector(StTrknumber(pair_id),pair_id)  = i
c                        StTrkNumSec(StTrknumber(pair_id),pair_id)  = itrk(i)
                        StTrkNumSec(StTrknumber(pair_id),pair_id)  = ntr
                        if(zz1.ge.leg_z_min.and.zz1.le.leg_z_max) then
                           l_st(StTrknumber(pair_id),pair_id)         = (zz1)-leg_z_min
                           StTrkLength(StTrknumber(pair_id),pair_id)  = sqrt((xx1-vx)**2+(yy1-vy)**2+(zz1-vz)**2)                     
                           trk_found(pair_id)        =.true.
                           ntrk_hit_leg(pair_id,1)   = ntrk_hit_leg(pair_id,1)+1
                           StTrkWhere(StTrknumber(pair_id),pair_id) = 1
                           found=.TRUE.
                        elseif(zz2.ge.nose_z_min.and.zz2.le.nose_z_max) then
                           l_st(StTrknumber(pair_id),pair_id) = (leg_z_max-leg_z_min) +(zz2-leg_z_max)/cos(nose_angle/180.*pi)
                           StTrkLength(StTrknumber(pair_id),pair_id) = sqrt((xx2-vx)**2+(yy2-vy)**2+(zz2-vz)**2)
                           trk_found(pair_id)=.true.
                           ntrk_hit_nose(pair_id,1) = ntrk_hit_nose(pair_id,1)+1
                           StTrkWhere(StTrknumber(pair_id),pair_id) = 2
                           found=.TRUE.
                        endif      	
                     endif    
 1001                continue   ! next track
                  enddo         !(do2)
               endif            !(if2)
            endif               !(if1)
         enddo                  !(do1)
      endif                     !(ifbtr)
c     
c     if nothing were found, it might be because the tracking wasn't turn on
c     
      if(.not.found) then
         do i=1,StNHits
            pair_id=StFoundHit(i)
            StTrknumber(pair_id)=-1
         enddo
	 ok = .false.         
      else
         ok = .true.  
      endif
      
      RETURN
      END
c
c----------------------------------------------------------------------

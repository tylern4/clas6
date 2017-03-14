      SUBROUTINE St_find_trk(ok,Trk_Result_BOS_name)
c
c_begin_doc
c  RCS ID string
c  $Id: st_find_trk.F,v 1.19 2002/01/24 17:32:42 pasyuk Exp $
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
      PARAMETER ( CFILE = '$RCSfile: st_find_trk.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.19 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2002/01/24 17:32:42 $')
      PARAMETER ( CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/
     1     '$Id: st_find_trk.F,v 1.19 2002/01/24 17:32:42 pasyuk Exp $
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
      integer idx,i,j,pair_id,itrk(6)
      logical trkfound,found
      integer isec,ncol,nd,k
      real  xx1,yy1,zz1,xx2,yy2,zz2,vx,vy,vz,l1,l2
      real leg_z_max,leg_z_min,nose_z_max,nose_z_min
      integer nami_stgm,ind_stgm
      integer MAMIND
      integer btr_ind, indt
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
      
c     read in geometry bank STG
      
      if(nami_stgm.eq.0) nami_stgm=MAMIND(jw,'STG ')
      ind_stgm=jw(nami_stgm)
      
      leg_z_max=rjw(ind_stgm+6) 
      leg_z_min=rjw(ind_stgm+7) 
      
      nose_z_max=rjw(ind_stgm+12)
      nose_z_min=rjw(ind_stgm+13)
      
      do i=1,3
         trk_found(i)=.false.
         do j=1,St_max_hit
            do k=1,ST_max_hit
               StFoundTrk(k,j,i) = 0
            enddo
         enddo  
      enddo 
      
      do i=1,6
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
      
      if ( indt.gt.0 .and. IW(indt) .gt. 0 .and. IW(indt-4) .le.  ST_max_hit) then !(ifbtr)
         
         do i=1,StNHits         !(do1)
            
            pair_id=StFoundHit(i)
            StTrknumber(pair_id)=0
            ntr = 0             ! count number of tracks in each pair
            
            do j=1,2            !(do2)
               ntrk_hit_leg(pair_id,j) = 0
               ntrk_hit_nose(pair_id,j) = 0
               idx=
     &              MLINK(iw,Trk_Result_BOS_name,pair_id_2_sect_id(pair_id,j))
               trkfound=idx.gt.0.
               
               if(trkfound) then !(if1)
                  StTrknumber(pair_id)=StTrknumber(pair_id)+iw(idx-4)/10
                  
                  isec = IW(idx-2)
                  ncol = IW(idx-5)
                  nd   = IW(idx)-1
                  
                  if(nd.GT.0) then !(if2)
                     
C     we will truncate if number of tracks in sector > max_track_per_sector=10
                     if (iw(idx-4)/10 .gt. max_track_per_sector) then
                        nd=planes_per_track*ncol*(max_track_per_sector-1)
                        
                        write(CRMESS,'(a,i2,a,i2,a)') 
     +                       'More than ', max_track_per_sector, 
     +                       ' tracks in sector ', 
     +                       isec, ', will truncate'
                        call RECMES(CRNAME,'W',CRMESS)
                     endif
                     
                     do k=0,nd,planes_per_track*ncol !(do3)
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
                        
c     if the tracking gives 2 hit planes, select the one that has shortest track length
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
                        
                        if(zz1.ge.leg_z_min.and.zz1.le.leg_z_max) then
                           ntr = ntr+1 
                           itrk(isec) = itrk(isec)+1
                           StFoundTrk(i,ntr,pair_id) = ntr
                           StTrkSector(ntr,pair_id) = isec
                           StTrkNumSec(ntr,isec) = itrk(isec)
                           l_st(ntr,pair_id) = (zz1)-leg_z_min
                           StTrkLength(ntr,pair_id) = sqrt((xx1-vx)**2+
     1                          (yy1-vy)**2+(zz1-vz)**2)                     
                           trk_found(pair_id)=.true.
                           ntrk_hit_leg(pair_id,j) = ntrk_hit_leg(pair_id,j)+1
                           found=.TRUE.
                        elseif(zz2.ge.nose_z_min.and.zz2.le.nose_z_max) then
                           ntr = ntr+1
                           StFoundTrk(i,ntr,pair_id) = ntr
                           itrk(isec) = itrk(isec)+1
                           StTrkSector(ntr,pair_id) = isec   
                           StTrkNumSec(ntr,isec) = itrk(isec)                  
                           l_st(ntr,pair_id) = (leg_z_max-leg_z_min) +
     1                          (zz2-leg_z_max)/cos(pi/6.)
                           StTrkLength(ntr,pair_id) = sqrt((xx2-vx)**2+
     1                          (yy2-vy)**2+(zz2-vz)**2)
                           trk_found(pair_id)=.true.
                           ntrk_hit_nose(pair_id,j) = ntrk_hit_nose(pair_id,j)+1
                           found=.TRUE.
                        endif      	
c     endif   ! if ntr<st_max_hits
                     enddo	!(do3)
                  endif         !(if2)
               endif            !(if1)
            enddo		!(do2)
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

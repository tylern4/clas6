      SUBROUTINE GROUP0(axis,layer,sector,iterr,ierr)
c
c     Input Parameters:  (Name   -  Type   - Meaning)
c     ----------------
c     axis   - Integer - U(25),V(26) or W(27) views
c     layer  - Integer - Whole(9),Inner(10) or Outer(11)
c     sector - Integer - # sector (1:6)
c     iterr  - Integer - 
c
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
      integer axis,layer,sector,iterr,ierr
c
      integer id_s(MAXstrips),maxs,i,stripID,mth,member,stID,j,mem
      real estrip(MAXstrips),e_last,group0E(maxpeaks)
      logical lowside,highside
c
      maxs=Ec_stripN(axis,layer,sector)
      do mth=1,Ec_stripN(axis,layer,sector)
         stripID= Ec_stripID(mth,axis,layer,sector) !id# of strip 
         estrip(mth)=Ec_strip(ENERGY,stripID,axis,layer,sector)
         strip_stat(stripID)=0
         do j=1,maxgroups
            strip_efr(j,stripID)=0.0
         enddo
      enddo
      call EcGus_sort(maxs,estrip,id_s)
c
c      print *,'Group 0',layer,axis,Ec_stripN(axis,layer,sector)
c      if(layer.eq.9)return
      do i=1,Ec_stripN(axis,layer,sector)
         stripID= Ec_stripID(id_s(i),axis,layer,sector) !id# of strip 
c         print *,stripID,Ec_strip(ENERGY,stripID,axis,layer,sector),strip_stat(stripID)
         if(strip_stat(stripID).eq.0.and.
     +        Ec_strip(ENERGY,stripID,axis,layer,sector).ge.EcFitEdge_peak_threshold/3.)then
            member=1
            Ngroup=Ngroup+1
            If(Ngroup.gt.Maxpeaks) Then
               EcFit_Npeak(axis,layer,sector)= 0 !Event is skipped
               RETURN
            ENDIF
            strip_stat(stripID)=2
            strip_efr(Ngroup,stripID)=1.
            group_E(Ngroup)= 
     $           Ec_strip(ENERGY,stripID,axis,layer,sector)
            groupN(Ngroup)=member
            groupID(member,Ngroup)=stripID
c
            lowside=.true.
            e_last=Ec_strip(ENERGY,stripID,axis,layer,sector)
c            if(layer.eq.10.or.layer.eq.12)then
c               if(axis.eq.ec_u)print *,'New group ',layer,stripID,e_last
c            endif
            do stID=stripID-1,1,-1
               if(lowside.and.strip_stat(stID).ne.2)then
c check if IDs are real, should be OK with next line
                  if(Ec_strip(ENERGY,stID,axis,layer,sector).GE.EcFitEdge_strip_threshold
     &                 .and.Ec_strip(ENERGY,stID,axis,layer,sector).le.e_last) then
                     call add_member(member,stID,axis,layer,sector)
                     e_last=Ec_strip(ENERGY,stID,axis,layer,sector)
                  elseif(Ec_strip(ENERGY,stID,axis,layer,sector).gt.e_last)then
                     if(stID.eq.1)then
                        call add_member(member,stID,axis,layer,sector)
                     elseif(Ec_strip(ENERGY,stID,axis,layer,sector).gt.Ec_strip(ENERGY,stID-1,axis,layer,sector))then
                        if(stID.eq.2)then
                           call add_member(member,stID,axis,layer,sector)
                           lowside=.false.
                        elseif(Ec_strip(ENERGY,stID-2,axis,layer,sector).gt.EcFitEdge_strip_threshold.and.
     &                          Ec_strip(ENERGY,stID-1,axis,layer,sector).gt.Ec_strip(ENERGY,stID-2,axis,layer,sector))then
                           lowside=.false.
                        else
                           call add_member(member,stID,axis,layer,sector)
                           lowside=.false.
                        endif
                     else
                        lowside=.false.
                     endif
                  else
                     lowside=.false.
                  endif
               endif
            enddo
c
            highside=.true.
            e_last=Ec_strip(ENERGY,stripID,axis,layer,sector)
            do stID=stripID+1,maxwstrips
               if(highside.and.strip_stat(stID).ne.2)then
c check if IDs are real, should be OK with next line
                  if(Ec_strip(ENERGY,stID,axis,layer,sector).GE.EcFitEdge_strip_threshold
     &                 .and.Ec_strip(ENERGY,stID,axis,layer,sector).le.e_last) then
                     call add_member(member,stID,axis,layer,sector)
                     e_last=Ec_strip(ENERGY,stID,axis,layer,sector)
                  elseif(Ec_strip(ENERGY,stID,axis,layer,sector).gt.e_last)then
                     if(stID.eq.maxwstrips)then
                        call add_member(member,stID,axis,layer,sector)
                     elseif(Ec_strip(ENERGY,stID,axis,layer,sector).gt.Ec_strip(ENERGY,stID+1,axis,layer,sector))then
                        if(stID.eq.maxwstrips-1)then
                           call add_member(member,stID,axis,layer,sector)
                           highside=.false.
                        elseif(Ec_strip(ENERGY,stID+2,axis,layer,sector).gt.EcFitEdge_strip_threshold.and.
     &                          Ec_strip(ENERGY,stID+1,axis,layer,sector).gt.Ec_strip(ENERGY,stID+2,axis,layer,sector))then
                           highside=.false.
                        else
                           call add_member(member,stID,axis,layer,sector)
                           highside=.false.
                        endif
                     else
                        highside=.false.
                     endif
                  else
                     highside=.false.
                  endif
               endif
            enddo
         endif
      enddo
c   First get the sum of energies of all strips that are present in one group only
      do i=1,ngroup
         group0E(i)=0.
         do member=1,groupn(i)
            stID=groupID(member,i)
            if(strip_stat(stID).gt.0)then
               group0E(i)=group0E(i)+Ec_strip(ENERGY,stID,axis,layer,sector)
            endif
         enddo
      enddo
c   Than share the energy of strips that are in two groups
      do i=1,ngroup
         do member=1,groupn(i)
            stID=groupID(member,i)
            if(strip_stat(stID).eq.-1)then
               if(i.eq.ngroup)print *,'Wrong status for the strip ',stID,i
               do j=i+1,ngroup
                  do mem=1,groupn(j)
                     if(stID.eq.groupID(mem,j))then
                        strip_efr(i,stID)=group0E(i)/(group0E(i)+group0E(j))
                        strip_efr(j,stID)=1.-strip_efr(i,stID)
                        strip_stat(stID)=-2
                     endif
                  enddo
               enddo
            endif
         enddo
      enddo
c   Now get the sum of energies of all strips with proper energy sharing
      do i=1,ngroup
         group_E(i)=0
         do member=1,groupn(i)
            stID=groupID(member,i)
            if(strip_stat(stID).ne.-1)then
               group_E(i)=group_E(i)+Ec_strip(ENERGY,stID,axis,layer,sector)*strip_efr(i,stID)
            endif
         enddo
      enddo
c     
      return
      end
c
      subroutine add_member(member,stID,axis,layer,sector)
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
      integer member,stID,axis,layer,sector
c
      member=member+1
      group_E(Ngroup)= group_E(Ngroup) +
     &     Ec_strip(ENERGY,stID,axis,layer,sector)
      groupN(Ngroup)=member
      groupID(member,Ngroup)=stID
      strip_efr(Ngroup,stID)=1.
c      print *,'New member ',ngroup,member,stID,strip_stat(stID),Ec_strip(ENERGY,stID,axis,layer,sector)
      if(strip_stat(stID).eq.0)then
         strip_stat(stID)=1
      else
         strip_stat(stID)=-1
      endif
c     
      return
      end

            SUBROUTINE GTRIG_TIME(ntrig_stat,trkstat)
c     
c     _begin_doc
c     
c     Documentation for subroutine GTRIG_TIME
c     
c     Purpose: For photon beam, use info from ST, SC/EC and TRK to get
C     start-time.
c     -------- 
c     
c     Input Parameters:   
c     ----------------      
c     
c     
c     Output Parameters: TPARTICLES =1 if triggered particle was found
c     -----------------  
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    Burin Asavapibhop
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
c     BOS common block  uncomment the next line for BOS include file
#include "clas_offsets.inc"
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "hbt_planes.inc"
#include "piddef.inc"
#include "pdgid.inc"
#include "echits.inc"
#include "sthits.inc"
#include "particle_out.inc"
#include "schits.inc"
#include "dc_inf.inc"
#include "ec_inf.inc"
#include "sc_inf.inc"
#include "lec_inf.inc"
#include "st_inf.inc"
#include "tag_hits.inc"
#include "tag_inf.inc"
#include "photon_stuff.inc"
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
      PARAMETER (CFILE = '$RCSfile: gtrig_time.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.23 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:23 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1     '$Id: gtrig_time.F,v 1.23 2008/03/16 04:33:23 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GTRIG_TIME')
      PARAMETER (CRAUTH='Burin Asavapibhop')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      INTEGER iprt,np_dc,np_sc,np_ec,np_st,np_lc,indx,ndel,idel(MaxParticles),jdel(MaxParticles)
      INTEGER trigger_status, trig_stat, ntrig,i,j,k,keep,ist,itag,nostdc(MaxParticles)
      INTEGER t0_stat(MaxParticles),best_st,better_st,good_st,trig_ok,trkstat,nosttrig
      INTEGER st_stat1,st_stat2,st_stat3,all_stat,mtrig,itrig,ntrig_stat,np_cc
      INTEGER ndifsect,idh,ipr1,ipr2,jpr1,jpr2,sect(MaxParticles),strig(MaxParticles),is,js,nostist,nostitag
      REAL td_time, tdr_trk,min,tdiff, rf_corr_time, zpos, zdiff, pel, tdiff2
      REAL beta_sc, beta_ec, t0(MaxParticles), t1, ttrig, st_ttime, tag_ttime(MaxParticles), eneut, t3
      REAL gt_rf_seb,get_gt_rf,tnost(MaxParticles,5), dt, dt3
      REAL t03(2), t03_stat(2), vx3(2), vy3(2),vz3(2)
      REAL vx(MaxParticles),vy(MaxParticles),vz(MaxParticles)
      LOGICAL same
      EXTERNAL get_gt_rf

c     _end_var
c     
c     executable code for routine TRIG_TIME:
c---- 6----------------------------------------------------------------72
c     
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='This routine determines a trigger time.'
         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF
      
c     - Get the triggered particle time  
c     - For photon beam experiments ... Here We Go!
      evid(1) = 0
      evstatus(1) = 0
      event_stat = 0
      trigger_status = 0
      st_ttime = 0.
      goodtag = 0
      keep = 0
      ndel = 0
      ttrig = 0.
      ntrig = 0
      nosttrig = 0
      Ttime = 0.
      mtrig = 0
      best_st = 0
      better_st = 0
      good_st = 0
      trig_ok = 0
      st_stat1 = 0
      st_stat2 = 0
      st_stat3 = 0
      all_stat = 0
      ist = 0
      itag = 0
      nostist=0
      nostitag=0
c
      IF(taghits.eq.0) GOTO 998
      
      Do indx=1,maxparticles
         gindex(indx)=0
         tag_ttime(indx)=0
         idel(indx)=0
         jdel(indx)=0
         strig(indx)=0
      EndDo
      grf=get_gt_rf()
c     IF (taghits.gt.0) trigger_status = 1
      eneut=0.0
      ntrig_stat=0
      trig_stat=0
      DO iprt = 1,gparticles
         np_dc = partstdc(iprt)
         np_sc = partstsc(iprt)
         np_cc = partstsc(iprt)
         np_ec = partstec(iprt)
         np_lc = partstlec(iprt)
         np_st = partstst(iprt)
         trig_stat=0
         IF (np_dc.GT.0.and.(np_ec.gt.0.or.np_sc.gt.0)) THEN
            if(trkstat*dcstatus(np_dc).le.0)goto 90
            If (np_sc.gt.0.and.np_st.gt.0) Then
               trig_stat=2      ! ST+DC+SC
               td_time = sctime(np_sc)-sttime(np_st)
               tdr_trk = scr_trk(np_sc)-str_trk(np_st)
               beta_sc = tdr_trk/td_time/vlight
               t1 = sttime(np_st)-str_trk(np_st)/beta_sc/vlight
            else if (np_ec.gt.0.and.np_st.gt.0) Then
               trig_stat=3      ! ST+DC+EC
               td_time = ectime(np_ec)-sttime(np_st)
               tdr_trk = ecr_trk(np_ec)-str_trk(np_st)
               beta_ec = tdr_trk/td_time/vlight
               t1 = sttime(np_st)-str_trk(np_st)/beta_ec/vlight
            endif 
c         ELSEIF(np_dc.gt.0.AND.sthits.eq.0)THEN
            nosttrig=nosttrig+1
            nostdc(nosttrig)=np_dc
            strig(nosttrig)=1
            pel=dcp(np_dc)
            beta_sc=pel/sqrt(pel**2+mpion**2)
            beta_ec=beta_sc
            If (np_sc.gt.0) Then
               trig_stat=4      ! DC+SC
               tnost(nosttrig,1) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
            else if (np_ec.gt.0) Then
               trig_stat=5      ! DC+EC
               tnost(nosttrig,1) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
            endif 
            strig(nosttrig)=strig(nosttrig)+1
            beta_sc=pel/sqrt(pel**2+mkaon**2)
            beta_ec=beta_sc
            If (np_sc.gt.0) Then
               tnost(nosttrig,2) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
            else if (np_ec.gt.0) Then
               tnost(nosttrig,2) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
            endif 
            if(np_cc.gt.0)then
               strig(nosttrig)=strig(nosttrig)+1
               beta_sc=pel/sqrt(pel**2+melectron**2)
               beta_ec=beta_sc
               If (np_sc.gt.0) Then
                  tnost(nosttrig,strig(nosttrig)) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
               else if (np_ec.gt.0) Then
                  tnost(nosttrig,strig(nosttrig)) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
               endif 
            endif
            if(dcq(np_dc).gt.0.and.trig_stat.gt.3)then
               strig(nosttrig)=strig(nosttrig)+1
               beta_sc=pel/sqrt(pel**2+mproton**2)
               beta_ec=beta_sc
               If (np_sc.gt.0) Then
                  tnost(nosttrig,strig(nosttrig)) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
               else if (np_ec.gt.0) Then
                  tnost(nosttrig,strig(nosttrig)) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
               endif 
               strig(nosttrig)=strig(nosttrig)+1
               beta_sc=pel/sqrt(pel**2+mdeuteron**2)
               beta_ec=beta_sc
               If (np_sc.gt.0) Then
                  tnost(nosttrig,strig(nosttrig)) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
               else if (np_ec.gt.0) Then
                  tnost(nosttrig,strig(nosttrig)) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
               endif 
            endif
         ELSEIF(np_ec.gt.0.)THEN
            if(ece_tot(np_ec).gt.0.and.ectime(np_ec).gt.0.)ntrig_stat=ntrig_stat+1
            if(ece_tot(np_ec).gt.eneut)then
               eneut=ece_tot(np_ec)
               t1 = ectime(np_ec)-ecr_trk(np_ec)/vlight
            endif
         ELSEIF(np_lc.gt.0)THEN
            if(lece(np_lc).gt.0.and.lectime(np_lc).gt.0.)ntrig_stat=ntrig_stat+1
            if(lece(np_lc).gt.eneut)then
               eneut=lece(np_lc)
               t1 = lectime(np_lc)-lecr_trk(np_lc)/vlight
            endif
         ENDIF 
c     Here we look for the best track candidate in ST in order to determine
c     the start time with tagger. If there is any track that has ststatus=1 
c     then use it otherwise use track that has ststatus=2 or 3 respectively.
         IF(trig_stat.gt.1.and.np_st.gt.0)THEN
            if(ststatus(np_st).eq.1) then ! one track per ST pair
               best_st = 1
               trig_ok = 1
               st_stat1 = st_stat1+1
            elseif(ststatus(np_st).eq.2) then  
c     elseif(ststatus(np_st).eq.2.and.best_st.eq.0) then  
               better_st = 1    ! 2 tracks in different side of pair
               trig_ok = 1
               st_stat2 = st_stat2+1
            elseif(ststatus(np_st).eq.3) then
c     elseif(ststatus(np_st).eq.3.and.best_st.eq.0.and.better_st.eq.0) then
               good_st = 1      ! 2 tracks in same side of pair
               trig_ok = 1
               st_stat3 = st_stat3+1
            endif   
            if(trig_ok.eq.1) then
c count each track with ST hits, t1 is the vertex time 
               ntrig = ntrig+1
               t0(ntrig) = t1
               vx(ntrig) = dcvertx(np_dc)
               vy(ntrig) = dcverty(np_dc)
               vz(ntrig) = vzpos(np_st)
               t0_stat(ntrig) = 1000000*ntrig+10000*st_stat3+100*st_stat2+st_stat1
               all_stat = 10000*st_stat3+100*st_stat2+st_stat1
               trigger_status = trig_stat
               trig_ok = 0
            endif 
         ELSEIF(neut_trigg_photon.gt.0.and.ntrig_stat.ge.neut_trigg_photon)THEN
            ntrig=ntrig+1
            t0(ntrig) = t1
            vx(ntrig) = 0.0
            vy(ntrig) = 0.0
            vz(ntrig) = 0.0
            t0_stat(ntrig) = 1000000*ntrig+51
            trigger_status=ntrig_stat
         ENDIF 
 90      Continue
      ENDDO
      call hcdir('//PAWC/SEB',' ')
      if(trkstat.lt.0)then
         call hf1(8990,float(nosttrig),1.)
      elseif(trkstat.gt.0)then
         call hf1(9990,float(nosttrig),1.)
      endif
      if(nosttrig.gt.1)then
         min=100.
         do i=1,nosttrig-1
            do j=i+1,nosttrig
               if(dcsector(nostdc(i)).ne.dcsector(nostdc(j)))then
                  do is=1,strig(i)
                     do js=1,strig(j)
                        dt=tnost(i,is)-tnost(j,js)
                        if(abs(dt).lt.abs(min))then
                           min=dt
                           ipr1=i
                           ipr2=is
                           jpr1=j
                           jpr2=js
                        endif
                     enddo
                  enddo
               endif
            enddo
         enddo
         if(trkstat.lt.0)then
            call hf1(9995,min,1.)
         elseif(trkstat.gt.0)then
            call hf1(9996,min,1.)
         endif
         if(min.lt.90.)then
            call hf1(8991,tnost(ipr1,ipr2),1.)
            call hf1(8992,tnost(jpr1,jpr2),1.)
            t03(1)=tnost(ipr1,ipr2)
            vx3(1) = dcvertx(nostdc(ipr1))
            vy3(1) = dcverty(nostdc(ipr1))
            vz3(1) = dcvertz(nostdc(ipr1))
            t03_stat(1) = 1000052
            t03(2)=tnost(jpr1,jpr2)
            vx3(2) = dcvertx(nostdc(jpr1))
            vy3(2) = dcverty(nostdc(jpr1))
            vz3(2) = dcvertz(nostdc(jpr1))
            t03_stat(2) = 2000052
            trigger_status = 4
            dt3=100.
            do i=1,2
               do j=1,taghits
                  if(trkstat.LT.0.OR.tagstat(j).eq.7.or.tagstat(j).eq.15) then	
                     tdiff = (Tgamma(j)+vz3(i)/vlight)-t03(i)-target_position(3)/vlight
                     if(trkstat.gt.0)then
                        call hf1(8996,tdiff,1.)
                     else
                        call hf1(8995,tdiff,1.)
                     endif
                     if(abs(tdiff).le.abs(dt3)) then
                        nostist = i
                        nostitag = j
                        dt3=tdiff
                     endif
                  endif
               enddo
            enddo
c            if(sthits.eq.0)then
            if(st_stat1.eq.0.and.st_stat2.eq.0.and.st_stat3.eq.0)then
               ntrig=2
               do i=1,2
                  vx(i)=vx3(i)
                  vy(i)=vy3(i)
                  vz(i)=vz3(i)
                  t0(i)=t03(i)
                  t0_stat(i)=t03_stat(i)
               enddo
            endif
         elseif(sthits.eq.0)then
            ntrig=0
            trigger_status = -1
         endif
      endif
c
      if(all_stat.gt.99.and.mod(all_stat,100).ne.0) then
         if(trkstat.gt.0)then
            call hf1(9992,t0(1),1.)
         else
            call hf1(9997,t0(1),1.)
         endif
         do i=1,ntrig
            if(mod(t0_stat(i),100).ne.0.and.mod(t0_stat(i),100).ne.51) then
               mtrig = mtrig+1
               itrig = t0_stat(i)/1000000
               t0(mtrig) = t0(itrig)
               vz(mtrig) = vz(itrig)
            endif
         enddo
         ntrig = mtrig
      endif   
      if(ntrig.gt.MaxParticles) ntrig=MaxParticles  !limit only for MaxParticles tracks
c   
      if(trkstat.gt.0)then
         call hf1(8981,float(trigger_status),1.)
      else
         call hf1(8982,float(trigger_status),1.)
      endif
      IF (trigger_status.EQ.0) GO TO 998
      
c     we have ST hit and TAG hit. Next select the tagger hit that coincident with
c     start counter hit. Here we consider only the tagger hit that has status=7 or 15.
      If (trigger_status.GT.1.and.taghits.gt.0) Then
         min = 100.
         dt=100.
         ist=0
         do i=1,ntrig
            do j=1,taghits
               if(trkstat.LT.0.OR.tagstat(j).eq.7.or.tagstat(j).eq.15) then	
                  tdiff = (Tgamma(j)+vz(i)/vlight)-t0(i)-target_position(3)/vlight
                  call hf1(8993,tdiff,1.)
                  if(abs(tdiff).le.min) then
                     min = abs(tdiff)
                     ist = i
                     itag = j
                     dt=tdiff
                     tvertx = vx(i)
                     tverty = vy(i)
                     tvertz = vz(i)
                  endif
               endif
            enddo
         enddo
         if(abs(dt3).le.abs(dt).and.nostist.gt.0)then
            ist=nostist
            itag=nostitag
            t0(ist)=t03(ist)
            vz(ist)=vz3(ist)
            dt=dt3
            tvertx = vx3(ist)
            tverty = vy3(ist)
            tvertz = vz3(ist)
         endif
         if(ist.ne.0.and.itag.ne.0) then
            if(trkstat.lt.0)then
               call hf1(9991,dt,1.)
            else
               call hf1(9994,dt,1.)
            endif
         else
            goto 998
         endif 
c
         do i = 1,taghits
            tag_ttime(i) = Tgamma(i)+vz(ist)/vlight-target_position(3)/vlight
         enddo         
c     get rid of tagger hits that do not match with ST within +/- st_tagger_match ns.
         do i = 1,taghits
            if(abs(t0(ist)-tag_ttime(i)).le.st_tagger_match.and.
     +           (trkstat.LT.0.OR.tagstat(i).eq.7.or.tagstat(i).eq.15)) then
               goodtag = goodtag+1
               tagptr(goodtag) = i*1000+tagstat(i)
               if(i.eq.itag)tagptr(goodtag) = -tagptr(goodtag)
               tagtime(goodtag) = tag_ttime(i)
               tagenergy(goodtag) = Egamma(i)
               tagTid(goodtag) = Tid(i)
               tagEid(goodtag) = Eid(i)
               dt_sttag(goodtag) = t0(ist)-tag_ttime(i)
               call hf1(9993,tagtime(goodtag),1.)
            endif  
         enddo
         if(goodtag.eq.0) then
            goto 998
          endif
      EndIf
c     get rid of the duplicated hit if there is any         
      do i = 1,goodtag-1
         if(i.eq.1) same=.false.
         do k = 1,ndel
            if(i.eq.idel(k)) same=.true.
         enddo 
         if(.not.same) then 
            do j = i+1,goodtag
               if(tagtime(i).eq.tagtime(j).and.
     &              tagenergy(i).eq.tagenergy(j).and.
     &              tagTid(i).eq.tagTid(j).and.
     &              tagEid(i).eq.tagEid(j)) then
                  ndel = ndel+1
                  idel(ndel) = j
                  jdel(j) = 1
               endif
            enddo
         endif  
      enddo
c     keep the final hits for tagger     	  
      do i=1,goodtag  
         if(jdel(i).ne.1) then	
            keep = keep+1
            tagptr(keep) = tagptr(i)
            tagtime(keep) = tagtime(i)
            tagenergy(keep) = tagenergy(i)
            dt_sttag(keep) = dt_sttag(i)
         endif 
      enddo
c     if there is good tagger time then use it for start time           
      if(keep.ge.1) then
         TTime = tag_ttime(itag)
c         print *,'Gtrig time',ttime,t0(ist) 
      else
         goto 998
      endif   
      if(goodtag.ge.1) goodtag = keep    
      call hf1(8994,ttime,1.)
      
c     I comment this part out(*) to avoid any complication. (12/12/98:burin)  
*     here is the case which has no ST time, use tagger time         
*     Else if(trigger_status.EQ.1.and.ntr_link.gt.0) Then
*     min = 100.
*     do i=1,ntr_link-1
*     do j=i+1,ntr_link
*     zdiff = zphoton(i)-zphoton(j)
*     if(abs(zdiff).le.min) then
*     min = abs(zdiff)
*     zpos = zphoton(i)+zphoton(j)
*     endif  
*     enddo
*     enddo 
*     if(ntr_link.eq.0) then
*     zpos = 0.
*     else if(ntr_link.eq.1) then  
*     zpos = zphoton(1)
*     else      
*     zpos = zpos/2.     ! average on tracks Z-positions
*     endif  
*     
*     do i = 1,taghits	
*     tag_ttime(i) = Tgamma(i)+zpos/vlight
*     enddo
*     
*     do i = 1,taghits
*     if(tagstat(i).eq.7.or.tagstat(i).eq.15) then 
*     tag_ttime(i) = Tgamma(i)+zpos/vlight 
*     if(abs(20.-tag_ttime(i)).le.1.) then
*     CAUTION: the number 20 ns here is taken from the average event start time 
*     from several runs from G1
*     
*     goodtag = goodtag+1
*     tagptr(goodtag) = i
*     tagtime(goodtag) = tag_ttime(i)
*     tagenergy(goodtag) = Egamma(i)
*     tagTid(goodtag) = Tid(i)
*     tagEid(goodtag) = Eid(i)
*     dt_sttag(goodtag) = -tag_ttime(i)
*     endif
*     else
*     goto 998
*     endif  
*     enddo 
*     
*     if(goodtag.eq.0) goto 998 
*     
*     do i = 1,goodtag-1
*     if(i.eq.1) same=.false.
*     do k = 1,ndel
*     if(i.eq.idel(k)) same=.true.
*     enddo 
*     if(.not.same) then 
*     do j = i+1,goodtag
*     if(tagtime(i).eq.tagtime(j).and.
*     &            tagenergy(i).eq.tagenergy(j).and.
*     &	          tagTid(i).eq.tagTid(j).and.
*     &	          tagEid(i).eq.tagEid(j)) then
*     ndel = ndel+1
*     idel(ndel) = j
*     jdel(j) = 1
*     endif
*     enddo
*     endif  
*     enddo
*     
*     do i = 1,goodtag
*     if(jdel(i).ne.1) then
*     keep = keep+1
*     tagptr(keep) = tagptr(i)
*     tagtime(keep) = tagtime(i)
*     tagenergy(keep) = tagenergy(i)
*     dt_sttag(keep) = dt_sttag(i)
*     endif
*     enddo
*     
*     if(goodtag.ge.1) goodtag = keep  
*     
*     min = 100.
*     do i=1,goodtag-1
*     do j=i+1,goodtag
*     tdiff = tagtime(i)-tagtime(j)
*     if(abs(tdiff).le.min) then
*     min = abs(tdiff)
*     ttrig = tagtime(i)+tagtime(j)
*     endif  
*     enddo
*     enddo 
*     if(goodtag.eq.1) then
*     ttime = tagtime(1)
*     else      
*     ttime = ttrig/2.     ! average on TAG hits
*     endif 
c      Else
c         goto 998
c      Endif    
      
c==========================================================================
      event_stat = 1
      nparticles = 1            ! at least found one particle given T0
      tparticles = nparticles  
      DO iprt = 1,gparticles
         np_dc = partstdc(iprt)
         np_sc = partstsc(iprt)
         np_ec = partstec(iprt)
         np_st = partstst(iprt)
         If(np_dc.Gt.0) then
            if(np_sc.GT.0) Then
               partbetta(iprt) = 
     =              scr_trk(np_sc)/(sctime(np_sc)-ttime)/vlight
            else if(np_ec.gt.0) then
               partbetta(iprt) = 
     =              ecr_trk(np_ec)/(ectime(np_ec)-ttime)/vlight
            Else
               partbetta(iprt) = 0.
            endif
         EndIf
      ENDDO
      call hcdir('//PAWC',' ')
      RETURN
 998  CONTINUE
      CRMESS='No triggered particle is found, event is skipped'
c     CALL RECMES(CRNAME,'I',CRMESS)
      event_stat = 0
      gparticles = 0
      TTIME = 0.
      RETURN
      END
c     
c------------------------------------------------------------------------
C ------

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
      PARAMETER (CREVIS = '$Revision: 1.16 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/06/02 15:56:03 $')
      PARAMETER (CAUTHO = '$Author: clasg10 $')
      DATA CRCSID/   
     1     '$Id: gtrig_time.F,v 1.16 2004/06/02 15:56:03 clasg10 Exp $'   
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
      INTEGER iprt,np_dc,np_sc,np_ec,np_st,np_lc,indx,ndel,idel(20),jdel(20)
      INTEGER trigger_status, trig_stat, ntrig,i,j,k,keep,ist,itag,nostdc(20)
      INTEGER t0_stat(20),best_st,better_st,good_st,trig_ok,trkstat,nosttrig
      INTEGER st_stat1,st_stat2,st_stat3,all_stat,mtrig,itrig,ntrig_stat,nosttrig_stat
      INTEGER ndifsect,idh,ipr1,ipr2,jpr1,jpr2,sect(20),strig(20),is,js,nostist,nostitag

      REAL td_time, tdr_trk,min,tdiff, rf_corr_time, vz(20), zpos, zdiff, pel,tdiff2
      REAL beta_sc, beta_ec, t0(20), t1, ttrig, st_ttime, tag_ttime(20), eneut, t3
      INTEGER ind_ord_tg(20),ind_nord_tg(20),nmin
      REAL    tdiff_min,btagptr(20),btagtime(20),btagenergy(20),bdt_sttag(20)
      REAL gt_rf_seb,get_gt_rf,tnost(20,4),dt,t03(2),vz3(2),t03_stat(2),dt3
      REAL t04(20),vz4(20),t04_stat(20),dt4
      INTEGER ist4,itag4,sntrig
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
      nosttrig_stat=0
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
      ist4 = 0
      itag4 = 0
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
c      print *,'Event ',gparticles
      DO iprt = 1,gparticles
         np_dc = partstdc(iprt)
         if(np_dc.le.0) goto 90
         if(trkstat*dcstatus(np_dc).le.0)goto 90
         np_sc = partstsc(iprt)
         np_ec = partstec(iprt)
         np_lc = partstlec(iprt)
         np_st = partstst(iprt)
         trig_stat=0
         IF (np_dc.GT.0) THEN
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
            strig(nosttrig)=2
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

            beta_sc=pel/sqrt(pel**2+mkaon**2)
            beta_ec=beta_sc
            If (np_sc.gt.0) Then
               tnost(nosttrig,2) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
            else if (np_ec.gt.0) Then
               tnost(nosttrig,2) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
            endif 

            if(dcq(np_dc).gt.0.and.trig_stat.gt.3)then
               strig(nosttrig)=3
               beta_sc=pel/sqrt(pel**2+mproton**2)
               beta_ec=beta_sc
               If (np_sc.gt.0) Then
                  tnost(nosttrig,3) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
               else if (np_ec.gt.0) Then
                  tnost(nosttrig,3) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
               endif 
c               beta_sc=pel/sqrt(pel**2+mdeuteron**2)
c               beta_ec=beta_sc
c               If (np_sc.gt.0) Then
c                  tnost(nosttrig,4) = sctime(np_sc)-scr_trk(np_sc)/beta_sc/vlight
c               else if (np_ec.gt.0) Then
c                  tnost(nosttrig,4) = ectime(np_ec)-ecr_trk(np_ec)/beta_ec/vlight
c               endif 
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
               ntrig = ntrig+1
               t0(ntrig) = t1
               vz(ntrig) = vzpos(np_st)
               t0_stat(ntrig) = 1000000*ntrig+10000*st_stat3+100*st_stat2+st_stat1
               all_stat = 10000*st_stat3+100*st_stat2+st_stat1
               trigger_status = trig_stat
               trig_ok = 0
            endif 
         ELSEIF(neut_trigg_photon.gt.0.and.ntrig_stat.ge.neut_trigg_photon)THEN
            ntrig=ntrig+1
            t0(ntrig) = t1
            vz(ntrig) = 0.0
            t0_stat(ntrig) = 1000000*ntrig+51
            trigger_status=ntrig_stat
         ENDIF 
 90      Continue
      ENDDO
      if(trkstat.lt.0)then
         idh=0
      elseif(trkstat.gt.0)then
         idh=10
      endif
      call hcdir('//PAWC/SEB',' ')
      if(nosttrig.gt.0.and.nosttrig_stat.eq.0)then
         call hf1(8975,float(nosttrig),1.)
         min=100.
         dt3=100.
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
            call hf1(8983,min,1.)
         elseif(trkstat.gt.0)then
            call hf1(8993,min,1.)
         endif
         if(min.lt.90.)then
            call hf1(8981+idh,tnost(ipr1,ipr2),1.)
            call hf1(8982+idh,tnost(jpr1,jpr2),1.)
c            ntrig=2
            t03(1)=tnost(ipr1,ipr2)
            vz3(1) = dcvertz(nostdc(ipr1))
            t03_stat(1) = 1000052
            t03(2)=tnost(jpr1,jpr2)
            vz3(2) = dcvertz(nostdc(jpr1))
            t03_stat(2) = 1000000*ntrig+52
c            trigger_status = trig_stat
            trigger_status = nosttrig
            do i=1,2
               do j=1,taghits
                  if(tagstat(j).eq.7.or.tagstat(j).eq.15) then	
                     tdiff = (Tgamma(j)+(vz3(i)-target_position(3))/vlight)-t03(i)
                     call hcdir('//PAWC/SEB',' ')
                     call hf1(8984+idh,tdiff,1.)
                     if(abs(tdiff).le.min) then
                        min = abs(tdiff)
                        nostist = i
                        nostitag = j
                        dt3=tdiff
                     endif
                  endif
               enddo
            enddo
            if(sthits.ne.0.AND.ntrig.gt.0) then
               sntrig=ntrig
               do j=1,ntrig
                  t04(j)=t0(j)
                  vz4(j)=vz(j)                        
                  t04_stat(j)=t0_stat(j)               
                  do i=1,2
                     call hf1(8985+idh,t03(i)-t04(j),1.)
                  enddo
               enddo
            endif
            if(sthits.eq.0)then
               ntrig=2
               do i=1,2
                  vz(i)=vz3(i)
                  t0(i)=t03(i)
                  t0_stat(i)=t03_stat(i)
               enddo
            endif
         elseif(sthits.eq.0)then
            ntrig=0
            trigger_status = 0
         endif
      endif
c      nostist=0
c
      if(all_stat.gt.99.and.mod(all_stat,100).ne.0) then
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
      if(ntrig.gt.20) ntrig=20  !limit only for 20 tracks
c   
      IF (trigger_status.EQ.0) GO TO 998
      
c     we have ST hit and TAG hit. Next select the tagger hit that coincident with
c     start counter hit. Here we consider only the tagger hit that has status=7 or 15.
      If (trigger_status.GT.1.and.taghits.gt.0) Then
         min = 200.
         dt=200.
         do i=1,ntrig
            do j=1,taghits
               if(tagstat(j).eq.7.or.tagstat(j).eq.15) then	
                  tdiff = (Tgamma(j)+(vz(i)-target_position(3))/vlight)-t0(i)
                  call hcdir('//PAWC/SEB',' ')
                  call hf1(9981+idh,tdiff,1.)
                  if(abs(tdiff).le.min) then
                     min = abs(tdiff)
                     ist = i
                     itag = j
                     dt=tdiff
                  endif
               endif
            enddo
         enddo
         if(sthits.gt.0.AND.sntrig.gt.0) then
            min = 200.
            dt4=200.
            do i=1,sntrig
               do j=1,taghits
                  if(tagstat(j).eq.7.or.tagstat(j).eq.15) then	
                     tdiff = (Tgamma(j)+(vz4(i)-target_position(3))/vlight)-t04(i)
                     call hcdir('//PAWC/SEB',' ')
                     call hf1(8986+idh,tdiff,1.)
                     if(abs(tdiff).le.min) then
                        min = abs(tdiff)
                        ist4 = i
                        itag4 = j
                        dt4=tdiff
                     endif
                  endif
               enddo
            enddo
         endif
c         if(abs(dt3).lt.abs(dt).and.nostist.gt.0)then
c            ist=nostist
c            itag=nostitag
c            t0(ist)=t03(ist)
c            vz(ist)=vz3(ist)
c            dt=dt3
c         endif
         if(ist.ne.0.and.itag.ne.0) then
            call hf1(9982+idh,dt,1.)
            if(ist4.ne.0.and.itag4.ne.0) then
               call hf2(8987+idh,dt,dt4,1.)
               call hf2(8988+idh,itag*1.,itag4*1.,1.)
           endif
         else
            goto 998
         endif 
c
         do i = 1,taghits
            tag_ttime(i) = Tgamma(i)+(vz(ist)-target_position(3))/vlight
         enddo         
c     get rid of tagger hits that do not match with ST within +/- st_tagger_match ns.
         do i = 1,taghits
            if(abs(t0(ist)-tag_ttime(i)).le.st_tagger_match.and.
     +           (tagstat(i).eq.7.or.tagstat(i).eq.15)) then
               goodtag = goodtag+1
               tagptr(goodtag) = i*1000+tagstat(i)
               tagtime(goodtag) = tag_ttime(i)
               tagenergy(goodtag) = Egamma(i)
               tagTid(goodtag) = Tid(i)
               tagEid(goodtag) = Eid(i)
               dt_sttag(goodtag) = t0(ist)-tag_ttime(i)
               call hf1(9983+idh,tagtime(goodtag),1.)
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
CCC modified on 06/18  (R. De Vita): target position included in definition of tag_ttime
c         TTime = tag_ttime(itag)-target_position(3)/vlight 
         TTime = tag_ttime(itag)
      else
         goto 998
      endif   
      if(goodtag.ge.1) goodtag = keep    
      call hf1(9984+idh,ttime,1.)
   
c  define order of tagr hits based on tdiff (from min to max)
      if(goodtag.gt.0) then
         tdiff_min=0
         do i=1,goodtag
            ind_nord_tg(i)=1
         enddo
         do i=1,goodtag
            min=200
            nmin=0
            btagptr(i)    = tagptr(i)
            btagtime(i)   = tagtime(i)
            btagenergy(i) = tagenergy(i)
            bdt_sttag(i)  = dt_sttag(i)
            do j=1,goodtag        
               tdiff = dt_sttag(j)
               if(ind_nord_tg(j).eq.1.and.
     &            abs(tdiff).ge.tdiff_min.AND.abs(tdiff).lt.min) then
                  min=abs(tdiff)
                  ind_ord_tg(i)=j
               endif
            enddo
            tdiff_min=min
            ind_nord_tg(ind_ord_tg(i))=0
         enddo
         do i=1,goodtag
            j=ind_ord_tg(i)
            tagptr(i) = btagptr(j)
            tagtime(i) = btagtime(j)
            tagenergy(i) = btagenergy(j)
            dt_sttag(i) = bdt_sttag(j)            
         enddo
      endif
      
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

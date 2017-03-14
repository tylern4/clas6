
      SUBROUTINE TAG_FILL(do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_fill.F,v 1.29 2008/09/02 13:13:11 pasyuk Exp $
c
c  Documentation for subroutine TAG_FILL
c
c  Purpose:
c  --------
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
c
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
#include "bcs.inc"
#include "tag_general.PAR"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------

      logical do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tag_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.29 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/09/02 13:13:11 $')
      PARAMETER (CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/   
     1'$Id: tag_fill.F,v 1.29 2008/09/02 13:13:11 pasyuk Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer MLINK,MAMIND,ind,size
      integer nam_st1
      data nam_st1 /0/
      integer i,j,k

      real unbinnedT
      real Tag_Hit(12)
      real Tagr_Hit(6)
      real T_mean,xoff,yoff
      real besttagtime
      integer tid,column,row,nbad_tagr,ngood_tagr
      integer tag_unbin,tag_nbc_bin

      integer nb_sti_hit
      integer sti_id(24),sti_status(24)
      real    sti_tdc1(24),sti_tdc2(24),sti_adc1(24),sti_adc2(24)
      integer nb_str_hit,st_sec
      real    xst_pd
      integer str_id(24),str_trkNum(24),str_stat(24),str_where(24),str_pd(24)
      real    str_time(24),str_length(24),str_pos(24)

      integer indRF
      real goodrf,rf1,rf2

      integer nb_tagr_hit
      real    Tagr_erg(40),Tagr_ttag(40),Tagr_tpho(40)
      integer Tagr_stat(40),Tagr_tid(40),Tagr_eid(40)

      integer nb_tgpb_hit,tgpb_ptr(40)
      real    tgpb_time(40),tgpb_energy(40),tgpb_dt(40)

      integer nb_tagi_hit
      real    Tagi_tl(40),Tagi_tr(40),Tagi_te(40),Tagi_tm(40),Tagi_trf(40)
      integer Tagi_tid(40),Tagi_eid(40)

      integer nb_tbid_hit
      integer Tbid_st_id(20),Tbid_sc_id(20),Tbid_sec(20)
      real    Tbid_vtime(20),Tbid_st_vtime(20)

      integer nb_part_hit
      integer Part_id(20), Part_tbid(20)
      real    Part_px(20),Part_py(20),Part_pz(20),Part_p(20)

      integer nb_scr_hit(6)
      real    Scr_e(6,20)

      integer itbid,ksc,kst,ksec
c -------- external functions

	real rf_corr_time


c_end_var
c
c  executable code for routine TAG_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling Tagger Histograms & ntuples, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF


c  First change PAWC directories
      call hcdir('//PAWC/TAG',' ')



c  Do your HFILLS below this line
     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- get the RF for this event:
    
      indRF = MLINK(iw,'CL01',0)

      if (indRF.ne.0) then
	 size = IW(indRF)
	 if (size.ne.0) then 
            goodrf = RW(indRF + 10) 
            rf1    = RW(indRF + 11) 
            rf2    = RW(indRF + 12)
         endif
      endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c  --- get Start counter stuff for this event:

c - - intermediate

c      ind = MLINK(iw,'ST1 ',1)
      if(nam_st1.eq.0) nam_st1 = MAMIND(iw,'ST1 ')
      ind=iw(nam_st1)
      if (ind.ne.0) then
         nb_sti_hit = min(iw(ind-4),24)

         do i=1,nb_sti_hit
            j = (i - 1)*iw(ind-5)
            sti_id(i)     = IW(ind + j + 1)/10   !=sector*100+segment*10
            sti_status(i) = IW(ind + j + 2)      !=2 (always)
            sti_tdc1(i)   = RW(ind + j + 3)
            sti_adc1(i)   = RW(ind + j + 4)
            sti_tdc2(i)   = IW(ind + j + 5)      !=0 for new ST
            sti_adc2(i)   = IW(ind + j + 6)      !=0 for new ST
         enddo

      else
         nb_sti_hit = 0
      endif


c - - Final

      ind = MLINK(iw,'STR ',1)
      
      if (ind.ne.0) then
         nb_str_hit = iw(ind-4)

         if(nb_str_hit.gt.20) nb_str_hit=20
         do i=1,nb_str_hit
            j = (i - 1)*iw(ind-5)
            str_id(i)        = IW(ind + j + 1)/10
            str_pd(i)        = (str_id(i)/10-1)*4+mod(str_id(i),10)
            str_where(i)     = IW(ind + j + 1)-10*str_id(i)
            str_trkNum(i)    = IW(ind + j + 2)
            str_time(i)      = RW(ind + j + 3)
            str_length(i)    = RW(ind + j + 4) 
            str_pos(i)       = RW(ind + j + 5) 
            str_stat(i)      = IW(ind + j + 6)  !=#hits in paddle *2
         enddo

      else
         nb_str_hit = 0
      endif



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  get TGPB bank

      ind = MLINK(iw,'TGPB',0)
      if (ind.ne.0) then
         nb_tgpb_hit = MIN(iw(ind-4),40)

         do i=1,nb_tgpb_hit
            j = (i - 1)*iw(ind-5)
            tgpb_ptr(i)    = IW(ind + j + 1)/1000  !negative for "trigger" photon
            tgpb_time(i)   = RW(ind + j + 2)     !start_time_TAG at "interaction point"
            tgpb_energy(i) = RW(ind + j + 3)
            tgpb_dt(i)     = RW(ind + j + 4)     !start_time_ST-start_time_TAG
         enddo

      else

          nb_tgpb_hit= 0

      endif
      
!>>> never booked:      call hf1(520,RW(ind + j + 4),1.)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get TAGR bank
      ind = MLINK(iw,'TAGR',1)

      if (ind.ne.0) then
         nb_tagr_hit = MIN(iw(ind-4),40)
         
         do i=1,nb_tagr_hit
            j = (i-1)*iw(ind-5)
            Tagr_erg(i)  = RW(ind+j+1) ! energy
            Tagr_ttag(i) = RW(ind+j+2) ! time
            Tagr_tpho(i) = RW(ind+j+3) ! time rf
            Tagr_stat(i) = IW(ind+j+4) ! status
            Tagr_tid(i)  = IW(ind+j+5) ! T_id
            Tagr_eid(i)  = IW(ind+j+6) ! E_id
            
            Tagr_hit(1) = RW(ind+j+1) ! energy
            Tagr_hit(2) = RW(ind+j+2) ! time
            Tagr_hit(3) = RW(ind+j+3) ! time rf
            Tagr_hit(4) = float(IW(ind+j+4)) ! status
            Tagr_hit(5) = float(IW(ind+j+5)) ! T_id
            Tagr_hit(6) = float(IW(ind+j+6)) ! E_id
            if (do_tagr_nt) call hfn(4,Tagr_hit)
            
         enddo
         
      else
         nb_tagr_hit = 0
      endif


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get TAGI bank
      ind = MLINK(iw,'TAGI',1)
      
      if (ind.ne.0) then
         nb_tagi_hit = MIN(iw(ind-4),40)
         
         do i=1,nb_tagi_hit
            j = (i-1)*iw(ind-5)
            Tagi_tid(i) = IW(ind+j+1)       ! T ID
            Tagi_tl(i)  = RW(ind+j+2)       ! T left
            Tagi_tr(i)  = RW(ind+j+3)       ! T right
            Tagi_eid(i) = IW(ind+j+4)       ! E Id
            Tagi_te(i)  = RW(ind+j+5)       ! E time
            Tagi_tm(i)  = RW(ind+j+6)       ! Mean time aligned 
            Tagi_trf(i) = RW(ind+j+7)       ! RF correction
            
            Tag_hit(1) = float(IW(ind+j+1)) ! T ID
            Tag_hit(2) = RW(ind+j+2)        ! T left
            Tag_hit(3) = RW(ind+j+3)        ! T right
            Tag_hit(4) = float(IW(ind+j+4)) ! E Id
            Tag_hit(5) = RW(ind+j+5)        ! E time
            Tag_hit(6) = RW(ind+j+6)        ! Mean time aligned
            Tag_hit(7) = RW(ind+j+7)        ! RF correction
            Tag_hit(8) = goodrf             ! good RF
            Tag_hit(9) = rf1                ! rf1
            Tag_hit(10) = rf2               ! rf2
            Tag_hit(11) = nb_tagi_hit ! Nb_good_Hits
            Tag_hit(12) = RW(ind+j+8) ! time difference with next T
	  
            if (do_tagirf_nt) call hfn(1,Tag_hit)
            
         enddo

      else
         nb_tagi_hit=0
      endif



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c get SCRC bank
      do k=1,6
         ind = MLINK(iw,'SCRC',k)

         if (ind.ne.0) then
            nb_scr_hit(k) = iw(ind-4)
            if(nb_scr_hit(k).gt.20) nb_scr_hit(k)=20

            do i=1,nb_scr_hit(k)
               j = (i-1)*iw(ind-5)
               
               Scr_e(k,i)    =  RW(ind+j+ 2) ! Tof energy
               
            enddo
         
         else
            nb_scr_hit(k) = 0
         endif
      enddo

c get TBID bank
      ind = MLINK(iw,'TBID',1)

      if (ind.ne.0) then
         nb_tbid_hit = iw(ind-4)
         if(nb_tbid_hit.gt.20) nb_tbid_hit=20

         do i=1,nb_tbid_hit
            j = (i-1)*iw(ind-5)
               
            Tbid_sec(i)      =  IW(ind+j+ 2)   ! track sector
            Tbid_vtime(i)    =  RW(ind+j+ 4)   ! track vtime
            Tbid_sc_id(i)    =  IW(ind+j+ 6)   ! track sc.id (ptr to SCRC entry)
            Tbid_st_vtime(i) =  RW(ind+j+27)   ! track st.vtime
            Tbid_st_id(i)    =  IW(ind+j+24)   ! track st.id (ptr to STR entry)
c            print *,Tbid_sec(i),Tbid_vtime(i),Tbid_sc_id(i)
         enddo
         
      else
         nb_tbid_hit = 0
      endif

c get PART bank
      ind = MLINK(iw,'PART',1)

      if (ind.ne.0) then
         nb_part_hit = iw(ind-4)
         if(nb_part_hit.gt.20) nb_part_hit=20

         do i=1,nb_part_hit
            j = (i-1)*iw(ind-5)
            
            Part_id(i)   = IW(ind+j+1)
            Part_px(i)   = RW(ind+j+6)
            Part_py(i)   = RW(ind+j+7)
            Part_pz(i)   = RW(ind+j+8)
            Part_tbid(i) = IW(ind+j+10)          !ptr to TBID entry
            Part_p(i)=sqrt(Part_px(i)**2+Part_py(i)**2+Part_pz(i)**2)
c            print *,Part_id(i),Part_px(i),Part_tbid(i)
         enddo
         
      else
         nb_part_hit = 0

      endif





      if (do_hist) then
         if(nb_tagr_hit.gt.0) then
ccc NUMBER OF HITS IN THE TAGGER
            call hf1(101,nb_tagr_hit*1.,1.) ! number of hits in TAGR bank
            ngood_tagr=0
            nbad_tagr=0
            do i=1,nb_tagr_hit
               if(Tagr_stat(i).eq.7.OR.Tagr_stat(i).eq.15) then
                  ngood_tagr=ngood_tagr+1
               else
                  nbad_tagr=nbad_tagr+1
               endif
            enddo 
            call hf1(102,ngood_tagr*1.,1.) ! number of good hits in TAGR bank 
            call hf1(103,nbad_tagr *1.,1.) ! number of bad hits in TAGR bank 
            call hf1(104,nb_tgpb_hit*1.,1.) ! number of good hits in TGPB bank 

ccc ENERGY OF THE PHOTONS
            do i=1,nb_tagr_hit
               call hf1(201,Tagr_erg(i),1.) ! Energy spectrum of all the reconstructed photons
               if(Tagr_stat(i).eq.7.OR.Tagr_stat(i).eq.15) then
                  call hf1(202,Tagr_erg(i),1.) ! Energy spectrum of all good reconstructed photons
               else
                  call hf1(203,Tagr_erg(i),1.) ! Energy spectrum of all bad reconstructed photons
               endif
            enddo 
            if(nb_tgpb_hit.gt.0) then
               do i=1,nb_tgpb_hit
                  call hf1(204,Tgpb_energy(i),1.) ! Energy spectrum photon from TGPB bank (within time window)
               enddo
               call hf1(205,Tgpb_energy(1),1.) ! Energy spectrum the good photon from TGPB bank
            endif

ccc TIME OF THE PHOTONS
            do i=1,nb_tagr_hit
               call hf1(301,Tagr_tpho(i),1.) ! Timing of all the reconstructed photons
               if(Tagr_stat(i).eq.7.OR.Tagr_stat(i).eq.15) then
                  call hf1(302,Tagr_tpho(i),1.) ! Timing of all good reconstructed photons
               else
                  call hf1(303,Tagr_tpho(i),1.) ! Timing of all bad reconstructed photons
               endif
            enddo 
            if(nb_tgpb_hit.gt.0) then
               do i=1,nb_tgpb_hit
                  if(IABS(Tgpb_ptr(i)).LE.40)
     &             call hf1(304,Tagr_Tpho(IABS(Tgpb_ptr(i))),1.) ! Timing of the photon from TGPB bank (within time window)
               enddo
               call hf1(305,Tagr_Tpho(IABS(Tgpb_ptr(1))),1.) ! Timing of the good photon from TGPB bank
            endif


ccc T TDC slopes CHECK
            do i=1,nb_tagi_hit
               tid = tag_unbin(Tagi_tid(i))
               row = (tid-1)/8
               column = mod(tid-1,8)
               xoff = 50. + column * 200.
               yoff = 2. + row * 4.
               call hf2(1001,xoff+Tagi_tm(i),yoff+Tagi_tl(i)-Tagi_tr(i),1.)
               yoff = 1. + row * 2.
               call hf2(1002,xoff+Tagi_tm(i),yoff+Tagi_trf(i),1.)
!>>> was Tagi_trf(7)
               call hf2(1003,rf1,Tagi_trf(i),1.)
               call hf2(1004,rf2,Tagi_trf(i),1.)
               call hf1(1005,Tagi_trf(i),1.)
               call hf2(1006,Tagi_eid(i)*1.,Tagi_tm(i) -Tagi_te(i),1.)
            enddo

ccc T alignement to RF
            do i=1,nb_tagr_hit
               call hf1(1007,Tagr_ttag(i)-Tagr_tpho(i),1.)
               call hf2(1008,Tagr_tid(i)*1.,Tagr_ttag(i)-Tagr_tpho(i),1.)
               call hf2(1009,tag_unbin(Tagr_tid(i))*1.,Tagr_ttag(i)-Tagr_tpho(i),1.)
            enddo
	  	

ccc E and T counter pattern histo 
            do i=1,nb_tagr_hit               
               call hf1(2001,Tagr_eid(i)*1.,1.)
               call hf1(2002,Tagr_tid(i)*1.,1.)
               if (Tagr_stat(i).eq.7.OR.Tagr_stat(i).eq.15) then
                  call hf1(2003,Tagr_tid(i)*1.,1.)
               endif
               unbinnedT = float(tag_unbin(Tagr_tid(i)))
               call hf1(2004,unbinnedT,1.)
               if (tag_nbc_bin(Tagr_tid(i)).eq.2) call hf1(2004,unbinnedT+1.,1.)
               if (Tagr_stat(i).eq.7.OR.Tagr_stat(i).eq.15) then
                  call hf1(2005,unbinnedT,1.)
                  if (tag_nbc_bin(Tagr_tid(i)).eq.2) call hf1(2005,unbinnedT+1.,1.)                     
               endif
            enddo


ccc ST plots
            call hf1(4001,nb_sti_hit*1.,1.)
            do i=1,nb_sti_hit                  
               st_sec=int(sti_id(i)/10)
               xst_pd=(st_sec-1)*4.+sti_id(i)-10.*st_sec
               call hf1(4011,xst_pd,1.)
               call hf1(4012,sti_tdc1(i),1.)
               call hf1(4013,sti_adc1(i),1.)
               call hf2(4014,xst_pd,sti_tdc1(i),1.)
               call hf2(4015,xst_pd,sti_adc1(i),1.)
            enddo               
            call hf1(4020,nb_str_hit*1.,1.)
            do i=1,nb_str_hit
               call hf1(4021,str_pd(i)+str_where(i)*0.4,1.)
               call hf1(4022,str_time(i),1.)
               call hf1(4023,str_length(i),1.)
               call hf1(4024,str_pos(i),1.)
               call hf1(4025,str_stat(i)*1.,1.)
               call hf2(4026,str_pd(i)*1.,str_time(i),1.)
               call hf2(4027,str_pd(i)*1.,str_pos(i),1.)
               call hf2(4028,str_pos(i),str_time(i),1.) 
               call hf2(4100+str_pd(i),str_pos(i),str_time(i),1.)
            enddo

ccc ST - tagger coincidence
            besttagtime=100000.
            do i=1,nb_tagr_hit               
               do k=1,nb_str_hit
                  call hf1(4501,Tagr_tpho(i)-str_time(k),1.)
                  call hf2(4502,Tagr_tpho(i),str_time(k),1.)
                  call hf2(4503,Tagr_tid(i)*1.,Tagr_tpho(i)-str_time(k),1.)
                  call hf2(4504,str_pd(k)*1.,Tagr_tpho(i)-str_time(k),1.)
                  call hf2(4505,Tagr_tid(i)*1.,-str_time(k),1.)
                  if (abs(Tagr_tpho(i)-str_time(k)).lt.abs(besttagtime-str_time(k)))
     +                                                     besttagtime = Tagr_tpho(i)
               enddo
            enddo

ccc STvtime-Vtime from TBID bank
            if(nb_part_hit.gt.0.AND.nb_tbid_hit.gt.0) then
               do i=1,nb_part_hit
                  if(Part_id(i).eq.8.OR.Part_id(i).eq.9.) then
                     itbid=Part_tbid(i)
                     if(Tbid_sc_id(itbid).gt.0.AND.Tbid_sc_id(itbid).le.nb_scr_hit(Tbid_sec(itbid))) then
                        ksc = Tbid_sc_id(itbid)
                        ksec= Tbid_sec(itbid)
                        if(SCR_e(ksec,ksc).gt.4.AND.SCR_e(ksec,ksc).lt.13.5.AND.Part_p(i).gt.0.3) then
                           if(Tbid_st_id(itbid).gt.0.AND.Tbid_st_id(itbid).le.nb_str_hit) then
                              kst=Tbid_st_id(itbid)
                              call hf2(4506,str_pd(kst)*1.,Tbid_vtime(itbid)-TBid_st_vtime(itbid),1.)
                              if(str_where(kst).eq.1) then                 ! hit is in the leg part
                                 call hf2(4507,str_pd(kst)*1.,Tbid_vtime(itbid)-TBid_st_vtime(itbid),1.)
                              else                                         ! hit is in the nose part
                                 call hf2(4508,str_pd(kst)*1.,Tbid_vtime(itbid)-TBid_st_vtime(itbid),1.)
                              endif
c                              print *,i,Part_id(i),Part_p(i),Part_tbid(i),Tbid_sec(itbid)
c                              print *,SCR_e(ksec,ksc)
c                              print *,Tbid_st_id(itbid),str_id(kst),st_sec,st_pd
c                              print *,' '
                           endif
                        endif
                     endif
                  endif
               enddo
            endif

ccc  Tagger - st histograms for the best tagger hit guess:
            do k=1,nb_str_hit
               call hf1(4511,besttagtime-str_time(k),1.)
               call hf2(4512,besttagtime,str_time(k),1.)
            enddo
            
         endif 
      endif



c------------------------------------------------------------------------------

c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------





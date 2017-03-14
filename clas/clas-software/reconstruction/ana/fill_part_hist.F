      SUBROUTINE FILL_PART_HIST
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_part_hist.F,v 1.21 2008/04/02 03:25:39 fklein Exp $
c     
c     Documentation for subroutine FILL_PART_HIST
c     
c     Purpose:
c     --------
c     
c     Input Parameters:  (Name - Type - Meaning)
c     ----------------
c     
c     Output Parameters:  (Name - Type - Meaning)
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
C    Starting Pass0v4 rf_time is coming from CL01 instead of HEVT
C     -Patrick Collins
c     
c     Author:         Created:  Wed Jun 11 00:14:11 EDT 1997
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
#include "bcs.inc"
#include "wcs.inc"
#include "user_control.inc"
#include "seb_nt.inc"
#include "MVRTnt.inc"
#include "clasmdl.inc"
#include "dtcl_var.inc" 
c     _end_inc
c     
c
      real u,qx,qy,qz,q2
      common/photv/u,qx,qy,qz,q2
c
      real mx2,ex,px,p0x,p0y,p0z,thx,phx
      common/mispart/mx2,ex,px,thx,phx,p0x,p0y,p0z
      real mi2,ey,py,p1x,p1y,p1z
      common/recpart/mi2,ey,py,p1x,p1y,p1z
c
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_part_hist.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.21 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/04/02 03:25:39 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: fill_part_hist.F,v 1.21 2008/04/02 03:25:39 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_PART_HIST')
      PARAMETER (CRAUTH='whoistheauthor')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE, mamind,mlink
      integer sect,hid,iecp,secth,ccphi
      integer ind,i,scstat,j
      real ccsg,eec,stv,thetah
      real w2,w,rfcor,phih
      real mprot,mpi,mk,md,mneut
      data  mprot,mpi,mk,md,mneut/0.9383,0.139,0.49368,1.8756,0.9396/
      real r2d,pi,vlight
      data r2d,pi,vlight/57.29578,3.14159,29.98/
      real perp(6,3),perp_thickness,sc_ec_t,drc
      data perp_thickness/20.7/
      real thetael,thetapr,thetapip,bpi,path,bp,dt(20),ftime
      real ftime_st,path_st,dt_st(20)
      real ftime_ec,path_ec,dt_ec(20)
      integer pd_st
      integer npip,npim,nkp,nkm,np,nd,nn,ng,nneut,nneg,npos,nch,idtch
      integer ineg(20),ipos(20),ich(20),itag
      integer ipip(20),ipim(20),ikp(20),ikm(15),ip(20),idd(20),in(20),ig(30)
      real mxn,mxnkk,mxnpik,mxnpipi
      integer nb_str_hit,st_sec,st_pd
      integer str_id(20),str_trkNum(20),str_stat(20)
      real    str_time(20),str_length(20),str_pos(20),zpos
      integer ihits,iii
      real ct_tag0,dt_st_tag0   !only for best matching Taghit with ST
      integer itag0
c     _end_var
c     
c     executable code for routine FILL_PART_HIST:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='This is a DUMMY routine, this message written once'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      call hcdir('//PAWC',' ')
      call hcdir('//PAWC/SEB',' ')
      IECP=MAMIND(JW,'ECP ')
      ind=IECP+1
 10   ind=JW(ind-1)
      if(ind.gt.0)then
         sect=jw(ind-2)
         perp(sect,1)=rjw(ind+1)
         perp(sect,2)=rjw(ind+2)
         perp(sect,3)=rjw(ind+3)
         go to 10
      endif
      ind = MLINK(iw,'STR ',1)
      if (ind.ne.0) then
         nb_str_hit = iw(ind-4)
         if(nb_str_hit.gt.20) nb_str_hit=20
         do i=1,nb_str_hit
            j = (i - 1)*iw(ind-5)
            str_id(i)        = IW(ind + j + 1)/10
            str_trkNum(i)    = IW(ind + j + 2)
            str_time(i)      = RW(ind + j + 3)
            str_length(i)    = RW(ind + j + 4) 
            str_pos(i)       = RW(ind + j + 5) 
            str_stat(i)      = IW(ind + j + 6) 
         enddo
      else
         nb_str_hit = 0
      endif
      IF(npart.gt.0)THEN
c  Must find the right photon first and remember RF offset
         u=0.
         itag0=0
         ct_tag0=-100.
         dt_st_tag0=-100.
         u=tag_energy(1)
         do i=1, taggoodhit
            if( tag_ptr(i) .LT. 0 ) then 
               itag0 = IABS(tag_ptr(i)) 
               if(itag0.gt.40) return            !give it up: too many tagger hits
               ct_tag0 = T_gamma( itag0 ) - T_gamma_noRF( itag0 )
               dt_st_tag0 = dt_st_tag(i)
               u = tag_energy(i)
            endif
         enddo
         qx=0.
         qy=0.
         qz=u
         q2=0.
         nneg=0
         npos=0
         npip=0
         npim=0
         np=0
         nd=0
         nkp=0
         nkm=0
         nneut=0
         ng=0
         nn=0
         nch=0

         call hf1(3101,rf_time,1.)
         call hf1(3102,ct_tag0,1.)
         call hf2(3103,rf_time,ct_tag0,1.)
         if(itag0.gt.0) call hf2(3105,Tid(itag0)*1.,T_gamma(itag0)-T_gamma_noRF(itag0),1.)

         call hf1(3112,dt_st_tag0,1.)
         call hf2(3113,rf_time,dt_st_tag0,1.)
         if(itag0.gt.0) call hf2(3115,Tid(itag0)*1.,dt_st_tag0,1.)

         call hf1(8970,gpart*1.,1.)
       	 call hf1(8971,sc_part*1.,1.)
         call hf1(8972,ec_part*1.,1.)
         call hf1(8973,st_part*1.,1.)
         call hf1(8974,TAGGOODHIT*1.,1.)
         if(taggoodhit.ge.1) call hf1(3197,tag_energy(1),1.)
         if(taggoodhit.ge.2) call hf1(3198,tag_energy(2),1.)
         if(taggoodhit.ge.3) call hf1(3199,tag_energy(3),1.)

         do i=1,gpart
           if(stat(i).gt.0.and.dc(i).gt.0.and.sc(i).gt.0)then
               call hf2(3104,vz(i),ct_tag0,1.)
               call hf2(3114,vz(i),dt_st_tag0,1.)
               thetah=acos(cz(i))*r2d
               phih =atan2(cy(i),cx(i))*r2d
               if(phih .lt.-30.)phih=phih +360.
               nch=nch+1
               ich(nch)=i
               ftime=sc_t(sc(i))
               path=sc_r(sc(i))
               if(q(i).gt.0.and.dc_stat(dc(i)).gt.0)then
                  call hf2(6041,vz(i),phih ,1.)
                  hid=6100+sc_sect(sc(i))
                  call hf2(6310,p(i),edep(sc(i)),1.)
                  call hf2(6310+sc_sect(sc(i)),p(i),edep(sc(i)),1.)
                  if(m(i).gt.0.and.sc_stat(sc(i)).eq.33)call hf1(6100,sqrt(m(i)),1.)
                  call hf2(hid,float(sc_pd(sc(i))),m(i),1.)
                  call hf2(6200+sc_sect(sc(i)),p(i),thetah,1.)
                  call hf1(6220+sc_sect(sc(i)),float(sc_pd(sc(i))),1.)
                  npos=npos+1
                  ipos(npos)=i
c                  if(m(i).lt.0.09)then

c                 dt for pi+ 
                  if( id(i) .EQ. +211 ) then
                     npip=npip+1
                     ipip(npip)=i
                     bp=p(i)/sqrt(p(i)**2+mpi**2)
                     dt(i)=ftime-path/bp/vlight
                     call hf1(6120,dt(i)-tr_time,1.)
                     call hf2(6120+sc_sect(sc(i)),float(sc_pd(sc(i))),dt(i)-tr_time,1.)
                     if(edep(sc(i)).gt.4.AND.edep(sc(i)).lt.13.5.AND.p(i).gt.0.6) then
                        call hf1(6140,dt(i)-tr_time,1.)
                        call hf2(6140+sc_sect(sc(i)),float(sc_pd(sc(i))),dt(i)-tr_time,1.)
                     endif

ccc repeat dt calulation for the start counter
                     if(st(i).gt.0.AND.st(i).le.st_part) then
                        ftime_st=st_time(st(i))
                        path_st=st_rtrk(st(i))
                        dt_st(i)=ftime_st-path_st/bp/vlight
                        pd_st=(st_sector(st(i))-1)*4+int(st_ihit(st(i))/10.)
                        zpos=-100.
                        do j=1,nb_str_hit
                           if(st_trkno(i).eq.str_trkNum(j)) zpos=str_pos(j)
                        enddo
!fk: why opposite to dt_sc-tr_time???
                        call hf1(7120,-dt_st(i)+tr_time,1.)
                        call hf2(7121,float(pd_st),-dt_st(i)+tr_time,1.)
                        call hf2(7122,zpos,-dt_st(i)+tr_time,1.)
                        if(edep(sc(i)).gt.4.AND.edep(sc(i)).lt.13.5.AND.p(i).gt.0.3.AND.p(i).lt.0.735) then
                           call hf1(7140,-dt_st(i)+tr_time,1.)
                           call hf2(7141,float(pd_st),-dt_st(i)+tr_time,1.)
                           call hf2(7142,zpos,-dt_st(i)+tr_time,1.)
                           call hf1(714100+pd_st,-dt_st(i)+tr_time,1.) 
                        endif
                     endif

                     if(ec(i).gt.0.and.sc(i).gt.0.and.dc(i).gt.0)then
                        ihits = 0
                        do iii = 1,ec_part ! count ec hits in this sector
                           if(ec_sect(iii).eq.ec_sect(ec(i)))ihits=ihits+1
                        enddo
                        
ccc   repeat dt calculation for EC
                        if(ec(i).gt.0.and.ec(i).le.ec_part)then
                           ftime_ec=ec_t(ec(i))
                           path_ec=ec_r(ec(i))
                           dt_ec(i)=ftime_ec-path_ec/bp/vlight
                           call hf1(7220, dt_ec(i)-tr_time,1.)
                           if(edep(sc(i)).gt.4.AND.edep(sc(i)).lt.13.5.AND.p(i).gt.0.6) then
                              call hf1(7240,dt_ec(i)-tr_time,1.)
                           endif
                        endif
                        
                        if(dc_stat(dc(i)).gt.0.and.sc_stat(sc(i)).eq.33.and.ihits.eq.1)then
                           if(dc_sect(dc(i)).eq.ec_sect(ec(i)))then 
                              secth=ec_sect(ec(i))
                              drc=perp(secth,1)
     *                             *(dc_cxsc(dc(i))*cos(pi*(secth-1)/3.)-dc_cysc(dc(i))*sin(pi*(secth-1)/3.))
                              drc=drc+perp(secth,2)
     *                             *(dc_cysc(dc(i))*cos(pi*(secth-1)/3.)+dc_cxsc(dc(i))*sin(pi*(secth-1)/3.))
                              drc=drc+perp(secth,3)*dc_czsc(dc(i))
                              sc_ec_t=ec_t(ec(i))-sc_t(sc(i))-perp_thickness/drc/b(i)/29.979
                              call hf1(6600      ,sc_ec_t,1.) 
                              call hf1(6600+secth,sc_ec_t,1.) 
                           endif
                        endif
                     endif
c                  elseif(m(i).lt.0.49)then
                  elseif( id(i) .EQ. +321 ) then
                     nkp=nkp+1
                     ikp(nkp)=i
                     bp=p(i)/sqrt(p(i)**2+mk**2)
                     dt(i)=ftime-path/bp/vlight
c                  elseif(m(i).lt.1.8)then


c                 dt for proton 
                  elseif( id(i) .EQ. 2212 ) then
                     np=np+1
                     ip(np)=i
                     bp=p(i)/sqrt(p(i)**2+mprot**2)
                     dt(i)=ftime-path/bp/vlight
                     if(p(i).gt.0.8) then
                        call hf1(6110,dt(i)-tr_time,1.)
                        call hf2(6110+sc_sect(sc(i)),float(sc_pd(sc(i))),dt(i)-tr_time,1.)
                     endif

ccc repeat dt calulation for the start counter
                     if(st(i).gt.0.AND.st(i).le.st_part) then
                        ftime_st=st_time(st(i))
                        path_st=st_rtrk(st(i))
                        dt_st(i)=ftime_st-path_st/bp/vlight
                        pd_st=(st_sector(st(i))-1)*4+int(st_ihit(st(i))/10.)
                        zpos=-100.
                        do j=1,nb_str_hit
                           if(st_trkno(i).eq.str_trkNum(j)) zpos=str_pos(j)
                        enddo
                        if(p(i).gt.0.8) then
                           call hf1(7110,-dt_st(i)+tr_time,1.)
                           call hf2(7111,float(pd_st)-0.5,-dt_st(i)+tr_time,1.)
                           call hf2(7112,zpos,-dt_st(i)+tr_time,1.)
                           call hf1(711100+pd_st,-dt_st(i)+tr_time,1.) 
                        endif
                     endif
                     if(ec(i).gt.0.and.sc(i).gt.0.and.dc(i).gt.0)then
                        ihits = 0
                        do iii = 1,ec_part ! count ec hits in this sector
                           if(ec_sect(iii).eq.ec_sect(ec(i)))ihits=ihits+1
                        enddo
                        
ccc   repeat dt calculation for EC
                        if(ec(i).gt.0.and.ec(i).le.ec_part)then
                           ftime_ec=ec_t(ec(i))
                           path_ec=ec_r(ec(i))
                           dt_ec(i)=ftime_ec-path_ec/bp/vlight
                           call hf1(7210, dt_ec(i)-tr_time,1.)
                        endif
                        
                        if(dc_stat(dc(i)).gt.0.and.sc_stat(sc(i)).eq.33.and.ihits.eq.1)then
                           if(dc_sect(dc(i)).eq.ec_sect(ec(i)))then 
                              secth=ec_sect(ec(i))
                              drc=perp(secth,1)
     *                             *(dc_cxsc(dc(i))*cos(pi*(secth-1)/3.)-dc_cysc(dc(i))*sin(pi*(secth-1)/3.))
                              drc=drc+perp(secth,2)
     *                             *(dc_cysc(dc(i))*cos(pi*(secth-1)/3.)+dc_cxsc(dc(i))*sin(pi*(secth-1)/3.))
                              drc=drc+perp(secth,3)*dc_czsc(dc(i))
                              sc_ec_t=ec_t(ec(i))-sc_t(sc(i))-perp_thickness/drc/b(i)/29.979
                              call hf1(6610      ,sc_ec_t,1.) 
                              call hf1(6610+secth,sc_ec_t,1.) 
                           endif
                        endif
                     endif
c                  elseif(m(i).gt.2.56.and.m(i).lt.4.84)then
                  elseif( id(i) .EQ. 45 )then
                     nd=nd+1
                     idd(nd)=i
                  endif
               elseif(q(i).lt.0.and.dc_stat(dc(i)).gt.0)then
                  call hf2(6042,vz(i),phih ,1.)
                  call hf2(6210+sc_sect(sc(i)),p(i),thetah,1.)
                  call hf1(6230+sc_sect(sc(i)),float(sc_pd(sc(i))),1.)

c                  if(m(i).lt.0.09)then

c                 dt for pi-
                  if( id(i) .EQ. -211 ) then
                     npim=npim+1
                     ipim(npim)=i
                     bp=p(i)/sqrt(p(i)**2+mpi**2)
                     dt(i)=ftime-path/bp/vlight
                     call hf1(6130,dt(i)-tr_time,1.)
                     call hf2(6130+sc_sect(sc(i)),float(sc_pd(sc(i))),dt(i)-tr_time,1.)
                     if(edep(sc(i)).gt.4.AND.edep(sc(i)).lt.13.5.AND.p(i).gt.0.6) then
                        call hf1(6150,dt(i)-tr_time,1.)
                        call hf2(6150+sc_sect(sc(i)),float(sc_pd(sc(i))),dt(i)-tr_time,1.)
                     endif
ccc repeat dt calulation for the start counter
                     if(st(i).gt.0.AND.st(i).le.st_part) then
                        ftime_st=st_time(st(i))
                        path_st=st_rtrk(st(i))
                        dt_st(i)=ftime_st-path_st/bp/vlight
                        pd_st=(st_sector(st(i))-1)*4+int(st_ihit(st(i))/10.)
                        zpos=-100.
                        do j=1,nb_str_hit
                           if(st_trkno(i).eq.str_trkNum(j)) zpos=str_pos(j)
                        enddo
                        call hf1(7130,-dt_st(i)+tr_time,1.)
                        call hf2(7131,float(pd_st)-0.5,-dt_st(i)+tr_time,1.)
                        call hf2(7132,zpos,-dt_st(i)+tr_time,1.)
                        if(edep(sc(i)).gt.4.AND.edep(sc(i)).lt.13.5.AND.p(i).gt.0.3) then
                           call hf1(7150,-dt_st(i)+tr_time,1.)
                           call hf2(7151,float(pd_st)-0.5,-dt_st(i)+tr_time,1.)
                           call hf2(7152,zpos,-dt_st(i)+tr_time,1.)
                           call hf1(715100+pd_st,-dt_st(i)+tr_time,1.) 
                        endif
                     endif
                      if(ec(i).gt.0.and.sc(i).gt.0.and.dc(i).gt.0)then
                        ihits = 0
                        do iii = 1,ec_part ! count ec hits in this sector
                           if(ec_sect(iii).eq.ec_sect(ec(i)))ihits=ihits+1
                        enddo
                        
ccc   repeat dt calculation for EC
                        if(ec(i).gt.0.and.ec(i).le.ec_part)then
                           ftime_ec=ec_t(ec(i))
                           path_ec=ec_r(ec(i))
                           dt_ec(i)=ftime_ec-path_ec/bp/vlight
                           call hf1(7230, dt_ec(i)-tr_time,1.)
                           if(edep(sc(i)).gt.4.AND.edep(sc(i)).lt.13.5.AND.p(i).gt.0.3) then
                              call hf1(7250,dt_ec(i)-tr_time,1.)
                           endif
                        endif
                        
                        if(dc_stat(dc(i)).gt.0.and.sc_stat(sc(i)).eq.33.and.ihits.eq.1)then
                           if(dc_sect(dc(i)).eq.ec_sect(ec(i)))then 
                              secth=ec_sect(ec(i))
                              drc=perp(secth,1)
     *                             *(dc_cxsc(dc(i))*cos(pi*(secth-1)/3.)-dc_cysc(dc(i))*sin(pi*(secth-1)/3.))
                              drc=drc+perp(secth,2)
     *                             *(dc_cysc(dc(i))*cos(pi*(secth-1)/3.)+dc_cxsc(dc(i))*sin(pi*(secth-1)/3.))
                              drc=drc+perp(secth,3)*dc_czsc(dc(i))
                              sc_ec_t=ec_t(ec(i))-sc_t(sc(i))-perp_thickness/drc/b(i)/29.979
                              call hf1(6600      ,sc_ec_t,1.) 
                              call hf1(6600+secth,sc_ec_t,1.) 
                           endif
                        endif
                     endif
c                  elseif(m(i).lt.0.49)then
                  elseif( id(i) .EQ. -321 ) then
                     nkm=nkm+1
                     ikm(nkm)=i
                     bp=p(i)/sqrt(p(i)**2+mk**2)
                     dt(i)=ftime-path/bp/vlight
                  endif 
               endif
            elseif(q(i).eq.0.and.ec(i).gt.0)then
               eec=max(etot(ec(i)),ec_ei(ec(i))+ec_eo(ec(i)))
               call hf1(6620,b(i),1.)
               call hf1(6620+ec_sect(ec(i)),b(i),1.)
               call hf2(6630+ec_sect(ec(i)),b(i),eec,1.)
               if(b(i).gt.0.2.and.b(i).lt.0.85.and.eec.lt.0.5)then
                  nneut=nneut+1
                  in(nneut)=i
                  p(i)=b(i)*mneut/sqrt(1.-b(i)**2)
                  phih =atan2(cy(i),cx(i))*r2d
                  if(phih .lt.-30.)phih=phih +360.
                  call hf2(6043,vz(i),phih ,1.)
               elseif(b(i).gt.0.85)then
                  ng=ng+1
                  ig(ng)=i
                  p(i)=eec/0.272
                  phih =atan2(cy(i),cx(i))*r2d
                  if(phih .lt.-30.)phih=phih +360.
                  call hf2(6043,vz(i),phih ,1.)
               endif
            endif
         enddo
      ENDIF

c     filling MVRT histos
      if(vidMVRT.gt.0.AND.ntrMVRT.gt.2) then
         if(ch2MVRT.lt.10) then
            call hf1(6051,xMVRT,1.)
            call hf1(6052,yMVRT,1.)
            call hf2(6053,xMVRT,yMVRT,1.)
         endif
      endif

      evntype=0
ccc missing mass for p pi+ X channel
      if(np.eq.1.and.npip.eq.1)then
         call miss_mass(ip(1),ipip(1),0,0,mprot,mpi,0.,0.,mprot)
         call hf1(10001,mx2,1.)
         if(mx2.gt.0.0) call hf1(10002,sqrt(mx2),1.)
      endif
ccc missing mass for p pi+ pi- X channel
      if(np.eq.1.and.npim.eq.1.and.npip.eq.1)then
         call miss_mass(ip(1),ipip(1),ipim(1),0,mprot,mpi,mpi,0.,mprot)
         if(mx2.gt.0.0) call hf1(10003,sqrt(mx2),1.)
      endif

ccc invariant mass for p pi+ K+ channel
c      if(npip.eq.1.and.np.eq.1.and.nkp.eq.1) then
c         call inv_mass( ip(1), ipim(1), 0, 0, mprot, mpi, 0., 0. )
c         call hf1(10005,sqrt(mi2),1.)
c      endif

ccc invariant mass for gg
      if(ng.ge.2) then
         if(ec(ig(1)).gt.0.AND.ec(ig(2)).gt.0) then
            call inv_mass(ig(1),ig(2),0,0,0.,0.,0.,0.)
            call hf1(6640,sqrt(mi2),1.)
            call hf1(6640+ec_sect(ec(ig(1))),sqrt(mi2),1.)
            if(sqrt(mi2).gt.0.08.AND.sqrt(mi2).lt.0.2) then
               call hf1(6650,b(ig(1)),1.)
               call hf1(6650,b(ig(2)),1.)
               call hf1(6650+ec_sect(ec(ig(1))),b(ig(1)),1.)
               call hf1(6650+ec_sect(ec(ig(1))),b(ig(2)),1.)
            endif
         endif
      endif

ccc missing mass for K+ X channel
      if(nkp.eq.1.and.np.le.1.and.npip.eq.0) then
         call miss_mass(ikp(1),0,0,0,mk,0.,0.,0.,mprot)
         if(mx2.gt.0.0.and.abs(m(ikp(1))-mk**2).lt.0.035) 
     &                         call hf1(10011,sqrt(mx2),1.)
         if(mx2.gt.0.0.and.abs(m(ikp(1))-mk**2).gt.0.035.AND.abs(m(ikp(1))-mk**2).lt.0.07) 
     &                         call hf1(10012,sqrt(mx2),1.)
      endif
      if(nkp.eq.1.and.np.eq.1.and.npip.eq.0.and.npim.eq.0) then
         call miss_mass(ip(1),ikp(1),0,0,mprot,mk,0.,0.,mprot)
         if(mx2.gt.0.0) call hf1(10013,sqrt(mx2),1.)
         if(abs(mx2-mk**2).lt.0.035) then
            call miss_mass(ip(1),0,0,0,mprot,0.,0.,0.,mprot)
            if(mx2.gt.0.0) call hf1(10014,sqrt(mx2),1.)
         endif
      endif

ccc missing mass for K+ pi+ pi- channel
      if(nkp.eq.1.and.npip.eq.1.and.npim.eq.1) then
         call inv_mass(ipip(1),ipim(1),0,0,mpi,mpi,0.,0.)
         call hf1(10021,sqrt(mi2),1.)
         call miss_mass(ipip(1),ipim(1),ikp(1),0,mpi,mpi,mk,0.,mprot)
         if(mx2.gt.0.0) call hf1(10022,sqrt(mx2),1.)
         if(abs(mx2-mprot**2).lt.0.04) then
            call miss_mass(ikp(1),0,0,0,mk,0.,0.,0.,mprot)
            if(mx2.gt.0.0) call hf1(10023,sqrt(mx2),1.)
            call miss_mass(ikp(1),ipim(1),0,0,mk,mpi,0.,0.,mprot)
            if(mx2.gt.0.0) call hf1(10024,sqrt(mx2),1.)
            call miss_mass(ikp(1),ipip(1),0,0,mk,mpi,0.,0.,mprot)
            if(mx2.gt.0.0) call hf1(10025,sqrt(mx2),1.)
         endif
      endif

ccc missing mass for K+ K- pi+  channel
      if(nkp.eq.1.and.nkm.eq.1.and.npip.eq.1) then
         call miss_mass(ikp(1),ikm(1),ipip(1),0,mk,mk,mpi,0.,mprot)
         if(mx2.gt.0.0) call hf1(10031,sqrt(mx2),1.)
         call inv_mass(ikm(1),ipip(1),0,0,mk,mpi,0.,0.)
         call hf1(10032,sqrt(mi2),1.)
      endif

ccc missing mass for p X channel
      if(np.eq.1)then
         call miss_mass(ip(1),0,0,0,mprot,0.,0.,0.,mprot)
         if(mx2.gt.0.0) call hf1(10006,sqrt(mx2),1.)
      endif
ccc missing mass for pi+ neutron channel
      if(npip.eq.1.and.np.eq.0.and.npim.eq.0)then
         call miss_mass(ipip(1),0,0,0,mpi,0.,0.,0.,mprot)
         if(mx2.gt.0.0) call hf1(10007,sqrt(mx2),1.)
      endif


c     
      call fill_csql()

      RETURN
      END
c
c------------------------------------------------------------------------------
C=======================================================================
      Subroutine miss_mass(id1,id2,id3,id4,s1,s2,s3,s4,mtarg)
c
      integer ids(4),i
      integer id1,id2,id3,id4
      real ms(4)
      real s1,s2,s3,s4,mtarg
c
#include "seb_nt.inc"
c
      real pi,r2d
      data pi,r2d/3.141592,57.29578/
c
      real u,qx,qy,qz
      common/photv/u,qx,qy,qz,q2
c
      real mx2,ex,px,p0x,p0y,p0z,thx,phx
      common/mispart/mx2,ex,px,thx,phx,p0x,p0y,p0z
      real mi2,ey,py,p1x,p1y,p1z
      common/recpart/mi2,ey,py,p1x,p1y,p1z
c
      ids(1)=id1
      ids(2)=id2
      ids(3)=id3
      ids(4)=id4
      ms(1)=s1
      ms(2)=s2
      ms(3)=s3
      ms(4)=s4
      ex=u+mtarg
      p0x=qx
      p0y=qy
      p0z=qz
      Do i=1,4
         if(ids(i).gt.0)then
            p0x=p0x-p(ids(i))*cx(ids(i))
            p0y=p0y-p(ids(i))*cy(ids(i))
            p0z=p0z-p(ids(i))*cz(ids(i))
            e0=sqrt(p(ids(i))**2+ms(i)**2)
            ex=ex-e0
         elseif(ids(i).lt.0)then
            p0x=p0x-p1x
            p0y=p0y-p1y
            p0z=p0z-p1z
            ex=ex-ey
         endif
      enddo
      px=sqrt(p0x**2+p0y**2+p0z**2)
      mx2=ex**2-px**2
c
      end
C=======================================================================
      Subroutine inv_mass(ip1,ip2,ip3,ip4,s1,s2,s3,s4)
      integer ip(4),ip1,ip2,ip3,ip4
      real ms(4),s1,s2,s3,s4
      integer i
c
#include "seb_nt.inc"
c
      real pi,r2d
      data pi,r2d/3.141592,57.29578/
c
      real u,qx,qy,qz
      common/photv/u,qx,qy,qz,q2
c
      real mx2,ex,px,p0x,p0y,p0z,thx,phx
      common/mispart/mx2,ex,px,thx,phx,p0x,p0y,p0z
      real mi2,ey,py,p1x,p1y,p1z
      common/recpart/mi2,ey,py,p1x,p1y,p1z
c
      ip(1)=ip1
      ip(2)=ip2
      ip(3)=ip3
      ip(4)=ip4
      ms(1)=s1
      ms(2)=s2
      ms(3)=s3
      ms(4)=s4
      p1x=0.
      p1y=0.
      p1z=0.
      ey=0.
c mass m1 is a invariant mass of ip(1) and ip(2), m2 is a mass of ip(1) and ip(3)
      do i=1,4
         if(ip(i).gt.0)then
            p1x=p1x+p(ip(i))*cx(ip(i))
            p1y=p1y+p(ip(i))*cy(ip(i))
            p1z=p1z+p(ip(i))*cz(ip(i))
            ey=sqrt(p(ip(i))**2+ms(i)**2)+ey
         elseif(ip(i).lt.0)then
            p1x=p1x+p0x
            p1y=p1y+p0y
            p1z=p1z+p0z
            ey=sqrt(px**2+ms(i)**2)+ey
         else
c            p1x=0.
c            p1y=0.
c            p1z=0.
c            ey=100.
         endif
      enddo
      py=sqrt(p1x**2+p1y**2+p1z**2)
      mi2=ey**2-py**2
c
      return
      end
C=======================================================================

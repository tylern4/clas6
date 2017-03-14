
      SUBROUTINE FILL_SEB_NT(ierr)
      
      IMPLICIT NONE
      SAVE
      
#include "bcs.inc"
#include "seb_nt.inc"

      INTEGER ierr
      INTEGER NWRITE
      INTEGER IHEVT, IEVNT, IDCPB, ICCPB, ISCPB, IECPB, ILCPB, ISTPB, ITAGR, ITGPB,IHEAD
      DATA IHEVT,IEVNT,IDCPB,ICCPB,ISCPB,IECPB,ILCPB,ISTPB,ITAGR,ITGPB,IHEAD/0,0,0,0,0,0,0,0,0,0,0/
      integer ind,i,indx,ncol,nd,ix,tb,tc
      INTEGER namind,MAMIND
      external namind,MAMIND
      
      IF (IHEAD.eq.0) IHEAD=NAMIND('HEAD')
      IF (IHEVT.eq.0) IHEVT=NAMIND('HEVT')
      IF (IEVNT.eq.0) IEVNT=NAMIND('EVNT')
      IF (IDCPB.eq.0) IDCPB=NAMIND('DCPB')
      IF (ICCPB.eq.0) ICCPB=NAMIND('CCPB')
      IF (ISCPB.eq.0) ISCPB=NAMIND('SCPB')
      IF (IECPB.eq.0) IECPB=NAMIND('ECPB')
      IF (ILCPB.eq.0) ILCPB=NAMIND('LCPB')
      IF(ISTPB.eq.0)ISTPB=MAMIND(IW,'STPB')
      IF(ITAGR.eq.0)ITAGR=MAMIND(IW,'TAGR') 
      IF(ITGPB.eq.0)ITGPB=MAMIND(IW,'TGPB') 
      
      ierr        = 0
      npart        = 0
      gpart        = 0
      ind        = 0
      
      if (IHEVT.gt.0) ind = IW(IHEVT)
      
      IF (ind.NE.0) THEN
         evstat                = IW(ind+1)
         evntid                = IW(ind+3)
         evtype                = IW(ind+4)
         npart                = IW(ind+5)/100
         gpart                = mod(IW(ind+5),100)
         tb                = amod(float(IW(ind+6)),float(65536))
         tc              = IW(ind+6)/65536
         if (tc.gt.2) tc = 3
         tb              = tb/10000+1+10*(tc-1)
         evclas          = tb
   !     evthel          = IW(ind+7)
         evntclas2       = IW(ind+6)
         q_l                = RW(ind+8)
         t_l                = RW(ind+9)
         tr_time                = RW(ind+10)
         rf_time1        = RW(ind+11)
         rf_time2        = RW(ind+12)
      ENDIF
     
      if (IHEAD.gt.0) ind = IW(IHEAD)

      IF (ind.NE.0) THEN
        evthel = IW(ind+7)
!     evthel2= IW(ind+1)      !This variable is helicity without the intt condition
      endif

      IF ( npart.GE.1) THEN
         
         ind=0
         if(IEVNT.gt.0)ind=IW(IEVNT)
         If(ind.NE.0) Then
            ncol                = iw(ind-5)
            nd                = iw(ind)
            i                = 0
            do ix=1,nd,ncol
               indx                = ind+ix-1
               i                = i+1
               id(i)                = iw(indx+1)
               p(i)                = rw(indx+2)
               m(i)                = rw(indx+3)
               q(i)                = iw(indx+4)
               b(i)                = rw(indx+5)
               cx(i)                = rw(indx+6)
               cy(i)                = rw(indx+7)
               cz(i)                = rw(indx+8)
               vx(i)                = rw(indx+9)
               vy(i)                = rw(indx+10)
               vz(i)                = rw(indx+11)
               dc(i)                = iw(indx+12)
               cc(i)                = iw(indx+13)
               sc(i)                = iw(indx+14)
               ec(i)                = iw(indx+15)
               lec(i)        = iw(indx+16)
               stat(i)         = iw(indx+18)
            enddo
         EndIf
         
         ind=0
         if(IDCPB.gt.0)ind=IW(IDCPB)
         If(ind.NE.0) Then
            ncol                = iw(ind-5)
            nd                = iw(ind)
            i                = 0
            do ix=1,nd,ncol
               indx                = ind+ix-1
               i                = i+1
               dc_sect(i)         = iw(indx+1)/100
               dc_trk(i)         = mod(iw(indx+1),100)
               dc_xSC(i)         = rw(indx+2)
               dc_ySC(i)         = rw(indx+3)
               dc_zSC(i)         = rw(indx+4)
               dc_cxSC(i)         = rw(indx+5)
               dc_cySC(i)         = rw(indx+6)
               dc_czSC(i)         = rw(indx+7)
               dc_xEC(i)         = rw(indx+8)
               dc_yEC(i)         = rw(indx+9)
               dc_zEC(i)         = rw(indx+10)
               dc_thCC(i)         = rw(indx+11)
               dc_c2(i)         = rw(indx+12)
               dc_stat(i)         = iw(indx+13)
               dc_part        = i
            enddo
         EndIf
         
         ind=0
         if(IECPB.gt.0)ind=IW(IECPB)
         ind=IW(IECPB)
         If(ind.NE.0) Then
            ncol                = iw(ind-5)
            nd                = iw(ind)
            i                = 0
            do ix=1,nd,ncol
               indx                = ind+ix-1
               i                = i+1
               ec_sect(i)         = iw(indx+1)/100
               ec_whol(i)         = mod(iw(indx+1),100)
               etot(i)         = rw(indx+2)
               ec_ei(i)         = rw(indx+3)
               ec_eo(i)         = rw(indx+4)
               ec_t(i)         = rw(indx+5)
               ec_r(i)         = rw(indx+6)
               ech_x(i)         = rw(indx+7)
               ech_y(i)         = rw(indx+8) 
               ech_z(i)         = rw(indx+9)
               ec_M2(i)         = rw(indx+10)
               ec_M3(i)         = rw(indx+11)
               ec_M4(i)         = rw(indx+12)
               ec_InSt(i)        = iw(indx+13)
               ec_OuSt(i)        = iw(indx+14)
               ec_c2(i)         = rw(indx+15)
               ec_stat(i)         = iw(indx+16)
               ec_part        = i
            enddo
         EndIf
         
         ind=0
         if(ILCPB.gt.0)ind=IW(ILCPB)
         ind=0
         If(ind.NE.0) Then
            ncol                = iw(ind-5)
            nd                = iw(ind)
            i                = 0
            do ix=1,nd,ncol
               indx                = ind+ix-1
               i                = i+1
               lec_sect(i)         = iw(indx+1)/100
               lec_hit(i)         = mod(iw(indx+1),100)
               lec_etot(i)         = rw(indx+2)
               lec_t(i)         = rw(indx+3)
               lec_r(i)         = rw(indx+4)
               lec_x(i)         = rw(indx+5)
               lec_y(i)         = rw(indx+6)
               lec_z(i)         = rw(indx+7)
               lec_c2(i)         = rw(indx+8)
               lec_stat(i)         = iw(indx+9)
               lac_part        = i
            enddo
         EndIf
c     
         ind=0
         if(ISCPB.gt.0)ind=IW(ISCPB)
         If(ind.NE.0) Then
            ncol                = iw(ind-5)
            nd                = iw(ind)
            i                = 0
            do ix=1,nd,ncol
               indx                = ind+ix-1
               i                = i+1
               sc_sect(i)         = iw(indx+1)/10000
               sc_pd(i)         = (iw(indx+1)-sc_sect(i)*10000)/100
               sc_hit(i)         = iw(indx+1)-sc_sect(i)*10000-sc_pd(i)*100
               edep(i)        = rw(indx+2)
               sc_t(i)         = rw(indx+3)
               sc_r(i)         = rw(indx+4)
               sc_c2(i)         = rw(indx+5)
               sc_stat(i)         = iw(indx+6)
               sc_part        = i
            enddo
         EndIf
c     
         ind=0
         if(ICCPB.gt.0)ind=IW(ICCPB)
         If(ind.NE.0) Then
            ncol                = iw(ind-5)
            nd                = iw(ind)
            i                = 0
            do ix=1,nd,ncol
               indx                = ind+ix-1
               i                = i+1
               cc_sect(i)         = iw(indx+1)/100
               cc_hit(i)         = MOD(iw(indx+1),100)
               nphe(i)         = rw(indx+2)
               cc_t(i)         = rw(indx+3)
               cc_r(i)         = rw(indx+4)
               cc_c2(i)         = rw(indx+5)
               cc_segm(i)         = iw(indx+6)
               cc_part        = i
            enddo
         EndIf
      ENDIF
c     
      ind=0
      if(ISTPB.gt.0)ind=IW(ISTPB)
      If(ind.NE.0) Then
         ncol=iw(ind-5)
         nd=iw(ind)
         i=0
         do ix=1,nd,ncol
            indx=ind+ix-1
            i=i+1
            st_status(i) = iw(indx+1)
            st_time(i) = rw(indx+2)
            st_rtrk(i) = rw(indx+3)
            st_part=i
         enddo
      EndIf
c
      if(gpart.gt.0)ierr=1
c      
      RETURN
      END




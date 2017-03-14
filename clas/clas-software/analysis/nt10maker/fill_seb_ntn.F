      SUBROUTINE FILL_SEB_NTN(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_seb_ntn.F,v 1.4 2003/06/03 11:44:23 stepanya Exp $
c     
c     Documentation for subroutine FILL_SEB_NT
c     
c     Purpose: To fill Ntuple #10 bloks - recontsructed particle information.
c     --------
c     
c     Input Parameters:  NON
c     ----------------
c     
c     Output Parameters:  Error flag
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    S. Stepanyan     Created:  Fri Mar  7 09:43:25 EST 1997
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
#include "seb_ntn.inc"
c     #include "photon_stuff.inc"
c     #include "histtcl.inc"
c     #include "clasmdl.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
      INTEGER ierr
c     
c     Local pre-defined variables:
c     ---------------------------
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER IHEVT, IEVNT, IDCPB, ICCPB, ISCPB, IECPB, ILCPB, ISTPB, ITAGR, ITGPB, ITRPB,ITGBI
      DATA IHEVT,IEVNT,IDCPB,ICCPB,ISCPB,IECPB,ILCPB,ISTPB,ITAGR,ITGPB,ITRPB,ITGBI/0,0,0,0,0,0,0,0,0,0,0,0/
      integer ind,i,indx,ncol,nd,ix,tc,tb,nrow,ncol1,nrow1,indx1,itmp, indx0
      INTEGER mamind, mbank, ibmpr, ix0, idc, iec,isc, ilec, ist, nd0, ncol0, ind0, i0
      real q_t_l, q_t, t_l_t, I_fc, totcpu, scr(20), ecr(20), str(20), lecr(20)
      INTEGER nelect,nelec(6),sect,ntbttrk,nhbttrk,nelectp,nelectn,nprot,npip, ndeut,nkaon
      data nelec/0,0,0,0,0,0/
      data nelect,ntbttrk,nhbttrk,nelectp,nelectn,nprot,npip,ndeut,nkaon/0,0,0,0,0,0,0,0,0/
      external mamind,mbank
c     _end_var
c     
c     executable code for routine FILL_PART_NT:
c---- 6----------------------------------------------------------------72
c     
c     List of banks to dump to N-tuple
      IF(IHEVT.eq.0)IHEVT=MAMIND(IW,'HEVT')
      IF(IEVNT.eq.0)IEVNT=MAMIND(IW,'EVNT')
      IF(IDCPB.eq.0)IDCPB=MAMIND(IW,'DCPB')
      IF(ITRPB.eq.0)ITRPB=MAMIND(IW,'TRPB')
      IF(ICCPB.eq.0)ICCPB=MAMIND(IW,'CCPB')
      IF(ISCPB.eq.0)ISCPB=MAMIND(IW,'SCPB')
      IF(IECPB.eq.0)IECPB=MAMIND(IW,'ECPB')
      IF(ILCPB.eq.0)ILCPB=MAMIND(IW,'LCPB')
      IF(ISTPB.eq.0)ISTPB=MAMIND(IW,'STPB')
      IF(ITAGR.eq.0)ITAGR=MAMIND(IW,'TAGR') 
      IF(ITGPB.eq.0)ITGPB=MAMIND(IW,'TGPB') 
      IF(ITGBI.eq.0)ITGBI=MAMIND(IW,'TGBI') 
c     
      ierr=0
      npart=0
      gpart=0
      dc_part=0
      ec_part=0
      sc_part=0
      st_part=0
c     
      ind=0
      if(IHEVT.gt.0)ind=IW(IHEVT)
      IF(ind.NE.0) THEN
         evstat=IW(ind+1)
         evntid=IW(ind+3)
         evntype=IW(ind+4)
         npart=IW(ind+5)/100
         gpart=mod(IW(ind+5),100)
         evclas=IW(ind+6)
         q_l=RW(ind+7)
         t_l=RW(ind+9)
         tr_time=RW(ind+10)
         rf_time=RW(ind+11)
      ENDIF
c     
      IF ( gpart.GE.1 ) THEN
c     
         ind=0
         if(ICCPB.gt.0)ind=IW(ICCPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               cc_sect(i) = iw(indx+1)/100
               cc_hit(i) = MOD(iw(indx+1),100)
               nphe(i) = int(rw(indx+2))
               if(nphe(i).gt.10000)nphe(i)=10000
               cc_t(i) = rw(indx+3)
               cc_r(i) = rw(indx+4)
               cc_c2(i) = rw(indx+5)
               cc_segm(i) = iw(indx+6)
               cc_part=i
            enddo
         EndIf
c     
         ind0=0
         if(IEVNT.gt.0)ind0=IW(IEVNT)
         If(ind0.NE.0) Then
            ind=0
            if(IDCPB.gt.0)ind=IW(IDCPB)
            ncol=iw(ind-5)
            nd=iw(ind)
            nrow=iw(ind-4)
            ncol0=iw(ind0-5)
            nd0=iw(ind0)
            i0=0
            i=0
            Do ix0=1,nd0,ncol0
               indx0=ind0+ix0-1
               i=i+1
               If(iw(indx0+4).ne.0.and.iw(indx0+12).gt.0)then
c     If carged particle - see if dc_stat is > 0
                  do ix=1,nrow
                     indx=ind+ncol*(ix-1)
                     if(ix.eq.iw(indx0+12).and.iw(indx+13).gt.0)then
                        dc_part=dc_part+1
                        dc_sect(dc_part) = iw(indx+1)/100
                        dc_trk(dc_part) = mod(iw(indx+1),100)
                        dc_xSC(dc_part) = rw(indx+2)
                        dc_ySC(dc_part) = rw(indx+3)
                        dc_zSC(dc_part) = rw(indx+4)
                        dc_cxSC(dc_part) = rw(indx+5)
                        dc_cySC(dc_part) = rw(indx+6)
                        dc_czSC(dc_part) = rw(indx+7)
                        dc_vx(dc_part) = rw(indx+8)
                        dc_vy(dc_part) = rw(indx+9)
                        dc_vz(dc_part) = rw(indx+10)
                        dc_vr(dc_part) = rw(indx+11)
                        dc_c2(dc_part) = rw(indx+12)
                        dc_stat(dc_part) = iw(indx+1)
                        idc=dc_part
                     elseif(ix.eq.iw(indx0+12).and.iw(indx+13).le.0)then
                        idc=-100
                     endif
                  enddo
               Else
                  idc=100
               EndIf
c     Fill evnt if valid particle    
               If(idc.gt.0)Then
                  i0=i0+1
                  dc(i0)=0
                  if(idc.lt.20)dc(i0)=dc_part
                  id(i0)=iw(indx0+1)
                  p(i0)=rw(indx0+2)
                  m(i0)=rw(indx0+3)
                  q(i0)=iw(indx0+4)
                  b(i0)=rw(indx0+5)
                  cx(i0)=rw(indx0+6)
                  cy(i0)=rw(indx0+7)
                  cz(i0)=rw(indx0+8)
                  vx(i0)=rw(indx0+9)
                  vy(i0)=rw(indx0+10)
                  vz(i0)=rw(indx0+11)
                  cc(i0)=iw(indx0+13)
                  if(cc(i0).gt.0)then
                     ccst(i0)=cc_segm(cc(i0))+1000*cc_sect(cc(i0))+10000*nphe(cc(i0))
                  else
                     ccst(i0)=0
                  endif
                  scr(i0)=iw(indx0+14)
                  sc(i0)=scr(i0)
                  ecr(i0)=iw(indx0+15)
                  ec(i0)=ecr(i0)
                  lecr(i0)=iw(indx0+16)
                  lec(i0)=lecr(i0)
                  str(i0)=iw(indx0+17)
                  st(i0)=str(i0)
                  stat(i0)=iw(indx0+18)
                  npart=i0
               EndIf
            EndDo
         EndIf
c     EVNT is done, fill PB banks (DCPB also done)
         IF(npart.GT.0)THEN
            gpart=npart
            ierr=npart
c
            ind=0
            if(IECPB.gt.0)ind=IW(IECPB)
            ind=IW(IECPB)
            If(ind.NE.0) Then
               ncol=iw(ind-5)
               nd=iw(ind)
               iec=0
               i=0
               do ix=1,nd,ncol
                  indx=ind+ix-1
                  i=i+1
                  do i0=1,npart
                     if(i.eq.ecr(i0))then
                        iec=iec+1
                        ec(i0)=iec
                        ec_sect(iec) = iw(indx+1)/100
                        ec_whol(iec) = mod(iw(indx+1),100)
                        etot(iec) = rw(indx+2)
                        ec_ei(iec) = rw(indx+3)
                        ec_eo(iec) = rw(indx+4)
                        ec_t(iec) = rw(indx+5)
                        ec_r(iec) = rw(indx+6)
                        ech_x(iec) = rw(indx+7)
                        ech_y(iec) =rw(indx+8) 
                        ech_z(iec) = rw(indx+9)
                        ec_M2(iec) = rw(indx+10)
                        ec_M3(iec) = rw(indx+11)
                        ec_M4(iec) = rw(indx+12)
                        ec_InSt(iec)=iw(indx+13)
                        ec_OuSt(iec)=iw(indx+14)
                        ec_c2(iec) = rw(indx+15)
                        ec_stat(iec) = iw(indx+16)+ec_sect(iec)*100**3
                        ec_part=iec
                     endif
                  enddo
               enddo
            EndIf
c     
            ind=0
            if(ILCPB.gt.0)ind=IW(ILCPB)
            If(ind.NE.0) Then
               ncol=iw(ind-5)
               nd=iw(ind)
               i=0
               ilec=0
               do ix=1,nd,ncol
                  indx=ind+ix-1
                  i=i+1
                  do i0=1,npart
                     if(i.eq.lecr(i0))then
                        ilec=ilec+1
                        lec(i0)=ilec
                        lec_sect(ilec) = iw(indx+1)/100
                        lec_hit(ilec) = mod(iw(indx+1),100)
                        lec_etot(ilec) = rw(indx+2)
                        lec_t(ilec) = rw(indx+3)
                        lec_r(ilec) = rw(indx+4)
                        lec_x(ilec) = rw(indx+5)
                        lec_y(ilec) = rw(indx+6)
                        lec_z(ilec) = rw(indx+7)
                        lec_c2(ilec) = rw(indx+8)
                        lec_stat(ilec) = lec_hit(ilec)+100*lec_sect(ilec)
                        lec_ein(ilec) = rw(indx+10)
                        lac_part=ilec
                     endif
                  enddo
               enddo
            EndIf
c     
            ind=0
            if(ISCPB.gt.0)ind=IW(ISCPB)
            If(ind.NE.0) Then
               ncol=iw(ind-5)
               nd=iw(ind)
               i=0
               isc=0
               do ix=1,nd,ncol
                  indx=ind+ix-1
                  i=i+1
                  do i0=1,npart
                     if(i.eq.scr(i0))then
                        isc=isc+1
                        sc(i0)=isc
                        sc_sect(isc) = iw(indx+1)/10000
                        sc_pd(isc) =(iw(indx+1)-sc_sect(isc)*10000)/100
                        sc_hit(isc) = iw(indx+1)-sc_sect(isc)*10000-sc_pd(isc)*100
                        edep(isc) = rw(indx+2)
                        sc_t(isc) = rw(indx+3)
                        sc_r(isc) = rw(indx+4)
                        sc_c2(isc) = rw(indx+5)
                        sc_stat(isc) = iw(indx+6)+100*sc_pd(isc)+10000*sc_sect(isc)
                        sc_part=isc
                     endif
                  enddo
               enddo
            EndIf
c     
            ind=0
            if(ISTPB.gt.0)ind=IW(ISTPB)
            If(ind.NE.0) Then
               ncol=iw(ind-5)
               nd=iw(ind)
               i=0
               ist=0
               do ix=1,nd,ncol
                  indx=ind+ix-1
                  i=i+1
                  do i0=1,npart
                     if(i.eq.str(i0))then
                        ist=ist+1
                        st(i0)=ist
                        st_status(ist) = iw(indx+1)
                        st_time(ist) = rw(indx+2)
                        st_rtrk(ist) = rw(indx+3)
                        st_part=ist
                     endif
                  enddo
               enddo
            EndIf
c     
            do i0=1,npart
               itmp=dc(i0)+sc(i0)*10**2+ec(i0)*10**4+lec(i0)*10**6+st(i0)*10**8
               if(stat(i0).ne.0)then
                  stat(i0)=stat(i0)*itmp/abs(stat(i0))
               else
                  stat(i0)=-itmp
               endif
            enddo
         EndIf
c     - End of the loop through particles in the event 
      ENDIF
      RETURN
      END
c------------------------------------------------------------------------------





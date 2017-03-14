      SUBROUTINE FILL_SEB_NTN(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_seb_ntn.F,v 1.5 2008/03/16 04:33:09 fklein Exp $
c     
c     Documentation for subroutine FILL_SEB_NTN
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
#include "user_control.inc"
#include "seb_ntn.inc"
#include "csql.inc"
c don't use this seb include #include "photon_stuff.inc"
#include "histtcl.inc"
#include "clasmdl.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
      INTEGER ierr
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_seb_ntn.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:09 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1     '$Id: fill_seb_ntn.F,v 1.5 2008/03/16 04:33:09 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_SEB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      INTEGER IHEVT, IEVNT, IDCPB, ICCPB, ISCPB, IECPB, ILCPB, ISTPB, ITAGR, ITGPB
      INTEGER ITRPB,ITGBI,IFBPM,IHLS
      DATA IHEVT,IEVNT,IDCPB,ICCPB,ISCPB,IECPB,ILCPB,ISTPB,ITAGR,ITGPB/0,0,0,0,0,0,0,0,0,0/
      DATA ITRPB,ITGBI,IFBPM,IHLS/0,0,0,0/
      integer ind,i,indx,ncol,nd,ix,tc,tb,nrow,ncol1,nrow1,ind1,indx1,itmp,ind2,ind3
      INTEGER mamind, mbank, ibmpr
      integer hbit,sbit,htbit,stbit,sbitold,hbitold,j,sect
      data sbitold,hbitold/2,2/
      external mamind,mbank
      real mp,mpi,r2d,pi
      data mp,mpi,r2d,pi/0.938,0.139,57.29578,3.14159/
      INTEGER btype
      LOGICAL YES_PHOTON
c     _end_var
c     
c     executable code for routine FILL_PART_NT:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='Filling reconstructed particles Ntuple.'
         CALL RECMES(CRNAME,'I',CRMESS)
c  get beam info
         YES_PHOTON = .FALSE.
         call get_beam_type(btype)
         if(btype.eq.1.or.btype.eq.3.or.btype.eq.4) YES_PHOTON = .TRUE.
c
         nhbttrk=0
         ntbttrk=0
         do i=1,6
            npimsect(i)=0
         enddo
         nelectp=0
         nelectn=0
         nprot=0
         npip=0
         npim=0
         ndeut=0
         nkaonp=0
         nkaonm=0
         nphoton=0
      ENDIF
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
      IF(IFBPM.eq.0)IFBPM=MAMIND(IW,'FBPM') 
      IF(IHLS.eq.0)IHLS=MAMIND(IW,'HLS ') 
      ierr=0
      npart=0
      gpart=0
      dc_part=0
      ind=0
      ind1=0
      ind2=0
      ind3=0
      stbit=3
      htbit=3
c     
      if(IHLS.gt.0)ind3=iw(ihls)
      if(ind3.gt.0)then
         stbit=int(iw(ind3+1)/2**31)
         htbit=int((iw(ind3+1)-stbit*2**31)/2**30)
      endif
      if(ITGBI.gt.0)ind1=IW(ITGBI)
      if(ind1.gt.0)then
         hbit=int(iw(ind1+1)/32768)
         sbit=int((iw(ind1+1)-32768*hbit)/16384)
         if(sbit.ne.sbitold)then
            hbitold=hbit
            sbitold=sbit
         endif
      endif
      if(IFBPM.gt.0)ind2=IW(IFBPM)
      if(IHEVT.gt.0)ind=IW(IHEVT)
      IF(ind.NE.0) THEN
         evstat=IW(ind+1)
         evntid=IW(ind+3)
         evntype=IW(ind+4)
         npart=IW(ind+5)/100
         gpart=mod(IW(ind+5),100)
         evntclas=IW(ind+6)
         q_l=RW(ind+7)
         t_l=RW(ind+9)
         tr_time=RW(ind+10)
         rf_time=RW(ind+11)
         if(ind1.gt.0)then
            lt1=iw(ind1+1)
            hlsc=iw(ind1+2)
            intt=iw(ind1+3)
         endif           
         if(ind2.gt.0)then
            rstr1=iw16(ind2*2+3)
            rstr2=iw16(ind2*2+6)
         endif           
      ENDIF
      IF ( npart.GE.1.OR.(YES_PHOTON.AND.gpart.GE.1)) THEN
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
               nphe(i) = rw(indx+2)
c     if(nphe(i).gt.10000)nphe(i)=10000
               cc_t(i) = rw(indx+3)
               cc_r(i) = rw(indx+4)
               cc_c2(i) = rw(indx+5)
               cc_segm(i) = iw(indx+6)
               cc_part=i
            enddo
         EndIf
c     
         ind=0
         if(IEVNT.gt.0)ind=IW(IEVNT)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               id(i)=iw(indx+1)
               p(i)=rw(indx+2)
               m(i)=rw(indx+3)
               q(i)=iw(indx+4)
               b(i)=rw(indx+5)
               cx(i)=rw(indx+6)
               cy(i)=rw(indx+7)
               cz(i)=rw(indx+8)
               vx(i)=rw(indx+9)
               vy(i)=rw(indx+10)
               vz(i)=rw(indx+11)
               dc(i)=iw(indx+12)
               cc(i)=iw(indx+13)
               sc(i)=iw(indx+14)
               ec(i)=iw(indx+15)
               lec(i)=iw(indx+16)
               st(i)=iw(indx+17)
               stat(i)=iw(indx+18)
               itmp=dc(i)+sc(i)*(2**5)+ec(i)*(2**10)+cc(i)*(2**15)
     +              +lec(i)*(2**20)+st(i)*(2**25)
               if(stat(i).ne.0)then
                  stat(i)=stat(i)*itmp/abs(stat(i))
               else
                  stat(i)=-itmp
               endif
            enddo
         EndIf
         ind=0
         if(IDCPB.gt.0)ind=IW(IDCPB)
         if(ITRPB.gt.0)ind1=IW(ITRPB)
         If(ind.NE.0) Then
            if(ind1.gt.0)then
               ncol1=iw(ind1-5)
               nrow1=iw(ind1-4)
            endif
            ncol=iw(ind-5)
            nd=iw(ind)
            nrow=iw(ind-4)
            i=0
            do ix=1,nrow
               indx=ind+ncol*(ix-1)
               i=i+1
               dc_sect(i) = iw(indx+1)/100
               dc_trk(i) = mod(iw(indx+1),100)
               dc_xSC(i) = rw(indx+2)
               dc_ySC(i) = rw(indx+3)
               dc_zSC(i) = rw(indx+4)
               dc_cxSC(i) = rw(indx+5)
               dc_cySC(i) = rw(indx+6)
               dc_czSC(i) = rw(indx+7)
c     dc_vx(i) = rw(indx+8)
c     dc_vy(i) = rw(indx+9)
c     dc_vz(i) = rw(indx+10)
               dc_vr(i) = rw(indx+11)
               dc_c2(i) = rw(indx+12)
               dc_stat(i) = iw(indx+1)
               if(iw(indx+13).lt.0)dc_stat(i)=-dc_stat(i)
               dc_part=i
               if(ind1.gt.0)then
                  indx1=ind1+ncol1*(ix-1)
c     tb_st(i) = iw(indx1+1)
                  tl1_x(i) = rw(indx1+2)
                  tl1_y(i) = rw(indx1+3)
                  tl1_z(i) = rw(indx1+4)
                  tl1_cx(i) = rw(indx1+5)
                  tl1_cy(i) = rw(indx1+6)
                  tl1_cz(i) = rw(indx1+7)
                  tl1_r(i) = rw(indx1+8)
               endif
            enddo
         EndIf
c     
         ind=0
         if(IECPB.gt.0)ind=IW(IECPB)
         ind=IW(IECPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               ec_sect(i) = iw(indx+1)/100
               ec_whol(i) = mod(iw(indx+1),100)
               etot(i) = rw(indx+2)
               ec_ei(i) = rw(indx+3)
               ec_eo(i) = rw(indx+4)
               ec_t(i) = rw(indx+5)
               ec_r(i) = rw(indx+6)
               ech_x(i) = rw(indx+7)
               ech_y(i) =rw(indx+8) 
               ech_z(i) = rw(indx+9)
               ec_M2(i) = rw(indx+10)
               ec_M3(i) = rw(indx+11)
               ec_M4(i) = rw(indx+12)
               ec_InSt(i)=iw(indx+13)
               ec_OuSt(i)=iw(indx+14)
               ec_c2(i) = rw(indx+15)
               ec_stat(i) = iw(indx+16)+ec_sect(i)*100**3
               ec_part=i
            enddo
         EndIf
         ind=0
         if(ILCPB.gt.0)ind=IW(ILCPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               lec_sect(i) = iw(indx+1)/100
               lec_hit(i) = mod(iw(indx+1),100)
               lec_etot(i) = rw(indx+2)
               lec_t(i) = rw(indx+3)
               lec_r(i) = rw(indx+4)
               lec_x(i) = rw(indx+5)
               lec_y(i) = rw(indx+6)
               lec_z(i) = rw(indx+7)
               lec_c2(i) = rw(indx+8)
               lec_stat(i) = lec_hit(i)+100*lec_sect(i)
               lec_ein(i) = rw(indx+10)
               lac_part=i
            enddo
         EndIf
         ind=0
         if(ISCPB.gt.0)ind=IW(ISCPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               sc_sect(i) = iw(indx+1)/10000
               sc_pd(i) =(iw(indx+1)-sc_sect(i)*10000)/100
               sc_hit(i) = iw(indx+1)-sc_sect(i)*10000-sc_pd(i)*100
               edep(i) = rw(indx+2)
               sc_t(i) = rw(indx+3)
               sc_r(i) = rw(indx+4)
               sc_c2(i) = rw(indx+5)
               sc_stat(i) = iw(indx+6)+100*sc_pd(i)+10000*sc_sect(i)
               sc_part=i
            enddo
         EndIf
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
               st_sector(i) = iw(indx+1)/100
               st_ihit(i) = iw(indx+1)-st_sector(i)*100
               st_time(i) = rw(indx+2)
               st_rtrk(i) = rw(indx+3)
               st_trkno(i) = iw(indx+4)
               st_status(i) = iw(indx+5)
               st_part=i
            enddo
         EndIf
         
         ind=0
         if(ITAGR.gt.0)ind=IW(ITAGR)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               taghit = i
               E_gamma(i) = rw(indx+1)
               T_gamma_noRF(i) = rw(indx+2) 
               T_gamma(i) = rw(indx+3)
               tagstat(i) = iw(indx+4)
               Tid(i) = iw(indx+5)
               Eid(i) = iw(indx+6)
            enddo 
         Endif
         
         ind=0
         if(ITGPB.gt.0)ind=IW(ITGPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               taggoodhit = i
               tag_ptr(i) = iw(indx+1)/1000
               vertex_time(i) = rw(indx+2)
               tag_energy(i) = rw(indx+3) 
               dt_st_tag(i) = rw(indx+4)
            enddo 
         Endif
         
c     write(6,*)'Define a flag for Hfill    ',dc(1),ec(1),id(1),dc_stat(dc(1))
c     write(6,*)'dc_part',dc_part,(dc_stat(i),i=1,dc_part)

         If(YES_PHOTON) then
            ierr = 1
         ElseIf(Npart.gt.0)then
            ierr=1
            do i=1,dc_part
               if (dc_stat(i).GT.0) then ! If there is a TB track
                  ierr=ierr+1
               endif
            enddo
         EndIf
c         
         Do i=1,gpart
            if(dc(i).gt.0)then
               nhbttrk=nhbttrk+1               
               if(stat(i).gt.0.and.dc_stat(dc(i)).gt.0)then
                  ntbttrk=ntbttrk+1
                  sect=dc_sect(dc(i))
                  if(i.eq.1.and.ec(i).gt.0.and.cc(i).gt.0.and.sc(i).gt.0.and.q(i).lt.0)then
                     if(nphe(cc(i)).gt.20)then
                        nelect(sect)=nelect(sect)+1
                        if(evntclas.gt.0)then
                           nelectp=nelectp+1
                        elseif(evntclas.lt.0)then
                           nelectn=nelectn+1
                        endif
                     endif
                  endif
c
                  if(id(i).eq.2212)then
                     nprot=nprot+1
                  endif
                  if(id(i).eq.211)then
                     npip=npip+1
                  endif
                  if(id(i).eq.-211)then
                     npim=npim+1
                     npimsect(sect) = npimsect(sect)+1
                  endif
                  if(id(i).eq.45)then
                     ndeut=ndeut+1
                  endif
                  if(id(i).eq.321)then
                     nkaonp=nkaonp+1
                  endif
                  if(id(i).eq.-321)then
                     nkaonm=nkaonm+1
                  endif
               endif
            elseif(q(i).eq.0.and.(ec(i).gt.0.or.lec(i).gt.0))then
               if(abs(b(i)-1.).lt.0.1)nphoton=nphoton+1
            endif
         EndDo
         call fill_csql()
      ELSE
         ierr=0
      ENDIF
c     
      RETURN
      END
c------------------------------------------------------------------------------





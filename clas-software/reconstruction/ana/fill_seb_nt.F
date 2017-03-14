      SUBROUTINE FILL_SEB_NT(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_seb_nt.F,v 1.29 2008/03/16 04:33:09 fklein Exp $
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
#include "seb_nt.inc"
#include "csql.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_seb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.29 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:09 $')
      PARAMETER (CAUTHO = '$Author: fklein $')


c      DATA CRCSID/   
c     1     '$Id: fill_seb_nt.F,v 1.29 2008/03/16 04:33:09 fklein Exp $'   
c     2     /   


c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_SEB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      INTEGER IHEVT, IEVNT, IDCPB, ICCPB, ISCPB, IECPB, ILCPB, ISTPB, ITAGR 
      INTEGER ITGPB, ITRPB, ITGBI, ITBTR, IHBTR, ICL01
      DATA IHEVT,IEVNT,IDCPB,ICCPB,ISCPB,IECPB,ILCPB,ISTPB,ITAGR/0,0,0,0,0,0,0,0,0/
      DATA ITGPB,ITRPB, ITGBI, ITBTR, IHBTR, ICL01/0,0,0,0,0,0/
      integer ind,i,indx,ncol,nd,ix,tc,tb,nrow,ncol1,nrow1,ind1,indx1,sect
      INTEGER mamind, mbank, ibmpr
      external mamind,mbank
      integer ihlst,iclc,itr,it,itg1(16),ib16,ib15,itgb(16)
      data ib16,ib15/32768,16384/
      integer itbt_evt, ihbt_evt, hb_sect, tb_sect
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
c get info on beam type 
         YES_PHOTON = .FALSE.
         call get_beam_type(btype)
         if(btype.eq.1.or.btype.eq.3.or.btype.eq.4) YES_PHOTON = .TRUE.
         write(6,*)'fill_seb: Beam type is ',btype,YES_PHOTON
c
         nhbttrk=0
         ntbttrk=0
         do i=1,6
            npimsect(i)   = 0
            npipsect(i)   = 0
            nprotsect(i)  = 0
            nkoanpsect(i) = 0
            nkoanmsect(i) = 0
            nphotsect(i)  = 0
            nhbsect(i)    = 0
            ntbsect(i)    = 0
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
      IF(ITBTR.eq.0)ITBTR=MAMIND(IW,'TBTR')
      IF(IHBTR.eq.0)IHBTR=MAMIND(IW,'HBTR')      
      IF(ICL01.eq.0)ICl01=MAMIND(IW,'CL01')      
C
C     Here we find the number of TBT and HBT for monitoring
C
      itbt_evt = 0
      ihbt_evt = 0
      if( ITBTR .GT. 0 .AND. iw(ITBTR).GT.0 ) then
         ind=IW( ITBTR )
         itbt_evt = iw(ind-4)
         if( ind .ne. 0 ) then
            ntbttrk=ntbttrk + itbt_evt  
cPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
            ncol=iw(ind-5)
            nd=iw(ind)
            nrow=iw(ind-4)
            do ix=1,nrow
               indx=ind+ncol*(ix-1)
               tb_sect = iw(indx+9)/100
               ntbsect(tb_sect) = ntbsect(tb_sect) + 1
            enddo
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
         endif
      endif
      if( IHBTR .GT. 0 .AND. IW(IHBTR).GT.0 ) then 
         ind=IW( IHBTR )
         ihbt_evt = iw(ind-4)         
         if( ind .ne. 0 ) then
            nhbttrk=nhbttrk + ihbt_evt 
cPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
            ncol=iw(ind-5)
            nd=iw(ind)
            nrow=iw(ind-4)
            do ix=1,nrow
               indx=ind+ncol*(ix-1)
               hb_sect = iw(indx+9)/100
               nhbsect(hb_sect) = nhbsect(hb_sect) + 1
            enddo
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
         endif
      endif
c     
      if(ITGBI.gt.0 .AND. IW(ITGBI).GT.0) then
         ind=IW(ITGBI)
         ihlst=int(iw(ind+1)/ib16)
         iclc=int(mod(iw(ind+1),ib16)/ib15)
         itr=int(iw(ind+1)-ihlst*ib16-iclc*ib15)
         it=0
         do i=1,16 
            it=mod(itr,2)
            itg1(i)=it
            itr=itr/2
         enddo
         it=0
         do i=1,8
            if(itg1(i).eq.1)then
               it=it+1
               itgb(it)=i
            endif
         enddo
         hlsc=iw(ind+2)
         intt=iw(ind+3)
         l2bit=mod(iw(ind+4),2**8)
         if(int(iw(ind+4)/ib16).eq.0)l2bit=l2bit*(-1)
         l3bit=iw(ind+5)
      endif
c
      ierr=0
      npart=0
      gpart=0
      dc_part=0
      ind=0
      if(IHEVT.gt.0)ind=IW(IHEVT)
      IF(ind.NE.0) THEN
         evstat=IW(ind+1)
         evntid=IW(ind+3)
         evntype=IW(ind+4)
         npart=IW(ind+5)/100
         gpart=mod(IW(ind+5),100)
         evntclas=IW(ind+6)
         q_l=RW(ind+8)
         t_l=RW(ind+9)
         tr_time=RW(ind+10)
         rf_time=RW(ind+11)
      ENDIF
cc
       IF(ICL01.gt.0)ind=IW(ICL01)
       IF(ind.ne.0) THEN
       endif
      
      IF ( npart.GE.1.OR.(YES_PHOTON.AND.gpart.GE.1)) THEN
         ind=0
         if(IEVNT.gt.0)ind=IW(IEVNT)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
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
            nrow=MIN(iw(ind-4),maxparts)
c     if(nrow.ne.nrow1)write(6,*)'Nomber of entries in the DCPB and TRPB are different'
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
               dc_vx(i) = rw(indx+8)
               dc_vy(i) = rw(indx+9)
               dc_vz(i) = rw(indx+10)
               dc_vr(i) = rw(indx+11)
               dc_c2(i) = rw(indx+12)
               dc_stat(i) = iw(indx+13)
               dc_part=i
               if(ind1.gt.0)then
                  indx1=ind1+ncol1*(ix-1)
                  tb_st(i) = iw(indx1+1)
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
         if(ITRPB.gt.0)ind=IW(ITRPB)
         If(ind.gt.0)Then
            
         EndIf
         ind=0
         ec_part=0
         if(IECPB.gt.0) ind=IW(IECPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
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
               ec_stat(i) = iw(indx+16)
               ec_part=i
            enddo
         EndIf
         lac_part=0
         ind=0
         if(ILCPB.gt.0)ind=IW(ILCPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
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
               lec_stat(i) = iw(indx+9)
               lec_ein(i) = rw(indx+10)
               lac_part=i
c     write(*,*)lec_sect(i),lec_hit(i),lec_etot(i),lec_t(i),lec_r(i)
            enddo
         EndIf
c     
         ind=0
         sc_part=0
         if(ISCPB.gt.0)ind=IW(ISCPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               sc_sect(i) = iw(indx+1)/10000
               sc_pd(i) =(iw(indx+1)-sc_sect(i)*10000)/100
               sc_hit(i) = iw(indx+1)-sc_sect(i)*10000-sc_pd(i)*100
               if( abs(rw(indx+2)).lt.1.e-5.or.abs(rw(indx+2)).gt.10000) then
                  edep(i)=0.0
               else
                  edep(i) = rw(indx+2)
               endif
               sc_t(i) = rw(indx+3)
               sc_r(i) = rw(indx+4)
               sc_c2(i) = rw(indx+5)
               sc_stat(i) = iw(indx+6)
               sc_part=i
            enddo
         EndIf
         cc_part=0
         ind=0
         if(ICCPB.gt.0)ind=IW(ICCPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               cc_sect(i) = iw(indx+1)/100
               cc_hit(i) = MOD(iw(indx+1),100)
               nphe(i) = rw(indx+2)
               cc_t(i) = rw(indx+3)
               cc_r(i) = rw(indx+4)
               cc_c2(i) = rw(indx+5)
               cc_segm(i) = iw(indx+6)
               cc_part=i
            enddo
         EndIf
         
         ind=0
         st_part=0
         if(ISTPB.gt.0)ind=IW(ISTPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
            i=0
            do ix=1,nd,ncol
               indx=ind+ix-1
               i=i+1
               st_sector(i) = iw(indx+1)/100
               st_ihit(i) = iw(indx+1)-st_sector(i)*100
               st_time(i) = rw(indx+2)
               st_rtrk(i) = rw(indx+3)
               st_trkno(i) = iw(indx+4)
               st_status(i) = min(iw(indx+5),1000000)
               st_part=i
            enddo
         EndIf
         
         ind=0
         taghit=0
         if(ITAGR.gt.0)ind=IW(ITAGR)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
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
         taggoodhit=0
         if(ITGPB.gt.0)ind=IW(ITGPB)
         If(ind.NE.0) Then
            ncol=iw(ind-5)
            nd=iw(ind)
            if(iw(ind-4).GT.maxparts) nd=maxparts*ncol
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
c       
         npos=0
         nneg=0
         nph=0
         nel=0
         Do i=1,gpart
            if(dc(i).gt.0)then
c               nhbttrk=nhbttrk+1               
               if(stat(i).gt.0.and.dc_stat(dc(i)).gt.0)then
c                  ntbttrk=ntbttrk+1
                  sect=dc_sect(dc(i))
                  if(q(i).gt.0)npos=npos+1
                  if(q(i).lt.0)nneg=nneg+1
                  if(i.eq.1.and.ec(i).gt.0.and.cc(i).gt.0.and.sc(i).gt.0.and.q(i).lt.0)then
                     if(nphe(cc(i)).gt.20)then
                        nel=nel+1
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
                     nprotsect(sect) = nprotsect(sect)+1
                  endif
                  if(id(i).eq.211)then
                     npip=npip+1
                     npipsect(sect) = npipsect(sect)+1
                     
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
                     nkoanpsect(sect) = nkoanpsect(sect) + 1
                  endif
                  if(id(i).eq.-321)then
                     nkaonm=nkaonm+1
                     nkoanmsect(sect) = nkoanmsect(sect) + 1
                  endif
               endif
            elseif(q(i).eq.0.and.(ec(i).gt.0.or.lec(i).gt.0))then
               if(abs(b(i)-1.).lt.0.15)then
                  nphoton=nphoton+1
                  nphotsect(ec_sect(ec(i))) = nphotsect(ec_sect(ec(i))) + 1
                  nph=nph+1
               endif
            endif
         EndDo
         call fill_csql()

         If(YES_PHOTON) then
            ierr = 1
         ElseIf(Npart.gt.0)then
            if(q(1).lt.0)then
               ierr=1
            elseif(q(1).gt.0)then
               if(nph.ge.4.or.npos+nneg.gt.1)ierr=1
            endif
         EndIf
      ELSE
         ierr=0
      ENDIF
      RETURN
      END
c------------------------------------------------------------------------------





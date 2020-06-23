      SUBROUTINE FILL_SEB_NT
c
c_begin_doc
c  RCS ID string
c  $Id: fill_seb_nt.F,v 1.3 2003/01/02 23:27:06 stepanya Exp $
c
c  Documentation for subroutine FILL_SEB_NT
c
c  Purpose: To fill Ntuple #10 bloks - recontsructed particle information.
c  --------
c
c  Input Parameters:  NON
c  ----------------
c
c  Output Parameters:  Error flag
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Fri Mar  7 09:43:25 EST 1997
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
#include "wcs.inc"
#include "ntuple_commons.inc"
#include "clasmdl.inc"
#include "ectimes2.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER ierr
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
c     PARAMETER (CFILE = '$RCSfile: fill_seb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2003/01/02 23:27:06 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_seb_nt.F,v 1.3 2003/01/02 23:27:06 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_SEB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER IHEVT, IEVNT, IDCPB, ISCPB, IECPB, IECPC, ICCPB
      DATA IHEVT,IEVNT,IDCPB,ISCPB,IECPB,IECPC,ICCPB/0,0,0,0,0,0,0/
      integer ind,i,indx,ncol,nd,ix,tc,tb,nrow,ncol1,nrow1,ind1,indx1
      INTEGER mamind, mbank, ibmpr
      real q_t_l, q_t, t_l_t, I_fc, totcpu, ccsg
      INTEGER nelect,nelec(6),sect,ntbttrk,nhbttrk,nelectp,nelectn,nprot,npip, ndeut,nkaon
      data nelec/0,0,0,0,0,0/
      data nelect,ntbttrk,nhbttrk,nelectp,nelectn,nprot,npip,ndeut,nkaon/0,0,0,0,0,0,0,0,0/
      external mamind,mbank
c_end_var
c
c  executable code for routine FILL_PART_NT:
c----6----------------------------------------------------------------72
c
      IF(IHEVT.eq.0)IHEVT=MAMIND(IW,'HEVT')
      IF(IEVNT.eq.0)IEVNT=MAMIND(IW,'EVNT')
      IF(IDCPB.eq.0)IDCPB=MAMIND(IW,'DCPB')
      IF(ISCPB.eq.0)ISCPB=MAMIND(IW,'SCPB')
      IF(IECPB.eq.0)IECPB=MAMIND(IW,'ECPB')
      IF(ICCPB.eq.0)ICCPB=MAMIND(IW,'CCPB')
      IF(IECPC.eq.0)IECPC=MAMIND(IW,'ECPC')
      npart=0
      gpart=0
      dc_part=0
      sc_part=0
      ec_part=0
      cc_part=0
      ecpc_nhit=0
      ind=0
      if(IHEVT.gt.0)ind=IW(IHEVT)
      IF(ind.NE.0) THEN
        evntid=IW(ind+3)
        evntype=IW(ind+4)
        npart=IW(ind+5)/100
        gpart=mod(IW(ind+5),100)
        evntclas=IW(ind+6)/10000
        q_l=RW(ind+7)
        t_l=RW(ind+9)
        tr_time=RW(ind+10)
        rf_time1=RW(ind+11)
        rf_time2=RW(ind+12)
      ENDIF
      IF ( npart.GE.1) THEN
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
c         m(i)=rw(indx+3)
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
c         lec(i)=iw(indx+16)
c         st(i)=iw(indx+17)
c         stat(i)=iw(indx+18)
          enddo
        EndIf
        ind=0
        if(IDCPB.gt.0)ind=IW(IDCPB)
        If(ind.NE.0) Then
          ncol=iw(ind-5)
          nd=iw(ind)
          nrow=iw(ind-4)
          i=0
          do ix=1,nrow
          indx=ind+ncol*(ix-1)
          i=i+1
          dc_sect(i) = iw(indx+1)/100
c         dc_trk(i) = mod(iw(indx+1),100)
          dc_xSC(i) = rw(indx+2)
          dc_ySC(i) = rw(indx+3)
          dc_zSC(i) = rw(indx+4)
          dc_cxSC(i) = rw(indx+5)
          dc_cySC(i) = rw(indx+6)
          dc_czSC(i) = rw(indx+7)
          dc_xEC(i) = rw(indx+8)
          dc_yEC(i) = rw(indx+9)
          dc_zEC(i) = rw(indx+10)
c         dc_thCC(i) = rw(indx+11)
c         dc_c2(i) = rw(indx+12)
          dc_stat(i) = iw(indx+13)
          dc_part=i
          enddo
        EndIf
c 
        ind=0
        if(ICCPB.gt.0)ind=IW(ICCPB)
        ind=IW(ICCPB)
        If(ind.NE.0) Then
          ncol=iw(ind-5)
          nd=iw(ind)
          do ix=1,nd,ncol
             indx=ind+ix-1
             cc_part=cc_part+1
             nphe(cc_part)=rw(indx+2)
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
c         ec_M2(i) = rw(indx+10)
c         ec_M3(i) = rw(indx+11)
c         ec_M4(i) = rw(indx+12)
c         ec_InSt(i)=iw(indx+13)
c         ec_OuSt(i)=iw(indx+14)
c         ec_c2(i) = rw(indx+15)
c         ec_stat(i) = iw(indx+16)
          ec_part=i
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
c         sc_hit(i) = iw(indx+1)-sc_sect(i)*10000-sc_pd(i)*100
          edep(i) = rw(indx+2)
          sc_t(i) = rw(indx+3)
          sc_r(i) = rw(indx+4)
c         sc_c2(i) = rw(indx+5)
          sc_stat(i) = iw(indx+6)
          sc_part=i
          enddo
        EndIf
        ind=0
        IF(IECPC.gt.0)then
           i=0
           ind=IECPC+1
 10        ind=IW(ind-1)
           if(ind.ne.0) then
              ncol=iw(ind-5)
              nd=iw(ind)
              do ix=1,nd,ncol
                 indx=ind+ix-1
                 i=i+1
                 ecpc_hit(i)=iw(ind-2)
                 ecpc_id(i)=iw(indx+1)
                 ecpc_tdc(i)=rw(indx+2)
                 ecpc_adc(i)=rw(indx+3)
                 ecpc_nhit=i
              enddo
              goto 10
           endif         
        endif
      ELSE
      ENDIF
      RETURN
      END
c------------------------------------------------------------------------------





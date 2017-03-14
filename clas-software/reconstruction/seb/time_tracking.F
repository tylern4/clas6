      SUBROUTINE TIME_TRACKING
c
c_begin_doc
c  RCS ID string
c  $Id: time_tracking.F,v 1.40 2008/06/12 15:20:51 devita Exp $
c
c  Documentation for subroutine TIME_TRACKING
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
c  Author:   S. Stepanyan      Created:  Tue Apr  1 22:16:46 EST 1997
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
#include "evnt_general.inc"
#include "echits.inc"
#include "particle_out.inc"
#include "hbt_planes.inc"
#include "dc_inf.inc"
#include "sc_inf.inc"
#include "ec_inf.inc"
#include "cc_inf.inc"
#include "tbtinf.inc"
#include "evnt_par.inc"
#include "piddef.inc"
#include "photon_stuff.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: time_tracking.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.40 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/06/12 15:20:51 $')
      PARAMETER (CAUTHO = '$Author: devita $')
      DATA CRCSID/   
     1'$Id: time_tracking.F,v 1.40 2008/06/12 15:20:51 devita Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TIME_TRACKING')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      DATA  NWRITE/0/
      INTEGER NAMTBT, NAMTBP, NAMTRL1
      DATA NAMTBT,NAMTBP,NAMTRL1/0,0,0/
      REAL pi,rp2,vel,drftel
      DATA pi/3.1415926/
      INTEGER IND, ND, IN, INDX, ncol
      INTEGER dctrk, tbttrk, itr, id, ipl,ierr,ntbt_trkl1
      INTEGER np_dc, np_sc, np_ec, np_cc, np_st, neuttrig
      INTEGER MAMIND
      EXTERNAL MAMIND
c_end_var
c
c  executable code for routine TIME_TRACKING:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='first entry into time_tracking.F'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      ntbt_trk=0
      ntbt_trkl1=0
      tbttrk=0
      IF(NAMTBP.EQ.0)NAMTBP=MAMIND(IW,'TDPL')
      IF(NAMTBT.EQ.0)NAMTBT=MAMIND(IW,'TBTR')
      IF(NAMTRL1.EQ.0)NAMTRL1=MAMIND(IW,'TRL1')
      IND=IW(NAMTBT)
      IF(IND.NE.0) THEN
      ND=IW(IND)
      ncol=IW(ind-5)
      DO in = 1, ND, ncol
        INDX=IND+IN-1
        if(RW(INDX+7).ne.0.)then
        ntbt_trk=ntbt_trk+1
        tbt_id(ntbt_trk) = IW(INDX+10)	      ! HB track number
        tbt_sect(ntbt_trk) = IW(INDX+9)/100
        tbt_c2(ntbt_trk) = RW(INDX+8)
        tbt_p(ntbt_trk) = SQRT(RW(INDX+4)**2+RW(INDX+5)**2+RW(INDX+6)**2)
        tbt_m(ntbt_trk) = 0.0
        tbt_q(ntbt_trk) = 1.
        if(RW(INDX+7).lt.0.)tbt_q(ntbt_trk) = -1.
        tbt_x(1,ntbt_trk) = RW(INDX+1)
        tbt_y(1,ntbt_trk) = RW(INDX+2)
        tbt_z(1,ntbt_trk) = RW(INDX+3)
        IF(tbt_p(ntbt_trk).GT.0.0) THEN
          tbt_cx(1,ntbt_trk) = RW(INDX+4)/tbt_p(ntbt_trk)
          tbt_cy(1,ntbt_trk) = RW(INDX+5)/tbt_p(ntbt_trk)
          tbt_cz(1,ntbt_trk) = RW(INDX+6)/tbt_p(ntbt_trk)
        ENDIF
        ENDIF
      ENDDO
      ENDIF
      IND=IW(NAMTRL1)
      IF(IND.NE.0) THEN
         IF(IW(ind-2).eq.0)THEN
            ND=IW(IND)
            ncol=IW(ind-5)
            DO in = 1, ND, ncol
               INDX=IND+IN-1
               if(IW(INDX+3).GT.0) then
               ntbt_trkl1=ntbt_trkl1+1
               tbt_lst(ntbt_trkl1) = IW(INDX+3) ! TB hits/superlayer on track
               tbt_x(11,ntbt_trkl1) = RW(INDX+4)
               tbt_y(11,ntbt_trkl1) = RW(INDX+5)
               tbt_z(11,ntbt_trkl1) = RW(INDX+6)
               tbt_cx(11,ntbt_trkl1) = RW(INDX+7)
               tbt_cy(11,ntbt_trkl1) = RW(INDX+8)
               tbt_cz(11,ntbt_trkl1) = RW(INDX+9)
               tbt_r(11,ntbt_trkl1) = RW(INDX+10)
               endif
            ENDDO
         ENDIF
      ENDIF
      IF(ntbt_trk.ne.ntbt_trkl1.and.NWRITE.lt.21)then 
         NWRITE=NWRITE+1
         write(6,*)'Wrong number of TB tracks in TBTR and TRL1',ntbt_trk,ntbt_trkl1
      elseif(NWRITE.eq.21)then
         NWRITE=NWRITE+1
         write(6,*)'Last worning !!!'
         write(6,*)'Wrong number of TB tracks in TBTR and TRL1',ntbt_trk,ntbt_trkl1
      endif
c
      IND=NAMTBP+1
 99   IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ND=IW(IND)
c        Sector=IW(IND-2)
        ncol=IW(ind-5)
        DO in = 1, ND, ncol
          INDX=IND+IN-1
          id = iw(indx+1)
          ipl = mod(id,100)
          if(ipl.ge.40)then
          	ipl=ipl-40+4
          endif
          if(ipl.le.10.and.ipl.gt.1.and.RW(INDX+8).lt.999.)then
            tbt_x(ipl,tbttrk) = RW(INDX+2)
            tbt_y(ipl,tbttrk) = RW(INDX+3)
            tbt_z(ipl,tbttrk) = RW(INDX+4)
            tbt_cx(ipl,tbttrk) = RW(INDX+5)
            tbt_cy(ipl,tbttrk) = RW(INDX+6)
            tbt_cz(ipl,tbttrk) = RW(INDX+7)
            tbt_r(ipl,tbttrk) = RW(INDX+8)
          elseif(ipl.eq.1)then
            rp2=RW(INDX+5)**2+RW(INDX+6)**2+RW(INDX+7)**2
            if(rp2.gt.0.00001)tbttrk=tbttrk+1
          endif
        ENDDO
        GO TO 99
      ENDIF
      IF(tbttrk.ne.ntbt_trk.and.NWRITE.lt.21)then
         NWRITE=NWRITE+1 
         write(6,*)'Wrong number of TB tracks in TBTR and TDPL',ntbt_trk,tbttrk
      elseif(NWRITE.eq.21)then
         NWRITE=NWRITE+1
         write(6,*)'Last worning !!!'
         write(6,*)'Wrong number of TB tracks in TBTR and TDPL',ntbt_trk,tbttrk
      endif
      DO dctrk=1,DPARTICLES
        itr=dcitrk(dctrk)
        Do tbttrk=1,ntbt_trk
          If(itr.Eq.tbt_id(tbttrk))Then
         if( dcsector(dctrk).ne.tbt_sect(tbttrk))then
           write(6,*)'Wrong HB/TB IDs ',dctrk,itr,tbttrk,dparticles,ntbt_trk
          endif
            dcp(dctrk)=tbt_p(tbttrk)
            dcq(dctrk)=tbt_q(tbttrk)
            dcm(dctrk)=tbt_m(tbttrk)
            dcchi2(dctrk)=tbt_c2(tbttrk)
            dccx(dctrk)=tbt_cx(1,tbttrk)
            dccy(dctrk)=tbt_cy(1,tbttrk)
            dccz(dctrk)=tbt_cz(1,tbttrk)
            dcvertx(dctrk)=tbt_x(1,tbttrk)
            dcverty(dctrk)=tbt_y(1,tbttrk)
            dcvertz(dctrk)=tbt_z(1,tbttrk)
            dcitrk(dctrk)=tbttrk
            dcstatus(dctrk)=tbttrk
c            if(trackid(tbt_id(tbttrk)).ne.trackid(itr))CALL RECMES(CRNAME,'W','Wrong TBT numbering')
          EndIf
        EndDo
      ENDDO

c Re-calculate track path lenghts to various detectors
      Do id=1,gparticles
         np_dc=partstdc(id)
         np_sc=partstsc(id)
         np_ec=partstec(id)
         np_cc=partstcc(id)
         np_st = partstst(id)
         if(np_dc.gt.0) then
            If(dcstatus(np_dc).gt.0)Then
               if(np_sc.gt.0)then
                  scr_trk(np_sc)=tbt_r(sctrk_pln(np_sc),dcstatus(np_dc))
               endif
               if(np_ec.gt.0)then
                  ecr_trk(np_ec)=tbt_r(fec_pln,dcstatus(np_dc))
               endif
               if(np_cc.gt.0)then
                  ccr_trk(np_cc)=tbt_r(cc_pln,dcstatus(np_dc))
               endif
            Endif
         endIf
      EndDo
c Get the trigger time
      IF(YES_PHOTON) then
         CALL GTRIG_TIME(neuttrig,1) ! --> for photon run(use ST)
      Else
         CALL TRIG_TIME(1)      ! --> for electron runs
      ENDIF

      tracking_hb=.false.
      if(ntbt_trk.gt.0)tracking_tb=.true.
      RETURN
      END
c
c------------------------------------------------------------------------------





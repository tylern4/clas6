      SUBROUTINE READ_RBANKS(time_do)
c
c_begin_doc
c  RCS ID string
c  $Id: read_rbanks.F,v 1.66 2005/10/06 14:42:08 clasdvcs Exp $
c
c  Documentation for subroutine READ_RBANKS
c
c  Purpose: Reads Pass 0 reconstruction banks.
c  --------
c
c  Input Parameters:  Non
c  ----------------
c  time_do is .false. it will become true if electron run and there is
c  a negative track in the event
c
c  Output Parameters:  Non
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S.Stepanyan      Created:  Fri Apr  4 21:58:30 EST 1997
c  -------
c
c  Major revisions:  Franz Klein (Nov.1997): Changed TRKL to HBTR & HDPL banks
c  ----------------
c     	- add call to start counter reconstruction bank(STR) :burin 4/7/98
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
#include "evnt_par.inc"
#include "evnt_general.inc"
#include "piddef.inc"
#include "hbt_planes.inc"
#include "tbtinf.inc"
#include "schits.inc"
#include "sc1hits.inc"
#include "cchits.inc"
#include "echits.inc"
#include "lechits.inc"
#include "sthits.inc"
#include "tag_hits.inc"
#include "photon_stuff.inc"
#include "clas_offsets.inc"
#include "run_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      LOGICAL time_do
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: read_rbanks.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.66 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2005/10/06 14:42:08 $')
      PARAMETER (CAUTHO = '$Author: clasdvcs $')
      DATA CRCSID/   
     1'$Id: read_rbanks.F,v 1.66 2005/10/06 14:42:08 clasdvcs Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='READ_RBANKS')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,NT
      INTEGER NAMIDC,NAMID2,NAMICC,NAMISC,NAMIEC,NAMILAC,NAMICA,NAMIBM,NAMISC1
      INTEGER NAMIST,NAMITAG,NAMICL0,NAMIRF
      DATA NAMIDC,NAMID2,NAMICC,NAMISC,NAMIEC,NAMILAC,NAMICA,NAMIBM,NAMISC1,
     &     NAMIST,NAMITAG,NAMICL0,NAMIRF/0,0,0,0,0,0,0,0,0,0,0,0,0/
      INTEGER nst,il
      DATA nst/0/
      INTEGER ind,nd,in,indx,id,ind2,ind2_off,ncol,ncol_hbtr,ccrc_col,scr_col
      INTEGER sc1_col,echb_col,str_col,tagr_col
      INTEGER jl,js,hit,sector,layer,ipl,ec_hits,lec_hits,sc_hits,scp_st,EC1R_COL
      integer call_ind,cl0_ind,rft_ind
      INTEGER MAMIND,MDROP,MLINK
      EXTERNAL MAMIND,MDROP,MLINK
      INTEGER ST_LEVEL
c_end_var
c
c  executable code for routine READ_RBANKS:
c----6----------------------------------------------------------------72
c

      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Reading Pass 0 reconstruction BOS banks'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      ntr_link = 0
      do id =1,Maxlink
        trkp(7,1,id) = 0.0
        trkp(10,4,id) = 1000.0
      enddo
      IF(NAMIDC.EQ.0)NAMIDC=MAMIND(IW,'HDPL')
      IF(NAMID2.EQ.0)NAMID2=MAMIND(IW,'HBTR')
c IF flag is set dtart from existing TB banks
      ST_LEVEL = 0
      if(start_tb)then
         NAMIDC=MAMIND(IW,'TDPL')
         NAMID2=MAMIND(IW,'TBTR')
         ST_LEVEL = 1
      endif
      

c- Read HBTR -
      IND2=IW(NAMID2)
      IF(ind2.GT.0)then
         IF(IW(ind2-4).le.Maxlink)then
            ncol_hbtr=IW(ind2-5)
            nd=IW(ind2)
            tracking_hb=.true.
            tracking_tb=.false.
            DO indx=1,nd,ncol_hbtr
               ntr_link=ntr_link+1 ! Number of ltracks
               ind2_off = ind2 + (ntr_link-1)*ncol_hbtr !get track# in HBTR
               id =   iw(ind2_off+9)             
               itr_sect(ntr_link)= id/100 ! Sector number
               trackid(ntr_link) =mod(id,100) ! Track number in the sector
               trkp(12,1,ntr_link) = rw(ind2_off+8) ! Chi2 for HBT track fitting
               trkp(2,1,ntr_link) = rw(ind2_off+2)
               trkp(3,1,ntr_link) = rw(ind2_off+3)
               trkp(1,1,ntr_link) = rw(ind2_off+1)
               trkp(7,1,ntr_link) = SQRT(rw(ind2_off+4)**2+rw(ind2_off+5)**2+rw(ind2_off+6)**2) ! HBT momentum
               trkp(5,1,ntr_link) = rw(ind2_off+5)/trkp(7,1,ntr_link)
               trkp(6,1,ntr_link) = rw(ind2_off+6)/trkp(7,1,ntr_link)
               trkp(4,1,ntr_link) = rw(ind2_off+4)/trkp(7,1,ntr_link)
               trkp(8,1,ntr_link) = 0.0 ! Mass (non defined)
               trkp(9,1,ntr_link) = rw(ind2_off+7) ! Charge
               if(trkp(9,1,ntr_link)*qtrigg.gt.0.or.
     +              trkp(9,1,ntr_link)*qtrigg_s.gt.0)time_do=.true.
            ENDDO
c     - Read HDPL per sector - 
            IND=NAMIDC+1
 11         IND=IW(IND-1)
            IF(IND.NE.0) THEN
               Sector=IW(IND-2)
               Ncol =IW(IND-5)
               ND = IW(IND)
               DO in = 1, ND, Ncol
                  indx = ind+in-1         
                  id = iw(indx+1)
                  ipl = mod(id,100)
                  nt = iw(indx+1)/100
                  if(nt.lt.0.or.nt.gt.ntr_link.or.Sector.ne.itr_sect(nt)) then
                     write(CRMESS,'(2(A,I3),2i3)') 'HDPL: Sector#',sector,
     +                    ' Track# not defined',nt,ntr_link,itr_sect(nt) 
                     call RecMes(CRNAME,'W',CRMESS)
                  else
c-Coordinates and direction cosines at vertex, (2) Start Counter planes,
c(1) Cerenkov Counter plane, (1) Scint.Counter plane, (2) El.magn.Cal. planes
                     id = 0
                     if(ipl.LT.4.and.ipl.gt.1) then 
                        id = ipl
                     elseif(ipl.GE.40) then
                        id = ipl -40 +4
                     endif
                     if(id.GT.1.and.id.lt.11) then
                        trkp(2,id,nt) = rw(indx+2)
                        trkp(3,id,nt) = rw(indx+3)
                        trkp(1,id,nt) = rw(indx+4)
                        trkp(5,id,nt) = rw(indx+5)
                        trkp(6,id,nt) = rw(indx+6)
                        trkp(4,id,nt) = rw(indx+7)
                        trkp(10,id,nt) = rw(indx+8) ! Track length from target to plane
                     endif
                  endif
               ENDDO
               GO TO 11
            endif
         else 
            write(CRMESS,*) 'HBTR has more than ', Maxlink, 
     +           ' tracks,  ignored'
            call RecMes(CRNAME,'W',CRMESS)
         endif
      endif
C- Check if there is a negative track in the electron run
      if(.not.time_do.and..not.yes_photon.and.qtrigg.ne.0)return
C-
      IF(NAMIEC.EQ.0)NAMIEC=MAMIND(IW,'ECHB')
      IND=NAMIEC+1
      DO js=1,maxsector
        Do jl=1,maxlayers
          ecfit_nhit(jl,js)=0
        EndDo
      ENDDO
      ec_hits=0
  14  IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ND = IW(IND)
        ECHB_COL=IW(IND-5)
        DO in = 1, ND, echb_col
          indx=ind+in-1
          sector=int(iw(indx+1)/100)
          layer=MOD(iw(indx+1),100)-8
          ecfit_nhit(layer,sector)=int(iw(indx+38)/100)
          if(layer.eq.1)ec_hits=ec_hits+1
          hit=MOD(iw(indx+38),100)
          if(hit.le.maxechits.and.layer.gt.0.and.layer.lt.4.and.sector.gt.0.and.sector.lt.7)then
          ecfit_hit(energy,hit,layer,sector)=rw(indx+2)
          ecfit_hit(time,hit,layer,sector)=rw(indx+4)
          ecfit_hit(ec_i,hit,layer,sector)=rw(indx+6)
          ecfit_hit(ec_j,hit,layer,sector)=rw(indx+7)
          ecfit_hit(ec_di,hit,layer,sector)=rw(indx+8)
          ecfit_hit(ec_dj,hit,layer,sector)=rw(indx+9)
          ecfit_hit(ec_x,hit,layer,sector)=rw(indx+10)
          ecfit_hit(ec_y,hit,layer,sector)=rw(indx+11)
          ecfit_hit(ec_z,hit,layer,sector)=rw(indx+12)
          ecfit_hit(ec_dx,hit,layer,sector)=rw(indx+13)
          ecfit_hit(ec_dy,hit,layer,sector)=rw(indx+14)
          ecfit_hit(ec_dz,hit,layer,sector)=rw(indx+15)
          Ec_peak(mom2,Hit,Ec_U,Layer,sector)=rw(indx+16)
          Ec_peak(mom2,Hit,Ec_V,Layer,sector)=rw(indx+17)
          Ec_peak(mom2,Hit,Ec_W,Layer,sector)=rw(indx+18)
          Ec_peak(mom3,Hit,Ec_U,Layer,sector)=rw(indx+19)
          Ec_peak(mom3,Hit,Ec_V,Layer,sector)=rw(indx+20)
          Ec_peak(mom3,Hit,Ec_W,Layer,sector)=rw(indx+21)
          Ec_peak(mom4,Hit,Ec_U,Layer,sector)=rw(indx+22)
          Ec_peak(mom4,Hit,Ec_V,Layer,sector)=rw(indx+23)
          Ec_peak(mom4,Hit,Ec_W,Layer,sector)=rw(indx+24)
          Ec_peak(dist,Hit,Ec_U,Layer,sector)=rw(indx+25)
          Ec_peak(dist,Hit,Ec_V,Layer,sector)=rw(indx+26)
          Ec_peak(dist,Hit,Ec_W,Layer,sector)=rw(indx+27)
          Ec_peak(path,Hit,Ec_U,Layer,sector)=rw(indx+28)
          Ec_peak(path,Hit,Ec_V,Layer,sector)=rw(indx+29)
          Ec_peak(path,Hit,Ec_W,Layer,sector)=rw(indx+30)
          ecfit_nhitU(hit,layer,sector)=iw(indx+31)
          ecfit_nhitV(hit,layer,sector)=iw(indx+32)
          ecfit_nhitW(hit,layer,sector)=iw(indx+33)
          ec_matchid1(hit,layer,sector)=iw(indx+34)
          ec_c2match1(hit,layer,sector)=rw(indx+35)
          ec_matchid2(hit,layer,sector)=iw(indx+36)
          ec_c2match2(hit,layer,sector)=rw(indx+37)
          endif
        ENDDO
        GO TO 14
      ENDIF
c- Larg angle calorimeter.
      IF(NAMILAC.EQ.0)NAMILAC=MAMIND(IW,'EC1R')
      IND=NAMILAC+1
      DO js=1,maxsector
        Do jl=1,maxlayers
          lec_nhit(jl,js)=0
        EndDo
      ENDDO
      lec_hits=0
  16  IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ND = IW(IND)
        EC1R_COL=IW(IND-5)
        DO in = 1, ND, ec1r_col
          indx=ind+in-1
          if(rw(ind+1).gt.0.00001.and.rw(ind+1).lt.10.)then
             sector=iw(indx+45)
             if(lec_hits.lt.maxechits.and.sector.gt.0.and.sector.lt.7)then
                lec_hits=lec_hits+1
                lec_nhit(1,sector)=lec_hits
                lec_hit(energy,lec_hits,1,sector)=rw(indx+1)
                lec_hit(time,lec_hits,1,sector)=rw(indx+3)
                lec_hit(ec_x,lec_hits,1,sector)=rw(indx+5)
                lec_hit(ec_y,lec_hits,1,sector)=rw(indx+6)
                lec_hit(ec_z,lec_hits,1,sector)=rw(indx+7)
                lec_hit(ec_dx,lec_hits,1,sector)=rw(indx+8)
                lec_hit(ec_dy,lec_hits,1,sector)=rw(indx+9)
                lec_hit(ec_dz,lec_hits,1,sector)=rw(indx+10)
                lec_hit(ec_i2,lec_hits,1,sector)=rw(indx+11)
             endif
          endif
       ENDDO
       GO TO 16
      ENDIF
      if(qtrigg.eq.0.and.ec_hits.eq.0.and.lec_hits.eq.0)return
      time_do=.true.
c- CC banks
      IF(NAMICC.EQ.0)NAMICC=MAMIND(IW,'CCRC')
      IND=NAMICC+1
      nrclust=0
  12  IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ccrc_col=IW(IND-5)
        ND = IW(IND)
        DO in = 1, ND, ccrc_col
          indx=ind+in-1
          if(nrclust.lt.maxcchits)then
          nrclust=nrclust+1
          nrsect(nrclust) = iw(indx+1)
          nrsegm(nrclust) = iw(indx+2)
          nrphe(nrclust) = iw(indx+5)
          nrtime(nrclust) = iw(indx+6)
          nrthet(nrclust) = iw(indx+7)
          nrdthet(nrclust) = iw(indx+8)
          nrphi(nrclust) = iw(indx+9)
          endif
        ENDDO
      GO TO 12
      ENDIF
c- SC banks
      IF(NAMISC.EQ.0)NAMISC=MAMIND(IW,'SCR ')
      IND=NAMISC+1
      DO js=1,maxsector
        scfit_nhits(js)=0
      ENDDO
      sc_hits=0
  15  IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ND = IW(IND)
        SCR_COL=IW(IND-5)
        sector=IW(ind-2)
        if (sector .gt. 6 .or. sector .lt. 1) then
           call recmes(crname,'w','Bad Sector ID')
           goto 15
        endif
        DO in = 1,ND,SCR_COL
          indx = ind + in -1
          if(sc_hits.lt.Maxschits)then
          sc_hits=sc_hits+1
          scfit_nhits(sector)=scfit_nhits(sector)+1
          scfit_hits(scfit_nhits(sector),sector)=iw(indx+1)
          scfit_paddle(sc_time,scfit_nhits(sector),sector)=rw(indx+3)
          scfit_paddle(sc_gev,scfit_nhits(sector),sector)=rw(indx+2)
          scfit_paddle(sc_x,scfit_nhits(sector),sector)=rw(indx+4)
          scfit_paddle(sc_y,scfit_nhits(sector),sector)=rw(indx+5)
          scfit_paddle(sc_z,scfit_nhits(sector),sector)=rw(indx+6)
          scfit_paddle(sc_dix,scfit_nhits(sector),sector)=rw(indx+7)
          scfit_paddle(sc_diy,scfit_nhits(sector),sector)=rw(indx+8)
          scfit_paddle(sc_diz,scfit_nhits(sector),sector)=rw(indx+9)
          endif
        ENDDO
        GO TO 15
      ENDIF
C
      IF(NAMISC1.EQ.0)NAMISC1=MAMIND(IW,'SC1 ')
      IND=NAMISC1+1
      DO js=1,maxsector
        sc1_hits(js)=0
      ENDDO
  13  IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ND = IW(IND)
        SC1_COL=IW(IND-5)
        sector=IW(ind-2)
        if (sector .gt. 6 .or. sector .lt. 1) then
           call recmes(crname,'w','Bad Sector ID')
           goto 13
        endif
        DO in = 1,ND,SC1_COL
          indx = ind + in -1
          if(sc1_hits(sector).lt.Maxschits)then
          scp_st=0
          sc1_hits(sector)=sc1_hits(sector)+1
          sc1_pd(sc1_hits(sector),sector)=iw(indx+1)
          sc1_l(sc_time,sc1_hits(sector),sector)=rw(indx+2)
          sc1_l(sc_gev,sc1_hits(sector),sector)=rw(indx+3)
          sc1_r(sc_time,sc1_hits(sector),sector)=rw(indx+4)
          sc1_r(sc_gev,sc1_hits(sector),sector)=rw(indx+5)
          if(rw(indx+2).gt.0)scp_st=scp_st+10
          if(rw(indx+4).gt.0)scp_st=scp_st+20
          if(rw(indx+3).gt.0)scp_st=scp_st+1
          if(rw(indx+5).gt.0)scp_st=scp_st+2
          scpd_stat(sc1_pd(sc1_hits(sector),sector),sector)=scp_st
          endif
        ENDDO
        GO TO 13
      ENDIF
C
      DO js=1,3
        rf_time(js)=0.
      ENDDO
      if(rf_status.eq.1.or.rf_status.eq.2)then
         IF(NAMICA.EQ.0)NAMICA=MAMIND(IW,'CALL')
         IND = IW(NAMICA)
         IF(IND.NE.0) THEN
            do in = 1, iwrow(ind), 1
               call_ind = 2*ind + (in-1)*iwcol(ind)
               if(iw16(call_ind+CALL_ID).eq.1) rf_time(3)= iw16(call_ind+CALL_ADC)
               if(iw16(call_ind+CALL_ID).eq.6) rf_time(1)= iw16(call_ind+CALL_TDC)*rf1slop
               if(iw16(call_ind+CALL_ID).eq.7) rf_time(2)=iw16(call_ind+CALL_TDC)*rf2slop
            enddo
         ENDIF
      elseif(rf_status.eq.3.or.rf_status.eq.4)then
         IF(NAMIRF.EQ.0)NAMIRF=MAMIND(IW,'RFT')
         IND = IW(NAMIRF)
         IF(IND.NE.0) THEN
         ENDIF
      endif
C
      IF(NAMICL0.EQ.0)NAMICL0=MAMIND(IW,'CL01')
      IND = IW(NAMICL0)
      IF(IND.NE.0) THEN
         do in = 1, iwrow(ind), 1
           cl0_ind = ind+(in-1)*iwcol(ind)
           rftest(1)= rw(cl0_ind+CL01_RF1)
           rftest(2)= rw(cl0_ind+CL01_RF2)
           rftest(3)= rw(cl0_ind+CL01_RF)
        enddo
      ENDIF
C
      IF(NAMIBM.EQ.0)NAMIBM=MAMIND(IW,'BMPR')
      IND = JW(NAMIBM)
      IF(IND.NE.0) THEN
        Q_t=rjw(ind + BMPR_Q_TOT)*1.E+06
        Q_t_l=rjw(ind+ BMPR_Q_TOT_LV)*1.E+06
        T_l_T=rjw(ind + BMPR_TL_TU)
        I_FC=rjw(ind+BMPR_CURRENT)
      ENDIF
C-
      if (YES_PHOTON) then
C-
C     IF(NAMIST.EQ.0) NAMIST=MAMIND(IW,'STR ')
C      IND=NAMIST+1
      IND = MLINK(IW,'STR ',ST_LEVEL)
      DO js=1,maxsector
        stfit_nhits(js)=0
      ENDDO
      sthits=0
      IF(IND.NE.0) THEN
        ND = IW(IND)
        str_col = IW(IND-5)
        DO in = 1,ND,str_col
          indx = ind + in -1
          sector=iw(indx+1)
          if(sector.gt.100) then ! new Start Counter
             sttype = 1
             sector=int(sector/100)
          else
             sttype = 0
          endif
          sthits=sthits+1
          stfit_nhits(sector)=stfit_nhits(sector)+1
          stfit_pd(stfit_nhits(sector),sector)=iw(indx+1)
          stfit_trkno(stfit_nhits(sector),sector)=iw(indx+2)
          stfit_time(stfit_nhits(sector),sector)=rw(indx+3)
          stfit_length(stfit_nhits(sector),sector)=rw(indx+4)
          stfit_status(stfit_nhits(sector),sector)=iw(indx+6)
        ENDDO
      ENDIF 
c      
      taghits = 0
      IF(NAMITAG.EQ.0) NAMITAG=MAMIND(IW,'TAGR')
      IND=NAMITAG+1
  25  IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ND = IW(IND)
        tagr_col = IW(IND-5)
        if(ND/tagr_col.gt.50) ND = 50*tagr_col ! maximum 50 hits
        DO in = 1,ND,tagr_col
          indx = ind + in -1
          taghits=taghits+1
          Egamma(taghits)=rw(indx+1)
          Tgamma_noRF(taghits)=rw(indx+2) 	! without RF correction
          Tgamma(taghits)=rw(indx+3)		! with RF correction
          tagstat(taghits)=iw(indx+4)
          Tid(taghits)=iw(indx+5)
          Eid(taghits)=iw(indx+6)
        ENDDO
C-        GO TO 25
      ENDIF 
c
        do in=1,ntr_link
          zphoton(in) = trkp(3,1,in)
c Vertex position (plane 1) along beam axis
        enddo
        time_do=.true.
      endif

      RETURN
      END
c
c------------------------------------------------------------------------------





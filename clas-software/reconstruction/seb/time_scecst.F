      SUBROUTINE time_scecst(time_do)
c     
c     _begin_doc
c     
c     Purpose: Reads Sc, St, Ec ... information after TBT.
c     --------
c     
c     Input Parameters: 
c     ----------------
c     time_do is .false. it will become true if electron run and there is
c     a negative track in the event
c     
c     Output Parameters:  Non
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   S.Stepanyan      Created:  Fri Apr  4 21:58:30 EST 1997
c     -------
c     
c     Major revisions:  Franz Klein (Nov.1997): Changed TRKL to HBTR & HDPL banks
c     ----------------
c     - add call to start counter reconstruction bank(STR) :burin 4/7/98
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
#include "evnt_general.inc"
#include "schits.inc"
#include "cchits.inc"
#include "echits.inc"
#include "lechits.inc"
#include "sthits.inc"
#include "clas_offsets.inc"
#include "particle_out.inc"
#include "photon_stuff.inc"
#include "sc_inf.inc"
#include "cc_inf.inc"
#include "ec_inf.inc"
#include "lec_inf.inc"
#include "st_inf.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
      LOGICAL time_do
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: time_scecst.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2005/09/06 18:23:08 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1     '$Id: time_scecst.F,v 1.7 2005/09/06 18:23:08 stepanya Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='time_scecst')
      PARAMETER (CRAUTH=' ')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE,NT
      INTEGER NAMICC,NAMISC,NAMIEC,NAMILAC,NAMIST
      DATA NAMICC,NAMISC,NAMIEC,NAMILAC,NAMIST/0,0,0,0,0/
      INTEGER nst,il
      DATA nst/0/
      INTEGER ind,nd,in,indx,id,ncol,ccrc_col,scr_col
      INTEGER sc1_col,echb_col,str_col,tagr_col
      INTEGER jl,js,hit,sector,layer,ec_hits,lec_hits,sc_hits,scp_st,EC1R_COL
      INTEGER i,tbtr_ind,hbtr_ind,indt,indh,ntr_link,trk_hbt
      INTEGER htr_id(MaxParticles),btr_id(MaxParticles),trk_id,ncol_tbtr,ncol_hbtr
      INTEGER MAMIND,MDROP,mlink
      EXTERNAL MAMIND,MDROP,mlink
c     _end_var
c     
c     executable code for routine READ_RBANKS:
c---- 6----------------------------------------------------------------72
c     

      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='Reading Pass 0 reconstruction BOS banks'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
C     -
c     IF(NAMIEC.EQ.0)NAMIEC=MAMIND(IW,'ECHB')
c     IND=NAMIEC+1
c     DO js=1,maxsector
c     Do jl=1,maxlayers
c     ecfit_nhit(jl,js)=0
c     EndDo
c     ENDDO
c     ec_hits=0
c     14   IND=IW(IND-1)
c     IF(IND.NE.0) THEN
c     ND = IW(IND)
c     ECHB_COL=IW(IND-5)
c     DO in = 1, ND, echb_col
c     indx=ind+in-1
c     sector=int(iw(indx+1)/100)
c     layer=MOD(iw(indx+1),100)-8
c     ecfit_nhit(layer,sector)=int(iw(indx+38)/100)
c     if(layer.eq.1)ec_hits=ec_hits+1
c     hit=MOD(iw(indx+38),100)
c     if(hit.le.maxechits.and.layer.gt.0.and.layer.lt.4.and.sector.gt.0.and.sector.lt.7)then
c     ecfit_hit(energy,hit,layer,sector)=rw(indx+2)
c     ecfit_hit(time,hit,layer,sector)=rw(indx+4)
c     ecfit_hit(ec_i,hit,layer,sector)=rw(indx+6)
c     ecfit_hit(ec_j,hit,layer,sector)=rw(indx+7)
c     ecfit_hit(ec_di,hit,layer,sector)=rw(indx+8)
c     ecfit_hit(ec_dj,hit,layer,sector)=rw(indx+9)
c     ecfit_hit(ec_x,hit,layer,sector)=rw(indx+10)
c     ecfit_hit(ec_y,hit,layer,sector)=rw(indx+11)
c     ecfit_hit(ec_z,hit,layer,sector)=rw(indx+12)
c     ecfit_hit(ec_dx,hit,layer,sector)=rw(indx+13)
c     ecfit_hit(ec_dy,hit,layer,sector)=rw(indx+14)
c     ecfit_hit(ec_dz,hit,layer,sector)=rw(indx+15)
c     Ec_peak(mom2,Hit,Ec_U,Layer,sector)=rw(indx+16)
c     Ec_peak(mom2,Hit,Ec_V,Layer,sector)=rw(indx+17)
c     Ec_peak(mom2,Hit,Ec_W,Layer,sector)=rw(indx+18)
c     Ec_peak(mom3,Hit,Ec_U,Layer,sector)=rw(indx+19)
c     Ec_peak(mom3,Hit,Ec_V,Layer,sector)=rw(indx+20)
c     Ec_peak(mom3,Hit,Ec_W,Layer,sector)=rw(indx+21)
c     Ec_peak(mom4,Hit,Ec_U,Layer,sector)=rw(indx+22)
c     Ec_peak(mom4,Hit,Ec_V,Layer,sector)=rw(indx+23)
c     Ec_peak(mom4,Hit,Ec_W,Layer,sector)=rw(indx+24)
c     Ec_peak(dist,Hit,Ec_U,Layer,sector)=rw(indx+25)
c     Ec_peak(dist,Hit,Ec_V,Layer,sector)=rw(indx+26)
c     Ec_peak(dist,Hit,Ec_W,Layer,sector)=rw(indx+27)
c     Ec_peak(path,Hit,Ec_U,Layer,sector)=rw(indx+28)
c     Ec_peak(path,Hit,Ec_V,Layer,sector)=rw(indx+29)
c     Ec_peak(path,Hit,Ec_W,Layer,sector)=rw(indx+30)
c     ecfit_nhitU(hit,layer,sector)=iw(indx+31)
c     ecfit_nhitV(hit,layer,sector)=iw(indx+32)
c     ecfit_nhitW(hit,layer,sector)=iw(indx+33)
c     ec_matchid1(hit,layer,sector)=iw(indx+34)
c     ec_c2match1(hit,layer,sector)=rw(indx+35)
c     ec_matchid2(hit,layer,sector)=iw(indx+36)
c     ec_c2match2(hit,layer,sector)=rw(indx+37)
c     endif
c     ENDDO
c     GO TO 14
c     ENDIF
c     - Larg angle calorimeter.
c     IF(NAMILAC.EQ.0)NAMILAC=MAMIND(IW,'EC1R')
c     IND=NAMILAC+1
c     DO js=1,maxsector
c     Do jl=1,maxlayers
c     lec_nhit(jl,js)=0
c     EndDo
c     ENDDO
c     lec_hits=0
c     16   IND=IW(IND-1)
c     IF(IND.NE.0) THEN
c     ND = IW(IND)
c     EC1R_COL=IW(IND-5)
c     DO in = 1, ND, ec1r_col
c     indx=ind+in-1
c     if(rw(ind+1).gt.0.00001.and.rw(ind+1).lt.10.)then
c     sector=iw(indx+45)
c     if(lec_hits.lt.maxechits.and.sector.gt.0.and.sector.lt.7)then
c     lec_hits=lec_hits+1
c     lec_nhit(1,sector)=lec_hits
c     lec_hit(energy,lec_hits,1,sector)=rw(indx+1)
c     lec_hit(time,lec_hits,1,sector)=rw(indx+3)
c     lec_hit(ec_x,lec_hits,1,sector)=rw(indx+5)
c     lec_hit(ec_y,lec_hits,1,sector)=rw(indx+6)
c     lec_hit(ec_z,lec_hits,1,sector)=rw(indx+7)
c     lec_hit(ec_dx,lec_hits,1,sector)=rw(indx+8)
c     lec_hit(ec_dy,lec_hits,1,sector)=rw(indx+9)
c     lec_hit(ec_dz,lec_hits,1,sector)=rw(indx+10)
c     lec_hit(ec_i2,lec_hits,1,sector)=rw(indx+11)
c     endif
c     endif
c     ENDDO
c     GO TO 16
c     ENDIF
c     
c     - CC banks
c     IF(NAMICC.EQ.0)NAMICC=MAMIND(IW,'CCRC')
c     IND=NAMICC+1
c     nrclust=0
c     12   IND=IW(IND-1)
c     IF(IND.NE.0) THEN
c     ccrc_col=IW(IND-5)
c     ND = IW(IND)
c     DO in = 1, ND, ccrc_col
c     indx=ind+in-1
c     if(nrclust.lt.maxcchits)then
c     nrclust=nrclust+1
c     nrsect(nrclust) = iw(indx+1)
c     nrsegm(nrclust) = iw(indx+2)
c     nrphe(nrclust) = iw(indx+5)
c     nrtime(nrclust) = iw(indx+6)
c     nrthet(nrclust) = iw(indx+7)
c     nrdthet(nrclust) = iw(indx+8)
c     nrphi(nrclust) = iw(indx+9)
c     endif
c     ENDDO
c     GO TO 12
c     ENDIF
c     - SC banks
      IF(NAMISC.EQ.0)NAMISC=MAMIND(IW,'SCR ')
      IND=NAMISC+1
      DO js=1,maxsector
         scfit_nhits(js)=0
      ENDDO
      sc_hits=0
 15   IND=IW(IND-1)
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
c     
      Do in=1,gparticles
         if(partstsc(in).gt.0)then
            sector=scsector(partstsc(in))
            do js=1,scfit_nhits(sector)
               if(scfit_hits(js,sector).eq.scpdid(partstsc(in)))then
                  sctime(partstsc(in))=scfit_paddle(sc_time,js,sector)
                  sce_dep(partstsc(in))=scfit_paddle(sc_gev,js,sector)
               endif
            enddo
         endif
      EndDo
C     
      IF (YES_PHOTON) THEN
C     -
c++ reads TBTR bank to redo matching with STR at time base level
       
         tbtr_ind=MAMIND(IW,'TBTR')
         hbtr_ind=MAMIND(IW,'HBTR')
         indt   =IW(tbtr_ind)
         indh   =IW(hbtr_ind)
         ntr_link=0
         if(indt.ne.0.AND.indh.ne.0) then
            nd=IW(indt)
            ncol_tbtr=IW(indt-5)
            ncol_hbtr=IW(indh-5)
            if(nd.gt.20*ncol_tbtr) nd=20*ncol_tbtr
            DO i=1,nd,ncol_tbtr
               ntr_link=ntr_link+1 ! Number of ltracks
               btr_id(ntr_link) = iw(indt+ 9+(ntr_link -1)*ncol_tbtr)             
               htr_id(ntr_link) = iw(indt+10+(ntr_link -1)*ncol_tbtr)
               htr_id(ntr_link) = iw(indh+ 9+(htr_id(ntr_link)-1)*ncol_hbtr)  
            EndDO
         endif


         IND=mlink(iw,'STR ',1)
         DO js=1,maxsector
            stfit_nhits(js)=0
         ENDDO
         IF(IND.NE.0) THEN
            ND = IW(IND)
            str_col = IW(IND-5)
            DO in = 1,ND,str_col
               indx = ind + in -1
               sector=iw(indx+1)
               if(sttype.eq.1) then ! new Start Counter
                  sector=int(sector/100)
               endif
               stfit_nhits(sector)=stfit_nhits(sector)+1
               stfit_pd(stfit_nhits(sector),sector)=iw(indx+1)
               stfit_trkno(stfit_nhits(sector),sector)=iw(indx+2)
               stfit_time(stfit_nhits(sector),sector)=rw(indx+3)
               stfit_length(stfit_nhits(sector),sector)=rw(indx+4)
               stfit_status(stfit_nhits(sector),sector)=iw(indx+6)
            ENDDO
c     

           if(sttype.eq.1) then                 ! New Start Counter
*** in the case of the New Start Counter redo matching after TBT
               DO in=1,STPARTICLES
                  sector = stsector(in)
                  trk_id=sector*100+sttrkno(in)
                  trk_hbt=0
                  do i=1,ntr_link             ! find id of HBT corresponding to this time base track
                     if(trk_id.eq.htr_id(i)) trk_hbt=mod(btr_id(i),100)
                  enddo
                  sttrkno(in) = - sttrkno(in) ! set trk no to negative value to distinguish HBT from TBT
                  do js=1,stfit_nhits(sector)
                     if(trk_hbt.eq.stfit_trkno(js,sector)) then ! this hit was associated with a TBT track
                        stihit(in)   = js
                        stsector(in) = sector
                        str_trk(in)  = stfit_length(js,sector)
                        sttrkno(in)  = stfit_trkno(js,sector)
                        sttime(in)   = StFit_time(js,sector) 
                        ststatus(in) = stfit_status(js,sector)
                        stid(in)     = stfit_pd(js,sector)
                     endif
                  enddo
               ENDDO
            else            ! old Start Counter
               Do in=1,gparticles
                  if(partstst(in).gt.0)then
                     sector=stsector(partstst(in))
                     do js=1,stfit_nhits(sector)
                        if(js.eq.stihit(partstsc(in)))then
                           sttime(partstst(in))=stfit_time(js,sector)
                        endif
                     enddo
                  endif
               EndDo               
            endif

         ENDIF
      ENDIF 
c     
      RETURN
      END
c
c------------------------------------------------------------------------------





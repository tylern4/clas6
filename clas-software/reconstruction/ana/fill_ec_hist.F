      SUBROUTINE FILL_EC_HIST
c
c_begin_doc
c  RCS ID string
c  $Id: fill_ec_hist.F,v 1.4 1999/06/09 19:35:32 stepanya Exp $
c
c  Documentation for subroutine FILL_EC_HIST
c
c  Purpose: Fills forward EC histograms for off-line monitiring.
c  --------
c
c  Input Parameters:  NON
c  ----------------
c
c  Output Parameters: NON
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Sun Jun  8 11:47:22 EDT 1997
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
c#include "bcs.inc"
#include "Ec_general.PAR"
#include "EcFit_general.CMN"
#include "ec_strip.inc"
#include "ec_peak.inc"
#include "ec_control.inc"
#include "EcStrip.CMN"
#include "EcEvu.CMN"
#include "EcCal.CMN"
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
      PARAMETER (CFILE = '$RCSfile: fill_ec_hist.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/06/09 19:35:32 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_ec_hist.F,v 1.4 1999/06/09 19:35:32 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_EC_HIST')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER s,irow,nst,ist,hist1id,hist2id,hist3id,hist4id,iin,iou,sti,sto
      REAL adcr,tdcr,etot_w,etot_i,etot_o,ediff,eiosum
      INTEGER pixeli,pixelo
c_end_var
c
c  executable code for routine FILL_EC_HIST:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Fills forward EC histograms.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      do s=1,6
      hist1id=100+s*1000
      hist2id=200+s*1000
      hist3id=300+s*1000
      hist4id=400+s*1000
      etot_i=0.
      etot_o=0.
      etot_w=0.
      pixeli=1
      pixelo=1
        if(ecfit_nhit(inner,s).gt.0)then
        DO irow = 1,ecfit_nhit(inner,s)
          pixeli=0
          if(ntstripc(irow,inner,s).eq.3)then
             pixeli=1
          endif
          etot_i=etot_i+ecfit_hit(energy,irow,inner,s)
          call hf1(hist3id+1,ecfit_hit(energy,irow,inner,s),1.)
c          call hf2(hist4id+1,ecfit_hit(ec_j,irow,inner,s),ecfit_hit(ec_i,irow,inner,s),1.)
          if(ntstripc(irow,inner,s).eq.3)then
            call hf1(hist3id+4,ecfit_hit(energy,irow,inner,s),1.)
            call hf2(hist4id+3,ecfit_hit(ec_j,irow,inner,s),ecfit_hit(ec_i,irow,inner,s),1.)
          endif
          do nst=1,EcEvu_Nreported(Ec_U,inner,s)
            ist=EcEvu_reportedID(nst,Ec_u,inner,s)
            adcr=ec_strip_e(irow,ist,ec_u,inner,s)
            tdcr=EcEvu_strip(TDC,ist,Ec_u,inner,s)
            if(stripc(ist,irow,Ec_U,inner,s).eq.1.and.ec_strip_e(irow,ist,ec_u,inner,s).gt.0.)then
c              call hf2(hist2id+1,float(ist),adcr,1.)
            if(ntstripc(irow,inner,s).eq.3)then
              call hf2(hist2id+11,float(ist),adcr,1.)
            endif
            endif 
          enddo
          do nst=1,EcEvu_Nreported(Ec_V,inner,s)
            ist=EcEvu_reportedID(nst,Ec_v,inner,s)
            adcr=ec_strip_e(irow,ist,ec_v,inner,s)
            tdcr=EcEvu_strip(TDC,ist,Ec_v,inner,s)
            if(stripc(ist,irow,Ec_v,inner,s).eq.1.and.ec_strip_e(irow,ist,ec_v,inner,s).gt.0.)then
c              call hf2(hist2id+2,float(ist),adcr,1.)
            if(ntstripc(irow,inner,s).eq.3)then
              call hf2(hist2id+12,float(ist),adcr,1.)
            endif
            endif 
          enddo
          do nst=1,EcEvu_Nreported(Ec_w,inner,s)
            ist=EcEvu_reportedID(nst,Ec_w,inner,s)
            adcr=ec_strip_e(irow,ist,ec_w,inner,s)
            tdcr=EcEvu_strip(TDC,ist,Ec_w,inner,s)
            if(stripc(ist,irow,Ec_w,inner,s).eq.1.and.ec_strip_e(irow,ist,ec_w,inner,s).gt.0.)then
c              call hf2(hist2id+3,float(ist),adcr,1.)
            if(ntstripc(irow,inner,s).eq.3)then
              call hf2(hist2id+13,float(ist),adcr,1.)
            endif
            endif 
          enddo
        ENDDO
        endif
        if(ecfit_nhit(outer,s).gt.0)then
        DO irow = 1,ecfit_nhit(outer,s)
          pixelo=0
          if(ntstripc(irow,inner,s).eq.3)then
             pixelo=1
          endif
          etot_o=etot_o+ecfit_hit(energy,irow,outer,s)
          call hf1(hist3id+2,ecfit_hit(energy,irow,outer,s),1.)
          call hf2(hist4id+2,ecfit_hit(ec_j,irow,outer,s),ecfit_hit(ec_i,irow,outer,s),1.)
          if(ntstripc(irow,outer,s).eq.3)then
            call hf1(hist3id+5,ecfit_hit(energy,irow,outer,s),1.)
            call hf2(hist4id+4,ecfit_hit(ec_j,irow,outer,s),ecfit_hit(ec_i,irow,outer,s),1.)
          endif
          do nst=1,EcEvu_Nreported(Ec_U,outer,s)
            ist=EcEvu_reportedID(nst,Ec_u,outer,s)
            adcr=ec_strip_e(irow,ist,ec_u,outer,s)
            tdcr=EcEvu_strip(TDC,ist,Ec_u,outer,s)
            if(stripc(ist,irow,Ec_U,outer,s).eq.1.and.ec_strip_e(irow,ist,ec_u,outer,s).gt.0.)then
c              call hf2(hist2id+4,float(ist),adcr,1.)
            if(ntstripc(irow,outer,s).eq.3)then
              call hf2(hist2id+14,float(ist),adcr,1.)
            endif
            endif 
          enddo
          do nst=1,EcEvu_Nreported(Ec_v,outer,s)
            ist=EcEvu_reportedID(nst,Ec_v,outer,s)
            adcr=ec_strip_e(irow,ist,ec_v,outer,s)
            tdcr=EcEvu_strip(TDC,ist,Ec_v,outer,s)
            if(stripc(ist,irow,Ec_v,outer,s).eq.1.and.ec_strip_e(irow,ist,ec_v,outer,s).gt.0.)then
c              call hf2(hist2id+5,float(ist),adcr,1.)
            if(ntstripc(irow,outer,s).eq.3)then
              call hf2(hist2id+15,float(ist),adcr,1.)
            endif
            endif 
          enddo
          do nst=1,EcEvu_Nreported(Ec_w,outer,s)
            ist=EcEvu_reportedID(nst,Ec_w,outer,s)
            adcr=ec_strip_e(irow,ist,ec_w,outer,s)
            tdcr=EcEvu_strip(TDC,ist,Ec_w,outer,s)
            if(stripc(ist,irow,Ec_w,outer,s).eq.1.and.ec_strip_e(irow,ist,ec_w,outer,s).gt.0.)then
c              call hf2(hist2id+6,float(ist),adcr,1.)
            if(ntstripc(irow,outer,s).eq.3)then
              call hf2(hist2id+16,float(ist),adcr,1.)
            endif
            endif 
          enddo
        ENDDO
        endif
        if(ecfit_nhit(whole,s).gt.0)then
        DO irow = 1,ecfit_nhit(whole,s)
          etot_w=etot_w+ecfit_hit(energy,irow,whole,s)
          eiosum=0.
          call hf1(hist3id+3,ecfit_hit(energy,irow,whole,s),1.)
          iin=matched(whole,inner,irow,s)
          iou=matched(whole,outer,irow,s)
          ediff=ecfit_hit(energy,irow,whole,s)
          if(iin.gt.0)then
            ediff=ediff-ecfit_hit(energy,iin,inner,s)
            sti=ntstripc(iin,inner,s)
            eiosum=eiosum+ecfit_hit(energy,iin,inner,s)
          endif
          if(iou.gt.0)then
            ediff=ediff-ecfit_hit(energy,iou,outer,s)
            sto=ntstripc(iou,outer,s)
            eiosum=eiosum+ecfit_hit(energy,iou,outer,s)
          endif
c          call hf2(hist3id+11,ecfit_hit(energy,irow,whole,s),eiosum,1.)
          call hf1(hist3id+13,ediff,1.)
          if(sti.eq.3.and.sto.eq.3)then
            call hf1(hist3id+6,ecfit_hit(energy,irow,whole,s),1.)
c            call hf2(hist3id+12,ecfit_hit(energy,irow,whole,s),eiosum,1.)
            call hf1(hist3id+14,ediff,1.)
          endif
        ENDDO
        call hf1(hist3id+15,etot_w-etot_i-etot_o,1.)
        if(pixeli.eq.1.and.pixelo.eq.1)call hf1(hist3id+16,etot_w-etot_i-etot_o,1.)
        endif
      enddo
      RETURN
      END
c
c------------------------------------------------------------------------------





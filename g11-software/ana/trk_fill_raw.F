      SUBROUTINE trk_fill_raw
c
c_begin_doc 
c  RCS ID string
c  $Id: trk_fill_raw.F,v 1.11 1999/03/09 17:40:43 boiarino Exp $
c
c  Documentation for subroutine trk_fill_raw
c
c  Purpose: Histogramming of raw data.  These histograms are booked in trk_init.
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:    Called from trk_fill
c  ------
c
c  Author:   Francois Roudot      Created:  Thu May 29 16:00:00 EDT 1997
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
c_end_inc

#include "bcs.inc"
c#include "sdakeys.inc"
#include "sdadigi.inc"
#include "trktcl_var.inc"



c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_fill_raw.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.11 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 17:40:43 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: trk_fill_raw.F,v 1.11 1999/03/09 17:40:43 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='trk_fill_raw')
      PARAMETER (CRAUTH='Francois H. Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,
     &        NDC0, LDC0, JDC0,
     &        MamInd,
     &        ISector, NHits(6,3), NCol,
     &        i, j,
     &        Lay, Wir, ID, TDC,
     &        isec, Reg, il, NLay, ih, nh,
     &        HistID

      real dc_info(3)
      real dc_hfn_info(4)
 
      parameter (NLay=6)
 

c_end_var
c
c  executable code for routine trk_fill_raw:
c----6----------------------------------------------------------------72
c

c	ipar_trk_histo = 3
c	ipar_trk_ntuple = 3
 
      NDC0  = MamInd(IW,'DC0 ')
      LDC0 = iw(NDC0)
      if ( LDC0 .le. 0 ) goto 666
 
      do i=1,6
         do j=1,3
            NHits(i,j) = 0
         enddo
      enddo

c     RAT 97.06.05
c     ntuples 101 - 106 have been superceded by ntuple 333
c     RAT
      if (ipar_trk_histo.ge.3.or.ipar_trk_ntuple.ge.3) then
        call hcdir('RAW',' ')
        do isec = 1, 6, 1
          do il = 1, 36, 1
            do ih = 1, dc_ndig(il,isec)
              dc_info(1) = dc_digi(1,ih,il,isec)     ! wire
              dc_info(2) = dc_digi(2,ih,il,isec)     ! tdc
              dc_info(3) = isec
              if(il.le.4) then
                if (ipar_trk_histo.ge.3) call hf2(10,dc_info(2),dc_info(1),1)
crat            if(dc_info(2).ge.2000.and.dc_info(2).le.2500) then
crat              if (ipar_trk_ntuple.ge.3) call hfn(101,dc_info)
crat            endif
              else
                if(il.le.12) then
                  if (ipar_trk_histo.ge.3) call hf2(20,dc_info(2),dc_info(1),1)
crat              if(dc_info(2).ge.2000.and.dc_info(2).le.2500) then
crat                if (ipar_trk_ntuple.ge.3) call hfn(102,dc_info)
crat              endif
                else
                  if(il.le.18) then
                    if (ipar_trk_histo.ge.3) call hf2(30,dc_info(2),dc_info(1),1)
crat                if(dc_info(2).ge.5000.and.dc_info(2).le.5500) then
crat                  if (ipar_trk_ntuple.ge.3) call hfn(103,dc_info)
crat                endif
                  else
                    if(il.le.24) then
                      if (ipar_trk_histo.ge.3) call hf2(40,dc_info(2),dc_info(1),1)
crat                  if(dc_info(2).ge.5000.and.dc_info(2).le.5500) then
crat                    if (ipar_trk_ntuple.ge.3) call hfn(104,dc_info)
crat                  endif
                    else
                      if(il.le.30) then
                        if (ipar_trk_histo.ge.3) call hf2(50,dc_info(2),dc_info(1),1)
crat                    if(dc_info(2).ge.7000.and.dc_info(2).le.7500) then
crat                      if (ipar_trk_ntuple.ge.3)  call hfn(105,dc_info)
crat                    endif
                      else
                        if(il.le.36) then
                          if (ipar_trk_histo.ge.3) call hf2(60,dc_info(2),dc_info(1),1)
crat                      if(dc_info(2).ge.7000.and.dc_info(2).le.7500) then
crat                        if (ipar_trk_ntuple.ge.3) call hfn(106,dc_info)
crat                      endif
                        endif
                      endif
                    endif
                  endif
                endif
              endif
            enddo
          enddo
        enddo

        Do isec = 1, 6
          Do Reg = 1, 3
            Do il = 12*Reg-11, 12*Reg
              nh = dc_ndig(il,isec)
              NHits(isec,Reg) = NHits(isec,Reg) + nh
              If (  nh.gt. 0 ) then
                Do ih = 1,nh
                  Wir = dc_digi(1,ih,il,isec)
                  TDC = dc_digi(2,ih,il,isec)

c                 fill the layer vs wire histograms
                  HistID =  2 + isec*100 + 7000
                  if (ipar_trk_histo.ge.3) call hfill(HistID,float(Wir),float(il),1.0)

c                 fill the tdc histograms
                  HistID = 3 + isec*100 + Reg*1000
                  if (ipar_trk_histo.ge.3) call hfill(HistID, float(TDC), 0.0, 1.0)

c                 fill the ntuples
c lmq 7-1-97                  if ( ipar_trk_ntuple .ge. 3) call hfn(333,float(Wir), float(TDC), float(il), float(isec))
                  dc_hfn_info(1) = float(Wir)
                  dc_hfn_info(2) = float(TDC)
                  dc_hfn_info(4) = float(il) 
                  dc_hfn_info(3) = float(isec)
                  if ( ipar_trk_ntuple .ge. 3) call hfn(333,dc_hfn_info)
                  
                Enddo
              Endif
            Enddo

c           fill the number of hits histograms
            HistID = 1 + isec*100 + 1000*Reg
            call hfill(HistId, float(NHits(isec,Reg)), 0.0, 1.0)

          Enddo
        Enddo
        call hcdir('//PAWC/TRK',' ')
      endif


 666  RETURN
      END
c
c------------------------------------------------------------------------------





      SUBROUTINE BOS_TAGR
c
c_begin_doc
c  RCS ID string
c  $Id: bos_tagr.F,v 1.5 2009/03/14 20:27:52 fklein Exp $
c
c  Documentation for subroutine BOS_TAGR
c
c  Purpose:
c  --------
c  This routine generates tagger result bank in GSIM.
c
c  Author:   Burin Asavapibhop      Created:  Sat Sep 12 17:36:29 EDT 1998
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
#include "gcbank.inc"
#include "gcnum.inc"
#include "gconst.inc"
#include "ffpar.inc"
#include "tagcal.CMN"
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
      PARAMETER (CFILE = '$RCSfile: bos_tagr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/14 20:27:52 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: bos_tagr.F,v 1.5 2009/03/14 20:27:52 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOS_TAGR')
      PARAMETER (CRAUTH='Burin Asavapibhop')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer ncol_tagr
      parameter (ncol_tagr=6)
c      real rad2tgt
c      parameter (rad2tgt=2198) ! distance from radiator to center of target(cm)
      
      CHARACTER*128  fname
      INTEGER i,j,k, ebin,tbin
      INTEGER ilun,iret, ind,MBANK, itvx,nt1,nt2,nwbuf
      REAL    Tbound(122),Ebound(768), E_gamma, Enorm
      REAL    val, buf(10), v_pos(3), tof_v
      LOGICAL OK
      integer where
c_end_var
c
c  executable code for routine BOS_TAGR:
c----6----------------------------------------------------------------72
c
c  skip this routine if Beam_energy NOT defined (CARD: Beam_energy_ff)
c                    or energy of incoming photon not defined
c
      if( Beam_energy_ff(1).LE.0.0 ) RETURN
      E_gamma = 0.0
      itvx = 0
      do while(E_gamma.EQ.0.0 .AND. itvx.LT.NVERTX)
         itvx = itvx+1
         call gfvert(itvx,v_pos,nt1,nt2,tof_v,buf,nwbuf)
         if(nt1+nt2.EQ.0 .AND. nwbuf.GT.1) then
            if(buf(1).LT.0.000001 .AND. buf(2).GT.0.0) E_gamma = buf(2)
         endif
      enddo
      if(E_gamma.EQ.0.0) then
         if(Beam_energy_ff(2).GT.0.0) then
            E_gamma=Beam_energy_ff(2)
         else
            RETURN
         endif
      endif

      IF (NWRITE .LT. 1) THEN

        CRMESS='Reading energy E and T boundaries from file'
        CALL RECMES(CRNAME,'I',CRMESS)
        OK = .FALSE.
        where = 0
        call tag_read_bounds(OK, where, Beam_energy_ff(1))
        if (.NOT. OK) then
           CRMESS='Problem with reading boundaries'
           CALL RECMES(CRNAME,'W',CRMESS)
           return
        endif

        CRMESS='Filling TAGR bank...'
        CALL RECMES(CRNAME,'I',CRMESS)
        NWRITE = NWRITE + 1
      ENDIF
C
C  fill TAGR bank:
C
C  (1) check whether energy inside Tagger range
C
      Enorm = E_gamma/Beam_energy_ff(1)
      if (Enorm.GE.Eboundary(1) .OR. Enorm.LE.Eboundary(768)) then
         RETURN
      endif
      if (Enorm.GE.Tboundary(1) .OR. Enorm.LE.Tboundary(122)) then
         RETURN
      endif

      ebin=1
      do while (Enorm.LT.Eboundary(ebin+1))
         ebin = ebin +1
      enddo

      tbin=1
      do while (Enorm.LT.Tboundary(tbin+1))
         tbin = tbin +1
      enddo

c  fill TAGR bank 
c The start time here is set to be negative so it'll be canceled out in
c recsis because in seb/gtrig_time, the tagger time needs to be corrected
c for the vertex position.

      ind=MBANK(IW,'TAGR',0,ncol_tagr,1)
      rw(ind+1) = Beam_energy_ff(1)*Eaverage(ebin) ! tagged photon energy(GeV)
      rw(ind+2) = -v_pos(3)/clight                ! start time(ns)
      rw(ind+3) = -v_pos(3)/clight                ! start time(ns)
      iw(ind+4) = 7	 	                  ! status
      if(tbin.eq.38) tbin= 37   !ignore T20 if overlap with T19
      iw(ind+5) = tbin                            ! T bin
      iw(ind+6) = ebin                            ! E bin

      END
c
c------------------------------------------------------------------------------








      SUBROUTINE trk_fill_banks_per_sect(Level,ntracks,isec)
*     ----------------------------------
*_begin_doc
*  RCS ID string
*  $Id: trk_fill_banks_per_sect.F,v 1.8 1999/02/16 01:58:30 boiarino Exp $
*
*  Documentation for subroutine TRK_FILL_BANKS_PER_SECT
*
*  Purpose: This routine fills the track layer BOS banks
*              (all Tracking Output banks depending on Sector no.)
*  --------                    
*                         
*  Inputs Parameters: - ISEC    Sector No.
*                     - Level   Tracking Analysis Level (2: HBT; 4: TBT)
*                     - ntracks #tracks that were reconstructed in all (!) sectors
*
*  Outputs Parameters: None
*  -------------------
*
*  Calling routines: ana_prfit, ana_fit
*  ----------------
*
*  Notes:  This routine is valid until tracking is working with BOS workbanks
*  ------
*
*  Author:       Franz Klein  (Nov.1997)
*  -------         
*
*  Major revisions: 
*  ---------------- 
*
*  Minor revisions:  Dec-2-97 (F.Klein): change TRKS bank to 10 columns
*  ----------------                      in order to save matched hit pos.
*                                        in CC, SC, EC
*_end_doc
*
      Implicit None
      Save
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "bcs.inc"
#include "anapatt.inc"
#include "sdaswim.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
      integer  isec, Level, ntracks
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_fill_banks_per_sect.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.8 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/02/16 01:58:30 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: trk_fill_banks_per_sect.F,v 1.8 1999/02/16 01:58:30 boiarino Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_FILL_BANKS_PER_SECT')
      PARAMETER (CRAUTH='Franz Klein')
*
*  Local User defined variables:
*  -----------------------------
      INTEGER  MBANK
      INTEGER  ind1, ind2, ind
      INTEGER  trklist(MAX_TRACKS), ntrk, trkno2(MAX_TRACKS), ntrk2
      INTEGER  trkno0(MAX_TRACKS), ntrk0
      INTEGER  it, i, j, is, ig, isg, icl, il, ipl
      INTEGER  n_dclay, n_planes, n_seg
      INTEGER  ncol_hbla, ncol_tbla, ncol_dpl, ncol_trks
       DATA ncol_hbla/16/
       DATA ncol_tbla/17/
       DATA ncol_dpl/8/
       DATA ncol_trks/10/

*
*_end_var
*
* Count the number of tracks that will be stored
* In order to compare HBT & TBT banks we count all Hit Based Tracks too
      ntrk  = 0
      ntrk0 = 0
      ntrk2 = 0
      do it = 1,ntracks
        if(itr_level(it).GE.TRK_LEVEL_hbt) then
          ntrk2 = ntrk2 +1
        endif
        if(itr_level(it).GE.Level) then
          ntrk0 = ntrk0 +1
          if(itr_sect(it).EQ.isec) then
            ntrk = ntrk +1
            trklist(ntrk) = it
            trkno0(ntrk) = ntrk0
            trkno2(ntrk) = ntrk2
          endif
        endif
      enddo

      if(ntrk.EQ.0) goto 999

*--c.a.meyer march 97 (cf. old: trk_fill_dcla.F)
*
*        Create the HBLA or TBLA bank to contain essentially a copy of the 
*        track-layer array. There is one bank for each sector containing data.
*        We are essentially ignoring the 2 missing layers in region 1, so we 
*        will subtract 2 from the actual number of planes.
*        In the Tracking the Drift Chambers are plane 'ndc_min'(#4) to 'ndc_max'(#39).
*   For the inner and outer detector planes (HDPL & TDPL):
*   1 row  for 'Vertex plane'=closest distance to beamline  (=plane #1)
*   2 rows for Start Counters   (=planes 'nst_min'(#2)-'nst_max'(#3))
*   1 row  for Cerenkov Counter (=plane  'ncc_min'(#40))
*   4 rows for TOF Counters (SC planes 1,2,3&4,5&6)  
*                           (=planes 'nsc_min'(#41)-'nsc_max'(#44))
*   2 rows for EC (forw.EC and LA EC)  (=planes 'nec_min'(#45)-'nec_max'(#46))

      n_dclay  = ntrk * (npl_dc-2) 
      n_planes = ntrk * (1+npl_st+npl_cc+npl_sc+npl_ec)

      n_seg = 0
      do it = 1,ntrk
        do is = 1,6
          icl = lnk_clust(is,trklist(it))
          if(icl.LE.0) then
            n_seg = n_seg +1
          else
            n_seg = n_seg + nsegmc(icl,is,isec)
          endif
        enddo
      enddo
 
*  Create the banks wrt. to Tracking Level 
*                   'name',rec_no,ncol,nrow

      if(Level.EQ.TRK_LEVEL_hbt) then

	ind1 = MBANK(iw,'HBLA', isec,ncol_hbla, n_dclay)
	ind2 = MBANK(iw,'HDPL', isec,ncol_dpl, n_planes)
	if(ind1.EQ.0 .OR. ind2.EQ.0) then
          write(CRMESS,'(A)')' Error Creating HBLA & HDPL & banks'
          call RecMes(CRNAME,'W',CRMESS)
          goto 999
        endif

      elseif(Level.EQ.TRK_LEVEL_tbt) then

	ind1 = MBANK(iw,'TBLA', isec,ncol_tbla, n_dclay)
	ind2 = MBANK(iw,'TDPL', isec,ncol_dpl, n_planes)
	if(ind1.EQ.0 .OR. ind2.EQ.0) then
          write(CRMESS,'(A)')' Error Creating TBLA & TDPL banks'
          call RecMes(CRNAME,'W',CRMESS)
          goto 999
        endif

      else
        goto 999
      endif

*   Fill Layer Bank (HBLA or TBLA) for Drift Chambers:
*   Loop over DC layers but skip non existing Layers 5 & 6
*
 10   continue
      ind = ind1
      do it = 1, ntrk
        do il = 1,npl_dc
          if(il.LT.5 .OR. il.GT.6) then
            ipl = nst_max + il
            IW(ind+1) = trkno0(it)*100+ipl		!100*track# + plane#
            RW(ind+2) = trkp(2,ipl,trklist(it))		!position
            RW(ind+3) = trkp(3,ipl,trklist(it))
            RW(ind+4) = trkp(1,ipl,trklist(it))
c (fk,6/98) changed to B field info
            RW(ind+5) = trkp(14,ipl,trklist(it))		!B-Field
            RW(ind+6) = trkp(15,ipl,trklist(it))
            RW(ind+7) = trkp(13,ipl,trklist(it))
            RW(ind+8) = trk(8,il,trklist(it))		!track length
            IW(ind+9) = trk_pt(il,trklist(it))		!ptr to DC1
            IW(ind+10)= IFIX(trk(12,il,trklist(it)))	!Status
            IW(ind+11)= IFIX(trk(1,il,trklist(it)))	!wire#
            RW(ind+12)= trk(3,il,trklist(it))		!drift time
            RW(ind+13)= trk(10,il,trklist(it))		!track angle in Layer
            RW(ind+14)= trk(9,il,trklist(it))		!wire length to preamp.
            RW(ind+15)= trk(5,il,trklist(it))		!Sigma doca
            RW(ind+16)= trk(7,il,trklist(it))		!fitted doca
            if(Level.EQ.TRK_LEVEL_tbt) then
              RW(ind+17)= trk(4,il,trklist(it))		!calculated doca
              ind = ind + ncol_tbla
            else
              ind = ind + ncol_hbla
            endif
          endif
        enddo
      enddo

      ind = ind2
      do it = 1, ntrk
        do ipl = 1,npln
          if( ipl.LE.nst_max .OR.
     &       (ipl.GE.ncc_min .AND. ipl.LE.ncc_max) .OR.
     &       (ipl.GE.nsc_min .AND. ipl.LE.nsc_max) .OR.
     &       (ipl.GE.nec_min .AND. ipl.LE.nec_max)) then

            IW(ind+1) = trkno0(it)*100+ipl		!100*track# + plane#
            RW(ind+2) = trkp(2,ipl,trklist(it))		!position
            RW(ind+3) = trkp(3,ipl,trklist(it))
            RW(ind+4) = trkp(1,ipl,trklist(it))
            RW(ind+5) = trkp(5,ipl,trklist(it))		!direction
            RW(ind+6) = trkp(6,ipl,trklist(it))
            RW(ind+7) = trkp(4,ipl,trklist(it))
            RW(ind+8) = trkp(10,ipl,trklist(it))	!track length
            ind = ind + ncol_dpl
          endif
        enddo
      enddo

999   continue
      return
      end





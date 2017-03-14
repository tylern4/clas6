
      SUBROUTINE trk_evnt_tbt(L_TBT)
*_begin_doc
*  RCS ID string
*  $Id: trk_evnt_tbt.F,v 1.18 2004/09/01 20:28:48 lcsmith Exp $
*
*  Documentation for subroutine TRK_EVNT_TBT
*
*  Purpose:  This routine has been stripped out of sda ana_main, 
*  --------  to be used as the event interface bewteen RecSis and sda.
*
*  Input Parameters: 
*  ----------------
*
*  Output Parameters: NONE
*  -----------------
*
*  Other routines:
*  ---------------
*
*  Notes:
*  ------
*
*  Author:         Curtis A. Meyer 
*  -------         Carnegie Mellon University
*
*  Creation Date:  22 March, 1996
*  --------------
*
*  Major revisions:
*  ----------------
*      03-June-1996 C.A.Meyer -- Added call to trktcl_evnt() at the 
*                  start of this routine to guarantee that things
*                  are up-to-date.
*      22-April-1997 F. Roudot -- separate trk_evnt into
*                  trk_evnt_hbt() Hit Based Tracking
*                  and trk_evnt_tbt() Time Based Tracking
*                  to allow a call to the SEB in between.
*         NOV-1997 F.Klein -- remove obsolete common block 'sdakeys.inc'
*
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "anapatt.inc"
#include "trktcl_var.inc"
#include "bcs.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
	logical L_TBT
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_evnt_tbt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.18 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/09/01 20:28:48 $')
      PARAMETER ( CAUTHO = '$Author: lcsmith $')
      DATA CRCSID/
     1'$Id: trk_evnt_tbt.F,v 1.18 2004/09/01 20:28:48 lcsmith Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_EVNT_TBT')
      PARAMETER (CRAUTH='Curtis A. Meyer')
*
*  Local User defined variables:
*  -----------------------------
*
      INTEGER  ntracks,ntr_fit,status,isec,irun,ievnt,irec
      LOGICAL Lgfrst
      SAVE    Lgfrst
      DATA    Lgfrst /.true./

      If( Lgfrst ) Then
        Lgfrst = .false.
        Call RecMes(CRNAME,'I',' Time Based Tracking processing.')
      Endif
      ntracks = 0
      IF(ipar_trk_level.LT.TRK_LEVEL_lramb) goto 999

      if(ipar_trk_print(5).ne.0 .or. ipar_trk_print(6).ne.0 .or. ipar_trk_print(7).ne.0) then
        call rernev(irun,ievnt,irec)
        print *,' '
        print *,' Start TBT for event',ievnt
        print *,' '
      endif


*  Read in Results Banks from Hit Based Tracking
*  or refit the track (cf. trk_calc_wire,tbt_segm in trk_dcam)

      if(ipar_trk_level.GE.TRK_LEVEL_scl) then
         call trk_read_bank(ntracks)
         if(ntracks.EQ.0) goto 999
      else
         call setup_anapatt
      endif

*  Derive DCA and errors on candidate tracks, resolve L-R Ambiguity. (Level 3)

      CALL trk_dcam(ntr_fit,ntracks)
      IF(ntr_fit.EQ.0) GOTO 999
c      CALL ana_statb(6)
      IF(ntracks.EQ.0) GOTO 999
c      CALL ana_statb(7)

*  Make final fits for each track (Level 4)

      IF(ipar_trk_level.GE.TRK_LEVEL_tbt) then
        CALL ana_fit(ntracks)
        IF(ntracks.EQ.0) GOTO 999
c        CALL ana_statb(8)
      endif 

c      if(ntracks.GE.ipar_trk_mintracks) CALL ana_statb(9)	! #reconstructed events

c Reconstruct tracks that hit only region 1
      if (ipar_trk_region1.ne.0) then
        do isec=1,6
c        print *,'call region1(',isec,')'
          call region1(isec,status)
        enddo
      endif

 999  continue
      if(ipar_trk_statistics.eq.2) then
        call trk_stati(ipar_trk_statistics)
      endif

      L_TBT=ntracks.GE.ipar_trk_mintracks
*
      RETURN
      END

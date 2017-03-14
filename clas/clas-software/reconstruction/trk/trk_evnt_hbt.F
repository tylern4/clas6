      SUBROUTINE trk_evnt_hbt()
*_begin_doc
*  RCS ID string
*  $Id: trk_evnt_hbt.F,v 1.24 2005/11/09 20:41:23 stepanya Exp $
*
*  Documentation for subroutine TRK_EVNT_HBT
*
*  Purpose:  This routine has been stripped out of sda ana_main, 
*  --------  to be used as the event interface bewteen RecSis and sda.
*
*  Input Parameters: NONE
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
*      Sept.1997   F.Klein -- add options to hit based tracking     
*      Nov. 1997   F.Klein -- remove obsolete common block 'sdakeys.inc',
*                             remove call to 'trktcl_evnt' (no more necess. 
*                              after removing 'sdakeys.inc')
*      Dec. 1997   F.Klein -- set cuts on DC1 bank and dc_digi arrays
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
*     
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_evnt_hbt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.24 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2005/11/09 20:41:23 $')
      PARAMETER ( CAUTHO = '$Author: stepanya $')
      DATA CRCSID/
     1'$Id: trk_evnt_hbt.F,v 1.24 2005/11/09 20:41:23 stepanya Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_EVNT_HBT')
      PARAMETER (CRAUTH='Curtis A. Meyer')
*     
*  Local User defined variables:
*  -----------------------------
      INTEGER ifail,irr,isec,irun,ievnt,irec
      INTEGER j,k,n_elsec, ncand,secno(6),secno2(6), elsec(6)
      INTEGER MAMIND, nam_eid0, ind, ncol_eid0
      LOGICAL Lgfrst, my_flag1, my_flag2, lfound
      SAVE    Lgfrst
      DATA    Lgfrst /.true./
      DATA    nam_eid0 /0/

      If ( Lgfrst ) Then
         Lgfrst = .false.
         Call RecMes(CRNAME,'I',' Entry to dc processing.')
         nam_eid0=MAMIND(iw,'EID0')
      Endif

c Start up HBT. First make sure all old HBT BosBanks are gone
      call BDROP(iw,'DC1 EID0TRKSMTRKSGMPCLSTHBTBHBTRHBERHBLAHDPLDHCLTBTRTBERTBLATDPLTRL1')

      if(ipar_trk_print(1).ne.0 .or. ipar_trk_print(2).ne.0 .or. ipar_trk_print(3).ne.0) then
         call rernev(irun,ievnt,irec)
         print *,' '
         print *,' Start HBT for event',ievnt
      endif

c check for electron candidates from matching EC*CC*SC

      if( ipar_trk_electron_id.ne.0) then
        call eid0(ncand)
        if(ncand.eq.0) goto 999

        if(nam_eid0.ne.0) ind=iw(nam_eid0)

        if(ind.le.0 .or. iw(ind).eq.0) then
          call rernev(irun,ievnt,irec)
          write(crmess,*)' NO EID0 bank in event',ievnt
          call recmes(crname,'W',crmess)
          goto 999
        endif
        do isec=1,6
          elsec(isec)=0
        enddo

        ncol_eid0=iw(ind-5)
        do j=1,iw(ind)/ncol_eid0
          isec=iw(ind+(j-1)*ncol_eid0+1)
          if(isec.gt.0.and.isec.le.6) elsec(isec)=j
        enddo
        k=0
        n_elsec=0
        do isec=1,6
          if(elsec(isec).gt.0) then
            n_elsec=n_elsec+1
            secno(n_elsec)=isec
          else
            k=k+1
            secno2(k)=isec
          endif
        enddo
        if(n_elsec.lt.6) then
          k=1
          do j=n_elsec+1,6
            secno(j)=secno2(k)
            k=k+1
          enddo
        endif
        write(crmess,*)' do HBT in sequence: Sect.',secno
        call recmes(crname,'I',crmess)
      else
        n_elsec=6
        do j=1,6
          secno(j)=j
        enddo
      endif

      call setup_anapatt()

c
c build DC1 bank wrt. cut_level on DC0 and TDC values 
c
      call make_dc1_digi()

*  if no procesing only initializing then bail out now
 
      IF(ipar_trk_level.LT.TRK_LEVEL_Linking) GOTO 999
      
*  if only Time Based Tracking is required -> exit

      IF(ipar_trk_level.GT.TRK_LEVEL_vx) THEN
        IF(ipar_trk_level.GE.TRK_LEVEL_scl)THEN  
          CALL READ_TRK_BANKS(irr)  !Read segments, clusters and links from BOS.
          if(irr.lt.0)then
            Call RecMes(CRNAME,'W','No segment and claster banks are found')
            GOTO 999
          endif  
          if(ipar_trk_level.EQ.trk_level_scl) GOTO 888
        ENDIF
        GOTO 999
      ENDIF

      my_flag1 = .FALSE.
      my_flag2 = .FALSE.
      lfound   = .false.
      DO 100 j = 1,6
        isec=secno(j)
        CALL ana_segm(isec,ifail)             ! Segment and Cluster Finding.
        IF(ifail.EQ.0)  GO TO 100
        if(.NOT.my_flag1) then
          my_flag1 = .TRUE.
        endif
        
        CALL trk_link(isec,ifail) ! Cluster linking a la Jim M.
        if(ifail.eq.0) goto 100
        if(j.le.n_elsec) then
           lfound=.true.
        else
           if(.not.lfound) goto 999
        endif
        IF(my_flag2) goto 100
        my_flag2 = .TRUE.
 100  CONTINUE

C- Fill BOS banks for found segments, clusters and links. 
      CALL STORE_BANK 
      if(ipar_trk_level.EQ.TRK_LEVEL_Linking) goto 999   ! only Segment&Cluster finding

*  Check number of track candidates versus required multiplicity
      if(ntr_link.LT.ipar_trk_mintracks) then
        ifail = 0
        GO TO 999
      endif

*  Loops over Track Candidates (ntr_link)  (Hit Based Tracking: level=2)
 888  continue
      CALL ana_prfit(ifail)
      IF(ifail.EQ.0) GO TO 999
c      IF(ifail.GE.ipar_trk_mintracks) CALL ana_statb(4)

 999  continue
      if( (ipar_trk_level.LE.TRK_LEVEL_hbt .OR. ifail.EQ.0)
     &     .AND. ipar_trk_statistics.eq.2) then
        call trk_stati(ipar_trk_statistics)
      endif

      RETURN
      END



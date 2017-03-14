      SUBROUTINE MFM_BRUN
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_brun.F,v 1.6 1999/03/13 03:51:44 kossov Exp $
c
c  Documentation for subroutine MFM_BRUN
c
c  Purpose: Readout & Initialization of the Magnetic Field Modified
c  -------- It should be called at the begin of run time
c
c  Calling Sequence: Check if the magnetic field should be calculated or
c  ----------------- retrieved. If calculate - calls MFM_FIELD in LOOP
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from: user_brun.F subroutine
c  ------------
c
c  Other routines: MFM_FIELD - calculates magnetic field in a particular point
c  ---------------
c
c  Notes: The parameters of the magnetic field weights are from -1 to +1. The
c  ------ real weights should be extracted from the run information (!)
c
c  Author:   Mikhail Kossov      Created:  Mon May 12 14:01:12 EST 1997
c  -------
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
#include "mfm_control.inc"
#include "mfm_grid.inc"
C -- added to communicate with TRK parameters -- M.K. - 17-Jan-98
#include "trktcl_var.inc" 
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_brun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.6 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/03/13 03:51:44 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='mfm_brun')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER NX3,NYNX3, IX,IY,IZ, Indx, nrw, IFIRST, LUNC
      INTEGER numra,numrb,icl,ierr, numdb,ncol,nrow,nch, nread
      REAL time_init, time_out, time_exe
      REAL*8 C30
      REAL B(3),R(3), Sbt,Smt,Spt, Cmax_t,Cmax_m,Cmax_p, T30,twoto20,twoto24
      PARAMETER (twoto20 =  1048576.)
      PARAMETER (twoto24 = 16777216.)
      REAL magfil(3)     ! 1-BT,2-MT,3-PT - should be read from Begin Run Bank
      REAL QNXP,QNX,Xmin_d,Ymin_d,Zmin_d,DX_d,DY_d,DZ_d
C      INTEGER NX_d,NY_d,NZ_d,NZ_i,NZ_l
      INTEGER NX_d,NY_d,NZ_d,NZ_l
C_ X - R [0,500,101*5], Y - tgPHI [0,.60(>tg30),16*.04], Z [-300.,500.,161*5.]
      DATA Xmin_d,Ymin_d,Zmin_d,DX_d,DY_d,DZ_d/-5.,-.036,-305.,5.,.036,5./
      DATA NX_d,NY_d,NZ_d/102,36,162/

C
      CHARACTER*24 dtimes
      CHARACTER*50 fname
      CHARACTER*12 format1,flnam,fl_bt,fl_mt,fl_pt
      CHARACTER*4  format2
      CHARACTER*8 kname,Tname,Mname,Pname, hname1,hname2,unit
      DATA IFIRST/0/
      DATA fl_bt,fl_mt,fl_pt/'bgr_mkbt.fpk','bgr_mkmt.fpk','bgr_mkpt.fpk'/
c
      EQUIVALENCE (QNX,NX)
      EQUIVALENCE (QNXP,NXP)
c
c_end_var
c
c  executable code for routine MFM_BRUN:
c  -----------------------------------------------------------------------
c
      IF (IFIRST .EQ. 0) THEN
        IFIRST = 1
        NX = -1
        NXP = -1
C
C -- Calculate a step in TAN(PI)
        C30    = DSQRT(3.D0)/2.D0
        T30    = 0.5D0/C30
        icl    = T30*twoto20
        DY_d   = FLOAT(icl+1)/twoto24
        Ymin_d = 0.- DY_d
        WRITE(CRMESS,*)'First call DY=',DY_d,', MFM_MAG_type=',MFM_MAG_type
        CALL RECMES(CRNAME,'I',CRMESS)
        IF ( 0. - Ymin_d .LT. DY_d .OR. T30/DY_d .GE. 16.) THEN
          WRITE(CRMESS,*)'Potential problem Y=',DY_d+Ymin_d,',T=',T30/DY_d-16.
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF 
      ENDIF
C Chenge for TRK package -- M.K. 17-Jan-98
c      magfil(1)=MFM_BigTorus
c      magfil(2)=MFM_MinTorus
c      magfil(3)=MFM_PolTargt
C
      magfil(1)=FLOAT(ipar_torus_current)
      magfil(2)=FLOAT(ipar_minitorus_current)
      magfil(3)=FLOAT(ipar_poltarget_current)
      MFM_comp_fld=0 ! fixed for TRK - no comparison (needs a lot of
                     ! additional (including DRAWING) stuff.
C ================= End of main changes for TRK package ============
C
C Open and read old table
C
C      PRINT*,'MFM_BRUN: trk_magtype=',ipar_trk_magtyp
      IF(ipar_trk_magtyp .EQ. 0) THEN ! TRK analog of "MFM_MAG_type" check
C      IF(MFM_MAG_type .EQ. 0) THEN
C
C Initialize in case nothing has been read
        Tname = 'BTORU'
        Mname = 'BMINT'
        Pname = 'BPLTG'
        Cmax_t = float(Imax_t)
        Cmax_m = float(Imax_m)
        Cmax_p = float(Imax_p)
C
C Read B for Big Torus or any new field for the new/old comparison
       IF(magfil(1).NE.0..AND.MFM_comp_fld.LT.3 .OR. MFM_comp_fld .EQ. 2) THEN
          CALL RESLUN(CRNAME,LUNC,ierr)
          IF (ierr .EQ. -1) CALL RECMES(CRNAME,'F',
     &        'unable to reserve fortran unit for Big Tor. or New Mag. Field')
          WRITE(CRMESS,*) ' Big Torus or New has got LUN #', LUNC
          CALL RECMES(CRNAME,'I',CRMESS)
          IF (LUNC .LT. 10) WRITE(unit, 9) LUNC
          IF (LUNC .GE. 10) WRITE(unit,10) LUNC
 9        FORMAT(' UNIT=',I1,' ')
 10       FORMAT('UNIT=',I2,' ')
C
          IF (magfil(1) .NE. 0.) THEN                               !New BT=>Q
            CALL REVINM('CLAS_PARMS','bgrid_bt.fpk',fname)
          ELSEIF (magfil(2) .NE. 0. .AND. MFM_comp_fld .EQ. 2) THEN !New MT=>Q
            CALL REVINM('CLAS_PARMS','bgrid_mt.fpk',fname)
          ELSEIF (magfil(3) .NE. 0. .AND. MFM_comp_fld .EQ. 2) THEN !New PT=>Q
            CALL REVINM('CLAS_PARMS','bgrid_pt.fpk',fname)
          ELSE
            WRITE(CRMESS,*) 'Not clear what to compare => fill MFM_BT,_MT,_PT'
            CALL RECMES(CRNAME,'I',CRMESS)            
            RETURN
          ENDIF
          CALL FPARM(' OPEN BGRID ' // unit // 'FILE="' // fname // '" ')
          CALL FRNAME('BGRID')
          CALL FRKEY(kname,numra,numrb,icl,ierr)
          Tname = kname
          CALL FRHDR(hname1,numdb,ncol,nrow,format1,nch,ierr)
          nrw = ncol*nrow
          CALL FRDAT(nread,QNX,nrw)
          WRITE(CRMESS,*)'a#of words in title=',nread,',grid:',NX,'x',NY,'x',NZ
          CALL RECMES(CRNAME,'I',CRMESS)
          WRITE(CRMESS,*)'XYZmin=',Xmin,Ymin,Zmin,',DXYZ=',DX,DY,DZ,',Cm=',CMAX
          CALL RECMES(CRNAME,'I',CRMESS)
          IF (Cmax .GT. 0.) Cmax_t = Cmax
          Ngr=NX*NY*NZ*3
          IF(Ngr.GT.Ng) THEN ! check limit of reserved arrays
            WRITE(CRMESS,*)'Grid volume for Big Torus ',Ngr,' > ',Ng
            CALL RECMES(CRNAME,'F',CRMESS)
            RETURN
          ENDIF
          CALL FRHDR(hname2,numdb,ncol,nrow,format2,nch,ierr)
          nrw = ncol*nrow
          CALL FRDAT(nread,Q,nrw)
          CALL FPARM('CLOSE BGRID')
          CLOSE(LUNC)
          CALL RESLUN(CRNAME,-LUNC,ierr)
          WRITE(CRMESS,*)'For Big Torus or New the file ',kname,' is read'
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF
C
C Read B for Mini Torus
        IF(magfil(2).NE.0. .AND. MFM_comp_fld .LT. 2) THEN ! Only for new field
          CALL RESLUN(CRNAME,LUNC,ierr)
          IF (ierr .EQ. -1) CALL RECMES(CRNAME,'F',
     &        'unable to reserve fortran unit for Mini Torus Magnetic Field')
          WRITE(CRMESS,*) ' Mini Torus has got LUN #', LUNC
          CALL RECMES(CRNAME,'I',CRMESS)
          IF (LUNC .LT. 10) WRITE(unit,9)LUNC
          IF (LUNC .GE. 10) WRITE(unit,10)LUNC
          CALL REVINM('CLAS_PARMS','bgrid_mt.fpk',fname)
          CALL FPARM(' OPEN BGRID ' // unit // 'FILE="' // fname // '" ')
          CALL FRNAME('BGRID')
          CALL FRKEY(kname,numra,numrb,icl,ierr)
          Mname = kname
          CALL FRHDR(hname1,numdb,ncol,nrow,format1,nch,ierr)
          nrw = ncol*nrow
          CALL FRDAT(nread,QNX,nrw)
          WRITE(CRMESS,*)'a#of words in title',nread,',grid:',NX,'x',NY,'x',NZ
          CALL RECMES(CRNAME,'I',CRMESS)
          WRITE(CRMESS,*)'XYZmin=',Xmin,Ymin,Zmin,',DXYZ=',DX,DY,DZ,',C=',CMAX
          CALL RECMES(CRNAME,'I',CRMESS)
          IF (Cmax .GT. 0.) Cmax_m = Cmax
          Ngr=NX*NY*NZ*3
          IF(Ngr.GT.Ng) THEN
            WRITE(CRMESS,*)'Grid volume for Mini.Tor. ',Ngr,' > ',Ng
            CALL RECMES(CRNAME,'I',CRMESS)
            RETURN
          ENDIF
          CALL FRHDR(hname2,numdb,ncol,nrow,format2,nch,ierr)
          nrw = ncol*nrow
          CALL FRDAT(nread,Qm,nrw) ! Read
          CALL FPARM('CLOSE BGRID')
          CLOSE(LUNC)
          CALL RESLUN(CRNAME,-LUNC,ierr)
c
          CALL GetASCIITime(dtime,dtimes)
          WRITE(CRMESS,*)'For Mini.Tor. file ',kname,' is read at',dtimes
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF
C
C Rescale and Sum the B-fields
        IF (MFM_comp_fld .LT. 2) THEN ! Only for the new field
          Sbt = magfil(1)/Cmax_t ! Was in SDA now back for TRK
          Smt = magfil(2)/Cmax_m ! Was in SDA now back for TRK
c          Sbt = magfil(1) ! MFM
c          Smt = magfil(2) ! MFM
          IF    (magfil(1).NE.0. .AND. magfil(2).EQ.0.) THEN
            CALL VSCALE(Q,Sbt,Q,Ng)
          ELSEIF(magfil(1).EQ.0. .AND. magfil(2).NE.0.) THEN
            CALL VSCALE(Qm,Smt,Q,Ng)
          ELSEIF(magfil(1).NE.0. .AND. magfil(2).NE.0.) THEN
            CALL VLINCO(Q,Sbt,Qm,Smt,Q,Ng)
          ENDIF
          WRITE(CRMESS,*)'H=',Sbt,'*',Tname,'+',Smt,'*',Mname
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF
C
C Read B for the Polarised Target or an old field
        IF(magfil(3).NE.0..AND.MFM_comp_fld.LT.2 .OR. MFM_comp_fld .GT. 1) THEN
          CALL RESLUN(CRNAME,LUNC,ierr)
          IF (ierr .EQ. -1) CALL RECMES(CRNAME,'F',
     &     'unable to reserve fortran unit for Pol.Targ or Old Magnetic Field')
          WRITE(CRMESS,*) ' Polarized Target or an Old field got LUN=', LUNC
          CALL RECMES(CRNAME,'I',CRMESS)
          IF (LUNC .LT. 10) WRITE(unit,9)LUNC
          IF (LUNC .GE. 10) WRITE(unit,10)LUNC
          IF (MFM_comp_fld .LT. 2) THEN ! get new Polarized Target field
            CALL REVINM('CLAS_PARMS','bgrid_pt.fpk',fname)
          ELSEIF (magfil(1) .NE. 0) THEN
            CALL REVINM('CLAS_PARMS','bgrid_t.fpk',fname)
          ELSEIF (magfil(2) .NE. 0) THEN
            CALL REVINM('CLAS_PARMS','bgrid_m.fpk',fname)
          ELSEIF (magfil(3) .NE. 0) THEN
            CALL REVINM('CLAS_PARMS','bgrid_ptgnew.fpk',fname)
          ELSE
            WRITE(CRMESS,*) 'Not clear what to compare => fill MFM_BT,_MT,_PT'
            CALL RECMES(CRNAME,'I',CRMESS)            
            RETURN
          ENDIF
c
          CALL FPARM(' OPEN BGRID ' // unit // 'FILE="' // fname // '" ')
          CALL FRNAME('BGRID')
          CALL FRKEY(kname,numra,numrb,icl,ierr)
          Pname = kname
          CALL FRHDR(hname1,numdb,ncol,nrow,format1,nch,ierr)
          nrw = ncol*nrow
C
          IF (MFM_comp_fld .GT. 1) THEN
            CALL FRDAT(nread,QNXP,nrw)
            WRITE(CRMESS,*)'a#of w in title',nread,',grd:',NXP,'x',NYP,'x',NZP
            CALL RECMES(CRNAME,'I',CRMESS)
            WRITE(CRMESS,*)'XYZ=',Xmip,Ymip,Zmip,',D=',DXP,DYP,DZP,',C=',CMAXP
            CALL RECMES(CRNAME,'I',CRMESS)
            IF (Cmaxp .GT. 0.) Cmax_p = Cmaxp
            Ngr=NXP*NYP*NZP*3
          ELSE
            CALL FRDAT(nread,QNX,nrw)
            WRITE(CRMESS,*)'a#of wds in title',nread,',grid:',NX,'x',NY,'x',NZ
            CALL RECMES(CRNAME,'I',CRMESS)
            WRITE(CRMESS,*)'XYZm=',Xmin,Ymin,Zmin,',DXYZ=',DX,DY,DZ,',C=',CMAX
            CALL RECMES(CRNAME,'I',CRMESS)
            IF (Cmax .GT. 0.) Cmax_p = Cmax
            Ngr=NX*NY*NZ*3
          ENDIF
C
          IF(Ngr.GT.Ng) THEN
            WRITE(CRMESS,*)'Grid volume for Pol.Targ. ',Ngr,' > ',Ng
            CALL RECMES(CRNAME,'I',CRMESS)
            RETURN
          ENDIF
          CALL FRHDR(hname2,numdb,ncol,nrow,format2,nch,ierr)
          nrw = ncol*nrow
          CALL FRDAT(nread,Qm,nrw) ! Read
          CALL FPARM('CLOSE BGRID')
          CLOSE(LUNC)
          CALL RESLUN(CRNAME,-LUNC,ierr)
c
          WRITE(CRMESS,*)'For Pol.Targ. or Old file ',kname,' is read'
          CALL RECMES(CRNAME,'I',CRMESS)

C
C Rescale and Sum the B-fields
          Spt = magfil(3)/Cmax_p ! Was in SDA Changed back for TRK
c          Spt = magfil(3) ! MFM
c
          IF (MFM_comp_fld .LT. 2 .AND. magfil(3) .NE. 0.) THEN
            IF   (magfil(1) .EQ. 0. .AND. magfil(2) .EQ. 0.) THEN
              CALL VSCALE(Qm,Spt,Q,Ng)
            ELSE
              CALL VLINCO(Q,1.,Qm,Spt,Q,Ng)
            ENDIF
            WRITE(CRMESS,*)'H=',Sbt,'*',Tname,'+',Smt,'*',Mname,'+',Spt,
     *                     '*',Pname
            CALL RECMES(CRNAME,'I',CRMESS)
          ENDIF
c
        ENDIF
      ENDIF
C
C  Calculate and Write Table (MAGN 'TORU' or 'MTOR' or 'TORM') - LONG ...
C
C      IF (MFM_MAG_type .GT. 0 .AND. MFM_MAG_type .LT. 4 ) THEN
      IF (ipar_trk_magtyp .GT. 0 .AND. ipar_trk_magtyp .LT. 4 ) THEN ! mod for TRK
        CALL TIMEX(time_init)
C                  ___ READ THE INTERMEDIATE STATE (When life is bad) ___
C____ Read the file if it exists
c        CALL RESLUN(CRNAME,LUNC,ierr)
c        IF (ierr .EQ. -1) CALL RECMES(CRNAME,'F',
c     &      'unable to reserve fortran unit for Intermediate state saving')
c        WRITE(CRMESS,*) ' Intermediate saving got LUN #', LUNC
c        CALL RECMES(CRNAME,'I',CRMESS)
c        IF (LUNC .LT. 10) WRITE(unit, 9) LUNC
c        IF (LUNC .GE. 10) WRITE(unit,10) LUNC
cC
c        CALL FPARM(' OPEN BGRID ' // unit // 'FILE="bgrid_54.fpk" ')
cC
c        CALL FRNAME('BGRID')
c        CALL FRKEY(kname,numra,numrb,icl,ierr)
c        Tname = kname
c        CALL FRHDR(hname1,numdb,ncol,nrow,format1,nch,ierr)
c        nrw = ncol*nrow
c        CALL FRDAT(nread,QNX,nrw)
c        WRITE(CRMESS,*)'a#of words in title=',nread,',grid:',NX,'x',NY,'x',NZ
c        CALL RECMES(CRNAME,'I',CRMESS)
c        WRITE(CRMESS,*)'XYZmin=',Xmin,Ymin,Zmin,',DXYZ=',DX,DY,DZ,',Cm=',CMAX
c        CALL RECMES(CRNAME,'I',CRMESS)
c        Ngr=NX*NY*NZ*3
c        IF(Ngr.GT.Ng) THEN ! check limit of reserved arrays
c          WRITE(CRMESS,*)'Grid volume for Big Torus ',Ngr,' > ',Ng
c          CALL RECMES(CRNAME,'F',CRMESS)
c          RETURN
c        ENDIF
c        CALL FRHDR(hname2,numdb,ncol,nrow,format2,nch,ierr)
c        nrw = ncol*nrow
c        CALL FRDAT(nread,Q,nrw)
c        CALL FPARM('CLOSE BGRID')
c        CLOSE(LUNC)
c        CALL RESLUN(CRNAME,-LUNC,ierr)
c        NZ_i=NZ
C ------------------------------- End of intermediate read (When life is bad)
C
C Get grid values from data
        NX   = NX_d
        NY   = NY_d
        NZ   = NZ_d
        Xmin = Xmin_d
        Ymin = Ymin_d
        Zmin = Zmin_d
        DX   = DX_d
        DY   = DY_d
        DZ   = DZ_d
        Ngr = NX*NY*NZ*3
        IF (Ngr .GT. Ng) THEN
          WRITE(CRMESS,*)'Needs ',Ngr,', but only ',Ng,' is available'
          CALL RECMES(CRNAME,'I',CRMESS)
          RETURN
        ENDIF
        NX3   = NX*3
        NYNX3 = NY*NX3
        R(3) = Zmin - DZ
        NZ_l = NZ
        DO IZ = 1,NZ_l
          WRITE(CRMESS,*)'IZ=',IZ,', max=',NZ
          CALL RECMES(CRNAME,'I',CRMESS)
C ___ Begin of intermediate WRITING (When life is bad) _____________________
c          IF (MOD(IZ,27) .EQ. 1 .AND. IZ .NE. 1) THEN
c            NZ=IZ-1
c            CALL RESLUN(CRNAME,LUNC,ierr)
c            IF (ierr .EQ. -1) CALL RECMES(CRNAME,'F',
c     &        'unable to reserve fortran unit for intermediate writing')
c            WRITE(CRMESS,*) ' bgrid_int.fpk got LUN=', LUNC
c            CALL RECMES(CRNAME,'I',CRMESS)
c            IF (LUNC .LT. 10) WRITE(unit, 9) LUNC
c            IF (LUNC .GE. 10) WRITE(unit,10) LUNC
c            IF (IZ .EQ. 28) THEN
c              flnam='bgrid_27.fpk'
c            ELSEIF (IZ .EQ. 55) THEN
c              flnam='bgrid_54.fpk'
c            ELSEIF (IZ .EQ. 82) THEN
c              flnam='bgrid_81.fpk'
c            ELSEIF (IZ .EQ. 109) THEN
c              flnam='bgrid_108.fpk'
c            ELSEIF (IZ .EQ. 136) THEN
c              flnam='bgrid_135.fpk'
c            ENDIF
c            CALL FPARM(' OPEN BGRID ' // unit // 'FILE="' // flnam // '"'//
c     &           'RECL=36000 ACTION=WRITE ACCESS=SEQ STATUS=NEW FORM=BINARY')
cC
c            CALL GetUnixTime(dtime)
c            IF(MFM_MAG_type.EQ.1) THEN
c              kname = 'BTORU'
c              Cmax  = Imax_t
c            ELSEIF(MFM_MAG_type.EQ.2) THEN
c              kname = 'BMINT'
c              Cmax = Imax_m
c            ELSEIF(MFM_MAG_type.EQ.3) THEN
c              kname = 'BPLTG'
c              Cmax = Imax_p
c            ENDIF
c            CALL FWNAME('BGRID')
cC----------------- kname numra numrb icl
c            CALL FWKEY(kname, 9999,MFM_MAG_type, 11)
c            nrw = 10
cC----------------- hname1,numdb ncol nrow   format1
c            CALL FWHDR('BHDR',    1, nrw,   1,'3I,6F,3I')
c            CALL FWDAT(nrw,QNX)
c            nrw = Ngr
cC----------------- hname1,numdb ncol nrow   format2
c            CALL FWHDR('BGRD',    1, nrw,   1,   'F'    )
c            CALL FWDAT(nrw,Q)
c            CALL FWEND(ierr)
c            IF(ierr.NE.0) THEN
c              WRITE(CRMESS,*) ' *** bgrid_to_fpk error =',ierr
c              CALL RECMES(CRNAME,'I',CRMESS)
c            ENDIF
c            CALL FWEOD
c            CALL FPARM('CLOSE BGRID')
c            CLOSE(LUNC)
c            CALL RESLUN(CRNAME,-LUNC,ierr)
cC
c            WRITE(CRMESS,*)'Grid:',NX,NY,NZ,',XYZmin=',Xmin,Ymin,Zmin
c            CALL RECMES(CRNAME,'I',CRMESS)
c            WRITE(CRMESS,*)'DXYZ=',DX,DY,DZ,',C=',Cmax,',t=',dtime,',N=',Ngr
c            CALL RECMES(CRNAME,'I',CRMESS)
c            MFM_MAG_type = 0
c            CALL TIMEX(time_out)
c            time_exe = time_out - time_init
c            WRITE(CRMESS,*)'CPU time of calculation =',time_exe
c            CALL RECMES(CRNAME,'I',CRMESS)
c          ENDIF
cC
cC ___ End of intermediate writing (When life is bad) _______________________
          R(3) = R(3) + DZ
          R(2) = Ymin - DY
          DO IY = 1,NY
            R(2) = R(2) + DY
            R(1) = Xmin - DX
            DO IX = 1,NX
C              WRITE(CRMESS,*)'Z=',IZ,' /',NZ,',Y=',IY,' /',NY,',X=',IX,' /',NX
C              CALL RECMES(CRNAME,'I',CRMESS)
C-??
C              PRINT*,'Z=',IZ,' /',NZ,',Y=',IY,' /',NY,',X=',IX,' /',NX
C-??
              R(1) = R(1) + DX
              CALL MFM_FIELD(R,B)
              Indx = (IZ-1)*NYNX3 + (IY-1)*NX3 + (IX-1)*3 + 1
              CALL UCOPY(B,Q(Indx),3)
            ENDDO
          ENDDO
        ENDDO
C
C Open and write new table
C
        CALL RESLUN(CRNAME,LUNC,ierr)
        IF (ierr .EQ. -1) CALL RECMES(CRNAME,'F',
     &      'unable to reserve fortran unit for magnetic field writing')
        WRITE(CRMESS,*) ' bgrid.fpk got LUN=', LUNC
        CALL RECMES(CRNAME,'I',CRMESS)
        IF (LUNC .LT. 10) WRITE(unit, 9) LUNC
        IF (LUNC .GE. 10) WRITE(unit,10) LUNC
C
C        IF (MFM_MAG_type .EQ. 1) THEN
        IF (ipar_trk_magtyp .EQ. 1) THEN ! modified for TRK
          flnam=fl_bt
C        ELSEIF (MFM_MAG_type .EQ. 2) THEN
        ELSEIF (ipar_trk_magtyp .EQ. 2) THEN ! modified for TRK
          flnam=fl_mt
C        ELSEIF (MFM_MAG_type .EQ. 3) THEN
        ELSEIF (ipar_trk_magtyp .EQ. 3) THEN ! modified for TRK
          flnam=fl_pt
        ENDIF
        CALL FPARM(' OPEN BGRID ' // unit // 'FILE="' // flnam // '"'//
     &      'RECL=36000 ACTION=WRITE ACCESS=SEQ STATUS=NEW FORM=BINARY')
C
        CALL GetUnixTime(dtime)
C
C        IF(MFM_MAG_type.EQ.1) THEN
        IF(ipar_trk_magtyp.EQ.1) THEN ! modified for TRK
          kname = 'BTORU'
          Cmax  = Imax_t
C        ELSEIF(MFM_MAG_type.EQ.2) THEN
        ELSEIF(ipar_trk_magtyp.EQ.2) THEN ! modified for TRK
          kname = 'BMINT'
          Cmax = Imax_m
C        ELSEIF(MFM_MAG_type.EQ.3) THEN
        ELSEIF(ipar_trk_magtyp.EQ.3) THEN ! modified for TRK
          kname = 'BPLTG'
          Cmax = Imax_p
        ENDIF
C
        CALL FWNAME('BGRID')
C  numra < 9999  (run No.)
C  numrb = MFM_MAG_type (Big TORUS, Mini TORUS or Polar. Targ. magnetic field)
C    icl = 11   (clasification 11 - 20  offline analysis)
C----------------- kname numra numrb icl
C        CALL FWKEY(kname, 9999,MFM_MAG_type, 11) ! Mod. for TRK
        CALL FWKEY(kname, 9999,ipar_trk_magtyp, 11)
        nrw = 10
C----------------- hname1,numdb ncol nrow   format1
        CALL FWHDR('BHDR',    1, nrw,   1,'3I,6F,3I')
        CALL FWDAT(nrw,QNX)
        nrw = Ngr
C----------------- hname1,numdb ncol nrow   format2
        CALL FWHDR('BGRD',    1, nrw,   1,   'F'    )
        CALL FWDAT(nrw,Q)
        CALL FWEND(ierr)
        IF(ierr.NE.0) THEN
          WRITE(CRMESS,*) ' *** bgrid_to_fpk error =',ierr
          CALL RECMES(CRNAME,'I',CRMESS)
        ENDIF
        CALL FWEOD
        CALL FPARM('CLOSE BGRID')
        CLOSE(LUNC)
        CALL RESLUN(CRNAME,-LUNC,ierr)
C
        WRITE(CRMESS,*)'Grid:',NX,NY,NZ,',XYZmin=',Xmin,Ymin,Zmin
        CALL RECMES(CRNAME,'I',CRMESS)
        WRITE(CRMESS,*)'DXYZ=',DX,DY,DZ,',C=',Cmax,',t=',dtime,',N=',Ngr
        CALL RECMES(CRNAME,'I',CRMESS)
        ipar_trk_magtyp = 0
C        MFM_MAG_type = 0 ! Modified for TRK
        CALL TIMEX(time_out)
        time_exe = time_out - time_init
        WRITE(CRMESS,*)'CPU time of calculation =',time_exe
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
C
      RETURN
      END

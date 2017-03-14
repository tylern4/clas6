      SUBROUTINE sda_minit
*_begin_doc
*  RCS ID string
*  $Id: sda_minit.F,v 1.20 2008/03/16 04:33:25 fklein Exp $
*
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads or create a table of magnetic field.
C-
C-   Inputs  :
C-   Outputs : COMMON/sdabgrid/
C-   Controls:
C-
C-   Library belongs: libsda.a
C-
C-   Calls: sda_torus
C-
C-   Created   28-OCT-1990   Bogdan Niczyporuk
C-   Modified: 27-MAR-1997   Curtis A. Meyer
C-             Use tcl controlled variables to control the bfield file
C-             and its location.
C-   Modified: 28-MAR-1997   Curtis A. Meyer
C-             Major modification to include reading of both the torus
C-             and mini-torus fields. I have also added TCL variables
C-             to control what is taken as the minitorus field name.
C-   Modified: 24-JULY-1997 Joseph J. Manak
C-             Torus and MiniTorus current are now controlled by the mapmanager
C-             database if the appropriate TCL variable is not defined.
C-             14-
C-
C-   Modified: 12-Jan-1999 Mikhail V. Kossov
C-             MFM magnetic field is initialized for ipar_trk_bgridtyp = 2
C-
C-   Called by sda_init
C-
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
#include "bcs.inc"
#include "sdabgrid.inc"
*--c.a.meyer march 27 1997
#include "trktcl_var.inc"
*--end of change
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
      PARAMETER ( CFILE = '$RCSfile: sda_minit.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.20 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:25 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: sda_minit.F,v 1.20 2008/03/16 04:33:25 fklein Exp $
     3cmeyer'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='SDA_MINIT')
      PARAMETER (CRAUTH='Curtis A. Meyer')
*
*  Local User defined variables:
*  -----------------------------
*
C
C Local variables
C
      Integer Ilun,Iret
      Character*7 Bopen
*
      INTEGER NX3,NYNX3, IX,IY,IZ, NYc, Indx, nrw
      INTEGER numra,numrb,icl,ierr, numdb,ncol,nrow,nch, nread
      INTEGER irun, ievt, irec
      REAL time_init, time_out, time_exe
      REAL B(3),R(3), Wlen
      REAL Sbt,Smt, Cmax_t,Cmax_m
      REAL def_zpos_poltarg
      DATA def_zpos_poltarg / -57.5 /
C
      CHARACTER*24  dtimes
      CHARACTER*256 fname
      CHARACTER*12  format1
      CHARACTER*4   format2
      CHARACTER*8   kname,Tname,Mname, hname1,hname2
      INTEGER Max_set
      PARAMETER (Max_set=7)
      REAL torus_set(Max_set), torus_set_curr
      data torus_set/750.,965.,1500.,1920.,1930.,2250.,3375./
C
      Integer I
      Integer Typ
*
*     ---------------------------------------------------------
*
C ....M. Kossov correction starts here
C --- Select the grid type for the magnetic field
C
      IF(ipar_trk_bgridtyp.EQ.2) THEN ! Radial grid
        WRITE(6,*)'************ MFM is initialized ****************'
        CALL mfm_brun ! to be modified to correspond to the =1 case
      ELSE
C
C ... End of M. Kossov correction + ENDIF at the end
C
C Open and read old table
C
       IF(ipar_trk_magtyp.LE.0) THEN
*
*        Initialize just in case nothing has been read
*
         Tname = 'BTORU'
         Mname = 'BMINT'
         Cmax_t = float(Imax_t)
         Cmax_m = float(Imax_m)
         torus_set_curr=float(ipar_torus_current)
         Do i=1,Max_set
            If(abs(abs(ipar_torus_current)-torus_set(i)).lt.0.002*torus_set(i))then
               write(crmess,127)torus_set_curr,torus_set(i)
               call recmes(CRNAME,'L',crmess)
               torus_set_curr=ipar_torus_current*torus_set(i)/abs(ipar_torus_current)
            endif
         EndDo
*
*           Pick up the full file name 
*
         call revinm(spar_torus_bfield_loc,spar_torus_bfield_name,fname)
         call RecMes(CRNAME,'I',fname)
*
*           Reserve a logical unit number:
*
         Call ResLun('Sda_Minit',Ilun,Iret)
*
         Write(Bopen,'(A,I2.2)') 'UNIT=',Ilun
         CALL FPARM(' OPEN BGRID ' // Bopen // ' FILE="' // fname // '" ')
         CALL FRNAME('BGRID')
         CALL FRKEY(kname,numra,numrb,icl,ierr)
         Tname = kname
         CALL FRHDR(hname1,numdb,ncol,nrow,format1,nch,ierr)
         nrw = ncol*nrow
         CALL FRDAT(nread,NX,nrw)
         IF(Ngr.GT.Ng) THEN
            WRITE(CRMESS,100)Ngr,Ng
            CALL RecMes(CRNAME,'W',CRMESS)
            CALL FPARM('CLOSE BGRID')
            GO TO 999
         ENDIF
         If ( Cmax .gt. 0.0) Cmax_t = Cmax
         CALL FRHDR(hname2,numdb,ncol,nrow,format2,nch,ierr)
         nrw = ncol*nrow
         CALL FRDAT(nread,Q,nrw)
         CALL FPARM('CLOSE BGRID')
         Close(ilun)
*
         CALL GetASCIITime(dtime,dtimes)
         WRITE(CRMESS,101) kname,dtimes
         CALL RecMes(CRNAME,'I',CRMESS)
         Sbt = torus_set_curr/Cmax_t
         WRITE(CRMESS,106) torus_set_curr,Imax_t,Sbt
         CALL RecMes(CRNAME,'L',CRMESS)
C
C      Repeat the above procedure for pol.target OR minitorus field.
C
         if(ipar_poltarget_current.NE.0) then
*
            call revinm(spar_poltarget_bfield_loc,spar_poltarget_bfield_name,fname)
            call RecMes(CRNAME,'I',fname)
            CALL FPARM(' OPEN BGRID ' // Bopen // ' FILE="' // fname // '" ')
            CALL FRNAME('BGRID')
            CALL FRKEY(kname,numra,numrb,icl,ierr)
            Mname = kname
            CALL FRHDR(hname1,numdb,ncol,nrow,format1,nch,ierr)
            nrw = ncol*nrow
            CALL FRDAT(nread,MX,nrw)
            write(6,*)mx,my,mz,xpmin,ypmin,zpmin,ddx,ddy,ddz,Cpmax,mgr
            IF(Mgr.GT.Mg) THEN
               WRITE(6,100) Mgr, Mg
               CALL FPARM('CLOSE BGRID')
               GO TO 999
            ENDIF
            CALL FRHDR(hname2,numdb,ncol,nrow,format2,nch,ierr)
            nrw = ncol*nrow
            CALL FRDAT(nread,Qm,nrw)
            CALL FPARM('CLOSE BGRID')
            Close(ilun)
*
            CALL GetASCIITime(dptime,dtimes)
            WRITE(CRMESS,104) kname,dtimes
            CALL RecMes(CRNAME,'I',CRMESS)
            if(ipar_trk_magtyp.eq.0) then
               Smt = float(ipar_poltarget_current)/1000./float(Cpmax)
            else
               Smt = float(ipar_poltarget_current)/float(Cpmax)
            endif
            WRITE(CRMESS,126) int(Smt*Cpmax),Cpmax,Smt, ipar_trk_magtyp
            CALL RecMes(CRNAME,'L',CRMESS)
            DO i = 1,Mgr
               Qm(i) = Smt*Qm(i)
            ENDDO
            ipar_minitorus_current=0  !don't trust that the map entry is ok

            if(ipar_trk_magtyp.eq.0 .AND. dpar_TargetPos(3).EQ.0.0) then
              dpar_TargetPos(3)=def_zpos_poltarg
              write(crmess,'(A,F8.3)')'Target position (z) was not set: set default=',def_zpos_poltarg
              CALL RecMes(CRNAME,'L',crmess)
            endif
*
* FROST maps: ipar_trk_magtyp -4: racetrack (3-dim: read by sda_snake_dipole.F)
*             ipar_trk_magtyp -3: solenoid (2-dim: read by sda_snake_pt.F)
            if(ipar_trk_magtyp.eq.-3) ipar_trk_magtyp=0
*
         elseif(ipar_minitorus_current.GT.0) then
*
            call revinm(spar_minitorus_bfield_loc,spar_minitorus_bfield_name,fname)
            call RecMes(CRNAME,'I',fname)
C
            CALL FPARM(' OPEN BGRID ' // Bopen // ' FILE="' // fname // '" ')
            CALL FRNAME('BGRID')
            CALL FRKEY(kname,numra,numrb,icl,ierr)
            Mname = kname
            CALL FRHDR(hname1,numdb,ncol,nrow,format1,nch,ierr)
            nrw = ncol*nrow
            CALL FRDAT(nread,NX,nrw)
*
            IF(Ngr.GT.Ng) THEN
               WRITE(CRMESS,100) Ngr, Ng
               Call RecMes(CRNAME,'I',CRMESS)
               CALL FPARM('CLOSE BGRID')
               GO TO 999
            ENDIF
            CALL FRHDR(hname2,numdb,ncol,nrow,format2,nch,ierr)
            nrw = ncol*nrow
            CALL FRDAT(nread,Qm,nrw)
            CALL FPARM('CLOSE BGRID')
            Close(ilun)
*
            CALL GetASCIITime(dtime,dtimes)
            WRITE(CRMESS,104) kname,dtimes
            CALL RecMes(CRNAME,'I',CRMESS)
            If ( Cmax .gt. 0.0) Cmax_m = Cmax
            Smt = Float(ipar_minitorus_current)/Cmax_m
            WRITE(CRMESS,116) ipar_minitorus_current,Cmax,Smt
            CALL RecMes(CRNAME,'L',CRMESS)
         endif
         Call ResLun('Sda_Minit',-Ilun,Iret)
C
C            Rescale and Sum the B-fields
C
C
         CRMESS='>>>>>>>>>>>>'
         IF(ipar_torus_current.NE.0 .AND. ipar_minitorus_current.EQ.0) THEN
            write(CRMESS,'(A,F6.4,A,I10,A)')'***** scale Torus field by',sbt,'; read',Nrw,' grids'
            CALL RECMES(CRNAME,'I',CRMESS)
            DO i = 1,Ngr
               Q(i) = Sbt*Q(i)
            ENDDO
         ELSE IF(ipar_torus_current.EQ.0 .AND. ipar_minitorus_current.NE.0) THEN
            write(CRMESS,'(A,F8.4,A,I10,A)')'***** scale Mini Torus field by',smt,'; read',Nrw,' grids'
            CALL RECMES(CRNAME,'I',CRMESS)
            DO i = 1,Ngr
               Q(i) = Smt*Qm(i)
            ENDDO
         ELSE IF(ipar_torus_current.NE.0 .AND. ipar_minitorus_current.NE.0) THEN
            write(CRMESS,'(A,2F8.4,A,I10,A)')'***** scale Torus & Minitorus field by',sbt,smt,'; read',Nrw,' grids'
            CALL RECMES(CRNAME,'I',CRMESS)
            DO i = 1,Ng
               Q(i) = Sbt*Q(i) + Smt*Qm(i)
            ENDDO
         ELSEIF(ipar_poltarget_current.EQ.0) THEN
            ipar_trk_magtyp = 5
         ENDIF
c         WRITE(CRMESS,105) Sbt,Tname, Smt,Mname
c         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF
C
C  Calculate and Write Table (MAGN 'TORU' or 'MTOR' or 'TOPM,)
C
       IF(ipar_trk_magtyp.GE.1.AND.ipar_trk_magtyp.LE.3 .AND. dpar_bfield_grid(1).GT.0.D0) THEN
         CALL timex(time_init)
         write(6,*)'************* calculate Torus / Minitorus field'

C Get grid values from data card
         NX = dpar_bfield_grid(2)
         NY = dpar_bfield_grid(3)
         NZ = dpar_bfield_grid(4)
         Xmin = dpar_bfield_grid(5)
         Ymin = dpar_bfield_grid(6)
         Zmin = dpar_bfield_grid(7)
         DX = dpar_bfield_grid(8)
         DY = dpar_bfield_grid(9)
         DZ = dpar_bfield_grid(10)
         Ngr = NX*NY*NZ*3
         IF(Ngr.GT.Ng) THEN
           WRITE(6,100) Ngr, Ng
           GO TO 999
         ENDIF
         NX3   = NX*3
         NYNX3 = NY*NX3
C
         R(3) = Zmin - DZ
         DO 3 IZ = 1,NZ
           R(3) = R(3) + DZ
C
           R(2) = Ymin - DY
           DO 2 IY = 1,NY
             R(2) = R(2) + DY
C
             R(1) = Xmin - DX
             DO 1 IX = 1,NX
               R(1) = R(1) + DX
C
               Wlen = ABS(R(1))*0.57735 + 3.*DY
               NYc = INT(Wlen/DY + 0.1)
               IF(NYc.GT.NY) NYc = NY
C
               IF(IY.GT.NYc) THEN
                 B(1) = 0.
                 B(2) = 0.
                 B(3) = 0.
               ELSE
                 CALL sda_torus(R,B)
               ENDIF
C
               Indx = (IZ-1)*NYNX3 + (IY-1)*NX3 + (IX-1)*3 + 1
               CALL UCOPY(B,Q(Indx),3)
C
    1        CONTINUE
    2      CONTINUE
    3    CONTINUE
C
C Open and write new table
C
         CALL GetUnixTime(dtime)
         typ = ipar_trk_magtyp
         CALL FPARM(' OPEN BGRID UNIT=55 FILE="bgrid.fpk"'//
     1      'RECL=36000 ACTION=WRITE ACCESS=SEQ STATUS=NEW FORM=BINARY')                          
C
         IF(ipar_trk_magtyp.EQ.1) kname = 'BTORU'
         IF(ipar_trk_magtyp.EQ.2) kname = 'BMINT'
         IF(ipar_trk_magtyp.EQ.3) kname = 'BTOPM'
C
         CALL FWNAME('BGRID')
C  numra < 9999 (run No.)
C  numrb = typ  (only TORUS magnetic field)
C    icl = 11   (clasification 11 - 20  offline analysis)
C----------------- kname numra numrb icl
         CALL FWKEY(kname, 9999,  typ, 11)
         nrw = 12
C----------------- hname1,numdb ncol nrow   format1
         CALL FWHDR('BHDR',    1, nrw,   1,'3I,6F,3I')
         CALL FWDAT(nrw,NX)
         nrw = Ngr
C----------------- hname1,numdb ncol nrow   format2
         CALL FWHDR('BGRD',    1, nrw,   1,   'F'    )
         CALL FWDAT(nrw,Q)
         CALL FWEND(ierr)
         IF(ierr.NE.0) WRITE(6,*) ' *** bgrid_to_fpk error ***'
         CALL FWEOD
         CALL FPARM('CLOSE BGRID')
C
         WRITE(6,102) NX,NY,NZ,Xmin,Ymin,Zmin,DX,DY,DZ,typ,dtime,Ngr
C Use this Table if there are events to be analyzed (change initial MAGTYP)
         ipar_trk_magtyp = 0
         CALL timex(time_out)
         time_exe = time_out - time_init
         WRITE(6,103) time_exe
       ENDIF
      ENDIF ! end of M.Kossov 'IF-selection'
C
 100  FORMAT(/' sda_minit.F: Needs',I8,' but in "sdabgrid.inc" is ',I8)
 101  FORMAT( ' Read "bgrid_t.fpk"  file for ',A8,' created on  ',A24)
 102  FORMAT(/' B-table "bgrid.fpk" written '/3I3,1X,6F8.2,1X,I2,I11,I8)       
 103  FORMAT(/' CPU time for calulation of TABLE =', F10.3)
 104  FORMAT( ' Read "bgrid_m.fpk"  file for ',A8,' created on  ',A24)
 105  FORMAT( '       The B-field = ',F6.3,'x',A5,' + ',F6.3,'x',A5)      
 106  FORMAT( ' Set Torus Current ',F9.1,' File Value ',I8,
     &        ' Scale Value ',F12.3)
 116  FORMAT( ' Set Mini-torus Current ',I8,' File Value ',I8,
     &        ' Scale Value ',F12.3)
 126  FORMAT( ' Set PT magnet Current ',I5,' File Value ',I8,
     &        ' Scale Value ',F12.3,' type',I3)
 127           format( ' OVERRIDING input torus current of ',F9.1,' with standard value ',F9.1)
 107  FORMAT( ' Insufficient BOS Memory to read minitorus field ')
 108  FORMAT( ' Requesting a workbank with ',I9,'  WOrds ')
 109  FORMAT( ' Work bank created at address:',I9)
 110  FORMAT( ' Gargantuan Work Bank has been dropped ')
 33   FORMAT('Loaded Torus current of ',i4,'A from the calibration database for Run:',1x,i4,1x)
 34   FORMAT('Loaded Minitorus current of ',i4, 'A from the calibration database for Run:',1x,i4,1x)
C
 999  RETURN
 9999 WRITE(CRMESS,107)
      CALL RecMes(CRNAME,'I',CRMESS)
      GOTO 999
      END

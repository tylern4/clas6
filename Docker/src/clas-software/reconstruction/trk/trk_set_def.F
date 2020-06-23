      subroutine trk_set_def()
*_begin_doc
*  RCS ID string
*  $Id: trk_set_def.F,v 1.26 2008/05/13 13:43:11 fklein Exp $
*
*  Documentation for subroutine TRK_SET_DEF
*
*  Purpose:  all tcl variables for Tracking are described in 'trktcl_var.inc':
*            prefix:  ipar_trk_...  for INTEGER  variables
*                     dpar_trk_...  for DOUBLE   variables
*                     spar_trk_...  for STRING   variables
*
*  Author:         Joe Manak 
*  -------         TJNAF
*
*  Creation Date:  14 Jan, 1998
*  --------------
*
*  tcl-variable     fortran-v  purpose
*  ------------     ---------  -------
*  bfield_file_env  bfloc      environment variable to bfield location.
*                              Default: CLAS_PARMS
*  bfield_file_name bfname     Bfield file name.
*                              Default: bgrid_t.fpk
*  mfield_file_env  bmloc      environment variable to mini-field location.
*                              Default: CLAS_PARMS
*  mfield_file_name bmname     Mini field file name.
*                              Default: bgrid_m.fpk
*  torus_current    Itorus     Integer torus current.      Default=Map Database
*  mini_torus_current Imtorus  Integer mini-torus current. Default=Map Database
*  prlink_file_env  prloc      Environment for prlink file location
*                              Default: CLAS_PARMS
*  prlink_file_name prname     File name for the prlink file.
*                              Default: ' ' (determined wrt. magnetic field setting -> sda_dinit.F  (fklein))
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
#include "trktcl_var.inc"
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
      PARAMETER ( CFILE = '$RCSfile: trk_set_def.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.26 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/05/13 13:43:11 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_set_def.F,v 1.26 2008/05/13 13:43:11 fklein Exp $
     3cmeyer'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRKTCL_INIT')
      PARAMETER (CRAUTH='Joe Manak')
*
*  Local User defined variables:
*  -----------------------------
      Integer  i                ! Loop Index

*
c ----------------------------------------------------------------------------
c Set up reasonable "default" values for the tcl control variables 

      ipar_trk_magtyp=0         ! Selected type of the magnetic field 
                                ! (<=0:  take table of magnetic field )
                                ! (=-3:  take table of FROST solenoid field)
                                ! (=-4:  take table of FROST racetrack field)
                                ! (=1:  calculate field for torus )
                                ! (=2:  calculate field for mini torus )
                                ! (=3:  calculate field for torus + mini torus 0
                                ! (=4:  uniform field (along wire) of -3.0 kG )
                                ! (=5:  No magnetic field )
      ipar_trk_bgridtyp=1       ! (=0:  no magnetic field)
                                ! (=1:  SDA bgrid (euclidian))
                                ! (=2:  Mikhails bgrid (radial))
      dpar_bfield_grid(1)=0.0D0		! control: 1=calculate
      dpar_bfield_grid(2)=61.0D0	! Nx
      dpar_bfield_grid(3)=41.0D0	! Ny
      dpar_bfield_grid(4)=87.0D0	! Nz
      dpar_bfield_grid(5)=-7.0D0	! Xmin
      dpar_bfield_grid(6)=-6.0D0	! Ymin
      dpar_bfield_grid(7)=-350.0D0	! Zmin
      dpar_bfield_grid(8)=7.0D0		! Dx
      dpar_bfield_grid(9)=6.0D0		! Dy
      dpar_bfield_grid(10)=10.0D0	! Dz
C
C Currents now read from the map database ONLY -  if they are incorrect in
C the map you should fix the map - JM     ^^^^ better not
      ipar_torus_current = -9999
      ipar_minitorus_current = -9999
C
C polarized target current not in the map yet - but it will be...
C
      ipar_poltarget_current=-9999  !  current of the Polarized Target Coils
C
C The polarity of the torus is determined from the currents in the map
C      ipar_Bsign_torus=1        !  Polarity of Torus Magnet 
C
      dpar_targetLen=4.0D0          !  Length of TargetCell
      dpar_TargetPos(1)=0.0D0       !  Offset of Target Position  
      dpar_TargetPos(2)=0.0D0
      dpar_TargetPos(3)=-9999.0D0
      dpar_TargetMagPos(1)=0.0D0    !  Offset of Target Magnet Position  
      dpar_TargetMagPos(2)=0.0D0
      dpar_TargetMagPos(3)=-9999.0D0
      dpar_BeamOff(1)=-9999.0D0         !  Beam Position at Target: {x_Koord}
      dpar_BeamOff(2)=-9999.0D0         !  Beam Position at Target: {y_Koord}
      dpar_BeamAngle(1)=0.0D0       !  Beam Direction at Target: {Theta(x,z)}
      dpar_BeamAngle(2)=0.0D0       !  Beam Direction at Target: {Phi(y,z)}
      dpar_SigmaBeam=0.03D0         !  Sigma of BeamPosition

      spar_torus_bfield_name='bgrid_t.fpk' ! Name of the main torus bfield file.
      spar_torus_bfield_loc='CLAS_PARMS'    ! Environment variable (location of
                                           !   'spar_torus_bfield_name')
      spar_minitorus_bfield_name='bgrid_m.fpk' ! Name of the mini torus bfield file.
      spar_minitorus_bfield_loc='CLAS_PARMS'   ! Environment variable (location
                                               !    of 'spar_minitorus_bfield_name')
      spar_poltarget_bfield_name='bgrid_p.fpk' ! Name of the polar.target bfield file.
      spar_poltarget_bfield_loc='CLAS_PARMS'     ! Environment variable (location
                                                 !   of 'spar_poltarget_bfield_name')
      spar_prlink_loc='CLAS_PARMS'   ! Environment variable (location of 
                                     !   'spar_prlink_name')
      spar_prlink_name='junk'        ! prlink file
C
C The name of the prlink file is determined from the currents in the map
C if you want to use a different prlink file talk to Manak or Weygand
C

      ipar_trk_statistics=1      !(=1:  print statistics at End-of-Run)
                                 !(=2:  print out some statistics on the tracking
                                 !               for each event)
      ipar_trk_level=4           ! requested Level of Tracking:
                                 ! (=1:  only Segment&Cluster finding)
			         ! (=2:  Hit Based Tracking)
			         ! (=3:  Resolving LeftRight ambiguity)
			         ! (=4:  Time Based Tracking)
			         ! (=5:  Vertex reconstruction  (not yet implemented)
      ipar_trk_sector=0        ! Sector# to be analyzed    (=0: all sectors)
      ipar_trk_mintracks=1     ! min. #tracks in final states (i.e. charged multiplicity)
      ipar_trk_minhits(1)=2    ! min. #hits per track segment in SuperLayer 1..6
      do i=2,6
        ipar_trk_minhits(i)=3
      enddo
      ipar_trk_maxsegm=30    ! perform linking only if #segments in a Superlayer
	                     !  is less than 'ipar_trk_maxsegm'
      ipar_trk_startlink=1   ! Region where to start the pattern recognition 
                             !           (linking) procedure 
			     ! (=0:  Mikhail's JETNET routines (not yet implem.)
			     ! (=1:  Jim's linking routine: start with SL1)
			     ! (=2,3: Franz's routine: start with Reg.2 or 3)
      ipar_trk_minlinks=5    ! min. #track-segments (Superlayers) 
      ipar_trk_minlramb=5    ! min. #track-segments with resolved LeftRight-ambig.
      ipar_trk_electron_id=0 ! 'level 3': geometrical matching of EC*SC*CC
                             ! (=-1:  check for electron before tracking)
      ipar_trk_make_prlink=0 ! write new templates for prlink dictionary
                             ! (=0:  Read only (no update))
                             ! (=1:  Write only (1-st time: generate new templets)
                             ! (=2:  Read&Write (read and update templets))
      ipar_trk_prlink_param=0   !use all start parameters provided by prlink file
                                !(=0: use only momentum info =old prlink files)

      dpar_trk_prfit_chi2=30.D0    ! chi2 cut in pattern recogn. fit (level=2:HBT)
      dpar_trk_lrambfit_chi2=20.D0 ! chi2 cut in LR-amb. fit         (level=3)
      dpar_trk_tbtfit_chi2=50.D0   ! chi2 cut in final track fit  (level=4:TBT)
      dpar_trk_vx2fit_chi2=50.D0   ! chi2 cut in vertex fit for combination of 2 tracks
      dpar_trk_vxfit_chi2=50.D0    ! chi2 cut in vertex fit       (level=5)
      dpar_trk_zstep(1)=0.1D0   ! min. step size for trajectory propagation
      dpar_trk_zstep(2)=10.D0   ! max. step size for trajectory propagation
      dpar_trk_zangle=0.005D0   ! max. deflection angle per Step
      dpar_trk_Chi2diff=1.0     ! Chi2 Difference for convergence 
      dpar_trk_SCerr_scale=5.0  ! scale factor for SC hit errors in SCR bank  
      ipar_trk_multscat=0   ! Multiple Scattering 
                            !(=0: Multiple Scattering is OFF )
                            !(=1: Multiple Scattering is ON : He & Air<Reg.1 & DC)
                            !(=2: Multiple Scattering is ON : He & Air<Reg.1 & DC & ST counter)
      ipar_trk_maxiter=8    ! max.number of iterations (=0: no fit)
      ipar_trk_fitregion=0  ! perform track fit only in (parts of) DC
                            ! (without extrapolating to z-axis (beamline))
                            !(=1: DC region 1 only)  (bit 0)
                            !(=2: DC region 2 only)  (bit 1)
                            !(=4: DC region 3 only)  (bit 2)
                            !(=8: DC axial superlayers only)  (bit 3)
                            !(=7: fit in DC only (no extrapolation to 'vertex')
      ipar_trk_vxconstr=0   ! Vertex Constraints: 
                            !(=0: unconstrained)
                            !(=1: transverse constraint (see 'dpar_SigmaBeam')
                            !(=2: long. constraint (along the beam)(see 'dpar_TargetLen')
                            !(=3: both (along & transverse) constrained)
      ipar_trk_SCconstr=0   ! constraints by hit in SC   (1...3 same as trk_VXconstr)
      ipar_trk_calcTM=20     ! Flag for calculating the Transport Matrix
                            !(=0: not calculated [assume uniform magn.field along z-axis])
                            !(=N: Transport Matrix is calculated 
                            !     for the N first iterations)
      do i=1,8
        ipar_trk_print(i)=0 ! print on stdout (LOGUNIT=6) for debug purpose
      enddo
      ipar_trk_histo=0      ! fill tracking histograms (cf. ana package)
                            !(=1: on Hit Based Tracking)
                            !(=2: on Hit&Time Based Tracking)
                            !(=3: on Raw data & Hit Based Tracking)
      ipar_trk_ntuple=0     ! fill tracking ntuples    (cf. ana package)

      ipar_trk_region1=0  ! does not process region1 track reconstruction

      ipar_trk_lramb=1    ! check all 'good' combinations in final fit

      ipar_ntswitch=0     ! Switch to get new routines running.

      End




c      include file TRKTCL_VAR
c
c_begin_doc
c  RCS ID string
c  $Id: trktcl_var.inc,v 1.9 2004/12/08 15:24:57 avakian Exp $
c
c  Documentation for include file TRKTCL_VAR
c
c  Purpose:  holds the TRK control variables for communication with
c  --------  TCL script
*
*            prefix:  ipar_trk_...  for INTEGER  variables
*                     dpar_trk_...  for DOUBLE   variables
*                     spar_trk_...  for STRING   variables
c
c  Used in routines: allmost all TRK routines
c  ----------------
c
c
c  Author:   Curtis A. Meyer     Created:  Mon Jun 03 10:11:52 EST 1996
c  -------
c
c  Major revisions: 27 March 1997 -- C.A.Meyer
c  ---------------- Added variables to control the location and name
c                   of both the bfield and prlink files.
c		    Aug.97 -- F.Klein
c                   Nov.97 -- F.Klein
c                   remove obsolete 'sdakeys' COMMON (all variables changed!!)
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file TRKTCL_VAR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CITRKTCL_VAR
      DATA CITRKTCL_VAR/
     1'$rcsId$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in RUN_CONTROL common block
c  --------------------------------------------
c
c_end_var
c
c  DZCUT  Double Precision Version of  zcut for connection via tcl.
c  DZTFIT Double Precision Version of ztfit for connection via tcl.
c  DZDERR Double Precision Version of zderr for connection via tcl.
c  DZBEAM
c  DZTARG
c  DZSTEP
c  DZSCST
c  DZGEOM
c  DBscale  SCale factor for magnetic field.
c  DShxyz
c---------------------------------------------------------------------------
c  TCL_VARIABLE    (DEFAULT)       DESCRIPTION
c---------------------------------------------------------------------------
c
c   ipar_trk_magtyp  (def.=0)      Selected type of the magnetic field 
c                                  (=0:  take table of magnetic field )
c                                  (=1:  calculate field for torus )
c                                  (=2:  calculate field for mini torus )
c                                  (=3:  calculate field for torus + mini torus 0
c                                  (=4:  uniform field (along wire) of -3.0 kG )
c                                  (=5:  No magnetic field )
c  ipar_trk_bgridtyp (def.=1)      (=1:  SDA bfield representation (euclidian))
c                                  (=2:  Mikhail Kossovs bfield repres. (radial))
c  dpar_bfield_grid(10)            parameters for calculating the field tables
c  ipar_torus_current              current of the torus  (may be read from Map)
c                    (def.=-9999) 
c  ipar_minitorus_current          current of the mini torus (may be read from Map)
c                    (def.=-9999) 
c  ipar_poltarget_current          current of the Polarized Target Coils
c                    (def.=0) 
c  ipar_Bsign_torus  (def.=1)      Polarity of Torus Magnet 
c  dpar_targetLen    (def.=4.)     Length of TargetCell
c  dpar_TargetPos(3) (def.=0.,0.,0.)   Offset of Target Position  
c  dpar_BeamOff(2)   (def.=0.,0.)  Beam Position at Target: {x_Koord,y_Koord}
c  dpar_BeamAngle(2) (def.=0.,0.)  Beam Direction at Target: {Theta(x,z),Phi(y,z)}
c  dpar_SigmaBeam    (def.=0.03)   Sigma of BeamPosition
c
c  spar_torus_bfield_name   (def.='bgrid_t.fpk')  Name of the main torus bfield file.
c  spar_torus_bfield_loc    (def.='CLAS_PARMS')   Environment variable (location of 'spar_bfname')
c  spar_minitorus_bfield_name   (def.='bgrid_m.fpk')  Name of the mini torus bfield file.
c  spar_minitorus_bfield_loc    (def.='CLAS_PARMS')   Environment variable (location of 'spar_bmname')
c  spar_poltarget_bfield_name  (def.='bgrid_ptg.fpk')  Name of the polar.target bfield file.
c  spar_poltarget_bfield_loc   (def.='CLAS_PARMS')     Environment variable (location of 'spar_bptname')
c  spar_prlink_name   (def.=' ')            Name of the prlink file (may be determined by
c                   		        setting of ipar_Itorus&ipar_Imtorus)
c  spar_prlink_loc    (def.='CLAS_PARMS')   Environment variable (location of 'spar_prname')
c
c  ipar_trk_statistics            (=2:  print out some statistics on the tracking
c                 (def.=1)               for each event)
c                                 (=1:  print statistics at End-of-Run)
c  ipar_trk_level  (def.=4)       requested Level of Tracking:
c                                 (=1:  only Segment&Cluster finding)
c			          (=2:  Hit Based Tracking)
c			          (=3:  Resolving LeftRight ambiguity)
c			          (=4:  Time Based Tracking)
c			          (=5:  Vertex reconstruction  (not yet implemented)
c  ipar_trk_sector     (def.=0)   Sector# to be analyzed    (=0: all sectors)
c  ipar_trk_mintracks  (def.=1)   min. #tracks in final states (i.e. charged multiplicity)
c  ipar_trk_minhits(1...6)        min. #hits per track segment in SuperLayer 1..6
c	         (def.= 2,3,3,3,3,3)
c  ipar_trk_maxsegm 	          perform linking only if #segments in a Superlayer
c	         (def.=30)         is less than 'ipar_trk_maxsegm'
c  ipar_trk_startlink	          Region where to start the pattern recognition 
c                (def.=1)                           (linking) procedure 
c			          (=0:  Mikhail's JETNET routines)
c			          (=1:  Jim's linking routine: start with SL1)
c			          (=2,3: Franz's routine: start with Reg.2 or 3)
c  ipar_trk_minlinks   (def.=5)   min. #track-segments (Superlayers) 
c  ipar_trk_minlramb   (def.=5)   min. #track-segments with resolved LeftRight-ambig.
c  ipar_trk_electron_id (def.=0)  do geometrical matching of EC*CC*SC before trakcing
c                                 (=-1:  check for electron before tracking)
c  ipar_trk_make_prlink (def.=0)  write new templates for prlink dictionary
c                                 (=0:  Read only (no update))
c                                 (=1:  Write only (1-st time: generate new templets)
c                                 (=2:  Read&Write (read and update templets))
c  ipar_trk_prlink_param (def.=1) (=0 use only momentum info provided by prlink file
c                                 (=1:use all start parameters from prlink file
c                                 (note: old prlink files have only momentum info)
c
c  dpar_trk_prfit_chi2            chi2 cut in pattern recogn. fit      (level=2:HBT)
c  dpar_trk_lrambfit_chi2         chi2 cut in LR-amb. fit              (level=3)
c  dpar_trk_tbtfit_chi2           chi2 cut in final track fit          (level=4:TBT)
c  dpar_trk_vx2fit_chi2           chi2 cut in vertex fit for combination of 2 tracks
c  dpar_trk_vxfit_chi2            chi2 cut in vertex fit               (level=5)
c              (def. for chi2-cuts: 50.0)
c
C  dpar_trk_zstep(3)     - min.&max. step size for trajectory propagation
C  dpar_trk_zangle       - max. deflection angle per Step
C  dpar_trk_Chi2diff  (def.=0.1)  Chi2 Difference for convergence 
C  ipar_trk_multscat   (def.=0)   Multiple Scattering 
C                            (=0: Multiple Scattering is OFF )
C                            (=1: Multiple Scattering is ON : He & Air<Reg.1 & DC)
C                            (=2: Multiple Scattering is ON : He & Air<Reg.1 & DC & ST counter)
C  ipar_trk_maxiter   (def.=3)    Max.number of iterations (=0: no fit)
C  ipar_trk_vxconstr              Vertex Constraint: 
C                 (def.=0)   (=0: unconstrained)
C                            (=1: transverse constraint (see 'dpar_SigmaBeam')
C                            (=2: long. constraint (along the beam)(see 'dpar_TargetLen')
C                            (=3: both (along & transverse) constrained)
c  ipar_trk_SCconstr (def.=0)     Constraint on hit in SC
c                            (=1... 3) as for vertex constraint (sigma from SCR bank)
C  ipar_trk_calcTM  (def.=1)   Flag for calculating the Transport Matrix
C                            (=0: not calculated [assume uniform magn.field along z-axis])
C                            (=N: Transport Matrix is calculated 
C                                 for the N first iterations)
c  ipar_trk_print(8)         print on stdout (LOGUNIT=6) for debug purpose
c                            (1):
c                            (2):
c                            (3):=1:found segments&clusters (ana_segm.F)
c                            (4):=2:track timing (trk_dtime.F)
c                            (5):=1:LRamb.fit (ana_lramb.F)
c                            (6):
c                            (7):
c                            (8):
c  ipar_trk_histo  (def.=0)  fill tracking histograms (cf. ana package)
c                            (=1: on Hit Based Tracking)
c                            (=2: on Hit&Time Based Tracking)
c                            (=3: on Raw data & Hit Based Tracking)
c  ipar_trk_ntuple (def.=0)  fill tracking ntuples    (cf. ana package)
c                            (=1: on Hit Based Tracking)
c                            (=2: on Hit&Time Based Tracking)
c                            (=3: on Raw data & Hit Based Tracking)
c  ipar_trk_region1 (def=0)  reconstruct tracks in region 1 (cf region1.F)
c  ipar_trk_lramb   (def=1)  choice for left-right amb. fit routines  
c                            (=1:  check all good combinations in final fit)
c                            (=-1: take best local combination only)
c  ipar_reg1_use_dhcl(def=1) (=-1: use DHCL to make clusters
c                             =0 : use DC0 to make clusters)
c  ipar_reg1_min,ipar_reg1_max  min and max regions for region1
      Character*128    spar_torus_bfield_name, spar_torus_bfield_loc
      Character*128    spar_minitorus_bfield_name, spar_minitorus_bfield_loc
      Character*128    spar_poltarget_bfield_name, spar_poltarget_bfield_loc
      Character*128    spar_prlink_name, spar_prlink_loc
      real*8           dpar_bfield_grid(10)
      real*8           dpar_TargetPos(3), dpar_TargetLen
      real*8           dpar_TargetMagPos(3),dpar_TargetMagAng(3)
      real*8           dpar_SigmaBeam, dpar_BeamOff(2), dpar_BeamAngle(2)
      real*8           dpar_trk_prfit_chi2, dpar_trk_lrambfit_chi2
      real*8           dpar_trk_tbtfit_chi2
      real*8           dpar_trk_vx2fit_chi2, dpar_trk_vxfit_chi2
      real*8           dpar_trk_zstep(2), dpar_trk_zangle
      real*8           dpar_trk_Chi2diff,dpar_trk_SCerr_scale
      integer          ipar_trk_magtyp,ipar_trk_bgridtyp
      integer          ipar_torus_current, ipar_minitorus_current
      integer          ipar_poltarget_current
      integer          ipar_Bsign_torus
      integer          ipar_trk_statistics, ipar_trk_level
      integer          ipar_trk_sector,  ipar_trk_mintracks 
      integer          ipar_trk_minhits(6)
      integer          ipar_trk_maxsegm, ipar_trk_startlink
      integer          ipar_trk_minlinks, ipar_trk_minlramb
      integer          ipar_trk_electron_id
      integer          ipar_trk_make_prlink, ipar_trk_prlink_param
      integer          ipar_trk_multscat, ipar_trk_maxiter
      integer          ipar_trk_VXconstr, ipar_trk_SCconstr
      integer          ipar_trk_calcTM
      integer          ipar_trk_print(8)
      integer          ipar_trk_histo, ipar_trk_ntuple
      integer          ipar_trk_region1, ipar_trk_lramb, ipar_ntswitch
      integer          ipar_reg1_use_dhcl,ipar_reg1_min,ipar_reg1_max
      integer          ipar_trk_fitregion
*
cc  Dzcut,Dztfit,Dzbeam,Dztarg,Dzstep,
      Common /trktcl/ spar_torus_bfield_name, spar_torus_bfield_loc, 
     &           spar_minitorus_bfield_name, spar_minitorus_bfield_loc,
     &           spar_poltarget_bfield_name, spar_poltarget_bfield_loc, 
     &           spar_prlink_name, spar_prlink_loc,
     &           dpar_bfield_grid,
     &           dpar_TargetPos, dpar_TargetLen,
     &           dpar_TargetMagPos,dpar_TargetMagAng,
     &           dpar_SigmaBeam, dpar_BeamOff, dpar_BeamAngle,
     &           dpar_trk_prfit_chi2, dpar_trk_lrambfit_chi2,
     &           dpar_trk_tbtfit_chi2,
     &           dpar_trk_vx2fit_chi2, dpar_trk_vxfit_chi2,
     &           dpar_trk_zstep, dpar_trk_zangle,
     &           dpar_trk_Chi2diff,dpar_trk_SCerr_scale,
     &           ipar_trk_magtyp,ipar_trk_bgridtyp,
     &           ipar_torus_current, ipar_minitorus_current, 
     &           ipar_poltarget_current,
     &           ipar_Bsign_torus,
     &           ipar_trk_statistics, ipar_trk_level,
     &           ipar_trk_sector,  ipar_trk_mintracks,
     &           ipar_trk_minhits,
     &           ipar_trk_maxsegm, ipar_trk_startlink,
     &           ipar_trk_minlinks, ipar_trk_minlramb,
     &           ipar_trk_electron_id,
     &           ipar_trk_make_prlink,ipar_trk_prlink_param,
     &           ipar_trk_multscat, ipar_trk_maxiter,
     &           ipar_trk_VXconstr, ipar_trk_SCconstr, ipar_trk_calcTM,
     &           ipar_trk_print, ipar_trk_histo, ipar_trk_ntuple,
     &           ipar_trk_region1, ipar_trk_lramb, ipar_ntswitch,
     &           ipar_reg1_use_dhcl,ipar_reg1_min,ipar_reg1_max,
     &           ipar_trk_fitregion

      Save   /trktcl/
c
c----6----------------------------------------------------------------72








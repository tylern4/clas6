      character*10 cl_target
      REAL cl_beam_energy,cl_zpos,cl_zwidth
      REAL  cl_ymin,cl_ymax,cl_tmin,cl_tmax,cl_thmin,cl_thmax
      REAL  cl_ycol,cl_dmin,cl_dmax,cl_wmin
      REAL  cl_q2min,cl_q2max,cl_xmin,cl_xmax
      integer cl_verblev,cl_rand,cl_nprint
      integer cl_triggers,cl_pol,cl_gpd
      integer  cl_smear,cl_proloss
      real  cl_sma,cl_smb,cl_smc,cl_smd
      real  cl_sme,cl_smf,cl_smg
      logical dvcsgenOK,bosOK,ntOK,weightOK,cl_mom,cl_dvcs,cl_pi0
      logical bossmearOK
      character*60 bosout
      common /OUT_NAMES/ bosout
      common /clasdvcs1/ cl_verblev,cl_nprint,cl_proloss
      common /clasdvcs2/ cl_gpd,cl_target
      common /clasdvcs3/ cl_triggers,cl_beam_energy,cl_pol
      common /clasdvcs4/ cl_zpos,cl_zwidth
      common /clasdvcs5/ cl_ymin,cl_ymax,cl_tmin,cl_tmax,cl_thmin,cl_thmax
      common /clasdvcs6/ cl_ycol,cl_dmin,cl_dmax,cl_wmin
      common /clasdvcs7/ cl_q2min,cl_q2max,cl_xmin,cl_xmax
      common /clasdvcs8/ cl_smear,cl_sma,cl_smb,cl_smc,cl_smd
      common /clasdvcs9/ cl_sme,cl_smf,cl_smg
      common /dvcsgencont/ dvcsgenOK,bosOK,ntOK,weightOK,bossmearOK,
     6cl_mom,cl_dvcs,cl_pi0
C..
      double precision Mp, mele, pi,mpi0
      parameter (Mp=0.93827D0)
      parameter (mele=0.000511D0)
      parameter (pi=3.1415926536D0)
      parameter (mpi0=0.135)
C..
       double precision Ed,dsBH, dsDVCS, dsINT
       INTEGER Ipn,Ich
       COMMON/DVDAT2/Ed,Ipn,Ich
C..
        double precision smax
        common /smax/smax
C..
      integer heli,helpi
      double precision V3k2(3), V3gam(3), V3gam2(3), V3p2(3)
      COMMON/gendvcs/V3k2,V3gam,V3gam2,V3p2,heli,helpi
c
C..
        integer  Ifl, Ivar, IGPD
        common /igpd/ Ifl, Ivar, IGPD 



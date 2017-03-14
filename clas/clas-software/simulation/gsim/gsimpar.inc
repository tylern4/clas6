c  gsimpar.inc

c  gsim parameters, command line flags, env variables, default file names, etc.


c  $Id: gsimpar.inc,v 1.18 2009/12/22 15:01:48 natasha Exp $

      character*132 cigsimpar
      data cigsimpar/
     1'$Id: gsimpar.inc,v 1.18 2009/12/22 15:01:48 natasha Exp $'
     1/


c  misc limits
      integer max_vtx
      parameter (max_vtx=40)            !max number of vertices per event


c  CLAS environment variables
      character*(*) clas_root,clas_parms,clas_doc
      parameter (clas_root   = 'CLAS_ROOT')
      parameter (clas_parms  = 'CLAS_PARMS')
      parameter (clas_doc    = 'CLAS_DOC')


c  command line options, no corresponding env variable or default name
c   flags should be reasonably short, but still unique
      character*(*) gsim_noffread_flag,gsim_nofield_flag
      character*(*) gsim_nohits_flag,gsim_nodigi_flag
      character*(*) gsim_nobosout_flag,gsim_trig_flag
      character*(*) gsim_nosec_flag,gsim_nomate_flag
      character*(*) gsim_kine_flag,gsim_nogeom_flag
      character*(*) gsim_nodata_flag,gsim_nomcdata_flag
      character*(*) gsim_skip_flag, gsim_E0_flag
      character*(*) gsim_runno_flag

      parameter (gsim_noffread_flag = 'noff')
      parameter (gsim_nofield_flag  = 'nofi')
      parameter (gsim_nogeom_flag   = 'nog')
      parameter (gsim_nohits_flag   = 'noh')
      parameter (gsim_nodigi_flag   = 'nodi')
      parameter (gsim_nobosout_flag = 'nob')
      parameter (gsim_trig_flag     = 't')
      parameter (gsim_skip_flag     = 'sk')
      parameter (gsim_nosec_flag    = 'nos')
      parameter (gsim_nomate_flag   = 'noma')
      parameter (gsim_kine_flag     = 'k')
      parameter (gsim_E0_flag       = 'E0')
      parameter (gsim_nodata_flag   = 'noda')
      parameter (gsim_nomcdata_flag = 'nomc')
      parameter (gsim_runno_flag = 'R')

c  filename command line flags, have corresponding env variable and default name
      character*(*) gsim_geom_flag,gsim_mcin_flag,gsim_tgrid_flag,gsim_mgrid_flag
      character*(*) gsim_lizin_flag,gsim_ffread_flag,gsim_bosout_flag

c  JWP; 4/2/97 -- added gsim_lundin_flag
      character*(*) gsim_ecatten_flag,gsim_lundin_flag
      
      parameter (gsim_geom_flag    = 'ge')
      parameter (gsim_mcin_flag    = 'mc')
      parameter (gsim_tgrid_flag   = 'tg')
      parameter (gsim_mgrid_flag   = 'mg')
      parameter (gsim_lizin_flag   = 'li')
      parameter (gsim_ffread_flag  = 'ff')
      parameter (gsim_bosout_flag  = 'bo')
      parameter (gsim_ecatten_flag = 'ec')

c  JWP; 4/2/97 -- added gsim_lundin_flag
      parameter (gsim_lundin_flag  = 'lu')
      

c  environment variables (n.b. all upper case)
      character*(*) gsim_geom_env,gsim_mcin_env,gsim_tgrid_env,gsim_mgrid_env
      character*(*) gsim_lizin_env,gsim_ffread_env,gsim_bosout_env
      character*(*) gsim_ecatten_env,gsim_lundin_env

      parameter (gsim_geom_env    = 'GSIM_GEOM')
      parameter (gsim_mcin_env    = 'GSIM_MCIN')
      parameter (gsim_tgrid_env   = 'GSIM_TGRID')
      parameter (gsim_mgrid_env   = 'GSIM_MGRID')
      parameter (gsim_lizin_env   = 'GSIM_LIZIN')
      parameter (gsim_ffread_env  = 'GSIM_FFREAD')
      parameter (gsim_bosout_env  = 'GSIM_BOSOUT')
      parameter (gsim_ecatten_env = 'GSIM_ECATTEN')

c  JWP; 4/2/97 -- added gsim_lundin_env
      parameter (gsim_lundin_env  = 'GSIM_LUNDIN')
      
      
c  default file names
      character*(*) gsim_geom_name,gsim_mcin_name,gsim_bgrid_name
      character*(*) gsim_tgrid_name, gsim_mgrid_name, gsim_bgridptg_name
      character*(*) gsim_lizin_name,gsim_ffread_name,gsim_bosout_name

c  JWP; 4/2/97 -- added gsim_lundin_name
      character*(*) gsim_ecatten_name,gsim_lundin_name
      character*(*) gsim_help_filename
      
      parameter (gsim_geom_name     =	'gsimgeom.bfb')
      parameter (gsim_mcin_name     =	'mcin.evt')
      parameter (gsim_tgrid_name    =	'bgrid_t.fpk')
      parameter (gsim_mgrid_name    =	'bgrid_m.fpk')
      parameter (gsim_lizin_name    =	'lizin.txt')
      parameter (gsim_ffread_name   =	'ffread.txt')
      parameter (gsim_bosout_name   =	'gsimout.A00')
      parameter (gsim_ecatten_name  =	'ec_atten.dat')
      parameter (gsim_bgrid_name    =	'bgrid.fpk')
      parameter (gsim_bgridptg_name =   'bgrid_ptg.fpk')

c  JWP; 4/2/97 -- added gsim_lundin_name
      parameter (gsim_lundin_name   =	'lundin.dat')
      parameter (gsim_help_filename =	'gsim.hlp')
      

c  sub-detector indices (arbitrary) and names
      integer all,cc,dc,ec,ec1,sc,st,toru,mini,foil,ptg,tg,tg2,ic,sol,sh,other
      integer max_detector
      
      parameter (all   =  1)
      parameter (cc    =  2)
      parameter (dc    =  3)
      parameter (ec    =  4)
      parameter (ec1   =  5)
      parameter (sc    =  6)
      parameter (st    =  7)
      parameter (toru  =  8)
      parameter (mini  =  9)
      parameter (foil  =  10)
      parameter (ptg   =  11)
      parameter (tg    =  12)
      parameter (tg2 =  13)
      parameter (ic    =  14)
      parameter (sol   =  15)
      parameter (sh   =  16)
      parameter (other =  17)
      parameter (max_detector=17)
      
      character*4 det_name(max_detector)
      data det_name/'ALL' ,'CC'  ,'DC'  ,'EC'  ,'EC1' ,
     1              'SC'  ,'ST'  ,'TORU','MINI','FOIL',
     1              'PTG' ,'TG'  ,'TG2','IC','SOL','SH','OTHE'/




c  key names for BGRID file
      integer max_bgrid_key
      parameter (max_bgrid_key=6)
      character*8 mg_key(max_bgrid_key)/'BTORU   ','BMINT   ','BPOLT   ','BSOLN   ','FROSTSOL','FROSTRAC'/
c
c  for now 2 type of target magnets
c
      character*11 gsim_bgridtmg_name(4)/'bgrid_p.fpk','bgrid_s.fpk','frost_s.fpk','frost_r.fpk'/
      character*4 gsim_bgridtmg_flag(4)/'bptg','bsol','fsol','frac'/
      character*(*) gsim_bgridtmg_env
      parameter (gsim_bgridtmg_env   = 'GSIM_BGRIDTGMAGNET')



















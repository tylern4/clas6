c      include file FFPAR_BCKGR
c
c_begin_doc
c  RCS ID string
c  $Id: ffpar_bckgr.inc,v 1.2 1997/04/24 15:38:27 holtrop Exp $
c
c  Documentation for include file FFPAR_BCKGR
c
c  Purpose:
c  --------
C  Define the FFREAD cards for the clas_bckgr.F routine.
c
c  Used in routines:
c  ----------------
c  ffky_bckgr.F
c  clas_bckgr.F
c
c  Notes:
c  ------
c
c  Author:   Maurik Holtrop      Created:  Wed Jan 15 12:15:24 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file FFPAR_BCKGR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIFFPAR_BCKGR
      DATA CIFFPAR_BCKGR/
     1'$Id: ffpar_bckgr.inc,v 1.2 1997/04/24 15:38:27 holtrop Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in FFPAR_BCKGR common block
c  --------------------------------------------
c
c  ntarget: is for preconfigured background files. When set .ne. 0 
c           the values for zelem,atom, etc will be overwritten in the
c           first call to clas_bckgr
c
c  olumen:  The luminocity. eq. 1e32
c  zelem :  Average Atomic Z of target material.
c  atom  :  Average Atomic A of target material.
c  rho   :  Density of target. g/cm^3
c  tleng :  Target lenght, in cm
c  timedc:  Time scale, time duration of one event.  : 250E-9 sec
c  Nincel:  Number of events in cel for Normalizarion: 1000000


      real olumen,zelem,atom,rho,tleng,timedc
      integer nincel

      character*15 hbooknam(4)
      integer      ntarget
      real         xzelem(4),xatom(4),xrho(4),xtleng(4),xnincel(4)
c
c_end_var
c
      COMMON/FFPAR_BCKGR/
     +     ntarget,
     +     olumen, zelem, atom, rho, tleng, timedc, nincel
     +     
C
C Names of currently known files and corresponding parameters.
C
C Notice: The filenames can not be more than 12 characters total due
C to the limitation of some versions of the revinm routine in librecutl.a

      DATA HBOOKNAM/'h_76mm_g.hst','h_76mm_l.hst',
     +     'nh3_15mm.hst','cu_10um.hst' /
      DATA xzelem /1.   ,   1.,   10.   , 29.   /
      DATA xatom  /1.   ,   1.,   17.   , 63.55 /
      DATA xrho   /7.E-3,   7.E-2, 1.   ,  8.96 /
      DATA xtleng /7.6  ,   7.6  , 1.5  ,  1.E-5/
      DATA xnincel/100000000,10000000, 4000000,10000000/
c
c----6----------------------------------------------------------------72



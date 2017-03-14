      subroutine init_flags
        
c_begin_doc

c  Documentation for subroutine init_flags
      
c  $Id: init_flags.F,v 1.12 2011/04/04 16:52:26 hyonsuk Exp $
      
c  Purpose:
c  --------
c       initializes and checks control flags
      
c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 6-nov-95
      
c  Modified  by:
c  -------------
c     Angela Biselli,Dipartimento di fisica,Italy,8-Apr-97
c     biselli@infnge.ge.infn.it
c_end_doc
      
      implicit none
      integer irunno
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_flags.F,v $')
      parameter (crevis= '$Revision: 1.12 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2011/04/04 16:52:26 $')
      parameter (cautho= '$Author: hyonsuk $')
      parameter (crname= 'INIT_FLAGS')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     &     $Id: init_flags.F,v 1.12 2011/04/04 16:52:26 hyonsuk Exp $
     &     '/
      save
      
      
c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar.inc"
#include "ffpar_mg.inc"
#include "ffpar_ptg.inc"
#include "gckine.inc"
#include "gcflag.inc"
#include "gclist.inc"
c_end_inc
      
c  input/output variables:
c  -----------------------
      
c  local variables:
c  ----------------
      integer find_arg,arg,iucomp,det,iargc
      character*20 carg
      logical find_param
c_end_var
      
      
c  executable code:
c  ----------------
      

c   specify run number
      arg=find_arg(gsim_runno_flag)
      if((arg.gt.0).and.(iargc().ge.(arg+1)))then
         call getarg(arg+1,carg)
         read(carg,*)idrun
      endif

c   reset IKINE if "kine" flag present on command line,
c    or if "noffread" and kine input file specified on command line or with env var
      arg=find_arg(gsim_kine_flag)
      if((arg.gt.0).and.(iargc().ge.(arg+1)))then
         call getarg(arg+1,carg)
         read(carg,*)ikine
         
      elseif(noffread)then
         if(find_param(gsim_mcin_flag,gsim_mcin_env))then
            ikine=1
            call recmes(crname,'I',
     &           'noffread...resetting to KINE 1 (mcin)')
         elseif(find_param(gsim_lizin_flag,gsim_lizin_env))then
            ikine=2
            call recmes(crname,'I',
     &           'noffread...resetting to KINE 2 (lizin)')
         elseif(find_param(gsim_lundin_flag,gsim_lundin_env))then
            ikine=5
            call recmes(crname,'I',
     &           'noffread...resetting to KINE 5 (lundin)')
         endif
      endif
      
      write(6,*) ' init flags0'
c   set number of events to generate if "trig" flag set on command line
      arg=find_arg(gsim_trig_flag)
      if((arg.gt.0).and.(iargc().ge.(arg+1)))then
         call getarg(arg+1,carg)
         read(carg,*)nevent
      endif

c   set number of events to skip if "skip" flag set on command line
      arg=find_arg(gsim_skip_flag)
      if((arg.gt.0).and.(iargc().ge.(arg+1)))then
         call getarg(arg+1,carg)
         read(carg,*)skip_ff
      endif

c   read Beam Energy as used for photon production
      arg=find_arg(gsim_E0_flag)
      if((arg.gt.0).and.(iargc().ge.(arg+1)))then
         call getarg(arg+1,carg)
         read(carg,*)Beam_energy_ff(1)
      endif

c  set GSTMED field parameters
c  n.b.  nofield means no torus field, no ptg field, no mini-torus field, etc.
      nofield=nofield.or.find_arg(gsim_nofield_flag).ne.0

      if(nofield)then
         magtype=0
         ptg_fieldm=0.0
      endif
c$$$
c$$$      if(magtype.eq.0.and.(nogeom(ptg).or. ptg_fieldm.eq.0))then
c$$$         init_ifield=0
c$$$         init_fieldm=0.0
c$$$      endif
c$$$      if(ptg_fieldm.eq.0.)then
c$$$         ptg_ifield=0
c$$$      endif
c$$$      
      
c  check if iflgk is legal
      if((iflgk_ff.lt.0).or.(iflgk_ff.gt.1))then
         iflgk_ff=0
         call recmes(crname,'I','illegal iflgk...resetting to 0')
      endif
      
      
c  check for -nogeom flag or NOGEOM 'ALL' and set nogeom array
      call vfill(nogeom,   max_detector,
     &     (find_arg(gsim_nogeom_flag).ne.0).or.
     &     (iucomp(det_name(all),nogeom_ff,max_ff).ne.0))
      
c  the following geom is OFF by default
      nogeom(st)=.true.
      nogeom(ptg)=.true.
      nogeom(ic)=.true.
      nogeom(sol)=.true.
      nogeom(sh)=.true.
      
c  set remaining flags
      call vfill(nomate,   max_detector,.false.)
      call vfill(nosec,    max_detector,.false.)
      call vfill(nohits,   max_detector,.false.)
      call vfill(nodigi,   max_detector,.false.)
      call vfill(nobosout, max_detector,.false.)
      call vfill(nodata,   max_detector,.false.)
      call vfill(nomcdata, max_detector,.false.)
      
      
c  set remaining flags based on nogeom and and FFREAD cards...this is tricky...
      do det=1,max_detector
         nogeom(det)=(nogeom(det).or.
     &        (iucomp(det_name(det),nogeom_ff,max_ff).ne.0)).and.
     &        (iucomp(det_name(det),lgeom,ngeom).eq.0)
         nomate(det)=nomate(all).or.
     &        (iucomp(det_name(det),nomate_ff,max_ff).ne.0) .or.
     &        (find_arg(gsim_nomate_flag).ne.0).or.
     &        (nogeom(det).and.(det.ne.all))
         nosec(det)=nosec(all).or.
     &        (iucomp(det_name(det),nosec_ff,max_ff).ne.0) .or.
     &        (find_arg(gsim_nosec_flag).ne.0).or.
     &        (nogeom(det).and.(det.ne.all))
         nohits(det)=nohits(all).or.
     &        (iucomp(det_name(det),nohits_ff,max_ff).ne.0) .or.
     &        (find_arg(gsim_nohits_flag).ne.0).or.
     &        (nogeom(det).and.(det.ne.all))
         nodigi(det)=nodigi(all).or.
     &        (iucomp(det_name(det),nodigi_ff,max_ff).ne.0) .or.
     &         nohits(det) .or. (find_arg(gsim_nodigi_flag).ne.0)
         nobosout(det)=nobosout(all).or.
     &        (iucomp(det_name(det),nobosout_ff,max_ff).ne.0) .or.
     &        nohits(det) .or. (find_arg(gsim_nobosout_flag).ne.0)
         nodata(det)=nodata(all).or.nobosout(det).or.
     &        (iucomp(det_name(det),nodata_ff,max_ff).ne.0) .or.
     &        (find_arg(gsim_nodata_flag).ne.0)
         nomcdata(det)=nomcdata(all).or.nobosout(det).or.
     &        (iucomp(det_name(det),nomcdata_ff,max_ff).ne.0) .or.
     &        (find_arg(gsim_nomcdata_flag).ne.0)
         fast_code(det)=fast_code(all).or.fast_code(det).or.
     &        (iucomp(det_name(det),fast_code_ff,max_ff).ne.0) .or.
     &        (find_arg('fast').ne.0)
      enddo

      return
      end
        
c------------------------------------------------------------------------------








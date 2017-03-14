      subroutine beg_run_summary

c_begin_doc

c  Documentation for subroutine beg_run_summary

c  $Id: beg_run_summary.F,v 1.7 2011/03/31 19:52:57 fxgirod Exp $

c  Purpose:
c  --------
c     Prints begin run summary before event generation starts

c  Notes:
c  ------
c     uses ISWIT(2) to control part of printout...set appropriate digit non-zero
c       xxxxx1     call gpmate(0) to list all material parameters
c       xxxx1x     call gptmed(0) to list all tmed parameters
c       xxx1xx     call gpvolu(0) to list all tmed parameters

c  Author:
c  -------
c     Elliott Wolin, W&M, 3-aug-95

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: beg_run_summary.F,v $')
      parameter (crevis= '$Revision: 1.7 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2011/03/31 19:52:57 $')
      parameter (cautho= '$Author: fxgirod $')
      parameter (crname= 'BEG_RUN_SUMMARY')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: beg_run_summary.F,v 1.7 2011/03/31 19:52:57 fxgirod Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar.inc"
#include "bgrid.inc"
#include "ffpar_ptg.inc"
#include "ffpar_dc.inc"
#include "ffpar_mg.inc"
#include "ffpar_ec.inc"
#include "gcbank.inc"
#include "gcnum.inc"
#include "gcflag.inc"
#include "gckine.inc"
#include "gctrak.inc"
#include "gctmed.inc"
#include "gcphys.inc"
#include "gcopti.inc"
#include "gccuts.inc"
#include "gcmulo.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      
c  local variables:
c  ----------------
      integer i
      character*2 maxdet
      character*13 val
      character*150 crmess
c_end_var


c  executable code:
c  ----------------

      write(maxdet,'(i2)')max_detector

      call recmes(crname,'I','*')
      call recmes(crname,'I',
     1     '*                                                       *')
      call recmes(crname,'I',
     1     '**************** GSIM Begin Run Summary *****************')
      call recmes(crname,'I',
     1     '*                                                       *')


      write(crmess,'(''Beginning random number seeds are: '',
     1     i13,2x,i13)')iq(jrung+19),iq(jrung+20)
      call recmes(crname,'I',crmess)
      call recmes(crname,'I','*')

      if(noffread)call recmes(crname,'I',
     1     'NOFFREAD specified on command line' //
     1     '...FFREAD cards NOT read in')

      if(nofield)call recmes(crname,'I',
     1     'NOFIELD specified...no B field used')

      if(helium_only)call recmes(crname,'I',
     1     'HELIUM specified...helium used instead of air')

      call recmes(crname,'I','*')

      write(val,'(i13)')nevent
      call recmes(crname,'I',
     1     'Number of events attempting to generate: '//val)

      call recmes(crname,'I','*')

      write(val,'(i13)')skip_ff
      call recmes(crname,'I',
     1     'Number of events to skip at beginning of file: '//val)

      call recmes(crname,'I','*')

      write(val,'(i13)')ikine
      call recmes(crname,'I',
     1     'Starting run with KINE:                  '//val)

      if(ikine.eq.3)then
         do i=1,7
            write(crmess,
     1 '(''pkine('',i1,'') is:                             '',f13.2)')
     1           i,pkine(i)
            call recmes(crname,'I',crmess)
         enddo
      endif

      call recmes(crname,'I','*')

      write(val,'(i13)')magtype
      call recmes(crname,'I',
     1     'Starting run with MAGTYPE:               '//val)

      if(magtype.eq.1)then
         write(val,'(f13.2)')torus_field
         call recmes(crname,'I',
     1     'Starting run with torus field:           '//val)
      endif

      write(val,'(f13.2)')magscale(1)
      call recmes(crname,'I',
     1     'Starting run with torus MAGSCALE:        '//val)

      write(val,'(f13.2)')magscale(2)
      call recmes(crname,'I',
     1     'Starting run with mini MAGSCALE:         '//val)

      write(val,'(i13)')init_ifield
      call recmes(crname,'I',
     1     'Starting run with IFIELD:                '//val)

      write(val,'(f13.2)')init_fieldm
      call recmes(crname,'I',
     1     'Starting run with FIELDM:                '//val)

      call recmes(crname,'I','*')

      write(val,'(i13)')iflgk_ff
      call recmes(crname,'I',
     1     'Starting run with IFLGK:                 '//val)

      write(val,'(i13)')nstepmax_ff
      call recmes(crname,'I',
     1     'Starting run with NSTEPMAX:              '//val)

      write(val,'(f13.2)')slengmax_ff
      call recmes(crname,'I',
     1     'Starting run with SLENGMAX:              '//val)

      write(val,'(f13.2)')rmax_ff
      call recmes(crname,'I',
     1     'Starting run with RMAX:                  '//val)

      write(val,'(f13.2)')zmin_ff
      call recmes(crname,'I',
     1     'Starting run with ZMIN:                  '//val)

      write(val,'(f13.2)')zmax_ff
      call recmes(crname,'I',
     1     'Starting run with ZMAX:                  '//val)

      call recmes(crname,'I','*')

      write(val,'(f13.2)')dc_dist_to_time
      call recmes(crname,'I',
     1     'Starting run with DC_DIST_TO_TIME:       '//val)

      do i=1,6
         write(crmess,
     1'(''dc_resolution('',i1,'') is:                      '',f12.3)')
     1        i,dc_resolution(i)
         call recmes(crname,'I',crmess)
      enddo
      
      call recmes(crname,'I','*')

      write(val,'(f13.2)')atlen
      call recmes(crname,'I',
     1     'Starting run with ATLEN:                 '//val)

      write(val,'(f13.2)')poiss
      call recmes(crname,'I',
     1     'Starting run with POISS:                 '//val)

      write(val,'(i13)')ec_tdc_to_channel
      call recmes(crname,'I',
     1     'Starting run with EC_TDC_TO_CHANNEL:     '//val)

      call recmes(crname,'I','*')

      write(val,'(l13)')helium_only
      call recmes(crname,'I',
     1     'Starting run with HELIUM:                '//val)

      call recmes(crname,'I','*')

      write(val,'(f13.2)')ptg_angle(1)
      call recmes(crname,'I',
     1     'Starting run with PTG_ANGLE(1):             '//val)

      write(val,'(f13.2)')ptg_angle(2)
      call recmes(crname,'I',
     1     'Starting run with PTG_ANGLE(2):             '//val)

      write(val,'(f13.2)')ptg_angle(3)
      call recmes(crname,'I',
     1     'Starting run with PTG_ANGLE(3):             '//val)

      write(val,'(i13)')ptg_ifield
      call recmes(crname,'I',
     1     'Starting run with PTG_IFIELD:            '//val)

      write(val,'(f13.2)')ptg_fieldm
      call recmes(crname,'I',
     1     'Starting run with PTG_FIELDM:            '//val)

      write(val,'(f13.2)')ptg_scale
      call recmes(crname,'I',
     1     'Starting run with PTG_SCALE:             '//val)

      call recmes(crname,'I','*')

      write(val,'(i13)')ioptim
      call recmes(crname,'I',
     1     'Starting run with IOPTI:                 '//val)

      write(val,'(i13)')nrndm(1)
      call recmes(crname,'I',
     1     'Starting run with NRNDM(1):              '//val)

      write(val,'(i13)')nrndm(2)
      call recmes(crname,'I',
     1     'Starting run with NRNDM(2):              '//val)

      write(val,'(i13)')idrun
      call recmes(crname,'I',
     1     'Starting run with IDRUN:                 '//val)

      write(val,'(i13)')idevt
      call recmes(crname,'I',
     1     'Starting run with IDEVT:                 '//val)

      write(val,'(f13.4)')noacc_ff(1)
      call recmes(crname,'I',
     1     'Starting run with noacc_ff(1):           '//val)

      write(val,'(f13.4)')noacc_ff(2)
      call recmes(crname,'I',
     1     'Starting run with noacc_ff(2):           '//val)

      write(val,'(f13.4)')noacc_ff(3)
      call recmes(crname,'I',
     1     'Starting run with noacc_ff(3):           '//val)

      call recmes(crname,'I','*')

      write(val,'(i13)')ianni
      call recmes(crname,'I',
     1     'Starting run with IANNI:                 '//val)

      write(val,'(i13)')igauto
      call recmes(crname,'I',
     1     'Starting run with IGAUTO:                '//val)

      write(val,'(i13)')ibrem
      call recmes(crname,'I',
     1     'Starting run with IBREM:                 '//val)

      write(val,'(i13)')itckov
      call recmes(crname,'I',
     1     'Starting run with ICKOV:                 '//val)

      write(val,'(i13)')icomp
      call recmes(crname,'I',
     1     'Starting run with ICOMP:                 '//val)

      write(val,'(i13)')idcay
      call recmes(crname,'I',
     1     'Starting run with IDCAY:                 '//val)

      write(val,'(i13)')idray
      call recmes(crname,'I',
     1     'Starting run with IDRAY:                 '//val)

      call recmes(crname,'I','*')

      write(val,'(1pe13.2)')cutgam
      call recmes(crname,'I',
     1     'Starting run with CUTGAM:                '//val)

      write(val,'(1pe13.2)')cutele
      call recmes(crname,'I',
     1     'Starting run with CUTELE:                '//val)

      write(val,'(1pe13.2)')cutneu
      call recmes(crname,'I',
     1     'Starting run with CUTNEU:                '//val)

      write(val,'(1pe13.2)')cuthad
      call recmes(crname,'I',
     1     'Starting run with CUTHAD:                '//val)

      write(val,'(1pe13.2)')cutmuo
      call recmes(crname,'I',
     1     'Starting run with CUTMUO:                '//val)

      write(val,'(1pe13.2)')bcute
      call recmes(crname,'I',
     1     'Starting run with BCUTE:                 '//val)

      write(val,'(1pe13.2)')bcutm
      call recmes(crname,'I',
     1     'Starting run with BCUTM:                 '//val)

      write(val,'(1pe13.2)')dcute
      call recmes(crname,'I',
     1     'Starting run with DCUTE:                 '//val)

      write(val,'(1pe13.2)')dcutm
      call recmes(crname,'I',
     1     'Starting run with DCUTM:                 '//val)

      write(val,'(1pe13.2)')ppcutm
      call recmes(crname,'I',
     1     'Starting run with PPCUTM:                '//val)

      write(val,'(1pe13.2)')tofmax
      call recmes(crname,'I',
     1     'Starting run with TOFMAX:                '//val)

      call recmes(crname,'I','*')

      write(val,'(1pe13.2)')gcuts(1)
      call recmes(crname,'I',
     1     'Starting run with GCUTS(1):              '//val)

      write(val,'(1pe13.2)')gcuts(2)
      call recmes(crname,'I',
     1     'Starting run with GCUTS(2):              '//val)

      write(val,'(1pe13.2)')gcuts(3)
      call recmes(crname,'I',
     1     'Starting run with GCUTS(3):              '//val)

      write(val,'(1pe13.2)')gcuts(4)
      call recmes(crname,'I',
     1     'Starting run with GCUTS(4):              '//val)

      write(val,'(1pe13.2)')gcuts(5)
      call recmes(crname,'I',
     1     'Starting run with GCUTS(5):              '//val)

      call recmes(crname,'I','*')

      write(val,'(1pe13.2)')ekmin
      call recmes(crname,'I',
     1     'Starting run with EKMIN:                 '//val)

      write(val,'(1pe13.2)')ekmax
      call recmes(crname,'I',
     1     'Starting run with EKMAX:                 '//val)

      write(val,'(i13)')nekbin
      call recmes(crname,'I',
     1     'Starting run with NEKBIN:                '//val)

      call recmes(crname,'I','*')

      write(val,'(i13)')ihadr
      call recmes(crname,'I',
     1     'Starting run with IHADR:                 '//val)

      write(val,'(i13)')ilabs
      call recmes(crname,'I',
     1     'Starting run with ILABS:                 '//val)

      write(val,'(i13)')iloss
      call recmes(crname,'I',
     1     'Starting run with ILOSS:                 '//val)

      write(val,'(i13)')imuls
      call recmes(crname,'I',
     1     'Starting run with IMULS:                 '//val)

      write(val,'(i13)')imunu
      call recmes(crname,'I',
     1     'Starting run with IMUNU:                 '//val)

      write(val,'(i13)')ipair
      call recmes(crname,'I',
     1     'Starting run with IPAIR:                 '//val)

      write(val,'(i13)')ipfis
      call recmes(crname,'I',
     1     'Starting run with IPFIS:                 '//val)

      write(val,'(i13)')iphot
      call recmes(crname,'I',
     1     'Starting run with IPHOT:                 '//val)

      write(val,'(i13)')irayl
      call recmes(crname,'I',
     1     'Starting run with IRAYL:                 '//val)

      write(val,'(i13)')istra
      call recmes(crname,'I',
     1     'Starting run with ISTRA:                 '//val)

      write(val,'(i13)')isync
      call recmes(crname,'I',
     1     'Starting run with ISYNC:                 '//val)


      call recmes(crname,'I','*')
      call recmes(crname,'I','*')


      call recmes(crname,'I',
     1     'Detector control parameters:')
      call recmes(crname,'I','*')

      write(crmess,'(''flag'',8x,'//maxdet//'(a4,1x))')
     1     (det_name(i),i=1,max_detector)
      call recmes(crname,'I',crmess)
      write(crmess,'(''----'',8x,'//maxdet//'(a4,1x))')
     1     ('----',i=1,max_detector)
      call recmes(crname,'I',crmess)

      write(crmess,'(''nogeom      '','//maxdet//'(l1,4x))')
     1     (nogeom(i),i=1,max_detector)
      call recmes(crname,'I',crmess)

      write(crmess,'(''nomate      '','//maxdet//'(l1,4x))')
     1     (nomate(i),i=1,max_detector)
      call recmes(crname,'I',crmess)

      write(crmess,'(''nosec       '','//maxdet//'(l1,4x))')
     1     (nosec(i),i=1,max_detector)
      call recmes(crname,'I',crmess)

      write(crmess,'(''nohits      '','//maxdet//'(l1,4x))')
     1     (nohits(i),i=1,max_detector)
      call recmes(crname,'I',crmess)

      write(crmess,'(''nodigi      '','//maxdet//'(l1,4x))')
     1     (nodigi(i),i=1,max_detector)
      call recmes(crname,'I',crmess)

      write(crmess,'(''nobosout    '','//maxdet//'(l1,4x))')
     1     (nobosout(i),i=1,max_detector)
      call recmes(crname,'I',crmess)
 
      write(crmess,'(''nodata      '','//maxdet//'(l1,4x))')
     1     (nodata(i),i=1,max_detector)
      call recmes(crname,'I',crmess)
 
      write(crmess,'(''nomcdata    '','//maxdet//'(l1,4x))')
     1     (nomcdata(i),i=1,max_detector)
      call recmes(crname,'I',crmess)
 
      write(crmess,'(''fast code   '','//maxdet//'(l1,4x))')
     1     (fast_code(i),i=1,max_detector)
      call recmes(crname,'I',crmess)

      call recmes(crname,'I','*')
      call recmes(crname,'I',
     1     '*                                                       *')
      call recmes(crname,'I',
     1     '*********************************************************')
      call recmes(crname,'I',
     1     '*                                                       *')


c  dump various info, as requested with ISWIT(2)
      if(mod(iswit(2),10     )        .ne.0)call gpmate(0)
      if(mod(iswit(2),100    )/10     .ne.0)call gptmed(0)
      if(mod(iswit(2),1000   )/100    .ne.0)call gpvolu(0)
      if(mod(iswit(2),10000  )/1000   .ne.0)call gpsets('*','*')

      return
      end

c---------------------------------------------------------------------------------









      subroutine init_mg
c_begin_doc

c  Documentation for subroutine init_mg

c  $Id: init_mg.F,v 1.14 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c     initialized magnetic field, incl reading in B grid data if needed
c     also for ptg_fld if needed
c  Notes:
c  ------
c     mini-torus not working yet....

c  magtype:
c     0       no torus or mini field
c     1       simple analytic torus
c     2       torus from lookup table
c     3       torus + mini from lookup table
c     4       mini-torus only from lookup table


c  Author:
c  -------
c     based on Bogdan's original magn_init.F

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 30-jan-95
c  Modified  by:
c  -------------
c     Angela Biselli,Dipartimento di fisica,Italy,8-Apr-97
c     biselli@infnge.ge.infn.it
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: init_mg.F,v $')
      parameter (crevis= '$Revision: 1.14 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'INIT_MG')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: init_mg.F,v 1.14 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "bgrid.inc"
#include "ffpar_mg.inc"
#include "ffpar_ptg.inc"
c_end_inc

c  local variables:
c  ----------------
      integer unit,iret,lenocc,flen,iflag,nread,key,ikey,nwr
      integer numra,numrb,icl,ierr,numdb,ncol,nrow,nch
      integer c_def_torus, c_def_mini, c_def_ptg
      data  c_def_torus, c_def_mini, c_def_ptg  /3860,8000,124/ 
      logical ptg_flg
      character*20 kname,hname,format
      character*120 filename
      character*200 crmess
      character*5 cunit
      real qtemp(max_grid)
c_end_var


c  executable code:
c  ----------------

c  is field requested
      if(nofield.or.(magtype.eq.0.and.ptg_ifield.eq.0))then
         call recmes(crname,'I',
     1        'nofield or magtype=0 specified...no magnetic field used')
         return
      endif

c  stop if field choice illegal
      if(magtype.gt.4)then
         write(crmess,'(''?illegal magtype: '',i3)')magtype
         call recmes(crname,'F',crmess)
      endif

c  warn about inconsistencies...stop if illegal
c  force geometry to be set consistently
      if(ptg_ifield.gt.0 .and. magtype.ge.3 )
     1     call recmes(crname,'F',
     1     '?ptg field and mini-torus field both specified')

      if(nogeom(toru).and.((magtype.ge.1).and.(magtype.le.3))) then
         call recmes(crname,'W',
     1'*** torus field used without torus geometry: torus geometry set ***')
         nogeom(toru)=.false.
      endif
c
      if(nogeom(mini).and.(magtype.ge.3)) then
         call recmes(crname,'W',
     1'*** mini-torus field used without mini-torus geometry: geometry set ***')
         nogeom(mini)=.false.
      endif
c          
      if(.NOT.nogeom(st) .AND. .NOT.(nogeom(ptg).AND.nogeom(mini))) then
         call recmes(crname,'W',
     1 '*** start counter geometry and ptg or mini-torus field specified ***')
         if(magtype.ge.3 .or. (ptg_ifield.gt.0.and.ptg_ifield.lt.3)) then
            call recmes(crname,'W','*** changed to NO start counter geometry ***')
            nogeom(st)=.TRUE.
         else
            call recmes(crname,'W','*** changed to NO ptg and mini-torus geometry ***')
            nogeom(mini)=.TRUE.
            nogeom(ptg)=.TRUE.
         endif
      endif
      if(.NOT.(nogeom(ptg).or.nogeom(mini))) then
         call recmes(crname,'W',
     1     '*** ptg geometry and mini-torus geometry both specified ***')
         if(ptg_ifield.gt.0) then
            call recmes(crname,'W','*** changed to NO mini-torus geometry ***')
            nogeom(mini)=.TRUE.
         else
            call recmes(crname,'W','*** changed to NO ptg geometry ***')
            nogeom(ptg)=.TRUE.
         endif
      endif

      call recmes(crname,'I',
     1     '*** Starting to set up magnetic field ***')
      write(crmess,'(''Magtype used is: '',i3)')magtype
      call recmes(crname,'I',crmess)
      write(crmess,'(''Ptg_ifield value is: '',i3)')ptg_ifield
      call recmes(crname,'I',crmess)


c  read in bgrid if needed
      ptg_flg=magtype.lt.3.and.ptg_ifield.gt.0.and.ptg_ifield.le.4  ! add new target magnets
      if(magtype.ge.0)then
         call vzero(Qptg,max_grid_ptg)
         call vzero(qfield,max_grid)
         do key=1,3
           filename='empty'
         if(key.eq.1.and.(magtype.eq.2 .or. magtype.eq.3))then
           call get_param(gsim_tgrid_flag,gsim_tgrid_env,filename,iflag)
           if(iflag.lt.0)then
             call revinm(clas_parms,gsim_tgrid_name,filename)
           elseif(iflag.eq.1)then
             call recmes(crname,'I',
     1           'getting Torus grid filename from command line flag: -' //
     2           gsim_tgrid_flag)
           else
             call recmes(crname,'I',
     1           'getting Torus grid filename from env variable: ' //
     2           gsim_tgrid_env)
           endif
         elseif(key.eq.2.and.magtype.ge.3)then
           call get_param(gsim_mgrid_flag,gsim_mgrid_env,filename,iflag)
           if(iflag.lt.0)then
             call revinm(clas_parms,gsim_mgrid_name,filename)
           elseif(iflag.eq.1)then
             call recmes(crname,'I',
     1           'getting MiniTorus grid filename from command line flag: -' //
     2           gsim_mgrid_flag)
           else
             call recmes(crname,'I',
     1           'getting MiniTorus grid filename from env variable: ' //
     2           gsim_mgrid_env)
           endif
         elseif(key.eq.3.and.ptg_flg) then
           call get_param(gsim_bgridtmg_flag(ptg_ifield),
     6                    gsim_bgridtmg_env,filename,iflag)
           if(iflag.lt.0)then
             call revinm(clas_parms,gsim_bgridtmg_name(ptg_ifield),filename)
           elseif(iflag.eq.1)then
             call recmes(crname,'I',
     1           'getting PolTarget grid filename from command line flag: -' //
     2           gsim_bgridtmg_flag(ptg_ifield))
           else
             call recmes(crname,'I',
     1           'getting PolTarget grid filename from env variable: ' //
     2           gsim_bgridtmg_env)
           endif
         endif
         if(filename.ne.'empty')then
         flen=lenocc(filename)
c  get bgrid file unit
         call reslun(crname,unit,iret)
         if(iret.le.0)call recmes(crname,'F',
     1        '?unable to get fortran unit')
         write(cunit,'(i5)')unit

c  open fpack file
         call recmes(crname,'I',
     1        'reading in magnetic field map from ' // filename)

         call fparm(
     1        ' OPEN BGRID'//
     1        ' UNIT='//cunit//
     1        ' FILE="' // filename(1:flen) // '"' //
     1        ' RECL=32760'//
     1        ' ACTION=READ STATUS=OLD FORM=BINARY')
         
         call fermes(crmess,1)
         if(lenocc(crmess).gt.0)call recmes(crname,'F',
     1        '?FPARM: '//crmess)
         
         call frname('BGRID')


c  read through grid file, scaling and adding grids as requested
            call frkey(kname,numra,numrb,icl,ierr)
            ikey=key
            if(key.eq.3) ikey=key-1+ptg_ifield
            if(ierr.ne.0)then
               call recmes(crname,'F',
     1           '?unable to read bgrid fpack file')
            elseif(kname(1:8).eq.mg_key(ikey))then
               if( ( key.eq.1 .and.(magtype.ge.2.and.magtype.lt.4)) .or.
     1             ( key.eq.2 .and. magtype.ge.3) .or.
     2             ( key.eq.3 .and. ptg_flg) ) then
                  call frhdr(hname,numdb,ncol,nrow,format,nch,ierr)
                  nwr=ncol*nrow
                  if(key.ne.3)then
                    call frdat(nread,nx,nwr)
                    ngr=3*nx*ny*nz
                    if(nread.NE.nwr) call recmes(crname,'F',
     1                 '?error reading grid control words')
                    call frhdr(hname,numdb,ncol,nrow,format,nch,ierr)
                    call frdat(nread,qtemp,max_grid)
                    write(crmess,'(''Grid used is: '',5i10)')ngr,nread,nx,ny,nz
                    call recmes(crname,'I',crmess)
                    if(nread.GT.ngr)call recmes(crname,'F',
     1                 '?error reading bgrid...ngr .ne. nread')
                    if(abs(magscale(key)).GT.2.0 .AND.
     &       c_max.GT.0.9*c_def_torus .AND. c_max.LE.1.2*c_def_mini) then
                       magscale(key)=magscale(key)/c_max
                    endif
                    call vscale(qtemp,magscale(key),qtemp,ngr)
                    call vadd(qfield,qtemp,qfield,ngr)

                  elseif(key.eq.3)then
                    call frdat(nread,nx_ptg,nwr)
                    if(nread.ne.ncol*nrow)call recmes(crname,'F',
     1                 '?error reading grid control words')
                    call frhdr(hname,numdb,ncol,nrow,format,nch,ierr)
                    nwr=ncol*nrow
                    call frdat(nread,Qptg,nwr)
                    write(crmess,'(''Grid used is: '',5I10)')
     &                                  ngr_ptg,nread,nx_ptg,ny_ptg,nz_ptg
                    call recmes(crname,'I',crmess)
                    if(nread.NE.ngr_ptg) call recmes(crname,'F',
     1                   '?error reading bgrid...ngr .ne. nread')
c
                    call vscale(Qptg,ptg_scale,Qptg,ngr_ptg)
                  endif
               endif

            else
               call recmes(crname,'F',
     1              '?error reading key: '//mg_key(key)//
     1              '?was read key: '//kname(1:8)//
     1              ' in fpack file: '//filename(1:flen))
            endif


c  close file
         call fparm('CLOSE BGRID')
         call fermes(crmess,1)
         if(lenocc(crmess).gt.0)call recmes(crname,'F',
     1        '?FPARM: '//crmess)

c  free unit
         call reslun(crname,-unit,iret)
         endif
         enddo
         
c  done reading in grid file
         call recmes(crname,'I',
     1        '*** Done reading magnetic field map ***')

      endif      !reading in bgrid file
      return
      end

c---------------------------------------------------------------------------------


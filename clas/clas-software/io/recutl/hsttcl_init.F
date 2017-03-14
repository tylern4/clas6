      SUBROUTINE HSTTCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: hsttcl_init.F,v 1.5 1998/10/27 20:52:31 freyberg Exp $
c
c  Documentation for subroutine HSTTCL_INIT
c
c  Purpose: link interactive HBOOK variables
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Sep 12 20:16:39 EDT 1996
c  -------
c
c  Major revisions:  26 June 1997  C.A.Meyer
c  ----------------     Use the $(CLAS_CMS)uname_clas function to extract
c                       the operating system name. In order to do this,
c                       we need to extract the environment.
c                    *  Disable global memory for SunOS4. This should
c                       really be off by default for all platforms! We
c                       are far to prone to novice users bringing down
c                       our systems by overflowing the /tmp directory!
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "rhist.inc"
#include "clasmdl.inc"
#include "run_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: hsttcl_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/10/27 20:52:31 $')
      PARAMETER (CAUTHO = '$Author: freyberg $')
      DATA CRCSID/   
     1'$Id: hsttcl_init.F,v 1.5 1998/10/27 20:52:31 freyberg Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='HSTTCL_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer lenocc
      external lenocc
  

      character*20 ts1, ts2, ts3, ts4, ts5, ts6, ts7, ts8
      character*20 ts9, ts10, ts11, ts12, ts13, ts14, ts15, ts16
      character*20 cosname
      character*256 clas_cms
      integer itcl_int, itcl_string, itcl_double, lstring, im, itup
      integer inum, iseq
      integer lfl

      data itcl_int, itcl_double, itcl_string/1,2,4/
c_end_var
c
c  executable code for routine HSTTCL_INIT:
c----6----------------------------------------------------------------72
c
c
c The following variables are in the rehist.inc file and contain the directory
c and ID of the histogram to dump
c

      CALL get_seq(INUM)

      lhist= .false.
      lsetvar = .false.
      call tcl_linkvar('lhist',lhist,itcl_int)
      call tcl_linkvar('lsetvar',lsetvar,itcl_int)
      nbook_stack = 0
      call tcl_linkvar('nbook_stack',nbook_stack,itcl_int)
      nfill_stack = 0
      call tcl_linkvar('nfill_stack',nfill_stack,itcl_int)
       
      lstring = 132
      do im = 1, max_tcl
        if (im .lt. 10) then
          write(ts1,'(a6,i1,a1)')'nhist(',im,')'
          write(ts2,'(a8,i1,a1)')'hist_id(',im,')'
          write(ts3,'(a9,i1,a1)')'hist_dir(',im,')'
          write(ts4,'(a5,i1,a1)')'xmin(',im,')'
          write(ts5,'(a5,i1,a1)')'xmax(',im,')'
          write(ts6,'(a5,i1,a1)')'ymin(',im,')'
          write(ts7,'(a5,i1,a1)')'ymax(',im,')'
          write(ts8,'(a7,i1,a1)')'nxchan(',im,')'
          write(ts9,'(a7,i1,a1)')'nychan(',im,')'
          write(ts10,'(a9,i1,a1)')'hist_tit(',im,')'
          write(ts11,'(a7,i1,a1)')'hist_x(',im,')'
          write(ts12,'(a7,i1,a1)')'hist_y(',im,')'
          write(ts13,'(a6,i1,a1)')'lntup(',im,')'
          write(ts16,'(a5,i1,a1)')'ndim(',im,')'
          do itup = 1, 25
           cntup(im,itup) = '        '
           if (itup .lt. 10) then
            write(ts14,'(a6,i1,a1,i1,a1)')'cntup(',im,',',itup,')'
            write(ts15,'(a8,i1,a1,i1,a1)')'ctclvar(',im,',',itup,')'
           else
            write(ts14,'(a6,i1,a1,i2,a1)')'cntup(',im,',',itup,')'
            write(ts15,'(a8,i1,a1,i2,a1)')'ctclvar(',im,',',itup,')'
           endif
           lstring = 8
           call tcl_linkchar(ts14(1:lenocc(ts14)),cntup(im,itup),
     1                       itcl_string,lstring)
           lstring = 80
           call tcl_linkchar(ts15(1:lenocc(ts15)),ctclvar(im,itup),
     1                       itcl_string,lstring)
          enddo
        else
          write(ts1,'(a6,i2,a1)')'nhist(',im,')'
          write(ts2,'(a8,i2,a1)')'hist_id(',im,')'
          write(ts3,'(a9,i2,a1)')'hist_dir(',im,')'
          write(ts4,'(a5,i2,a1)')'xmin(',im,')'
          write(ts5,'(a5,i2,a1)')'xmax(',im,')'
          write(ts6,'(a5,i2,a1)')'ymin(',im,')'
          write(ts7,'(a5,i2,a1)')'ymax(',im,')'
          write(ts8,'(a7,i2,a1)')'nxchan(',im,')'
          write(ts9,'(a7,i2,a1)')'nychan(',im,')'
          write(ts10,'(a9,i2,a1)')'hist_tit(',im,')'
          write(ts11,'(a7,i2,a1)')'hist_x(',im,')'
          write(ts12,'(a7,i2,a1)')'hist_y(',im,')'
          write(ts13,'(a6,i2,a1)')'lntup(',im,')'
          write(ts16,'(a5,i2,a1)')'ndim(',im,')'
          do itup = 1, 25
           cntup(im,itup) = '        '
           if (itup .lt. 10) then
            write(ts14,'(a6,i2,a1,i1,a1)')'cntup(',im,',',itup,')'
            write(ts15,'(a8,i2,a1,i1,a1)')'ctclvar(',im,',',itup,')'
           else
            write(ts14,'(a6,i2,a1,i2,a1)')'cntup(',im,',',itup,')'
            write(ts15,'(a8,i2,a1,i2,a1)')'ctclvar(',im,',',itup,')'
           endif
           lstring = 8
           call tcl_linkchar(ts14(1:lenocc(ts14)),cntup(im,itup),
     1                       itcl_string,lstring)
           lstring = 80
           call tcl_linkchar(ts15(1:lenocc(ts15)),ctclvar(im,itup),
     1                       itcl_string,lstring)
          enddo
        endif
        nxchan(im) = 0
        nychan(im) = 0
        xmin(im) = 0.
        xmax(im) = 0.
        ymin(im) = 0.
        ymax(im) = 0.
        hist_tit(im) = ' '
        hist_dir(im) = '//PAWC'
        nhist(im) = 0
        hist_id(im) = 0
        lntup(im) = .false.
        call tcl_linkvar(ts13(1:lenocc(ts13)),lntup(im),itcl_int)
        call tcl_linkvar(ts1(1:lenocc(ts1)),nhist(im),itcl_int)
        call tcl_linkvar(ts2(1:lenocc(ts2)),hist_id(im),itcl_int)
        call tcl_linkvar(ts4(1:lenocc(ts4)),xmin(im),itcl_double)
        call tcl_linkvar(ts5(1:lenocc(ts5)),xmax(im),itcl_double)
        call tcl_linkvar(ts6(1:lenocc(ts6)),ymin(im),itcl_double)
        call tcl_linkvar(ts7(1:lenocc(ts7)),ymax(im),itcl_double)
        call tcl_linkvar(ts8(1:lenocc(ts8)),nxchan(im),itcl_int)
        call tcl_linkvar(ts9(1:lenocc(ts9)),nychan(im),itcl_int)
        call tcl_linkvar(ts16(1:lenocc(ts16)),ndim(im),itcl_int)
        lstring = 132
        call tcl_linkchar(ts3(1:lenocc(ts3)),hist_dir(im),itcl_string,lstring)
        lstring = 80
        call tcl_linkchar(ts10(1:lenocc(ts10)),hist_tit(im),
     +                                        itcl_string,lstring)
        call tcl_linkchar(ts11(1:lenocc(ts11)),hist_x(im),itcl_string,lstring)
        call tcl_linkchar(ts12(1:lenocc(ts12)),hist_y(im),itcl_string,lstring)
      enddo

      lhbook = .false.
      call tcl_linkvar('lhbook',lhbook,itcl_int)
      hid = 0
      call tcl_linkvar('hid',hid,itcl_int)
      lstring = 80
      call tcl_linkchar('hist_cut',hist_cut,itcl_string,lstring)
      hdir = '//PAWC'
      call tcl_linkchar('hdir',hdir,itcl_string,lstring)
    
c------------------hbook stuff follows--------------------------------
c
c link the shared memory name so that users can set it to "    " if they
c want to turn shared memory off

      lstring = 4
      call tcl_linkchar('cshared_name',cshared_name,itcl_string,lstring)

c  get the osname
      lstring = 20
      call tcl_linkchar('cosname',cosname,itcl_string,lstring)
*
*        Extract the CLAS_CMS environment
*
      call getenv('CLAS_CMS',clas_cms)
      lfl = lenocc(clas_cms)
*
*        If CLAS_CMS is not defined, defualt to uname:
*
      If ( lfl .le. 1 ) Then
         call tcl_eval('setc cosname [exec uname]')
      else
         call tcl_eval
     &      ('setc cosname [exec '//clas_cms(1:lfl)//'/uname_clas]')
      endif


      write(crmess,10)cosname(1:lenocc(cosname))
 10   format('Operating system determined to be: ',a)
      call recmes(crname,'i',crmess)
      
      iseq = 0
      cshared_name = '    '
      if (cosname .eq. 'HP-UX') then
       call recmes(crname,'i','PAW shared memory disabled for HP-UX')
      elseif (cosname .eq. 'SunOS4') then
       call recmes(crname,'i','PAW shared memory disabled for SunOS4')
      else

       iseq = mod(inum,100)
       write(cshared_name,25)iseq
 25    format('HB',I2.2)

      endif
      call tcl_linkvar('iseq',iseq,itcl_int)
     
      RETURN
      END
c
c------------------------------------------------------------------------------






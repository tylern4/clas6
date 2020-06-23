      subroutine ec1filecalib
      implicit none

      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ec1filecalib.F,v $')
      parameter (crevis= '$Revision: 1.1.1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/03/28 20:57:59 $')
      parameter (cautho= '$Author: marki $')
      parameter (crname= 'EC1FILECALIB')
      parameter (crauth= 'Harout Avakian')
      data crcsid/'
     1$Id: ec1filecalib.F,v 1.1.1.1 1997/03/28 20:57:59 marki Exp $
     1 '/
      save
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "ec1_control.inc"
#include "ec1_calibr.inc"
#include "ec1_pedstl.inc"
c_end_inc
      character*(*) clas_parms
      character*120 filename
      parameter (clas_parms  = 'CLAS_PARMS')
      integer unit,iret
      integer i,k,j,icount,n
      do j=1,2
       do i=1,4
        do k=1,40
         ec1_pedl(k,i,j)=0
         ec1_pedr(k,i,j)=0
         ec1_cala1l(k,i,j)=ec1_caliba1
         ec1_cala2l(k,i,j)=ec1_caliba2
         ec1_calt1l(k,i,j)=ec1_calibt1
         ec1_calt2l(k,i,j)=ec1_calibt2
         ec1_cala1r(k,i,j)=ec1_caliba1
         ec1_cala2r(k,i,j)=ec1_caliba2
         ec1_calt1r(k,i,j)=ec1_calibt1
         ec1_calt2r(k,i,j)=ec1_calibt2
         enddo
       enddo
      enddo
      if (ec1_calib.ne.1) return
c  translate env variable name and get geom input file name
	call revinm(clas_parms,'ec1calib.dat',filename)
	call reslun(crname,unit,iret)
	if(iret.eq.-1)call recmes(crname,'F',
     1     '?unable to reserve fortran unit')

	call reopen(filename,unit,'old',iret)
	if(iret.eq.-1)call recmes(crname,'F',
     1     '?unable to open ec1 calib file')

        j=1
        icount=0
       do 14 n=1,256
11       READ(unit,*,ERR=11,END=14) j,i,k,ec1_pedl(k,i,j),ec1_pedr(k,i,j),
     1 ec1_cala1l(k,i,j),ec1_cala1r(k,i,j),
     1 ec1_cala2l(k,i,j),ec1_cala2r(k,i,j),
     1 ec1_calt1l(k,i,j),ec1_calt1r(k,i,j),
     1 ec1_calt2l(k,i,j),ec1_calt2r(k,i,j)
       icount=icount+1
14     continue
       if( icount.eq.256) then
      call recmes(crname,'I','EC1 calibration read normally')
       else
      call recmes(crname,'I','EC1 ??? missing stacks replaced by default')
       endif
        close (unit)
	call reslun(crname,-unit,iret)

        return
        end


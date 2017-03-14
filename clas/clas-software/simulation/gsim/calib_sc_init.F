
      subroutine calib_sc_init()

c_begin_doc

c  Documentation for subroutine calib_sc_init

c  $Id: calib_sc_init.F,v 1.4 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c   fills the common sccalib with data from the BOS Bank SCC

c  Author:
c  ----------------
c     John Ficenec, Virginia Tech, 27 August 1996

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: calib_sc_init.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'calib_sc_init')
      parameter (crauth= 'John Ficenec')
      data crcsid/'
     1$Id: calib_sc_init.F,v 1.4 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "scgeom.inc"
#include "sccalib.inc"
#include "wcs.inc"

c  local variables:
c  ----------------
	integer i,j,k,unit,iret,ij,ik,id,kj
        integer isec,indg,nchobs,flen,lenocc
	character*120 filename
	character*20 end_stat
        character*4 cunit
c_end_var

c  External functions:
c  -------------------
        integer MLINK
c  end externals


c  executable code:
c  ----------------


c  translate env var name and get input calibration file name


	call revinm(clas_parms,'sc_calib.bfp',filename)
        flen=lenocc(filename)
	call reslun(crname,unit,iret)
	if(iret.eq.-1)call recmes(crname,'F','?unable to reserve unit')
        write(cunit,'(I4)') unit
	call fparm('OPEN SCCIN UNIT='//cunit//' FILE="'//filename(1:flen)//
     1            '" READ STATUS=OLD')

c      read from bos bank

       call blist(jw,'E=','SCC ')
       call frbos(jw,unit,'E',iret)
       if(iret.ne.0) call recmes(crname,'F','?no SC calibration to read')

c  loop over the sectors of CLAS; remember right now geometry is identical
c       for the six sectors.
       do isec = 1,nsecmx
       indg=mlink(jw,'SCC ',isec)
       if(indg.NE.0) then
       nchobs=jw(indg)/nboscal
       if(nchobs.NE.nelcch) call recmes(crname,'F','?SC calibration bad')

c  this loop fills a common block with 57 elements of calibration      
c       information from the 48 calibration channels by duplicating
c       the information for each of the two paddles ganged from nddica=40.

         do i=1,nelcch
         id=jw(indg+1)
       if(i.NE.id) call recmes(crname,'F','?SC paddle id out of order')
           if (i.LT.nddica) then
           ij=id
           ik=id
           else
           ij=2*id-nddica
           ik=ij+1
           end if
             do j=1,2
               do k=1,10
               kj=indg-1+4*k+2*j
               sccal_values(k,j,ij,isec)=rjw(kj)
               sccal_values(k,j,ik,isec)=rjw(kj)
               enddo
               sccal_values(11,j,ij,isec)=rjw(kj+5-j)
               sccal_values(11,j,ik,isec)=rjw(kj+5-j)
               sccal_values(12,j,ij,isec)=rjw(kj+7-j)
               sccal_values(12,j,ik,isec)=rjw(kj+7-j)
             enddo
         indg=indg+nboscal
         enddo
       else
       call recmes(crname,'F','?SC calibration index is zero')
       end if
       enddo
       call fparm('CLOSE SCCIN')
       call reslun(crname,-unit,iret)
       close(unit)
       call recmes(crname,'I','SC calibration data read OK')

	return
	end
	
c------------------------------------------------------------------------------------

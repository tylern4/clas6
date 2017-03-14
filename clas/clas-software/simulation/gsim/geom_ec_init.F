	subroutine geom_ec_init()

c_begin_doc

c  Documentation for subroutine geom_ec_init

c  $Id: geom_ec_init.F,v 1.5 1998/01/22 19:21:00 lcsmith Exp $

c  Purpose:
c  --------
c   fills the common ecgeom with data from file ecgeom.dat

c  Author:
c  ----------------
c     Etienne Burtin, University of South Carolina, 01.24.95 

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_ec_init.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/01/22 19:21:00 $')
      parameter (cautho= '$Author: lcsmith $')
      parameter (crname= 'GEOM_EC_INIT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geom_ec_init.F,v 1.5 1998/01/22 19:21:00 lcsmith Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "ffpar_ec.inc"
#include "ecgeom.inc"
c_end_inc

c  local variables:
c  ----------------
	character*120 filename
	character*20 end_stat
	integer unit,iret,uvw,i_strip,iflag
        integer iilayer,iisect,iistrip,ilayer,isect,istrip,iostat,itype
        integer classec(6)
        real data,fact(2)
        
        data classec/6,5,1,4,3,2/ ! Map from assembly to installation order
        data fact/1.,1.15/	  ! Correction factor for attenuation lengths
c_end_var


c  executable code:
c  ----------------

c  translate env variable name and get geom input file name-------------
	call revinm(clas_parms,'ecgeom.dat',filename)
	call reslun(crname,unit,iret)
	if(iret.eq.-1)call recmes(crname,'F',
     1     '?unable to reserve fortran unit')

	call reopen(filename,unit,'old',iret)
	if(iret.eq.-1)call recmes(crname,'F',
     1     '?unable to open ec geom file')
        
        read(unit,*)
        read(unit,*)Ec_basic_R
        Ec_basic_R = Ec_basic_R*centimeter
        
        read(unit,*)
        read(unit,*)Ec_basic_angle
        Ec_basic_angle = Ec_basic_angle*degree
        
        read(unit,*)
        read(unit,*)Ec_basic_Uin
        Ec_basic_Uin = Ec_basic_Uin*centimeter
        
        read(unit,*)
        read(unit,*)Ec_basic_in_hght
        Ec_basic_in_hght = Ec_basic_in_hght*centimeter
        
        read(unit,*)
        read(unit,*)Ec_basic_out_hght
        Ec_basic_out_hght = Ec_basic_out_hght*centimeter
        
        read(unit,*)
        read(unit,*)Ec_basic_in_wdth
        Ec_basic_in_wdth = Ec_basic_in_wdth*centimeter
        
        read(unit,*)
        read(unit,*)Ec_basic_out_wdth
        Ec_basic_out_wdth = Ec_basic_out_wdth*centimeter
        
        read(unit,*)
        read(unit,*)cov1th
        
        read(unit,*)
        read(unit,*)cov2th
        
        read(unit,*)
        read(unit,*)cov3th
        
        read(unit,*)
        read(unit,*)pbthi
        
        read(unit,*)
        read(unit,*)scthi
        
        read(unit,'(a)')end_stat
        if (end_stat.eq.'end') then
           call recmes(crname,'I','EGN geometry read normally')
        else
           call recmes(crname,'I','**** error in EGN geometry file')
        endif      

        close(unit)
	call reslun(crname,-unit,iret)
        


c  read in EC attenuation lengths if requested-------------------------
      if(atlen.lt.0.)then

         call recmes(crname,'I','Reading in EC attenuation lengths')

c  get file name
         call get_param(gsim_ecatten_flag,gsim_ecatten_env,
     1        filename,iflag)
         if(iflag.lt.0)then
            call revinm(clas_parms,gsim_ecatten_name,filename)
         elseif(iflag.eq.1)then
            call recmes(crname,'I',
     1           'getting ECATTEN filename from command line flag: -' //
     1           gsim_ecatten_flag)
         else
            call recmes(crname,'I',
     1           'getting ECATTEN filename from env variable: ' //
     1           gsim_ecatten_env)
         endif

c  open and read file
         call reslun(crname,unit,iret)
         if(iret.eq.-1)call recmes(crname,'F',
     1        '?unable to reserve fortran unit')
         call reopen(filename,unit,'old',iret)
         if(iret.eq.-1)call recmes(crname,'F',
     1        '?unable to open ec atten len file: '//filename)
         
         do iisect=1,6
            uvw = 0
	    do iilayer =  1,nlayer
               
               uvw = uvw + 1
               if(uvw.gt.3)uvw=1
               
               do iistrip = 1,nstrip
                  read(unit,*,iostat=iostat)
     1                 isect,ilayer,istrip,data,itype
                  if(iostat.eq.0)then
                     i_strip=istrip
                     if(uvw.eq.1)i_strip = 37 - istrip
                     att_length(classec(isect),ilayer,i_strip) = data*fact(itype+1)
                  else
                     call recmes(crname,'I',
     1 '?Error reading attenuation lengths...using 376. instead')
                     close(unit)
                     call reslun(crname,-unit,iret)
                     atlen=376.
                     return
                  endif
               enddo
	    enddo
         enddo  

         close(unit)
         call reslun(crname,-unit,iret)
      endif

	return
	end
	
c---------------------------------------------------------------------------

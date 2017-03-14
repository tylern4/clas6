      subroutine geom_ec1_ini
      implicit none

      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geom_ec1_ini.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:31 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GEOM_EC1_INI')
      parameter (crauth= 'Harout Avakian')
      data crcsid/'
     1$Id: geom_ec1_ini.F,v 1.2 1997/04/24 15:38:31 holtrop Exp $
     1 '/
      save

c     Fills the common lacgeom with data from ec1geom.dat
C     Reads the attenuation length tables from ec1atten.dat
C
C
C
c  include files:
c  --------------
#include "gcunit.inc"
#include "gsimpar.inc"
#include "ec1geom.inc"
#include "ffpar_ec1.inc"
C      COMMON /LACGEOM/HS1,HS2,HB1,HB2,PBSTHI,SCSTHI,HWOSEC,DS,DB,NPBL
C     *,TEX1,TEX2,TEY1,TEY2,TEZ1,TEZ2
C     *,FIX1,FIX2,FIY1,FIY2,FIZ1,FIZ2
C     *,XC1,XC2,YC1,YC2,ZC1,ZC2
C     *,STS1,STS2,AIR1,AIR2
C      REAL HS1,HS2,HB1,HB2,PBSTHI,SCSTHI,HWOSEC,DS,DB
C      REAL TEX1,TEX2,TEY1,TEY2,TEZ1,TEZ2
C      REAL FIX1,FIX2,FIY1,FIY2,FIZ1,FIZ2
C      REAL XC1,XC2,YC1,YC2,ZC1,ZC2
C      REAL STS1,STS2,AIR1,AIR2
C      INTEGER NPBL
C
C attenuation lenghts in the LAC calorimeter 2 x (16x24)+(17x40) strips
C      INTEGER lattlen
C      REAL    laclumi
C      COMMON /LACATTN/ lattlen(256),laclumi(256,2)
CC LAC variables
C

      integer unit,iret
      integer i,k,j
      integer lac_part,nlayer,nstrip,latlng,nabs
      real lumil,lumir
      character*120 filename
      character*20 end_stat
C
C    Read the geometry for the LAC
C
C      unit=12  ! reslun to be used
C      OPEN(UNIT=unit,FILE='ec1geom.dat',STATUS='UNKNOWN'
C     *,FORM='FORMATTED')
C
c  translate env variable name and get geom input file name
	call revinm(clas_parms,'ec1geom.dat',filename)
	call reslun(crname,unit,iret)
	if(iret.eq.-1)call recmes(crname,'F',
     1     '?unable to reserve fortran unit')

	call reopen(filename,unit,'old',iret)
	if(iret.eq.-1)call recmes(crname,'F',
     1     '?unable to open ec1 geom file')

11     READ(unit,*,ERR=11,END=14)PBSTHI,SCSTHI,NPBL 
12    READ(unit,*,ERR=12,END=14)HS1,HB1,HS2,HB2,HWOSEC
 13   READ(unit,*,ERR=13,END=14)STS1,STS2,AIR1,AIR2
 23   READ(unit,*,ERR=23,END=14) TEX1,FIX1,TEY1,FIY1,TEZ1,FIZ1
 24   READ(unit,*,ERR=24,END=14)  XC1,YC1,ZC1
 25   READ(unit,*,ERR=25,END=14) TEX2,FIX2,TEY2,FIY2,TEZ2,FIZ2
 26   READ(unit,*,ERR=26,END=14)  XC2,YC2,ZC2
      READ(unit,'(a20)') end_stat
      if(end_stat.ne.'end') go to 14
           call recmes(crname,'I','EC1 geometry read normally')
      goto 15
 14     call recmes(crname,'I','**** error in EC1 geometry file')
 15   continue

      DS = (HS2-HS1)/NPBL
      DB = (HB2-HB1)/NPBL
c
        close(unit)   
	call reslun(crname,-unit,iret)

c    read the attenuation length and lumi tables

c
cc*********************************************************
      j=0
      if(iatten.lt.0)then

C
C     Fill the array with mean value

      do 1 i=1,256
      laclumi(i,1)=photoel   ! mean luminos.
      laclumi(i,2)=photoel   ! mean luminos.
 1    lattlen(i)=-iatten    ! the mean attenuation length

         call recmes(crname,'I','Reading in LAEC atten lengths')
         call revinm(clas_parms,'ec1atten.dat',filename)
         call reslun(crname,unit,iret)
         if(iret.eq.-1)call recmes(crname,'F',
     1        '?unable to reserve fortran unit')
         call reopen(filename,unit,'old',iret)
         if(iret.eq.-1)call recmes(crname,'F',
     1        '?unable to open ec atten len file: '//filename)

	 do 200 k=1,256
101      read(unit,100,err=101,end=102) lac_part,nlayer,nstrip
     *,latlng,lumil,lumir
c
c   calculating the absolute strip number
         nabs=128*(lac_part-1)+8*mod(nlayer+1,2)
     *+32*(nlayer-1)+nstrip
         lattlen(nabs)=latlng
         laclumi(nabs,1)=lumil
         laclumi(nabs,2)=lumir
c         print *, nabs,lac_part,nlayer,latlng,lumil,lumir
         j=j+1
 200     continue

100      format(3i3,i5,2f7.2)
 102     continue
        close (unit)
	call reslun(crname,-unit,iret)
                        write (chmail,1200) j,(256-j),-iatten
                        call recmes(crname,'I',chmail)

 1200 format ('Read attenuat. length for ',i4,' strips other'
     1       ,i4,' replaced with mean  ',i4)
        else
                        write (chmail,1201) iatten
                        call recmes(crname,'I',chmail)
 1201 format ('Attenuation length for all strips',
     1 ' replaced with mean value ',i4 )
         endif
        return
        end












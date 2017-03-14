      subroutine ec1filegeom(IRUN)
      implicit none


      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ec1filegeom.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/08/04 15:07:09 $')
      parameter (cautho= '$Author: staylor $')
      parameter (crname= 'EC1FILEGEOM')
      parameter (crauth= 'Harout Avakian')
      data crcsid/'
     1$Id: ec1filegeom.F,v 1.2 1998/08/04 15:07:09 staylor Exp $
     1 '/

      save


c     Fills the common lacgeom with data from ec1geom.dat
C     Reads the attenuation length tables from ec1atten.dat
C
C
C
c  include files:
c  --------------
#include "wcs.inc"
#include "ec1rot.inc"
#include "ec1geom.inc"
c      REAL HS1,HS2,HB1,HB2,PBSTHI,SCSTHI,HWOSEC,DS,DB
c      REAL TEX1,TEX2,TEY1,TEY2,TEZ1,TEZ2
c      REAL FIX1,FIX2,FIY1,FIY2,FIZ1,FIZ2
c      REAL XC1,XC2,YC1,YC2,ZC1,ZC2
c      REAL STS1,STS2,AIR1,AIR2
c      INTEGER NPBL
C
C attenuation lenghts in the LAC calorimeter 2 x (2x24)+(2x40) strips
C
c      INTEGER lattlen(256)
c      REAL    laclumi(256,2)

      character*(*) clas_parms
      character*120 filename
      parameter (clas_parms  = 'CLAS_PARMS')
      integer irun,unit,iret
      integer i,k,j,iatten,photoel
      integer lac_part,nlayer,nstrip,latlng,nabs
      real lumil,lumir
      character*20 end_stat
      integer ind
      integer mbank
      real sintzz,sintyz,sintxz,costzz,costyz,costxz
      real sinfzx,sinfyx,sinfxx,cosfzx,cosfyx,cosfxx
      real pi
      CHARACTER*132 CRMESS
c
c  translate env variable name and get geom input file name
c
	call revinm(clas_parms,'ec1geom.dat',filename)
        unit=0
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
c
c      now defining EC1TRAN,EC1RMAT for CLAS <- LAC coordinate conversion
c
c     EC1TRAN transition matrix
c
      EC1TRAN(1,1)=XC2
      EC1TRAN(2,1)=YC2
      EC1TRAN(3,1)=ZC2
      EC1TRAN(1,2)=XC1
      EC1TRAN(2,2)=YC1
      EC1TRAN(3,2)=ZC1
c
c    EC1RMAT rotation matrix
c
      pi=2.*acos(0.)
      sintzz=sin(TEZ2*pi/180.0)
      costzz=cos(TEZ2*pi/180.0)
      sintyz=sin(TEY2*pi/180.0)
      costyz=cos(TEY2*pi/180.0)
      sintxz=sin(TEX2*pi/180.0)
      costxz=cos(TEX2*pi/180.0)
c
      sinfzx=sin(FIZ2*pi/180.0)
      cosfzx=cos(FIZ2*pi/180.0)
      sinfyx=sin(FIY2*pi/180.0)
      cosfyx=cos(FIY2*pi/180.0)
      sinfxx=sin(FIX2*pi/180.0)
      cosfxx=cos(FIX2*pi/180.0)
      
      EC1RMAT(1,1)=sintxz*cosfxx
      EC1RMAT(2,1)=sintxz*sinfxx
      EC1RMAT(3,1)=costxz
c
      EC1RMAT(4,1)=sintyz*cosfyx
      EC1RMAT(5,1)=sintyz*sinfyx
      EC1RMAT(6,1)=costyz
c
      EC1RMAT(7,1)=sintzz*cosfzx
      EC1RMAT(8,1)=sintzz*sinfzx
      EC1RMAT(9,1)=costzz

c
c    second LAC module
c
      sintzz=sin(TEZ1*pi/180.0)
      costzz=cos(TEZ1*pi/180.0)
      sintyz=sin(TEY1*pi/180.0)
      costyz=cos(TEY1*pi/180.0)
      sintxz=sin(TEX1*pi/180.0)
      costxz=cos(TEX1*pi/180.0)
c
      sinfzx=sin(FIZ1*pi/180.0)
      cosfzx=cos(FIZ1*pi/180.0)
      sinfyx=sin(FIY1*pi/180.0)
      cosfyx=cos(FIY1*pi/180.0)
      sinfxx=sin(FIX1*pi/180.0)
      cosfxx=cos(FIX1*pi/180.0)
      
      EC1RMAT(1,2)=sintxz*cosfxx
      EC1RMAT(2,2)=sintxz*sinfxx
      EC1RMAT(3,2)=costxz
c
      EC1RMAT(4,2)=sintyz*cosfyx
      EC1RMAT(5,2)=sintyz*sinfyx
      EC1RMAT(6,2)=costyz
c
      EC1RMAT(7,2)=sintzz*cosfzx
      EC1RMAT(8,2)=sintzz*sinfzx
      EC1RMAT(9,2)=costzz
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c         print *,'converting-> ,i,EC1TRAN(i,1),EC1RMAT(i,1),EC1RMAT(2*i,1),EC1RMAT(3*i,1),EC1TRAN(i,2)'
c       do i=1,3
c         print *,'converting-> ',i,EC1TRAN(i,1),EC1RMAT(i,1),EC1RMAT(2*i,1),EC1RMAT(3*i,1),EC1TRAN(i,2)
c       enddo
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      DS = (HS2-HS1)/NPBL
      DB = (HB2-HB1)/NPBL
c
        close(unit)   
	call reslun(crname,-unit,iret)

c    read the attenuation length and lumi tables

c
cc*********************************************************
      j=0
      iatten=-400 !hardcoded as not important variables
      photoel=20  
      if(iatten.lt.0)then

C
C     Fill the array with mean value

      do 1 i=1,256
      laclumi(i,1)=photoel   ! mean luminos.
      laclumi(i,2)=photoel   ! mean luminos.
 1    lattlen(i)=-iatten     ! the mean attenuation length

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
c         print *,'ec1filegeom', latlng,lumil,lumir,nabs,lac_part,nlayer
         lattlen(nabs)=latlng
         laclumi(nabs,1)=lumil
         laclumi(nabs,2)=lumir
c         print *,nabs,lac_part,nlayer,nstrip,latlng,lumil,lumir
         j=j+1
 200     continue

100      format(3i3,i5,2F7.2)
 102     continue
        close (unit)
	call reslun(crname,-unit,iret)
                        write (crmess,1200) j,(256-j),-iatten
                        call recmes(crname,'I',crmess)

 1200 format ('Read attenuat. length for ',i4,' strips other'
     1       ,i4,' replaced with mean  ',i4)
        else
                        write (crmess,1201) iatten
                        call recmes(crname,'I',crmess)
 1201 format ('Attenuation length for all strips',
     1 ' replaced with mean value ',i4 )
         endif

* Get the plane for track matching
*
C    first LAC  module ...
c
        ind=mbank(jw,'EC1P',1,4,1)
         IF(ind.LE.0) THEN
           WRITE(CRMESS,10) ind
           CALL RECMES(CRNAME,'F',CRMESS)
            return
         ENDIF
        rjw(ind+1)=0.96593 ! sin(75)
        rjw(ind+2)=0.0
        rjw(ind+3)=0.25882  !cos(75)
        rjw(ind+4)=500.0   ! 490 +10
c         print *, '---ec1p-1',ind,rjw(ind+1),rjw(ind+2),rjw(ind+3),rjw(ind+4)

C    second module ...
        ind=mbank(jw,'EC1P',2,4,1)
          IF(ind.LE.0) THEN
           WRITE(CRMESS,10) ind
           CALL RECMES(CRNAME,'F',CRMESS)
            return
         ENDIF
        rjw(ind+1)=0.96593*0.5   ! sin(75)*cos(60)
        rjw(ind+2)=0.96593*0.866 ! sin(75)*sin(60)
        rjw(ind+3)=0.25882  !cos(75)
        rjw(ind+4)=500.0   ! 490 +10
  10       FORMAT('Error in MBANK, ind=',I6)
c         print *, '---ec1p-2',ind,rjw(ind+1),rjw(ind+2),rjw(ind+3),rjw(ind+4)
c         print *, 'geomec1',tez1,tez2,XC2,YC2,ZC2

        return
        end












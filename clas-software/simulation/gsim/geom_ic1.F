        subroutine geom_ic1
       
c     This subroutine defines the geometry of the inner calorimeter (IC)
c     including PbWO4 crystals, crystal wrapping, thin support foils (steel),
c     support frames (aluminium).
c     It also defines the geometry of the conical lead shielding placed 
c     behind the inner calorimeter aiming to stop the Moller electrons
c     -----------------------------------------------------------------------
c     Author : Hyon-Suk Jo - IPN Orsay - 2005
C     Rustam Niyazov RPI 10-03-2008 Added IC structures around IC; 5-side copper cover on top, 3 aluminum frames, brass cradle plates and brass plate at the bottom of IC, aluminum and copper plates at the back and some other stuff 
C     This is needed to match the acceptance in angles 16-20 between data and MC.       
C     Ahmed Fradi IPN-Orsay 31/08/2011 added new IC shielding for the e1-dvcs2 and eg1-dvcs experiments.
       implicit none
       character*(*) crname
       parameter (crname= 'GEOM_IC1')
  
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gconst.inc"
#include "gclist.inc"
#include "lcgeom.inc"
#include "icgeom.inc"
#include "ictmed.inc"
#include "cltmed.inc"
#include "ffpar_ic.inc"
#include "ffpar_tg.inc"
c
c     add the vacuum pipe
c
      real parvac(3),vacz               ! vacuum pipe
      data parvac /0.0,1.79,274.5/      ! IC shield starts from 1.8 (icgeom.inc)
      data vacz /325.5/                 ! up to end of FOIL starting from z=52cm
c
      real cfp,chh,alphar,alphad
      real betaa,betab,betac,betad,betae,betaf,betag
      real mxxi,mxzi,pxxi,pxzi
      real myyj,myzj,pyyj,pyzj
      real gap,mgapy,mgapz
      real thickwrap,thickfoil,lengthfoil,thickairfoil,lengthairfoil
      real gap_m,thickwrap_d,thickwrap_m,chh_m,cfp_m,alphar_m,alphad_m
      real forward,forwardy,forwardz
      real thickplate,lengthplate
      real thickaluframes,aluframespos,aluframespos2
      real deltax,deltay,holex,holey
      real thickaluwall,aluwallpos(3)
      
      integer ivol,nxydiv,rot_matrix,i,j,ibin,jbin,k,get_rotm,h,hx,hy
      integer crwrnb,crnb
      integer crwrnb1,crnb1,crwrnb2,crnb2,crwrnb3,crnb3,crwrnb4,crnb4
      integer ssfnb,afnb,cafnb
      integer pbcrnb
      integer rot_mat
      real ic_pos(3)
      real x,y,z,thex,they,thez,phix,phiy,phiz,nn
      real xf,yf,zf,thexf,phixf,theyf,phiyf,thezf,phizf
      real xp,yp,zp,thexp,phixp,theyp,phiyp,thezp,phizp
      real xb,yb,zb,thexb,phixb,theyb,phiyb,thezb,phizb
      real xsf,ysf,zsf,thexsf,phixsf,theysf,phiysf,thezsf,phizsf
      real thexal,theyal,thezal,phixal,phiyal,phizal
      real n,x1,y1,z1,x2,y2,z2
      character*128 fname
      
      real  brs1vol(10)
      real  posbrass1(3)
      

      real icvol5a(4),icvol5b(4),icvol5c(4),icvol5d(4)
      real icvol5e(4),icvol5f(4),icvol5g(4)
      real icvol6a(4),icvol6b(4),icvol6c(4),icvol6d(4)
      real icvol6e(4),icvol6f(4),icvol6g(4)
      real icvol7a(4),icvol7b(4)
      real icvol8(4),icvol9(3)
      real icvol10(3),icvol11(3),icvol12(3)
      real icvol13(3),icvol13l(4),icvol13r(4),icvol14(3),icvol15(5)
      real icvolr(19)
      real shldpos(3)
      real dXa,dXb,dXc,dXd,dXe,dXf,dXg
      real frsh
      data dXa / 15.5942 /
      data dXb / 14.2212 /
      data dXc / 12.8532 /
      data dXd / 11.4896 /
      data dXe / 10.1298 /
      data dXf / 8.7733 /
      data dXg / 7.4196 /
      real dXaa,dXab
      data dXaa / 3.3694 /
      data dXab / 2.0217 /
      data frsh / 0.7 / 

C  variables for the new shielding for e1dvcs2 and eg1dvcs: Ahmed Fradi
      
      real icback1(10),icback2(10),icback3(10),icback4(10),icbackair(3)
      real icback1pos(3),icback2pos(3),icbackmo1(3),icbackmo1pos(3)
      real  icbackmo2(3),icbackmo2pos(3)
      real  icbackmo3(3),icbackmo3pos(3)
      real  icbackmo4(3),icbackmo4pos(3)
      real  icbackmo5(3),icbackmo5pos(3)
      real icback0(33),icback0pos(3)
      real  icbackmo6(3),icbackmo6pos(3)
      real  parvacum(3),vacumz 


C Rustam 10/03/2008 Made smaller the size of the frames to let brass craddle plates and 5-side IC cover to fit in; subtracted value of frsh from the size of the frames
C
      call recmes(crname,'I','IC1 geometry routine begun...')
      ic_mod = abs(ic_mod)

c     Mother volume for IC     

      chh = icvol1w(1) + (icvol1w(2)-icvol1w(1))/2
      cfp = chh/(icvol1w(2)-icvol1w(1))*2*icvol1w(5) 
      z0p = ic_z0 + icvol1w(5) - cfp
      
      ic_pos(1) = ic_front(1)
      ic_pos(2) = ic_front(2)
      ic_pos(3) = ic_front(3) - 79.5852
      
      call gsvolu('IC  ','TUBE',clas_med_air,icvol,3,IVOL)
      call gspos('IC  ',1,'CLAS', ic_pos(1), ic_pos(2), ic_pos(3),0,'MANY')
      
C     === Hyon-Suk Jo - April 4, 2011 === Definition of an intermediary
C     volume, IC0, forming with IC a "double-level" mother volume for IC
C     volumes, in order to define the IC volumes at a deeper level and thus
C     give them a higher priority. This resolves all priority conflicts
C     between the IC volumes flagged as MANY and the LSS- volumes (and FOI2
C     before it was removed (see geom_foil.F)).
      
      call gsvolu('IC0 ','TUBE',clas_med_air,icvol,3,IVOL)
      call gspos('IC0 ',1,'IC  ', 0, 0, 0,0,'MANY')
      

c     Possibility to choose from the full calorimeter and the prototype
c     ic_mod = 2 => Full calorimeter geometry (424 crystals)
c     ic_mod = 3 => Prototype geometry (100 crystals + lead blocks)

c     Define the geometry of the full calorimeter (424 crystals)
      if (ic_mod.eq.2) then 
                   
c     Use trapezoid shape TRD2 for individual crystal volume IC1

c     Define cristal wrapping shape and dimensions

         thickwrap = 0.0065
         call gsvolu('CRWR','TRD2',ic_med_mylar,icvol1w,5,IVOL)
         call gsatt('CRWR','COLO',4)

c     Define cristal shape and dimensions
         
         icvol1c(1) = icvol1w(1) - thickwrap
         icvol1c(2) = icvol1w(2) - thickwrap
         icvol1c(3) = icvol1w(3) - thickwrap
         icvol1c(4) = icvol1w(4) - thickwrap
         call gsvolu('IC1 ','TRD2',ic_med_pbwo4,icvol1c,5,IVOL)
         call gsatt('IC1 ','COLO',1)

         gap = 0.03
         alphar = 2.*atan(chh/cfp)
         alphad = alphar*180./pi

         forward = 0.11

c     Use trapezoid shape TRD1 for every stainless steel foil
c     And define dimensions for the different stainless steel foils (a,b,c,d,e,f,g)
         
         thickfoil = 0.006
         lengthfoil = 8.65
         
         betaa = 11.5*alphar
         betab = 10.5*alphar
         betac = 9.5*alphar
         betad = 8.5*alphar
         betae = 7.5*alphar
         betaf = 6.5*alphar
         betag = 5.5*alphar
         
         icvol5a(1) = dXa - 0.65*tan(betaa)
         icvol5a(2) = dXa + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betaa)
         icvol5a(3) = thickfoil
         icvol5a(4) = lengthfoil
         icvol5b(1) = dXb - 0.65*tan(betab)
         icvol5b(2) = dXb + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betab)
         icvol5b(3) = thickfoil
         icvol5b(4) = lengthfoil
         icvol5c(1) = dXc - 0.65*tan(betac)
         icvol5c(2) = dXc + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betac)
         icvol5c(3) = thickfoil
         icvol5c(4) = lengthfoil
         icvol5d(1) = dXd - 0.65*tan(betad)
         icvol5d(2) = dXd + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betad)
         icvol5d(3) = thickfoil
         icvol5d(4) = lengthfoil
         icvol5e(1) = dXe - 0.65*tan(betae)
         icvol5e(2) = dXe + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betae)
         icvol5e(3) = thickfoil
         icvol5e(4) = lengthfoil
         icvol5f(1) = dXf - 0.65*tan(betaf)
         icvol5f(2) = dXf + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betaf)
         icvol5f(3) = thickfoil
         icvol5f(4) = lengthfoil
         icvol5g(1) = dXg - 0.65*tan(betag)
         icvol5g(2) = dXg + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betag)
         icvol5g(3) = thickfoil
         icvol5g(4) = lengthfoil

         call gsvolu('SSF1','TRD1',ic_med_steel,icvol5a,4,IVOL)
         call gsvolu('SSF2','TRD1',ic_med_steel,icvol5b,4,IVOL)
         call gsvolu('SSF3','TRD1',ic_med_steel,icvol5c,4,IVOL)
         call gsvolu('SSF4','TRD1',ic_med_steel,icvol5d,4,IVOL)
         call gsvolu('SSF5','TRD1',ic_med_steel,icvol5e,4,IVOL)
         call gsvolu('SSF6','TRD1',ic_med_steel,icvol5f,4,IVOL)
         call gsvolu('SSF7','TRD1',ic_med_steel,icvol5g,4,IVOL)
         call gsatt('SSF1','COLO',2)
         call gsatt('SSF2','COLO',2)
         call gsatt('SSF3','COLO',2)
         call gsatt('SSF4','COLO',2)
         call gsatt('SSF5','COLO',2)
         call gsatt('SSF6','COLO',2)
         call gsatt('SSF7','COLO',2) 
         
c     Use trapezoid shape TRD1 to define air foils which role is to finalise
c     - by 'cutting' - the shape of every stainless steel foil    
         
         thickairfoil = 0.006
         lengthairfoil = 7.55
         
         icvol6a(1) = dXa + 0.45*tan(betaa)
         icvol6a(2) = dXa + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betaa)
         icvol6a(3) = thickairfoil
         icvol6a(4) = lengthairfoil
         icvol6b(1) = dXb + 0.45*tan(betab)
         icvol6b(2) = dXb + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betab)
         icvol6b(3) = thickairfoil
         icvol6b(4) = lengthairfoil
         icvol6c(1) = dXc + 0.45*tan(betac)
         icvol6c(2) = dXc + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betac)
         icvol6c(3) = thickairfoil
         icvol6c(4) = lengthairfoil
         icvol6d(1) = dXd + 0.45*tan(betad)
         icvol6d(2) = dXd + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betad)
         icvol6d(3) = thickairfoil
         icvol6d(4) = lengthairfoil
         icvol6e(1) = dXe + 0.45*tan(betae)
         icvol6e(2) = dXe + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betae)
         icvol6e(3) = thickairfoil
         icvol6e(4) = lengthairfoil
         icvol6f(1) = dXf + 0.45*tan(betaf)
         icvol6f(2) = dXf + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betaf)
         icvol6f(3) = thickairfoil
         icvol6f(4) = lengthairfoil
         icvol6g(1) = dXg + 0.45*tan(betag)
         icvol6g(2) = dXg + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betag)
         icvol6g(3) = thickairfoil
         icvol6g(4) = lengthairfoil
         
         call gsvolu('AF1 ','TRD1',ic_med_air,icvol6a,4,IVOL)
         call gsvolu('AF2 ','TRD1',ic_med_air,icvol6b,4,IVOL)
         call gsvolu('AF3 ','TRD1',ic_med_air,icvol6c,4,IVOL)
         call gsvolu('AF4 ','TRD1',ic_med_air,icvol6d,4,IVOL)
         call gsvolu('AF5 ','TRD1',ic_med_air,icvol6e,4,IVOL)
         call gsvolu('AF6 ','TRD1',ic_med_air,icvol6f,4,IVOL)
         call gsvolu('AF7 ','TRD1',ic_med_air,icvol6g,4,IVOL)
         call gsatt('AF1 ','COLO',2)
         call gsatt('AF2 ','COLO',2)
         call gsatt('AF3 ','COLO',2)
         call gsatt('AF4 ','COLO',2)
         call gsatt('AF5 ','COLO',2)
         call gsatt('AF6 ','COLO',2)
         call gsatt('AF7 ','COLO',2)

c     Use trapezoid shape TRD1 to define air foils which role
c     is to 'cut' the stainless steel foils in the center of the calorimeter
         
         icvol7a(1) = dXaa - 0.65*tan(5./2.*alphar)
         icvol7a(2) = dXaa + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(5./2.*alphar)
         icvol7a(3) = thickfoil
         icvol7a(4) = lengthfoil
         icvol7b(1) = dXab - 0.65*tan(3./2.*alphar)
         icvol7b(2) = dXab + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(3./2.*alphar)
         icvol7b(3) = thickfoil
         icvol7b(4) = lengthfoil
         
         call gsvolu('CAF1','TRD1',ic_med_air,icvol7a,4,IVOL)
         call gsvolu('CAF2','TRD1',ic_med_air,icvol7b,4,IVOL)
         call gsatt('CAF1','COLO',3)
         call gsatt('CAF2','COLO',3)
         


c     Define the front parts of the support frames
         
         deltax = 1.348
         deltay = 1.364 
         
         icvol10(2) = 0.1315
         icvol10(3) = 0.95
         aluframespos = -ic_pos(3)+ic_front(3)-0.1-icvol10(3)
         icvol10(1) = 7.887-frsh
         call gsvolu('ALF1','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF1',1,'IC0 ',0.,-1.33/2.+icvol10(2)+11.*deltay,aluframespos,0,'MANY')
         call gspos('ALF1',2,'IC0 ',0.,-1.33/2.+icvol10(2)-11.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 9.245-frsh
         call gsvolu('ALF2','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF2',1,'IC0 ',0.,-1.33/2.+icvol10(2)+10.*deltay,aluframespos,0,'MANY')
         call gspos('ALF2',2,'IC0 ',0.,-1.33/2.+icvol10(2)-10.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 10.607-frsh
         call gsvolu('ALF3','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF3',1,'IC0 ',0.,-1.33/2.+icvol10(2)+9.*deltay,aluframespos,0,'MANY')
         call gspos('ALF3',2,'IC0 ',0.,-1.33/2.+icvol10(2)-9.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 11.971-frsh
         call gsvolu('ALF4','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF4',1,'IC0 ',0.,-1.33/2.+icvol10(2)+8.*deltay,aluframespos,0,'MANY')
         call gspos('ALF4',2,'IC0 ',0.,-1.33/2.+icvol10(2)-8.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 13.339-frsh
         call gsvolu('ALF5','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF5',1,'IC0 ',0.,-1.33/2.+icvol10(2)+7.*deltay,aluframespos,0,'MANY')
         call gspos('ALF5',2,'IC0 ',0.,-1.33/2.+icvol10(2)-7.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 14.71-frsh
         call gsvolu('ALF6','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF6',1,'IC0 ',0.,-1.33/2.+icvol10(2)+6.*deltay,aluframespos,0,'MANY')
         call gspos('ALF6',2,'IC0 ',0.,-1.33/2.+icvol10(2)-6.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 16.085-frsh
         call gsvolu('ALF7','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF7',1,'IC0 ',0.,-1.33/2.+icvol10(2)+5.*deltay,aluframespos,0,'MANY')
         call gspos('ALF7',2,'IC0 ',0.,-1.33/2.+icvol10(2)-5.*deltay,aluframespos,0,'MANY')
         call gspos('ALF7',3,'IC0 ',0.,-1.33/2.+icvol10(2)+4.*deltay,aluframespos,0,'MANY')
         call gspos('ALF7',4,'IC0 ',0.,-1.33/2.+icvol10(2)-4.*deltay,aluframespos,0,'MANY')
         call gspos('ALF7',5,'IC0 ',0.,-1.33/2.+icvol10(2)+3.*deltay,aluframespos,0,'MANY')
         call gspos('ALF7',6,'IC0 ',0.,-1.33/2.+icvol10(2)-3.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 7.2805
C Rustam Niyazov Replaced 1.524 by 1.52
         call gsvolu('ALF8','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF8',1,'IC0 ',1.52+icvol10(1),-1.33/2.+icvol10(2)+2.*deltay,aluframespos,0,'MANY')
         call gspos('ALF8',2,'IC0 ',-1.52-icvol10(1),-1.33/2.+icvol10(2)+2.*deltay,aluframespos,0,'MANY')
         call gspos('ALF8',3,'IC0 ',1.52+icvol10(1),-1.33/2.+icvol10(2)-2.*deltay,aluframespos,0,'MANY')
         call gspos('ALF8',4,'IC0 ',-1.52-icvol10(1),-1.33/2.+icvol10(2)-2.*deltay,aluframespos,0,'MANY')
         icvol10(1) = 6.6255
         call gsvolu('ALF9','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF9',1,'IC0 ',2.834+icvol10(1),-1.33/2.+icvol10(2)+1.*deltay,aluframespos,0,'MANY')
         call gspos('ALF9',2,'IC0 ',-2.834-icvol10(1),-1.33/2.+icvol10(2)+1.*deltay,aluframespos,0,'MANY')
         call gspos('ALF9',3,'IC0 ',2.834+icvol10(1),-1.33/2.+icvol10(2)-1.*deltay,aluframespos,0,'MANY')
         call gspos('ALF9',4,'IC0 ',-2.834-icvol10(1),-1.33/2.+icvol10(2)-1.*deltay,aluframespos,0,'MANY')
         call gspos('ALF9',5,'IC0 ',2.834+icvol10(1),-1.33/2.+icvol10(2),aluframespos,0,'MANY')
         call gspos('ALF9',6,'IC0 ',-2.834-icvol10(1),-1.33/2.+icvol10(2),aluframespos,0,'MANY')

         icvol11(2) = 0.155
         icvol11(3) = 0.5
         aluframespos2 = aluframespos-icvol10(3)+icvol11(3)
         icvol11(1) = 7.887-frsh
         call gsvolu('ALG1','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG1',1,'IC0 ',0.,-1.33/2.-icvol11(2)+11.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG1',2,'IC0 ',0.,-1.33/2.-icvol11(2)-11.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 9.245-frsh
         call gsvolu('ALG2','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG2',1,'IC0 ',0.,-1.33/2.-icvol11(2)+10.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG2',2,'IC0 ',0.,-1.33/2.-icvol11(2)-10.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 10.607-frsh
         call gsvolu('ALG3','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG3',1,'IC0 ',0.,-1.33/2.-icvol11(2)+9.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG3',2,'IC0 ',0.,-1.33/2.-icvol11(2)-9.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 11.971-frsh
         call gsvolu('ALG4','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG4',1,'IC0 ',0.,-1.33/2.-icvol11(2)+8.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG4',2,'IC0 ',0.,-1.33/2.-icvol11(2)-8.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 13.339-frsh
         call gsvolu('ALG5','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG5',1,'IC0 ',0.,-1.33/2.-icvol11(2)+7.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG5',2,'IC0 ',0.,-1.33/2.-icvol11(2)-7.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 14.71-frsh
         call gsvolu('ALG6','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG6',1,'IC0 ',0.,-1.33/2.-icvol11(2)+6.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG6',2,'IC0 ',0.,-1.33/2.-icvol11(2)-6.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 16.085-frsh
         call gsvolu('ALG7','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG7',1,'IC0 ',0.,-1.33/2.-icvol11(2)+5.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG7',2,'IC0 ',0.,-1.33/2.-icvol11(2)-5.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG7',3,'IC0 ',0.,-1.33/2.-icvol11(2)+4.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG7',4,'IC0 ',0.,-1.33/2.-icvol11(2)-4.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG7',5,'IC0 ',0.,-1.33/2.-icvol11(2)+3.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG7',6,'IC0 ',0.,-1.33/2.-icvol11(2)-3.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 7.2805
         call gsvolu('ALG8','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG8',1,'IC0 ',1.524+icvol11(1),-1.33/2.-icvol11(2)+2.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG8',2,'IC0 ',-1.524-icvol11(1),-1.33/2.-icvol11(2)+2.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG8',3,'IC0 ',1.524+icvol11(1),-1.33/2.-icvol11(2)-2.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG8',4,'IC0 ',-1.524-icvol11(1),-1.33/2.-icvol11(2)-2.*deltay,aluframespos2,0,'MANY')
         icvol11(1) = 6.6255
         call gsvolu('ALG9','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALG9',1,'IC0 ',2.834+icvol11(1),-1.33/2.-icvol11(2)+1.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG9',2,'IC0 ',-2.834-icvol11(1),-1.33/2.-icvol11(2)+1.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG9',3,'IC0 ',2.834+icvol11(1),-1.33/2.-icvol11(2)-1.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG9',4,'IC0 ',-2.834-icvol11(1),-1.33/2.-icvol11(2)-1.*deltay,aluframespos2,0,'MANY')
         call gspos('ALG9',5,'IC0 ',2.834+icvol11(1),-1.33/2.-icvol11(2),aluframespos2,0,'MANY')
         call gspos('ALG9',6,'IC0 ',-2.834-icvol11(1),-1.33/2.-icvol11(2),aluframespos2,0,'MANY')
         
         icvol12(1)=0.3
         icvol12(2)=0.325
         icvol12(3)=0.5
         call gsvolu('ALFF','BOX ',clas_med_alu,icvol12,3,IVOL)
         h=0
         do hy=0,11
            h=h+1
            call gspos('ALFF',h,'IC0 ',3.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-3.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
         enddo
C Rustam 08/05/2008 Took away two bottom aluminum blocks to get more space for brass craddle 
         do hy=1,10
            h=h+1
            call gspos('ALFF',h,'IC0 ',3.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-3.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')
         enddo
         do hy=0,5
            h=h+1
            call gspos('ALFF',h,'IC0 ',9.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',10.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-9.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-10.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
         enddo
         do hy=1,4
            h=h+1
            call gspos('ALFF',h,'IC0 ',9.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',10.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-9.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-10.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')
         enddo
         do hx=4,8
            do hy=1,5
               if ((hx+11-hy).eq.14) then
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',(hx+0.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+(11-hy)*deltay,
     &                 aluframespos2,0,'MANY')
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',-(hx+0.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+(11-hy)*deltay,
     &                 aluframespos2,0,'MANY')
               else
               endif
            enddo   
         enddo         
         do hx=1,4
            do hy=5,8
               if ((9-hx+hy).eq.13) then
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',(9-hx+0.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,
     &                 aluframespos2,0,'MANY')
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',(9-hx+1.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,
     &                 aluframespos2,0,'MANY')
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',-(9-hx+0.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,
     &                 aluframespos2,0,'MANY')
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',-(9-hx+1.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,
     &                 aluframespos2,0,'MANY')
               else
               endif
            enddo
         enddo
         h=h+1
         call gspos('ALFF',h,'IC0 ',5.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-9.*deltay,aluframespos2,0,'MANY')
         h=h+1
         call gspos('ALFF',h,'IC0 ',-5.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-9.*deltay,aluframespos2,0,'MANY')
c     frames edges
         do hx=5,10
            do hy=1,6
               if ((hx+13-hy).eq.17) then
                  h=h+1
C Rustam Niyazov Replaced (hx+0.5) with (hx+0.4)
                  call gspos('ALFF',h,'IC0 ',(hx+0.4)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+(13-hy)*deltay,
     &                 aluframespos2,0,'MANY')
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',-(hx+0.4)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+(13-hy)*deltay,
     &                 aluframespos2,0,'MANY')
               else
               endif
            enddo
         enddo
         do hy=0,6
            h=h+1
C Rustam Niyazov Replaced 11.5*deltax with 11.4*deltax
            call gspos('ALFF',h,'IC0 ',11.4*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-11.4*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+hy*deltay,aluframespos2,0,'MANY')
         enddo
         do hy=1,4
            h=h+1
             call gspos('ALFF',h,'IC0 ',11.4*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')

            h=h+1
             call gspos('ALFF',h,'IC0 ',-11.4*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,aluframespos2,0,'MANY')
         enddo
         do hx=1,6
            do hy=5,10
               if ((11-hx+hy).eq.15) then
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',(11-hx+0.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,
     &                 aluframespos2,0,'MANY')
                  h=h+1
                  call gspos('ALFF',h,'IC0 ',-(11-hx+0.5)*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-hy*deltay,
     &                 aluframespos2,0,'MANY')
               else
               endif
            enddo
         enddo
         h=h+1
         call gspos('ALFF',h,'IC0 ',1.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+3.*deltay,aluframespos2,0,'MANY')
         h=h+1
         call gspos('ALFF',h,'IC0 ',-1.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+3.*deltay,aluframespos2,0,'MANY')
         h=h+1
         call gspos('ALFF',h,'IC0 ',1.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-1.*deltay,aluframespos2,0,'MANY')
         h=h+1
         call gspos('ALFF',h,'IC0 ',-1.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)-1.*deltay,aluframespos2,0,'MANY')
         do hy=1,3
            h=h+1
            call gspos('ALFF',h,'IC0 ',2.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+(3-hy)*deltay,aluframespos2,0,'MANY')
            h=h+1
            call gspos('ALFF',h,'IC0 ',-2.5*deltax,-1.33/2.-2.*icvol11(2)-icvol12(2)+(3-hy)*deltay,aluframespos2,0,'MANY')
         enddo         

C Rustam Niyazov 07/16/2008 Add new IC structure parts, including copper cover plates,
C 3 aluminum frames, bottom brass cradle and side brass plates. See below 
C Use polygon to derive the octagon
C Front aluminum frame

C starting Phi=phi1
         icvolr(1)=45./2.
C opening phi angle=dphi  
         icvolr(2)=360.
C number of sides between phi1 and phi1+dphi 
         icvolr(3)=8.
C number of planes perp to Z axis
         icvolr(4)=2.
C zcoordinate of the section
         icvolr(5)=-0.316
C minimal radius in cm
         icvolr(6)=15.93
C maximal radius in cm
         icvolr(7)=16.83
C_____________________
         icvolr(8)=0.316
         icvolr(9)=15.93
         icvolr(10)=16.83

         aluwallpos(1) = 0. 
         aluwallpos(2) = 0.
         aluwallpos(3) = -ic_pos(3) + ic_front(3) - 3.3 - 0.62
        call gsvolu('ALR1  ','PGON',clas_med_alu,icvolr,10,IVOL)
        call gspos('ALR1 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')
C_____________________
C first back aluminum frame 
C_____________________
         icvolr(1)=45./2.
         icvolr(2)=360.
         icvolr(3)=8.
         icvolr(4)=2.
         icvolr(5)=-0.8
         icvolr(6)=19.10
         icvolr(7)=20.4
C_____________________
         icvolr(8)=0.8
         icvolr(9)=19.10
         icvolr(10)=20.4

         aluwallpos(1) = 0. 
         aluwallpos(2) = 0.
         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 18.95
        call gsvolu('ALR2  ','PGON',clas_med_alu,icvolr,10,IVOL)
        call gspos('ALR2 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')
C_____________________
C second back aluminum frame 
C_____________________
         icvolr(1)=45./2.
         icvolr(2)=360.
         icvolr(3)=8.
         icvolr(4)=2.
         icvolr(5)=-0.5
         icvolr(6)=21.75
         icvolr(7)=22.45
         icvolr(8)=0.5
         icvolr(9)=21.75
         icvolr(10)=22.45

         aluwallpos(1) = 0. 
         aluwallpos(2) = 0.
         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 25.7
        call gsvolu('ALR3  ','PGON',clas_med_alu,icvolr,10,IVOL)
        call gspos('ALR3 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')
C_____________________        
C aluminum assembly plate at the back with size 118x6x45 mm
C_____________________

        icvolr(1) = 5.9
        icvolr(2) = 0.5
        icvolr(3) = 2.25
        aluwallpos(1) = 0 
        aluwallpos(2) = -20.5
        aluwallpos(3) = -ic_pos(3) + ic_front(3) +22.3 
	rot_mat = get_rotm()
        thexal = 90.
        phixal = 0.
        theyal = 90.-11.0
        phiyal = 90.
        thezal = 0.
        phizal = 0.
        call mat_cor(thexal,phixal,theyal,phiyal,thezal,phizal)
        call gsrotm(rot_mat,thexal,phixal,theyal,phiyal,thezal,phizal)
        call gsvolu('ALR4 ','BOX ',clas_med_alu,icvolr,3,IVOL)
        call gspos('ALR4 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),rot_mat,'ONLY')
C_____________________
C aluminum 3 mm thick octagon at the back. The dimensions of the octagon are the same as for the 10x8 mm aluminum frame at the back. The beam hole is an 5 cm ID octagon.  
C_____________________
         icvolr(1)=45./2.
         icvolr(2)=360.
         icvolr(3)=8.
         icvolr(4)=2.
         icvolr(5)=-0.15
         icvolr(6)=2.5
         icvolr(7)=22.45
C_____________________
         icvolr(8)=0.15
         icvolr(9)=2.5
         icvolr(10)=22.45
         aluwallpos(1) = 0. 
         aluwallpos(2) = 0.
         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 23.4 + 4.9	
         call gsvolu('ALR5  ','PGON',clas_med_alu,icvolr,10,IVOL)
         call gspos('ALR5 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')

C_____________________
C 1mm thick 5 side copper plate from IC cover
C_____________________
         icvolr(1)=-22.5
         icvolr(2)=225.
         icvolr(3)=5.
         icvolr(4)=2.
         icvolr(5)=-10.8
         icvolr(6)=16.0
         icvolr(7)=16.1
C_____________________  
         icvolr(8)=10.8
         icvolr(9)=20.4
         icvolr(10)=20.5
         aluwallpos(1) = 0.0 
         aluwallpos(2) = 0.0
         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 7.3
         call gsvolu('COR1  ','PGON',clas_med_copper,icvolr,10,IVOL)
         call gspos('COR1 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')
C_____________________
C 2 mm copper octagon at the back to simulate interaction with mother board material (3.2 mm epoxy/glass fiber/copper tracks/copper). The beam hole is an 5 cm ID octagon.  
C_____________________
         icvolr(1)=45./2.
         icvolr(2)=360.
         icvolr(3)=8.
         icvolr(4)=2.
         icvolr(5)=-0.1
         icvolr(6)=2.5
         icvolr(7)=22.45
C_____________________
         icvolr(8)=0.1
         icvolr(9)=2.5
         icvolr(10)=22.45
         aluwallpos(1) = 0. 
         aluwallpos(2) = 0.
         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 23.4 + 3.5
         call gsvolu('COR2  ','PGON',clas_med_copper,icvolr,10,IVOL)
         call gspos('COR2 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')

C_____________________
C      10mm Brass plate to support IC at the bottom. 
C      brass and copper have almost the same density 
C_____________________
C New Brass cradle made by Ahmed Fradi  04/03/2011
C Measurements from  Emanuel Rindel    

C first part BRS1
           
	 brs1vol(1)=-67.5 
         brs1vol(2)=45.   
         brs1vol(3)=1.
         brs1vol(4)=2.
         brs1vol(5)=-10.7
         brs1vol(6)=16.4
         brs1vol(7)=17
         brs1vol(8)=10.7
         brs1vol(9)=20.8
         brs1vol(10)=21.4
         posbrass1(1) = 0.0 
         posbrass1(2) = 0.0
         
	 posbrass1(3) = -ic_pos(3) + ic_front(3) + 8.2
	 
         call gsvolu('BRS1','PGON',clas_med_copper,brs1vol,10,IVOL)
         call gspos('BRS1',1,'IC0 ',posbrass1(1),posbrass1(2),posbrass1(3),0,'ONLY')
         
	 
C 2nd part BRS2
           
	 icvolr(1)=-157.5 
         icvolr(2)=45.
         icvolr(3)=1.
         icvolr(4)=2.
         icvolr(5)=-10.7
         icvolr(6)=16.4
         icvolr(7)=17.  
         icvolr(8)=10.7
         icvolr(9)=20.8
         icvolr(10)=21.4
         aluwallpos(1) = 0.0 
         aluwallpos(2) = 0.0

         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 8.2


	 call gsvolu('BRS2','PGON',clas_med_copper,icvolr,10,IVOL)
         call gspos('BRS2',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')

          

C 3rd  part BRS3
           
	 icvolr(1)=-112.5 
         icvolr(2)=45.
         icvolr(3)=1.
         icvolr(4)=2.
         icvolr(5)=-13.65
         icvolr(6)=16.3
         icvolr(7)=17.3
         icvolr(8)=13.65
         icvolr(9)=21.6
         icvolr(10)=22.6

          

         aluwallpos(1) = 0.0 
         aluwallpos(2) = 0.0
 

C origianl position of the BRS3 is aluwallpos(3) = -ic_pos(3) + ic_front(3) + 9.75

C I shift the original  position of BRS3 by 0.5 cm to fit with the front aluminum frame ("ALR1")
     
         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 9.75 + 0.5  	 
          
         call gsvolu('BRS3','PGON',clas_med_copper,icvolr,10,IVOL)
         call gspos('BRS3',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')
     
C End of my work  Ahmed Fradi    

C_____________________ 
C First brass plate on the top of the sides 6 mm thick
C_____________________

        icvolr(1) = 0.3
        icvolr(2) = 1.0
        icvolr(3) = 10.94
        aluwallpos(1) = -19.0 
        aluwallpos(2) = -6.6
        aluwallpos(3) = -ic_pos(3) + ic_front(3)+8.5 
        rot_mat = get_rotm()
        thexal = 90.-11.05
        phixal = 0.0
        theyal = 90.-5.2 
        phiyal = 90.
        thezal = 0.
        phizal = 0.0
        call mat_cor(thexal,phixal,theyal,phiyal,thezal,phizal)
        call gsrotm(rot_mat,thexal,phixal,theyal,phiyal,thezal,phizal)
        call gsvolu('BRR4 ','BOX ',clas_med_copper,icvolr,3,IVOL)
        call gspos('BRR4 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),rot_mat,'ONLY')

C_____________________ 
C Second brass plate on the top of the sides 6 mm thick
C_____________________

        icvolr(1) = 0.3
        icvolr(2) = 1.0
        icvolr(3) = 10.94
        aluwallpos(1) =  19.0 
        aluwallpos(2) = -6.6
        aluwallpos(3) = -ic_pos(3) + ic_front(3)+8.5 
        rot_mat = get_rotm()
        thexal = 90.+11.05
        phixal = 0.0
        theyal = 90.-5.2 
        phiyal = 90.
        thezal = 0.
        phizal = 0.0
        call mat_cor(thexal,phixal,theyal,phiyal,thezal,phizal)
        call gsrotm(rot_mat,thexal,phixal,theyal,phiyal,thezal,phizal)
        call gsvolu('BRR5 ','BOX ',clas_med_copper,icvolr,3,IVOL)
        call gspos('BRR5 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),rot_mat,'ONLY')

C_____________________
C 12mm thick foam around IC
C_____________________
         icvolr(1)=45./2.
         icvolr(2)=360.
         icvolr(3)=8.
         icvolr(4)=2.
         icvolr(5)=-10.8
         icvolr(6)=17.5
         icvolr(7)=18.7
C_____________________
         icvolr(8)=10.8
         icvolr(9)=22.0
         icvolr(10)=23.2

         aluwallpos(1) = 0. 
         aluwallpos(2) = 0.
         aluwallpos(3) = -ic_pos(3) + ic_front(3) + 7.3

         call gsvolu('FOR1  ','PGON',clas_med_lowdfoam,icvolr,10,IVOL)
         call gspos('FOR1 ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'ONLY')

C End of my work Rustam Niyazov 
C_____________________
c     Define aluminum front plate (with a 50mm diameter hole to let the beam go through)
c     which is an element from the "box" containing the calorimeter

         thickaluwall = 0.2
         icvol13(1) = 7.93
         icvol13(2) = 16.25
         icvol13(3) = thickaluwall
         aluwallpos(1) = 0. 
         aluwallpos(2) = 0.
         aluwallpos(3) = -ic_pos(3) + ic_front(3) - 7.5 - thickaluwall
         call gsvolu('ALP ','BOX ',clas_med_alu,icvol13,3,IVOL)
         call gspos('ALP ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'MANY')
         icvol13l(1) = 7.0
         icvol13l(2) = 16.2
         icvol13l(3) = thickaluwall
         icvol13l(4) = 4.285
         rot_mat = get_rotm()
         thexal = 90.
         phixal = 90.
         theyal = 180.
         phiyal = 0.
         thezal = 90.
         phizal = 180.
         call mat_cor(thexal,phixal,theyal,phiyal,thezal,phizal)
         call gsrotm(rot_mat,thexal,phixal,theyal,phiyal,thezal,phizal)
         call gsvolu('ALPL','TRD1',clas_med_alu,icvol13l,4,IVOL)
         call gspos('ALPL',1,'IC0 ',aluwallpos(1)+icvol13(1)+icvol13l(4),aluwallpos(2),aluwallpos(3),rot_mat,'MANY')
         icvol13r(1) = 7.0
         icvol13r(2) = 16.2
         icvol13r(3) = thickaluwall
         icvol13r(4) = 4.285
         rot_mat = get_rotm()
         thexal = 90.
         phixal = 90.
         theyal = 0.
         phiyal = 0.
         thezal = 90.
         phizal = 0.
         call mat_cor(thexal,phixal,theyal,phiyal,thezal,phizal)
         call gsrotm(rot_mat,thexal,phixal,theyal,phiyal,thezal,phizal)
         call gsvolu('ALPR','TRD1',clas_med_alu,icvol13r,4,IVOL)
         call gspos('ALPR',1,'IC0 ',aluwallpos(1)-icvol13(1)-icvol13r(4),aluwallpos(2),aluwallpos(3),rot_mat,'MANY') 
         icvol14(1) = 0. 
         icvol14(2) = 2.5
         icvol14(3) = thickaluwall
         call gsvolu('ALPH','TUBE',clas_med_air,icvol14,3,IVOL)
         call gspos('ALPH',1,'IC0 ',0.,0.,aluwallpos(3),0,'ONLY')

c     Define the side parts of the support frames
         
         icvol15(1) = 0.2
         icvol15(2) = 0.2
         icvol15(3) = 1.1/2.
         icvol15(4) = 1.4/2.
         icvol15(5) = 16./2.
         call gsvolu('ALSP','TRD2',clas_med_alu,icvol15,5,IVOL)
     
c     crystals for y >= 0
         
c     1st part of the calorimeter : j=0..11 and i=0..11
         
         myyj = 0.
         myzj = 0.
         mgapy = 0.
         mgapz = 0.
         pyyj = 0.
         pyzj = 0.
         crwrnb = 0
         crnb = 0
         
         do j=0,11
            
            if (j.eq.0) then
               myyj = 0.
               myzj = 0.
               mgapy = 0.
               mgapz = 0.
               pyyj = 0.
               pyzj = 0.
            elseif (j.eq.1) then
               myyj = 0.
               myzj = 0.
               mgapy = gap*cos(alphar/2.)
               mgapz = gap*sin(alphar/2.)
               pyyj = gap*sin(alphar/2.)*tan(alphar/2.)
     &              + 2.*icvol1w(1)*sin(alphar)*tan(alphar/2.)
               pyzj = gap*sin(alphar/2.) + 2.*icvol1w(1)*sin(alphar)
            else
               myyj = myyj + 2.*cos((j-1.)*alphar)
               myzj = myzj + 2.*sin((j-1.)*alphar)
               mgapy = mgapy + gap*cos((2.*j-1.)*alphar/2.)
               mgapz = mgapz + gap*sin((2.*j-1.)*alphar/2.)
               pyyj = pyyj + gap*sin((2.*j-1.)*alphar/2.)*tan((2.*j-1.)*alphar/2.)
     &              + 2.*icvol1w(1)*sin(j*alphar)*tan((2.*j-1.)*alphar/2.)
               pyzj = pyzj + gap*sin((2.*j-1.)*alphar/2.)
     &              + 2.*icvol1w(1)*sin(j*alphar)
            endif
            
            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.
            
            do i=0,11
               
               rot_matrix = get_rotm()
               thex = 90. + i*alphad
               phix = 0.
               they = 90. + j*alphad
               phiy = 90.
               thez = 0.
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)

               if (i.eq.0) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 0.
                  pxzi = 0.
               elseif (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w(1)*sin(alphar)*tan(alphar/2.)
                  pxzi = 2.*icvol1w(1)*sin(alphar)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar)
                  pxxi = pxxi + 2.*icvol1w(1)*sin(i*alphar)*tan((2.*i-1.)*alphar/2.)
                  pxzi = pxzi + 2.*icvol1w(1)*sin(i*alphar)
               endif
          
               if (i.eq.0) then
                  ibin = 0
               else
                  ibin = 1
               endif
               if (j.eq.0) then
                  jbin = 0
               else
                  jbin = 1
               endif
               
               k = i + j            
               if (k.eq.0) then
                  x = 0.
                  y = 0.
                  z = cfp
               elseif ((i.eq.0).and.(j.eq.1)) then
                  x = 0.
                  y = chh*(1.+cos(alphar)) + pyyj + mgapy
                  z = cfp - chh*sin(alphar) + pyzj - mgapz
               else
                  x = ibin * (chh*(1.+cos(i*alphar)+mxxi) + pxxi)
                  y = jbin * (chh*(1.+cos(j*alphar)+myyj) + pyyj + mgapy)
                  z = cfp + jbin * (- chh*(sin(j*alphar)+myzj) + pyzj - mgapz)
     &                 + ibin * (- chh*(sin(i*alphar)+mxzi) + pxzi)
               endif
               
               if ((k.ge.4).and.(k.le.16)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               elseif ((i.eq.0).and.(j.eq.3)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb1 = 421 
                  crnb1 = 421
                  call gspos('CRWR',crwrnb1,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb1,'IC0 ',x,y,z,rot_matrix,'ONLY')
               elseif ((i.eq.3).and.(j.eq.0)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb2 = 422 
                  crnb2 = 422
                  call gspos('CRWR',crwrnb2,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb2,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
            
c     position of the stainless steel foils (+ air foils) for j=0...11
            
               if (i.eq.0) then
                  xf = 0.
                  yf = y
     &                 - chh*cos(j*alphar)
     &                 - thickfoil*cos((j-0.5)*alphar)
                  zf = z
     &                 + chh*sin(j*alphar)
     &                 + thickfoil*sin((j-0.5)*alphar)
                  rot_matrix = get_rotm()
                  thexf = thex
                  phixf = phix
                  theyf = they - alphad/2.
                  phiyf = phiy
                  thezf = thez - alphad/2.
                  phizf = phiz               
                  call mat_cor(thexf,phixf,theyf,phiyf,thezf,phizf) 
                  call gsrotm(rot_matrix,thexf,phixf,theyf,phiyf,thezf,phizf)
                  ssfnb = j
                  afnb = j
                  cafnb = j
                  if (j.le.5) then
                     call gspos('SSF1',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF1 ',afnb,'SSF1',0.,0.,0.,0,'MANY')
                  elseif (j.eq.6) then
                     call gspos('SSF2',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF2 ',afnb,'SSF2',0.,0.,0.,0,'MANY')
                  elseif (j.eq.7) then
                     call gspos('SSF3',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF3 ',afnb,'SSF3',0.,0.,0.,0,'MANY')
                  elseif (j.eq.8) then
                     call gspos('SSF4',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF4 ',afnb,'SSF4',0.,0.,0.,0,'MANY')
                  elseif (j.eq.9) then
                     call gspos('SSF5',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF5 ',afnb,'SSF5',0.,0.,0.,0,'MANY')
                  elseif (j.eq.10) then
                     call gspos('SSF6',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF6 ',afnb,'SSF6',0.,0.,0.,0,'MANY')
                  else
                     call gspos('SSF7',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF7 ',afnb,'SSF7',0.,0.,0.,0,'MANY')
                  endif
                  if (j.le.1) then
                     call gspos('CAF1',cafnb,'IC0 ',xf,yf,zf,rot_matrix,'ONLY')
                  elseif (j.eq.2) then 
                     call gspos('CAF2',cafnb,'IC0 ',xf,yf,zf,rot_matrix,'ONLY')
                  else
                  endif
               else
               endif
               
c     side parts of the support frames

               if (((j.eq.0).or.(j.eq.1).or.(j.eq.2).or.(j.eq.3).or.(j.eq.4).or.(j.eq.5)).and.(i.eq.11)) then
                  rot_mat = get_rotm()
                  thexsf = 90. + 11*alphad + alphad/2.
                  phixsf = 0.
                  theysf = 90. + j*alphad
                  phiysf = 90.
                  thezsf = 0. + alphad/2.
                  phizsf = 0.
                  xsf = x + chh*cos(11*alphar) + 0.21*cos((11-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(11*alphar) - 0.21*sin((11-0.5)*alphar)
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+1,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY') 
               else
               endif
               if ((j.ge.6).and.(i.ge.5).and.(i.le.10).and.((j+i).eq.16)) then
                  rot_mat = get_rotm()
                  thexsf = 90. + i*alphad + alphad/2.
                  phixsf = 0.
                  theysf = 90. + j*alphad
                  phiysf = 90.
                  thezsf = 0. + alphad/2.
                  phizsf = 0.
                  xsf = x + chh*cos(i*alphar) + 0.21*cos((i-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(i*alphar) - 0.21*sin((i-0.5)*alphar)
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+1,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY')
               else
               endif

            enddo
         
c     2nd part of the calorimeter : j=0..11 and i=-1..-11
         
            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.
            
            do i=1,11               

               rot_matrix = get_rotm()
               thex = 90. - i*alphad 
               phix = 0. 
               they = 90. + j*alphad
               phiy = 90.
               thez = 0.
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)
               if (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w(1)*sin(alphar)*tan(alphar/2.)
                  pxzi = 2.*icvol1w(1)*sin(alphar)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar)
                  pxxi = pxxi + 2.*icvol1w(1)*sin(i*alphar)*tan((2.*i-1.)*alphar/2.)
                  pxzi = pxzi + 2.*icvol1w(1)*sin(i*alphar)
               endif
               
               if (j.eq.0) then
                  jbin = 0
               else
                  jbin = 1
               endif
               
               x = -chh*(1.+cos(i*alphar)+mxxi) - pxxi
               y = jbin * (chh*(1.+cos(j*alphar)+myyj) + pyyj + mgapy)
               z = cfp + jbin * (- chh*(sin(j*alphar)+myzj) + pyzj - mgapz)
     &              - chh*(sin(i*alphar)+mxzi) + pxzi
               
               k = i + j
               if ((k.ge.4).and.(k.le.16)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               elseif ((i.eq.3).and.(j.eq.0)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb4 = 424
                  crnb4 = 424
                  call gspos('CRWR',crwrnb4,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb4,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
            
c     side parts of the support frames

               if (((j.eq.0).or.(j.eq.1).or.(j.eq.2).or.(j.eq.3).or.(j.eq.4).or.(j.eq.5)).and.(i.eq.11)) then
                  rot_mat = get_rotm()
                  thexsf = 90. - 11*alphad - alphad/2.
                  phixsf = 0.
                  theysf = 90. + j*alphad
                  phiysf = 90.
                  thezsf = 0. - alphad/2.
                  phizsf = 0.
                  xsf = x - chh*cos(11*alphar) - 0.21*cos((11-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(11*alphar) - 0.21*sin((11-0.5)*alphar) 
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+13,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY') 
               else
               endif
               if ((j.ge.6).and.(i.ge.5).and.(i.le.10).and.((j+i).eq.16)) then
                  rot_mat = get_rotm()
                  thexsf = 90. - i*alphad - alphad/2.
                  phixsf = 0.
                  theysf = 90. + j*alphad
                  phiysf = 90.
                  thezsf = 0. - alphad/2.
                  phizsf = 0.
                  xsf = x - chh*cos(i*alphar) - 0.21*cos((i-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(i*alphar) - 0.21*sin((i-0.5)*alphar) 
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+13,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY')
               else
               endif
  
            enddo
               
         enddo

c     crystals for y < 0
         
c     3rd part of the calorimeter : j=-1..-11 and i=0..11
         
         myyj = 0.
         myzj = 0.
         pyyj = 0.
         pyzj = 0.
         mgapy = 0.
         mgapz = 0.
         
         do j=1,11
                
            if (j.eq.1) then
               myyj = 0.
               myzj = 0.
               mgapy = gap*cos(alphar/2.)
               mgapz = gap*sin(alphar/2.)
               pyyj = gap*sin(alphar/2.)*tan(alphar/2.)
     &              + 2.*icvol1w(1)*sin(alphar)*tan(alphar/2.)
               pyzj = gap*sin(alphar/2.) + 2.*icvol1w(1)*sin(alphar)
            else
               myyj = myyj + 2.*cos((j-1.)*alphar)
               myzj = myzj + 2.*sin((j-1.)*alphar)
               mgapy = mgapy + gap*cos((2.*j-1.)*alphar/2.)
               mgapz = mgapz + gap*sin((2.*j-1.)*alphar/2.)
               pyyj = pyyj + gap*sin((2.*j-1.)*alphar/2.)*tan((2.*j-1.)*alphar/2.)
     &              + 2.*icvol1w(1)*sin(j*alphar)*tan((2.*j-1.)*alphar/2.)
               pyzj = pyzj + gap*sin((2.*j-1.)*alphar/2.)
     &              + 2.*icvol1w(1)*sin(j*alphar)
            endif

            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.

            do i=0,11
               
               rot_matrix = get_rotm()
               thex = 90. + i*alphad 
               phix = 0.
               they = 90. - j*alphad
               phiy = 90.
               thez = 0. 
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)
 
               if (i.eq.0) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 0.
                  pxzi = 0.
               elseif (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w(1)*sin(alphar)*tan(alphar/2.)
                  pxzi = 2.*icvol1w(1)*sin(alphar)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar)
                  pxxi = pxxi + 2.*icvol1w(1)*sin(i*alphar)*tan((2.*i-1.)*alphar/2.)
                  pxzi = pxzi + 2.*icvol1w(1)*sin(i*alphar)
               endif
               
               if (i.eq.0) then
                  ibin = 0
               else
                  ibin = 1
               endif

               if ((i.eq.0).and.(j.eq.1)) then
                  x = 0.
                  y = -chh*(1.+cos(alphar)) - pyyj - mgapy
                  z = cfp - chh*sin(alphar) + pyzj - mgapz
               else   
                  x = ibin * (chh*(1.+cos(i*alphar)+mxxi) + pxxi)
                  y = -chh*(1.+cos(j*alphar)+myyj) - pyyj - mgapy
                  z = cfp - chh*(sin(j*alphar)+myzj) + pyzj - mgapz
     &                 + ibin * (- chh*(sin(i*alphar)+mxzi) + pxzi)
               endif
               
               k = i + j
               if ((k.ge.4).and.(k.le.16)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               elseif ((i.eq.0).and.(j.eq.3)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb3 = 423
                  crnb3 = 423
                  call gspos('CRWR',crwrnb3,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb3,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
               
c     position of the stainless steel foils (+ air foils) for j=-1..-11

               if (i.eq.0) then
                  xf = 0.
                  yf = y
     &                 - chh*cos(-j*alphar)
     &                 - thickfoil*cos((-j-0.5)*alphar)
                  zf = z
     &                 - chh*sin(abs(-j*alphar))
     &                 - thickfoil*sin(abs((-j-0.5)*alphar))
                  rot_matrix = get_rotm()
                  thexf = thex
                  phixf = phix
                  theyf = they - alphad/2.
                  phiyf = phiy
                  thezf = thez - alphad/2.
                  phizf = phiz
                  call mat_cor(thexf,phixf,theyf,phiyf,thezf,phizf)
 
                  call gsrotm(rot_matrix,thexf,phixf,theyf,phiyf,thezf,phizf)
                  ssfnb = -j
                  afnb = -j
                  cafnb = -j
                  if (j.le.5) then
                     call gspos('SSF1',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF1 ',afnb,'SSF1',0.,0.,0.,0,'MANY')
                  elseif (j.eq.6) then
                     call gspos('SSF2',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF2 ',afnb,'SSF2',0.,0.,0.,0,'MANY')
                  elseif (j.eq.7) then
                     call gspos('SSF3',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF3 ',afnb,'SSF3',0.,0.,0.,0,'MANY')
                  elseif (j.eq.8) then
                     call gspos('SSF4',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF4 ',afnb,'SSF4',0.,0.,0.,0,'MANY')
                  elseif (j.eq.9) then
                     call gspos('SSF5',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF5 ',afnb,'SSF5',0.,0.,0.,0,'MANY')
                  elseif (j.eq.10) then
                     call gspos('SSF6',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF6 ',afnb,'SSF6',0.,0.,0.,0,'MANY')
                  else
                     call gspos('SSF7',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF7 ',afnb,'SSF7',0.,0.,0.,0,'MANY')
                  endif
                  if (j.le.1) then
                     call gspos('CAF1',cafnb,'IC0 ',xf,yf,zf,rot_matrix,'ONLY')
                  elseif (j.eq.2) then 
                     call gspos('CAF2',cafnb,'IC0 ',xf,yf,zf,rot_matrix,'ONLY')
                  else
                  endif
               else
               endif
               
c     side parts of the support frames

               if (((j.eq.1).or.(j.eq.2).or.(j.eq.3).or.(j.eq.4).or.(j.eq.5)).and.(i.eq.11)) then
                  rot_mat = get_rotm()
                  thexsf = 90. + 11*alphad + alphad/2.
                  phixsf = 0.
                  theysf = 90. - j*alphad
                  phiysf = 90.
                  thezsf = 0. + alphad/2.
                  phizsf = 0.
                  xsf = x + chh*cos(11*alphar) + 0.21*cos((11-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(11*alphar) - 0.21*sin((11-0.5)*alphar)    
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
      
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+24,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY') 
               else
               endif
               if ((j.ge.6).and.(i.ge.5).and.(i.le.10).and.((j+i).eq.16)) then
                  rot_mat = get_rotm()
                  thexsf = 90. + i*alphad + alphad/2.
                  phixsf = 0.
                  theysf = 90. - j*alphad
                  phiysf = 90.
                  thezsf = 0. + alphad/2.
                  phizsf = 0.
                  xsf = x + chh*cos(i*alphar) + 0.21*cos((i-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(i*alphar) - 0.21*sin((i-0.5)*alphar) 
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+24,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY')
               else
               endif

            enddo
      
c     4th part of the calorimeter : i=-1..-11 and j=-1..-11
                  
            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.

            do i=1,11
               
               rot_matrix = get_rotm()
               thex = 90. - i*alphad
               phix = 0.
               they = 90. - j*alphad
               phiy = 90.
               thez = 0.
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)
               if (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w(1)*sin(alphar)*tan(alphar/2.)
                  pxzi = 2.*icvol1w(1)*sin(alphar)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar)
                  pxxi = pxxi + 2.*icvol1w(1)*sin(i*alphar)*tan((2.*i-1.)*alphar/2.)
                  pxzi = pxzi + 2.*icvol1w(1)*sin(i*alphar)
               endif

               x = -chh*(1.+cos(i*alphar)+mxxi) - pxxi
               y = -chh*(1.+cos(j*alphar)+myyj) - pyyj - mgapy
               z = cfp - chh*(sin(j*alphar)+myzj) + pyzj - mgapz 
     &              - chh*(sin(i*alphar)+mxzi) + pxzi
               
               k = i + j
               if ((k.ge.4).and.(k.le.16)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
               
c     side parts of the support frames

               if (((j.eq.1).or.(j.eq.2).or.(j.eq.3).or.(j.eq.4).or.(j.eq.5)).and.(i.eq.11)) then
                  rot_mat = get_rotm()
                  thexsf = 90. - 11*alphad - alphad/2.
                  phixsf = 0.
                  theysf = 90. - j*alphad
                  phiysf = 90.
                  thezsf = 0. - alphad/2.
                  phizsf = 0.
                  xsf = x - chh*cos(11*alphar) - 0.21*cos((11-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(11*alphar) - 0.21*sin((11-0.5)*alphar)
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+36,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY') 
               else
               endif
               if ((j.ge.6).and.(i.ge.5).and.(i.le.10).and.((j+i).eq.16)) then
                  rot_mat = get_rotm()
                  thexsf = 90. - i*alphad - alphad/2.
                  phixsf = 0.
                  theysf = 90. - j*alphad
                  phiysf = 90.
                  thezsf = 0. - alphad/2.
                  phizsf = 0.
                  xsf = x - chh*cos(i*alphar) - 0.21*cos((i-0.5)*alphar)
                  ysf = y 
                  zsf = z - chh*sin(i*alphar) - 0.21*sin((i-0.5)*alphar)
                  call mat_cor(thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gsrotm(rot_mat,thexsf,phixsf,theysf,phiysf,thezsf,phizsf)
                  call gspos('ALSP',j+36,'IC0 ',xsf,ysf,zsf,rot_mat,'MANY')
               else
               endif
               
            enddo
            
         enddo

c     Define the geometry of the prototype (100 crystals + lead blocks)
      elseif (ic_mod.eq.3) then

c     Use trapezoid shape TRD2 for individual crystal volume IC1

c     Define cristal wrapping shape and dimensions

c     Measured value
         thickwrap_m = 0.0065
c     Value from design
         thickwrap_d = 0.0075
         call gsvolu('CRWR','TRD2',ic_med_mylar,icvol1w_m,5,IVOL)
         call gsatt('CRWR','COLO',4)

c     Define cristal shape and dimensions
         
c     Dimensions from design
         icvol1c(1) = icvol1w_d(1) - thickwrap_d
         icvol1c(2) = icvol1w_d(2) - thickwrap_d
         icvol1c(3) = icvol1w_d(3) - thickwrap_d
         icvol1c(4) = icvol1w_d(4) - thickwrap_d

c     Dimensions from measurement
         icvol1c_m(1) = icvol1w_m(1) - thickwrap_m
         icvol1c_m(2) = icvol1w_m(2) - thickwrap_m
         icvol1c_m(3) = icvol1w_m(3) - thickwrap_m
         icvol1c_m(4) = icvol1w_m(4) - thickwrap_m
         call gsvolu('IC1 ','TRD2',ic_med_pbwo4,icvol1c_m,5,IVOL)

         gap = 0.025
         gap_m = 0.033
c         gap_m = 0.02

         chh_m = icvol1w_m(1) + (icvol1w_m(2)-icvol1w_m(1))/2
         cfp_m = chh_m/(icvol1w_m(2)-icvol1w_m(1))*2*icvol1w_m(5)

         alphar = 2.*atan(chh/cfp)
         alphad = alphar*180./pi

         alphar_m = 2.*atan(chh_m/cfp_m)
         alphad_m = alphar_m*180./pi

         forward = 0.11

c     Use trapezoid shape TRD1 for every stainless steel foil
c     And define dimensions for the different stainless steel foils (a,b,c,d,e,f,g)
         
         thickfoil = 0.006
         lengthfoil = 8.65

c     Only one type/size of foil used for the prototype
         
         betae = 7.5*alphar

         icvol5e(1) = dXe - 0.65*tan(betae)
         icvol5e(2) = dXe + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(betae)
         icvol5e(3) = thickfoil
         icvol5e(4) = lengthfoil
         call gsvolu('SSF5','TRD1',ic_med_steel,icvol5e,4,IVOL)
         call gsatt('SSF5','COLO',2)

c     Use trapezoid shape TRD1 to define air foils which role is to finalise
c     - by 'cutting' - the shape of every stainless steel foil    
         
         thickairfoil = 0.006
         lengthairfoil = 7.55
         
         icvol6e(1) = dXe + 0.45*tan(betae)
         icvol6e(2) = dXe + (2.*icvol1c(5)/cos(alphar/2.)-0.45)*tan(betae)
         icvol6e(3) = thickairfoil
         icvol6e(4) = lengthairfoil
         call gsvolu('AF5 ','TRD1',ic_med_air,icvol6e,4,IVOL)
         call gsatt('AF5 ','COLO',2)

c     Use trapezoid shape TRD1 to define air foils which role
c     is to 'cut' the stainless steel foils in the center of the calorimeter
         
         icvol7a(1) = dXaa - 0.65*tan(5./2.*alphar)
         icvol7a(2) = dXaa + (2.*icvol1c(5)/cos(alphar/2.)+0.65)*tan(5./2.*alphar)
         icvol7a(3) = thickfoil
         icvol7a(4) = lengthfoil
         call gsvolu('CAF1','TRD1',ic_med_air,icvol7a,4,IVOL)
         call gsatt('CAF1','COLO',3)
         
c     Define aluminum plate

         thickplate = 0.25
         lengthplate = lengthfoil

         icvol8(1) = icvol5e(1)
         icvol8(2) = icvol5e(2)
         icvol8(3) = thickplate 
         icvol8(4) = lengthplate
         call gsvolu('ALUP','TRD1',clas_med_alu,icvol8,4,IVOL)

c     Define lead blocks

         icvol9(1) = 10.16
         icvol9(2) = 2.54
         icvol9(3) = 10.16
         call gsvolu('LBOX','BOX ',ic_med_lead,icvol9,3,IVOL)

c     Define elements from aluminum "wall" representing the front parts of the support frames
         
         deltax = 1.3397
         deltay = 1.3709
         
         thickaluframes = 0.822
         icvol10(1) = 7.5*deltax+0.4
         icvol10(2) = 2*deltay
         icvol10(3) = thickaluframes
         aluframespos = -ic_pos(3)+ic_front(3)-0.4-icvol10(3)
         call gsvolu('ALF1','BOX ',clas_med_alu,icvol10,3,IVOL)
         call gspos('ALF1',1,'IC0 ',0.,-5.*deltay,aluframespos,0,'ONLY')

         icvol11(1) = 2.5*deltax+0.4
         icvol11(2) = 2*deltay
         icvol11(3) = thickaluframes
         call gsvolu('ALF2','BOX ',clas_med_alu,icvol11,3,IVOL)
         call gspos('ALF2',1,'IC0 ',5.*deltax,-deltay,aluframespos,0,'ONLY')
         call gspos('ALF2',2,'IC0 ',-5.*deltax,-deltay,aluframespos,0,'ONLY')

c     Define air blocks representing holes in the aluminum "wall"
         
         icvol12(1) = 0.4
         icvol12(2) = 0.4
         icvol12(3) = thickaluframes
         call gsvolu('ALFH','BOX ',ic_med_air,icvol12,3,IVOL)
         h=0
         do hx=3,7
            do hy=0,1
               holex=hx*deltax
               holey=hy*deltay
               h=h+1
               call gspos('ALFH',h,'IC0 ',holex,holey,aluframespos,0,'ONLY')
               h=h+1
               call gspos('ALFH',h,'IC0 ',-holex,holey,aluframespos,0,'ONLY')
            enddo
            do hy=1,2
               holex=hx*deltax
               holey=-hy*deltay
               h=h+1
               call gspos('ALFH',h,'IC0 ',holex,holey,aluframespos,0,'ONLY')
               h=h+1
               call gspos('ALFH',h,'IC0 ',-holex,holey,aluframespos,0,'ONLY')
            enddo
         enddo
         do hx=1,7
            do hy=3,7
               holex=hx*deltax
               holey=-hy*deltay
               h=h+1
               call gspos('ALFH',h,'IC0 ',holex,holey,aluframespos,0,'ONLY')
               h=h+1
               call gspos('ALFH',h,'IC0 ',-holex,holey,aluframespos,0,'ONLY')
            enddo
         enddo
         do hy=3,7
            holey=-hy*deltay
            h=h+1
            call gspos('ALFH',h,'IC0 ',0.,holey,aluframespos,0,'ONLY')
         enddo

c     Define aluminum wall (with a 1.5cm diameter hole to let the beam go through)
c     which is an element from the "box" containing the calorimeter

         thickaluwall = 0.2
         icvol13(1) = 7.5*deltax+0.7
         icvol13(2) = 4*deltay+0.7
         icvol13(3) = thickaluwall
         aluwallpos(1) = 0. 
         aluwallpos(2) = -2.5*deltay
         aluwallpos(3) = -ic_pos(3) + ic_front(3) - 5. - thickaluwall
         call gsvolu('ALW ','BOX ',clas_med_alu,icvol13,3,IVOL)
         call gspos('ALW ',1,'IC0 ',aluwallpos(1),aluwallpos(2),aluwallpos(3),0,'MANY')
         icvol14(1) = 0. 
         icvol14(2) = 2.5
         icvol14(3) = thickaluwall
         call gsvolu('ALWH','TUBE',clas_med_air,icvol14,3,IVOL)
         call gspos('ALWH',1,'IC0 ',0.,0.,aluwallpos(3),0,'ONLY')

c     crystals for y >= 0
         
c     1st part of the calorimeter : j=0,1 and i=0..7
 
         myyj = 0.
         myzj = 0.
         mgapy = 0.
         mgapz = 0.
         pyyj = 0.
         pyzj = 0.
         crwrnb = 0
         crnb = 0
         pbcrnb = 0
        
         do j=0,11
              
            if (j.eq.0) then
               myyj = 0.
               myzj = 0.
               mgapy = 0.
               mgapz = 0.
               pyyj = 0.
               pyzj = 0.
               forwardy = + forward*sin(abs((alphar_m-alphar)-alphar_m/2.))
               forwardz = - forward*cos((alphar_m-alphar)-alphar_m/2.)
            elseif (j.eq.1) then
               myyj = 0.
               myzj = 0.
               mgapy = gap_m*cos(alphar_m/2.)
               mgapz = gap_m*sin(alphar_m/2.)
               pyyj = gap_m*sin(alphar_m/2.)*tan(alphar_m/2.)
     &              - 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)
               pyzj = gap_m*sin(alphar_m/2.)
               forwardy = - forward*sin(alphar_m+(alphar_m-alphar)-alphar_m/2.)
               forwardz = - forward*cos(alphar_m+(alphar_m-alphar)-alphar_m/2.)
            else
               myyj = myyj + 2.*cos((j-1.)*alphar_m)
               myzj = myzj + 2.*sin((j-1.)*alphar_m)
               mgapy = mgapy + gap_m*cos((2.*j-1.)*alphar_m/2.)
               mgapz = mgapz + gap_m*sin((2.*j-1.)*alphar_m/2.)
               pyyj = pyyj + (gap_m*sin((2.*j-1.)*alphar_m/2.)
     &              + 2.*icvol1w_m(2)*sin((j-1.)*alphar_m))*tan((2.*j-1.)*alphar_m/2.)
     &              - 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)             
               pyzj = pyzj + gap_m*sin((2.*j-1.)*alphar_m/2.)
     &              + 2.*icvol1w_m(2)*sin((j-1.)*alphar_m)
               forwardy = - forward*sin(j*alphar_m+(alphar_m-alphar)-alphar_m/2.)
               forwardz = - forward*cos(j*alphar_m+(alphar_m-alphar)-alphar_m/2.)
            endif
               
            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.
         
            do i=0,11
        
               rot_matrix = get_rotm()
               thex = 90. + i*alphad_m
               phix = 0.
               they = 90. + j*alphad_m + (alphad_m - alphad)
               phiy = 90.
               thez = 0.
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)

               if (i.eq.0) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 0.
                  pxzi = 0.
               elseif (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)
                  pxzi = 2.*icvol1w_m(1)*sin(alphar_m)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar_m)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar_m)
                  pxxi = pxxi + 2.*icvol1w_m(1)*sin(i*alphar_m)*tan((2.*i-1.)*alphar_m/2.)
                  pxzi = pxzi + 2.*icvol1w_m(1)*sin(i*alphar_m)
               endif
                        
               if (i.eq.0) then
                  ibin = 0
               else
                  ibin = 1
               endif
               if (j.eq.0) then
                  jbin = 0
               else
                  jbin = 1
               endif
               
               k = i + j            
               if (k.eq.0) then
                  x = 0.
                  y = forwardy
                  z = cfp + forwardz             
               elseif ((i.eq.0).and.(j.eq.1)) then
                  x = 0.
                  y = chh_m*(1.+cos(alphar_m)) + pyyj + mgapy + forwardy
                  z = cfp - chh_m*sin(alphar_m) + pyzj - mgapz + forwardz
               else
                  x = ibin * (chh_m*(1.+cos(i*alphar_m)+mxxi) + pxxi)
                  y = jbin * (chh_m*(1.+cos(j*alphar_m)+myyj) + pyyj + mgapy) + forwardy
                  z = cfp + jbin * (- chh_m*(sin(j*alphar_m)+myzj) + pyzj - mgapz)
     &                 + ibin * (- chh_m*(sin(i*alphar_m)+mxzi) + pxzi) + forwardz
               endif

               
c     Putting only 100 crystals

               if (((i.ge.3).and.(i.le.7)).and.(j.le.1)) then               
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY') 
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
            
c     position of the stainless steel foils (+ air foils) for j=0,1
            
               if (i.eq.0) then
                  xf = 0.
                  yf = y - forwardy
     &                 - chh_m*cos(j*alphar_m+(alphar_m-alphar))
     &                 - thickfoil*cos((j-0.5)*alphar_m+(alphar_m-alphar))
                  zf = z - forwardz
     &                 + chh_m*sin(j*alphar_m+(alphar_m-alphar))
     &                 + thickfoil*sin((j-0.5)*alphar_m+(alphar_m-alphar))
                  rot_matrix = get_rotm()
                  thexf = thex
                  phixf = phix
                  theyf = they - alphad_m/2.
                  phiyf = phiy
                  thezf = thez - alphad_m/2.
                  phizf = phiz               
                  call mat_cor(thexf,phixf,theyf,phiyf,thezf,phizf)
                  call gsrotm(rot_matrix,thexf,phixf,theyf,phiyf,thezf,phizf)
                  ssfnb = j
                  afnb = j
                  cafnb = j
                  if (j.le.1) then
                     call gspos('SSF5',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF5 ',afnb,'SSF5',0.,0.,0.,0,'MANY')
                  else
                  endif

                  if (j.le.1) then
                     call gspos('CAF1',cafnb,'IC0 ',xf,yf,zf,rot_matrix,'ONLY')
                  else
                  endif
               else
               endif

c     Position of the aluminium plate

               if ((i.eq.0).and.(j.eq.1)) then
                  xp = 0.
                  yp = y + chh_m*cos(alphar_m+(alphar_m-alphar))
     &                 + (0.2 + thickplate)*cos(alphar_m+(alphar_m-alphar)+0.5*alphar_m)
                  zp = z - chh_m*sin(alphar_m+(alphar_m-alphar))
     &                 - (0.2 + thickplate)*sin(alphar_m+(alphar_m-alphar)+0.5*alphar_m)
                  rot_matrix = get_rotm()
                  thexp = 90.
                  phixp = 0.
                  theyp = 90. + alphad_m + (alphad_m - alphad) + 0.5*alphad_m
                  phiyp = 90.
                  thezp = alphad_m + (alphad_m - alphad) + 0.5*alphad_m
                  phizp = 0.
                  call mat_cor(thexp,phixp,theyp,phiyp,thezp,phizp)
                  call gsrotm(rot_matrix,thexp,phixp,theyp,phiyp,thezp,phizp)
                  call gspos('ALUP',1,'IC0 ',xp,yp,zp,rot_matrix,'ONLY')

c     Position of the lead box

                  xb = 0.
                  yb = yp + (thickplate+icvol9(2))*cos(alphar_m+(alphar_m-alphar)+0.5*alphar_m)
                  zb = zp - (thickplate+icvol9(2))*sin(alphar_m+(alphar_m-alphar)+0.5*alphar_m)
                  rot_matrix = get_rotm()
                  thexb = thexp
                  phixb = phixp
                  theyb = theyp
                  phiyb = phiyp
                  thezb = thezp
                  phizb = phizp
                  call mat_cor(thexb,phixb,theyb,phiyb,thezb,phizb)
                  call gsrotm(rot_matrix,thexb,phixb,theyb,phiyb,thezb,phizb)
                  call gspos('LBOX',1,'IC0 ',xb,yb,zb,rot_matrix,'ONLY')
                  call gsatt('LBOX','COLO',2)

               else
               endif

            enddo
         
c     2nd part of the calorimeter : j=0,1 and i=-1..-7
         
            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.
         
            do i=1,11
               
               rot_matrix = get_rotm()
               thex = 90. - i*alphad_m 
               phix = 0. 
               they = 90. + j*alphad_m + (alphad_m - alphad)
               phiy = 90.
               thez = 0.
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)
               if (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)
                  pxzi = 2.*icvol1w_m(1)*sin(alphar_m)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar_m)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar_m)
                  pxxi = pxxi + 2.*icvol1w_m(1)*sin(i*alphar_m)*tan((2.*i-1.)*alphar_m/2.)
                  pxzi = pxzi + 2.*icvol1w_m(1)*sin(i*alphar_m)
               endif           
               
               if (j.eq.0) then
                  jbin = 0
               else
                  jbin = 1
               endif
               
               x = -chh_m*(1.+cos(i*alphar_m)+mxxi) - pxxi
               y = jbin * (chh_m*(1.+cos(j*alphar_m)+myyj) + pyyj + mgapy) + forwardy
               z = cfp + jbin * (- chh_m*(sin(j*alphar_m)+myzj) + pyzj - mgapz)
     &              - chh_m*(sin(i*alphar_m)+mxzi) + pxzi + forwardz
               
c     Putting only 100 crystals

               if (((i.ge.3).and.(i.le.7)).and.(j.le.1)) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY') 
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
               
            enddo

         enddo

c     crystals for y < 0
         
c     3rd part of the calorimeter : j=-1..-6 and i=0..7
         
         myyj = 0.
         myzj = 0.
         mgapy = 0.
         mgapz = 0.
         pyyj = 0.
         pyzj = 0.

         do j=1,11
                    
            if (j.eq.1) then
               myyj = 0.
               myzj = 0.
               mgapy = gap_m*cos(alphar_m/2.)
               mgapz = gap_m*sin(alphar_m/2.)
               pyyj = gap_m*sin(alphar_m/2.)*tan(alphar_m/2.)
     &              - 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)              
               pyzj = gap_m*sin(alphar_m/2.)
               forwardy = + forward*sin(abs(-alphar_m+(alphar_m-alphar)-alphar_m/2.))
               forwardz = - forward*cos(-alphar_m+(alphar_m-alphar)-alphar_m/2.)
            else
               myyj = myyj + 2.*cos((j-1.)*alphar_m)
               myzj = myzj + 2.*sin((j-1.)*alphar_m)
               mgapy = mgapy + gap_m*cos((2.*j-1.)*alphar_m/2.)
               mgapz = mgapz + gap_m*sin((2.*j-1.)*alphar_m/2.)
               pyyj = pyyj + (gap_m*sin((2.*j-1.)*alphar_m/2.)
     &              + 2.*icvol1w_m(2)*sin((j-1.)*alphar_m))*tan((2.*j-1.)*alphar_m/2.)
     &              - 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)              
               pyzj = pyzj + gap_m*sin((2.*j-1.)*alphar_m/2.)
     &              + 2.*icvol1w_m(2)*sin((j-1.)*alphar_m)             
               forwardy = + forward*sin(abs(-j*alphar_m+(alphar_m-alphar)-alphar_m/2.))
               forwardz = - forward*cos(-j*alphar_m+(alphar_m-alphar)-alphar_m/2.)
            endif
            
            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.
            
            do i=0,11
               
               rot_matrix = get_rotm()
               thex = 90. + i*alphad_m 
               phix = 0.
               they = 90. - j*alphad_m + 2*(alphad_m - alphad)
               phiy = 90.
               thez = 0.
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)
               if (i.eq.0) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 0.
                  pxzi = 0.
               elseif (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)
                  pxzi = 2.*icvol1w_m(1)*sin(alphar_m)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar_m)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar_m)
                  pxxi = pxxi + 2.*icvol1w_m(1)*sin(i*alphar_m)*tan((2.*i-1.)*alphar_m/2.)
                  pxzi = pxzi + 2.*icvol1w_m(1)*sin(i*alphar_m)
               endif
               
               if (i.eq.0) then
                  ibin = 0
               else
                  ibin = 1
               endif
               
               if ((i.eq.0).and.(j.eq.1)) then
                  x = 0.
                  y = -chh_m*(1.+cos(alphar_m)) - pyyj - mgapy + forwardy
                  z = cfp - chh_m*sin(alphar_m) + pyzj - mgapz + forwardz 
               else   
                  x = ibin * (chh_m*(1.+cos(i*alphar_m)+mxxi) + pxxi)
                  y = -chh_m*(1.+cos(j*alphar_m)+myyj) - pyyj - mgapy + forwardy
                  z = cfp + jbin * (- chh_m*(sin(j*alphar_m)+myzj) + pyzj - mgapz)
     &                 + ibin * (- chh_m*(sin(i*alphar_m)+mxzi) + pxzi) + forwardz
               endif
               
c     Putting only 100 crystals

               if ((((i.ge.3).and.(i.le.7)).and.((j.eq.1).or.(j.eq.2)))
     &              .or.((i.le.7).and.((j.ge.3).and.(j.le.6)))) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
         
c     position of the stainless steel foils (+ air foils) for j=-1..-6

               if (i.eq.0) then
                  xf = 0.
                  yf = y - forwardy
     &                 - chh_m*cos(-j*alphar_m+(alphar_m-alphar))
     &                 - thickfoil*cos((-j-0.5)*alphar_m+(alphar_m-alphar))
                  zf = z - forwardz
     &                 - chh_m*sin(abs(-j*alphar_m+(alphar_m-alphar)))
     &                 - thickfoil*sin(abs((-j-0.5)*alphar_m+(alphar_m-alphar)))
                  rot_matrix = get_rotm()
                  thexf = thex
                  phixf = phix
                  theyf = they - alphad_m/2.
                  phiyf = phiy
                  thezf = thez - alphad_m/2.
                  phizf = phiz
                  call mat_cor(thexf,phixf,theyf,phiyf,thezf,phizf)
                  call gsrotm(rot_matrix,thexf,phixf,theyf,phiyf,thezf,phizf)
                  ssfnb = -j
                  afnb = -j
                  cafnb = -j
                  if (j.le.6) then
                     call gspos('SSF5',ssfnb,'IC0 ',xf,yf,zf,rot_matrix,'MANY')
                     call gspos('AF5 ',afnb,'SSF5',0.,0.,0.,0,'MANY')
                  else
                  endif
                  
                  if (j.le.2) then
                     call gspos('CAF1',cafnb,'IC0 ',xf,yf,zf,rot_matrix,'ONLY')
                  else
                  endif
               else
               endif
               
            enddo
       
c     4th part of the calorimeter : j=-1..-6 and i=-1..-7
      
            mxxi = 0.
            mxzi = 0.
            pxxi = 0.
            pxzi = 0.
         
            do i=1,11
                           
               rot_matrix = get_rotm()
               thex = 90 - i*alphad_m
               phix = 0.
               they = 90. - j*alphad_m + 2*(alphad_m - alphad)
               phiy = 90.
               thez = 0.
               phiz = 0.
               call mat_cor(thex,phix,they,phiy,thez,phiz)
               if (i.eq.1) then
                  mxxi = 0.
                  mxzi = 0.
                  pxxi = 2.*icvol1w_m(1)*sin(alphar_m)*tan(alphar_m/2.)
                  pxzi = 2.*icvol1w_m(1)*sin(alphar_m)
               else
                  mxxi = mxxi + 2.*cos((i-1.)*alphar_m)
                  mxzi = mxzi + 2.*sin((i-1.)*alphar_m)
                  pxxi = pxxi + 2.*icvol1w_m(1)*sin(i*alphar_m)*tan((2.*i-1.)*alphar_m/2.)
                  pxzi = pxzi + 2.*icvol1w_m(1)*sin(i*alphar_m)
               endif           
                
               x = -chh_m*(1.+cos(i*alphar_m)+mxxi) - pxxi
               y = -chh_m*(1.+cos(j*alphar_m)+myyj) - pyyj - mgapy + forwardy
               z = cfp - chh_m*(sin(j*alphar_m)+myzj) + pyzj - mgapz 
     &              - chh_m*(sin(i*alphar_m)+mxzi) + pxzi + forwardz

c     Putting only 100 crystals

               if ((((i.ge.3).and.(i.le.7)).and.((j.eq.1).or.(j.eq.2))) 
     &              .or.((i.le.7).and.((j.ge.3).and.(j.le.6)))) then
                  call gsrotm(rot_matrix,thex,phix,they,phiy,thez,phiz)
                  crwrnb = crwrnb + 1
                  crnb = crnb + 1
                  call gspos('CRWR',crwrnb,'IC0 ',x,y,z,rot_matrix,'MANY')
                  call gspos('IC1 ',crnb,'IC0 ',x,y,z,rot_matrix,'ONLY')
               else
               endif
               
            enddo
            
         enddo
           
      else
      endif
           
	   
	  
C Modif Ahmed Fradi 31/08/2011: New IC shielding for e1dvcs2, eg1dvcs           
       
     
        if((tg_type_ff.eq.28).or.(tg_type_ff.eq.30).or.(tg_type_ff.eq.31)) then
       
C        Wrapping shield of IC:octagonal shape

C        OCTAGONE1

        
	 icback1(1)=45./2.
         icback1(2)=360.
         icback1(3)=8.
         icback1(4)=2.
         icback1(5)=-0.2
         icback1(6)=7.
         icback1(7)=24.4
         icback1(8)=0.2
         icback1(9)=7.
         icback1(10)=24.4      
         icback1pos(1) = 0.
         icback1pos(2) = 0.
         icback1pos(3) = -ic_pos(3) + ic_front(3) + 2.*icvol1c(5) 
     &     + 14.4 - icback1(8)
        
        call gsvolu('ICT1','PGON',ic_med_lead,icback1,10,IVOL)
        call gspos('ICT1',1,'IC0 ',icback1pos(1),icback1pos(2),
     &  icback1pos(3),0,'ONLY')



C        TUBE d'air pour avoir la bonne forme:


           icbackair(1)=0.
           icbackair(2)=8.
           icbackair(3)=0.2

           call gsvolu('ICT0','TUBE',ic_med_air,icbackair,3,IVOL)
        call gspos('ICT0',1,'IC0 ',icback1pos(1),icback1pos(2),
     &  icback1pos(3),0,'ONLY')

            
       
C          Backward shield for mollers 

           icbackmo1(1)=8.22
           icbackmo1(2)=23.42
           icbackmo1(3)=0.32
           icbackmo1pos(1)=0.
	   icbackmo1pos(2)=0.
	   icbackmo1pos(3)=icback1pos(3)+ icback1(8)+ 2. + icbackmo1(3)
           
	   call gsvolu('ICM1','TUBE',ic_med_steel,icbackmo1,3,IVOL)
        call gspos('ICM1',1,'IC0 ',icbackmo1pos(1),icbackmo1pos(2),
     &  icbackmo1pos(3),0,'ONLY')
     
       
       
           icbackmo2(1)=7.80
           icbackmo2(2)=8.22
           icbackmo2(3)=7.19
           icbackmo2pos(1)=0.
	   icbackmo2pos(2)=0.
	   icbackmo2pos(3)=icbackmo1pos(3)+icbackmo2(3)
           
	   call gsvolu('ICM2','TUBE',ic_med_steel,icbackmo2,3,IVOL)
        call gspos('ICM2',1,'IC0 ',icbackmo2pos(1),icbackmo2pos(2),
     &  icbackmo2pos(3),0,'ONLY')
     
     
        
       
           icbackmo3(1)=3.70
           icbackmo3(2)=7.80
           icbackmo3(3)=0.32
           icbackmo3pos(1)=0.
	   icbackmo3pos(2)=0.
	   icbackmo3pos(3)=icbackmo2pos(3)+icbackmo2(3)
           
	   call gsvolu('ICM3','TUBE',ic_med_steel,icbackmo3,3,IVOL)
        call gspos('ICM3',1,'IC0 ',icbackmo3pos(1),icbackmo3pos(2),
     &  icbackmo3pos(3),0,'ONLY')
     
       
C                Beam line tube         
       
                   
           icbackmo5(1)=3.20
           icbackmo5(2)=3.70
           icbackmo5(3)=25.88
           icbackmo5pos(1)=0.
	   icbackmo5pos(2)=0.
	   icbackmo5pos(3)=icbackmo3pos(3)+icbackmo5(3)
           
	   call gsvolu('ICM5','TUBE',ic_med_steel,icbackmo5,3,IVOL)
        call gspos('ICM5',1,'IC0 ',icbackmo5pos(1),icbackmo5pos(2),
     &  icbackmo5pos(3),0,'MANY')
     


c           Downstream     beam pipe vaccum window   
        
           icbackmo4(1)=0.
           icbackmo4(2)=2.375
           icbackmo4(3)=0.00355
           icbackmo4pos(1)=0.
	   icbackmo4pos(2)=0.
	   icbackmo4pos(3)=icbackmo3pos(3)-icbackmo3(3)+icbackmo4(3)+ic_pos(3)
             
	     
	                
	   call gsvolu('ICM4','TUBE',clas_med_alu,icbackmo4,3,IVOL)
        call gspos('ICM4',1,'FOIL',icbackmo4pos(1),icbackmo4pos(2),
     &  icbackmo4pos(3),0,'ONLY')
     
       

     
C               Vacuum pipe

               
            parvacum(1)=0.0
	    parvacum(2)=2.375
	    parvacum(3)=274.5    
                                   

                   
          vacumz=icbackmo4pos(3)+icbackmo4(3)+ 274.5      
                   
          call gsvolu('ICMV','TUBE',clas_med_vac,parvacum,3,IVOL)  ! vacuum pipe
	  call gspos('ICMV',1,'FOIL',0.0,0.0,vacumz,0,'ONLY')       
       

C             We put now the lead

            
	 icback0pos(1) = 0.
         icback0pos(2) = 0.
         icback0pos(3) = icbackmo1pos(3)+icbackmo1(3) 
	   
	 icback0(1)=45./2.
         icback0(2)=360.
         icback0(3)=10
        
	 icback0(4)=0.
         icback0(5)=8.22
         icback0(6)=23.42
        
	 icback0(7)=icback0(4)+5.
	 icback0(8)=8.22
         icback0(9)=23.42
        
	 icback0(10)=icback0(7)
	 icback0(11)=8.22
	 icback0(12)=13.41
        
	 icback0(13)=icback0(10)+9.60
	 icback0(14)=8.22
         icback0(15)=13.41
	
	 
	 icback0(16)=icback0(13)
	 icback0(17)=3.7
	 icback0(18)=13.41
        
	 icback0(19)=icback0(16)+11.43
	 icback0(20)=3.7
         icback0(21)=13.41
	
	 icback0(22)=icback0(19)
	 icback0(23)=3.7
	 icback0(24)=10.
	
	 icback0(25)=icback0(22)+14.96
	 icback0(26)=3.7
	 icback0(27)=10.
	
	 icback0(28)=icback0(25)
	 icback0(29)=3.7
	 icback0(30)=6.21
	
	 icback0(31)=icback0(28)+12.50
	 icback0(32)=3.7
	 icback0(33)=6.21
	 
	 	 
          	 
	 
        call gsvolu('ICMS','PCON',ic_med_lead,icback0,33,IVOL)
        call gspos('ICMS',1,'IC0 ',icback0pos(1),icback0pos(2),
     &  icback0pos(3),0,'ONLY')

 
	                 
                
           icbackmo6(1)=3.70
           icbackmo6(2)=6.21
           icbackmo6(3)=18.4
           icbackmo6pos(1)=0.
	   icbackmo6pos(2)=0.
	   icbackmo6pos(3)=icback0pos(3)+icback0(31)+2.05+icbackmo6(3)
           
	   call gsvolu('ICM6','TUBE',ic_med_lead,icbackmo6,3,IVOL)
        call gspos('ICM6',1,'IC0 ',icbackmo6pos(1),icbackmo6pos(2),
     &  icbackmo6pos(3),0,'ONLY')
       
       
           
C        OCTAGONE2

        
	 icback2(1)=45./2.
         icback2(2)=360.
         icback2(3)=8.
         icback2(4)=2.
         icback2(5)=-6.8
         icback2(6)=24.
         icback2(7)=24.4
         icback2(8)=6.8
         icback2(9)=24.
         icback2(10)=24.4
       
         icback2pos(1) = 0.
         icback2pos(2) = 0.
         icback2pos(3) = -ic_pos(3) + ic_front(3) + 2.*icvol1c(5) 
     &   + icback2(8)+ 2*icback1(8)
        
        call gsvolu('ICT2','PGON',ic_med_lead,icback2,10,IVOL)
        call gspos('ICT2',1,'IC0 ',icback2pos(1),icback2pos(2),
     &  icback2pos(3),0,'ONLY')



C        OCTAGONE3

        
	 icback3(1)=45./2.
         icback3(2)=360.
         icback3(3)=8.
         icback3(4)=2.
         icback3(5)=-4.
         icback3(6)=22.4
         icback3(7)=22.8
         icback3(8)=4.
         icback3(9)=24.
         icback3(10)=24.4
       
       
        call gsvolu('ICT3','PGON',ic_med_lead,icback3,10,IVOL)
        call gspos('ICT3',1,'IC0 ',icback2pos(1),icback2pos(2),
     &  icback2pos(3)-icback2(8)-icback3(8),0,'ONLY')


           if(tg_type_ff.eq.30) then


C        OCTAGONE4

        
	 icback4(1)=45./2.
         icback4(2)=360.
         icback4(3)=8.
         icback4(4)=2.
         icback4(5)=-8.8
         icback4(6)=18.6
         icback4(7)=18.8
         icback4(8)=8.8
         icback4(9)=22.6
         icback4(10)=22.8
       
       
        call gsvolu('ICT4','PGON',ic_med_lead,icback4,10,IVOL)
        call gspos('ICT4',1,'IC0 ',icback2pos(1),icback2pos(2),
     &  icback2pos(3)-icback2(8)-2*icback3(8)-icback4(8),0,'ONLY')

        else
	endif
	
	
	    
	   else 
	  
	  
C      Old Shielding done by Rustam
   
c     Lead shielding behind the calorimeter to trap Moller electrons
   
      shldpos(1) = 0.
      shldpos(2) = 0.
      shldpos(3) = -ic_pos(3) + ic_front(3) + 2.*icvol1c(5) + 16. + icsh1(1)
      
      call gsvolu('ICS1','CONE',ic_med_lead,icsh1,5,IVOL)
      call gspos('ICS1',1,'IC0 ',shldpos(1),shldpos(2),shldpos(3),0,'ONLY')
      icshf(1)=24.58
      icshf(2)=24.8
      icshf(3)=3.5
C     Rustam Niyazov Lead sheet around the back of the IC (2.2 mm thick); top part
      call gsvolu('ICSF','TUBE',ic_med_lead,icshf,3,IVOL)
      call gspos('ICSF',1,'IC0 ',shldpos(1),shldpos(2),shldpos(3)-icsh1(1)-3.5,0,'ONLY')

      icshf(1)=17.58
      icshf(2)=24.8
      icshf(3)=0.11
C     Rustam Niyazov Lead sheet around the back of the IC (2.2 mm thick); back part
      call gsvolu('ICSR','TUBE',ic_med_lead,icshf,3,IVOL)
      call gspos('ICSR',1,'IC0 ',shldpos(1),shldpos(2),shldpos(3)-icsh1(1)-0.22,0,'ONLY')

      call gsvolu('ICS2','CONE',ic_med_lead,icsh2,5,IVOL)
      call gspos('ICS2',1,'IC0 ',shldpos(1),shldpos(2),shldpos(3)+icsh1(1)+icsh2(1),0,'ONLY')

      call gsvolu('ICS3','TUBE',ic_med_lead,icsh3,3,IVOL)
      call gspos('ICS3',1,'IC0 ',shldpos(1),shldpos(2),shldpos(3)+icsh1(1)+2.*icsh2(1)+icsh3(3),0,'ONLY')

      call gsvolu('ICSI','TUBE',ic_med_lead,icshi,3,IVOL)
      call gspos('ICSI',1,'IC0 ',shldpos(1),shldpos(2),shldpos(3)+icsh1(1)+icshi(3),0,'ONLY')

      call gsvolu('ICS4','TUBE',ic_med_lead,icsh4,3,IVOL)
      call gspos('ICS4',1,'IC0 ',shldpos(1),shldpos(2),-ic_pos(3)+87.5+icsh4(3),0,'ONLY')
      
      call gsvolu('ICVP','TUBE',clas_med_vac,parvac,3,IVOL)  ! vacuum pipe
      call gspos('ICVP',1,'FOIL',0.0,0.0,vacz,0,'ONLY')
      
     
     
           endif
      
      
      end

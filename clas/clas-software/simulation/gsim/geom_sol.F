      subroutine geom_sol

c Saclay solenoid coil and cryostat volumes for DVCS experiment
c Dimensions provided by M. Garcon

      implicit none
      character*(*) crname
      parameter (crname = 'GEOM_SOL')

#include "cltmed.inc"
#include "clmate.inc"
#include "ffpar_tg.inc"
#include "ffpar_mg.inc"

#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gclist.inc"
#include "gconst.inc"
#include "ffpar.inc"
#include "ffpar_ptg.inc"
#include "ptgtmed.inc"
#include "lcgeom.inc"
#include "ffpar_foil.inc"

c     Temporary -- Possibility to choose between the geometry of the Saclay solenoid
c                  and the geometry of the EG1 Helmholtz coils (polarized target) from geom_ptg.F


c  -- Parameters needed to define the geometry of the Saclay solenoid --

         real soln(3)
         real coil1(12),coil2(12),coil3(15),coil4(3),coil5(15),coil6(15)
         real cryo1(3),cryo2(3),cryo3(3)
         real coilz(4),cryoz(3)
         integer ivol
         
c         call recmes(crname,'I','SOLENOID geometry routine begun...')
         
         data soln/11.2,47.,30.0/
         
         data coil1/0.0 ,360.0 ,3  ,-28.0 ,11.5 ,46.0 ,16.3 ,
     +        11.5 ,46.0 ,28.0 ,31.4 ,46.0/
         data coil2/0.0 ,360.0 ,3  ,-27.5 ,12. ,45.5 ,16.1 ,
     +        12. ,45.5 ,27.5 ,31.7 ,45.5/
         data coil3/0.0 ,360.0 ,4  ,-14.6,28.8,28.8, -7.8, 17.,28.8 ,7.8,
     +        17. ,28.8, 14.6,28.8,28.8/
         data coil4/39.9,41.8,10.7/
         data coil5/0.,360.0 ,4  ,-14.6,28.8,37.9, -7.8, 17.,37.9 ,7.8,
     +        17. ,37.9, 14.6,28.8,37.9/
         data coil6/0.,360.0 ,4  ,-16.6,27.7,39.9, -8.9, 15.,39.9 ,8.9,
     +        15. ,39.9, 16.6,27.7,39.9/
         data coilz/0.,0.,0.,0./
         
c     data cryo1/14.5,15.0,13.6/
c     data cryo2/32.0,32.5,22.6/
c     data cryo3/46.0,46.5,22.6/
c     data cryoz/0.,0.,0./

         soln_pos = mg_pos_ff(3)-5.5
         print *,'soln_pos===',soln_pos
         call gsvolu('SOLN','TUBE',clas_med_air,soln,3,IVOL) 
         call gspos('SOLN',1,'CLAS',0.,0.,soln_pos,0,'ONLY')
         
         call gsvolu('SOL1','PCON',clas_med_alu    ,coil1,12,IVOL)
         call gsvolu('SOL2','PCON',clas_med_helium4,coil2,12,IVOL) 
         call gsvolu('SOL3','PCON',clas_med_copper ,coil3,15,IVOL)
         call gsvolu('SOL4','TUBE',clas_med_copper ,coil4,3 ,IVOL)
         call gsvolu('SOL5','PCON',clas_med_helium4,coil5,15,IVOL)
         call gsvolu('SOL6','PCON',clas_med_iron   ,coil6,15,IVOL)
         
c     call gsvolu('CRY1','TUBE',clas_med_ss,cryo1,3,IVOL)
c     call gsvolu('CRY2','TUBE',clas_med_ss,cryo2,3,IVOL)
c     call gsvolu('CRY3','TUBE',clas_med_ss,cryo3,3,IVOL)
         
         call gspos('SOL1',1,'SOLN',0.,0.,0.,0,'ONLY')
         call gspos('SOL2',1,'SOL1',0.,0.,0.,0,'ONLY')
         call gspos('SOL3',1,'SOL5',0.,0.,0.,0,'ONLY')
         call gspos('SOL4',1,'SOL2',0.,0.,5.5,0,'ONLY')
         call gspos('SOL5',1,'SOL6',0.,0.,0.,0,'ONLY')
         call gspos('SOL6',1,'SOL2',0.,0.,5.5,0,'ONLY')
         
c     call gspos('CRY1',1,'SOLN',0.,0.,cryoz(1),0,'ONLY')
c     call gspos('CRY2',1,'SOLN',0.,0.,cryoz(2),0,'ONLY')
c     call gspos('CRY3',1,'SOLN',0.,0.,cryoz(3),0,'ONLY')

         
      end

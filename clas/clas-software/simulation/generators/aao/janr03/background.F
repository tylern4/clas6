      SUBROUTINE BACKGROUND (S,Q2)
     
      IMPLICIT NONE
      SAVE

#include "gpar.inc"
#include "back_ampl.inc"
#include "fit_par.inc"

      REAL S, Q2

      INTEGER NWRITE
      DATA NWRITE/0/
      REAL exte_r1(2,9), exte_b1(2,9)
      REAL extm_r1(2,9), extm_b1(2,9)
      REAL exts_r1(2,9), exts_b1(2,9)
      REAL exte_r3(8), exte_b3(8)
      REAL extm_r3(8), extm_b3(8)
      REAL exts_r3(8), exts_b3(8)
      REAL ae1(2,9), cre1(2,9),cbe1(2,9)
      REAL am1(2,9), crm1(2,9),cbm1(2,9)
      REAL ae3(8), cre3(8),cbe3(8)
      REAL am3(8), crm3(8),cbm3(8)
      REAL as3(8), as1(2,9)
      INTEGER i,j

      CALL back_born(S,Q2)
      CALL back_regge(S,Q2)
      
C     Transverse amplitudes, parametrizations from photoproduction
c- M1-
	am1(1,1)=1.
	crm1(1,1)=0.
	cbm1(1,1)=0.
	am3(8)=0.05
	crm3(8)=0.
	cbm3(8)=0.
c- E0+
	ae1(1,2)=0.05
	cre1(1,2)=0.
	cbe1(1,2)=0.
	ae3(5)=0.05
	cre3(5)=0.
	cbe3(5)=0.
c- M1+
	am1(1,7)=0.05
	crm1(1,7)=0.
	cbm1(1,7)=0.
	am3(1)=1.
	crm3(1)=0.
	cbm3(1)=0.
c- E1+
	ae1(1,7)=0.05
	cre1(1,7)=0.
	cbe1(1,7)=0.
	ae3(1)=1.
	cre3(1)=0.
	cbe3(1)=0.
c- M2-
	am1(1,3)=1.
	crm1(1,3)=3.
	cbm1(1,3)=1.
	am3(2)=1.
	crm3(2)=0.
	cbm3(2)=0.
c- E2-
	ae1(1,3)=1.
	cre1(1,3)=0.
	cbe1(1,3)=-0.9
	ae3(2)=1.
	cre3(2)=0.
	cbe3(2)=1.
c- M2+
	am1(1,9)=1.
	crm1(1,9)=0.
	cbm1(1,9)=0.
	am3(7)=1.
	crm3(7)=0.
	cbm3(7)=0.
c- E2+
	ae1(1,9)=1.
	cre1(1,9)=0.
	cbe1(1,9)=0.
	ae3(7)=1.
	cre3(7)=0.
	cbe3(7)=0.
c- M3-
	am1(1,6)=1.
	crm1(1,6)=3.
	cbm1(1,6)=1.
	am3(6)=1.
	crm3(6)=0.
	cbm3(6)=0.
c- E3-
	ae1(1,6)=1.
	cre1(1,6)=2.
	cbe1(1,6)=1.
	ae3(6)=1.
	cre3(6)=1.
	cbe3(6)=0.
c- M3+
	am1(1,8)=1.
	crm1(1,8)=0.
	cbm1(1,8)=0.
	am3(3)=1.
	crm3(3)=2.
	cbm3(3)=1.
c- E3+
	ae1(1,8)=1.
	cre1(1,8)=0.
	cbe1(1,8)=0.
	ae3(3)=1.
	cre3(3)=0.
	cbe3(3)=0.

      do i=1,2
        do j=1,9
          exte_r1(i,j)=ae1(i,j)*(s/4)**cre1(i,j)*(s-1.16)**2
          exte_r1(i,j)=exte_r1(i,j)/(ae1(i,j)*(s-1.16)**2+1.)
          exte_b1(i,j)=(1.16/s)**cbe1(i,j)/(ae1(i,j)*(s-1.16)**2+1.)
          extm_r1(i,j)=am1(i,j)*(s/4)**crm1(i,j)*(s-1.16)**2
          extm_r1(i,j)=extm_r1(i,j)/(am1(i,j)*(s-1.16)**2+1.)
          extm_b1(i,j)=(1.16/s)**cbm1(i,j)/(am1(i,j)*(s-1.16)**2+1.)
        enddo    
      enddo
          
      do j=1,8
        exte_r3(j)=ae3(j)*(s/4)**cre3(j)*(s-1.16)**2
        exte_r3(j)=exte_r3(j)/(ae3(j)*(s-1.16)**2+1.)
        exte_b3(j)=(1.16/s)**cbe3(j)/(ae3(j)*(s-1.16)**2+1.)
        extm_r3(j)=am3(j)*(s/4)**crm3(j)*(s-1.16)**2
        extm_r3(j)=extm_r3(j)/(am3(j)*(s-1.16)**2+1.)
        extm_b3(j)=(1.16/s)**cbm3(j)/(am3(j)*(s-1.16)**2+1.)
      enddo
          
      do i=1,2
        do j=1,9
          BM1(i,j)=extm_r1(i,j)* RM1(i,j)+extm_b1(i,j)* BbM1(i,j)
          BE1(i,j)=exte_r1(i,j)* RE1(i,j)+exte_b1(i,j)* BbE1(i,j)
        enddo    
      enddo
          
      do j=1,8
        BM3(j)=extm_r3(j)* RM3(j)+extm_b3(j)* BbM3(j)
        BE3(j)=exte_r3(j)* RE3(j)+exte_b3(j)* BbE3(j)
      enddo
         
C----- Longitudinal amplitudes
c- S1-
	as1(1,1)=0.2407 
	as3(8)=0.5525   
c- S0+
	as1(1,2)=0.
	as3(5)=0.
c- S1+
	as1(1,7)=0.0
	as3(1)=0.
c- S2-
	as1(1,3)=1.
	as3(2)=0.
c- S2+
	as1(1,9)=0.762
	as3(7)=0.2254
c- S3-
	as1(1,6)=1.
	as3(6)=0.63688
c- S3+
	as1(1,8)=1.
	as3(3)=1.
	
      do i=1,2
        do j=1,9
          exts_r1(i,j)=as1(i,j)*(s-1.16)**2/(as1(i,j)*(s-1.16)**2+1.)
          exts_b1(i,j)=1./(as1(i,j)*(s-1.16)**2+1.)
          BS1(i,j)=exts_r1(i,j)* RS1(i,j)+exts_b1(i,j)* BbS1(i,j)
        enddo    
      enddo  
        
      do j=1,8
        exts_r3(j)=as3(j)*(s-1.16)**2/(as3(j)*(s-1.16)**2+1.)
        exts_b3(j)=1./(as3(j)*(s-1.16)**2+1.)
        BS3(j)=exts_r3(j)* RS3(j)+exts_b3(j)* BbS3(j)
      enddo    
    
c- Background multipole amplitudes(3/2) for resonances with isospin 3/2.
c- resonances (1),(4),(10)  => (1+)
c       BM3(1),BE3(1),BS3(1)
c- resonances (2),(12)      => (2-)
c       BM3(2),BE3(2),BS3(2)
c- resonances  (3)          => (3+)
c       BM3(3),BE3(3),BS3(3)  
c- resonances  (5),(9)      => (0+)
c       BE3(5),BS3(5)
c- resonances  (6),(13)     => (3-)
c       BM3(6),BE3(6),BS3(6)
c- resonances  (7)          => (2+)  
c       BM3(7),BE3(7),BS3(7)
c- resonances  (8),(11)     => (1-)  
c       BM3(8),BE3(8),BS3(8)
           
c        do i=1,2
c- Background multipole amplitudes p(1/2) and n(1/2) 
c- for resonances with isospin 1/2.
c- resonances  (1),(10)          => (1-)
c        BM1(i,1),BS1(i,1)
c- resonances  (2),(5)           => (0+)
c        BE1(i,2),BS1(i,2)
c- resonances  (3),(4),(12)      => (2-)
c       BM1(i,3),BE1(i,3),BS1(i,3) 
c- resonances  (6),(13)          => (3-)
c       BM1(i,6),BE1(i,6),BS1(i,6) 
c- resonances  (7),(11)          => (1+)
c       BM1(i,7),BE1(i,7),BS1(i,7) 
c- resonances  (8)               => (3+)
c       BM1(i,8),BE1(i,8),BS1(i,8) 
c- resonances  (9)               => (2+)
c       BM1(i,9),BE1(i,9),BS1(i,9)   
c       enddo

      RETURN
      END

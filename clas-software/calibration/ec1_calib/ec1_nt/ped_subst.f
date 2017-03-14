      subroutine ped_subst(pedest)
c  
c pedestals substructing and control (and correction if necessary) of range. 
c
      IMPLICIT NONE
*      SAVE
      include "bcs.inc"
      include "EC1nt.inc"
      include "ntpl_com.inc"
*
      INTEGER*4 pedest(2,2,1088)
      INTEGER id_p , i
      
      DO i=1,nEC1
	 id_p   = 256*layer(i)+strip(i)
         adcl(i) = adcl(i) - pedest(sector(i),1,ID_p)
         adcr(i) = adcr(i) - pedest(sector(i),2,ID_p)
         if(adcl(i).lt.0) adcl(i)=0
         if(adcr(i).lt.0) adcr(i)=0

c convert ADC to energy using calibration constants

         E_l(i) = adcl(i) *  A_l(sector(i),layer(i),strip(i)) 
     &                    + dA_l(sector(i),layer(i),strip(i))
        
         E_r(i) = adcr(i) *  A_r(sector(i),layer(i),strip(i))
     &                    + dA_r(sector(i),layer(i),strip(i))
                            
*
c force TDC to upper limit if it is out of range 
         if(tdcl(i).gt.4095 .or. tdcl(i).lt.0) tdcl(i) = 4095
         if(tdcr(i).gt.4095 .or. tdcr(i).lt.0) tdcr(i) = 4095
*
c convert TDC to time using calibration constants

         T_l(i) = tdcl(i) * cT_l(sector(i),layer(i),strip(i)) 
     &                 + dT_l(sector(i),layer(i),strip(i))
         T_r(i) = tdcr(i) * cT_r(sector(i),layer(i),strip(i))
     &                 + dT_r(sector(i),layer(i),strip(i))
c time walk correction
c         T_l(i) = T_l(i)-4.54*exp(-.05*adcl(i)/20.)
c         T_r(i) = T_r(i)-4.54*exp(-.05*adcr(i)/20.)
         t_l(i) = t_l(i)-3.79*exp(-.05*adcl(i)/28.)
         t_r(i) = t_r(i)-3.79*exp(-.05*adcr(i)/28.)
             
      END DO
      return
      end
      

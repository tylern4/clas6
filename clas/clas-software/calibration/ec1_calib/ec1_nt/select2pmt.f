      subroutine select2pmt
*  
* select only signals with coincedence left+right PMT
* and calculate number of hits in each layer. 
*
      IMPLICIT NONE
      INTEGER *4 i , i2pmt , Nlayer , Nsector, thresh
*      SAVE
      include "bcs.inc"
*
      include "EC1nt.inc"
*
      include "ntpl_com.inc"
*
      i2pmt = 0
      thresh= 100           ! threshold in ADC channels 
      CALL izero(Nhits,8)   ! initialize array
      
      DO i=1,nEC1
cc	 id   = 265*layer(i)+strip(i)
	 if(adcl(i).GT.thresh .AND. adcr(i).GT.thresh) then
c         if(adcl(i)*adcr(i).NE.0) then   ! select only signals with coincedence
           i2pmt = i2pmt+1               ! left+right PMT 
           sector(i2pmt) = sector(i)
           layer (i2pmt) = layer (i)
           strip (i2pmt) = strip (i)
           adcL  (i2pmt) = adcL  (i)
           adcR  (i2pmt) = adcR  (i)
           tdcL  (i2pmt) = tdcL  (i)
           tdcR  (i2pmt) = tdcR  (i)
           E_L   (i2pmt) = E_L   (i)
           E_R   (i2pmt) = E_R   (i)
           T_L   (i2pmt) = T_L   (i)
           T_R   (i2pmt) = T_R   (i)
           Nlayer        = layer (i2pmt)
           Nsector       = sector(i2pmt)
           Nhits(Nsector,Nlayer) = Nhits(Nsector,Nlayer)+1
         end if
       END DO
       nEC1 = i2pmt
       return 
       end    

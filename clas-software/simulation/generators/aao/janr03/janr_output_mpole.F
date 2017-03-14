      subroutine janr_output_mpole
      
      real q2,mpion(2),mpa,mpb,mp2,mp,qpi1,qpi2
      data mpion/0.13498,0.13957/
      data mp/0.93827/
      
#include "janrtab.inc"  
#include "res_func.inc"    
      common/ifintg/lm,is,ii,jj,w,q2

      mpa = mpion(1)**2
      mpb = mpion(2)**2
      mp2 = mp**2
      
      open(unit=1,file='janr-PPpi.tbl',type='UNKNOWN')
      open(unit=2,file='janr-PNpi.tbl',type='UNKNOWN')
      
      do q2=q2min,q2max,q2step
        call mult_ana(q2)
        call read_multables
        call regge_multables
        call high_multables
      do w=wmin,wmax,wstep
        print *, 'Multipole tables written at W,Q2=',w,q2
        qpi1=sqrt(((w*w-mp2+mpa)/2./w)**2-mpa)
        qpi2=sqrt(((w*w-mp2+mpb)/2./w)**2-mpb)
        ratio = qpi2/qpi1        
        call janr_run(w,q2,0.5,1.0,0.0,.true.) 
        call write_mpoles(w,q2,ratio)
      enddo
      enddo
      
      close(1)
      close(2)
      
      end
                      


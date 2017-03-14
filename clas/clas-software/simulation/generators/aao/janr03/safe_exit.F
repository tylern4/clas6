      subroutine safe_exit
      
#include "fit_par.inc"
#include "inpar.inc"

      write(f_out,*)'Program is interupted by user'
      write(6,*)'Program is interupted by user'
      close(f_out)  
      if (grfopt.eq.'N') call exit
       
77    call hjanr_plot 
      call hjanr_pick
      go to 77

      END





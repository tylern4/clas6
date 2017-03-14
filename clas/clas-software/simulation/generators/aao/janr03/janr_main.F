      PROGRAM JANR_MAIN
      
#include "inpar.inc"
#include "janrtab.inc"

      call assign_sig    
      call janr_init
      
      print *, 'JANR_MAIN: Begin reading tables'
      call read_multables
      call regge_multables
      call high_multables         
      print *, 'JANR_MAIN: End reading tables'
         
      if (fitopt.eq.'F') call janr_fit
      if (fitopt.eq.'T') call janr_output
      if (fitopt.eq.'M'.or.fitopt.eq.'T') call janr_output_mpole
      END




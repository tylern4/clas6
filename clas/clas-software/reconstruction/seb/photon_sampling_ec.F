      Real Function Photon_sampling_ec(dE,ierr)
c
c - Function based on GSIM simulations for the sampling fraction
c - of the photon energy in the forward EC
c
c - Inputs:
c                 dE - energy of the cluster in EC
c               ierr - flag (not used)
c
c - Outputs: Function value is the sampling fraction
c
c  Notes:
c  ------
c Fit to the GSIM simulated sampling fraction is done for two
c energy regions separatly: dE<0.6 and dE>0.6 (Natasha Dashyan)
c
c  Author:   S. Stepanyan    Created:  March 13, 2006
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
      Real dE
      Integer ierr
c
      Integer i
      Real sampling
c
      real p_l06(6)
      data p_l06/0.25161,-0.088369,1.9664,-8.3068,14.074,-8.5664/
      real p_g06(2)
      data p_g06/0.2733,-0.0016/
c
      if(dE.lt.0.6)then
         sampling=p_l06(1)
         do i=2,6
            sampling=sampling+p_l06(i)*dE**(i-1)
         enddo
      else
         sampling=p_g06(1)+p_g06(2)*dE
      endif
c
      Photon_sampling_ec=sampling
c
      RETURN
      END
c
c------------------------------------------------------------------------------

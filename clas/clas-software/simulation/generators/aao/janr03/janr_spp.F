      subroutine janr_spp(q2,w,eps,csm,phi,chan,crs,alt,altp,iupd)
      
      real q2,w,eps,csm,phi,crs,alt,altp
      integer chan,ierr
      logical iupd

#include "fit_par.inc"
#include "res_func.inc"

      pi_type = chan

      call janr_run(w,q2,eps,csm,phi,iupd)
      
      crs  = sigma(chan)
      sltp = sigmaTLP(chan)
      slt  = sigmaTL(chan)
      altp = sltp*sqrt(2.*eps*(1-eps))*sin(phi*3.1416/180.)/crs
      alt  =  slt*sqrt(2.*eps*(1+eps))*cos(phi*3.1416/180.)/crs
      end
      
      
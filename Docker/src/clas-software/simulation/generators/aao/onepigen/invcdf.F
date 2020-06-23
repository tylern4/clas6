c     Inverse CDF of Bonn-CD Fermi momentum distribution.  Uses linear
c     interpolation to estimate the CDF between sample points.
      function invcdf(p)
      implicit none
      real invcdf
      real p

      integer lower_index           ! the index for which cdf(i) is just
! below p

      real slope, intercept     ! linear interpolation coefficients
      
      integer nsamples
      parameter (nsamples = 1001)
      real cdf(nsamples)

c     Initialize cdf values, no need to see the 1001 lines
#include "fermi_cdf.inc"

c     Stupid implementation: Loop across entire array until
c     cdf(lower_index) > p.  Ideally, this should be a binary search,
c     but at the moment I just want to get it off the ground.
c     Improvements pending.
      do 10 lower_index=1,1001
         if(cdf(lower_index) .eq. p) then
c     lower_index starts with 1, but p starts at 0, so adjust index
            invcdf=1e-3*(lower_index-1)
            goto 3
         endif
         if(cdf(lower_index) .gt. p) then
            goto 2
         endif
 10   continue
 2    lower_index=lower_index-1 ! as long as p > 0 it's good
      slope = cdf(lower_index+1)-cdf(lower_index)
      intercept = cdf(lower_index)-slope*lower_index
      invcdf = 1e-3*(p-intercept)/slope
 3    return
      end

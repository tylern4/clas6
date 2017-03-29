c     Note: This function assumes there is only one electron per event.
c     I don't know how to handle the general case of multiple electrons
c     as smearing needs to preserve the beam and the scattered electron
c     is the one which is used to compute the virtual photon.

      function convert(jw,rjw)
      implicit none

#include "bcs.inc"

      integer convert
      integer jw(Nbcs)
      real rjw(Nbcs)

      integer mlink
      integer mbank
      integer geant2pdg
      real pdg_pid2mass
      real geant_pid2mass

c     Misc
      integer ierr

c     Loop variables
      integer iind, oind
      integer i, j, k, iindx, oindx

      integer npart
      real momentum
      real fv(4,1)

c     Initialization
      data fv/4*0.0/
      momentum = 0

c     Always return success
      convert=1
c     Copy HEAD bank
      iind = mlink(iw,'HEAD',0)
      oind = mbank(jw,'HEAD',0,8,1)

      jw(oind+1) = iw(iind+1)   ! version
      jw(oind+2) = iw(iind+2)   ! run
      jw(oind+3) = iw(iind+3)   ! event number
      jw(oind+4) = iw(iind+4)   ! event time
      jw(oind+5) = iw(iind+5)   ! event type
      jw(oind+6) = iw(iind+6)   ! ROC
      jw(oind+7) = iw(iind+7)   ! event class
      jw(oind+8) = iw(iind+8)   ! weight

c     Convert PART bank to MCTK and MCVX
      iind = mlink(iw,'PART',0)
      if(iind .ne. 0) then
         npart = iw(iind-4)
c     MCTK output
         oind = mbank(jw,'MCTK',0,11,npart)
         iindx   = iind
         
         do j=1,npart
            iindx       = iind+(j-1)*13
            oindx       = oind+(j-1)*11
c     Get particle 4-vector
            do k=1,4
               fv(k,1)=rw(iindx+k+4)
            enddo
            momentum = sqrt(fv(2,1)**2 ! momentum
     &           + fv(3,1)**2
     &           + fv(4,1)**2)

            if(momentum .eq. 0) then
               rjw(oindx+1)  = 0 ! x cosine
               rjw(oindx+2)  = 0 ! y cosine
               rjw(oindx+3)  = 0 ! z cosine
            else
               rjw(oindx+1)  = fv(2,1)/momentum ! x cosine
               rjw(oindx+2)  = fv(3,1)/momentum ! y cosine
               rjw(oindx+3)  = fv(4,1)/momentum ! z cosine
            endif
            rjw(oindx+4)  = momentum
            rjw(oindx+5)  = geant_pid2mass(iw(iindx+1)) ! mass
            rjw(oindx+6)  = rw(iindx+9) ! charge
c     PDG ID
            jw(oindx+7)  = geant2pdg(iw(iindx+1))
            jw(oindx+8)  = 0    ! track flag
            jw(oindx+9)  = 1    ! beginning vertex number
            jw(oindx+10) = 0    ! ending vertex number
            jw(oindx+11) = 0    ! parent track
         enddo
         
c     End MCTK output

c     MCVX output
         iind = mlink(iw,'PART',0)
         oind = mbank(jw,'MCVX',0,5,npart)

c     input particles
         do i=1,npart
            iindx = iind + (i-1)*13
            oindx = oind + (i-1)*5

            rjw(oindx+1) = rw(iindx+2) ! x vertex
            rjw(oindx+2) = rw(iindx+3) ! y vertex
            rjw(oindx+3) = rw(iindx+4) ! z vertex
            rjw(oindx+4) = 0.   ! secs of flight (not sure what it means)
            jw(oindx+5) = 0     ! vertex flag
         enddo

c     End MCVX output
         
      endif
      
 1    return
      end

c     Note: This function assumes there is only one electron per event.
c     I don't know how to handle the general case of multiple electrons
c     as smearing needs to preserve the beam and the scattered electron
c     is the one which is used to compute the virtual photon.

      function smear(target_type,beam,spectator_p,target,spectator,jw,
     &     rjw)
      implicit none

#include "bcs.inc"

      integer smear
      integer target_type
      real beam
      integer spectator_p
      real target(4)
      real spectator(4)
      integer jw(Nbcs)
      real rjw(Nbcs)

      integer matrix_mult, lorentz_boost

      integer mlink
      integer mbank
      integer geant2pdg
      real pdg_pid2mass
      real geant_pid2mass

c     Misc
      integer ierr

c     Loop variables
      integer iind, oind
      integer i, j, k, iindx, oindx, scattered_iindx

      integer npart, extra_particles, spectator_type

      real bf(3)                ! beta of Fermi motion
      real q(4)                 ! virtual photon
      real beamfv(4)            ! beam four vector

      real boost(4,4)           ! Lorentz boost matrix
      real fv1(4,1)
      real fv2(4,1)

      real momentum

      real xmeanvertex, ymeanvertex, zmeanvertex

      data bf/3*0.0/, q/4*0.0/, boost/16*0.0/,
     &     fv1/4*0.0/, fv2/4*0.0/

      beamfv(1) = beam
      beamfv(2) = 0.
      beamfv(3) = 0.
      beamfv(4) = beam

      xmeanvertex = 0
      ymeanvertex = 0
      zmeanvertex = 0

      momentum = 0

c$$$  c     DEBUG
c$$$  write(*,*) "Beam:"
c$$$  write(*,*) beam
c$$$  c     END DEBUG

c     set spectator settings
      if(spectator_p .eq. 0) then
         extra_particles = 0
      else
         spectator_type = 1-target_type ! arithmetic trick
         extra_particles = 1
      endif

c     Calculate boost
      do i=1,3
         bf(i)=target(i+1)/target(1)
      enddo
c$$$c     DEBUG
c$$$      write(*,*) "beta: ",bf(1),bf(2),bf(3)
c$$$c     END DEBUG
      smear = lorentz_boost(bf,boost)

c$$$c     DEBUG
c$$$      write(*,*) "Lorentz boost:"
c$$$      do i=1,4
c$$$         write(*,*) boost(i,1),boost(i,2),boost(i,3),boost(i,4)
c$$$      enddo
c$$$
c$$$c     END DEBUG

      if(smear .ne. 1) then
         write(0,*) "Error: beta > 1 in lorentz boost"
         goto 1
      endif

c     HEAD bank
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

c     ADD OTHER INPUT BANK CASES HERE
c     Input from PART bank
      iind = mlink(iw,'PART',0)
      if(iind .ne. 0) then
         npart = iw(iind-4) + extra_particles
c     MCTK output
         oind = mbank(jw,'MCTK',0,11,npart)
c     Calculate q
         iindx   = iind
         if(geant2pdg(iw(iindx+1)) .eq. 11) then
            scattered_iindx=iindx
            q(1) = beam - rw(iindx+5)
            q(2) = -rw(iindx+6)
            q(3) = -rw(iindx+7)
            q(4) = beam - rw(iindx+8)
         endif
c     particles from input
         do j=1,npart-extra_particles
            iindx       = iind+(j-1)*13
            oindx       = oind+(j-1)*11
c     Boost particle due to Fermi motion
            if(scattered_iindx .eq. iindx) then
               do k=1,4
                  fv1(k,1) = q(k)
               enddo
               ierr = matrix_mult(4,4,boost,4,1,fv1,fv2)
               do k=1,4
                  fv2(k,1) = beamfv(k)-fv2(k,1)
               enddo
            else
               do k=1,4
                  fv1(k,1)=rw(iindx+k+4)
               enddo
               ierr = matrix_mult(4,4,boost,4,1,fv1,fv2)
            endif

            momentum = sqrt(fv2(2,1)**2 ! momentum
     &           + fv2(3,1)**2
     &           + fv2(4,1)**2)

            if(momentum .eq. 0) then
               rjw(oindx+1)  = 0 ! x cosine
               rjw(oindx+2)  = 0 ! y cosine
               rjw(oindx+3)  = 0 ! z cosine
            else
               rjw(oindx+1)  = fv2(2,1)/momentum ! x cosine
               rjw(oindx+2)  = fv2(3,1)/momentum ! y cosine
               rjw(oindx+3)  = fv2(4,1)/momentum ! z cosine
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

c     additional particles
c     spectator
         if(spectator_p .eq. 1) then
            oindx = oind + (npart-1)*11

            momentum = sqrt(spectator(2)**2 ! momentum
     &           + spectator(3)**2
     &           + spectator(4)**2)

            if(momentum .eq. 0) then
               rjw(oindx+1)  = 0 ! x cosine
               rjw(oindx+2)  = 0 ! y cosine
               rjw(oindx+3)  = 0 ! z cosine
            else
               rjw(oindx+1)  = spectator(2)/momentum ! x cosine
               rjw(oindx+2)  = spectator(3)/momentum ! y cosine
               rjw(oindx+3)  = spectator(4)/momentum ! z cosine
            endif
            rjw(oindx+4)  = momentum
c     turns out better to do mass after particle ID
            rjw(oindx+6)  = real(spectator_type) ! charge
c     PDG ID
            if(spectator_type .eq. 0) then
               jw(oindx+7)  = 2112
            else
               jw(oindx+7) = 2212
            endif
            rjw(oindx+5) = pdg_pid2mass(jw(oindx+7)) ! mass
            jw(oindx+8)  = 0    ! track flag
            jw(oindx+9)  = 1    ! beginning vertex number
            jw(oindx+10) = 0    ! ending vertex number
            jw(oindx+11) = 0    ! parent track
         endif
c     End MCTK output

c     MCVX output
         iind = mlink(iw,'PART',0)
         oind = mbank(jw,'MCVX',0,5,npart)

c     input particles
         do i=1,npart-extra_particles
            iindx = iind + (i-1)*13
            oindx = oind + (i-1)*5
c     copy input to output
            rjw(oindx+1) = rw(iindx+2) ! x vertex
            rjw(oindx+2) = rw(iindx+3) ! y vertex
            rjw(oindx+3) = rw(iindx+4) ! z vertex
            rjw(oindx+4) = 0.   ! secs of flight (not sure what it means)
            jw(oindx+5) = 0     ! vertex flag
c     accumulate mean vertexes for added particles
            xmeanvertex=xmeanvertex + rw(iindx+2)
            ymeanvertex=ymeanvertex + rw(iindx+3)
            zmeanvertex=zmeanvertex + rw(iindx+4)
         enddo

c     divide mean vertices by denominator
         xmeanvertex=xmeanvertex/real(npart-extra_particles)
         ymeanvertex=ymeanvertex/real(npart-extra_particles)
         zmeanvertex=zmeanvertex/real(npart-extra_particles)
c     additional particles
c     spectator
         if(spectator_p .eq. 1) then
            oindx = oind + (npart-1)*5
            rjw(oindx+1) = xmeanvertex ! x vertex
            rjw(oindx+2) = ymeanvertex ! y vertex
            rjw(oindx+3) = zmeanvertex ! z vertex
            rjw(oindx+4) = 0.   ! secs of flight (copying from genev)
            jw(oindx+5) = 0     ! vertex flag (copying from genev)
         endif
c     End MCVX output
      else
c     MCTK and MCVX input
         iind = mlink(iw,'MCTK',0)
         npart = iw(iind-4) + extra_particles
         oind = mbank(jw,'MCTK',0,11,npart)
c     MCTK output
c     Calculate q
         iindx   = iind
         if(iw(iindx+7) .eq. 11) then
            scattered_iindx=iindx
c$$$c     DEBUG
c$$$            write(*,*) "Electron before: ",
c$$$     &           sqrt(rw(iindx+5)**2+rw(iindx+4)**2),
c$$$     &           rw(iindx+1)*rw(iindx+4),
c$$$     &           rw(iindx+2)*rw(iindx+4),
c$$$     &           rw(iindx+3)*rw(iindx+4)
c$$$c     END DEBUG
            q(1) = beam - sqrt(rw(iindx+5)**2+rw(iindx+4)**2)
            q(2) = -rw(iindx+1)*rw(iindx+4)
            q(3) = -rw(iindx+2)*rw(iindx+4)
            q(4) = beam - rw(iindx+3)*rw(iindx+4)

c$$$c$$$  c     DEBUG
c$$$            write(*,*) "Before Q^2:"
c$$$            write(*,*) q(2)**2+q(3)**2+q(4)**2-q(1)**2
c$$$            write(*,*) "Before q_mu:"
c$$$            write(*,*) q(1),q(2),q(3),q(4)
c$$$c$$$  c     END DEBUG
         endif
c     particles from input:
         do j=1,npart-extra_particles
            iindx       = iind+(j-1)*11
            oindx       = oind+(j-1)*11
c     Boost particle due to Fermi motion

            if(scattered_iindx .eq. iindx) then
               do k=1,4
                  fv1(k,1) = q(k)
               enddo
               ierr = matrix_mult(4,4,boost,4,1,fv1,fv2)
c$$$c$$$  c     DEBUG
c$$$               write(*,*) "After q_mu:"
c$$$               write(*,*) fv2(1,1),fv2(2,1),fv2(3,1),fv2(4,1)
c$$$               write(*,*) "After Q^2:"
c$$$               write(*,*) fv2(2,1)**2+fv2(3,1)**2+fv2(4,1)**2
c$$$     &              -fv2(1,1)**2
c$$$c$$$  c     END DEBUG
               do k=1,4
                  fv2(k,1) = beamfv(k)-fv2(k,1)
               enddo
c$$$c     DEBUG
c$$$               write(*,*) "Electron after: ",
c$$$     &              fv2(1,1),
c$$$     &              fv2(2,1),
c$$$     &              fv2(3,1),
c$$$     &              fv2(4,1)
c$$$c     END DEBUG
            else
               momentum = rw(iindx+4)
               fv1(1,1) = sqrt(rw(iindx+5)**2+momentum**2)
               do k=2,4
                  fv1(k,1) = momentum*rw(iindx+k-1)
               enddo

               ierr = matrix_mult(4,4,boost,4,1,fv1,fv2)
            endif
            momentum = 0
            do k=2,4
               momentum = momentum+fv2(k,1)**2
            enddo
            momentum=sqrt(momentum)

            if(momentum .eq. 0) then
               rjw(oindx+1)  = 0 ! x cosine
               rjw(oindx+2)  = 0 ! y cosine
               rjw(oindx+3)  = 0 ! z cosine
            else
               rjw(oindx+1)  = fv2(2,1)/momentum ! x cosine
               rjw(oindx+2)  = fv2(3,1)/momentum ! y cosine
               rjw(oindx+3)  = fv2(4,1)/momentum ! z cosine
            endif
            rjw(oindx+4)  = momentum
            rjw(oindx+5)  = rw(iindx+5) ! mass
            rjw(oindx+6)  = rw(iindx+6) ! charge
c     PDG ID
            jw(oindx+7)  = iw(iindx+7) ! PDG particle ID
            jw(oindx+8)  = iw(iindx+8) ! track flag
            jw(oindx+9)  = iw(iindx+9) ! beginning vertex number
            jw(oindx+10) = iw(iindx+10) ! ending vertex number
            jw(oindx+11) = iw(iindx+11) ! parent track
         enddo
c     additional particles
c     Spectator
         oindx = oind+(npart-1)*11

         momentum = 0
         do j=2,4
            momentum = momentum+spectator(j)**2
         enddo
         momentum = sqrt(momentum)

         if(momentum .eq. 0) then
            rjw(oindx+1)  = 0   ! x cosine
            rjw(oindx+2)  = 0   ! y cosine
            rjw(oindx+3)  = 0   ! z cosine
         else
            rjw(oindx+1)  = spectator(2)/momentum ! x cosine
            rjw(oindx+2)  = spectator(3)/momentum ! y cosine
            rjw(oindx+3)  = spectator(4)/momentum ! z cosine
         endif
         rjw(oindx+4)  = momentum
c     mass done after particle ID
         rjw(oindx+6)  = real(spectator_type) ! charge
c     PDG ID
         if(spectator_type .eq. 0) then
            jw(oindx+7)  = 2112
         else
            jw(oindx+7) = 2212
         endif
         rjw(oindx+5) = pdg_pid2mass(jw(oindx+7)) ! mass
         jw(oindx+8)  = 0 ! track flag
         jw(oindx+9)  = 1 ! beginning vertex number
         jw(oindx+10) = 0 ! ending vertex number
         jw(oindx+11) = 0 ! parent track
c     End MCTK output
c     MCVX output
         iind = mlink(iw,'MCVX',0)
         oind = mbank(jw,'MCVX',0,5,npart)

c     input particles
         do i=1,npart-extra_particles
            iindx = iind + (i-1)*5
            oindx = oind + (i-1)*5
c     copy input to output
            rjw(oindx+1) = rw(iindx+1) ! x vertex
            rjw(oindx+2) = rw(iindx+2) ! y vertex
            rjw(oindx+3) = rw(iindx+3) ! z vertex
            rjw(oindx+4) = rw(iindx+4) ! secs of flight (not sure what it means)
            jw(oindx+5) = iw(iindx+5) ! vertex flag
c     accumulate mean vertexes for added particles
            xmeanvertex=xmeanvertex + rw(iindx+1)
            ymeanvertex=ymeanvertex + rw(iindx+2)
            zmeanvertex=zmeanvertex + rw(iindx+3)
         enddo

c     divide mean vertices by denominator
         xmeanvertex=xmeanvertex/real(npart-extra_particles)
         ymeanvertex=ymeanvertex/real(npart-extra_particles)
         zmeanvertex=zmeanvertex/real(npart-extra_particles)
c     additional particles
c     spectator
         if(spectator_p .eq. 1) then
            oindx = oind + (npart-1)*5
            rjw(oindx+1) = xmeanvertex ! x vertex
            rjw(oindx+2) = ymeanvertex ! y vertex
            rjw(oindx+3) = zmeanvertex ! z vertex
            rjw(oindx+4) = 0.   ! secs of flight (copying from aao_rad)
            jw(oindx+5) = 0     ! vertex flag (copying from aao_rad)
         endif
      endif

 1    return
      end

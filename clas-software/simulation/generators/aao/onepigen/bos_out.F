      subroutine bos_out
      implicit none
      
#include "bcs.inc"
#include "mc.inc"
     
      integer nbank,ind,ierr,indx,j,output_counter
      logical do_mctk

      SAVE output_counter

      do_mctk = .false.
      
      output_counter	= output_counter+1
      
      ind		= nbank('HEAD',0,8,1)
      iw(ind+1)		= 1                 	! version
      iw(ind+2)		= 1                 	! run
      iw(ind+3)		= output_counter     	! event number
      iw(ind+4)		= 100                	! event time
      iw(ind+5) 	= -2                 	! event type
      iw(ind+6) 	= 0                 	! ROC
      iw(ind+7) 	= 7                 	! event class
      iw(ind+8)     = 1                  	! weight

      ind		= nbank('TGBI',0,4,1)
      if (hel.gt.0.5) then
        iw(ind+1) = 32768
      else
        iw(ind+1) = 0 
      endif
         
      ind		= nbank('PART',0,13,npart) 
      do j=1,npart
         indx 		= ind+(j-1)*13
         iw(indx+1)	= id(j)			! particle ID (GEANT)
         rw(indx+2)	= v(j,1)		! x vertex position
         rw(indx+3)	= v(j,2)		! y vertex position
         rw(indx+4)	= v(j,3)		! z vertex position
         rw(indx+5)	= p(j,4)		! energy 
         rw(indx+6)	= p(j,1)		! px
         rw(indx+7)	= p(j,2)		! px
         rw(indx+8)	= p(j,3)		! px
         rw(indx+9)	= q(j)			! charge
         iw(indx+10) 	= j			! Track pointer
         rw(indx+11)	= xsec
         rw(indx+12) 	= asym
         iw(indx+13) 	= 1
      enddo
      
      call fwbos(iw,33,'E',ierr) ! write banks to file
      call bdrop(iw,'E')        ! drop the bank, reclaim the space
      call bgarb(iw)            ! garbage collection
      
      return
      END

c     Fermi motion version of the same routine.  
      subroutine bos_out_fermi_motion(target_type,spectator_type,
     +     spectator,
     +     invrotate,invboost)
      implicit none
      
#include "bcs.inc"
#include "mc.inc"
     
      integer nbank,ind,ierr,indx,j,output_counter
      logical do_mctk
      
      integer target_type, spectator_type

      real spectator_charge

      integer spectator_id
      
      real spectator(4), invrotate(3,3), invboost(4,4)

      real spectator_energy, spectator_mass, spectator_mom2

      real fv1(4,1), fv2(4,1), v1(3,1), v2(3,1)

      integer matrix_mult

      integer k

      real mp, mn
      DATA MP   /.938/
      DATA MN   /.9396/
      integer npart_true
      
      SAVE output_counter

      if(spectator_type.eq.0) then
c     Neutron:
         spectator_id = 13
         spectator_mass = mn
         spectator_charge = 0.0
      else
c     Proton:
         spectator_id = 14
         spectator_mass = mp
         spectator_charge = 1.0
      endif
      
      npart_true = npart+1
      
      do_mctk = .false.
      
      output_counter	= output_counter+1
      
      ind		= nbank('HEAD',0,8,1)
      iw(ind+1)		= 1                 	! version
      iw(ind+2)		= 1                 	! run
      iw(ind+3)		= output_counter     	! event number
      iw(ind+4)		= 100                	! event time
      iw(ind+5) 	= -2                 	! event type
      iw(ind+6) 	= 0                 	! ROC
      iw(ind+7) 	= 7                 	! event class
      iw(ind+8)     = 1                  	! weight

      ind		= nbank('TGBI',0,4,1)
      if (hel.gt.0.5) then
        iw(ind+1) = 32768
      else
        iw(ind+1) = 0 
      endif
         
      ind		= nbank('PART',0,13,npart_true) 
      do j=1,npart
         indx 		= ind+(j-1)*13
         iw(indx+1)	= id(j)			! particle ID (GEANT)
         rw(indx+2)	= v(j,1)		! x vertex position
         rw(indx+3)	= v(j,2)		! y vertex position
         rw(indx+4)	= v(j,3) ! z vertex position

c     First apply invrotate
         do k=1,3
            v1(k,1) = p(j,k)
         enddo
         ierr = matrix_mult(3,3,invrotate,3,1,v1,v2)
c     And then apply the inverse boost:
         fv1(1,1) = p(j,4)
         do k=1,3
            fv1(k+1,1) = v2(k,1)
         enddo
         ierr = matrix_mult(4,4,invboost,4,1,fv1,fv2)
c     And set the appropriate variables
c$$$         rw(indx+5)	= p(j,4) ! energy 
c$$$         rw(indx+6)	= p(j,1)		! px
c$$$         rw(indx+7)	= p(j,2)		! py
c$$$  rw(indx+8)	= p(j,3)		! pz
         rw(indx+5) = fv2(1,1)
         rw(indx+6) = fv2(2,1)
         rw(indx+7) = fv2(3,1)
         rw(indx+8) = fv2(4,1)
         rw(indx+9)	= q(j)  ! charge
         iw(indx+10) 	= j			! Track pointer
         rw(indx+11)	= xsec
         rw(indx+12) 	= asym
         iw(indx+13) 	= 1
      enddo

c     Add spectator

      j = npart_true
      
      indx 		= ind+(j-1)*13
      iw(indx+1)	= spectator_id ! particle ID (GEANT)
      rw(indx+2)	= v(j-1,1) ! x vertex position
      rw(indx+3)	= v(j-1,2) ! y vertex position
      rw(indx+4)	= v(j-1,3) ! z vertex position
c     Spectator has to be on-mass shell
      spectator_mom2 = 0
      do k=1,3
         spectator_mom2=spectator_mom2+spectator(k+1)**2
      enddo
      
      spectator_energy = sqrt(spectator_mom2+spectator_mass**2)
      rw(indx+5)	= spectator_energy ! energy 
      rw(indx+6)	= spectator(2) ! px
      rw(indx+7)	= spectator(3) ! py
      rw(indx+8)	= spectator(4) ! pz
      rw(indx+9)	= spectator_charge  ! charge
      iw(indx+10) 	= j     ! Track pointer
      rw(indx+11)	= xsec
      rw(indx+12) 	= asym
      iw(indx+13) 	= 1
c     Finally write banks
      
      call fwbos(iw,33,'E',ierr)	! write banks to file
      call bdrop(iw,'E')     		! drop the bank, reclaim the space
      call bgarb(iw)     		! garbage collection
         
      return
      END

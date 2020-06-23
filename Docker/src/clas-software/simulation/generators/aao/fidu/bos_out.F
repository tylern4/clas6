      subroutine bos_out(mctk)
      implicit none
      
#include "bcs.inc"
#include "mc.inc"
     
      integer nbank,ind,ierr,indx,j,k,output_counter
      integer mctk
      character*44 cbank 
     1    /'HEADDC0 DOCAMCEVEC  CC  SC  MCVXSPINPARTMCTK'/

      SAVE

      call bdrop(iw,'PART')    ! drop the bank, reclaim the space    
      call bdrop(iw,'HEAD')    ! drop the bank, reclaim the space  
      call bdrop(iw,'SPIN')    ! drop the bank, reclaim the space  
      
      output_counter	= output_counter+1
      
      ind		= nbank('HEAD',0,8,1)
      iw(ind+1)		= 1                 	! version
      iw(ind+2)		= 1                 	! run
      iw(ind+3)		= output_counter     	! event number
      iw(ind+4)		= 100                	! event time
      iw(ind+5) 	= -4                 	! event type
      iw(ind+6) 	= 0                 	! ROC
      iw(ind+7) 	= 15                 	! event class
      iw(ind+8)     = 1                  	! weight

      ind = nbank('SPIN',0,3,npart) 
C      print *, 'spin ind', ind
      do j=1,npart
         indx = ind+(j-1)*3
         do k=1,3
            iw(indx+k)	= sp(j,k)	! spin
C            write(6,*) sp(j,k), iw(indx+k)
         enddo
      enddo  
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
           iw(indx+10) 	= j			    ! Track pointer
           rw(indx+11)	= 1.
           rw(indx+12) 	= 1.
           iw(indx+13) 	= flag(j)       ! result of accvb
      enddo
      
      call fwbos(iw,33,cbank,ierr)	! write banks to file
      call bdrop(iw,'E')     		! drop the bank, reclaim the space
      call bgarb(iw)	     		! garbage collection
         
      END

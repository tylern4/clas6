      Subroutine ic_brun(irun)
c     
      IMPLICIT NONE
      SAVE
#include "icraw.inc"
#include "iccalib.inc"
#include "ictcl.inc"
#include "ic.inc"
      CHARACTER*99  geom_vec
      integer irun,iopen,ist,idmod
      integer ix,iy,ixt,iyt
      integer index
      real thx,thy,thz,phx,phy,phz 
      real dx1,dx2,dy1,dy2,dz
      integer result_ent(length_ic)
      real result_reel(length_ic)
      integer longueur(1)
      CHARACTER*10 subsystem,item
c
      subsystem='Energy\0'
      item='Gain0\0'
      longueur(1)=length_ic

      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)
      Do iy=1,MaxY
         do ix=1,MaxX
            gain(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo

      subsystem='Energy\0'
      item='Pedestals\0'
      
      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)
      Do iy=1,MaxY
         do ix=1,MaxX
            pedestal(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo
      
      subsystem='Timing\0'
      item='Tch\0'
      
      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)

      Do iy=1,MaxY
         do ix=1,MaxX
            tchan(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo
      subsystem='status\0'
      item='status\0' 
      
      call ic_read_map_int(irun,longueur,result_ent,subsystem,item)
      
      Do ix=1,MaxX
         do iy=1,MaxY
            ic_status(ix,iy)=result_ent(ix+23*(iy-1))
         enddo
      Enddo

      subsystem='Geometry\0'
      item='stepx\0'
      longueur(1)=1
      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)
      xstep = result_reel(1)

      item='stepy\0'
      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)
      ystep = result_reel(1)
      
      item='x0\0'
      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)
      x0 = result_reel(1)

      item='y0\0'
      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)
      y0 = result_reel(1)

      item='z0\0'
      call ic_read_map_float(irun,longueur,result_reel,subsystem,item)
      z0 = result_reel(1)

      print *,ehitmin,egrplmin,egrpmin,ihalf,MaxX,MaxY
      
      Do ix=1,MaxX
         do iy=1,MaxY
            zpos(ix,iy)=z0
            xpos(ix,iy)=x0+float(12-ix)*xstep
            ypos(ix,iy)=y0+float(iy-12)*ystep
            if(ix.eq.ihalf.and.iy.eq.ihalf)print *,'X0 and Y0',xpos(ix
     $           ,iy),ypos(ix,iy),x0,y0
         enddo
      EndDo
      rdist=sqrt(xstep*xstep+ystep*ystep)

 101  continue
 111  format(i4,12f9.2)
c     
      return
      end

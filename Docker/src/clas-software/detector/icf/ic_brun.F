      Subroutine ic_brun(irun)
c     
      IMPLICIT NONE
      SAVE
#include "icraw.inc"
#include "iccalib.inc"
#include "ic.inc"
      CHARACTER*99  call_map
      integer irun,iopen,ist,idmod
      integer ix,iy,ixt,iyt
      integer index
      real thx,thy,thz,phx,phy,phz 
      real dx1,dx2,dy1,dy2,dz,target_position(3)
      integer result_ent(length_ic)
      real result_reel(length_ic)
      integer longueur(1)
      CHARACTER*10 subsystem,item
c

      longueur(1)=length_ic
      call ic_read_map_float(irun,longueur,result_reel,'Energy','Gain0')

      Do iy=1,MaxY
         do ix=1,MaxX
            gain(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo

      call ic_read_map_float(irun,longueur,result_reel,'Energy','Pedestals')
      Do iy=1,MaxY
         do ix=1,MaxX
            pedestal(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo
      
      subsystem='Timing\0'
      item='Tch\0'
      call ic_read_map_float(irun,longueur,result_reel,'Timing','Tch')

      Do iy=1,MaxY
         do ix=1,MaxX
            tchan(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo
      call ic_read_map_float(irun,longueur,result_reel,'Timing','T0')

      Do iy=1,MaxY
         do ix=1,MaxX
            IC_TDC0(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo

      subsystem='Timing\0'
      item='T1\0'
      call ic_read_map_float(irun,longueur,result_reel,'Timing','T1')

      Do iy=1,MaxY
         do ix=1,MaxX
            IC_TDC1(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo
      call ic_read_map_float(irun,longueur,result_reel,'Timing','T2')

      Do iy=1,MaxY
         do ix=1,MaxX
            IC_TDC2(ix,iy)=result_reel(ix+(iy-1)*MaxY)
         enddo
      EndDo
      call ic_read_map_int(irun,longueur,result_ent,'status','status')
      
      Do ix=1,MaxX
         do iy=1,MaxY
            ic_status(ix,iy)=result_ent(ix+23*(iy-1))
         enddo
      Enddo

      subsystem='Geometry\0'
      item='stepx\0'
      longueur(1)=1
      call ic_read_map_float(irun,longueur,result_reel,'Geometry','stepx')
      xstep = result_reel(1)

      item='stepy\0'
      call ic_read_map_float(irun,longueur,result_reel,'Geometry','stepy')
      ystep = result_reel(1)
      
      item='x0\0'
      call ic_read_map_float(irun,longueur,result_reel,'Geometry','x0')
      x0 = result_reel(1)

      item='y0\0'
      call ic_read_map_float(irun,longueur,result_reel,'Geometry','y0')
      y0 = result_reel(1)
      if(z0d.lt.-500)then
       item='z0\0'
       call ic_read_map_float(irun,longueur,result_reel,'Geometry','z0')
       z0 = result_reel(1)
       print *,"db call IC front position:",z0
      else
       z0=z0d 
       print *,"tcl call IC front position:",z0
      endif 

      Do ix=1,MaxX
         do iy=1,MaxY
            zpos(ix,iy)=z0
            xpos(ix,iy)=x0+float(12-ix)*xstep
            ypos(ix,iy)=y0+float(iy-12)*ystep
         enddo
      EndDo
c
      subsystem='Recon\0'
      item='deepZ\0'
      longueur(1)=1
      call ic_read_map_float(irun,longueur,result_reel,'Recon','deepZ')
      deep = result_reel(1)
 
      if(ehitmind.lt.0.and.egrplmind.lt.0.and.egrpmind.lt.0)then
      item='ehitmin\0'
      call ic_read_map_float(irun,longueur,result_reel,'Recon','ehitmin')
      ehitmin = result_reel(1)
      item='eclustmin\0'
      call ic_read_map_float(irun,longueur,result_reel,'Recon','eclustmin')
      egrpmin = result_reel(1)
      item='ecentrmin\0'
      call ic_read_map_float(irun,longueur,result_reel,'Recon','ecentrmin')
      egrplmin = result_reel(1)
      print *,"db call IC thresholds:",ehitmin,egrplmin,egrpmin
      else 
         ehitmin=ehitmind
         egrplmin=egrplmind
         egrpmin=egrpmind
         print *,"tcl call IC thresholds:",ehitmin,egrplmin,egrpmin
      endif   
      item='w0\0'
      call ic_read_map_float(irun,longueur,result_reel,'Recon','w0')
      w0 = result_reel(1)
      item='encorr\0'
      longueur(1)=48
      call ic_read_map_float(irun,longueur,result_reel,'Recon','encorr')
      Do iy=1,16
         do ix=1,3
            encorr(ix,iy)=result_reel(ix+(iy-1)*3)
         enddo
      EndDo
      item='deepcorr\0'
      longueur(1)=48
      call ic_read_map_float(irun,longueur,result_reel,'Recon','deepcorr')
      Do iy=1,16
         do ix=1,3
            dcorr(ix,iy)=result_reel(ix+(iy-1)*3)
         enddo
      EndDo
c     
c     get beam position
      call revinm('CLAS_PARMS','Maps/GEOMETRY.map',call_map)
      CALL map_get_float(call_map,'target','position',3,target_position,irun,index)
      zt0=z0-target_position(3)

 101  continue
 111  format(i4,12f9.2)
c     
      return
      end

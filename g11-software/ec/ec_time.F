C=========================================
      real function ec_time(Layer,hit,sct)
      implicit none
C     Subroutine function to calculate an EC hit time by taking an ADC/rms weighted
C     average of hit times of strips(stacks) involved in a given hit.
C     Author: Ky Kim (Univ of Pittsburgh)
C     First Release: May-11-1999

#include "Ec_general.PAR"
#include "EcFit_general.CMN"
#include "EcFit_edge.CMN"
#include "ec_strip.inc"
#include "ec_rawdata.inc"
#include "EcCal.CMN"

      integer Layer,hit,sct

C.... local variables
      real pars(5)              ! calibration parameters of a strip
      real rms                  ! rms of a strip
      real RawADC               ! raw ADC of a strip (pedestal subtracted)
      real RawTDC               ! raw TDC of a strip

      integer axis              ! index for three views (U, V, & W)
      integer ist               ! index
      real xin,yin              ! (x,y) coordinate of the entrance point
      real xout,yout            ! (x,y) coordinate of the exit point

      real distance             ! a distance from EC_hit to the photo-tube
      real exit_distance
      real volume               ! time walk correction factor

      real thickness(Inner:Outer)
C
      integer strip             ! as implied
      integer ipars             ! index for calibration parameters
      integer mlayer            ! index for layer
      integer HitID             ! Hit pointing to inner/outer hit
      real max_adc 

      real sumofectime          ! ADC/rms weighted sum of strip time
      real weighting            ! ADC/rms weighting factor

      real dist2ec_pmt_f        ! a subroutine function
      real ec_strip_time        ! a subroutine function
C

      sumofectime=0.0
      weighting=0.0
      ec_time=-999.

C.... First deal with "WHOLE_HIT"

      if(Layer.eq.Whole) then

C.... Resolve it into "INNER/OUTER" hit
         do mlayer=Inner, Outer

            HitID=matched(Layer,mlayer,hit,sct)

            if(HitID.gt.0) then

               call get_geom(EcFit_hit(Ec_x,HitID,mlayer,sct),
     &              EcFit_hit(Ec_y,HitID,mlayer,sct),
     &              EcFit_hit(Ec_z,HitID,mlayer,sct),
     &              xin,yin,xout,yout,thickness,sct,mlayer)


               do axis=Ec_U, Ec_W
C.... Find the highest ADC strip in a given view
                  strip=0
                  max_adc=0.
                  do ist=1, Maxstrips
                     if(stripc(ist,HitID,axis,mlayer,sct).ge.1.0) then
                        if(ec_raw_adcs(ist,axis,mlayer,sct).gt.max_adc) then
                           strip=ist
                           max_adc=ec_raw_adcs(ist,axis,mlayer,sct)
                        endif
                     endif
                  enddo

                  if(strip.gt.0) then
                     RawADC=ec_raw_adcs(strip,axis,mlayer,sct) ! pedestal subtracted
                     if(RawADC.ge.10000.) RawADC=10000.

                     RawTDC=ec_raw_tdcs(strip,axis,mlayer,sct)
                     if(RawTDC.ge.4096.) RawTDC=4096.

C.... get calibration constants
                     if(EcCal_Tch(strip,axis,mlayer,sct).gt.0.0 .and. 
     &                    RawADC.gt.1.0 .and. RawTDC.gt.1.0) then
                        pars(1)=EcCal_Tch(strip,axis,mlayer,sct)
                        pars(2)=EcCal_To(strip,axis,mlayer,sct)
                        pars(3)=EcCal_Tadc(strip,axis,mlayer,sct)
                        pars(4)=EcCal_dT1(strip,axis,mlayer,sct)
                        pars(5)=EcCal_dT2(strip,axis,mlayer,sct)
                        rms=EcCal_Trms(strip,axis,mlayer,sct)

C.... Calculate the distances to the phototube edge for the entrance point and 
C     the exit point.
                        distance = dist2ec_pmt_f(xin,yin,axis-Ec_U+1,mlayer-Inner+1)
                        exit_distance = dist2ec_pmt_f(xout,yout,axis-Ec_U+1,mlayer-Inner+1)

C     Now calculate the "volume" quantity
                        volume=-thickness(mlayer)/30.
     &                       +(distance-exit_distance)/SPEED_IN_PLASTIC

                        if(mlayer.eq.Inner) then
                           sumofectime=sumofectime
     &                          +ec_strip_time(pars,RawTDC,RawADC,distance,volume)
     &                          *sqrt(RawADC)/rms
                        elseif(mlayer.eq.Outer) then
                           sumofectime=sumofectime
     &                          +(ec_strip_time(pars,RawTDC,RawADC,distance,volume)
     &                          -thickness(Inner)/30.)
     &                          *sqrt(RawADC)/rms
                        endif
                        weighting=weighting+sqrt(RawADC)/rms

                     endif      ! if(pars(1).gt.0.0 ...
                  endif         ! if(strip.gt.0) ...
               enddo            ! do axis = Ec_U, ...
            endif               ! if(HitID.gt.0) ...
         enddo                  ! do mlayer=Inner, ...


C.... Now we are dealing with the Inner/Outer hits
      elseif(Layer.eq.Inner .or. Layer.eq.Outer) then

         call get_geom(EcFit_hit(Ec_x,Hit,Layer,sct),
     &        EcFit_hit(Ec_y,Hit,Layer,sct),
     &        EcFit_hit(Ec_z,Hit,Layer,sct),
     &        xin,yin,xout,yout,thickness,sct,Layer)

         do axis=Ec_U, Ec_W

C.... Find the highest ADC strip in a given view
            strip=0
            max_adc=0.
            do ist=1, Maxstrips
               if(stripc(ist,hit,axis,Layer,sct).ge.1.0) then
                  if(ec_raw_adcs(ist,axis,Layer,sct).gt.max_adc) then
                     strip=ist
                     max_adc=ec_raw_adcs(ist,axis,Layer,sct)
                  endif
               endif
            enddo

            if(strip.gt.0) then
               RawADC=ec_raw_adcs(strip,axis,Layer,sct)
               if(RawADC.ge.10000.) RawADC=10000.

               RawTDC=ec_raw_tdcs(strip,axis,Layer,sct)
               if(RawTDC.ge.4096.) RawTDC=4096.

C.... get calibration constants
               if(EcCal_Tch(strip,axis,Layer,sct).gt.0.0 .and.
     &              RawADC.gt.1.0 .and. RawTDC.gt.1.0) then
                  pars(1)=EcCal_Tch(strip,axis,Layer,sct)
                  pars(2)=EcCal_To(strip,axis,Layer,sct)
                  pars(3)=EcCal_Tadc(strip,axis,Layer,sct)
                  pars(4)=EcCal_dT1(strip,axis,Layer,sct)
                  pars(5)=EcCal_dT2(strip,axis,Layer,sct)
                  rms=EcCal_Trms(strip,axis,Layer,sct)

C     Calculate the distances to the phototube edge for the entrance point and 
C     the exit point.
                  distance = dist2ec_pmt_f(xin,yin,axis-Ec_U+1,Layer-Inner+1)
                  exit_distance = dist2ec_pmt_f(xout,yout,axis-Ec_U+1,Layer-Inner+1)

C     Now calculate the "volume" quantity
                  volume=-thickness(Layer)/30.+
     &                 (distance-exit_distance)/SPEED_IN_PLASTIC

                  if(Layer.eq.Inner) then
                     sumofectime=sumofectime
     &                    +ec_strip_time(pars,RawTDC,RawADC,distance,volume)
     &                    *sqrt(RawADC)/rms
                  elseif(Layer.eq.Outer) then
                     sumofectime=sumofectime
     &                    +(ec_strip_time(pars,RawTDC,RawADC,distance,volume)
     &                    -thickness(Inner)/30.)
     &                    *sqrt(RawADC)/rms
                  endif
                  weighting=weighting+sqrt(RawADC)/rms

               endif
            endif
         enddo
      endif

      if(weighting.gt.0.0) ec_time=sumofectime/weighting

      return
      end

C
C==============================================================
      real function ec_strip_time(pars,tdc,adc,distance,volume)
      implicit none

      real pars(5)
      real tdc,adc,distance,volume
      real SPEED_IN_PLASTIC/18.1/
C
      ec_strip_time = pars(1)*(tdc/1.E3)
     &     +pars(2)
     &     +pars(3)/sqrt(abs(adc))
     &     +pars(4)*abs(distance)**2
C     &     +pars(5)*abs(distance)**3
     &     +pars(5)*abs(volume)
     &     -distance/SPEED_IN_PLASTIC

      return
      end

C
C================================================
      real function dist2ec_pmt_f(x,y,axis,Layer)
C     Calculate the distance to the phototube edge from an EC hit
      implicit none

      real x,y
      integer axis,Layer

      integer i
      real a(2,3),b(3),c(3),d(3)
C....          u   ,    v   ,  w   axis
C      data a/94.860, 193.560, 94.860,    ! inner layer
C     &       96.290, 202.100, 96.290/    ! outer layer
      data (a(1,i),i=1,3) /94.860, 193.560, 94.860/   ! inner layer
      data (a(2,i),i=1,3) /96.290, 202.100, 96.290/   ! outer layer

      data b/-1.000,   0.000,  1.000/
      data c/ 0.512,  -1.000,  0.512/
      data d/ 1.000,   1.123,  1.097/

      dist2ec_pmt_f=(a(axis,Layer)+b(axis)*x+c(axis)*y)*d(axis)

      return
      end

C
C=====================================================================
      subroutine get_geom(x,y,z,xin,yin,xout,yout,thickness,sct,Layer)
      implicit none

#include "Ec_general.PAR"

C.... inputs
      real x,y,z
      integer sct,Layer

C.... outputs
      real xin,yin,zin
      real xout,yout,zout
      real thickness(Inner:Outer)

C.... internals
      real norm(3)
      real mag
      real rexit(3)

      integer i
C.... perpendicular unit vectors to each sector: via perp(sector,xyz)
      real perp(6,3)
      data (perp(1,i),i=1,3) / 0.423,  0.000,  0.906/ ! sector 1
      data (perp(2,i),i=1,3) / 0.212,  0.366,  0.906/ ! sector 2
      data (perp(3,i),i=1,3) /-0.212,  0.366,  0.906/ ! sector 3
      data (perp(4,i),i=1,3) /-0.423,  0.000,  0.906/ ! sector 4
      data (perp(5,i),i=1,3) /-0.212, -0.366,  0.906/ ! sector 5
      data (perp(6,i),i=1,3) / 0.212, -0.366,  0.906/ ! sector 6

C.... perpendicular thicknesses of inner and outer layer in cm
      real perp_thickness(Inner:Outer)
      data perp_thickness /18.1,29.3/

C
      
      thickness(Inner)=0.
      thickness(Outer)=0.
      xin=0.
      yin=0.
      xout=0.
      yout=0.

      mag=sqrt(x*x+y*y+z*z)
      norm(1)=x/mag
      norm(2)=y/mag
      norm(3)=z/mag

C.... Calculate the thickness traversed through the calorimeter layer
      mag=norm(1)*perp(sct,1)+norm(2)*perp(sct,2)+norm(3)*perp(sct,3)

      thickness(Inner) = perp_thickness(Inner)/mag ! thickness in cm
      thickness(Outer) = perp_thickness(Outer)/mag ! thickness in cm

C.... Calculate the vector to the exit point of the stack:
      rexit(1)=x+norm(1)*thickness(Layer)
      rexit(2)=y+norm(2)*thickness(Layer)
      rexit(3)=z+norm(3)*thickness(Layer)

C     In the following, calculate local coordinates from the global coordinates, assuming
C     the ideal geometry. A better way would be to have the local coordinates directly 
C     from the data file, in case some day we use the survey-corrected geometry.

C     Calculate the local coordinates of the entrance point:
      call ec_local_xyz_f(x,y,z,xin,yin,zin)

C     Calculate the local coordinates of the exit point:
      call ec_local_xyz_f(rexit(1),rexit(2),rexit(3),xout,yout,zout)

      return
      end

C
C================================================================
      subroutine ec_local_xyz_f(lab_x,lab_y,lab_z,ec_x,ec_y,ec_z)
      implicit none

c     Stepan's originally, modified by W. Brooks to invert
c     what is used in the reconstruction for inner and 
c     outer.
      real lab_x,lab_y,lab_z    ! input
      real ec_x,ec_y,ec_z       ! output

C     locals
      real theta/0.4363323/     ! 25 degree
      real phi
      real costheta, sintheta, cosphi, sinphi


C      real x,y,z,u,v,w,xi,yi,zi
C      real EC_the,ec_phi,phi,ylow(2),yhi(2),tgrho,sinrho,cosrho
C      data EC_the/0.4363323/

c     I believe these numbers are not exactly what was in the
c     design drawings, however, they're in the reconstruction code.
c      data ylow,yhi/-182.974,189.956/
c      data ylow / -182.974-0.,-182.974-15.*0.43708 /
c      data yhi  /  189.956+0., 189.956+15.*0.45419 /

C      data ylow/-182.974,-189.530 /
C      data yhi/ 189.956, 196.956 /

C      data tgrho,sinrho,cosrho/1.95325,0.8901256,0.455715/
C      real rot(3,3)
C      integer in_out            ! = 1 for inner, = 2 for outer

C      real costheta, sintheta, cosphi, sinphi

      phi=atan2(lab_y,lab_x)*57.29578
      if(phi.lt.0.) phi=phi+360.
      phi=phi+30.
      if(phi.ge.360.) phi=phi-360.      
      phi=int(phi/60.)*1.0471975

      costheta = cos(theta)
      sintheta = sin(theta)
      cosphi = cos(phi)
      sinphi = sin(phi)
c
C      rot(1,1)=cos(Ec_the)*cos(Ec_phi)
C      rot(1,2)=-sin(Ec_phi)
C      rot(1,3)=sin(Ec_the)*cos(Ec_phi)
C      rot(2,1)=cos(Ec_the)*sin(Ec_phi)
C      rot(2,2)=cos(Ec_phi)
C      rot(2,3)=sin(Ec_the)*sin(Ec_phi)
C      rot(3,1)=-sin(Ec_the)
C      rot(3,2)=0.
C      rot(3,3)=cos(Ec_the)
c
C      xi=x*rot(1,1)+y*rot(2,1)+z*rot(3,1)
C      yi=x*rot(1,2)+y*rot(2,2)+z*rot(3,2)
C      zi=x*rot(1,3)+y*rot(2,3)+z*rot(3,3)
C      zi=zi-510.32


      ec_x = lab_x*costheta*cosphi + lab_y*costheta*sinphi - lab_z*sintheta
      ec_y = -lab_x*sinphi + lab_y*cosphi
      ec_z = lab_x*sintheta*cosphi + lab_y*sintheta*sinphi + lab_z*costheta - 510.32

C      u=(yi-ylow(in_out))/sinrho
C      v=(yhi(in_out)-ylow(in_out))/tgrho-xi+(yhi(in_out)-yi)/tgrho
C      w=((yhi(in_out)-ylow(in_out))/tgrho+xi+(yhi(in_out)-yi)/tgrho)/
C     &     2./cosrho

      return
      end


c*******************************************************************************
	subroutine angles(iflag,v1,v2,v3,theta,phi)
c**********	
c+ INPUT	
c++	iflag = 0 calculating th and phi in degrees
c++	      = 1 calculating th and phi in rad
c++	v1,v2,v3 = input vector components
c+ OUTPUT
c++     theta, phi = calcalated angles      
c*******************************************************************************

	implicit none
	integer iflag
	real v1,v2,v3,v_m,theta,phi,php1

	v_m = sqrt(v1**2+v2**2+v3**2)

c-----------------------------------------
c	angles in the new frame (degrees)
c-----------------------------------------

	if(iflag.eq.0) then

	  if(v_m.gt.0.) then

	    theta = acos((v3/v_m))*180./3.14159265

	    if (v1.eq.0..and.v2.eq.0.) phi = 0

	    if (v1.gt.0..and.v2.ge.0.) then
 	    php1 = v2/v1
	    phi = atan(php1)*180./3.14159265
	    endif

	    if (v1.eq.0..and.v2.gt.0.) phi = 90.

	    if (v1.lt.0..and.v2.ge.0.) then
	    php1 = v2/v1
	    phi = atan(php1)*180./3.14159265 + 180.
	    endif
	
	    if (v1.lt.0..and.v2.lt.0.) then
	    php1 = v2/v1
	    phi = atan(php1)*180./3.14159265 + 180.
	    endif

	    if (v1.eq.0..and.v2.lt.0.) phi = 270.

	    if (v1.gt.0..and.v2.lt.0.) then
	    php1 = v2/v1
	    phi = atan(php1)*180./3.14159265 + 360.
	    endif

	  else

	    theta = 0.
	    phi = 0.

	  endif


	endif

c-----------------------------------------
c	angles in the new frame (radians)
c-----------------------------------------

	if(iflag.eq.1) then
	

	  if(v_m.gt.0.) then

	    theta = acos(v3/v_m)

	    if (v1.eq.0..and.v2.eq.0.) phi = 0.

	    if (v1.gt.0..and.v2.ge.0.) then
	    php1 = v2/v1
	    phi = atan(php1)
	    endif

	    if (v1.eq.0..and.v2.gt.0.) phi = 1.5708

	    if (v1.lt.0..and.v2.ge.0.) then
	    php1 = v2/v1
 	    phi = atan(php1) + 3.14159265
	    endif
	
	    if (v1.lt.0..and.v2.lt.0.) then
	    php1 = v2/v1
	    phi = atan(php1) + 3.14159265
	    endif

	    if (v1.eq.0..and.v2.lt.0.) phi = 4.7124

	    if (v1.gt.0..and.v2.lt.0.) then
	    php1 = v2/v1
	    phi = atan(php1) + 6.2831852
	    endif

	  else

	    theta = 0.
	    phi = 0.

	  endif


	endif
c-------------------------------------------------------------------------------


	return

	end



c------------------------------------------------------------------------------
c       *************************
        function sector(angle)
c       ******c       Modified to be in accordance with PHI range from
c       -180 - +180;
c       
	real angle
        integer sector

        if (angle.le.30.or.angle.ge.330)   sector = 1
        if (angle.ge.30.and.angle.le.90)   sector = 2
        if (angle.ge.90.and.angle.le.150)  sector = 3
        if (angle.ge.150.and.angle.le.210) sector = 4
        if (angle.ge.210.and.angle.le.270) sector = 5
        if (angle.ge.270.and.angle.le.330) sector = 6

	return
	end


c ******************************************************************
c      sind,cosd,tand - functions with an argument being in radians
c      They should be intrinsic but in LINUX they are not 
c
c ******************************************************************
c      function sind(angle)
c      real angle,sind
c      sind = sin(angle*3.14159/180.)
c      return
c      end
c      function cosd()
c      real angle,cosd
c      cosd = cos(angle*3.14159/180.)
c      return
c      end
c      function tand()
c      real angle,tand
c      tand = tan(angle*3.14159/180.)
c      return
c      end

c ******************************************************************


C*********************************
	SUBROUTINE GAUSS(S,AM,V)
C********************************
	IMPLICIT NONE
	REAL RRAN
	REAL A,AM,V,S
	INTEGER*4 IZ,I
	iz=337699231
	A=0.
	DO 50 I=1,12
50	A=A+rran()
	V=(A-6.0)*S+AM
	END    

	FUNCTION RRAN()
	REAL RRAN
	CALL RANLUX(RRAN,1)
	RETURN
	END

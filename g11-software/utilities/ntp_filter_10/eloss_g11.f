      SUBROUTINE eloss_g11( pout, pmass, vertex, iflag, icell, pin )
c
c_begin_doc
c  RCS ID string
c  $Id: momcor.F,v 1.6 2004/02/24 21:44:59 pasyuk Exp $
c
c  Documentation for subroutine MOMCOR
c
c  Purpose: calculates charged particle momentum at vertex.  
c  --------
c
c!!!!!!!!!!! configuration for photon runs only !!!!!!!!!!!!!!!!!!!!!!!
c
c           Accounts for energy losses in cryo target, its wall,
c           carbon cylinder and start counter.
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  vertex(3) (real) vertex coordinates
c  pmass     (real) particle mass
c  pout(3)   (real) reconstructed particle momentum (vector) behind 
c                   the start counter
c
c  iflag     (integer) iflag = 0 - empty target 
c                      iflag = 1 - LH2 target
c                      iflag = 2 - LD2 target
c                      iflag = 3 = L3He target
c                      iflag = 4 = L4He target
c 
c  icell     (integer) target cell type:
c                      icell = 0 - no target
c                      icell = 1 - G1A/G1B/G6 running period
c                      icell = 2 - G2A running period
c                      icell = 3 - G1C running period
c                      icell = 4 - G3  running period
c                      icell = 5 - G6C/G8 running period
c                      icell = 6 - G11 running period
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  pin(3)    (real) particle momentum at vertex
c
c  Other routines:
c  ---------------
c  stcounter, ccylinder, LH2targ, loss
c
c  Notes:
c  ------
c
c  Author:   Eugene Pasyuk      Created:  Tue Apr  6 16:39:57 EDT 1999
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER iflag
      INTEGER icell,iwin
      REAL pout(3)
      REAL pmass
      REAL vertex(3)
      REAL pin(3)
      
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: momcor.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.6 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/24 21:44:59 $')
      PARAMETER (CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/   
     1'$Id: momcor.F,v 1.6 2004/02/24 21:44:59 pasyuk Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='MOMCOR')
      PARAMETER (CRAUTH='Eugene Pasyuk')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER i
      INTEGER ierr
      INTEGER mate
      INTEGER loss
      REAL p(3)
      REAL po(3)
      REAL pi(3)
      REAL dist
      REAL dist1
      REAL dens_foam, dens_scint, dens_sins
      REAL cdir(3)
      REAL pmom
      REAL gap
      REAL st_point(3)
      REAL r1point(3)

      real target_offset(3), st_offset
      common/eloss_geom/ target_offset, st_offset
      data target_offset, st_offset/0.0, 0.0, 0.0, 0.0/
      data nwrite/0/
c_end_var
c
c  executable code for routine MOMCOR:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
c        CRMESS='First call to MOMCOR'
c        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c---------------------------------------------------------------------

      if(icell.eq.6)then
        target_offset(1)=  0.0
        target_offset(2)=  0.0
        target_offset(3)=-10.0
        st_offset = -10.0
      else
        continue
      endif

      pmom = sqrt(pout(1)**2 + pout(2)**2 + pout(3)**2)
      do i = 1,3
         po(i) = pout(i)
         cdir(i) = pout(i)/pmom
      enddo
      

ccc      print *,'ELOSS: '
ccc      print *,'ELOSS: ============================================'
cc      print *,'ELOSS: iflag, icell=',iflag, icell
ccc      print *,'ELOSS: pout=',pout
cc      print *,'ELOSS: dir   =',cdir(1),cdir(2),cdir(3)
cc      print *,'ELOSS: vertex=',vertex

      call air_gap(vertex, cdir, r1point) !air gap

cc      print *,'ELOSS: point DC1=',r1point

      call stcounter(icell,vertex, cdir, dist, st_point)

cc      print *,'ELOSS: point ST =',st_point
cc      print *,'ELOSS: BEFOR AIR =',po


      gap = sqrt((r1point(1)-st_point(1))**2 + 
     +     (r1point(2)-st_point(2))**2 +
     +     (r1point(3) - st_point(3))**2)
      mate = 8                          ! air
      ierr = loss(mate, gap, pmass, po, p)
      do i = 1,3 
         pi(i) = p(i)
      enddo

ccc      print *,'ELOSS: ST dist = ',dist
ccc      print *,'ELOSS: AFTER AIR =',p

c dist is an affective dist foam+scintillator
      mate = 2            ! Start Counter scintillator
      ierr = loss(mate, dist, pmass, pi, p)
      do i = 1,3
         pi(i) = p(i)
      enddo

ccc      print *,'ELOSS: AFTER  ST =',p

      call ccylinder(icell,vertex, cdir, dist, iwin) ! carbon cylinder

      if ( dist .ne. 0.) then
         if(icell.eq.6)then
           if(iwin.eq.0)then
           mate = 2  ! sciltill.in place of foam+scint (dencity scaled)
           dens_foam  = 0.107 / (0.385*2.54) !g/cc
           dens_scint = 1.03                 !g/cc
           dist = dist * (dens_foam/dens_scint)
           else
           mate = 6  ! exit window (Al)
           endif
         else
           mate = 3  ! carbon cylinder (Carbon)
         endif
         ierr = loss(mate, dist, pmass, pi, p)
      endif

      do i = 1,3
         pi(i) = p(i)
      enddo

ccc      print *,'ELOSS: AFTER  CC =',p

      call targcell(icell, vertex, cdir, dist, dist1) ! target cell

      if ( dist .ne. 0.) then

c For simplicity we will use plastic scintillator (mate=2) 
c as a target cell wall material.
c
c Even for G3 cell, which has aluminum foil besides plastic.
c The thickness of Al is 0.001" is small compared to total thickness (0.0075") 

c For G11 cell use plastic scintillator
c in place of superinsulation layers (density scaled)

         mate = 2          
         if(icell.eq.6)then
           dens_sins = 5*1.88E-3 / 0.25 !g/cc Density of superins mate
           dens_scint = 1.03            !g/cc Density of plastic scint
           dist = dist * (dens_sins/dens_scint)
         else
           continue
         endif
         ierr = loss(mate, dist1, pmass, pi, p)

         do i = 1,3
            pi(i) = p(i)
         enddo

ccc         print *,'ELOSS: AFTER  CE =',p


         if(iflag .ne. 0) then   !check if the target is empty or full
            mate = 0

            if(iflag .eq. 1) mate = 1 ! LH2
            if(iflag .eq. 2) mate = 4 ! LD2
            if(iflag .eq. 3) mate = 5 ! L3He
            if(iflag .eq. 4) mate = 7 ! L4He

            ierr = loss(mate, dist, pmass, pi, p)
            do i = 1,3
               pi(i) = p(i)
            enddo
         endif
      endif

      do i = 1,3
         pin(i) = pi(i)
      enddo

ccc      print *,'ELOSS: AFTER  H2 =',pi

      RETURN
      END
c
c------------------------------------------------------------------------------
      SUBROUTINE CCYLINDER( icell, vertex, cdir, dist, iwin )
c
c_begin_doc
c  RCS ID string
c  $Id: ccylinder.F,v 1.5 2002/12/07 16:38:19 silvia Exp $
c
c  Documentation for subroutine CCYLINDER
c
c  Purpose: Calcualte the ranage within carbon cylinder
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  vertex(3) (real) vertex coordinates
c  cdir(3)   (real) track direction cosines
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  dist (real) track length within the scintillator
c  iwin      (ineger) 0->part. goes through walls 1-> part. goes through 
c                     exit window  
c
c  Other routines:
c  ---------------
c  IntsectionCylindre
c
c  Notes:
c  ------
c
c  Author:   Eugene Pasyuk      Created:  Tue Apr  6 16:39:04 EDT 1999
c  -------
c
c  Major revisions:
c  ----------------
c  substituted IntsectionCylindre with IntCylinder 
c  (Dec. 6, 2002, Niccolai/Strauch)
c     
c
c_end_doc
c
      IMPLICIT NONE
c      SAVE
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER icell,iwin
      REAL vertex(3)
      REAL cdir(3)
      REAL dist

c  Local pre-defined variables:
c  ---------------------------
      REAL centre(3)            
      REAL rayon                !radius of the pipe
      REAL rayon_win            !radius of the exit window
      REAL thickness            !thicknes of the pipe
      REAL thickness_win        !thicknes of the exit window
      REAL extremtube           !half length
      REAL alpha                !cone angle
      REAL dir(3)               !direction of pipe axis
      REAL pi
      DATA rayon/9.4/
      DATA thickness/0.2/
      DATA extremtube/17.2/
      DATA alpha/30./
      DATA dir/0., 0., 1./
      DATA pi/3.14159265359/
     
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccylinder.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/12/07 16:38:19 $')
      PARAMETER (CAUTHO = '$Author: silvia $')
      DATA CRCSID/   
     1'$Id: ccylinder.F,v 1.5 2002/12/07 16:38:19 silvia Exp $'   
     2/   

c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CCYLINDER')
      PARAMETER (CRAUTH='Eugene Pasyuk')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ierr
      INTEGER i
      REAL point(3)
      REAL pos1(3), pos2(3)
      REAL intersec(3)
      REAL r

      DATA NWRITE/0/

      real target_offset(3), st_offset
      common/eloss_geom/ target_offset, st_offset

c#ifdef Linux

c for Linux these functions are not implemeted in standard library
c we define them here
c
c      REAL sind, cosd, tand, angle
c      sind(angle) = sin( angle*pi/180. )
c      cosd(angle) = cos( angle*pi/180. )
c      tand(angle) = tan( angle*pi/180. )
c#endif

c#include "eloss_geom.PAR"


c_end_var
c
c  executable code for routine CCYLINDER:
c----6----------------------------------------------------------------72

      iwin = 0

      if(icell.eq.6)then
        thickness     = 0.385*2.54   !thicknes of the pipe
        thickness_win = 0.071        !thicknes of the exit window
        rayon      = (3.72/2)*2.54 - thickness/2.!radius of the pipe
        rayon_win  = (1.575/2.)*2.54             !radius of the exit win
        extremtube = 8.928*2.54   !half length
        alpha      = 22.          !cone angle
      endif

cc      print *,'  LOSS: CCYL-----------------------------'


c------------------------------------------------------------------------------
      do i = 1,3
         point(i) = vertex(i) + 50.*cdir(i)
         centre(i) = target_offset(i)
      enddo
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
c         write(CRMESS,*) 
c     + 'CCYLINDER: target position is: ', target_offset
c         CRMESS = 'First call to TARGCELL'
c         CALL RECMES(CRNAME,'I',CRMESS)
         
      ENDIF
c     
c------------------------------------------------------------------------------

      call IntCylinder(vertex, cdir, centre, dir, rayon,
     $     pos1, ierr)
      if (ierr .eq. 0 .or. ierr .eq. 2) then

c        No further treatment if:
c         - particle travels along the cylinder axis (ierr = 0)
c         - particle starts outside of the cylinder and travels 
c           away from the cylinder (ierr = 0)
c         - particle starts outside of the cylinder and travels
c           towards the cylinder and would have two intersections (ierr = 2)
c           [ should not happen in the case of a true event ]

         dist = 0.              ! get out
         return
      endif
cc      print *,'  LOSS: CCYL,pos1=',pos1

c--- cylinder or cone?
      if (pos1(3) .gt. extremtube+centre(3) .or. ierr .eq. -1) then
cc         print *,'  LOSS: CONO'
         if(icell.eq.6)then  ! Nose is a flat circle
           centre(3) = centre(3)+ extremtube
           call IntsectionPlan(vertex, point, centre, dir, pos1, ierr)

           if(sqrt(pos1(1)**2+pos1(2)**2) .lt. rayon_win) iwin=1
cc           print *,'  LOSS0: =',ierr,iwin,pos1
           intersec(1) = pos1(1)
           intersec(2) = pos1(2)
           intersec(3) = pos1(3)
           if(iwin.eq.0) centre(3) = centre(3)+ thickness
           if(iwin.eq.1) centre(3) = centre(3)+ thickness_win
           call IntsectionPlan(vertex, point, centre, dir, pos1, ierr)

         else               ! Nose is a cone
           centre(3) = centre(3)+ extremtube + rayon/tan(alpha*pi/180.)
           call IntsectionCone(vertex, point, centre, dir, alpha,
     +          pos1, pos2, ierr)
           intersec(1) = pos1(1)
           intersec(2) = pos1(2)
           intersec(3) = pos1(3)
           centre(3) = centre(3) + thickness/sin(alpha*pi/180.)
           call IntsectionCone(vertex, point, centre, dir, alpha,
     +          pos1, pos2, ierr)
         endif

      else
c--- track goes through the cylinder

cc      print *,'  LOSS: CYLI'
         intersec(1) = pos1(1)
         intersec(2) = pos1(2)
         intersec(3) = pos1(3)
         r = rayon + thickness

         call IntCylinder(vertex, cdir, centre, dir,
     +        r, pos1, ierr)

      endif

      dist = 0.
      do i = 1,3
         dist = dist + (pos1(i) - intersec(i)) * (pos1(i) - intersec(i))
      enddo

      dist = sqrt(dist)	

c      print *,'  LOSS: inter=',intersec
c      print *,'  LOSS: pos1 =',pos1
c      print *,'  LOSS: thickness,dist =',thickness,dist

      RETURN
      END

c**********************************************************************

C
C set of subroutines which find an intersection, if any, of a strait line
C with a cylinder, plane, sphere and cone
c
c**********************************************************************

      subroutine IntCylinder (vertex, cdir, center, d, R, x, ierr)

c**********************************************************************


      implicit NONE

      real vertex(3), cdir(3), center(3)
      real d(3), r
      real x(3)
      real vp(3), va(3), vb(3)
      real a, b, c, delta
      real k1, k2, k

      real vp_dot_d
      real c_dot_d

      integer i,ierr

      ierr = -1

      do i = 1, 3
         vp(i) = vertex(i) - center(i)
         x(i) = 0.
      enddo

      vp_dot_d = 0.
      c_dot_d = 0.
      do i = 1, 3
         vp_dot_d = vp_dot_d + vp(i) * d(i)
         c_dot_d = c_dot_d + cdir(i) * d(i)
      enddo

      do i = 1, 3
         va(i) = cdir(i) - c_dot_d * d(i)
         vb(i) = vp(i) - vp_dot_d * d(i)
      enddo

      a = 0.
      b = 0.
      c = 0.
      do i = 1, 3
         a = a + va(i) * va(i)
         b = b + 2. * va(i) * vb(i)
         c = c + vb(i) * vb(i)
      enddo
      c = c - R * R

      delta = b * b - 4 * a * c

      if (delta .ge. 0.) then
         k1 = (-b + sqrt(delta)) / a / 2
         k2 = (-b - sqrt(delta)) / a / 2
         if (k1 .ge. 0.) then

c           valid intercept

            if (k2 .gt. 0.) then

c              two intercepts in the direction of the track

               k = k2
               ierr = 2

            else

c              one and only one intercept in the direction of the track

               k = k1
               ierr = 1
            
            endif

            do i = 1, 3
               x(i) = vertex(i) + k * cdir(i)
            enddo

         else

c           particle passed the cylinder already.            
c           no more intercept in the direction of the track.

            ierr = 0
         endif

      else

c        no intercept with cylinder

         ierr = 0
      endif
      end


c**********************************************************************

      subroutine IntsectionCylindre(m1, m2, m0, d, r, m, mm, rep)

c**********************************************************************


      implicit NONE

      real m1(3), m2(3), m0(3), d(3), m(3), mm(3), r
      real mm1(4), mm2(4), mm0(4), dd(4)
      integer rep, i
      real a, b, c, k1, k2, kt, delta, norme

      norme = 0.
      do i = 1,3
         norme = norme + d(i)*d(i)
      enddo
      
      if (1e-10 .gt. norme) then 
         rep = -3
         return
      endif

      do i = 1,3
         d(i) = d(i)/norme
      enddo

      do i = 1,3
         mm1(i) = m1(i)
         mm2(i) = m2(i)
         mm0(i) = m0(i)
         dd(i) = d(i)
      enddo

      mm1(4) = m1(1)
      mm2(4) = m2(1)
      mm0(4) = m0(1)
      dd(4) = d(1)

      a = 0.
      do i = 1,3
         a = a + (m2(i) - m1(i)) * (m2(i) - m1(i)) * (1. - d(i)*d(i))
     +        -2.*(m2(i) - m1(i)) * (mm2(i+1) - mm1(i+1)) *d(i)*dd(i+1)
      enddo
      
      if (1e-10 .gt. abs(a)) then
         rep = 0
         return
      endif

      b = 0.
      do i = 1,3
         b = b + (m2(i) - m1(i))*(m1(i) - m0(i))*(1. - d(i)*d(i))
         b = b - (m2(i) - m1(i))*(mm1(i+1) - mm0(i+1))*d(i)*dd(i+1)
         b = b - (m1(i) - m0(i))*(mm2(i+1) - mm1(i+1))*d(i)*dd(i+1)
      enddo

      c = 0.
      do i = 1,3
         c = c + (m1(i) - m0(i))*(m1(i) - m0(i))*(1. - d(i)*d(i))
         c = c - 2.*(m1(i) - m0(i))*(mm1(i+1) - mm0(i+1))*d(i)*dd(i+1)
      enddo
      c = c - r*r     
      delta = b*b - a*c
      if (0 .gt. delta) then
         rep = 0
         return
      endif

      k1 = (-b + sqrt(delta))/a
      k2 = (-b - sqrt(delta))/a

      rep = -1
      if (k1 .gt. 0. .and. 1. .gt. k1) rep = 1
      if (k2 .gt. 0. .and. 1. .gt. k2) then
         if (rep .eq. 1) then
            rep = 2
         else
            rep = 1
            kt = k1
            k1 = k2
            k2 = kt
         endif
      endif


      if (rep .eq. 2 .or. rep .eq. -1) then
         if (abs(k1) .gt. abs(k2)) then
            kt = k1
            k1 = k2
            k2 = kt
         endif
      endif
      
      do i = 1,3
         m(i) = m1(i) + k1*(m2(i) - m1(i))
         mm(i) = m1(i) + k2*(m2(i) - m1(i))
      enddo

      return
      end	
      
c     ******************************************************************

      subroutine IntsectionPlan(m1, m2, m0, n, m, rep)

c     ******************************************************************


      implicit NONE

      real m1(3), m2(3), m0(3), n(3), m(3)
      integer rep, i
      real kd, k

      if (n(1) .eq. 0. .and. n(2) .eq. 0. .and. n(3) .eq. 0.) then
         rep = -3
         return
      endif	

      rep = 0
      k = 0.
      do i = 1,3 
         k = k + (m1(i) - m0(i))*n(i)
      enddo
      
      if (1e-10 .gt. abs(k)) rep = -2

      kd = 0.
      do i = 1,3
         kd = kd + (m2(i) - m1(i))*n(i)
      enddo

      if (1e-10 .gt. kd) then
         if (rep .ne. -2) rep = 0
         return
      endif

      k = 0.
      do i = 1,3
         k = k + (m0(i) - m1(i))*n(i)
      enddo

      k = k/kd
      if (k .gt. 0. .and. 1. .gt. k)	then 
         rep = 1 
      else 
         rep = -1
      endif

      do i = 1,3
         m(i) = m1(i) + k*(m2(i) - m1(i))
      enddo

      return
      end	
      
c     ******************************************************************

      subroutine IntsectionSphere(m1, m2, m0, r, m, mm, rep)

c     ******************************************************************

      implicit NONE

      real m1(3), m2(3), m0(3), m(3), mm(3), r
      integer rep, i
      real a, b, c, k1, k2, kt, delta

      a = 0.
      do i = 1,3
         a = a + (m2(i) - m1(i))*(m2(i) - m1(i))
      enddo

      if (1e-10 .gt. abs(a)) then
         rep = 0
         return
      endif

      b = 0.
      do i = 1,3
         b = b + (m2(i) - m1(i))*(m1(i) - m0(i))
      enddo

      c = 0.
      do i = 1,3
         c = c + (m1(i) - m0(i))*(m1(i) - m0(i))
      enddo

      c = c - r*r
      delta = b*b - a*c
      if (0.gt.delta) then
         rep = 0
         return
      endif

      k1 = (-b + sqrt(delta))/a
      k2 = (-b - sqrt(delta))/a

      rep = -1
      if (k1 .gt. 0. .and. 1. .gt. k1) rep = 1
      if (k2 .gt. 0. .and. 1. .gt. k2) then
         if (rep .eq. 1) then
            rep = 2
         else
            rep = 1
            kt = k1
            k1 = k2
            k2 = kt
         endif
      endif

      if (rep .eq. 2 .or. rep .eq. -1) then
         if (abs(k1) .gt. abs(k2)) then
            kt = k1
            k1 = k2
            k2 = kt
         endif
      endif
      
      do i = 1,3
         m(i) = m1(i) + k1*(m2(i) - m1(i))
         mm(i) = m1(i) + k2*(m2(i) - m1(i))
      enddo

      return
      end	



c     ******************************************************************

      SUBROUTINE IntZCone(vertex, cdir, r1,r2, h, pos1, ierr)

c     ******************************************************************
      implicit NONE

      real vertex(3), cdir(3), pos1(3)
      real r1,r2, h, alf,bet
      integer ierr
      real a, b, c, d, t1,t2

      Alf = (r1-r2)/(-h)
      Bet =  r1
      A = cdir(1)**2 + cdir(2)**2 - alf**2*cdir(3)**2
      B = 2.*( cdir(1)*vertex(1) + cdir(2)*vertex(2) 
     &         - alf**2*cdir(3)*vertex(3) - alf*cdir(3)*bet )
      C = ( vertex(1)**2 + vertex(2)**2 - alf**2*vertex(3)**2 
     &      - bet**2 - 2.*alf*vertex(3)*bet )
      D = B**2 - 4.*A*C
      t1 = (-B - sqrt(D) )/(2.*A)
      t2 = (-B + sqrt(D) )/(2.*A)
      if(t2.gt.0.) t1=t2
      ierr=1
      if(D.lt.0) t1=0.
      if(D.lt.0) ierr=0
      pos1(1) = t1*cdir(1) + vertex(1)
      pos1(2) = t1*cdir(2) + vertex(2)
      pos1(3) = t1*cdir(3) + vertex(3)


      RETURN
      END	



c     ******************************************************************

      subroutine IntsectionCone(m1, m2, m0, d, alpha, m, mm, rep)

c     ******************************************************************

      implicit NONE

      real m1(3), m2(3), m0(3), d(3), m(3), mm(3), alpha
      real mm1(4), mm2(4), mm0(4), dd(4)
      integer rep, i
      real a, b, c, k1, k2, kt, delta, norme, c2

      c2 = cos(alpha/180.*3.14159265359)
      c2 = c2*c2

      norme = 0.
      do i = 1,3
         norme = norme + d(i)*d(i)
      enddo
      
      if (1e-10 .gt. norme) then 
         rep = -3
         return
      endif

      do i = 1,3
         d(i) = d(i)/norme
      enddo

      do i = 1,3
         mm1(i) = m1(i)
         mm2(i) = m2(i)
         mm0(i) = m0(i)
         dd(i) = d(i)
      enddo

      mm1(4) = m1(1)
      mm2(4) = m2(1)
      mm0(4) = m0(1)
      dd(4) = d(1)

      a = 0.
      do i = 1,3

         a = a + (m2(i) - m1(i))*(m2(i) - m1(i))*(d(i)*d(i) - c2)
     +        +2.*(m2(i) - m1(i))*(mm2(i+1) - mm1(i+1))*d(i)*dd(i+1)
      enddo
      
      if (1e-10 .gt. abs(a)) then
         rep = 0
         return
      endif

      b = 0.
      do i = 1,3
         b = b + (m2(i) - m1(i))*(m1(i) - m0(i))*(d(i)*d(i) - c2)
         b = b + (m2(i) - m1(i))*(mm1(i+1) - mm0(i+1))*d(i)*dd(i+1)
         b = b + (m1(i) - m0(i))*(mm2(i+1) - mm1(i+1))*d(i)*dd(i+1)
      enddo

      c = 0.
      do i = 1,3
         c = c + (m1(i) - m0(i))*(m1(i) - m0(i))*(d(i)*d(i) - c2)
         c = c + 2.*(m1(i) - m0(i))*(mm1(i+1) - mm0(i+1))*d(i)*dd(i+1)
      enddo
      
      delta = b*b - a*c
      if (0. .gt. delta) then
         rep = 0
         return
      endif

      k1 = (-b + sqrt(delta))/a
      k2 = (-b - sqrt(delta))/a

      rep = -1
      if (k1 .gt. 0. .and. 1. .gt. k1) rep = 1
      if (k2 .gt. 0. .and. 1. .gt. k2) then
         if (rep .eq. 1) then
            rep = 2
         else
            rep = 1
            kt = k1
            k1 = k2
            k2 = kt
         endif
      endif


      if (rep .eq. 2 .or. rep .eq. -1) then
         if (abs(k1) .gt. abs(k2)) then
            kt = k1
            k1 = k2
            k2 = kt
         endif
      endif
      
      do i = 1,3
         m(i) = m1(i) + k1*(m2(i) - m1(i))
         mm(i) = m1(i) + k2*(m2(i) - m1(i))
      enddo

      return
      end	
      

C--------------------------------------------------------------------

      SUBROUTINE LH2TARG(  vertex, cdir, dist, dist1 )
c
c_begin_doc
c  RCS ID string
c  $Id: lh2targ.F,v 1.1.1.1 2000/04/17 15:30:33 marki Exp $
c
c  Documentation for subroutine LH2TARG
c
c  Purpose: calculates track length within LH2 target and its mylar walls.
c  --------
c           g1 run target geometry
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  vertex(3) (real) vertex coordinates
c  cdir(3)   (real) track direction cosines  
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  dist (real) track length within the scintillator
c  dist1 (real) track length in mylar wall
c
c  Other routines:
c  ---------------
c  IntsectionSphere, IntsectionCylindre
c
c  Notes:
c  ------
c
c  Author:   Eugene Pasyuk      Created:  Tue Apr  6 16:37:59 EDT 1999
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      REAL vertex(3)
      REAL cdir(3)
      REAL dist
      REAL dist1 
c  Local pre-defined variables:
c  ---------------------------
      REAL long
      REAL rayon
      REAL wall
      REAL centre(3)
      REAL dir(3)

      DATA long /18.5/          !target length
      DATA rayon /3./           !target radius
      DATA wall /0.017/         !mylar wall thickness

      DATA centre /0., 0., 0./  !coordinates of the center of a target
      DATA dir /0., 0., 1./     !target axis direction


c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: lh2targ.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/04/17 15:30:33 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: lh2targ.F,v 1.1.1.1 2000/04/17 15:30:33 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='LH2TARG')
      PARAMETER (CRAUTH='Eugene Pasyuk')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ierr
      INTEGER ierr1 
      INTEGER extrm
      INTEGER i

      REAL pos1(3)
      REAL pos2(3)
      REAL pos1w(3)
      REAL pos2w(3)
      REAL point(3)
      REAL r
      REAL x

c_end_var
c
c  executable code for routine LH2TARG:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
c         CRMESS = 'This is a DUMMY routine, this message written once'
c         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c     
c------------------------------------------------------------------------------

      do i = 1,3
         point(i) = vertex(i) + 10.*cdir(i)
      enddo


      call IntsectionCylindre(vertex, point, centre, dir, rayon,
     $     pos1, pos2, ierr)

      if (ierr .eq. 0 .or. ierr .eq. 2 .or.
     $     ((vertex(1) - pos1(1))*(vertex(1) - pos2(1))
     +     +(vertex(2) - pos1(2))*(vertex(2) - pos2(2))) .gt. 0.) then
         dist = 0.
         return                 ! no intersection, get out
      endif

      if( (vertex(3) - pos1(3))*(vertex(3) - pos2(3)) .gt. 0.) then
         dist = 0.
         return                 !no intersection, get out
      endif

c---  cylinder or sphere?
      extrm = 0
      if (pos1(3) .gt. (long/2. - rayon)) extrm = 1
      if (-(long/2. - rayon) .gt. pos1(3)) extrm = -1

c---  sphere
      if (extrm .ne. 0) then 
         centre(3) = 1.*extrm*(long/2. - rayon)
         call IntsectionSphere(vertex, point, centre, rayon,
     $        pos1, pos2, ierr)
         if (ierr .eq. 0) then 
            dist = 0.
            return              !no intersection, get out
         endif
c---  spheric wall
         r = rayon + wall
         call IntsectionSphere(vertex, point, centre, r,
     $        pos1w, pos2w, ierr1)

         if (ierr .eq. 2) then
            pos1(1) = pos2(1)
            pos1(2) = pos2(2)
            pos1(3) = pos2(3)
            pos1w(1) = pos2w(1)
            pos1w(2) = pos2w(2)
            pos1w(3) = pos2w(3)
         endif
c---  cylinder wall
      else 
         r = rayon + wall
         call IntsectionCylindre(vertex, point, centre, dir, r,
     $        pos1w, pos2w, ierr)
      endif

      dist = 0.
      x = 0.

      do i = 1,3
         dist = dist + (pos1(i) - vertex(i))*(pos1(i) - vertex(i))
         x = x + (pos1w(i) - vertex(i))*(pos1w(i) - vertex(i))
      enddo

      dist = sqrt(dist)
      x = sqrt(x)
      dist1 = x - dist

      RETURN
      END


C--------------------------------------------------------------------

      INTEGER  FUNCTION  LOSS ( mate, thick, pmass, pout, pin ) 
c     
c     _begin_doc
c     RCS ID string
c     $Id: loss.F,v 1.5 2004/02/24 21:44:59 pasyuk Exp $
c     
c     Documentation for subroutine LOSS
c     
c     Purpose: 
c     -------
c     Calculate reversed energy losses for charged particles 
c     in different materials.
c     Uses polynomial interpolation from the momentum-range table.  
c     
c     Input Parameters:  (Name - Type - Meaning)
c     ----------------
c     mate  (int)  material index
c
c     mate = 1 LH2 (dencity 0.0708 g/cc)
c     mate = 2 plastic scintillator
c     mate = 3 carbon (density 1.7 g/cc)
c     mate = 4 LD2 (dencity 0.1623 g/cc)
c     mate = 5 L3He (dencity 0.072 g/cc)
c     mate = 6 Al
c     mate = 7 L4He (dencity 0.14 g/cc)
c     mate = 8 air (gas, dencity 0.0129*10^-3 g/cc)
c
c     thick (real) material thickness (cm)
c     pmass (real) particle mass (GeV)
c     pout(3) (real) particle momentum vector after degrader (GeV/c)
c     
c     Output Parameters:  (Name - Type - Meaning)
c     -----------------
c     
c     pin(3) (real) particle momentum vector before degrader (GeV/c) 
c     LOSS (integer) return status
c     LOSS = 1 for normal completion
c     LOSS = 0 error, unknown material
c     LOSS = -1 error, particle momentum out of range
c     
c     Other routines:
c     ---------------
c     DIVDIF - function interpolation (kernlib)
c     
c     Notes:
c     ------
c     
c     Author:   Eugene Pasyuk      Created:  Fri Mar 26 17:09:19 EST 1999
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c
c_begin_var
c  input/output variables:
c  -----------------------
c

      INTEGER mate
      REAL thick
      REAL pmass
      REAL pin(3) 
      REAL pout(3)
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER  j
      REAL PI
      REAL PO      
      REAL X
      REAL POVERM
      REAL DP
      REAL R
      REAL piom
      REAL divdif

c  Local pre-defined variables:
c  ---------------------------
      INTEGER npoint            ! number of points
      DATA npoint /190/
      INTEGER k                 ! degree of interpolation polynomial
      DATA k /3/

      REAL xom(190,8)            ! range/M
      REAL rlh2(190)
      REAL rscint(190)
      REAL rcarbon(190)
      REAL rld2(190)
      REAL rl3he(190)
      REAL ral(190)
      REAL rl4he(190)
      REAL rair(190)

      EQUIVALENCE (xom(1,1), rlh2(1)) 
      EQUIVALENCE (xom(1,2), rscint(1))
      EQUIVALENCE (xom(1,3), rcarbon(1))
      EQUIVALENCE (xom(1,4), rld2(1))
      EQUIVALENCE (xom(1,5), rl3he(1))
      EQUIVALENCE (xom(1,6), ral(1))
      EQUIVALENCE (xom(1,7), rl4he(1))
      EQUIVALENCE (xom(1,8), rair(1))

      DATA rlh2 /
     +      0.01626,      0.19810,      0.86906,      2.48583,  !LH2
     +      5.59986,     10.81970,     18.77489,     30.08313,  !LH2
     +     45.32279,     65.01154,     89.59175,    119.42226,  !LH2
     +    154.77570,    195.84036,    242.72554,    295.46909,  !LH2
     +    354.04630,    418.37939,    488.34689,    563.79236,  !LH2
     +    644.53259,    730.36487,    821.07288,    916.43225,  !LH2
     +   1016.21484,   1120.19214,   1228.13806,   1339.83142,  !LH2
     +   1455.05713,   1573.60779,   1695.28442,   1819.89661,  !LH2
     +   1947.26355,   2077.21313,   2209.58325,   2344.21997,  !LH2
     +   2480.97900,   2619.72388,   2760.32666,   2902.66724,  !LH2
     +   3046.63281,   3192.11768,   3339.02271,   3487.25513,  !LH2
     +   3636.72778,   3787.35913,   3939.07324,   4091.79858,  !LH2
     +   4245.46826,   4400.01953,   4555.39404,   4711.53662,  !LH2
     +   4868.39551,   5025.92285,   5184.07324,   5342.80420,  !LH2
     +   5502.07617,   5661.85205,   5822.09619,   5982.77588,  !LH2
     +   6143.86035,   6305.32031,   6467.12891,   6629.26025,  !LH2
     +   6791.69043,   6954.39648,   7117.35742,   7280.55322,  !LH2
     +   7443.96484,   7607.57422,   7771.36475,   7935.32031,  !LH2
     +   8099.42676,   8263.66992,   8428.03613,   8592.51270,  !LH2
     +   8757.08887,   8921.75293,   9086.49316,   9251.30176,  !LH2
     +   9416.16895,   9581.08398,   9746.04102,   9911.03125,  !LH2
     +  10076.04590,  10241.08008,  10406.12598,  10571.17676,  !LH2
     +  10736.22754,  10901.27246,  11066.30566,  11231.32324,  !LH2
     +  11396.31934,  11561.29004,  11726.23047,  11891.13672,  !LH2
     +  12056.00586,  12220.83398,  12385.61719,  12550.35254,  !LH2
     +  14194.48145,  15831.35742,  17459.71484,  19078.91211,  !LH2
     +  20688.68359,  22289.00195,  23879.98242,  25461.82227,  !LH2
     +  27034.77539,  28599.11914,  30155.14258,  31703.13867,  !LH2
     +  33243.39453,  34776.19531,  36301.80859,  37820.50000,  !LH2
     +  39332.51562,  40838.08984,  42337.45312,  43830.82031,  !LH2
     +  45318.39062,  46800.35938,  48276.91016,  49748.21094,  !LH2
     +  51214.43359,  52675.73438,  54132.25391,  55584.14453,  !LH2
     +  57031.53125,  58474.54688,  59913.31250,  61347.94141,  !LH2
     +  62778.55078,  64205.24219,  65628.11719,  67047.27344,  !LH2
     +  68462.79688,  69874.78906,  71283.32031,  72688.48438,  !LH2
     +  74090.35156,  75489.00781,  76884.50781,  78276.92969,  !LH2
     +  79666.33594,  81052.80469,  82436.37500,  83817.12500,  !LH2
     +  85195.10156,  86570.36719,  87942.97656,  89312.96875,  !LH2
     +  90680.39844,  92045.32031,  93407.77344,  94767.81250,  !LH2
     +  96125.46875,  97480.78906,  98833.81250, 100184.58594,  !LH2
     + 101533.14062, 102879.51562, 104223.74219, 105565.86719,  !LH2
     + 106905.91406, 108243.92188, 109579.92188, 110913.93750,  !LH2
     + 112246.00781, 113576.15625, 114904.42188, 116230.82031,  !LH2
     + 117555.38281, 118878.14062, 120199.10938, 121518.32812,  !LH2
     + 122835.80469, 124151.57812, 125465.66406, 126778.09375,  !LH2
     + 128088.87500, 129398.03125, 130705.60156, 132011.59375,  !LH2
     + 133316.01562, 134618.90625, 135920.28125, 137220.15625,  !LH2
     + 138518.54688, 139815.46875/                              !LH2

      DATA rscint/
     +      0.00277,      0.03113,      0.13251,      0.37281,  !scint
     +      0.83084,      1.59302,      2.74826,      4.38343,  !scint
     +      6.57950,      9.40861,     12.93209,     17.19936,  !scint
     +     22.24760,     28.10206,     34.77688,     42.27616,  !scint
     +     50.59524,     59.72206,     69.63850,     80.32157,  !scint
     +     91.74466,    103.87846,    116.69184,    130.15266,  !scint
     +    144.22830,    158.88615,    174.09407,    189.82062,  !scint
     +    206.03529,    222.70874,    239.81279,    257.32062,  !scint
     +    275.20663,    293.44672,    312.01794,    330.89874,  !scint
     +    350.06885,    369.50919,    389.20175,    409.12982,  !scint
     +    429.27759,    449.63031,    470.17416,    490.89621,  !scint
     +    511.78436,    532.82733,    554.01447,    575.33588,  !scint
     +    596.78241,    618.34521,    640.01636,    661.78815,  !scint
     +    683.65350,    705.60577,    727.63879,    749.74670,  !scint
     +    771.92401,    794.16565,    816.46674,    838.82288,  !scint
     +    861.22980,    883.68359,    906.18048,    928.71704,  !scint
     +    951.28992,    973.89612,    996.53271,   1019.19702,  !scint
     +   1041.88647,   1064.59863,   1087.33118,   1110.08228,  !scint
     +   1132.84961,   1155.63135,   1178.42590,   1201.23157,  !scint
     +   1224.04651,   1246.86963,   1269.69922,   1292.53418,  !scint
     +   1315.37305,   1338.21497,   1361.05859,   1383.90295,  !scint
     +   1406.74707,   1429.58997,   1452.43091,   1475.26904,  !scint
     +   1498.10352,   1520.93359,   1543.75867,   1566.57812,  !scint
     +   1589.39124,   1612.19751,   1634.99634,   1657.78723,  !scint
     +   1680.56982,   1703.34351,   1726.10791,   1748.86255,  !scint
     +   1975.80359,   2201.47534,   2425.73364,   2648.51562,  !scint
     +   2869.80640,   3089.62085,   3307.99048,   3524.95679,  !scint
     +   3740.56592,   3954.86719,   4167.90967,   4379.74121,  !scint
     +   4590.40967,   4799.95996,   5008.43506,   5215.87695,  !scint
     +   5422.32373,   5627.81299,   5832.37939,   6036.05664,  !scint
     +   6238.87500,   6440.86523,   6642.05420,   6842.46924,  !scint
     +   7042.13477,   7241.07520,   7439.31201,   7636.86768,  !scint
     +   7833.76221,   8030.01465,   8225.64355,   8420.66602,  !scint
     +   8615.09863,   8808.95801,   9002.25879,   9195.01562,  !scint
     +   9387.24121,   9578.95117,   9770.15527,   9960.86816,  !scint
     +  10151.10059,  10340.86230,  10530.16602,  10719.02051,  !scint
     +  10907.43750,  11095.42383,  11282.99121,  11470.14648,  !scint
     +  11656.89941,  11843.25781,  12029.22949,  12214.82129,  !scint
     +  12400.04199,  12584.89844,  12769.39551,  12953.54297,  !scint
     +  13137.34473,  13320.80762,  13503.93848,  13686.74219,  !scint
     +  13869.22461,  14051.39160,  14233.24805,  14414.79980,  !scint
     +  14596.05078,  14777.00586,  14957.67090,  15138.04980,  !scint
     +  15318.14746,  15497.96680,  15677.51367,  15856.79102,  !scint
     +  16035.80371,  16214.55469,  16393.04883,  16571.28906,  !scint
     +  16749.27930,  16927.02148,  17104.52148,  17281.78125,  !scint
     +  17458.80469,  17635.59570,  17812.15430,  17988.48633,  !scint
     +  18164.59375,  18340.48047,  18516.14648,  18691.59766,  !scint
     +  18866.83594,  19041.86328/                              !scint 

      DATA rcarbon /
     +      0.00195,      0.02144,      0.09061,      0.25396,  !carbon
     +      0.56460,      1.08069,      1.86201,      2.96689,  !carbon
     +      4.44967,      6.35871,      8.73509,     11.61184,  !carbon
     +     15.01378,     18.95769,     23.45291,     28.50200,  !carbon
     +     34.10168,     40.24369,     46.91570,     54.10216,  !carbon
     +     61.78504,     69.94456,     78.55972,     87.60883,  !carbon
     +     97.06992,    106.92102,    117.14048,    127.70715,  !carbon
     +    138.60049,    149.80078,    161.28906,    173.04729,  !carbon
     +    185.05832,    197.30586,    209.77458,    222.45000,  !carbon
     +    235.31847,    248.36717,    261.58411,    274.95795,  !carbon
     +    288.47818,    302.13489,    315.91879,    329.82123,  !carbon
     +    343.83411,    357.94983,    372.16132,    386.46191,  !carbon
     +    400.84543,    415.30609,    429.83844,    444.43741,  !carbon
     +    459.09824,    473.81650,    488.58804,    503.40897,  !carbon
     +    518.27557,    533.18457,    548.13257,    563.11676,  !carbon
     +    578.13416,    593.18225,    608.25848,    623.36060,  !carbon
     +    638.48633,    653.63373,    668.80078,    683.98572,  !carbon
     +    699.18683,    714.40253,    729.63129,    744.87170,  !carbon
     +    760.12250,    775.38232,    790.65009,    805.92468,  !carbon
     +    821.20502,    836.49011,    851.77911,    867.07104,  !carbon
     +    882.36511,    897.66064,    912.95679,    928.25293,  !carbon
     +    943.54834,    958.84253,    974.13483,    989.42474,  !carbon
     +   1004.71173,   1019.99536,   1035.27515,   1050.55066,  !carbon
     +   1065.82153,   1081.08728,   1096.34766,   1111.60229,  !carbon
     +   1126.85083,   1142.09302,   1157.32861,   1172.55737,  !carbon
     +   1324.41663,   1475.39026,   1625.38574,   1774.36475,  !carbon
     +   1922.32031,   2069.26465,   2215.22144,   2360.22021,  !carbon
     +   2504.29346,   2647.47534,   2789.80005,   2931.30127,  !carbon
     +   3072.01099,   3211.96094,   3351.18042,   3489.69800,  !carbon
     +   3627.54028,   3764.73267,   3901.29883,   4037.26172,  !carbon
     +   4172.64258,   4307.46143,   4441.73730,   4575.48877,  !carbon
     +   4708.73242,   4841.48486,   4973.76123,   5105.57568,  !carbon
     +   5236.94287,   5367.87500,   5498.38477,   5628.48486,  !carbon
     +   5758.18555,   5887.49805,   6016.43311,   6144.99951,  !carbon
     +   6273.20752,   6401.06592,   6528.58301,   6655.76709,  !carbon
     +   6782.62598,   6909.16797,   7035.39941,   7161.32764,  !carbon
     +   7286.95947,   7412.30127,   7537.35889,   7662.13867,  !carbon
     +   7786.64648,   7910.88770,   8034.86719,   8158.59082,  !carbon
     +   8282.06348,   8405.28906,   8528.27344,   8651.02051,  !carbon
     +   8773.53516,   8895.82031,   9017.88086,   9139.72168,  !carbon
     +   9261.34473,   9382.75488,   9503.95508,   9624.95020,  !carbon
     +   9745.74219,   9866.33398,   9986.73047,  10106.93359,  !carbon
     +  10226.94629,  10346.77246,  10466.41309,  10585.87305,  !carbon
     +  10705.15430,  10824.25879,  10943.18945,  11061.94922,  !carbon
     +  11180.54004,  11298.96484,  11417.22461,  11535.32324,  !carbon
     +  11653.26270,  11771.04395,  11888.66895,  12006.14160,  !carbon
     +  12123.46289,  12240.63477,  12357.65820,  12474.53613,  !carbon
     +  12591.27051,  12707.86328/                              !carbon

      DATA rld2/
     +      0.01419,      0.17283,      0.75821,      2.16879,  !LD2
     +      4.88565,      9.43974,     16.38031,     26.24628,  !LD2
     +     39.54225,     56.71986,     78.16508,    104.19096,  !LD2
     +    135.03535,    170.86256,    211.76793,    257.78448,  !LD2
     +    308.89066,    365.01862,    426.06235,    491.88538,  !LD2
     +    562.32788,    637.21295,    716.35193,    799.54907,  !LD2
     +    886.60522,    977.32104,   1071.49939,   1168.94714,  !LD2
     +   1269.47681,   1372.90735,   1479.06519,   1587.78418,  !LD2
     +   1698.90649,   1812.28223,   1927.76953,   2045.23450,  !LD2
     +   2164.55103,   2285.60010,   2408.27026,   2532.45654,  !LD2
     +   2658.06055,   2784.98999,   2913.15845,   3042.48486,  !LD2
     +   3172.89355,   3304.31348,   3436.67773,   3569.92407,  !LD2
     +   3703.99463,   3838.83423,   3974.39185,   4110.61963,  !LD2
     +   4247.47266,   4384.90869,   4522.88818,   4661.37451,  !LD2
     +   4800.33301,   4939.73047,   5079.53662,   5219.72314,  !LD2
     +   5360.26221,   5501.12988,   5642.30078,   5783.75391,  !LD2
     +   5925.46729,   6067.42188,   6209.59863,   6351.97998,  !LD2
     +   6494.54932,   6637.29199,   6780.19238,   6923.23730,  !LD2
     +   7066.41309,   7209.70801,   7353.11084,   7496.61035,  !LD2
     +   7640.19580,   7783.85791,   7927.58789,   8071.37646,  !LD2
     +   8215.21582,   8359.09766,   8503.01562,   8646.96191,  !LD2
     +   8790.93164,   8934.91602,   9078.91211,   9222.91211,  !LD2
     +   9366.91211,   9510.90723,   9654.89160,   9798.86230,  !LD2
     +   9942.81445,  10086.74414,  10230.64844,  10374.52246,  !LD2
     +  10518.36426,  10662.16895,  10805.93555,  10949.66016,  !LD2
     +  12384.09473,  13812.20117,  15232.87500,  16645.55664,  !LD2
     +  18050.01562,  19446.22852,  20834.29102,  22214.38086,  !LD2
     +  23586.71875,  24951.54297,  26309.10742,  27659.66992,  !LD2
     +  29003.47852,  30340.78320,  31671.81836,  32996.81250,  !LD2
     +  34315.98438,  35629.53516,  36937.66797,  38240.56641,  !LD2
     +  39538.41016,  40831.36719,  42119.59375,  43403.24609,  !LD2
     +  44682.46484,  45957.38672,  47228.14062,  48494.85156,  !LD2
     +  49757.63672,  51016.60938,  52271.87109,  53523.52734,  !LD2
     +  54771.67578,  56016.40234,  57257.80078,  58495.95703,  !LD2
     +  59730.94531,  60962.84766,  62191.73438,  63417.68359,  !LD2
     +  64640.75391,  65861.01562,  67078.53125,  68293.36719,  !LD2
     +  69505.57031,  70715.19531,  71922.31250,  73126.95312,  !LD2
     +  74329.18750,  75529.04688,  76726.58594,  77921.85156,  !LD2
     +  79114.87500,  80305.71094,  81494.39844,  82680.96875,  !LD2
     +  83865.46875,  85047.92969,  86228.39062,  87406.88281,  !LD2
     +  88583.44531,  89758.10156,  90930.88281,  92101.82812,  !LD2
     +  93270.96875,  94438.32031,  95603.92188,  96767.79688,  !LD2
     +  97929.97656,  99090.47656, 100249.32812, 101406.55469,  !LD2
     + 102562.17969, 103716.23438, 104868.72656, 106019.68750,  !LD2
     + 107169.13281, 108317.09375, 109463.57812, 110608.60938,  !LD2
     + 111752.21094, 112894.40625, 114035.20312, 115174.61719,  !LD2
     + 116312.67969, 117449.39844, 118584.78906, 119718.87500,  !LD2
     + 120851.67188, 121983.18750/                              !LD2

      DATA rl3he/
     +      0.02915,      0.33741,      1.45183,      4.10851,  !L3He
     +      9.19061,     17.66856,     30.54270,     48.79166,  !L3He
     +     73.32904,    104.97010,    144.40912,    192.20676,  !L3He
     +    248.78625,    314.43680,    389.32220,    473.49332,  !L3He
     +    566.90216,    669.41699,    780.83752,    900.90863,  !L3He
     +   1029.33362,   1165.78528,   1309.91565,   1461.36475,  !L3He
     +   1619.76709,   1784.75708,   1955.97400,   2133.06494,  !L3He
     +   2315.68701,   2503.51050,   2696.21851,   2893.50879,  !L3He
     +   3095.09448,   3300.70264,   3510.07568,   3722.97095,  !L3He
     +   3939.15918,   4158.42529,   4380.56689,   4605.39404,  !L3He
     +   4832.72949,   5062.40625,   5294.26807,   5528.16895,  !L3He
     +   5763.97217,   6001.54932,   6240.78076,   6481.55469,  !L3He
     +   6723.76562,   6967.31592,   7212.11377,   7458.07324,  !L3He
     +   7705.11328,   7953.15918,   8202.13965,   8451.98828,  !L3He
     +   8702.64453,   8954.04883,   9206.14648,   9458.88770,  !L3He
     +   9712.22363,   9966.11035,  10220.50293,  10475.36426,  !L3He
     +  10730.65625,  10986.34375,  11242.39355,  11498.77441,  !L3He
     +  11755.45801,  12012.41699,  12269.62500,  12527.05762,  !L3He
     +  12784.69238,  13042.50684,  13300.48242,  13558.59863,  !L3He
     +  13816.83789,  14075.18359,  14333.61914,  14592.12988,  !L3He
     +  14850.70117,  15109.31934,  15367.97266,  15626.64844,  !L3He
     +  15885.33594,  16144.02441,  16402.70312,  16661.36328,  !L3He
     +  16919.99609,  17178.59180,  17437.14453,  17695.64453,  !L3He
     +  17954.08594,  18212.46289,  18470.76758,  18728.99414,  !L3He
     +  18987.13867,  19245.19336,  19503.15625,  19761.01953,  !L3He
     +  22333.38477,  24892.36328,  27436.20312,  29964.09570,  !L3He
     +  32475.79297,  34971.39062,  37451.18750,  39915.60156,  !L3He
     +  42365.11719,  44800.24609,  47221.51172,  49629.43359,  !L3He
     +  52024.51953,  54407.25781,  56778.12109,  59137.55078,  !L3He
     +  61485.97266,  63823.79688,  66151.39844,  68469.14062,  !L3He
     +  70777.36719,  73076.40625,  75366.56250,  77648.12500,  !L3He
     +  79921.37500,  82186.56250,  84443.93750,  86693.75000,  !L3He
     +  88936.20312,  91171.53125,  93399.91406,  95621.56250,  !L3He
     +  97836.64844, 100045.35156, 102247.84375, 104444.28125,  !L3He
     + 106634.81250, 108819.58594, 110998.74219, 113172.41406,  !L3He
     + 115340.73438, 117503.82031, 119661.79688, 121814.77344,  !L3He
     + 123962.86719, 126106.17188, 128244.78906, 130378.82812,  !L3He
     + 132508.37500, 134633.51562, 136754.34375, 138870.93750,  !L3He
     + 140983.39062, 143091.78125, 145196.17188, 147296.64062,  !L3He
     + 149393.26562, 151486.09375, 153575.23438, 155660.71875,  !L3He
     + 157742.60938, 159820.98438, 161895.89062, 163967.37500,  !L3He
     + 166035.51562, 168100.35938, 170161.95312, 172220.35938,  !L3He
     + 174275.60938, 176327.76562, 178376.85938, 180422.96875,  !L3He
     + 182466.09375, 184506.31250, 186543.64062, 188578.14062,  !L3He
     + 190609.85938, 192638.79688, 194665.03125, 196688.57812,  !L3He
     + 198709.46875, 200727.76562, 202743.46875, 204756.64062,  !L3He
     + 206767.29688, 208775.46875, 210781.20312, 212784.51562,  !L3He
     + 214785.45312, 216784.03125/

      DATA ral/
     +      0.00124,      0.01398,      0.05955,      0.16763,  !Al
     +      0.37370,      0.71670,      1.23666,      1.97274,  !Al
     +      2.96141,      4.23518,      5.82171,      7.74326,  !Al
     +     10.01660,     12.65314,     15.65925,     19.03679,  !Al
     +     22.78370,     26.89454,     31.36115,     36.17323,  !Al
     +     41.31876,     46.78455,     52.55661,     58.62045,  !Al
     +     64.96138,     71.56472,     78.41599,     85.50103,  !Al
     +     92.80611,    100.31799,    108.02400,    115.91203,  !Al
     +    123.97060,    132.18878,    140.55627,    149.06339,  !Al
     +    157.70096,    166.46037,    175.33357,    184.31299,  !Al
     +    193.39149,    202.56245,    211.81964,    221.15723,  !Al
     +    230.56975,    240.05212,    249.59958,    259.20764,  !Al
     +    268.87213,    278.58914,    288.35504,    298.16641,  !Al
     +    308.02002,    317.91287,    327.84219,    337.80536,  !Al
     +    347.79987,    357.82343,    367.87387,    377.94922,  !Al
     +    388.04752,    398.16699,    408.30597,    418.46289,  !Al
     +    428.63626,    438.82471,    449.02692,    459.24170,  !Al
     +    469.46783,    479.70432,    489.95007,    500.20416,  !Al
     +    510.46567,    520.73376,    531.00769,    541.28662,  !Al
     +    551.56982,    561.85675,    572.14667,    582.43903,  !Al
     +    592.73328,    603.02887,    613.32532,    623.62219,  !Al
     +    633.91895,    644.21527,    654.51068,    664.80487,  !Al
     +    675.09753,    685.38824,    695.67670,    705.96271,  !Al
     +    716.24585,    726.52600,    736.80286,    747.07611,  !Al
     +    757.34570,    767.61127,    777.87274,    788.12988,  !Al
     +    890.43024,    992.16199,   1093.25977,   1193.69458,  !Al
     +   1293.45996,   1392.56201,   1491.01477,   1588.83679,  !Al
     +   1686.04883,   1782.67273,   1878.73047,   1974.24402,  !Al
     +   2069.23438,   2163.72168,   2257.72559,   2351.26465,  !Al
     +   2444.35620,   2537.01685,   2629.26245,   2721.10791,  !Al
     +   2812.56714,   2903.65356,   2994.37964,   3084.75732,  !Al
     +   3174.79810,   3264.51221,   3353.90991,   3443.00122,  !Al
     +   3531.79468,   3620.29932,   3708.52344,   3796.47461,  !Al
     +   3884.16040,   3971.58789,   4058.76440,   4145.69580,  !Al
     +   4232.38867,   4318.84863,   4405.08154,   4491.09277,  !Al
     +   4576.88818,   4662.47168,   4747.84912,   4833.02441,  !Al
     +   4918.00195,   5002.78662,   5087.38232,   5171.79248,  !Al
     +   5256.02197,   5340.07324,   5423.95068,   5507.65723,  !Al
     +   5591.19678,   5674.57178,   5757.78613,   5840.84180,  !Al
     +   5923.74268,   6006.49072,   6089.08936,   6171.54053,  !Al
     +   6253.84766,   6336.01221,   6418.03711,   6499.92480,  !Al
     +   6581.67725,   6663.29639,   6744.78516,   6826.14453,  !Al
     +   6907.37744,   6988.48535,   7069.47021,   7150.33447,  !Al
     +   7231.07861,   7311.70557,   7392.21680,   7472.61377,  !Al
     +   7552.89746,   7633.07080,   7713.13428,   7793.08984,  !Al
     +   7872.93896,   7952.68262,   8032.32275,   8111.86035,  !Al
     +   8191.29688,   8270.63379,   8349.87207,   8429.01367,  !Al
     +   8508.05859,   8587.00879/

      DATA rl4he/
     +      0.01999,      0.23137,      0.99554,      2.81726,  !L4He
     +      6.30214,     12.11558,     20.94357,     33.45714,  !L4He
     +     50.28277,     71.97950,     99.02340,    131.79892,  !L4He
     +    170.59628,    215.61380,    266.96381,    324.68115,  !L4He
     +    388.73288,    459.02878,    535.43140,    617.76593,  !L4He
     +    705.82880,    799.39557,    898.22784,   1002.07867,  !L4He
     +   1110.69739,   1223.83350,   1341.23938,   1462.67310,  !L4He
     +   1587.89966,   1716.69287,   1848.83545,   1984.12036,  !L4He
     +   2122.35034,   2263.33887,   2406.90894,   2552.89429,  !L4He
     +   2701.13770,   2851.49146,   3003.81714,   3157.98462,  !L4He
     +   3313.87183,   3471.36426,   3630.35522,   3790.74438,  !L4He
     +   3952.43799,   4115.34814,   4279.39258,   4444.49463,  !L4He
     +   4610.58203,   4777.58838,   4945.44971,   5114.10742,  !L4He
     +   5283.50635,   5453.59473,   5624.32422,   5795.64941,  !L4He
     +   5967.52734,   6139.91895,   6312.78662,   6486.09473,  !L4He
     +   6659.81055,   6833.90381,   7008.34521,   7183.10693,  !L4He
     +   7358.16455,   7533.49268,   7709.06982,   7884.87402,  !L4He
     +   8060.88574,   8237.08594,   8413.45703,   8589.98242,  !L4He
     +   8766.64648,   8943.43359,   9120.33105,   9297.32520,  !L4He
     +   9474.40332,   9651.55469,   9828.76758,  10006.03223,  !L4He
     +  10183.33789,  10360.67676,  10538.03809,  10715.41602,  !L4He
     +  10892.80176,  11070.18750,  11247.56738,  11424.93457,  !L4He
     +  11602.28223,  11779.60547,  11956.89844,  12134.15625,  !L4He
     +  12311.37305,  12488.54590,  12665.66895,  12842.73926,  !L4He
     +  13019.75195,  13196.70508,  13373.59277,  13550.41309,  !L4He
     +  15314.32129,  17069.04883,  18813.39648,  20546.80859,  !L4He
     +  22269.11523,  23980.38281,  25680.81445,  27370.69922,  !L4He
     +  29050.36523,  30720.16797,  32380.46484,  34031.61328,  !L4He
     +  35673.95703,  37307.83594,  38933.56641,  40551.46094,  !L4He
     +  42161.80859,  43764.88672,  45360.95703,  46950.26953,  !L4He
     +  48533.05469,  50109.53906,  51679.92969,  53244.42969,  !L4He
     +  54803.22656,  56356.50000,  57904.41797,  59447.14062,  !L4He
     +  60984.82812,  62517.62109,  64045.65625,  65569.07031,  !L4He
     +  67087.99219,  68602.53125,  70112.80469,  71618.93750,  !L4He
     +  73121.01562,  74619.14062,  76113.42188,  77603.93750,  !L4He
     +  79090.78906,  80574.04688,  82053.80469,  83530.13281,  !L4He
     +  85003.10938,  86472.80469,  87939.28125,  89402.62500,  !L4He
     +  90862.88281,  92320.12500,  93774.40625,  95225.78906,  !L4He
     +  96674.32812,  98120.07812,  99563.08594, 101003.40625,  !L4He
     + 102441.09375, 103876.18750, 105308.72656, 106738.77344,  !L4He
     + 108166.35938, 109591.53125, 111014.32031, 112434.77344,  !L4He
     + 113852.92969, 115268.82031, 116682.48438, 118093.96094,  !L4He
     + 119503.27344, 120910.46875, 122315.56250, 123718.60156,  !L4He
     + 125119.60938, 126518.60938, 127915.64062, 129310.72656,  !L4He
     + 130703.89844, 132095.17188, 133484.59375, 134872.15625,  !L4He
     + 136257.92188, 137641.89062, 139024.09375, 140404.54688,  !L4He
     + 141783.28125, 143160.32812, 144535.68750, 145909.39062,  !L4He
     + 147281.45312, 148651.90625/

      DATA rair/
     +      0.78103,      8.37273,     35.08553,     97.90381,  !Air
     +    217.05725,    414.66125,    713.42236,   1135.47620,  !Air
     +   1701.41797,   2429.56177,   3335.43994,   4431.52881,  !Air
     +   5727.17480,   7228.68018,   8939.50781,  10860.56543,  !Air
     +  12990.53516,  15326.21582,  17862.86914,  20594.53711,  !Air
     +  23514.32812,  26614.68945,  29887.61328,  33324.83203,  !Air
     +  36917.97266,  40658.67578,  44538.71094,  48550.01953,  !Air
     +  52684.81250,  56935.57031,  61295.11328,  65756.57031,  !Air
     +  70313.43750,  74959.53906,  79689.04688,  84496.46094,  !Air
     +  89376.61719,  94324.66406,  99336.02344, 104406.42969,  !Air
     + 109531.88281, 114708.63281, 119933.16406, 125202.19531,  !Air
     + 130512.67188, 135861.70312, 141246.62500, 146664.90625,  !Air
     + 152114.23438, 157592.39062, 163097.32812, 168627.14062,  !Air
     + 174180.03125, 179754.31250, 185348.40625, 190960.87500,  !Air
     + 196590.31250, 202235.43750, 207895.04688, 213568.01562,  !Air
     + 219253.26562, 224949.79688, 230656.70312, 236373.07812,  !Air
     + 242098.12500, 247831.07812, 253571.17188, 259317.78125,  !Air
     + 265070.21875, 270827.90625, 276590.28125, 282356.81250,  !Air
     + 288127.00000, 293900.37500, 299676.50000, 305454.93750,  !Air
     + 311235.34375, 317017.28125, 322800.46875, 328584.56250,  !Air
     + 334369.21875, 340154.18750, 345939.21875, 351723.96875,  !Air
     + 357508.28125, 363291.90625, 369074.62500, 374856.21875,  !Air
     + 380636.53125, 386415.34375, 392192.50000, 397967.87500,  !Air
     + 403741.28125, 409512.59375, 415281.68750, 421048.40625,  !Air
     + 426812.65625, 432574.34375, 438333.34375, 444089.56250,  !Air
     + 501481.15625, 558523.00000, 615182.06250, 671445.06250,  !Air
     + 727310.87500, 782785.06250, 837877.43750, 892599.87500,  !Air
     + 946965.50000,1000987.68750,1054679.87500,1108055.25000,  !Air
     +1161126.50000,1213905.62500,1266404.25000,1318633.37500,  !Air
     +1370603.37500,1422324.00000,1473804.50000,1525053.62500,  !Air
     +1576079.62500,1626890.37500,1677493.25000,1727895.00000,  !Air
     +1778102.37500,1828121.62500,1877958.50000,1927618.87500,  !Air
     +1977107.75000,2026430.37500,2075591.50000,2124595.75000,  !Air
     +2173447.25000,2222150.50000,2270709.00000,2319127.00000,  !Air
     +2367407.75000,2415554.75000,2463571.50000,2511461.00000,  !Air
     +2559226.50000,2606870.50000,2654396.00000,2701805.75000,  !Air
     +2749102.00000,2796287.75000,2843365.00000,2890336.00000,  !Air
     +2937203.00000,2983968.25000,3030634.00000,3077201.75000,  !Air
     +3123673.50000,3170051.50000,3216337.00000,3262532.00000,  !Air
     +3308638.25000,3354657.00000,3400590.25000,3446439.00000,  !Air
     +3492205.25000,3537890.25000,3583495.25000,3629021.50000,  !Air
     +3674470.50000,3719843.25000,3765141.50000,3810365.75000,  !Air
     +3855517.75000,3900598.25000,3945608.50000,3990549.50000,  !Air
     +4035422.25000,4080227.75000,4124967.00000,4169641.00000,  !Air
     +4214250.50000,4258797.00000,4303280.50000,4347702.50000,  !Air
     +4392063.50000,4436364.50000,4480606.50000,4524790.00000,  !Air
     +4568915.50000,4612984.00000,4656996.50000,4700953.50000,  !Air
     +4744855.50000,4788703.50000/

       REAL pom(190)              ! p/M
       DATA pom/
     +       0.05,  0.10,  0.15,  0.20,  0.25,  0.30,  0.35,  0.40,
     +       0.45,  0.50,  0.55,  0.60,  0.65,  0.70,  0.75,  0.80,
     +       0.85,  0.90,  0.95,  1.00,  1.05,  1.10,  1.15,  1.20,
     +       1.25,  1.30,  1.35,  1.40,  1.45,  1.50,  1.55,  1.60,
     +       1.65,  1.70,  1.75,  1.80,  1.85,  1.90,  1.95,  2.00,
     +       2.05,  2.10,  2.15,  2.20,  2.25,  2.30,  2.35,  2.40,
     +       2.45,  2.50,  2.55,  2.60,  2.65,  2.70,  2.75,  2.80,
     +       2.85,  2.90,  2.95,  3.00,  3.05,  3.10,  3.15,  3.20,
     +       3.25,  3.30,  3.35,  3.40,  3.45,  3.50,  3.55,  3.60,
     +       3.65,  3.70,  3.75,  3.80,  3.85,  3.90,  3.95,  4.00,
     +       4.05,  4.10,  4.15,  4.20,  4.25,  4.30,  4.35,  4.40,
     +       4.45,  4.50,  4.55,  4.60,  4.65,  4.70,  4.75,  4.80,
     +       4.85,  4.90,  4.95,  5.00,  5.50,  6.00,  6.50,  7.00,
     +       7.50,  8.00,  8.50,  9.00,  9.50, 10.00, 10.50, 11.00,
     +      11.50, 12.00, 12.50, 13.00, 13.50, 14.00, 14.50, 15.00,
     +      15.50, 16.00, 16.50, 17.00, 17.50, 18.00, 18.50, 19.00,
     +      19.50, 20.00, 20.50, 21.00, 21.50, 22.00, 22.50, 23.00,
     +      23.50, 24.00, 24.50, 25.00, 25.50, 26.00, 26.50, 27.00,
     +      27.50, 28.00, 28.50, 29.00, 29.50, 30.00, 30.50, 31.00,
     +      31.50, 32.00, 32.50, 33.00, 33.50, 34.00, 34.50, 35.00,
     +      35.50, 36.00, 36.50, 37.00, 37.50, 38.00, 38.50, 39.00,
     +      39.50, 40.00, 40.50, 41.00, 41.50, 42.00, 42.50, 43.00,
     +      43.50, 44.00, 44.50, 45.00, 45.50, 46.00, 46.50, 47.00,
     +      47.50, 48.00, 48.50, 49.00, 49.50, 50.00/

  
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: loss.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/24 21:44:59 $')
      PARAMETER (CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/ '$Id: loss.F,v 1.5 2004/02/24 21:44:59 pasyuk Exp $' /   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='LOSS')
      PARAMETER (CRAUTH='Eugene Pasyuk')


c_end_var
c
c  executable code for routine LOSS:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
c        CRMESS='This is a LOSS routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
C---------------------------------------------------------------------

      if ( thick .le. 0. ) then
         LOSS = 1
         PIN(1) = POUT(1)
         PIN(2) = POUT(2)
         PIN(3) = POUT(3)
         RETURN                 !no absorber, no losses
      endif


      if ( mate .lt. 1 .or. mate .gt. 8 ) then
         LOSS = 0
         PIN(1) = POUT(1)
         PIN(2) = POUT(2)
         PIN(3) = POUT(3)
         RETURN                 !unknown material, no losses
      endif


      PO = SQRT(POUT(1)**2 + POUT(2)**2 + POUT(3)**2)
      poverm = po/pmass

      if ( poverm .lt. 0.05 .or. poverm .gt. 50.) then
         LOSS = -1
         PIN(1) = POUT(1)
         PIN(2) = POUT(2)
         PIN(3) = POUT(3)
         RETURN                 !momentum is out of range, no losses
      endif

      LOSS = 1
      r = divdif(xom(1,mate), pom,  npoint, poverm, k)
      x = r + thick/pmass
      piom = divdif(pom, xom(1,mate), npoint, x, k)

      pi= piom * pmass

      DP = PI/PO

      PIN(1) = POUT(1)*DP
      PIN(2) = POUT(2)*DP
      PIN(3) = POUT(3)*DP

      RETURN
      END
c
c------------------------------------------------------------------------------



      SUBROUTINE STCOUNTER(icell, vertex, cdir, dist, xpoint)
c
c_begin_doc
c  RCS ID string
c  $Id: stcounter.F,v 1.6 2004/02/24 21:44:59 pasyuk Exp $
c
c  Documentation for subroutine STCOUNTER
c
c  Purpose: Calculate track length within the start counter scintillator
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  vertex(3) (real) vertex coordinates
c  cdir(3)   (real) track direction cosines  
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  dist (real) - track length within the scintillator
c  xpoint(3) - intercept of a track with start counter
c
c  Other routines:
c  ---------------
c  IntesectionPlan 
c
c  Notes:
c  ------
c
c  Author:   Eugene Pasyuk      Created:  Tue Apr  6 16:38:45 EDT 1999
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE 
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER icell
      REAL vertex(3)
      REAL cdir(3)
      REAL dist
      REAL xpoint(3)
c
c  Local pre-defined variables:
c  ---------------------------

      REAL d_leg                !distance from beam axis to the 
                                !face of scintillator
      REAL thickness            !scintillator thickness
      REAL half_length          !half length of the leg
      REAL pi

      DATA d_leg/10.2/
      DATA thickness/0.32/      
c changed thickness from 0.3 to 0.32 per Gordon's estimate of wrapping maerial
      DATA half_length/20.0/
      DATA pi/3.14159265359/
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: stcounter.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.6 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/24 21:44:59 $')
      PARAMETER (CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/   
     1'$Id: stcounter.F,v 1.6 2004/02/24 21:44:59 pasyuk Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='STCOUNTER')
      PARAMETER (CRAUTH='Eugene Pasyuk')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer i
      integer ierr
      REAL alpha
      REAL w
      REAL phi 
      REAL point(3)
      REAL np(3)
      REAL start(3)
      REAL theta
      REAL dir(3)
      REAL aupoint(3)
      REAL norme
      REAL csinus

      DATA NWRITE/0/

	real target_offset(3), st_offset
	common/eloss_geom/ target_offset, st_offset


c#ifdef Linux
c for Linux these functions are not implemeted in standard library
c
c      REAL sind, cosd, angle
c      sind(angle) = sin( angle*pi/180. )
c      cosd(angle) = cos( angle*pi/180. )
c
c#endif

c#include "eloss_geom.PAR"


c_end_var
c
c  executable code for routine STCOUNTER:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
c         write(CRMESS,*) 
c     + 'STCOUNTER: ST offset is: ', st_offset
c        CRMESS='This is a STCOUNTER routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c
c-------------Intersection avec le Start Counter
c

      if(icell.eq.6)then
        d_leg = 8.092*2.54/2                               ! g6->/10.2/
        half_length=(19.756 + 2./tan(67.5*pi/180.))*2.54/2.! g6->/20.0/
        thickness = 0.215
      endif


      alpha = atan2(cdir(2),cdir(1))
      alpha = 3.*alpha/pi
      w = nint(alpha)
      phi = w*60.

cc      print *,'  LOSS: STCOUNTER-----------------------------'

      np(1) = cos(phi*pi/180.)
      np(2) = sin(phi*pi/180.)
      np(3) = 0.

      do i = 1,3
         point(i) = vertex(i) + 500.*cdir(i)
         start(i) = d_leg*np(i)
      enddo

cc      print *,'  LOSS: point =',point
cc      print *,'  LOSS: start =',start
cc      print *,'  LOSS:    np =',np

      call IntsectionPlan(vertex, point, start, np, aupoint, ierr)

cc      print *,'  LOSS: aupoint =',aupoint
 
      norme = 0.
      do i = 1,3
         xpoint(i) = aupoint(i)
         norme = norme+(aupoint(i)-vertex(i))*(aupoint(i)-vertex(i))
      enddo
      norme = sqrt(norme)
 
c      theta = 90.
      
      if ( aupoint(3) .gt. (half_length + st_offset) ) then !leg or nose?
         theta = 60.
         if(icell.eq.6) theta = 65.
cc         print *,'  LOSS: NOSE'
      else
         theta = 90.
cc         print *,'  LOSS: LEG '
      endif

      dir(1) = sin(theta*pi/180.)*cos(phi*pi/180.)
      dir(2) = sin(theta*pi/180.)*sin(phi*pi/180.)
      dir(3) = cos(theta*pi/180.)
     
      csinus = 0.
      do i = 1,3
         csinus = csinus+(aupoint(i)-vertex(i))*dir(i)
      enddo

      csinus = csinus/norme
      dist = thickness/csinus

      if ( aupoint(3) .gt. (half_length + st_offset) ) then
c     -- nose --
         call IntsectionPlan(vertex, point, start, dir, aupoint, ierr)
         do i = 1,3
            xpoint(i) = aupoint(i)
         enddo
      endif

      RETURN
      END
c
c------------------------------------------------------------------------------




      SUBROUTINE TARGCELL( icell, vertex, cdir, dist, dist1 )
c
c_begin_doc
c  RCS ID string
c  $Id: targcell.F,v 1.8 2002/12/07 19:04:04 pasyuk Exp $
c
c  Documentation for subroutine TARGCELL
c
c  Purpose: calculates track length within LH2 target and its mylar walls.
c  --------
c           g1 run target geometry
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  icell     (integer) target cell type: 
c
c		       icell = 0 - no target
c                      icell = 1 - G1A/G1B/G6A/G6b running period 
c                      icell = 2 - G2A running period 
c                      icell = 3 - G1C running period 
c                      icell = 4 - G3 running period
c                      icell = 5 - G6C/G8A running period 
c                      icell = 6 - G11 running period 
c     
c                
c  vertex(3) (real) vertex coordinates
c  cdir(3)   (real) track direction cosines  
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  dist (real)  track length within target material
c  dist1 (real) track length in target wall
c
c  Other routines:
c  ---------------
c  IntsectionSphere, IntsectionCylindre
c
c  Notes:
c  ------
c
c  Author:   Eugene Pasyuk      Created:  Tue Apr  6 16:37:59 EDT 1999
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      INTEGER icell
      REAL vertex(3)
      REAL cdir(3)
      REAL dist
      REAL dist1 
      REAL dir(3)

c     Local pre-defined variables:
c     ---------------------------
c
c      REAL centre(3)

      REAL LC(6)                !Half-length of a cylinder
      REAL LS(6)                !sphere center offset
      REAL RC(6)                !Cylinder radius ! larger cone radius 
      REAL RC2(6)               !                ! smaller cone radius
      REAL RS(6)                !Sphere radius
      REAL WALL(6)              !wall thickness

      DATA LC/6.0, 4.5, 8.5, 8.5, 8.5,    20.0/
      DATA LS/6.0, 1.,  5.,  5.,  6.85,   16.0/
      DATA RC/ 3., 2.,  2.,  2.,  1.4,    2.595/
      DATA RC2/3., 2.,  2.,  2.,  1.4,    2.00/
      DATA RS/ 3., 4.,  4.,  4.,  2.248,  4.00/
c      DATA WALL/0.017, 0.0127, 0.0127, 0.019/ !originall wall thickness
      DATA WALL/0.032, 0.0277, 0.0277, 0.034, 0.0277,  0.25/ 
c wall thickness includes superinsulation, which is about 15 mg/cm^2


      DATA dir /0., 0., 1./     !target axis direction cosines


c     RCS information: 

      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: targcell.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.8 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/12/07 19:04:04 $')
      PARAMETER (CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/   
     1     '$Id: targcell.F,v 1.8 2002/12/07 19:04:04 pasyuk Exp $'/
      
c     Module information:

      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TARGCELL')
      PARAMETER (CRAUTH='Eugene Pasyuk')
c     
c     Local User defined variables:
c     -----------------------------
c
      INTEGER NWRITE
      INTEGER ierr
      INTEGER ierr1 
      INTEGER extrm
      INTEGER i

      REAL pos1(3)
      REAL pos2(3)
      REAL pos1w(3)
      REAL pos2w(3)
      REAL point(3)
      REAL cc(3)
      REAL r,r1,r2,h
      REAL x
      data nwrite/0/

      real target_offset(3), st_offset
      common/eloss_geom/ target_offset, st_offset

c     _end_var
c     
c     executable code for routine LH2TARG:
c---- 6----------------------------------------------------------------72

cc      print *,'  LOSS: TARGCELL-----------------------------'

      if(icell.eq.0) then
	dist=0.
	dist1=0.
	return
      endif

      do i = 1,3
         point(i) = vertex(i) + 50.*cdir(i)
         cc(i) = target_offset(i)
      enddo

c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
c         write(CRMESS,*) 
c     + 'TARGCELL: target position is: ', target_offset
c
c         CALL RECMES(CRNAME,'I',CRMESS)
         
      ENDIF
c     
c------------------------------------------------------------------------------

      if(icell.eq.6)then ! target cel is a cone
        r1 = ((RC(6)+RC2(6))/2.) 
        r2 = RC2(6)
        h  = LC(6) + target_offset(i)
        r1 = ((r1-r2)/LC(6))*target_offset(3) + r1
        CALL IntZCone(vertex, cdir, r1 ,r2, h, pos1, ierr)
cc        print *,'   CSS: pos1,ierr=',pos1,ierr
      else  ! target cel is a cylinder
        call IntCylinder(vertex, cdir, cc, dir, RC(icell),
     $       pos1, ierr)
      endif

      if (ierr .eq. 0 .or. ierr .eq. 2 ) then
         dist = 0.
         dist1 = 0.
         return                 ! no intersection, get out
      endif


c---  cylinder or sphere?
      extrm = 0
      if ( pos1(3) .gt. LC(icell)+target_offset(3) ) extrm = 1
      if ( target_offset(3)-LC(icell) .gt. pos1(3)) extrm = -1

c---  sphere
      if (extrm .ne. 0) then 
         cc(3) = extrm*LS(icell) + target_offset(3)
         call IntsectionSphere(vertex, point, cc, RS(icell),
     $        pos1, pos2, ierr)
         if (ierr .eq. 0) then 
            dist = 0.
            dist1 = 0. 
            return              !no intersection, get out
         endif
c---  spheric wall
         r = RS(icell) + wall(icell)
         call IntsectionSphere(vertex, point, cc, r,
     $        pos1w, pos2w, ierr1)
cc         print *,'   CSS: SPHERE pos1 =',pos1,pos1w
         if (ierr .eq. 2) then
            pos1(1) = pos2(1)
            pos1(2) = pos2(2)
            pos1(3) = pos2(3)
            pos1w(1) = pos2w(1)
            pos1w(2) = pos2w(2)
            pos1w(3) = pos2w(3)
         endif
c---  cylinder or cone wall
      else 
        if(icell.eq.6)then ! target cel is a cone
          r1=r1+wall(icell)
          r2=r2+wall(icell)
          CALL IntZCone(vertex, cdir, r1 ,r2, h, pos1w, ierr)
cc          print *,'   CSS: CYLL pos2 =',pos1w
        else  ! target cel is a cylinder
          r = RC(icell) + wall(icell)
          call IntCylinder(vertex, cdir, cc, dir, r,
     $                     pos1w, ierr)
        endif
      endif

      dist = 0.
      x = 0.

      do i = 1,3
         dist = dist + (pos1(i) - vertex(i))*(pos1(i) - vertex(i))
         x = x + (pos1w(i) - vertex(i))*(pos1w(i) - vertex(i))
      enddo

      dist = sqrt(dist)
      x = sqrt(x)
      dist1 = x - dist

cc         print *,'   CSS: CYLL dist =',dist ,dist1

      RETURN
      END




c-----------------------------------------------------------------------------

      SUBROUTINE AIR_GAP(vertex, cdir, xpoint)
c
c_begin_doc
c  RCS ID string
c  $Id: air_gap.F,v 1.1 2004/02/24 21:44:59 pasyuk Exp $
c
c  Documentation for subroutine AIR_GAP
c
c  Purpose: Finds a point wher track hits Region 1 DC
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c  vertex(3) (real) vertex coordinates
c  cdir(3)   (real) track direction cosines  
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c  xpoint(3) - intercept of a track with Region 1 DC
c
c  Other routines:
c  ---------------
c  IntesectionPlan 
c  InterCylinder
c
c  Notes:
c  ------
c
c  Author:   Eugene Pasyuk      Created:  Tue Feb  17 15:38:45 EST 2004
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE 
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL vertex(3)
      REAL cdir(3)
      REAL xpoint(3)
c
c  Local pre-defined variables:
c  ---------------------------

      REAL d_flat               !distance from beam axis to the 
                                !flat part of Region 1 DC
      REAL center_offset        !Offset of the center of the 
                                !cylindrical part from the beam axis
      REAL cyl_radius           !radius of cylidrical part

      REAL ncp(3)               !normal vector to central plane

      REAL pi

      DATA d_flat/57./
      DATA center_offset/53./
      DATA cyl_radius/110./

      DATA pi/3.14159265359/
      DATA ncp/0.,0.,1./



c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: air_gap.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/24 21:44:59 $')
      PARAMETER (CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/   
     1'$Id: air_gap.F,v 1.1 2004/02/24 21:44:59 pasyuk Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='AIR_GAP')
      PARAMETER (CRAUTH='Eugene Pasyuk')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer i
      integer ierr
      REAL alpha
      REAL w
      REAL phi 
      REAL point(3)
      REAL np(3)
      REAL start(3)
      REAL theta
      REAL dir(3)
      REAL aupoint(3)
      REAL norme
      REAL csinus
      REAL center(3)
      REAL cpi(3)


      DATA NWRITE/0/


c#ifdef Linux
c for Linux these functions are not implemeted in standard library
c
c      REAL sind, cosd, angle
c      sind(angle) = sin( angle*pi/180. )
c      cosd(angle) = cos( angle*pi/180. )
c
c#endif


c_end_var
c
c  executable code for routine AIR_GAP:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
c        CRMESS='This is a AIR_GAP routine, this message written once'
c        write(CRMESS,*) 
c        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c
c-------------Intersection with Region 1 DC
c

      alpha = atan2(cdir(2),cdir(1))
      alpha = 3.*alpha/pi
      w = nint(alpha)
      phi = w*60.

c -- vector normal to plane --
      np(1) = cos(phi*pi/180.)
      np(2) = sin(phi*pi/180.)
      np(3) = 0.

      do i = 1,3
         point(i) = vertex(i) + 1000.*cdir(i)
         start(i) = d_flat*np(i)
      enddo

c First let's find out if track hits flat part of Region 1

      call IntsectionPlan(vertex, point, start, np, aupoint, ierr)

      if ( aupoint(3) .le. 0. ) then !plane or cylinder?
c     -- plane --
C         write(*,*) 'plane'

         do i = 1,3
            xpoint(i) = aupoint(i)
         enddo
c We are done
         RETURN
      else

c     -- cylinder --
C define cylinder
C         write(*,*) 'cylinder'
         center(3) = 0.
         center(1) = center_offset*np(1)
         center(2) = center_offset*np(2)
c     -- axis direction
         dir(1) = np(2)
         dir(2) = -np(1)
         dir(3) = 0 

c -- following tricks to avoid double intercept
c-- central plane intercept
         do i = 1, 3 
            start(i) = 0.
         enddo
         call IntsectionPlan(vertex, point, start, ncp, aupoint, ierr)

         call IntCylinder(aupoint, cdir, center, dir,
     +        cyl_radius, xpoint, ierr)
      endif
C Done with cylinder

      RETURN
      END
c
c------------------------------------------------------------------------------




c        tanalpha = (RC(6)-RC2(6)) / (2.*LC(6)) *15.
c        alpha = atan(tanalpha)
c        centre(3) = centre(3) - LC(6) + RC(6)/tanalpha


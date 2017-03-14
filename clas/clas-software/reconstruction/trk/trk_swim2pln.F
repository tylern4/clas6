C=======================================================================
      SUBROUTINE trk_swim2pln(pln,vins,vect,tintp,sint,zangle,minstep,maxstep,lmax,l_int,stepnumber)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Track swimming (traj. propagation) in magnetic
C-                         field until track intersects a plane.
C-
C-   Inputs  :
C-             pln     - plane (see sdaswim.inc: sda_pln)
C-                     note: if pln(8)>pln(7) plane is assumed to be
C-                           a plane, otherwise it is assumed to be a
C-                           cylinder.  also note that this does not take
C-                           take into account R1 DC where the "plane"
C-                           is made up of both a plane & a cylinder.  The
C-                           user of this routine is presumed to know which 
C-                           half he is looking for.
C-             vins(9) - Starting values {x,y,z,Px/P,Py/P,Pz/P,P,m,Q}
C-             zangle  - replaces tcl variables trk_zangle
C-             minstep - replaces tcl variable trk_zstep(1)
C-             maxstep - replaces tcl variable trk_zstep(2)
C-             lmax    - maximum track length before giving up
C-             stepnumber - used to determine if "first" step
C-
C-   Outputs : vect(9) -last starting value of step before intercept (same def as vins)
C-             tintp(7)- track intersection with plane
C-                      {x,y,z,Px/P,Py/P,Pz/P,L}   
C-                      where L is the track length from vins to tintp
C-             l_int =0 if no intercept found, 1 otherwise
C-             sint    - step size from last starting point to intercept
C-   Controls:
C-
C-   Calls   : sda_helix, trk_icyl, trk_ipln, 
C-           not really called: sda_trmat
C-
C-   Created   OCT-26-1990  Bogdan Niczyporuk
C-   Modified  AUG-2-1992   Billy Leung
C-             Nov.1997     F.Klein
C-             12 June 1998 J.W.C. McNabb
C-   
C-   Called by Joe's car
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  Subroutine parameters
*  ---------------------
      REAL vins(9), tintp(7), pln(8)
      REAL zangle, minstep, maxstep, lmax
      INTEGER l_int
      INTEGER stepnumber
*
*  External
*  --------
      REAL    VMOD
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_swim2pln.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:28 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_swim2pln.F,v 1.5 2008/03/16 04:33:28 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_SWIM2PLN')
      PARAMETER (CRAUTH='Franz Klein')
*
*  Local variables
*  ---------------
      INTEGER i,j,ist,iflag,nstep, ptg_flag
      REAL ECon, Binv, Bold, Dangle, sint, stot
      REAL vect(9), vout(9), xint(9), bfld(3), step
      real bfldold(3)
      integer isec
      data isec /0/
C
      DATA ECon/2.99792E-4/
C
C
C TRK_SWIM2PLN begins
      l_int = 0 
      Dangle = zangle/ECon

C Polarized Target field use is disabled for now      
      ptg_flag = 1
C Buffer "vect" is used in DO loop
      DO i=1,9
         vect(i) = vins(i)
      ENDDO
C Initialise hit array
      DO j=1,3
         tintp(j)=1000.
         tintp(j+3) = 0.
      ENDDO
      tintp(7) = 1000.
C Initialise indices
      nstep = 1000./minstep
      stot  = 0.
      ptg_flag = 1
C Do loop traces particle through detector
      DO 100 ist = stepnumber,nstep
C Calculate a step size
         IF(vins(9).NE.0.) THEN
            Bold = VMOD(bfld,3)
            IF(minstep.LT.maxstep) THEN
               IF(Bold.LT.0.005) THEN
                  Binv = 200.
               ELSE
                  Binv = 1./Bold
               ENDIF
               step = Dangle*vect(7)*Binv
               IF(step.LT.minstep) then
                  step = minstep
               ELSEIF(step.GT.maxstep) then
                  step = maxstep
               ENDIF
            ELSE
               step = minstep
            ENDIF
         ELSE
            step = 10.
         ENDIF
C First step size have to smaller then a distance to the 1-st plane
         IF(ist.EQ.1) then
            step = 0.05
         ENDIF

C Save old Bfld info
         DO i=1,3
            bfldold(i) = bfld(i)
         ENDDO

C Call Helix
         CALL sda_helix(step,vect, vout,bfld, ptg_flag, isec)
c       write(*,*) 
c     &       'trk step number: ',ist,'step size: ',step,
c     &       'vout: ',vout(1),',',vout(2),',',vout(3)
c        call Reclog(CRNAME,'W',CRMESS)

         sint = step
         iflag = 0
         IF(pln(8).GT.pln(7)) THEN
            CALL trk_ipln(vect,vout,pln,sint,xint,iflag)
         ELSE
            CALL trk_icyl(vect,vout,pln,sint,xint,iflag)
         ENDIF
C if Track crossed a cylinder or plane
         IF(iflag.EQ.1) THEN
            DO i = 1,6
               tintp(i) = xint(i)
            ENDDO
            tintp(7) = stot + sint
C only layer has been reached so
            l_int = 1
            GO TO 999
         ENDIF

         stot = stot + step
C if track went to far
         IF(stot.GT.lmax) GO TO 999
C otherwise keep going
         DO j=1,9
            vect(j) = vout(j)
         ENDDO
 100  CONTINUE                  ! End loop over Nstep
C
 999  CONTINUE
      DO i=1,3
         bfld(i) = bfldold(i)
      ENDDO

      stepnumber = ist
C     
      RETURN
      END










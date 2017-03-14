C======================================================================
      SUBROUTINE sda_trmat(new,vect,vout,sl,bfld,ipl,iflag)
C----------------------------------------------------------------------
*_begin_doc
*  RCS ID string
*  $Id: sda_trmat.F,v 1.5 1999/12/13 17:24:04 feuerbac Exp $
C-
C-    Purpose:  Calculates transport matrix for one step.
C-              Records matrix at point of layer penetration.
C-
C-    Inputs:  NEW = 1 TRANSMAT called 1-st time, otherwise: NEW = 0
C-             VECT(9)  -  vector {x,y,z,px/p,py/p,pz/p,p,m,Q} at begining step
C-             VOUT(9)  -  vector at end of step
C-             SL       -  Current step length
C-             BFLD(3)  -  Magnetic field: Bx,By,Bz
C-             IPL      -  Index of current layer (plane)
C-             IFLAG       = 1 track crossed a layer
C-                         = 0 not
C-    Output:  ERTRSP (5,5,nch)      - Transport matrix
C-  Controls:         
C-
C-  Library belongs: libsda.a
C-
C-     Calls: sda_xmm55
C-
C-    Author:  S. Laubach, B. Niczyporuk (Jul. 1990)
C-    Modified by: B. Niczyporuk in October 1990 (see CEBAF-PR-91-004)
C-                 B. Niczyporuk on Dec.9, 1996 (when 1-st plane < 1-st step) 
C-
C-    Modified by: C.A.Meyer January 28, 1998.
C-                 This routine is called a large number of times during
C-                 track fitting. As such, any speed ups in its execution
C-                 could greatly help RECSIS in general. In this light, 
C-                 one will notice that the array only has a subset of
C-                 Elements which change. All other elements are fixed.
C-                 In the older version, the matrix was set to the 
C-                 (5x5) identity matrix on every entry. This has been
C-                 changed to a DATA statement. 
C-
C-                 Also noticed that the matrix copies were being done
C-                 relative to the fortran array indices a(i,j) by looping
C-                 over j inside the loop over i. Fortran stores things such
C-                 that it is better to loop over the first index inside
C-                 the second.
C-
C-                 An important thing to note is the the b-matrix is
C-                 retained between calls to this routine. It is initialized
C-                 when the NEW flag is 1, and as long as IFLAG is not
C-                 equal to 1, it contains the propogating matrix. Once
C-                 Iflag is set to 1 (cross  the VTX plane), the result
C-                 is stored in sda_tmat(*,*,ipl)
C-
C-    Called by sda_swim
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
c      SAVE
C
#include "sdaswim.inc"
C
      INTEGER new, ipl, iflag
      REAL    vect(9), vout(9), bfld(3), sl
C
C Local variables
* RCS information
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: sda_trmat.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/12/13 17:24:04 $')
      PARAMETER ( CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/
     1'$Id: sda_trmat.F,v 1.5 1999/12/13 17:24:04 feuerbac Exp $
     3'/

      INTEGER i,k
      REAL cfact,pm12,sinl,cosl,cosl1,sinp,cosp,hamx,fact,b0,b2,b3,tgl 
      REAL a(5,5),b(5,5), tn(3),hn(3),ch
C
      DATA cfact / 2.9979251E-4 /
C
C        Initialize a(5,5) as an identity matrix:
C
      DATA a / 1.0,5*0.0,1.0,5*0.0,1.0,5*0.0,1.0,5*0.0,1.0 /
      SAVE a,b,crcsid,cfact

C
C ERROR PROPAGATION ON A HELIX
C
      ch    = vect(9)
      tn(1) = vect(4) + vout(4)
      tn(2) = vect(5) + vout(5)
      tn(3) = vect(6) + vout(6)
      pm12  = 1. / SQRT(tn(1)**2 + tn(2)**2 + tn(3)**2)
      tn(1) = tn(1) * pm12
      tn(2) = tn(2) * pm12
      tn(3) = tn(3) * pm12
C
      sinl = tn(3)
      cosl = SQRT(ABS(1. - sinl**2))
      IF (cosl.EQ.0) THEN
c         this actually does mean something.
c         WRITE(6,*)'Particle travels in z-direction'
         GO TO 999
      ENDIF
      cosl1 = 1./cosl
      sinp  = tn(2) * cosl1
      cosp  = tn(1) * cosl1
C
C DEFINE TRANSFORMATION MATRIX BETWEEN X1 AND X2 FOR
C NEUTRAL PARTICLE OR FIELDFREE REGION
C
***>      DO i = 1,5
***>        DO k = 1,5
***>          a(i,k) = 0.
***>        ENDDO
***>        a(i,i) = 1.
***>      ENDDO
C
      a(4,3) = sl * cosl
      a(5,2) = sl
C
      IF(ch.EQ.0.) GO TO 100
      pm12 = 2./(vect(7) + vout(7))
      hamx  = SQRT(bfld(1)**2 + bfld(2)**2 + bfld(3)**2)*pm12
      IF(hamx.LE.0.005) GO TO 100
C
C DEFINE AVERAGE MAGNETIC FIELD AND GRADIENT
C
      fact = ch*cfact
      hn(1) = bfld(1)*fact
      hn(2) = bfld(2)*fact
      hn(3) = bfld(3)*fact
C
      b0  =  hn(1)*cosp + hn(2)*sinp
      b2  = -hn(1)*sinp + hn(2)*cosp
      b3  = -b0*sinl + hn(3)*cosl
      tgl =  sinl*cosl1
C
C COMPLETE TRANSFORMATION MATRIX BETWEEN ERRORS AT X1 AND X2
C
      a(2,1) =  sl*b2
      a(2,3) = -b0*(sl*pm12)
      a(2,4) =  b3*(b2*pm12*(sl*pm12))
      a(2,5) = -b2*(b2*pm12*(sl*pm12))
C
      a(3,1) = -sl*b3*cosl1
      a(3,2) =  b0*sl*pm12*cosl1**2
      a(3,3) =  1. + tgl*b2*sl*pm12
      a(3,4) = -b3*b3*pm12*(sl*pm12)*cosl1
      a(3,5) =  b3*(b2*pm12*(sl*pm12))*cosl1
C
      a(4,5) = -(b3*tgl*sl*pm12)
C
      a(5,4) =  (b3*tgl*sl*pm12)
C
  100 CONTINUE
C
C new = 0  transformation matrix is updated
C new = 1  transformation matrix is initialized
C
C        The following code only happens when we start a new track. As
C        such, other than reversing the order of the indicies, it does
C        not seem necessary to equivalence arrays, and copy wihout a
C        nested do loop. I have modified this so that we will never 
C        execute more than one do-loop.
C
      IF(new.EQ.1) THEN
         new = 0
C
C Already the track crossed VTX plane on the 1-st step (modified: Dec.9,1996)
C
         IF ( Iflag .eq. 1) Then
            DO i = 1,5
               DO k = 1,5
                  b(k,i) = a(k,i)
                  sda_tmat(k,i,ipl) = a(k,i) 
               ENDDO
            ENDDO
         ELSE
            DO i = 1,5
               DO k = 1,5
                  b(k,i) = a(k,i)
               ENDDO
            ENDDO
         ENDIF
        GO TO 999
      ENDIF
C
      IF(iflag.EQ.1) THEN
C
C          Track have cross a layer (plane), store transport matrix sda_tmat(5,5,ipl)
C
         CALL sda_xmm55(a,b,sda_tmat(1,1,ipl))
C
      ELSE
C
C          Not crossed, continue the matrix error propagation
C
         CALL sda_xmm55(a,b,b)
C
      ENDIF
C
 999  RETURN
      END

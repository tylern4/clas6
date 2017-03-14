c  lcgeom.inc

c  $Id: lcgeom.inc,v 1.2 1997/04/24 15:38:47 holtrop Exp $

      character*132 cilcgeom
      data cilcgeom/
     1'$Id: lcgeom.inc,v 1.2 1997/04/24 15:38:47 holtrop Exp $'
     1/


c  from patch  in file /usr/user3/burtin/gclas/glas.car on Tue Nov 15 11:02:18 EST 1994
c  this is seq: LCGEOM
*-----------------------------------------------------------------------
*   LCGEOM - Common block containing commonly used geometrical parameters.
*   Initialized in GELAS
*   PARAMETERS: NSECT      : Number of sectors
*   VARIABLES : SINSEC     : Sinus for rotation to elementary sector
*               COSSEC     : Cosinus for rotation to elementary sector
*               TNHASE     : Tangens of half sector angle (GELAS)
*               PTANG      : Orientation of the polarized target coils
*               HTHMG      : Half thickness of the coil (GELAS)
*-----------------------------------------------------------------------

      integer nsect
      real rnsect, rtmp
      parameter (nsect = 6)
      parameter (rtmp = nsect)
      parameter (rnsect = 1./rtmp)

      real sinsec,cossec,tnhase,ptang,hthmg
      common /lcgeom/ sinsec(nsect), cossec(nsect), tnhase, ptang, hthmg


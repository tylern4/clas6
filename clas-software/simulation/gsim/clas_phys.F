      subroutine clas_phys

c_begin_doc

c  Documentation for subroutine clas_phys

c  $Id: clas_phys.F,v 1.2 1997/04/24 15:38:19 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       Initialize material constants for all the physics        *
c     *       mechanisms used by GEANT and corrects same tables.       *
c     *                                                                *
c     *    ==>Called by : uginit                                       *
c     *       Author    M.Guckes  *******       (Nov. 1986)            *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 18-nov-94
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_phys.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:19 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'clas_phys')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_phys.F,v 1.2 1997/04/24 15:38:19 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcunit.inc"
#include "gconst.inc"
#include "dcmate.inc"
c_end_inc


c  local variables:
c  ----------------
c    Argon Ethan shell energies (1-3  ->  K, L, M),
c    probability for next shell (4-6) and florescence yield (7-9)
      real arekz(9), pbxsec(15), bexsec(15), cuxsec(15),
     1          fexsec(15), arxsec(15),cxsec(15),hxsec(15)
      data       pbxsec /  1.59e-5, 6.85e-5, 4.48e-5, 7.04e-5, 1.21e-4,
     1                     1.89e-4, 2.52e-4, 3.85e-4, 4.90e-4, 6.75e-4,
     2                     6.75e-4, 1.02e-3, 2.91e-3, 6.14e-3, 1.09e-2/
      data       bexsec /  9.47e-2, 7.66e-1,  2.69e0,  6.58e0,  1.31e1,
     1                      2.25e1,  3.18e1,  5.03e1,  6.76e1,  8.68e1,
     2                      8.68e1,  2.43e2,  3.03e2,  3.31e2,  3.48e2/
      data       cuxsec /  1.11e-5, 5.41e-5, 1.55e-4, 3.33e-4, 6.09e-4,
     1                     9.98e-4, 1.39e-3, 2.19e-3, 3.72e-3, 5.21e-4,
     2                     5.21e-4, 3.33e-4, 1.03e-2, 2.31e-2, 4.30e-2/
      data       fexsec /  1.47e-5, 8.20e-5, 2.38e-4, 5.15e-4, 9.44e-4,
     1                     1.55e-3, 2.54e-3, 4.20e-4, 5.52e-4, 7.52e-3,
     2                     7.52e-3, 4.99e-3, 1.57e-2, 3.53e-2, 6.54e-2/
      data       arxsec /  2.14e-4, 1.40e-3, 4.20e-3, 9.10e-4, 1.64e-3,
     1                     2.70e-3, 3.57e-3, 6.00e-3, 7.94e-3, 1.13e-2,
     2                     1.13e-2, 8.07e-2, 2.68e-1, 5.87e-1, 1.03e+0/
      data       cxsec  /  1.99e-4, 1.51e-3, 5.07e-3, 1.21e-2, 2.29e-2,
     1                     4.18e-2, 5.89e-2, 9.98e-2, 1.25e-1, 1.92e-1,
     2                     1.92e-1, 1.02e+0, 1.74e+0, 2.13e+0, 2.36e+0/
      data       hxsec  /  1.98e+0, 1.35e+1, 2.55e+1, 3.14e+1, 3.40e+1,
     1                     3.53e+1, 3.56e+1, 3.65e+1, 3.68e+1, 3.71e+1,
     2                     3.71e+1, 3.87e+1, 4.00e+1, 4.13e+1, 4.26e+1/
      data       arekz  /    1.e10,   3.2e-6,  0.25e-6,
     1                          1.,    0.023,      0.3,
     2                          0.,     0.15,      0.1/

      integer jma,jprob,jphot
c_end_var


c  executable code:
c  ----------------

c  modify DC gas, if defined
      if(dc_mat_dcgas.gt.0)then
         jma = lq(jmate-dc_mat_dcgas)

c  address of material constants
         jprob = lq(jma-4)

c  copy shell energies
         call ucopy(arekz(1), q(jprob+5), 3)

c  copy probability for next shell and shell flourescence efficiency
         call ucopy(arekz(4), q(jprob+34), 6)
      endif


c  correct the cross sections for lead
      jma = lq(jmate - 13)
      jphot = lq(jma - 6)
      call ucopy(pbxsec(1),q(jphot+1),15)

c  correct the cross sections for beryllium
      jma = lq(jmate - 5)
      jphot = lq(jma - 6)
      call ucopy(bexsec(1),q(jphot+1),15)

c  correct the cross section for copper
      jma = lq(jmate - 11)
      jphot = lq(jma - 6)
      call ucopy(cuxsec(1),q(jphot+1),15)

c  correct the photo cross section for iron
      jma = lq(jmate - 10)
      jphot = lq(jma - 6)
      call ucopy(fexsec(1),q(jphot+1),15)

c  correct the photo cross section for carbon
      jma = lq(jmate - 6)
      jphot = lq(jma - 6)
      call ucopy(cxsec(1),q(jphot+1),15)

c  correct the photo cross section for hydrogen
      jma = lq(jmate - 1)
      jphot = lq(jma - 6)
      call ucopy(hxsec(1),q(jphot+1),15)

      return
      end

c----------------------------------------------------------------------------

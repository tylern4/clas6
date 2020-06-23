      subroutine clas_part

c_begin_doc

c  Documentation for subroutine clas_part

c  $Id: clas_part.F,v 1.2 1997/04/24 15:38:19 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *    Initialize particle constants.                              *
c     *    Define standard GEANT (GPART) and additional particles and  *
c     *    resonances plus selected decay modes and branching ratios.  *
c     *                                                                *
c     *    All data taken from : M. AGUILAR-BENITEZ et al,             *
c     *                          Review of Particle Properties,        *
c     *                          Rev. Mod. Phys. 56(1984).             *
c     *                                                                *
c     *    MODE(I,J)      I'th decay mode of (J-49)'th particle        *
c     *    BRATIO(I,J)    Branching ratio for I'th decay mode of       *
c     *                   (J-49)'th particle                           *
c     *                                                                *
c     *    ==>Called by : UGINIT                                       *
c     *       Author    M.Guckes   *********     April 1988            *
c     *                                                                *
c     ******************************************************************


c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 18-nov-94

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: clas_part.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:19 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'clas_part')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: clas_part.F,v 1.2 1997/04/24 15:38:19 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcflag.inc"
#include "gcphys.inc"
#include "gcnum.inc"
c_end_inc


c  local variables:
c  ----------------
      integer mxpar
      parameter (mxpar=12)
      real bratio(6,mxpar), mode(6,mxpar)

c  Branching ratios for the particles defined below
      data bratio /
     &          95.00,       5.,       0.,       0.,       0.,       0.,  !b 0
     1          95.00,       5.,       0.,       0.,       0.,       0.,  !b +
     2          95.00,       5.,       0.,       0.,       0.,       0.,  !b -
     3          84.30,      2.9,     0.31,   0.0015,       0.,       0.,  !f2
     4          70.10,    14.50,     10.6,     4.90,     0.27,   0.0008,  !a2 0
     5          70.10,    14.50,     10.6,     4.90,     0.27,   0.0008,  !a2 +
     6          70.10,    14.50,     10.6,     4.90,     0.27,   0.0008,  !a2 -
     7          99.90,    0.046,   0.0067,   0.0045,       0.,       0.,  !rho 0
     8          99.90,    0.046,       0.,       0.,       0.,       0.,  !rho +
     9          99.90,    0.046,       0.,       0.,       0.,       0.,  !rho -
     &          89.60,     8.70,      1.7,     0.01,   0.0067,       0.,  !omega
     1          65.20,    30.16,      2.7,     0.17,    0.009,       0./  !eta'

c  Decay modes according the branching ratios above.
      data mode   /
     &          07 60,    57 17,        0,        0,        0,        0,  !b0
     1          08 60,    58 17,        0,        0,        0,        0,  !b+
     2          09 60,    59 17,        0,        0,        0,        0,  !b-
     3          08 09,    10 16,    17 17,    01 01,        0,        0,  !f2
     4          07 57,    07 17, 60 07 07,    10 16,    07 01,    01 01,  !a2 0
     5          08 57,    08 17, 60 07 08,    10 16,    08 01,        0,  !a2 +
     6          09 57,    09 17, 60 07 09,    10 16,    09 01,        0,  !a2 -
     7          08 09,    07 01,    05 06,    02 03,        0,        0,  !rho 0
     8          07 08,    08 01,        0,        0,        0,        0,  !rho +
     9          07 09,    09 01,        0,        0,        0,        0,  !rho -
     &       07 08 09,    07 01,    08 09, 07 05 06,    02 03,        0,  !omega
     1       17 08 09,    57 01,    60 01, 07 07 07, 05 06 01,        0/  !eta'

      integer i,ipar
c_end_var



c  executable code:
c  ----------------

c    Particle number  name  tracking type  mass  charge  life time
c                 \/   \/           \/     \/    \/      \/
ccc   call gspart(50, 'b 0$      ', 3, 1.233000, 0., 4.388000e-24, 0, 0)
      call gspart(51, 'b +$      ', 4, 1.233000, 1., 4.388000e-24, 0, 0)
      call gspart(52, 'b -$      ', 4, 1.233000,-1., 4.388000e-24, 0, 0)
      call gspart(53, 'f2$       ', 3, 1.274000, 0., 3.740000e-24, 0, 0)
      call gspart(54, 'a2 0$     ', 3, 1.318000, 0., 5.984000e-24, 0, 0)
      call gspart(55, 'a2 +$     ', 4, 1.318000, 1., 5.984000e-24, 0, 0)
      call gspart(56, 'a2 -$     ', 4, 1.318000,-1., 5.984000e-24, 0, 0)
      call gspart(57, 'rho 0$    ', 3, 0.770000, 0., 4.302000e-24, 0, 0)
      call gspart(58, 'rho +$    ', 4, 0.770000, 1., 4.302000e-24, 0, 0)
      call gspart(59, 'rho -$    ', 4, 0.770000,-1., 4.302000e-24, 0, 0)
      call gspart(60, 'omega$    ', 3, 0.782600, 0., 6.720000e-23, 0, 0)
      call gspart(61, 'eta''$    ', 3, 0.957570, 0., 2.740000e-21, 0, 0)

c  ???not sure why this works...ejw
c  define decay modes.
      if (idcay.le.0)return
      do i=1,mxpar
        ipar = i+49
        if (bratio(1,i).le.0.)return
        call gsdk(ipar, bratio(1,i), mode(1,i))
      enddo

      return
      end

c------------------------------------------------------------------------

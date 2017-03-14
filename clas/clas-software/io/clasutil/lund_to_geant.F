      integer function lund_to_geant(idlund)

c_begin_doc

c  Documentation for integer function lund_to_geant(idlund)

c  $Id: lund_to_geant.F,v 1.1.1.1 1997/03/28 20:50:01 marki Exp $

c  Purpose:
c  --------
c     translates lund to geant id, 0 if none found

c  Input:
c  ------
c     idlund       lund id

c  Output:
c  -------
c     returns translation from lund to geant id, 0 if none found

c  Notes:
c  ------
c     Mostly stolen from geant321.car

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 14-jul-95
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: lund_to_geant.F,v $')
      parameter (crevis= '$Revision: 1.1.1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/03/28 20:50:01 $')
      parameter (cautho= '$Author: marki $')
      parameter (crname= 'LUND_TO_GEANT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: lund_to_geant.F,v 1.1.1.1 1997/03/28 20:50:01 marki Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      integer idlund

c  local variables:
c  ----------------
      integer nsel
      parameter (nsel=44)

      integer iplund(nsel)
      data iplund/
     +    22,   -11,    11,    12,   -13,    13,   111,   211,
     +  -211,   130,   321,  -321,  2112,  2212, -2212,   310,
     +   221,  3122,  3222,  3212,  3112,  3322,  3312,  3334,
     + -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +   -15,    15,   411,  -411,   421,  -421,   431,  -431,
     +  4122,    24,   -24,    23/
c_end_var


c  executable code:
c  ----------------

      do lund_to_geant=1,nsel
         if(idlund.eq.iplund(lund_to_geant))return
      enddo
            
      lund_to_geant=0

      return
      end

c--------------------------------------------------------------------

      subroutine uftmed

c_begin_doc

c  Documentation for subroutine uftmed

c  $Id: uftmed.F,v 1.2 1997/04/24 15:38:57 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       Get parameters for tracking medium NUMED.                *
c     *                                                                *
c     *    ==>Called by : UTCHMY                                       *
c     *       Authors    M.Guckes  *********      (Nov.1987)           *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 5-dec-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: uftmed.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:57 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'uftmed')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: uftmed.F,v 1.2 1997/04/24 15:38:57 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcjloc.inc"
#include "gccuts.inc"
#include "gckine.inc"
#include "gconst.inc"
#include "gcphys.inc"
#include "gcflag.inc"
#include "gctmed.inc"
#include "gcmate.inc"
#include "gcsets.inc"
#include "gctrak.inc"
#include "gcking.inc"
c_end_inc

c  local variables:
c  ----------------
      integer i,jtmn
      real cmuls,dmaxms

      integer meca(5,12)
      equivalence (meca(1,1),ipair)
c_end_var


c  executable code:
c  ----------------

c  Get tracking medium and material parameters
      iupd   = 0
      jtm    = lq(jtmed - numed)
      call ucopy(iq(jtm+1), natmed, 5)

      nmat   = ifix(q(jtm + 6))
      isvol  = ifix(q(jtm + 7))
      ifield = ifix(q(jtm + 8))
      call ucopy(q(jtm+9), fieldm, 6)

      if (lq(jtm).eq.0) then
         if(istpar.ne.0)then
            call ucopy(q(jtmed+1), cutgam, 10)
            do i=1,12
               meca(1,i)=q(jtmed+10+i)
            enddo
            istpar=0
         endif

      else
         jtmn=lq(jtm)
         call ucopy(q(jtmn+1), cutgam, 10)
         do i=1,12
            meca(1,i)=q(jtmed+10+i)
         enddo
         istpar=1
      endif

      if (imuls.le.0) dmaxms=0.
      if (iloss.le.0) deemax=0.

      jma   = lq(jmate- nmat)
      jprob =  lq(jma-4)
      jmixt =  lq(jma-5)

      call ucopy(iq(jma+1), namate, 5)
      call ucopy(q(jma+6), a, 5)

c  Update Field and multiple scattering constants
      if(fieldm.ne.0.)cfield=3333.*degrad*tmaxfd/abs(fieldm)
      cmuls=400.*radl*dmaxms*dmaxms

      return
      end


c---------------------------------------------------------------------------------

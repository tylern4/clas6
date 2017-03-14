      subroutine utchmy

c_begin_doc

c  Documentation for subroutine utchmy

c  $Id: utchmy.F,v 1.2 1997/04/24 15:38:58 holtrop Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *       Perform absorption of X-rays and charged particles       *
c     *       in mylar foil.                                           *
c     *                                                                *
c     *    ==>Called by : GUSTEP                                       *
c     *       Author    M.Guckes  *******       (Nov. 1987)            *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 5-dec-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: utchmy.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:58 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'utchmy')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: utchmy.F,v 1.2 1997/04/24 15:38:58 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcunit.inc"
#include "gcvolu.inc"
#include "gctrak.inc"
#include "gckine.inc"
#include "gctmed.inc"
#include "gconst.inc"
c_end_inc

c  local variables:
c  ----------------

      integer nupd,nupds,jpar,numeds
      real xloc(3),ymom,snexts

c    dymy   : thickness of mylar
c    nummyl : mylar medium number
      integer nummyl
      real dymy
      data    dymy,nummyl
     1     /10.e-4,    21/
c_end_var


c  executable code:
c  ----------------

c  Get coordinate in local system
      call gtrnsf(vect,gtran(1,nlevel),grmat(1,nlevel),xloc)

c  Does the particle enter or leave at the mylar foil?
      jpar = lq(jgpar-nlevel)
      if (abs(xloc(2)).lt.q(jpar+4)-epsil) go to 99

c  Remember old step length
      snexts = snext

c  Remember old tracking medium number
      numeds = numed
      numed  = nummyl
      nupds  = nupd

c  Get tracking medium parameters
      call uftmed

c  Compute step length in mylar
c  Y-component of unit momentum vector in local coordinates:
      ymom = abs(vect(4)*grmat(4,nlevel) + vect(5)*grmat(5,nlevel)
     1             + vect(6)*grmat(6,nlevel))
      if (ymom .lt. 0.01) then
         snext = 100.*dymy
      else
         snext = dymy/ymom
      end if

c  Call the GEANT tracking routine for this particle type
      if (itrtyp.eq.1)then
         call gtgama
      else if (itrtyp.eq.2)then
         call gtelec
      else if (itrtyp.eq.4)then
         call gthadr
      else if (itrtyp.eq.5)then
         call gtmuon
      end if

c  Don't count the step in mylar for the track length
      sleng = sleng-step

c  Remember, it stopped in mylar
      if (istop.ne.0) then
         nmec = nmec+1
         lmec(nmec) = 25
      end if

c  Restore the old parameters again
      snext = snexts
      numed = numeds
      nupd  = nupds

c  All parameters only if the particle was entering the volume
      if (inwvol.eq.1) then
         call uftmed
      end if

 99   continue

      return
      end

c---------------------------------------------------------------------------------


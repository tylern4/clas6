      subroutine  calib_sc

c_begin_doc

c  Documentation for subroutine calib_sc

c  $Id: calib_sc.F,v 1.2 1997/04/24 15:38:04 holtrop Exp $


c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the Calibration Constants for the Scintillators   *
c     *                                                                *
c     *    ==>Called by :                                              *
c     *       Author  J. Ficenec ********     (Aug. 96)                *
c     *                                                                *
c     ******************************************************************

c  Major revisions:
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: calib_sc.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:04 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'calib_sc')
      parameter (crauth= 'John Ficenec')
      data crcsid/'
     1$Id: calib_sc.F,v 1.2 1997/04/24 15:38:04 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gconst.inc"
#include "gclist.inc"
#include "lcgeom.inc"
#include "sctmed.inc"
#include "scgeom.inc"
#include "sccalib.inc"
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  read SC calibration file
c  commented out until decide on source call for calib_sc()
c      call calib_sc_init()

c  add any other code to this portion of the program
c  structure is meant to mirror the geometry code

      return
      end

c----------------------------------------------------------------------------------

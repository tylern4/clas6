      subroutine ffky_ec1

c_begin_doc

c  Documentation for subroutine ffky_ec1

c  $Id: ffky_ec1.F,v 1.5 1998/06/08 19:29:29 fklein Exp $

c  Purpose:
c  --------
c	ffkey defs for ec1

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 2-may-95

c  Major revisions:
c  Harout Avakian 5 Jan 1996
c  ----------------
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: ffky_ec1.F,v $')
      parameter (crevis= '$Revision: 1.5 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/06/08 19:29:29 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'ffky_ec1')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: ffky_ec1.F,v 1.5 1998/06/08 19:29:29 fklein Exp $
     1 '/
      save


c  include files:
#include "ffpar_ec1.inc"
#include "ec1geom.inc"
c  --------------
c_end_inc

c  local variables:
c  ----------------
c_end_var


c  executable code:
c  ----------------

c  0 means use no atten, positive means use as mean value, 
c    neg read in from file
c  default is to read from file
      iatten=350   ! mean attenuation length
      call ffkey('EC1ATTENUAT', iatten,  1, 'INTEGER')
      ithreshold=50 ! ADC threshold
      call ffkey('EC1THRESHOLD', ithreshold,  1, 'INTEGER')
      delay=5.5    ! mean delay
      call ffkey('EC1DELAY', delay,  1, 'REAL')
      photoel=4.5  ! mean luminosity
      call ffkey('EC1PHOTOELEC', photoel,  1, 'REAL')
      reflect=0.45  ! reflection coefficient
      call ffkey('EC1REFLECT', reflect,  1, 'REAL')
      charge_to_adc=12.7
      call ffkey('EC1CH2ADC', charge_to_adc, 1, 'REAL')

c  cuts are:  cutgam,cutele,cutneu,cuthad,cutmuon
c
      call ffkey('CEC1CUTS',ec1_cuts,5,'REAL')
      ec1_cuts(1)=0.001
      ec1_cuts(2)=0.001
      ec1_cuts(3)=0.001
      ec1_cuts(4)=0.001
      ec1_cuts(5)=0.001
c
      return
      end

c---------------------------------------------------------------------------------


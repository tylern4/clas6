      SUBROUTINE CCCOMPR
c
c_begin_doc
c  RCS ID string
c  $Id: cccompr.F,v 1.2 1997/04/24 15:38:05 holtrop Exp $
c
c  Documentation for subroutine CCCOMPR
c
c  Purpose:  Printing of CC common blocks ( for debugging )
c  --------
c
c  Input Parameters:  (Name - Type - Meaning) No
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning) No
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov      Created:  Mon Jul 15 11:20:14 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "ccdig.inc"
#include "cccom.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cccompr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:05 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/   
     1'$Id: cccompr.F,v 1.2 1997/04/24 15:38:05 holtrop Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='CCCOMPR')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
c
      integer i
c_end_var
c
c  executable code for routine CCCOMPR:
c----6----------------------------------------------------------------72
c
  901 format(6i8,f10.5)
  902 format(' Input  point :',3f10.4)
  903 format(' Output point :',3f10.4)
  904 format(6i8)
c
      print *,'  -------------  cc common blocks printing  -----------'

c
      print *
      print *, '   ****   /CCdig/   ****'
      print *, 'n_hits_CC =', n_hits_CC 
c
      do i = 1, n_hits_CC
        print *,'   Sector ','Segment ','  TDC  ','     ADC  ','  Track'
        print 904, n_SECT_CC(i), n_CC_hit(i), I_TDC_CC(i), I_ADC_CC(i),
     *  I_TRACK_N(i)
      end do
c
      print *
      print *, '   ****   /CCHIT1/   ****'
      print *, 'jhtrk =', jhtrk
c
      do i = 1, jhtrk
c
        print *,'      N','   N track','    ID  ','   N hit ',
     *          ' Sector ', '  Segm ','   Momen '
        print 901,i,itrk_num(i),idh_prt(i),ihitrk(i),itrsect(i),
     *  itrsegm(i), ph_tot(i)
        print 902, xenter(1,i), xenter(2,i), xenter(3,i)
        print 903, xexit (1,i), xexit (2,i), xexit (3,i)
c
      end do
c
      print *
      print *, '   ****   /CCHIT2/   ****'
      print *, 'icchit =', icchit
c
      do i = 1, icchit
        print *,' Track #', '  Sector ',' Segment ','  TDC  '
        print 904, itrnum(i), iccsect(i), iccsegm(i), icctdc(i)
      end do
c      
      print *,'  -------------  cc common blocks printing  -----------'
c
      RETURN
      END
c
c----------------------------------------------------------------------







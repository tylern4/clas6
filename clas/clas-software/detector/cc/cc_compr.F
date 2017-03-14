      SUBROUTINE CC_COMPR
c
c_begin_doc
c  RCS ID string
c  $Id: cc_compr.F,v 1.2 1998/04/17 19:35:26 vlassov Exp $
c
c  Documentation for subroutine CC_COMPR
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
c  Author:   Alexander Vlassov    Created:  Mon Jul 15 11:20:14 EDT 1996
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
#include "cc_rc.inc"
#include "cc_revt.inc"
#include "cc_hrb.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_compr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/04/17 19:35:26 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_compr.F,v 1.2 1998/04/17 19:35:26 vlassov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='CC_COMPR')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
c
      integer i
c_end_var
c
c  executable code for routine CC_COMPR:
c----6----------------------------------------------------------------72
c
  901 format(6i8,f10.5)
  902 format(' Input  point :',3f10.4)
  903 format(' Output point :',3f10.4)
  904 format(6i8)
c
      print *,'  -------------  cc common blocks printing  -----------'
      print *
      print *, '   ****   /CCHIT/   ****'
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
      print *, '   ****   /CC_REVT/   ****'
      print *, 'n_hits_CC =', n_hits_CC 
c
      do i = 1, n_hits_CC
        print *,'   Sector ','Segment ','  TDC  ','     ADC  '
        print 904, n_SECT_CC(i), n_CC_hit(i), I_TDC_CC(i), I_ADC_CC(i)
      end do
c
      print *
      print *, '   ****   /CC_RC/   ****'
      print *, ' nrclust =', nrclust
c
      print *,'   Sector ',' Segment ','  Time ','  N ph.e. '
      do i = 1, nrclust
        print 904, nrsect(i), nrsegm(i), nrtime(i), nrphe(i)
      end do
c
      print *,'   Theta  ',' Delta th ',' Phy ind ',' i (EC) ',
     *        ' delta i '
      do i = 1, nrclust
        print 904, nrthet(i), nrdthet(i), nrphy(i), nriec(i), nrdiec(i)
      end do
c      
      print *,'  -------------  cc common blocks printing  -----------'
c
      RETURN
      END
c
c----------------------------------------------------------------------






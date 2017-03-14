      SUBROUTINE HBOOK_CLOSE
c
c  Documentation for subroutine HBOOK_CLOSE
c
c  Purpose:  closes the output file associated with HBOOK
c  --------
c
c  Author:   Arne Freyberger      Created:  Tue Oct 29 20:47:42 EST 1996
c  -------
c
      IMPLICIT NONE
      SAVE
c
c  include files :
c  ---------------------
#include "run_control.inc"
#include "re_units.inc"
#include "rhist.inc"
c
c  Local variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132 CRCSID
      DATA CRCSID/   
     1'$Id: hbook_close.F,v 1.5 2000/12/14 00:41:58 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='HBOOK_CLOSE')
c
      integer*4 iret, icycle
c
c  executable code for routine HBOOK_CLOSE:
c----6----------------------------------------------------------------72
c
c
c the following block of calls is to close the entire HBOOK/PAW file 
c
c change back to top directory       
      if(lhst.eq.0) return
      call hcdir('//PAWC',' ')
      call hcdir('//LUN1',' ')
      CALL HROUT(0,ICYCLE,'T')
      CALL HREND('//LUN1')
      write(crmess,10)chist_filename
 10   format('Closing file: ', a80)
      call recmes(crname,'i',crmess)
      close(LHST)
      CALL RESLUN(CRNAME,-LHST,IRET)

      RETURN
      END
c
c------------------------------------------------------------------------------





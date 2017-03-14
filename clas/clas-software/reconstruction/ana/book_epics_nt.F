      SUBROUTINE BOOK_EPICS_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_epics_nt.F,v 1.4 2008/03/16 04:33:08 fklein Exp $
c
c  Documentation for subroutine BOOK_EPICS_NT.F
c
c  Purpose:
c  --------
c
c  Input Parameters: Non (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:Non  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan       Created:  2004
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
#include "epics_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_epics_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:08 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: book_epics_nt.F,v 1.4 2008/03/16 04:33:08 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_EPICS_NT.F')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, NTID
c_end_var
c
c  executable code for routine BOOK_SCRW_NT.F:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for IC hits'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      print *,"Booking EPICS",ntid
      CALL hbname(NTID,'epics',run,
     +'run:I,event:I,evntime:I,intt:R,ebsya:R,ebsyb:R,etiefa:R,'//
     +'x2c21a:R,y2c21a:R,c2c21a:R,x2c24a:R,y2c24a:R,c2c24a:R,'//
     +'pscurrent:R,psprobe:R,taggercurrent:R,taggerprobe:R')
c
c  initialize common to -10. (in case that epics value not existing in data)
      ebsya=-10
      ebsyb=-10
      etiefa=-10
      x2c21a=-10
      y2c21a=-10
      c2c21a=-10
      x2c24a=-10
      y2c24a=-10
      c2c24a=-10
      pscurrent=-10
      psprobe=-10
      taggercurrent=-10
      taggerprobe=-10
      RETURN
      END
c
c------------------------------------------------------------------------------





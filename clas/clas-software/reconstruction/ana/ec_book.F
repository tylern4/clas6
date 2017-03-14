      SUBROUTINE EC_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: ec_book.F,v 1.5 1999/06/09 19:35:32 stepanya Exp $
c
c  Documentation for subroutine EC_BOOK
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
#include "histtcl.inc"
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
      PARAMETER (CFILE = '$RCSfile: ec_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/06/09 19:35:32 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ec_book.F,v 1.5 1999/06/09 19:35:32 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_BOOK')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ID
c_end_var
c
c  executable code for routine EC_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking EGN N-tupls and Histograms'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk

      call hmdir('//PAWC/EC','S')
c 
      IF(LFEC_HIST)THEN
        CALL BOOK_EC_HIST
        CRMESS='Histograms for forward EC are booked'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF(LECHB_NT_DO)THEN
        call hcdir('//LUN1/EC',' ') 
        ID=14
        CALL hbnt(ID,'Hits_out',' ')
        CALL hbname(ID,' ',0,'$clear')
        CALL BOOK_ECHB_NT(ID)
        CRMESS='N-tupl corresponding to ECHB is booked'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF(.NOT.LECHB_NT_DO.AND..NOT.LFEC_HIST)THEN
        CRMESS='No Forward EC N-tupls or Histograms are booked'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c changed back to top directory
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------
















      SUBROUTINE SEB_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: seb_book.F,v 1.9 2002/03/20 23:17:04 stepanya Exp $
c
c  Documentation for subroutine SEB_BOOK
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
      PARAMETER (CFILE = '$RCSfile: seb_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.9 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/03/20 23:17:04 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: seb_book.F,v 1.9 2002/03/20 23:17:04 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='SEB_BOOK')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ID
c_end_var
c
c  executable code for routine SEB_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking SEB Histograms'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
c  Do your HbookS below this line
      IF(LSEB_HIST)THEN
c      call hmdir('//LUN2/SEB','S')
         call hmdir('//PAWC/SEB','S')
c         
         CALL BOOK_PART_HIST
         CRMESS='Histograms for forward EC are booked'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF(.not.lall_nt_do.and.LSEB_NT_DO)THEN
        call hmdir('//LUN1/SEB',' ')
        call hcdir('//LUN1/SEB',' ')
        ID=15
        CALL hbnt(ID,'Hits_out',' ')
        CALL hbname(ID,' ',0,'$clear')
        CALL BOOK_SEB_NT(ID)
        CRMESS='N-tupl corresponding to output event is booked'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF(.NOT.LSEB_NT_DO.AND..NOT.LSEB_HIST)THEN
        CRMESS='No N-tupls or Histograms are booked for event output'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c changed back to top directory
      call hcdir('//PAWC',' ')
c      call hcdir('//LUN1',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------
















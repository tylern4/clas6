      SUBROUTINE IC_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: ic_book.F,v 1.4 2004/02/19 18:07:46 clasdvcs Exp $
c
c  Documentation for subroutine IC_BOOK
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
      PARAMETER (CFILE = '$RCSfile: ic_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/19 18:07:46 $')
      PARAMETER (CAUTHO = '$Author: clasdvcs $')
      DATA CRCSID/   
     1'$Id: ic_book.F,v 1.4 2004/02/19 18:07:46 clasdvcs Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='IC_BOOK')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ID
c_end_var
c
c  executable code for routine IC_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking IC N-tupls and Histograms'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
      
c 
      IF(LIC_HIST)THEN
         call hmdir('//PAWC/IC','S')
c        CALL BOOK_IC_HIST
        CRMESS='Histograms for forward EC are booked'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      IF(.not.lall_nt_do.and.LICHB_NT_DO)THEN
        call hcdir('//LUN1/IC',' ') 
        ID=21
        print *,'IC N-tuple',id
        CALL hbnt(ID,'icnt_out',' ')
        CALL hbname(ID,' ',0,'$clear')
        CALL BOOK_ICHB_NT(ID)
        CRMESS='N-tupl corresponding to ECHB is booked'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c changed back to top directory
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------
















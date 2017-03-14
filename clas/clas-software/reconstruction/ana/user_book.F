      SUBROUTINE USER_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: user_book.F,v 1.3 2004/04/30 22:00:36 stepanya Exp $
c
c  Documentation for subroutine USER_BOOK
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
c#include "bcs.inc"
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
      PARAMETER (CFILE = '$RCSfile: user_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/04/30 22:00:36 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: user_book.F,v 1.3 2004/04/30 22:00:36 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_BOOK')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, NTID
c_end_var
c
c  executable code for routine USER_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking USER Histograms'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
      call hmdir('USER','S')
      call hcdir('//PAWC',' ')
      call hmdir('USER','S')
      call hcdir('//PAWC/USER','S')

c  Do your HbookS below this line


c changed back to top directory
      if(lusr1_h_do)then
         call hcdir('//PAWC',' ')
         call hcdir('//LUN1',' ')
         print *,'Booking a new ntuple'
         ntid=77
         CALL hbnt(NTID,'tagcalib',' ')
         CALL hbname(NTID,' ',0,'$clear')
         call book_epics_nt(ntid)
         CALL BOOK_TAGR_NT(NTID)
         call book_msd_nt(ntid)
      endif
      RETURN
      END
c
c------------------------------------------------------------------------------
















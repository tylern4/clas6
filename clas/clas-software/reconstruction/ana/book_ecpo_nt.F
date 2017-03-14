      SUBROUTINE BOOK_ECPO_NT(ID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_ecpo_nt.F,v 1.1 1998/08/21 21:33:01 stepanya Exp $
c
c  Documentation for subroutine BOOK_ECPO_NT
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
c  Author:    S. Stepanyan     Created:  Fri Aug 14 00:41:53 EST 1998
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
#include "ecpo_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_ecpo_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/08/21 21:33:01 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: book_ecpo_nt.F,v 1.1 1998/08/21 21:33:01 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_ECPO_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ID
c_end_var
c
c  executable code for routine BOOK_ECPO_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for pointers to EC'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      CALL hbname(ID,'echit',nhit,
     +'nhit[0,30]:I,'//
     +'slh(nhit):I,'//
     +'uid1(nhit):I,uid2(nhit):I,'//
     +'vid1(nhit):I,vid2(nhit):I,'//
     +'wid1(nhit):I,wid2(nhit):I')
      RETURN
      END
c
c------------------------------------------------------------------------------





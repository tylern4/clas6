      SUBROUTINE BOOK_ECHB_NT(ID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_echb_nt.F,v 1.2 1997/06/13 03:12:44 tung Exp $
c
c  Documentation for subroutine BOOK_ECHB_NT
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
c  Author:    S. Stepanyan     Created:  Fri Dec 13 00:41:53 EST 1996
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
#include "Ec_general.PAR"
#include "echb_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_echb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/06/13 03:12:44 $')
      PARAMETER (CAUTHO = '$Author: tung $')
      DATA CRCSID/   
     1'$Id: book_echb_nt.F,v 1.2 1997/06/13 03:12:44 tung Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_ECHB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ID
c_end_var
c
c  executable code for routine BOOK_ECHB_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for EC hits'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      CALL hbname(ID,'echit',nhit,
     +'nhit[0,30]:I,'//
     +'nsecthit(nhit):I,'//
     +'layer(nhit):I,'//
     +'nhitl(nhit):I,'//
     +'nhitid(nhit):I,'//
     +'nstrpu(nhit):I,'//
     +'nstrpv(nhit):I,'//
     +'nstrpw(nhit):I,'//
     +'match1(nhit):I,'//
     +'match2(nhit):I,'//
     +'ehit(nhit):R,'//
     +'thit(nhit):R,'//
     +'ihit(nhit):R,'//
     +'jhit(nhit):R,'//
     +'xhit(nhit):R,'//
     +'yhit(nhit):R,'//
     +'zhit(nhit):R,'//
     +'dihit(nhit):R,'//
     +'djhit(nhit):R,'//
     +'dxhit(nhit):R,'//
     +'dyhit(nhit):R,'//
     +'dzhit(nhit):R,'//
     +'widths(nhit):R,'//
     +'distu(nhit):R,'//
     +'distv(nhit):R,'//
     +'distw(nhit):R,'//
     +'pathu(nhit):R,'//
     +'pathv(nhit):R,'//
     +'pathw(nhit):R,'//
     +'m2uhit(nhit):R,'//
     +'m2vhit(nhit):R,'//
     +'m2whit(nhit):R,'//
     +'m3uhit(nhit):R,'//
     +'m3vhit(nhit):R,'//
     +'m3whit(nhit):R,'//
     +'m4uhit(nhit):R,'//
     +'m4vhit(nhit):R,'//
     +'m4whit(nhit):R,'//
     +'c2match1(nhit):R,'//
     +'c2match2(nhit):R')
      RETURN
      END
c
c------------------------------------------------------------------------------





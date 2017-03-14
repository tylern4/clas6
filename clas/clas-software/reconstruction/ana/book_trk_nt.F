      SUBROUTINE BOOK_TRK_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_trk_nt.F,v 1.1 1997/06/11 04:41:30 stepanya Exp $
c
c  Documentation for subroutine BOOK_TRK_NT
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
c  Author:   S. Stepanyan      Created:  Sat Feb 15 18:08:08 EST 1997
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
#include "trk_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_trk_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/06/11 04:41:30 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: book_trk_nt.F,v 1.1 1997/06/11 04:41:30 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_TRK_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,NTID
c_end_var
c
c  executable code for routine BOOK_TRK_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      call hbname(NTID,'trk',ntrk,
     +'ntrk[0,10]:I,itrk(ntrk):I,isect(ntrk):I,'//
     +'ptrk(ntrk):R,mtrk(ntrk):R,chtrk(ntrk):R,'//
     +'xtrktrg(ntrk):R,ytrktrg(ntrk):R,ztrktrg(ntrk):R,'//
     +'cxtrktrg(ntrk):R,cytrktrg(ntrk):R,cztrktrg(ntrk):R')
      RETURN
      END
c
c------------------------------------------------------------------------------





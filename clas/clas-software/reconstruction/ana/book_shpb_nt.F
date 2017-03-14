      SUBROUTINE BOOK_SHPB_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_shpb_nt.F,v 1.1 2009/03/31 19:00:04 myurov Exp $
c
c  Documentation for subroutine BOOK_ICHB_NT
c
c  Purpose: Booking Ntuple for inner calorimeter reconstructed particles.
c  --------
c
c  Input Parameters:  Non
c  ----------------
c
c  Output Parameters: Non
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S,. Stepanyan    Created:  Fri Mar  7 09:43:38 EST 1997
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
#include "shpb_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_shpb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/31 19:00:04 $')
      PARAMETER (CAUTHO = '$Author: myurov $')
      DATA CRCSID/   
     1'$Id: book_shpb_nt.F,v 1.1 2009/03/31 19:00:04 myurov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_SHPB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER NTID,btype/0/
c_end_var
c
c  executable code for routine BOOK_PART_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for reconstructed particles.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      if(maxparts_shpb.GT.54) 
     &  call recmes(crname,'W','Mismatch of max.hits in ntuple')
      call hbname(NTID,'shpb',shpart,
     +'shpart[0,54]:I,shid(shpart):I,shx(shpart):R,'//
     +'shy(shpart):R,shz(shpart):R,shnphe(shpart):R,'// 
     +'shtime(shpart):R,shstat(shpart):I')  
      RETURN
      END
c
c------------------------------------------------------------------------------





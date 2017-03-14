      SUBROUTINE BOOK_MC_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_mc_nt.F,v 1.5 2009/03/14 17:31:56 fklein Exp $
c
c  Documentation for subroutine BOOK_MC_NT
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
c  Author:         Created:  Tue Mar  4 16:46:29 EST 1997
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
#include "mc_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_mc_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/14 17:31:56 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: book_mc_nt.F,v 1.5 2009/03/14 17:31:56 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_MC_NT')
      PARAMETER (CRAUTH='whoistheauthor')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,NTID
c_end_var
c
c  executable code for routine BOOK_MC_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Bookoing N-tuple block MC'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      CALL hbname(NTID,'mcevt',mcnentr,
     +'mcnentr[0,40]:I,mcnpart[0,40]:I,mcst(mcnentr):I,'//
     +'mcid(mcnentr):I,mcpid(mcnentr):I,'//
     +'mctheta(mcnentr):R,mcphi(mcnentr):R,'//
     +'mcp(mcnentr):R,mcm(mcnentr):R,mcvx(mcnentr):R,mcvy(mcnentr):R,'//
     +'mcvz(mcnentr):R,mctof(mcnentr):R')
      RETURN
      END
c
c------------------------------------------------------------------------------





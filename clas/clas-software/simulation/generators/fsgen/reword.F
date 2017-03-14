      SUBROUTINE REWORD(CLINE,NWORDS,CWORDS)
c
c_begin_doc
c  RCS ID string
c  $Id: reword.F,v 1.1.1.1 2006/04/18 21:08:11 stepanya Exp $
c
c  Documentation for subroutine REWORD
c
c  Purpose: Breaks string CLINE into WORDS delineated by spaces
c  --------
c
c  Input Parameters:  CLINE : C*(*) :  input string
c  ----------------
c
c  Output Parameters:  NWORDS : I : Number of words found
c  -----------------   CWORDS : C*20(40) : Words
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Mon Oct 23 15:24:37 EDT 1995
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: reword.F,v 1.1.1.1 2006/04/18 21:08:11 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REWORD')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer maxwords
      parameter (maxwords = 40)
      INTEGER INDEXC, ISCAN, LENOCC
      EXTERNAL INDEXC, ISCAN, LENOCC
c
      CHARACTER*(*) CLINE
      CHARACTER*20 CWORDS(maxwords)
      INTEGER NWORDS, IBEG, IEND_LINE, IEND
c_end_var
c
c  executable code for routine REWORD:
c----6----------------------------------------------------------------72
c
      NWORDS = 0
      IBEG = 1
      IEND_LINE = LENOCC(CLINE)
10    CONTINUE
c
c  find first character
c
      IBEG = IBEG + INDEXC(CLINE(IBEG:IEND_LINE),' ') - 1
      IF (IBEG .EQ. 0) RETURN
      IEND = IBEG + ISCAN(CLINE(IBEG:IEND_LINE),' ') - 1
      IF (IEND .LT. IBEG) IEND = IEND_LINE
      NWORDS = NWORDS + 1
      IF (NWORDS .GT. maxwords) THEN
       write(*,*)nwords, cline(1:iend)
       write(*,*)nwords, cline(iend+1:iend_line)
       RETURN
      ENDIF
      IF (IEND .EQ. 0) THEN
        RETURN
      ENDIF
      CWORDS(NWORDS) = CLINE(IBEG:IEND)
      IBEG = IEND+1
      IEND = IEND_LINE
      IF (IBEG .GT. IEND_LINE) RETURN
      IF (INDEXC(CLINE(IBEG:IEND),' ') .NE. 0) GOTO 10
      RETURN
      END
c
c------------------------------------------------------------------------------





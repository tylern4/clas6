      SUBROUTINE RETCL_MES(CPARM, ILENGTH)
c
c_begin_doc
c  RCS ID string
c  $Id: retcl_mes.F,v 1.1.1.1 1997/03/28 20:58:32 marki Exp $
c
c  Documentation for subroutine RETCL_MES
c
c  Purpose:  keeps a character string buffer of messages that is accesible
c  --------  to TCL 
c
c  Input Parameters:  CPARM    : character*(*) : message
c  ----------------   ILENGTH : INTEGER     : LENGTH OF STRING
c
c  Output Parameters: none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Jun 21 13:28:19 EDT 1996
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
#include "rtclmes.inc"
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
      PARAMETER (CFILE = '$RCSfile: retcl_mes.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:32 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: retcl_mes.F,v 1.1.1.1 1997/03/28 20:58:32 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RETCL_MES')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LSTRING, ITCL_STRING, LENOCC, words, indexb
      EXTERNAL LENOCC, words, indexb
      PARAMETER (LSTRING = 10000)
      PARAMETER (ITCL_STRING = 4)

      CHARACTER*256 CTCL,CPARM
      CHARACTER*10000 subword, word, ctemp
      external subword, word
      CHARACTER*1 CTERM

      INTEGER ILENGTH, IEND, ITERM, IWARN, LTEMP, ISTART, nwords

      DATA IEND,IWARN,ISTART/1,1,1/
      DATA CTERM,ITERM/'`',1/
c_end_var
c
c  executable code for routine RETCL_MES:
c----6----------------------------------------------------------------72
c
c  determine the end of the buffer....ctcl_buff is reset to via TCL

       iend =  indexb(ctcl_buff,cterm) + 1
       CTCL =  CPARM(1:ilength)

c
      IF (IEND + ILENGTH .LT. LSTRING-ITERM ) THEN
       CTCL_BUFF(IEND:IEND+ILENGTH-1) = CTCL(1:ilength)
       CTCL_BUFF(IEND+ILENGTH:IEND+ILENGTH-1+ITERM) = CTERM
      ELSE

c  this block of code is meant to scroll the oldest messages out of the buffer
c  and keep the most recent messages.
       
c  define the word separator/terminator

         call wordsep(cterm)

 10      continue

c  remove first message and recalculate the length, only remove messages
c  that are informational  (I:) or do not contain a level flag (no :).
         nwords = words(ctcl_buff(1:iend-1))
         ctemp = word(ctcl_buff,iwarn)
         LTEMP = LENOCC(CTEMP)

         IF ( indexb(ctemp(1:ltemp),'W:') .eq. 0 .and.
     1        indexb(ctemp(1:ltemp),'E:') .eq. 0 .and.
     1        indexb(ctemp(1:ltemp),'F:') .eq. 0 .and.
     1        indexb(ctemp(1:ltemp),'L:') .eq. 0) THEN

          ctemp(1:) = subword(ctcl_buff(1:iend),
     1                     IWARN+1,nwords-iwarn)

          ctcl_buff(istart:)=ctemp(1:)

          iend = indexb(ctcl_buff,cterm) + 1
          IF (IEND + ILENGTH .GT. LSTRING-ITERM ) goto 10

          CTCL_BUFF(IEND:) = CTCL
          CTCL_BUFF(IEND+ILENGTH:IEND+ILENGTH+ITERM-1) = CTERM

         ELSE
          IWARN = IWARN + 1
          ISTART = ISTART + LTEMP + 2
          GOTO 10
         ENDIF
      ENDIF
      IEND = IEND+ILENGTH+1
      CTCL = ' '
      RETURN
      END
c
c------------------------------------------------------------------------------




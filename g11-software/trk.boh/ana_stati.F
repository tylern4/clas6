
C======================================================================
      SUBROUTINE ana_stati(icut,strg,value)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize cut tallies.
C-
C-
C-   Inputs  : icut       - cut type
C-           : strg       - cut description
C-           : value      - cut value
C-   Outputs :
C-
C-   Controls:
C-
C-
C-   Calls              : none
C-
C-   Created  JUL-20-1992  Billy Leung
C-            Nov.1997     F.Klein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER icut
      CHARACTER *(*) strg
      REAL  value
*
*  Include Files
*  -------------
#include "trktcl_var.inc"
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_stati.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/11/22 21:24:50 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: ana_stati.F,v 1.2 1997/11/22 21:24:50 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_STATI')
      PARAMETER (CRAUTH='Franz Klein')
C
      INTEGER type, dtype
      REAL    xcut
      LOGICAL ack
      CHARACTER*30 caption
C
      COMMON/ErrBook/ type(10), dtype(10), xcut(10), ack(10), caption(10)
C
C
C Local variables
      INTEGER ifirst, i
      DATA ifirst /1/
C
C
      IF (ifirst .EQ. 1) THEN
        ifirst = 0
        DO i = 1,10
          ack(i)  = .FALSE.
          type(i) = 0
        ENDDO 
      ENDIF
C
      IF (icut.GE.1 .AND. icut.LE.10) THEN
        ack(icut) = .TRUE.
        caption(icut) = strg
        xcut(icut) = value
      ENDIF
C
      RETURN
      END

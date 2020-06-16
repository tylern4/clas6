
C======================================================================
      SUBROUTINE ana_statb(icut)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : keep tallies on different types of
C-                         analysis cuts
C-
C-
C-   Inputs  : icut       - cut type
C-
C-   Outputs :
C-
C-   Controls:
C-
C-   Calls              : none
C-
C-   Created  JUL-20-1992  Billy Leung
C-   mod.     AUG-1997     F.Klein  (similar to SDA '97)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER  icut
*
*  Include Files
*  -------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_statb.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/11/22 21:23:39 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: ana_statb.F,v 1.3 1997/11/22 21:23:39 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_STATB')
      PARAMETER (CRAUTH='Franz Klein')
C
      INTEGER TYPE, dtype
      REAL xcut
      LOGICAL ack
      CHARACTER*30 caption
C
      COMMON/ErrBook/ type(10), dtype(10), xcut(10), ack(10), caption(10)
C

      IF( ack(icut) ) THEN
	dtype(icut)=1
        TYPE(icut) = TYPE(icut) + 1
      ENDIF

      END

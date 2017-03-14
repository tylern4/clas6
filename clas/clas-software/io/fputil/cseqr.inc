c
c_begin_doc
c  RCS ID string
c  $Id: cseqr.inc,v 1.1.1.1 1997/03/28 20:55:41 marki Exp $
c
c  Purpose:
c  --------
C      some COMMONs for FSEQR etc
c
c  Author:
c  -------
c     Volker Blobel          xxx Apr 15 15:00:00 EDT 1994
c
c  Major revisions:
c  ----------------
c     Serguei Boiarinov      Created:  Thu Oct 12 12:23:13 EDT 1995
c
c_end_doc
c_begin_var
c  variables for include file:
c  ---------------------------
c  RCS information:
      CHARACTER*(132)  CICSEQR
      DATA CICSEQR/
     1'$rcsId$'
     2/
c_end_var
      INTEGER NREC,ISPECL,KEYNR1,KEYNR2,NDATWD,LUNW,LUNT,NLUNW,LIMSEC,
     &        LIMREC
      REAL    TBEGIN
      LOGICAL EOD,USTOP,URSKP
      CHARACTER*8 NAMKEY
      COMMON/CSEQRN/EOD,USTOP,URSKP,NREC,ISPECL,
     +  KEYNR1,KEYNR2,NDATWD,LUNW(4),LUNT(4),NLUNW,LIMSEC,LIMREC,TBEGIN
      SAVE  /CSEQRN/
      COMMON/CSEQRC/NAMKEY
      SAVE  /CSEQRC/

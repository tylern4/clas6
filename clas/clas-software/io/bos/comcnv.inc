c
c_begin_doc
c  RCS ID string
c  $Id: comcnv.inc,v 1.1.1.1 1997/03/28 20:49:36 marki Exp $
c
c  Purpose:
c  --------
C      Common block for Card Image Input
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
      CHARACTER*(132)  CICOMCNV
      DATA CICOMCNV/
     1'$rcsId$'
     2/
c_end_var
      INTEGER LABEL,ISL,NT,NFR,LFR
      COMMON/COMCNV/LABEL,ISL,NT,NFR(128),LFR(128)



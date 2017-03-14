c
c_begin_doc
c  RCS ID string
c  $Id: cwkfto.inc,v 1.1.1.1 1997/03/28 20:55:41 marki Exp $
c
c  Purpose:
c  --------
C      workbank pointers for COMPRESSION stuff
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
      CHARACTER*(132)  CICWKFTO
      DATA CICWKFTO/
     1'$rcsId$'
     2/
c_end_var
*  stores workbank pointer
      INTEGER INDWO,INDWO2,INDWO3
      COMMON/CWKFTO/INDWO,INDWO2,INDWO3


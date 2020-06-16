c
c_begin_doc
c  RCS ID string
c  $Id: bosmdl.inc,v 1.1.1.1 1997/03/28 20:55:40 marki Exp $
c
c  Purpose:
c  --------
C      COMMON/BOSMDL/ for connection between FSEQST and FSEQR
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
      CHARACTER*(132)  CIBOSMDL
      DATA CIBOSMDL/
     1'$rcsId$'
     2/
c_end_var
      INTEGER       LCCRUN,NCCRUN,NEVENT,
     +              IHA,IBS,IDB,IDATEL,LUP,ISN,JSN
      LOGICAL       BEGJOB,ENDRUN,BEGRUN,REVENT,ENDJOB,OTHDAT
      COMMON/BOSMDL/BEGJOB,ENDRUN,BEGRUN,REVENT,ENDJOB,OTHDAT,
     +              LCCRUN,NCCRUN,NEVENT,
     +              IHA,IBS,IDB,IDATEL,LUP,ISN,JSN
      SAVE  /BOSMDL/

      SUBROUTINE REVINI(PNAME,rcsVERSION,PDATE)
c
c_begin_doc
c  RCS ID string
c  $Id: revini.F,v 1.3 1998/05/05 19:21:43 marki Exp $
c
c  Documentation for subroutine REVINI
c
c  Purpose: Stores the names and RCS versions of intialized packages.  This
c  -------- routine is to be call at initialization phase of ALL packages
c           For example the following call:
c
c             CALL REVINI('DCCL',CREVIS,CDATE)
c
c           will store the fact that DCCL (drift chamber calibration) package
c           RCS version /CREVIS was used on this data.  CREVIS /'$Revision'
c           is expanded by RCS and used at execution time.
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (PNAME - A* - Package name, only first 
c                                   four characters stored)
c  ----------------   (rcsVERSION - A* - rcs version)
c                     (PDATE - A* - rcs date for the package)
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Apr 11 10:00:03 EDT 1995
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
#include "reversion.inc"
#include "clasmdl.inc"
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
      PARAMETER ( CFILE = '$RCSfile: revini.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/05/05 19:21:43 $')
      PARAMETER ( CAUTHO = '$Author: marki $')
      DATA CRCSID/
     1'$Id: revini.F,v 1.3 1998/05/05 19:21:43 marki Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REVINI')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*(*) PNAME, rcsVERSION, PDATE
      INTEGER CEND, LENOCC, IBEG, IEND, I
      EXTERNAL LENOCC
c_end_var
c
c  executable code for routine REVINI:
c  -------------------------------------
c
c skip if you're running the online version of recsis
c and you are in the midst of a run number change. all the
c packages have been registered at this point.
c 
      NPACKAGES= NPACKAGES + 1
      CPACK(NPACKAGES) = PNAME(1:4)
      CEND = LENOCC(rcsVERSION)
      IBEG = 0
      DO 20 I=1,CEND
         IF (IBEG .EQ. 0 .AND. rcsVERSION(I:I) .EQ. ':') IBEG = I+1 
         IF (rcsVERSION(I:I) .EQ. '$') IEND = I-1 
 20   CONTINUE
      VPACK(NPACKAGES) = rcsVERSION(IBEG:MIN(IBEG+3,IEND))
      CEND = LENOCC(PDATE)
      IBEG = 0
      DO 30 I=1,CEND
         IF (IBEG .EQ. 0 .AND. PDATE(I:I) .EQ. ':') IBEG = I+1 
         IF (PDATE(I:I) .EQ. '$') IEND = I-1 
 30   CONTINUE
      DPACK(NPACKAGES) = PDATE(IBEG:MIN(IBEG+20,IEND))
      RETURN
      END
c     
c------------------------------------------------------------------------------

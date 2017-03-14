      SUBROUTINE REDIT(BNAME,NROW,NCOL,NREC,ISW,VALUE)
c
c_begin_doc
c  RCS ID string
c  $Id: redit.F,v 1.2 1999/07/03 02:15:14 boiarino Exp $
c
c  Documentation for subroutine REDIT
c
c  Purpose:  Allow the user to interactively EDIT a BOS bank
c  --------
c
c  Input Parameters:  BNAME: C4 : the BANK name
c  ----------------   NROW : I  : the row in the bank
c                     NCOL : I  : the column in the bank
C                     NREC : I  : record/sector number
c                     ISW  : I  : 1 = IW array, 2=JW array
c                    VALUE : R  : the new value
C
c  Output Parameters:  none
c  -----------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Mon Sep  9 21:14:52 EDT 1996
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
#include "bcs.inc"
#include "wcs.inc"
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
      PARAMETER (CFILE = '$RCSfile: redit.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/07/03 02:15:14 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: redit.F,v 1.2 1999/07/03 02:15:14 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REDIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MLINK
      EXTERNAL MLINK
c
      CHARACTER*4 BNAME
      CHARACTER*1 CONE
      Character*256 CFMT
      CHARACTER*8 CDISC(100)
      INTEGER NCOL, NROW, NREC, NCOL_TOT, ISTART, IDEX, ISW
      REAL VALUE
      LOGICAL LOK
c_end_var
c
c  executable code for routine REDIT:
c----6----------------------------------------------------------------72
c
      CALL BNKTST(BNAME, LOK, NCOL_TOT, CFMT, CDISC)

       IF (.NOT. LOK) THEN
        WRITE(CRMESS,10)BNAME
 10     FORMAT('Bank ',a4,' NOT formatted via ddl file!!')
        CALL RECMES(CRNAME,'W',CRMESS)
        RETURN
       ENDIF
        if (isw .ne. 2) then
         Istart = mlink(iw,bname,nrec)

         if (istart .eq. 0) then
           write(crmess,14)bname
 14        format('Sorry no ',a4,' bank found in this event')
           call recmes(crname,'i',crmess)
           return
         else

c  get the format for the NCOL column
          IF (CFMT(1:3) .EQ. 'B16') THEN
           IDEX = 2*ISTART
           IW16(idex+(nrow-1)*ncol_tot+ncol) = INT(VALUE)
          ELSEIF (CFMT(1:3) .EQ. 'B32') THEN
           IDEX = ISTART
           IW(idex+(nrow-1)*ncol_tot+ncol) = INT(VALUE)
          ELSE
           IDEX = ISTART
           CONE = CFMT((NCOL-1)*2+1:(NCOL-1)*2+1)
           IF (CONE .EQ. 'I') THEN
            IW(idex+(nrow-1)*ncol_tot+ncol) = INT(VALUE)
           ELSEIF (CONE .EQ. 'F') THEN
            RW(idex+(nrow-1)*ncol_tot+ncol) = VALUE
           ENDIF
          ENDIF
         ENDIF

        ELSE
c  this is for the JW working arrays

         Istart = Mlink(JW,bname,nrec)

         if (istart .eq. 0) then
           write(crmess,14)bname
           call recmes(crname,'i',crmess)
           return
         else

c  get the format for the NCOL column
          IF (CFMT(1:3) .EQ. 'B16') THEN
           IDEX = 2*ISTART
           JW16(idex+(nrow-1)*ncol_tot+ncol) = INT(VALUE)
          ELSEIF (CFMT(1:3) .EQ. 'B32') THEN
           IDEX = ISTART
           JW(idex+(nrow-1)*ncol_tot+ncol) = INT(VALUE)
          ELSE
           IDEX = ISTART
           CONE = CFMT((NCOL-1)*2+1:(NCOL-1)*2+1)
           IF (CONE .EQ. 'I') THEN
            JW(idex+(nrow-1)*ncol_tot+ncol) = INT(VALUE)
           ELSEIF (CONE .EQ. 'F') THEN
            RJW(idex+(nrow-1)*ncol_tot+ncol) = VALUE
           ENDIF
          ENDIF
         ENDIF
        ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------






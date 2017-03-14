      SUBROUTINE ETTA
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine ETTA
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Mon Dec 16 10:20:22 EST 1996
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
#include "ettapar.inc"
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
      PARAMETER (CFILE = '$RCSfile$')
      PARAMETER (CREVIS = '$Revision$')
      PARAMETER (CSTATE = '$State$')
      PARAMETER (CDATE = '$Date$')
      PARAMETER (CAUTHO = '$Author$')
      DATA CRCSID/   
     1'$Id$'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ETTA')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ETampl(6)
      INTEGER ETisot(3)
      INTEGER i,k,m 
c_end_var
c
c  executable code for routine ETTA:
c----6----------------------------------------------------------------72
c
c      IF (NWRITE .LT. 1) THEN
c        NWRITE = NWRITE + 1
c        CRMESS='This is a DUMMY routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
c      ENDIF
c
c- Definition of coefficents that denote the symmetry of Invariant 
c- Amplitudes under the interchange "s<==>u". 
c
      ETampl(1) = 1
      ETampl(2) = 1
      ETampl(6) = 1
      do i=3,5
         ETampl(i) = -1
      end do
c
c- Coefficients which determine the symmetry of Isospin Amplitudes 
c- (+),(-),(0) under the interchange "s<==>u".
c
      ETisot(1) = 1
      ETisot(2) = -1
      ETisot(3) = 1
c
c Products of two symmetry coefficients.
c
      DO k=1,6
        do m=1,3
           ett(k,m) = ETampl(k)*ETisot(m)
        enddo
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------





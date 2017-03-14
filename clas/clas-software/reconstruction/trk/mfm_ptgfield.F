      SUBROUTINE mfm_ptgfield(R,Z,BR,BZ)
c
c_begin_doc
c  RCS ID string
c  $Id: mfm_ptgfield.F,v 1.1 1999/02/16 16:50:44 kossov Exp $
c
c  Documentation for subroutine MFM_PTGFIELD
c
c  Purpose: Calculation of the new magnetic field for the polarized target
c  --------
c
c  Calling Sequence: Current is proportional to cross section + call MFM_DUSP
c  ----------------
c
c  Input Parameters:  R - radius of the point, Z - coordinate along the beam 
c  ----------------
c
c  Output Parameters: BR - radial coordinate, BZ - longitudinal coordinate
c  -----------------
c
c  Called from: MFM_FIELD
c  ------------
c
c  Other routines: NONE
c  ---------------
c
c  Notes: MFM_DUSP is more general (trapecoidal) then needed (rectangular)
c  ------
c
c  Author:   Mikhail Kossov      Created:  Mon May 12 14:01:12 EST 1997
c  -------
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
c#include ""
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: mfm_ptgfield.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/02/16 16:50:44 $')
      PARAMETER (CAUTHO= '$Author: kossov $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='mfm_ptgfield')
      PARAMETER (CRAUTH='Mikhail Kossov')
c
c  Local User defined variables:
c  -----------------------------
c
      INTEGER NCL,IFIRST
C      PARAMETER (NCL = 4)
      PARAMETER (NCL = 6)
      REAL R,Z,BR,BZ,RI(NCL),ZI(NCL),ZO(NCL),RO(NCL), D, RJ(NCL)
C- RI (NCL) - internal radius
C- RO (NCL) - external radius
C- ZI (NCL) - start of the coil in Z
C- ZO (NCL) - end of the coil in Z
C- CUR(NCL) - current in the coil
c      DATA RI/ 16.5, 11.7, 11.7, 16.5 /
c      DATA RO/ 21.9, 15.1, 15.1, 21.9 /
c      DATA ZI/-12.3, -8.1,  5.2,  7.6 /
c      DATA ZO/ -7.6, -5.2,  8.1, 12.3 /
      DATA RI/ 16.6, 13.41, 11.73, 11.73, 13.41,  16.6 /
      DATA RO/ 22.0, 15.14, 13.37, 13.37, 15.14,  22.0 /
      DATA ZI/-12.2, -8.20, -8.20,  5.20,  5.20,   7.5 /
      DATA ZO/ -7.5, -5.20, -5.20,  8.20,  8.20,  12.2 /
C
      DATA RJ/6239., 1288.,  864.,  864., 1288., 6239. /
C
      DATA IFIRST/0/
c
c_end_var
c
c  executable code for routine MFM_PTGFIELD:
c  -----------------------------------------------------------------------
c
      IF (IFIRST .EQ. 0) THEN
        WRITE(CRMESS,*)'First call r=',R,', z=',Z
        CALL RECMES(CRNAME,'I',CRMESS)
        IFIRST = 1
c        DO i=1,NCL ! calculate currents to be proportional to cross sections
c          RJ(i) = ABS(RO(i)-RI(i)) * ABS(ZO(i)-ZI(i))
c        ENDDO
      ENDIF
c
      CALL MFM_DUSP(NCL,R,Z,RJ,RI,ZI,ZO,RO,ZI,ZO,1.,BR,BZ,D)
C
      RETURN
      END

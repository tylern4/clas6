CDECK  ID>, SORTBL.
      SUBROUTINE SORTBL(A,IDX,N)
c
c_begin_doc
c  RCS ID string
c  $Id: sortbl.F,v 1.1.1.1 1997/03/28 20:49:41 marki Exp $
c
c  Purpose:
c  --------
c     Sort elements in ascending order (integer elements)
c
c        define indices IDX(1)...IDX(N)
c        CALL SORTBL(A,IDX,N)
c
c     At return the index values are exchanged and corresponds to the
c     ordering
c              A(IDX(1)) =< A(IDX(2)) =< ... =< A(IDX(N))
c     leaving the array A(*) unchanged. If N is given negative, the
c     indices are defined internally as in example below.
c
c     Example: Code to sort an array A(1)...A(N)
c           DO 10 I=1,N
c        10 IDX(I)=I
c           CALL SORTBL(A,IDX,N)
c     or
c           CALL SORTBL(A,IDX,-N)
c
c
c  Author:
c  -------
c     Volker Blobel          xxx Apr 15 15:00:00 EDT 1994
c
c  Major revisions:
c  ----------------
c     Serguei Boiarinov      Thu Oct 12 11:52:54 EDT 1995
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER A(*),IDX(*),N
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information:
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/
     1'$Id: sortbl.F,v 1.1.1.1 1997/03/28 20:49:41 marki Exp $'
     2/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRAUTH='Serguei Boiarinov')
      PARAMETER (CRNAME='SORTBL.F')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER J,L,M,LL
c_end_var
c----6----------------------------------------------------------------72
      IF(N.LT.0) THEN
*     define sequential pointer for negative N
         DO 10 J=1,-N
   10    IDX(J)=J
      END IF
*     determine initial increment
      M=4
      IF(IABS(N).GT.13) THEN
   20    M=3*M+1
         IF(M.LT.IABS(N)) GOTO 20
         M=(M-1)/3
      END IF
   30 M=(M-1)/3
*     sort with increment M
      DO 50 J=1,IABS(N)-M
      L=J
*     compare elements with indices IDX(L) and IDX(L+M)
   40 IF(A(IDX(L+M)).LT.A(IDX(L))) THEN
*        exchange the two index values
         LL=IDX(L+M)
         IDX(L+M)=IDX(L)
         IDX(L)=LL
         L=L-M
         IF(L.GT.0) GOTO 40
      END IF
   50 CONTINUE
      IF(M.GT.1) GOTO 30
  100 RETURN
      END

      INTEGER FUNCTION PTRACB(V,P,PQ,VLIM1,VLIM2,STEP,Z,ZPLANE,ARC)
C   THIS IS C-INTERFACE TO TRACB FOR PLANES
C
C
      include 'trac1.inc'
      include 'trac2.inc'
      include 'trac3.inc'
C
C
      REAL*4 V(3),P(3),PQ,VLIM1(3),VLIM2(3),STEP,Z,ZPLANE(3)
      REAL*4 ARC
C
      ALIM(1,1) = VLIM1(1)
      ALIM(2,1) = VLIM2(1)
      ALIM(1,2) = VLIM1(2)
      ALIM(2,2) = VLIM2(2)
      ALIM(1,3) = VLIM1(3)
      ALIM(2,3) = VLIM2(3)
      IEXTN = 0
      ICHK = 0
      ZTN = Z
      DO 10 K = 1,3
10    ZT(K) = ZPLANE(K)
      CALL MOVLEV(V,A,3)
      CALL MOVLEV(P,A(4),3)
      A(7) = ARC
      A(8) = PQ
      CALL TRACB(1,STEP,IFAIL)
C      IF (IFAIL .EQ. 0) THEN
        CALL MOVLEV(A,V,3)
        CALL MOVLEV(A(4),P,3)
C      ENDIF
      PTRACB = IFAIL
      RETURN
      END




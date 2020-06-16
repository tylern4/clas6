      FUNCTION CFGAM1(A,X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  returns the incomplete gamma function
C
C                               x
C                               /
C                         1     |  -t   a-1
C         P(a,x) =    --------  | e    t    dt
C                     gamma(a)  /
C                               0
C
C  From "Numerical Recipes" Press,Flannery,Teukolsky,Vetterling
C        Cambridge
C-   Inputs  : A, X
C-   Created  12-AUG-1988   A. Bay
C-   Original function name  :  GAMDF1
c    Modified:
c              2-Jul-98  David J. Tedeschi
C----------------------------------------------------------------------
      REAL    CFGAM1,A,X
      REAL    GAMMA,LOGGAM
      REAL    AP,DEL,SUM
      REAL    P,GG,A0,A1,B0,B1,FAC,AN,ANA,ANF,GOLD
      INTEGER N
      INTEGER ITMAX
      PARAMETER( ITMAX =1000 )
      REAL    EPS
      PARAMETER( EPS =3.E-7  )
C
      IF ( X.LT.0 .OR. A.LE.0 ) THEN
        CFGAM1 = -99999.
        RETURN
      ENDIF
C
      LOGGAM = LOG(GAMMA(A))
C
      IF ( X.LT.A+1. ) THEN
C
C- incomplete gamma function, by its series
        IF ( X.LE.0 ) THEN
          IF (X.LT.0)
cdjt
     >     call recmes('CFGAM1','F','X less than 0 in Gamma function')
c     >     CALL SUERR('F','1',1,'CFGAM1',
c     >     'X < 0. in Gamma function')
          CFGAM1 = 0.
        ENDIF
        AP = A
        SUM = 1./A
        DEL = SUM
        DO 10 N = 1,ITMAX
          AP = AP + 1
          DEL = DEL*X/AP
          SUM = SUM + DEL
          IF (ABS(DEL).LT.ABS(SUM)*EPS) GOTO 1
   10   CONTINUE
cdjt
        call recmes('CFGAM1','F', 'A too large in GSER')
c        CALL SUERR('F','1',1,'CFGAM1',
c     >       'A too large in GSER')
    1   CFGAM1 = SUM*EXP(-X + A*LOG(X) - LOGGAM)
C
      ELSE
C- Incomplete Gamma function evaluated by its continued
C-                              fraction representation
        GOLD = 0
        A0 = 1
        A1 = X
        B0 = 0
        B1 = 1
        FAC = 1
        DO 20 N = 1,ITMAX
          AN = FLOAT(N)
          ANA = AN - A
          A0 = (A1 + A0*ANA)*FAC
          B0 = (B1+B0*ANA)*FAC
          ANF = AN*FAC
          A1 = X*A0 + ANF*A1
          B1 = X*B0 + ANF*B1
          IF (A1.NE.0.) THEN
            FAC = 1./A1
            GG = B1*FAC
            IF (ABS((GG-GOLD)/GG).LT.EPS) GOTO 2
            GOLD = GG
          ENDIF
   20   CONTINUE
cdjt
        call recmes('CFGAM1','F', 'A too large in GCF')
c         CALL SUERR('F','1',1,'CFGAM1','A too large in GCF')
    2   P = EXP(-X + A*ALOG(X) - LOGGAM)*GG
        CFGAM1 = 1. - P
C
      ENDIF
C
      END

      SUBROUTINE AKDN(usearatio,dsearatio)
      IMPLICIT NONE
c
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/
**************************************************************
C     Authors:  Aram Kotzinian and Dmitry Naumov
C     PURPOSE: DISTINGUISH BETWEEN VALENCE AND SEA QUARKS FOR 
C     TARGET REMNANT SPLITTING
**************************************************************
C     ...  Reweighting valence and sea U and D quarks ...
C...D.N. 
      REAL usearatio,dsearatio
      REAL xx, scale, pdf(-6:6), dpdf(-6:6)
      DOUBLE PRECISION dqvk,dsea,uqvk,usea
C
      xx=x
      scale=q2
c
      call parton(xx,scale,pdf,dpdf)
      dqvk=pdf(1)
      dsea=pdf(-1)
      uqvk=pdf(2)
      usea=pdf(-2)
      IF(dqvk.EQ.0.) THEN
         IF(x.LT.0.01) dsearatio = 1.
         IF(x.GE.0.01) dsearatio = 0.
      ELSE
         dsearatio = dsea/dqvk
      ENDIF
      IF(uqvk.EQ.0.) THEN
         IF(x.LT.0.01) usearatio = 1.
         IF(x.GE.0.01) usearatio = 0.
      ELSE
         usearatio = usea/uqvk
      ENDIF
      RETURN
      END

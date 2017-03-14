      SUBROUTINE CFCOMD
CA)========================================(begin of short description)
C KUIP action routine for CF package
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by:
C
C    Input arguments :  None
C
C    Input/output arguments :  None
C
C    Output arguments:  None
C
C    Side effects (other variables/banks modified, etc.)
C
C    Created:   20-APR-1994  Serguei Boiarinov
C    Modified:
C
CC)-------------------------------------------(end of long description)
      implicit none
CD)----------------------------------(declare common blocks, variables)
C  Common blocks
#include "cfpara.inc"
#include "cflset.inc"
#include "cftmed.inc"
C  Variables in argument list
C  Save variables
C  Local variables
      INTEGER I,J,K,L
      INTEGER NLEV,NPAR
      CHARACTER*32 CHPATH(10),CHPATL
      CHARACTER*4 VOLN
      REAL SECUT,SECT2,ENOR,SHCUT,SHCT2,HNOR
C  External references
      INTEGER IUCOMP
C  Statement functions
C  Local data statements
CE)===============================================(end of declarations)
C
      CALL KUPATH(CHPATH,NLEV,NPAR)
      CHPATL=CHPATH(NLEV)
C
      IF (CHPATL.EQ.'EM_SHOWER') THEN
       CALL KUGETI(IPARAE)
       RETURN
      END IF
C
      IF (CHPATL.EQ.'HD_SHOWER') THEN
       CALL KUGETI(IPARAH)
       RETURN
      END IF
C
      IF (CHPATL.EQ.'EM_PARAMETER') THEN
       CALL KUGETR(HIECUT)
       CALL KUGETR(HIECT2)
       IF (NPAR.GE.3) CALL KUGETR(SHCUTE)
       IF (NPAR.GE.4) CALL KUGETR(SHSPOE)
       IF (NPAR.GE.5) CALL KUGETR(STEPX0)
       IF (NPAR.GE.6) CALL KUGETR(SCALE)
       IF (NPAR.GE.7) CALL KUGETR(EMUTO)
       IF (NPAR.GE.8) CALL KUGETR(SIGTHE)
       RETURN
      END IF
C
      IF (CHPATL.EQ.'HD_PARAMETER') THEN
       CALL KUGETR(HIHCUT)
       CALL KUGETR(HIHCT2)
       IF (NPAR.GE.3) CALL KUGETR(SHCUTH)
       IF (NPAR.GE.4) CALL KUGETR(SHSPOH)
       IF (NPAR.GE.5) CALL KUGETR(STEPL0)
       IF (NPAR.GE.6) CALL KUGETR(SCALH)
       IF (NPAR.GE.7) CALL KUGETR(PMUTO)
       IF (NPAR.GE.8) CALL KUGETR(SIGTHH)
       RETURN
      END IF
C
      IF (CHPATL.EQ.'EM_RESOLUTION') THEN
       CALL KUGETR(RESO)
       CALL KUGETR(RESOC)
       RETURN
      END IF
C
      IF (CHPATL.EQ.'HD_RESOLUTION') THEN
       CALL KUGETR(RHSO)
       CALL KUGETR(RHSOC)
       RETURN
      END IF
C
      IF (CHPATL.EQ.'SWITCHES') THEN
       CALL KUGETI(I)
       IF (I.EQ.1) THEN
        LTRNE=.TRUE.
       ELSE
        LTRNE=.FALSE.
       END IF
       CALL KUGETI(I)
       IF (I.EQ.1) THEN
        LESPOT=.TRUE.
       ELSE
        LESPOT=.FALSE.
       END IF
       CALL KUGETI(I)
       IF (I.EQ.1) THEN
        LTRNH=.TRUE.
       ELSE
        LTRNH=.FALSE.
       END IF
       CALL KUGETI(I)
       IF (I.EQ.1) THEN
        LHSPOT=.TRUE.
       ELSE
        LHSPOT=.FALSE.
       END IF
       RETURN
      END IF
C
      IF (CHPATL.EQ.'MEDIA') THEN
       IF (NPAR.EQ.0) THEN
        WRITE(*,*)'Tracking media for starting parametrized ',
     &                 'showers'
        IF (NSHT.GT.0) THEN
         WRITE(*,*) (ISHT(I),I=1,NSHT)
        ELSE
         WRITE(*,*)' >>>>>>> NOT defined <<<<<<<'
        END IF
       ELSE
        DO 10 I=1,NPAR
        CALL KUGETI(J)
        K=IUCOMP(IABS(J),ISHT,99)
        IF (J.GT.0) THEN
         IF (K.GT.0) THEN
          WRITE(*,*)'TRACKING MEDIA NUMBER',J,
     &                   ' IS ALREADY DEFINED.'
         ELSE
          NSHT=NSHT+1
          ISHT(NSHT)=J
         END IF
        ELSE IF (J.LT.0) THEN
         IF (K.EQ.0) THEN
          WRITE(*,*)'TRACKING MEDIA NUMBER',J,' IS NOT DEFINED.'
         ELSE
          DO 11 L=K+1,NSHT
          ISHT(L-1)=ISHT(L)
11        CONTINUE
          NSHT=NSHT-1
         END IF
        END IF
10      CONTINUE
       END IF
       RETURN
      END IF
C
      IF (CHPATL.EQ.'VOLUME') THEN
       CALL KUGETC(VOLN,I)
       CALL CFVTMD(VOLN,J)
       IF (J.LE.0) THEN
        WRITE(*,700) VOLN
700     FORMAT(' Volume name ',A4,' is not defined. Command skiped!!!')
        RETURN
       ENDIF
       K=IUCOMP(J,ISHT,99)
       IF (K.LE.0) THEN
        NSHT=NSHT+1
        ISHT(NSHT)=J
        K=NSHT
       ELSE
        WRITE(*,701) J,VOLN
701     FORMAT(' Tracking media <',I5,'> of volume name <',A4,
     >   '> has already defined.',
     >  /' Threshold parameters are overwritten.')
       ENDIF
       SECUT=-1.
       SECT2=-1.
       SHCUT=-1.
       SHCT2=-1.
C
       IF (NPAR.GE.2) CALL KUGETR(SECUT)
       IF (NPAR.GE.3) CALL KUGETR(SECT2)
       IF (NPAR.GE.4) CALL KUGETR(ENOR)
C      
       IF (NPAR.GE.5) CALL KUGETR(SHCUT)
       IF (NPAR.GE.6) CALL KUGETR(SHCT2)
       IF (NPAR.GE.7) CALL KUGETR(HNOR)
C
       SPECUT(K)=SECUT
       SPECT2(K)=SECT2
       ENORM(K) =ENOR
       SPHCUT(K)=SHCUT
       SPHCT2(K)=SHCT2
       HNORM(K) =HNOR
C
       RETURN
      ENDIF
C
      END

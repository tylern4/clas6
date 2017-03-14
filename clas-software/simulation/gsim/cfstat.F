      SUBROUTINE CFSTAT
CA)========================================(begin of short description)
C Initialize CF package 
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
C    Modified:  30-JUN-1996  David Tedeschi
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
#include "cfpid.inc"
      INTEGER I,ISTAT
C  External references
      INTEGER IUCOMP
C  Statement functions
C  Local data statements
CE)===============================================(end of declarations)
C
C Define pseudo-particles ( 6 mean "GEANTINO" ==> s/r GTRACK will
C call GTNINO ) for parametrized shower
c
      CALL GSPART(IDCF1,'ESHO$',6,0.0,0.0,1.E9,0,0)
      CALL GSPART(IDCF2,'HSHO$',6,0.0,0.0,1.E9,0,0)
C
C Set Shower parameters for media on cf list
c see cfblkd.F for values
c
c      CALL SUREAD('CFPARA',0,'KUIP',ISTAT)
      DO I=1,NSHT
        IF (SPECUT(I).LT.0.) SPECUT(I)=HIECUT
        IF (SPECT2(I).LT.0.) SPECT2(I)=HIECT2
        IF (SPHCUT(I).LT.0.) SPHCUT(I)=HIHCUT
        IF (SPHCT2(I).LT.0.) SPHCT2(I)=HIHCT2
      ENDDO
c
c print out parameters
c
      call recmes('CFSTAT','I',' Parameter Listing')
c
c electromagnetic shower parameters
c
      WRITE(*,*)'EM Parametrization function type :',IPARAE
      IF (IABS(IPARAE).GT.2) THEN
        WRITE(*,*)'IPARAE IS NOT DEFINED!!!!',
     &  '   DEFAULT VALUE (=1) ASSUMED.'
        IPARAE=1
      END IF
      IF (IPARAE.NE.0) THEN
        WRITE(*,*)'Transverse EM Shower development : ',LTRNE
        WRITE(*,*)'num spots prop to EM energy : ',LESPOT
        WRITE(*,*)'EM cal. Resolution : RESO=',RESO,' RESOC=',
     &             RESOC
        IF (SCALE.LT.0.) SCALE=1.0
        IF (IABS(IPARAE).EQ.2) THEN
CBOY        IF (EMUTO.LT.0.) EMUTO=1.4
        IF (EMUTO.LT.0.) EMUTO=1.0
       ELSE IF (IABS(IPARAE).EQ.1) THEN
        IF (EMUTO.LT.0.) EMUTO=1.0
       END IF
       IF (SIGTHE.LT.0.) SIGTHE=0.6
      ELSE
        call recmes('CFSTAT','I','No Electromagnetic Parameterization')
      END IF
c
c hadronic shower parameters
c
      WRITE(*,*)'HAD Parametrization function type :',
     & IPARAH
      IF (IABS(IPARAH).GT.2) THEN
       WRITE(*,*)'IPARAH IS NOT DEFINED!!!!',
     &  '   DEFAULT VALUE (=1) ASSUMED.'
       IPARAH=1
      END IF
      IF (IPARAH.NE.0) THEN
       WRITE(*,*)'Transverse Had Shower development : ',LTRNH
       WRITE(*,*)'num spots prop to HAD energy : ',LHSPOT
       WRITE(*,*)'HD cal. Resolution : RHSO=',RHSO,'RHSOC=',
     &  RHSOC
       IF (IABS(IPARAH).EQ.2) THEN
        IF (SCALH.LT.0.) SCALH=0.5
       ELSE IF (IABS(IPARAH).EQ.1) THEN
        IF (SCALH.LT.0.) SCALH=0.4634
       END IF
       IF (PMUTO.LT.0.) PMUTO=1.0
       IF (SIGTHH.LT.0.) SIGTHH=0.2
      ELSE
        call recmes('CFSTAT','I','No Hadronic Parameterization')
      END IF
c
c media defined for parameterization
c
      NSHT=IUCOMP(-1,ISHT,99)-1
      IF (NSHT.GT.0) THEN
        call recmes('CFSTAT','I',
     &           'Tracking media for starting parametrized showers')
        WRITE(*,*) (ISHT(I),I=1,NSHT)
      ELSE
        call recmes('CFSTAT','W',
     & 'Tracking media for starting parametrized showers >>>> NOT defined <<<<')
      END IF
C
      call recmes('CFSTAT','I','End of parameter listing')
      RETURN
      END






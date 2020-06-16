      SUBROUTINE CFKING
CA)========================================(begin of short description)
C Store kinematics of shower particles
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by: clas_step
C
C    Input arguments :  None
C
C    Input/output arguments :  None
C
C    Output arguments:  cf_inuse - flag to say whether cf used or not
C
C    Side effects (other variables/banks modified, etc.)
C       COMMON /GCKING/ is modified
C
C    Created:   20-APR-1994  Serguei Boiarinov
C    Modified:  29-JUN-1998  David Tedeschi
C
C-------------------------------------------(end of long description)
      implicit none
c
#include "gctrak.inc"
#include "gcking.inc"
#include "gckine.inc"
#include "gctmed.inc"
c
#include "cfpara.inc"
#include "cftmed.inc"
#include "cfpid.inc"

      REAL P, T, P2, M2, ID
      LOGICAL LEM,LHD,LSH,LSTP
      INTEGER I,KT

      character*4 namechar(30)
      equivalence (namec,namechar)

      INTEGER IUCOMP                 ! cernlib compare routine
      EXTERNAL IUCOMP
c
c----------------------------------------------------------------------
c
c      call recmes('CFKING','I','CFKING reached')  
c
c set logical control flags
c lsh - shower param if true
c lstp - stop tracking is true
c
      LSH=.FALSE.
      LSTP=.FALSE.
C
C CHOOSE NORMAL GHEISHA GENERATION
c reasons:  control flags not set
c           no media on list
c     
c           em particlem but em param not set
c           had particle but had param not set
c           particle that is not covered by this scheme
c
      IF (IPARAE.EQ.0 .AND. IPARAH.EQ.0) GO TO 500
      KT=IUCOMP(NUMED,ISHT,NSHT)
      IF (KT.LE.0) GO TO 500
      IF (ITRTYP.GE.5) GO TO 500
c
      LEM=(IPART.LE.3 .OR. IPART.EQ.7)
      LHD=IPART.GT.7.AND.IPART.LT.48
      IF (LEM .AND. IPARAE.EQ.0) GO TO 500
      IF (LHD .AND. IPARAH.EQ.0) GO TO 500
      IF ((.NOT.LEM) .AND. (.NOT.LHD)) GO TO 500
c
c CHOOSE PRIMARY PARTICLE TO PARAMETRIZE
c
c  gekin (from /gctrak/) - kinetic energy  
c
c      write(6,*) 'CFKING -1- if gekin lt spect2 goto 700',gekin,spect2(kt)
      IF (LEM .AND. GEKIN.LT.SPECT2(KT)) GO TO 700
      IF (LHD .AND. GEKIN.LT.SPHCT2(KT)) GO TO 700
c
c ELSE HYBRID PARAMETRIZATION
c
c First do secondaries'
c ngkine,gkin,iflgk (from /gcking/) 
c vect(1,2,3) - x,y,z position (from /gctrak/)
c vect(4,5,6) - direction cosines
c vect(7) - total energy
c
c      write(6,*) 'CFKING -2- else loop over ngkine secondaries',ngkine
      DO I=1,NGKINE
        P2=GKIN(1,I)*GKIN(1,I)+GKIN(2,I)*GKIN(2,I)+GKIN(3,I)*GKIN(3,I)
        P = SQRT(P2)
        M2 =  GKIN(4,I)*GKIN(4,I) - P2
        IF( M2 .GE. 0.)THEN
          T = GKIN(4,I) - SQRT(M2)          ! kinetic energy
         ELSE
          T = GKIN(4,I)                   
        ENDIF
        IF( T.LE.0.) THEN
          IFLGK(I)=-1
          GOTO 10
        ENDIF
        IF (T.LT.SPECUT(KT) .AND. GKIN(5,I).LE.3.) THEN
          GKIN(1,I)=T*GKIN(1,I)/P
          GKIN(2,I)=T*GKIN(2,I)/P
          GKIN(3,I)=T*GKIN(3,I)/P
          GKIN(4,I)=T
          GKIN(5,I)=IDCF1
c-- put position as well - don't need here because this secondaries
c-- were born by standard way (!?)
*          GPOS(1,NGKINE)=VECT(1)
*          GPOS(2,NGKINE)=VECT(2)
*          GPOS(3,NGKINE)=VECT(3)
c
          IFLGK(I)=0    
          LSH=.TRUE.
         ELSEIF(T.LT.SPHCUT(KT) .AND. GKIN(5,I).GT.7.)THEN
          GKIN(1,I)=T*GKIN(1,I)/P
          GKIN(2,I)=T*GKIN(2,I)/P
          GKIN(3,I)=T*GKIN(3,I)/P
          GKIN(4,I)=T
          GKIN(5,I)=IDCF2
c-- put position as well
*          GPOS(1,NGKINE)=VECT(1)
*          GPOS(2,NGKINE)=VECT(2)
*          GPOS(3,NGKINE)=VECT(3)
          IFLGK(I)=0
          LSH=.TRUE.
        ENDIF
  10    CONTINUE
      ENDDO
c
c Now check primary
c require istop=0 (was istop!=0)
c    
c      write(6,*) 'CFKING -2a- if istop ne 0 and gekin lt specut',istop,gekin,specut(kt)
      IF( LEM .AND. GEKIN.LT.SPECUT(KT)
     &     .AND. ISTOP.EQ.0 .AND. GEKIN.GT.0.) THEN
          ID=IDCF1
      ELSEIF(LHD .AND. GEKIN.LT.SPHCUT(KT)
     &     .AND. ISTOP.EQ.0 .AND. GEKIN.GT.0.) THEN
          ID=IDCF2
      ELSE
          GO TO 500                 ! primary not ready 
      ENDIF
c
c put primary on stack as a geantino so it can be tracked later
c put position as well
c
      NGKINE = NGKINE + 1
      GKIN(1,NGKINE)=VECT(4)*GEKIN
      GKIN(2,NGKINE)=VECT(5)*GEKIN
      GKIN(3,NGKINE)=VECT(6)*GEKIN
      GKIN(4,NGKINE)=GEKIN
      GKIN(5,NGKINE)=ID
      TOFD(NGKINE)=0.
      GPOS(1,NGKINE)=VECT(1)
      GPOS(2,NGKINE)=VECT(2)
      GPOS(3,NGKINE)=VECT(3)
      IFLGK(NGKINE)=0   
c
c set shower flags and stop original particle (set gekin to zero)
c
      LSH=.TRUE.
      LSTP=.TRUE.
      NMEC=NMEC+1
      LMEC(NMEC)=30
      GEKIN=0.
      GO TO 500
c
c particle has less than cutoff2
c so turn it into a geantino and sum secondaries back with it
c then put it on the stack as a secondary
c
700   CONTINUE
c
      IF (LEM) THEN
          ID=IDCF1
      ELSEIF (LHD) THEN
          ID=IDCF2
      ENDIF
c
      IF( ISTOP .EQ. 0 )THEN
        DO I=1,NGKINE
          P2=GKIN(1,I)*GKIN(1,I)+GKIN(2,I)*GKIN(2,I)+GKIN(3,I)*GKIN(3,I)
          P = SQRT(P2)
          M2 =  GKIN(4,I)*GKIN(4,I) - P2
          IF( M2 .GE. 0.)THEN
            T = GKIN(4,I) - SQRT(M2)
          ELSE
            T = GKIN(4,I)
          ENDIF
          GEKIN = GEKIN + T
        ENDDO
      ENDIF
c
c remove the summed secondaries
c
      DO I=1,NGKINE
        IFLGK(I)=-1
      ENDDO
c
c add the new particle to the stack
c put position as well
c
      NGKINE = NGKINE + 1
      GKIN(1,NGKINE)=VECT(4)*GEKIN
      GKIN(2,NGKINE)=VECT(5)*GEKIN
      GKIN(3,NGKINE)=VECT(6)*GEKIN
      GKIN(4,NGKINE)=GEKIN
      GKIN(5,NGKINE)=ID
      TOFD(NGKINE)=0.
      GPOS(1,NGKINE)=VECT(1)
      GPOS(2,NGKINE)=VECT(2)
      GPOS(3,NGKINE)=VECT(3)
      IFLGK(NGKINE)=0    
c
c set shower flags and stop original particle (set gekin to zero)
c
      LSH=.TRUE.         
      LSTP=.TRUE.
      NMEC=NMEC+1
      LMEC(NMEC)=30
      GEKIN=0.
c
500   CONTINUE
      END



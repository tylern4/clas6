      SUBROUTINE CFSTEP
CA)========================================(begin of short description)
C Stop shower
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods : stop the shower pseudo-particle
C                          when it goes out from the calorimeter
C                          mother volume SGE0.
C
C    Called by: GUSTEP
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
C    Modified:  29-JUN-1998  David Tedeschi
C
CC)-------------------------------------------(end of long description)
      implicit none
CD)----------------------------------(declare common blocks, variables)
C  Common blocks
#include "gckine.inc"
#include "gcking.inc"
#include "gcmate.inc"
#include "gctmed.inc"
#include "gctrak.inc"
#include "gcvolu.inc"
#include "gconst.inc"
#include "cfpid.inc"

      CHARACTER*4 volnam(15)
      EQUIVALENCE(names(1),volnam(1))
      integer i
c
C===============================================(end of declarations)
C
c      call recmes('CFSTEP','I','CFSTEP reached')
c
c  check if we are in NEW volume or exitting the volume
c  inwvol = 0  -  normal transport
c  inwvol = 1  -  entering a new volume or beginning a new track
c  inwvol = 2  -  exiting a volume
c
      IF (INWVOL.EQ.0) RETURN
c
c******************************************************************
c     Stop Shower Pseudo-particle
cdjt  when it exits the ec
c   see trak001-3 for details
c  istop = 0   - normal transport
c        = 1   - particle has disappeared
c   
c  lmec  -  mechanisms array
c           mechanism 30 is STOP
c
*******************************************************************
c
      IF ( (IPART.EQ.IDCF1.OR.IPART.EQ.IDCF2).AND.(INWVOL.EQ.2).AND.
     #     (volnam(nlevel).EQ.'EC  ') ) THEN
CDJT     &   .AND.ECINWV(VECT(1),VECT(2),VECT(3)).EQ.2) THEN


        ISTOP=1
        NMEC=NMEC+1
        LMEC(NMEC)=30

c        call recmes('CFSTEP','I','CFSTEP Stop shower Pseudo-particle')
c        write(6,*) ' cfstep:   IPART=',IPART,'     INWVOL=',INWVOL
c        write(6,*) ' cfstep: nlevel=',nlevel
c        write(6,*) ' cfstep: volnam(nlevel)=',volnam(nlevel)
c        write(6,*) ' cfstep: number(nlevel)=',number(nlevel)
      ENDIF
C
      END

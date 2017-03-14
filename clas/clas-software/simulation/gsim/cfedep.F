      SUBROUTINE CFEDEP(I,VEC,DE)
CA)========================================(begin of short description)
C Energy depositions by parametrized showers
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :
C
C    Called by: CFEMSH, CFHDSH, (ECSTEP, ..)
C
C    Input arguments :
C       I = 1 : Called by CFEMSH (EM shower)
C         = 2 : Called by CFHDSH (HD shower)
C        (= 0 : Called by xxSTEP (dE/dx etc.) --- Optionally)
C        (       xxSTEP can call xxEDEP directly.           )
C       VEC(3) : Point (x, y, z) of the spot
C       DE : Energy deposition
C
C    Input/output arguments :  None
C
C    Output arguments:  None
C
C    Side effects (other variables/banks modified, etc.)
C
C    Created:   20-APR-1994  Serguei Boiarinov
C    Modified:  29-JUN-1998  David Tedeschi
c
c------------------------------------------
c  Common blocks
c
#include "gcvolu.inc"
#include "gctmed.inc"
#include "cftmed.inc"
c
c  Variables in argument list
c
      INTEGER I
      REAL VEC(3),DE
c
c  Local variables
c
      INTEGER IDUM, NUMD
      CHARACTER*4 CH4
      CHARACTER*2 CH2
c
c===============================================(end of declarations)
c Find tracking media and volume at the current point
c
c      write(6,*) 'CFEDEP -I- cfedep code reached'
c      write(6,*) 'CFEDEP -I- i vec de = ',i,vec,de
      IDUM=0
      CALL GTMEDI(VEC,IDUM)
      IDUM=0
      CALL GMEDIA(VEC,IDUM)
      CALL GFINDS
c
c     normalize the deposited energy with Serguei's enorms
c     enorm=4.6 for scintillator
c     enorm=0.7782 for lead
c
c      write(*,*) 'numed,destep= ',numed,destep
      if(numed.eq.19)destep=de*4.6
      if(numed.eq.21)destep=de*0.7782
c      write(*,*) 'destep= ',destep

      call step_ec

c
c      CALL UHTOC( NAMES(NLEVEL), 4, CH4, 4 )
c      CH2 = CH4(1:2)
c
c      IF (CH2.EQ.'EC') THEN
c        IF(NUMED.EQ.33) NUMD=1
c        IF(NUMED.EQ.13) NUMD=2
c        write(*,*) ' DE,ENORM=',DE,enorm(numd)
c        DE=DE*ENORM(NUMD)
c        CALL ECEDEP(I,VEC,DE)
c      ENDIF
c
      RETURN
      END

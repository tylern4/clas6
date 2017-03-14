      Subroutine repart(cm_mom,n1,n2,no)
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c     -------
c     
c     Major revisions:
c     ----------------
c     
      IMPLICIT NONE
      save
C     
#include "parm1.inc"
#include "parm3.inc"
#include "leptou.inc"
#include "ludat.inc"
#include "lujet.inc"
#include "gbd.inc"
c
      real cm_mom(4)
      integer n1,n2,no
c
      REAL PCM_1(4),PCM1(4),P_VE(3)
      integer i,i1,i_ve,j
c
C     
C     ****  Go back to the LAB system 
C     
      DO I = n1, n2
         DO I1 = 1,3
            P_VE(I1)=PCM(I1,I-no)
         ENDDO
         PCM_1(4)=PCM(4,I-no)
         DO I1=1,3
            PCM_1(I1)=P_VE(I1)
         ENDDO
         CALL LORENB(TECM,CM_MOM,PCM_1,PCM1)
C     
C     ****  Write LUND event
C     
         LINE=LINE+1
         p(line,4)=pcm1(4)
         p(line,5)=AMASS(I-no)
         k(line,2)=kp(i)
         k(line,1)=1
         DO I_VE=1,3
            P(line,I_VE)=PCM1(I_VE)
         ENDDO
       ENDDO
c
      end




      SUBROUTINE HISPRE(Y,N)
#include "user_par.inc"
C *************************************************************************
C         SUBROUTINE TO GENERATE RANDOM NUMBERS 
C         ACCORDING TO AN EMPIRICAL DISTRIBUTION    
C         SUPPLIED BY THE USER IN THE FORM OF A HISTOGRAM   
C         F. JAMES,    MAY, 1976    
C        MODIFIED JUNE 6, 1988 DON JOYCE
C                              IF Y SUM IS ZERO NORMALIZED ARRAY IS ZERO
C *************************************************************************
      real Y(MAXGRID),ytot    
c      DATA IERR,NTRY,NXHRAN,NXHPRE/0,3HRAN,3HRAN,3HPRE/ 
c      NTRY=NXHRAN   
c      NTRY=NXHPRE   
c   50 CONTINUE  
      YTOT = 0.
      DO 100 I= 1, N    
      YTOT = YTOT + Y(I)                                          
  100 Y(I) = YTOT  
C ***********************************************************************
C
C     TEST IF ARRAY IS ZERO, IF SO SET HIGHEST ELEMENT TO 1 AND CONTINUE
C
C ***********************************************************************
      IF(YTOT.EQ.0.0) THEN
         GOTO 112
      ENDIF
      YINV = 1.0/YTOT   
      DO 110 I= 1, N    
  110 Y(I) = Y(I) * YINV    
      Y(N) = 1.0  
  112 CONTINUE 
      RETURN
      END

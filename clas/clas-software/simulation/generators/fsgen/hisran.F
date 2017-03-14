C         NOW GENERATE RANDOM NUMBER BETWEEN 0 AND ONE  

      SUBROUTINE HISRAN(Y,N,XLO,XWID,XRAN,L)  
C *************************************************************************
C         SUBROUTINE TO GENERATE RANDOM NUMBERS 
C         ACCORDING TO AN EMPIRICAL DISTRIBUTION    
C         SUPPLIED BY THE USER IN THE FORM OF A HISTOGRAM   
C         F. JAMES,    MAY, 1976    
C        MODIFIED JUN 6, 1988 DON JOYCE,CEBAF
C                              IF Y SUM IS ZERO NORMALIZED ARRAY IS ZERO
C                 AUG 9, 1988 Don Joyce, CEBAF
C                              modified to avoid divide by zero when
C                              Y(L) = Y(L+1)
C                              passed L value as an argument            
C *************************************************************************
      real Y(N)    
      DATA IERR,NTRY,NXHRAN,NXHPRE/0,3HRAN,3HRAN,3HPRE/ 
  200 CONTINUE  
C ****************************************************************************
C	TRY RANDOM NUMBER GENERATOR FROM FLORIDA STATE PAPER
C ****************************************************************************
C 2013.05.02 -- changed RANDOM_NUM() to RAND() following the
C               FORTRAN intrisics for gfortran v4.7.2
C      YR = RANDOM_NUM()
      YR = RAND()
C         AND TRANSFORM IT INTO THE CORRESPONDING X-VALUE   
      L = LOCATF(Y,N,YR)    
      IF(L.EQ.0) GOTO 240   
      IF(L.GT.0) GOTO 250   
C         USUALLY COME HERE.    
      L = IABS(L)   
C ***************************************************************************
C       modified to check for Y(L+1) = Y(L) to avoid divide by
C       zero problems
C ***************************************************************************
      IF(L.eq.102)goto 200  !T.-Y 10/8/1996 for 1/milion out of boundary
c      
      IF(Y(L+1).EQ.Y(L)) THEN
         XRAN = XLO + XWID * (FLOAT(L) + YR)
      ELSE
         XRAN = XLO + XWID * (FLOAT(L) +
     &          ((YR-Y(L))/(Y(L+1)-Y(L)))) 
      ENDIF
      RETURN    
C         POINT FALLS IN FIRST BIN.  SPECIAL CASE   
  240 XRAN = XLO + XWID * (YR/Y(1)) 
      RETURN    
C         GUARD AGAINST SPECIAL CASE OF FALLING ON EMPTY BIN    
  250 XRAN = XLO + FLOAT(L) * XWID  
      RETURN    
  900 CONTINUE  
      IERR = IERR + 1   
      XRAN = 0. 
      RETURN    
      END   

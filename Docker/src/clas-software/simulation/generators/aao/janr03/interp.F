        SUBROUTINE INTERP(X,Y,NPTS,NTERMS,XIN,YOUT)
C
C       interpolate between data points to evaluate a function
C       copy form R. Bevington, p.266
C
C
C       THIS PROGRAM INTERPOLATES BETWEEN DATA POINTS.
C       (X,Y) ARE DATA POINTS,
C       NPTS= NUMBER OF SUCH DATA POINT PAIRS,
C       NTERMS= ORDER OF THE POLYNOMIAL USED + 1,
C       XIN= INPUT X,
C       YOUT= INTERPOLATED VALUE.
C       WE ARE OK FOR UP TO 10 TERMS.
C
        implicit none
        INTEGER I1,I2,K,J,NPTS,NTERMS,i,ix,imax,ixmax
        REAL DELTAX,SUM,PROD,DENOM
        REAL DELTA(10),A(10)
        REAL  X(51),Y(51),XIN,YOUT
        DO 19 I=1,NPTS
        IF (XIN-X(I)) 13,17,19
13      I1=I-NTERMS/2
        IF (I1) 15,15,21
15      I1=1
        GO TO 21
17      YOUT=Y(I)
        GO TO 61
19      CONTINUE
        I1=NPTS-NTERMS+1
21      I2=I1+NTERMS-1
        IF (NPTS-I2) 23,31,31
23      I2=NPTS
        I1=I2-NTERMS+1
        IF (I1) 26,26,31
26      I1=1
        NTERMS=I2-I1+1
31      DENOM=X(I1+1)-X(I1)
        DELTAX=(XIN-X(I1))/DENOM
        DO 35 I=1,NTERMS
        IX=I1+I-1
35      DELTA(I)=(X(IX)-X(I1))/DENOM
        A(1)=Y(I1)
        DO 50 K=2,NTERMS
        PROD=1.
        SUM=0.
        IMAX=K-1
        IXMAX=I1+IMAX
        DO 49 I=1,IMAX
           J=K-I
           PROD=PROD*(DELTA(K)-DELTA(J))
 49        SUM=SUM-A(J)/PROD
 50        A(K)=SUM+Y(IXMAX)/PROD
           SUM=A(1)
           DO 57 J=2,NTERMS
              PROD=1.
              IMAX=J-1
              DO 56 I=1,IMAX
 56              PROD=PROD*(DELTAX-DELTA(I))
 57              SUM=SUM+A(J)*PROD
                 YOUT=SUM
 61              CONTINUE
        RETURN
        END     


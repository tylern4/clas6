      subroutine GWEPR08(qq,wc,eps,a,phi,irx,pl,pn,pt,sigu,sigt,sigl,sigi,sige,siget,sigpt)

c These routines were sent by Igor Strakovski and Dick Arndt OCT 27 2008
c Modified this routine for my purposes (LCS)
c User must call EPRSET first time before calling GWEPR
c This routine can be modified to output whatever observable is desired
c The routine EPROUT below can be used to create multipole tables
      
c Input: qq = Q2 (GeV)
c        wc = W  (GeV)
c       eps = epsilon
c         a = theta(c.m.) (deg)
c       phi = phi(c.m.) (deg)
c       irx = 1=ppi0 2=npi+ 3=npi- 4=pion
c       crs = sigma_T (microbarns)
c      sltp = sigma_LT' (microbarns)

      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
 
      SAVE
      
      complex hh1,hh2,hh3,hh4,hh5,hh6
      
      IR=IRX
      Q2=QQ
      WCM=WC
      THTA=A
      EPSG=EPS
      PHIG=PHI
      CALL EPRHA
      CALL EPRSET
      
      fkt=QCM*WCM/(WCM**2-938.28**2)/50.0
      
      hh1=cmplx(hrx(1),hix(1))
      hh2=cmplx(hrx(2),hix(2))
      hh3=cmplx(hrx(3),hix(3))
      hh4=cmplx(hrx(4),hix(4))
      hh5=cmplx(hrx(5),hix(5))
      hh6=cmplx(hrx(6),hix(6))
            
c You have to hunt through the routine EPROBS to find the index for each observable.
c The list below is from the original code.      
c Observables are numbered starting with DSG
c (DSG )1   (DT  )  (DL  ) (DTT )  (DTL )  (DTLP)  (DSTL)
c (SGT )8   (SGTT)  (SGTL)  (T   )  (P   )  (S   )  (E   )
c (F   )15  (G   )  (H   )  (OX  )  (OZ  )  (CX  )  (CZ  )
c (TX  )22  (TZ  )  (LX  )  (LZ  )  (T00 )  (T0Y )  (TY0 )
c (TXX )29  (TXZ )  (TZX )  (TZZ )  (L00 )  (L0Y )  (LXX )
c (LZX )36  (TL00)  (TL0X)  (TL0Y)  (TL0Z)  (TLX0)  (TLZ0)
c (TLXX)43  (TLZX)  (TT00)  (TT0X)  (TT0Z)  (TTX0)  (TTZ0)
c (LP00)50  (LP0X)  (LP0Y)  (LP0Z)  (LPX0)  (LPZ0)  (LPXX)
c (LPZX)57  (TP0X)  (TP0Z)  (TPX0)  (TPZ0)  (A0Y )  (PY  )
c (PX  )64  (PZ  )  (EMRI)  (ST1 )  (ST3 )  (ST13)  (A0YP)
c (AXYZ)71

      phicm_rad = phi*3.1415926/180.0
      cph  = cos(phicm_rad)
      sph  = sin(phicm_rad)
      c2ph = cos(2*phicm_rad)
      s2ph = sin(2*phicm_rad)
      
      vt   = 1.0
      vl   = eps
      vtt  = eps
      vlt  = sqrt(2*eps*(1+eps))
      vltp = sqrt(2*eps*(1-eps))
      vttp = sqrt(1-eps**2)
      
      sig_t  = (cabs(hh1)**2+cabs(hh2)**2
     1         +cabs(hh3)**2+cabs(hh4)**2)/2.
      sig_l  =  cabs(hh5)**2+cabs(hh6)**2
      sig_t32 = (cabs(hh2)**2+cabs(hh4)**2)
      sig_t12 = (cabs(hh1)**2+cabs(hh3)**2)
      sig_tt =  real(-conjg(hh1)*hh4+conjg(hh2)*hh3)
      
      sig_lt  =   real(conjg(hh5)*(hh1-hh4)  + 
     1                 conjg(hh6)*(hh2+hh3))/sqrt(2.0)      
      sig_ltp = -aimag(conjg(hh5)*(hh1-hh4)  + 
     1                 conjg(hh6)*(hh2+hh3))/sqrt(2.0)

      sigt_t_y   = -aimag(conjg(hh1)*hh2+conjg(hh3)*hh4)
      sigt_l_y   = -2*aimag(conjg(hh5)*hh6)
      
      sigt_lt_x  = aimag(conjg(hh5)*(hh3-hh2) - 
     1                   conjg(hh6)*(hh1+hh4))/sqrt(2.)
      sigt_ltp_x =  real(conjg(hh5)*(hh2-hh3) +
     1                   conjg(hh6)*(hh1+hh4))/sqrt(2.)
     
      sigt_lt_y  = aimag(-conjg(hh5)*(hh2+hh3) + 
     1                    conjg(hh6)*(hh1-hh4))/sqrt(2.)
      sigt_ltp_y =  real(-conjg(hh5)*(hh2+hh3) + 
     1                    conjg(hh6)*(hh1-hh4))/sqrt(2.)
     
      sigt_lt_z  = aimag(-conjg(hh5)*(hh1+hh4) + 
     1                    conjg(hh6)*(hh2-hh3))/sqrt(2.)
      sigt_ltp_z = -real(-conjg(hh5)*(hh1+hh4) + 
     1                    conjg(hh6)*(hh2-hh3))/sqrt(2.)
     
      sigt_tt_x  =  aimag(conjg(hh1)*hh3-conjg(hh2)*hh4)
      sigt_tt_y  = -aimag(conjg(hh1)*hh3+conjg(hh2)*hh4)
      sigt_tt_z  = -aimag(conjg(hh1)*hh4+conjg(hh2)*hh3)
      
      sigt_ttp_x = real(conjg(hh1)*hh2+conjg(hh3)*hh4)
      sigt_ttp_z = (cabs(hh1)**2-cabs(hh2)**2
     1             +cabs(hh3)**2-cabs(hh4)**2)/2.
     
      sigu   = fkt * sig_t
      sigl   = fkt * sig_l   * vl
      sigt   = fkt * sig_tt  * vtt  * c2ph
      sigi   = fkt * sig_lt  * vlt  * cph
      sige   = fkt * sig_ltp * vltp * sph

      sigma_ltp_x = fkt  * vltp * pt * sigt_ltp_x * cph  
      sigma_ltp_y = fkt  * vltp * pn * sigt_ltp_y * sph  
      sigma_ltp_z = fkt  * vltp * pl * sigt_ltp_z * cph 
      sigma_ttp_x = fkt  * vttp * pt * sigt_ttp_x
      sigma_ttp_z = fkt  * vttp * pl * sigt_ttp_z
      
      siget  = sigma_ltp_x+sigma_ltp_y+sigma_ltp_z
      siget  = siget+sigma_ttp_x+sigma_ttp_z
      
      sigma_t_y   = fkt  * vt   * pn * sigt_t_y
      sigma_l_y   = fkt  * vl   * pn * sigt_l_y          
      sigma_lt_x  = fkt  * vlt  * pt * sigt_lt_x  * sph 
      sigma_lt_y  = fkt  * vlt  * pn * sigt_lt_y  * cph 
      sigma_lt_z  = fkt  * vlt  * pl * sigt_lt_z  * sph 
      sigma_tt_x  = fkt  * vtt  * pt * sigt_tt_x  * s2ph
      sigma_tt_y  = fkt  * vtt  * pn * sigt_tt_y  * c2ph
      sigma_tt_z  = fkt  * vtt  * pl * sigt_tt_z  * s2ph

      sigpt  = sigma_t_y+sigma_l_y
      sigpt  = sigpt+sigma_lt_x+sigma_lt_y+sigma_lt_z
      sigpt  = sigpt+sigma_tt_x+sigma_tt_y+sigma_tt_z      
      
      RETURN
      END
      
      subroutine helout(qq,irx)
      
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
     
      SAVE
      
      IR=irx
      Q2=qq
      fac=0.141383
      
      do wcm=1080,2000,10
      do z=-0.8,-0.8
      thta=acos(z)*180/3.14159
      call eprha
      call eprset
      call eprobs(3)
      write(*,100) q2,wcm,obsprd,(hrx(i),hix(i),i=1,6)
100   format(2f7.1,f7.4,13f7.2)
      enddo
      enddo
      
      end
      
      subroutine epr
      
      call eprset
      do q2=0.0,5.0,0.05
      do ww=1.1,2.0,0.01
      call eprout(q2,ww)
      enddo
      enddo
      
      end
      
      subroutine eprout(qq,wc) 
      
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
     
      INTEGER IND1(3),IND2(3)
      REAL REAC(2,2,3,12),fac(2)
      DATA IND1/4,3,8/IND2/2,1,7/FAC/1.41383,-1.41383/
      SAVE

      Q2  = QQ
      WCM = WC*1000.
      call EPRHA
      CALL EPRSETA
      
c i=1,2 (real,imag) j=1,2 (pi0,pi+) k=1,2,3 (M,E,S) l=1-12 (0-,0+,1-,1+,2-,2+,etc.)	

      DO L=1,6
      DO K=1,3 
      IN1=IND1(K)
      IN2=IND2(K)
      DO LL=1,2
      LLL=2*(L-1)+LL
      Reac(1,1,K,LLL) = EMR(IN1,LL,L)+EMR(IN2,LL,L)/1.5
      Reac(2,1,K,LLL) = EMI(IN1,LL,L)+EMI(IN2,LL,L)/1.5
      Reac(1,2,K,LLL) = SQRT(2.)*(EMR(IN1,LL,L)-EMR(IN2,LL,L)/3.)
      Reac(2,2,K,LLL) = SQRT(2.)*(EMI(IN1,LL,L)-EMI(IN2,LL,L)/3.)
      ENDDO
      ENDDO
      ENDDO
      
      do j=1,2
        write(j,330) wc,q2
        write(j,331) 'SL+'
        write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=2,6,2)
        write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=8,12,2)
        write(j,331) 'SL-'
        write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=1,5,2)
        write(j,332) ((Reac(i,j,3,l)/fac(j),i=1,2),l=7,11,2)
        write(j,331) 'EL+'
        write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=2,6,2)
        write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=8,12,2)
        write(j,331) 'EL-'
        write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=1,5,2)
        write(j,332) ((Reac(i,j,2,l)/fac(j),i=1,2),l=7,11,2)
        write(j,331) 'ML+'
        write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=2,6,2)
        write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=8,12,2)
        write(j,331) 'ML-'
        write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=1,5,2)
        write(j,332) ((Reac(i,j,1,l)/fac(j),i=1,2),l=7,11,2)
      enddo
         
      do j=1,2
         write(20+j,333) ((Reac(i,j,2,l)/fac(j),i=1,2),l=2,12,2)
         write(20+j,333) ((Reac(i,j,2,l)/fac(j),i=1,2),l=1,11,2)
         write(20+j,333) ((Reac(i,j,1,l)/fac(j),i=1,2),l=2,12,2)
         write(20+j,333) ((Reac(i,j,1,l)/fac(j),i=1,2),l=1,11,2)
         write(20+j,333) ((Reac(i,j,3,l)/fac(j),i=1,2),l=2,12,2)
         write(20+j,333) ((Reac(i,j,3,l)/fac(j),i=1,2),l=1,11,2)
      enddo
         
 330  format('W= ',f5.3,1x,'Q2= ',f5.3)
 331  format(1x,a3)
 332  format(4x,6e15.5)
 333  format(1x,12f8.4)
      
      END

C **********************************************************
      SUBROUTINE EPRSET
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DATA SOL/4HGWSI/
      DATA WCMM,Q2MN,IRM/0.0,0.0,-1/
      SAVE
      IF(IR.NE.IRM) WCMM=0.0
      IF(Q2.NE.Q2MN) WCMM=0.0
      IF(WCM.EQ.WCMM) GO TO 99
      WCMM=WCM
      Q2MN=Q2
      IRM=IR
      WGX=1000.0*SQRT(Q2)
      CALL EPRKIN(WCM,WGX,IR,EPI,ZKCM,QCM)
      TITLE(1)=SOL
      CALL SAIDI(TITLE,221,IR,Q2,WCM,DER,DEI)
      NNL=6
      SQ=SQRT(2.0)
      CIS(2,1)=SQ
      CIS(2,2)=-SQ/3.0
      CIS(1,1)=1.0
      CIS(1,2)=2.0/3.0
      CIS(3,1)=SQ
      CIS(3,2)=SQ/3.0
      CIS(4,1)=-1.0
      CIS(4,2)=2.0/3.0
      ZL=0.0
      DO 3 L=1,NNL
      CH(1,1,L)=-1.0
      CH(1,2,L)=-1.0
      CH(1,3,L)=1.0
      CH(1,4,L)=-1.0
      CH(2,1,L)=ZL
      CH(2,2,L)=-ZL-2.0
      CH(2,3,L)=ZL+2.0
      CH(2,4,L)=ZL
      CH(3,1,L)=1.0
      CH(3,2,L)=1.0
      CH(3,3,L)=1.0
      CH(3,4,L)=-1.0
      CH(4,1,L)=-ZL
      CH(4,2,L)=ZL+2.0
      CH(4,3,L)=ZL+2.0
      CH(4,4,L)=ZL
    3 ZL=ZL+1.0
   99 RETURN
      END
C ********************************************************
      SUBROUTINE EPRHA
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DATA ISTART/0/
      SAVE
      IF(ISTART.EQ.0) CALL EPRSET
      ISTART=1
      CALL EPRSETA
      WGX=SQRT(Q2)*1000.0
      CALL EPROPEC(WCM,WGX,EMPI)
      DO 5 K=1,6
      HR(K)=0.0
    5 HI(K)=0.0
      CALL EHOPEC(WCM,WGX,THTA,IR,HR)
      MMX=9
      IF(Q2.EQ.0.0) MMX=6
      DO 8 L=1,NNL
      LL=L
      DO 8 J=1,2
      JJ=J
      IF(J.EQ.1.AND.L.EQ.1) GO TO 8
      DO 6 M=1,MMX
      NP=0
      IF(M.EQ.3.OR.M.EQ.4.OR.M.EQ.8) NP=1
      IF(M.EQ.5.OR.M.EQ.6.OR.M.EQ.9) NP=2
      IF(IR.GT.2.AND.NP.EQ.1) GO TO 6
      IF(IR.LT.3.AND.NP.EQ.2) GO TO 6
      MM=M
      DER=EMR(M,J,L)-EMPI(M,J,L)
      DEI=EMI(M,J,L)
      MJL=100*MM+10*JJ+LL-1
      CALL EPRDA
      DO 7 K=1,6
      HR(K)=HRX(K)
    7 HI(K)=HIX(K)
    6 CONTINUE
    8 CONTINUE
      RETURN
      END
C ************************************************************
      SUBROUTINE EPRDA
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION BAS(4,6)
      DATA THTM/-1.0/
      SAVE
      IF(THTA.EQ.THTM) GO TO 10
      CALL HBAS(THTA,BAS,NNL)
      THTM=THTA
   10 DO 1 K=1,6
      HRX(K)=HR(K)
    1 HIX(K)=HI(K)
      IF(DER.EQ.0.0.AND.DEI.EQ.0.0) GO TO 99
      L=LL-2+JJ
      IF(L.LT.1) GO TO 99
      IF(MM.GT.6) GO TO 3
      IF(IR.LT.3.AND.MM.GT.4) GO TO 99
      IF(IR.GT.2.AND.MM.EQ.3)  GO TO 99
      IF(IR.GT.2.AND.MM.EQ.4) GO TO 99
      MXM=MM/2
      MXM=1-MM+2*MXM
C MXM=0(E), 1(M)
      J=2*JJ+MXM-1
      ISP=1
      IF(MM.LT.3) ISP=2
      CI=CIS(IR,ISP)
      DO 2 K=1,4
      C=CI*CH(K,J,L)*BAS(K,L)
      HRX(K)=HR(K)+C*DER
    2 HIX(K)=HI(K)+C*DEI
      GO TO 99
    3 IF(MM.EQ.8.AND.IR.GT.2) GO TO 99
      IF(MM.EQ.9.AND.IR.LT.3) GO TO 99
      ISP=1
      IF(MM.EQ.7) ISP=2
      Z=2*L
      Z=Z*CIS(IR,ISP)
      ZZ=-1000.0*SQRT(Q2)/ZKCM/SQRT(2.0)
      Z=Z*ZZ
C MULTIPLY H5, H6 BY SQRT(-Q**2)/Q0 Q0=C.M. ENERGY OF PHOTON
      HRX(5)=HR(5)+Z*DER*BAS(2,L)
      HIX(5)=HI(5)+Z*DEI*BAS(2,L)
      IF(JJ.EQ.1) Z=-Z
      HRX(6)=HR(6)+DER*Z*BAS(4,L)
      HIX(6)=HI(6)+DEI*Z*BAS(4,L)
   99 RETURN
      END
C ****************************************************************
      SUBROUTINE EPRSETA
C  INITIALIZE PARTIAL WAVE AMPLITUDES
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DATA WCMM,Q2MN,IRM/0.0,0.0,-1/
      SAVE
      IF(IR.NE.IRM) WCMM=0.0
      IF(Q2.NE.Q2MN) WCMM=0.0
      IF(WCM.EQ.WCMM) GO TO 99
      WCMM=WCM
      Q2MN=Q2
      IRM=IR
      DO 3 M=1,9
      DO 3 J=1,2
      DO 3 L=1,NNL
      MJL=100*M+10*J+L-1
      DER=0.0
      DEI=0.0
      K=IMJL(MJL,MM,JJ,LL)
      IF(K.LT.0) GO TO 3
      CALL SAIDI(TITLE,MJL,IR,Q2,WCM,EMR(M,J,L),EMI(M,J,L))
    3 CONTINUE
   99 RETURN
      END
C ****************************************************
      SUBROUTINE EPROBS(IT)
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
C     OBSERV FOR PI-N ELECTROPRODUCTION HRX/HIX ARE IN MILLI-FM
C Revised 7/99 to correspond to Knochlein, Drechsel, Tiator notations 
      DIMENSION JK(15)
      DATA JK/2,3,-20,-34,33,-22,21,23,24,-35,-36,4,6,-5,7/
      SAVE
      OBSPRD=0.0
C For BORN solution set OBS(25 ...)=HRX(J-24) 
C     IF(IT.GT.NITTL) GO TO 98
      IF(IT.EQ.66) GO TO 30
      IF(IT.LT.26) GO TO 1
C get Response functions from Knochlein, Drechsel, and Tiator
      IF(IT.LT.62) GO TO 11
C 62=A0Y, 63=PY, 64=PXH(180 deg), 65=PZH(180 deg)
C encode A0Y and PY. MIT data 7/15/99 PXH and PYH from Schmeiden
C anf Tiator nucl-th/9909046 1999
      I=IT-62
      SF=SQRT(2.0*EPSG*(1.0+EPSG))
      IF(I.EQ.2) SF=SQRT(2.0*EPSG*(1.0-EPSG))
      IF(I.EQ.3) SF=SQRT(1.0-EPSG**2)
      Z=RF(25,HRX,HIX)
      IF(I.EQ.1) Z=RF(14,HRX,HIX)
      IF(I.EQ.2) Z=RF(26,HRX,HIX)
      IF(I.EQ.3) Z=RF(34,HRX,HIX)
      D=RF(1,HRX,HIX)+EPSG*RF(8,HRX,HIX)
      IF(I.EQ.0) D=D+EPSG*RF(20,HRX,HIX)
      OBSPRD=-Z/D*SF
      GO TO 98
   11 J=IT-25
      OBSPRD=RF(J,HRX,HIX)/100.0
      GO TO 98
    1 IF(IT.LT.11) GO TO 2
      J=IT-10
      K=JK(J)
      S=1.0
      IF(K.LT.0) S=-1.0
      IF(K.LT.0) K=-K
      OBSPRD=RF(K,HRX,HIX)/RF(1,HRX,HIX)*S
      GO TO 98
C Cross-sections 
C DSU=ST, DSL=SL, DSP=STT, DSI=STL from Knochlein
C DSU=DT, DSL=DL, DSP=DTT, DSI=DTL
    2 QK=QCM*WCM/(WCM**2-938.28**2)/50.0
      IF(IT.GT.7) GO TO 3
      DT=QK*RF(1,HRX,HIX)
      DL=QK*RF(8,HRX,HIX)
      DTT=QK*RF(20,HRX,HIX)
      DTL=QK*RF(12,HRX,HIX)
      DTLP=QK*RF(25,HRX,HIX)
      OBSPRD=DT
      IF(IT.EQ.3) OBSPRD=DL
      IF(IT.EQ.4) OBSPRD=DTT
      IF(IT.EQ.5) OBSPRD=DTL
      IF(IT.EQ.6) OBSPRD=DTLP
      IF(IT.EQ.7) OBSPRD=DT+EPSG*DL
      IF(IT.NE.1) GO TO 98
      OBSPRD=DT+EPSG*DL+EPSG*DTT*COS(2.0*PHIG*0.0174532)
      OBSPRD=OBSPRD+SQRT(EPSG*(EPSG+1.0)*2.0)*COS(0.0174532*PHIG)*DTL
      GO TO 98
C Total Cross-sections
    3 SGT=0.0
      SGTL=0.0
      M1=3
      IF(IR.GT.2) M1=5
      M1S=8
      IF(IR.GT.2) M1S=9
      M2=M1+1
      C1=CIS(IR,1)
      C2=CIS(IR,2)
      DO 21 L=1,NNL
      ZR=C1*EMR(M1,2,L)+C2*EMR(1,2,L)
      ZI=C1*EMI(M1,2,L)+C2*EMI(1,2,L)
      Z2=ZR**2+ZI**2
      IF(L.EQ.NNL) GO TO 22
      ZR=C1*EMR(M2,1,L+1)+C2*EMR(2,1,L+1)
      ZI=C1*EMI(M2,1,L+1)+C2*EMI(2,1,L+1)
      Z2=Z2+ZR**2+ZI**2
   22 ZL=L
      SGT=SGT+ZL**2*(ZL+1.0)*Z2
      ZR=C1*EMR(M1S,2,L)+C2*EMR(7,2,L)
      ZI=C1*EMI(M1S,2,L)+C2*EMI(7,2,L)
      Z2=ZR**2+ZI**2
      IF(L.EQ.NNL) GO TO 23
      ZR=C1*EMR(M1S,1,L+1)+C2*EMR(7,1,L+1)
      ZI=C1*EMI(M1S,1,L+1)+C2*EMI(7,1,L+1)
      Z2=Z2+ZR**2+ZI**2
   23 SGTL=SGTL+ZL**3*Z2
      IF(L.EQ.1) GO TO 21
      ZR=C1*EMR(M2,2,L)+C2*EMR(2,2,L)
      ZI=C1*EMI(M2,2,L)+C2*EMI(2,2,L)
      Z2=ZR**2+ZI**2
      IF(L.EQ.NNL) GO TO 24
      ZR=C1*EMR(M1,1,L+1)+C2*EMR(1,1,L+1)
      ZI=C1*EMI(M1,1,L+1)+C2*EMI(1,1,L+1)
      Z2=Z2+ZR**2+ZI**2
   24 SGT=SGT+ZL**2*(ZL-1.0)*Z2
   21 CONTINUE
      QK=2.0*3.1415927*QK
      SGTU=SGT*QK
      SGTL=2.0*QK*SGTL*1.0E6*Q2/ZKCM**2
      OBSPRD=SGTU+EPSG*SGTL
      IF(IT.EQ.9) OBSPRD=SGTU
      IF(IT.EQ.10) OBSPRD=SGTL
   98 IF(OBSPRD.EQ.0.0) OBSPRD=1.0E-10
      GO TO 99
   30 MJL=THTA
      K=IMJL(MJL,MM,JJ,LL)
      IF(K.GT.50) OBSPRD=PEM(K-50,MM,JJ,LL+1)
      IF(K.GT.50) GO TO 99
      IF(K.LT.0) GO TO 99
      ZR=EMR(MM,JJ,LL+1)
      ZI=EMI(MM,JJ,LL+1)
      OBSPRD=ZR
      IF(K.EQ.1) OBSPRD=ZI
      IF(K.EQ.2) OBSPRD=SQRT(ZR**2+ZI**2)
      IF(K.NE.3) GO TO 99
      OBSPRD=57.296*ATAN(ZI/(ZR+1.0E-9))
      IF(ZR.GT.0.0) GO TO 99
      IF(ZI.GT.0.0) OBSPRD=OBSPRD+180.0
      IF(ZI.LT.0.0) OBSPRD=OBSPRD-180.0
   99 RETURN
      END
C ******************************************************
      SUBROUTINE EPRKIN(WCM,WG,IRR,EPI,ZKCM,QCM)
C GET PION-ELECTROPRODUCTION KINEMATIC PARAMETERS
      DATA WP,WN,WPI0,WPIC/938.280,939.65,134.96,139.65/
      DATA EPIT/10.0/
      SAVE
      IR=IRR
      IF(IR.EQ.5) IR=1
      WT=WP
      IF(IR.GT.2) WT=WN
      WX=WP
      IF(IR.EQ.2.OR.IR.EQ.4) WX=WN
      WPI=WPI0
      IF(IR.EQ.2.OR.IR.EQ.3) WPI=WPIC
      QCM=0.0
      EPI=0.0
      ZKCM=0.0
C     IF(WCM.LT.WX+WG) GO TO 99
      IF(WCM.LT.WPI+WX) GO TO 99
      S=WCM**2
      WG2=WG**2
      WX2=WX**2
      WT2=WT**2
      E1=(S+WT2+WG**2)/2.0/WCM
      ZKCM=SQRT(E1**2-WT2)
      STH=(WPI+WX)**2
      QCM=SQRT((S-STH)*(S-(WPI-WX)**2)/4.0/S)
      EPI=(S-STH)/2.0/WX
   99 RETURN
      END
C ********************************************************
      SUBROUTINE PRKIN(E,IRR,EPI,ZKCM,QCM)
C GET PION-PHOTOPRODUCTION KINEMATIC PARAMETERS
      COMMON/PRKC/IPRK
      DATA WP,WN,WPI0,WPIC/938.256,939.65,135.04,139.65/
      DATA EPIT/10.0/
      SAVE
      IR=IRR
      IF(IR.EQ.5) IR=1
      WT=WP
      IF(IR.GT.2) WT=WN
      WX=WP
      IF(IR.EQ.2.OR.IR.EQ.4) WX=WN
      WPI=WPI0
      IF(IR.EQ.2.OR.IR.EQ.3) WPI=WPIC
      WPX=WPI
      SPITH=(WX+WPI)**2
      IF(IR.GT.5) WT=WP
      WXX=WX
      IF(IR.EQ.6) WPX=547.27
      IF(IR.EQ.6) WXX=WP
      IF(IR.EQ.7) WPX=494.0
      IF(IR.EQ.7) WXX=1115.0
      ZKCM=E/SQRT(1.0+2.0*E/WT)
      S=WT*(WT+2.0*E)
      QCM=0.0
      EPI=0.0
      STH=(WPX+WXX)**2
      IF(S.LE.STH) GO TO 99
      QCM=SQRT((S-STH)*(S-(WPX-WXX)**2)/4.0/S)
      EPI=(S-SPITH)/2.0/WX
      IF(IR.GT.5) GO TO 99
      IF(IPRK.EQ.1) GO TO 99
C the following makes Epi dependent upon Wcm and independent of charge
C channel. Generally used for solutions before April 1996. RAA
      EPI=(S-(WP+WPIC)**2)/2.0/WP
      IF(EPI.GE.EPIT) GO TO 99
      ST=(WP+WPIC)**2+2.0*EPIT*WP
      Z=(S-STH)/(ST-STH)
C "STRETCH OUT" THRESHOLD BELOW EPI=10 MEV
      EPI=EPIT*Z**2
   99 RETURN
      END
C ***************************************************
      SUBROUTINE EHOPEC(WCM,WGM,TH,IRP,BORN)
C This subroutine was extracted from BORNT. It gives BORN amplitudes
C =OPEC+NP+PV. Checked against HOPEC for WGM=0 2/96 R. Arndt
C It appears to follow Berends, NP B4, 1(67) with an added PV term
C      SUBROUTINE BORNT(IR,W,Q2,EPCOS,CUT,BORN)
C* IR=1,2,3,4 for pi0p,pion,pi+n,pi-p  IRP=1,4,2,3 for SAID assigns
c*
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
       DIMENSION BORN(6),GAMMA(5,3),CIS(30)
       DIMENSION A(5,3),AR(5),FR(6),F1(3),F2(3)
       DIMENSION C(6),B(6,5),ETA(6),EPS(3)
      DATA CIS/1.0,1.0,0.0,0.0,1.0,1.0,0.0,1.0,-1.0,1.0,-1.0,0.0
     C,1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,1.0,0.0,-1.0,0.33333
     C,1.0,0.666667,-0.333333,1.0,-0.666667/
      DATA XMP,XMPI,GN/0.9382796,0.134963,62.28/
C EC=Sqrt(4*pi/ALFA), GG=Sqrt(4*pi*g2/4pi)=13.12 for g2/4pi=13.7
C Hbarc*EC*GG/(4*pi) should be 62.28 for consistency with HOPEC
       DATA ETA/1.,1.,-1.,1.,-1.,-1./,EPS/1.,1.,-1./
       DATA CUT,SQ2/0.4,1.4142136/
      SAVE
C  SET UP ARGUMENTS AS FED BY EHOPEC SUBROUTINE
      GN=197.32*SQRT(GPI2/137.0)
      THR=0.0174532*TH
      COSX=COS(THR)
      DO 1 I=1,6
    1 BORN(I)=0.0
C     IF(GPI2.LT.1.0) GO TO 99
C SAID order for reactions is pi0p, pi+n, pi-p, pi0n so convert IRP->IR
C BERENDS orders as: pi0p, pi0n, pi+n, pi-p
      W=WCM/1000.0
      Q2=(WGM/1000.0)**2
      CALL OPEKIN(W,WGM,S,E1,P,E2,Q,P0,Q0)
       Z1=SQRT(E1+XMP)
       Z2=SQRT(E2+XMP)
       FG=GN/2.0/W
       C(1)=FG*(W-XMP)*Z1*Z2
       C(2)=FG*(W+XMP)*P*Q/Z1/Z2
       C(3)=FG*(W+XMP)*P*Q*Z2/Z1
       C(4)=FG*(W-XMP)*Q**2*Z1/Z2
       C(5)=FG*Z1/Z2*Q
       C(6)=FG*P*Z2/Z1
      CALL OPEFF(WGM,F1V,F1S,F2V,F2S,FPI,A5GT)
      F2V=-2000.0*F2V
      F2S=-2000.0*F2S
      F1V=2.0*F1V
      F1S=2.0*F1S
       F1(1)=F1V
       F1(2)=F1S
       F1(3)=F1V
       F2(1)=F2V
       F2(2)=F2S
       F2(3)=F2V
C* 1,3,2, CORESSPONDING TO THE ISOVECTOR (+,-) AND ISOSCALAR (0).
C* PION FORM FACTOR.
   10  T=2.*Q*P*COSX-2.*Q0*P0+XMPI**2-Q2
       U=-2.*Q*P*COSX-2.*P0*E2+XMP**2-Q2
      DS=1.0/(S-XMP**2)
      DT=1.0/(T-XMPI**2)
      DU=1.0/(U-XMP**2)
       DO 15,J=1,3
         GAMMA(1,J)=F1(J)/2.0
         GAMMA(2,J)=-F1(J)*DT
         GAMMA(3,J)=-0.5*F2(J)
         GAMMA(4,J)=GAMMA(3,J)
   15    GAMMA(5,J)=0.5*GAMMA(2,J)
       DO 25 I=1,5
       DO 25 J=1,3
      A(I,J)=(DS+EPS(J)*ETA(I)*DU)*GAMMA(I,J)
      IF(Q2.LE.0.0.AND.I.GT.4) A(I,J)=0.0
   25 CONTINUE
       A(5,3)=A(5,3)+A5GT*DT
C combine Isospins IR=1...7 for pi0, pi0n, pi+, pi-, I3/2, pI1/2, nI1/2
C IRP=1, 2, 3, 4, 5, 6, 7 for pi0, pi+, pi-, pi0n, +, 0, -
C IRP=8, 9, 10 FOR I=3/2, P1/2, N1/2
      K=3*IRP-2
      FC=1.0
      IF(IRP.EQ.2.OR.IRP.EQ.3) FC=SQ2
      CP=FC*CIS(K)
      C0=FC*CIS(K+1)
      CM=FC*CIS(K+2)
       DO 50 I=1,5
   50  AR(I)=CP*A(I,1)+C0*A(I,2)+CM*A(I,3)
C PV non-zero adds in pseudo-vector term
c* Add in the pseudovector extra term, here we simply used F2(1,2) same
c* as in non-axial term.
      EPVV=F2V/XMP/2.0
      EPVS=F2S/XMP/2.0
      PV=CP*EPVV+C0*EPVS
      AR(1)=AR(1)+PV
       PQ=(T-XMPI**2+Q2)/2.
       B(1,1)=1.
       B(1,2)=0.
       B(1,3)=-PQ/(W-XMP)
       B(1,4)=W-XMP+PQ/(W-XMP)
       B(1,5)=0.
       B(2,1)=-1.
       B(2,2)=0.
       B(2,3)=-PQ/(W+XMP)
       B(2,4)=W+XMP+PQ/(W+XMP)
       B(2,5)=0.
       B(3,1)=0.
       B(3,2)=(S-XMP**2+Q2/2.)/(W+XMP)
       B(3,3)=1.
       B(3,4)=-1.
       B(3,5)=-Q2/(W+XMP)
       B(4,1)=0.
       B(4,2)=-(S-XMP**2+Q2/2.)/(W-XMP)
       B(4,3)=1.
       B(4,4)=-1.
       B(4,5)=Q2/(W-XMP)
C CORRECT STUFF HERE
       B(5,1)=-E1+XMP
       B(5,2)=1./(2.*P0)*(P**2*(3.*PQ+2.*P0*W)-P*Q*COSX
     C *(2.*(S-XMP**2)+Q2))
       B(5,3)=PQ+Q0*(W-XMP)
       B(5,4)=-PQ-Q0*(W-XMP)+(E1-XMP)*(W+XMP)
       B(5,5)=Q0*Q2-P0*PQ
       B(6,1)=E1+XMP
       B(6,2)=-B(5,2)
       B(6,3)=PQ+Q0*(W+XMP)
       B(6,4)=-PQ-Q0*(W+XMP)+(E1+XMP)*(W-XMP)
       B(6,5)=-B(5,5)
       DO 55 I=1,6
       ZZ=0.
       DO 65 J=1,5
C SCALE AMPLITUDES TO MFM
       ZZ=ZZ+B(I,J)*AR(J)
   65  CONTINUE
      FR(I)=ZZ*C(I)
      BORN(I)=FR(I)
   55  CONTINUE
C     IF(IRP.NE.0) GO TO 99
C Convert F's to SAID H's
       SINX=SIN(THR)
       SINX2=SIN(THR/2.0)
       COSX2=COS(THR/2.0)
       BORN(1)=-SINX*COSX2/SQ2*(FR(3)+FR(4))
       BORN(2)=-SQ2*COSX2*(FR(1)-FR(2))+SINX2*SINX*(FR(3)-FR(4))/SQ2
       BORN(3)=SINX2*SINX/SQ2*(FR(3)-FR(4))
       BORN(4)=SQ2*SINX2*(FR(1)+FR(2))+COSX2*SINX*(FR(3)+FR(4))/SQ2
C following factor gives Mainz h5,h5 times Q/omega so Xsect=rho*h**2
C see eprda for definitions. 7/21/99 RAA
      z=-wgm/1000.0/p/sqrt(2.0)
C     write(*,222) z
C 222 format(' SFCT(opec)=',f9.5)
       BORN(5)=-SQ2*COSX2*(FR(6)+FR(5))*Z
       BORN(6)=SQ2*SINX2*(FR(6)-FR(5))*Z
C BORNT USES A CUTTOFF- SUPPRESS FOR SAID
       IF(W.LT.5.3) GO TO 99
       FCUT=CUT**2/(CUT**2+(W-5.3)**2)
       DO 75 I=1,6
       BORN(I)=BORN(I)*FCUT
   75  CONTINUE
   99  RETURN
      END
C ***************************************************************
      SUBROUTINE EPROPEC(WCM,WGM,EX)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION RN(8),RP(8),T(8,2),QT(12),QU(12),EM(9,2,6),GG(3,2)
     C,CIS(18),EX(9,2,6)
      DATA SQ2,WX,WN,GN/1.41421,134.963,938.2796,62.28/
C GN=SQRT(ALFA*G2)*HBARC = SQRT(13.75/137)*197.32 = 62.51
      DATA CIS/1.0,0.0,-1.0,0.33333,1.0,0.66666,-0.33333,1.0,-0.66666
     C,1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0/
      DATA WCML,WGML,CUT/0.0,0.0,400.0/
C from Berends, Donnachie, and Weaver, NPB4, 1(67) with ADDED PV term
      SAVE
C M=1..6 for E(3/2), M(3/2), pE(1/2), pM(1/2), nE(1/2), nM(1/2)
C M=7..9 FOR S(3/2), pS(1/2), nS(1/2) ; Amplitudes in mFm
      IF(WGM.NE.WGML) WCML=0.0
      IF(WCM.EQ.WCML) GO TO 10
      WGML=WGM
      WCML=WCM
C     GN=197.32*SQRT(GPI2/137.0)
      GN=197.32/SQRT(137.0)
      DO 1 M=1,9
      DO 1 J=1,2
      DO 1 L=1,6
    1 EM(M,J,L)=0.0
      GX=GN*WN/4.0/WCM
      CALL OPEKIN(WCM,WGM,S,E1,ZK,E2,Q,ZK0,Q0)
      ZT=(2.0*ZK0*Q0+WGM**2)/2.0/ZK/Q
      ZU=(2.0*ZK0*E2+WGM**2)/2.0/ZK/Q
      E1P=E1+WN
      E2P=E2+WN
      WP=WCM+WN
      WM=WCM-WN
      QK=Q/ZK
      CALL OPEFF(WGM,F1V,F1S,F2V,F2S,FPI,GTX)
   15 CALL QJOFX(QT,ZT,9)
      CALL QJOFX(QU,ZU,9)
C GET RN, RP, T(J)  J=1(J=L-1/2), J=2(J=L+1/2)
      Z=WP/E1P/E2P/WM
      SN=1.0
      SF=1.0
      DO 2 L=1,8
      QM=0.0
      IF(L.GT.1) QM=QU(L-1)
      RN(L)=SN*(QU(L+1)-QM)/SF
      T(L,1)=SN*(QU(L)/ZK/Q-Z*QM)
      T(L,2)=SN*(QU(L)/ZK/Q-Z*QU(L+1))
      QM=0.0
      IF(L.GT.1) QM=QT(L-1)
      RP(L)=(QT(L+1)-QM)/SF
      SF=SF+2.0
    2 SN=-SN
      Z=WM*SQRT(E1P*E2P)*1000.0/WN
C   Do in ISOSPIN order 3/2, 1/2p, 1/2n
      I=0
    4 I=I+1
      IF(I.EQ.4) GO TO 10
      K=3*I-2
      G1=(F1V*(CIS(K)+CIS(K+2))+F1S*CIS(K+1))*GX
      G3=(F2V*(CIS(K)+CIS(K+2))+F2S*CIS(K+1))*GX
      EG1=(F1V*(CIS(K)-CIS(K+2))+F1S*CIS(K+1))*GX
      EG3=(F2V*(CIS(K)-CIS(K+2))+F2S*CIS(K+1))*GX
C EPV adds in pseudo-vector coupling as in SAID PR programs 2/28/96 Arnd
      EPV=-2.0*(CIS(K)*F2V+CIS(K+1)*F2S)*GX
      EPVS=EPV*1000.0/WN**2
      GT=GX*GTX*CIS(K+2)
      C1=2.0*(G1-EG1)+GT
      C2=2.0*(EG1-WP*EG3)
      C3=2.0*(EG1+WM*EG3)
      C4=EG1-2.0*WN*EG3
      C5=(2.0*Q0-WCM)*EG1-WN*(2*Q0-ZK0)*EG3
      C6=WN*EG1+(ZK0*WCM+WGM**2-WX**2)*EG3
      ZL=0.0
      SN=1.0
      KE=2*I-1
      KM=KE+1
      KS=6+I
      IF(I.LT.4) GO TO 11
      KE=1
      KM=2
      KS=7
   11 DO 3 L=1,6
      ZLP=ZL+1.0
      SN=-SN
C GET EL+ = M(KE,2,L)
      A1=-C4*T(L,2)
      A2=-C1*(ZL*RP(L)/E1P-ZLP*RP(L+1)*QK/E2P)/WM
      A3=(C3*QK*ZLP*RN(L+1)/E2P-C2*ZL*RN(L)/E1P)/WM
      AA=A1+A2+A3
      A4=0.0
      IF(L.EQ.1) A4=(G1+WM*G3-WP*EG3)*2.0/WP/WM+EPV/WN
      EM(KE,2,L)=(AA+A4)*Z/ZLP
C GET ML+ = M(KE,2,L)
      IF(L.EQ.1) GO TO 8
      A2=C1*RP(L)/E1P/WM
      A3=C2*RN(L)/E1P/WM
      AA=A1+A2+A3
      EM(KM,2,L)=AA*Z/ZLP
C NEXT GET SL+ = M(KS,2,L)
    8 AA=0.0
      A4=0.0
      IF(WGM.EQ.0.0) GO TO 5
      A1=C1/2.0*(2.0*Q0-ZK0)
      A1=A1*(QT(L)/Q/E1P-QT(L+1)/ZK/E2P)/WM
      A2=SN*C5*(QU(L)/Q/E1P+QU(L+1)/ZK/E2P)/WM
      A3=SN*C6*(QU(L)/Q/E1P-QU(L+1)/ZK/E2P)/WM
      AA=A1+A2+A3
      IF(L.GT.1) GO TO 5
      A4=E1P*G3-G1+WP*EG3+ZK0*WP*GT/2.0/WGM**2
C     A4=2.0*ZK*A4/E1P/WP/WM+EPVS/WN*ZK/WM
      A4=2.0*ZK*A4/E1P/WP/WM
      a4=a4+epvs*(E1+WN)*ZK*SQRT(E2P/E1P)/Z
    5 EM(KS,2,L)=(AA+A4)/ZLP*Z
C GET EL- = M(KE,1,L)
      IF(L.EQ.1) GO TO 3
      IF(L.EQ.2) GO TO 9
      A1=-C4*T(L,1)
      A2=C1*(ZLP*RP(L)/E1P-Q*ZL*RP(L-1)/ZK/E2P)/WM
      A3=C2*ZLP/E1P*RN(L)/WM-C3*ZL*QK*RN(L-1)/E2P/WM
      EM(KE,1,L)=(A1+A2+A3)/ZL*Z
C GET ML- = M(KM,1,L)
    9 A1=C4*T(L,1)
      A2=-C1*RP(L)/E1P/WM
      A3=-C2*RN(L)/E1P/WM
      AA=A1+A2+A3
      A4=0.0
      IF(L.NE.2) GO TO 7
      A4=2.0*Q*ZK/E1P/E2P/WM**2*(WP*G3-G1-WM*EG3)
      pvf=WP*ZK*Q/SQRT(E1P*E2P)*1000.0/WN/wn
      depv=epv*pvf/z
      a4=a4-depv
    7 EM(KM,1,L)=(AA+A4)/ZL*Z
C GET SL- = M(KS,1,L)
      IF(WGM.EQ.0.0) GO TO 3
      A1=(2.0*Q0-ZK0)*C1/2.0*(QT(L)/Q/E1P-QT(L-1)/ZK/E2P)/WM
      A2=SN*(QU(L)/Q/E1P+QU(L-1)/ZK/E2P)*C5/WM
      A3=SN*C6/WM*(QU(L)/Q/E1P-QU(L-1)/ZK/E2P)
      AA=A1+A2+A3
      A4=0.0
      IF(L.NE.2) GO TO 6
      A4=G1+(E1-WN)*G3+WM*EG3-ZK0*WM*GT/2.0/WGM**2
      A4=2.0*Q/E2P*A4/WM**2
      a4=a4-epvs*(E1-WN)*Q*SQRT(E1P/E2P)/Z
    6 EM(KS,1,L)=(AA+A4)*Z/ZL
    3 ZL=ZL+1.0
      GO TO 4
   10 FCT=SQRT(GPI2)
      IF(WCM.GT.5300) FCT=FCT*CUT**2/(CUT**2+(WCM-5300.0)**2)
      DO 12 M=1,9
      DO 12 J=1,2
      DO 12 L=1,6
   12 EX(M,J,L)=EM(M,J,L)*FCT
   99 RETURN
      END
C ************************************************************
      SUBROUTINE OPEKIN(WCM,WGM,S,E1,P,E2,Q,P0,Q0)
C get kinematics for EPROPEC, EHOPEC WGM(MeV), WCM(MeV or GeV)
      DATA XMP,XMPI/0.9382796,0.134963/
      W=WCM
      IF(W.GT.100.0) W=W/1000.0
      S=W**2
      Q2=(WGM/1000.0)**2
      E1=(S+XMP**2+Q2)/2.0/W
      P=SQRT(E1**2-XMP**2)
      E2=(S-XMPI**2+XMP**2)/2.0/W
      IF(E2.LT.XMP) GO TO 99
      Q=SQRT(E2**2-XMP**2)
      P0=W-E1
      Q0=W-E2
      IF(WCM.LT.100.0) GO TO 99
      S=S*1.0E6
      E1=E1*1000.0
      P=P*1000.0
      E2=E2*1000.0
      Q=Q*1000.0
      W=WCM
   99 P0=W-E1
      Q0=W-E2
      RETURN
      END
C ***************************************************************
      SUBROUTINE OPEFF(WGM,F1V,F1S,F2V,F2S,FPI,GTX)
      DATA WN,UP,UN/938.256,1.793,-1.913/
      WG2=(WGM/1000.0)**2
      CAP=(WGM/WN/2.0)**2
      GEP=1.0/(1.0+WG2*(3.04+WG2*(1.54+WG2*0.068)))/(1.0+CAP)
      F1P=GEP*(1.0+CAP*(1.0+UP))
      F1N=GEP*UN*CAP
      F2P=GEP*UP/2.0/WN
      F2N=GEP*UN/2.0/WN
      FPI=1.0/(1.0+2.0*WG2)
      F1V=F1P-F1N
      F1S=F1P+F1N
      F2V=F2P-F2N
      F2S=F2P+F2N
      F2V=-F2V/2.0
      F2S=-F2S/2.0
      GTX=2.648-(1.0+UP-UN)/2.0*(1000.0/WN)**2
      IF(WG2.NE.0.0) GTX=2.0*(FPI-F1V)/WG2
      F1V=F1V/2.0
      F1S=F1S/2.0
      RETURN
      END
C **********************************************
      SUBROUTINE EPROMG(WCM,WGM,EX)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION F(6,8),EX(9,2,6),EM(3,2,6),QT(10)
      DATA WX,WN,WO,PI/134.96,938.280,782.6,3.1415927/
      DATA WGMM,WCMM,GOMM/-20.0,-20.0,0.0/
      DATA GWPG/3.06765/
      SAVE
C M=1..6 for E(3/2), M(3/2), pE(1/2), pM(1/2), nE(1/2), nM(1/2)
C M=7..9 FOR S(3/2), pS(1/2), nS(1/2) ; Amplitudes in mFm
      GOMM=GOM1
      DO 1 M=1,3
      DO 1 J=1,2
      DO 1 L=1,6
    1 EM(M,J,L)=0.0
      IF(GOM1.EQ.0.0) GO TO 10
C     GX=GOM1*197.32/0.1395*0.36*SQRT(4.0*PI/137.0)/4.0/PI
      GX=GWPG*GOM1
C THIS SETS COUPLING TO VALUE USED BY PROPEC 5/31/96
      S=WCM**2
      E1=(S+WN**2+WGM**2)/2.0/WCM
      ZK=SQRT(E1**2-WN**2)
      E2=(S+WN**2-WX**2)/2.0/WCM
      IF(E2.LT.WN) GO TO 10
      Q=SQRT(E2**2-WN**2)
      Q0=WCM-E2
      ZK0=WCM-E1
      ZT=(2.0*ZK0*Q0+WGM**2+WO**2-WX**2)/2.0/ZK/Q
      E1P=E1+WN
      E2P=E2+WN
      E1M=E1-WN
      E2M=E2-WN
      WP=WCM+WN
      WM=WCM-WN
      WG2=WGM**2/1.E6
C VERTEX FUNCTIONS GV(N,I) N=1(GV1), 2(GV3), 3(GVT)  I=1(+), 2(-), 3(0)
C Note!  Vertex functions carry the coupling constant (e*g) and hbarc
      CAP=(WGM/WN/2.0)**2
      GEP=1.0/(1.0+WG2*(3.04+WG2*(1.54+WG2*0.068)))/(1.0+CAP)
      GX=GX*GEP/WCM
      CALL QJOFX(QT,ZT,8)
      F1=(WCM*(E1M+E2M)+WO**2/2.0)/SQRT(E1M*E2M)
      F2=(WCM*(E1P+E2P)+WO**2/2.0)/SQRT(E1P*E2P)
      F3=-WP*SQRT(E2P/E1P)
      F4=-WM*SQRT(E2M/E1M)
      F5=(WN*(E1-E2)-WO**2/2.0)/SQRT(E2P*E1M)
      F6=-(WN*(E1-E2)+WO**2/2.0)/SQRT(E1P*E2M)
      DO 2 L=1,8
      Z=QT(L)*GX
      F(1,L)=F1*Z
      F(2,L)=F2*Z
      F(3,L)=F3*Z
      F(4,L)=F4*Z
      F(5,L)=F5*Z
      F(6,L)=F6*Z
      IF(L.GT.1) GO TO 2
C     IF(L.EQ.1) GO TO 2
      Z=GX*ZK*Q
      F(1,L)=F(1,L)-Z/SQRT(E1M*E2M)
      F(2,L)=F(2,L)-Z/SQRT(E1P*E2P)
      F(5,L)=F(5,L)+Z/SQRT(E2P*E1M)
      F(6,L)=F(6,L)+Z/SQRT(E1P*E2M)
    2 CONTINUE
C     WRITE(*,222) WCM,((F(K,L),L=1,4),K=1,4)
C 222 FORMAT(' WC=',F7.2,4F10.4,3(/11X,4F10.4))
      DO 3 L=1,6
      ZL=L-1
      F3M=0.0
      IF(L.GT.1) F3M=F(3,L-1)
      F3X=(F(3,L+1)-F3M)/(2.0*ZL+1.0)
      Z=F(1,L)-F(2,L+1)-ZL*F3X-(ZL+1.0)/(2.0*ZL+3.0)*(F(4,L+2)-F(4,L))
      EM(1,2,L)=Z/(ZL+1.0)
      EM(3,2,L)=(F(5,L+1)+F(6,L))/(ZL+1.0)
      EM(3,1,L)=F(6,L)
      EM(1,1,L)=0.0
      EM(2,1,L)=0.0
      EM(2,2,L)=0.0
      IF(L.EQ.1) GO TO 3
      EM(3,1,L)=(F(6,L)+F(5,L-1))/ZL
      EM(2,1,L)=(F(2,L-1)-F(1,L)-F3X)/ZL
      EM(2,2,L)=(F(1,L)-F(2,L+1)+F3X)/(ZL+1.0)
      IF(L.LT.3) GO TO 3
      Z=F(1,L)-F(2,L-1)+(ZL+1.0)*F3X+ZL*(F(4,L)-F(4,L-2))/(2.0*ZL-1.0)
      EM(1,1,L)=Z/ZL
    3 CONTINUE
   10 DO 4 I=1,3
      ZI=1.0
      IF(I.GE.2) ZI=1.0/3.0
      IF(I.EQ.3) ZI=-ZI
      IE=2*I-1
      IS=6+I
      DO 5 L=1,6
      DO 5 J=1,2
      EX(IE,J,L)=ZI*EM(1,J,L)+EX(IE,J,L)
      EX(IE+1,J,L)=ZI*EM(2,J,L)+EX(IE+1,J,L)
    5 EX(IS,J,L)=ZI*EM(3,J,L)+EX(IS,J,L)
    4 CONTINUE
   99 RETURN
      END
C ***************************************************
      SUBROUTINE EPRHOMG(WCM,WGM,TH,IR,BORN)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION F(6),BORN(6),CIS(7)
C GET CGLN F AMPLITUDES FOR OMEGA XCHG FROM WALECKA PR5 167, 1479(68)
      DATA CIS/1.0,0.0,0.0,1.0,1.0,0.333333,-0.333333/
      DATA WX,WN,WO,PI/134.96,938.280,782.6,3.1415927/
      DATA WGMM,WCMM,GOMM/-20.0,-20.0,0.0/
      DATA GWPG/3.06765/
      SAVE
C M=1..6 for E(3/2), M(3/2), pE(1/2), pM(1/2), nE(1/2), nM(1/2)
C M=7..9 FOR S(3/2), pS(1/2), nS(1/2) ; Amplitudes in mFm
      IF(WCMM.NE.WCM) WGMM=-20.0
      IF(WGM.EQ.WGMM) GO TO 10
      WCMM=WCM
      WGMM=WGM
      GOMM=GOM1
      DO 1 L=1,6
    1 F(L)=0.0
      IF(GOM1.EQ.0.0) GO TO 10
C     GX=GOM1*197.32/0.1395*0.36*SQRT(4.0*PI/137.0)/4.0/PI
      GX=GWPG*GOM1*2.0
C THIS SETS COUPLING TO VALUE USED BY PROPEC 5/31/96
      S=WCM**2
      E1=(S+WN**2+WGM**2)/2.0/WCM
      ZK=SQRT(E1**2-WN**2)
      E2=(S+WN**2-WX**2)/2.0/WCM
      IF(E2.LT.WN) GO TO 10
      Q=SQRT(E2**2-WN**2)
      Q0=WCM-E2
      ZK0=WCM-E1
      ZT=(2.0*ZK0*Q0+WGM**2+WO**2-WX**2)/2.0/ZK/Q
      E1P=E1+WN
      E2P=E2+WN
      E1M=E1-WN
      E2M=E2-WN
      WP=WCM+WN
      WM=WCM-WN
      WG2=WGM**2/1.E6
C VERTEX FUNCTIONS GV(N,I) N=1(GV1), 2(GV3), 3(GVT)  I=1(+), 2(-), 3(0)
C Note!  Vertex functions carry the coupling constant (e*g) and hbarc
      CAP=(WGM/WN/2.0)**2
      GEP=1.0/(1.0+WG2*(3.04+WG2*(1.54+WG2*0.068)))/(1.0+CAP)
      GX=GX*GEP/WCM
      F1=(WCM*(E1M+E2M)+WO**2/2.0)*SQRT(E1P*E2P)
      F2=(WCM*(E1P+E2P)+WO**2/2.0)/SQRT(E1P*E2P)*Q*ZK
      F3=-WP*SQRT(E2P/E1P)*Q*ZK
      F4=-WM*SQRT(E1P/E2P)*Q*Q
      F5=(WN*(E1-E2)-WO**2/2.0)*SQRT(E1P/E2P)*Q
      F6=-(WN*(E1-E2)+WO**2/2.0)/SQRT(E2P/E1P)*ZK
   10 C=0.0
      IF(IR.GT.0.AND.IR.LT.8) C=CIS(IR)*GX
      Z=COS(0.0174532*TH)
      ZZ=2.0*ZK*Q*(ZT-Z)
      BORN(1)=C*(F1/ZZ-SQRT(E1P*E2P)/2.0)+BORN(1)
      BORN(2)=C*(F2/ZZ-Q*ZK/2.0/SQRT(E1P*E2P))+BORN(2)
      BORN(3)=C*F3/ZZ+BORN(3)
      BORN(4)=C*F4/ZZ+BORN(4)
      BORN(5)=C*(F5/ZZ+Q*SQRT(E1P/E2P)/2.0)+BORN(5)
      BORN(6)=C*(F6/ZZ+ZK*SQRT(E2P/E1P)/2.0)+BORN(6)
   99 RETURN
      END
C ***********************************************************
      SUBROUTINE EPRBORN(WCM,WGM,EM,NF)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EM(9,2,6),EX(6,2,6)
      SAVE
      IF(GPI2.LT.1.0) GO TO 99
C Note!!!! we need the BORN partial waves!!!!
C     IF(GOM2.NE.0.0.OR.GP1.NE.0.0) GO TO 5
      IF(WGM.EQ.0.0) GO TO 5
      CALL EPROPEC(WCM,WGM,EM)
      IF(NF.LT.2) GO TO 99
      CALL EPROMG(WCM,WGM,EM)
      GO TO 99
    5 TL=(WCM**2-938.256**2)/2.0/938.256
C Take BORN terms from PHOTO-Prod
      CALL PRBORN(TL,EX,5)
      DO 2 M=1,9
      DO 2 J=1,2
      DO 2 L=1,6
      EM(M,J,L)=0.0
      IF(M.GT.6) GO TO 2
      EM(M,J,L)=EX(M,J,L)
    2 CONTINUE
   99 RETURN
      END
C ***********************************************************
      SUBROUTINE PRBORN(EPI,EM,NF)
      DIMENSION EM(6,2,6)
      SAVE
      CALL PROPEC(EPI,EM)
      CALL PRNPOL(EPI,EM)
      IF(NF.LT.3) GO TO 99
      CALL PREPV(EPI,EM)
      CALL PROMEGA(EPI,EM)
      IF(NF.LT.5) GO TO 99
      CALL PRRHO(EPI,EM)
   99 RETURN
      END
C ***********************************************************
      SUBROUTINE PROPEC(EL,EM)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),AA(4),BB(4),CC(4),F(12,4),QT(12)
     C,QU(12),EM(6,2,6)
      DATA SQ2,WP,WN,UP,UN,GN/1.41421,135.04,938.256,1.793,-1.913,62.51/
C GN=SQRT(ALFA*G2)*HBARC = SQRT(13.75/137)*197.32
      DATA B,GPI2M/0,13.75/
      SAVE
      IF(GPI2.EQ.GPI2M) GO TO 20
      IF(GPI2.EQ.0.0) GPI2=GPI2M
      GN=197.32*SQRT(GPI2/137.0)
      GPI2M=GPI2
   20 CONTINUE
      DO 1 M=1,6
      DO 1 J=1,2
      DO 1 L=1,6
    1 EM(M,J,L)=0.0
      IF(GPI2.LT.1.0) GO TO 99
      S=WN*(WN+2.0*EL)
      W=SQRT(S)
      ZK=EL/SQRT(1.0+2.0*EL/WN)
      Q=SQRT((S-(WN+WP)**2)*(S-(WN-WP)**2)/4.0/S)
      IF(Q.LT.0.0)  GO TO 99
      Z2=SQRT(Q**2+WN**2)
      ZU=-Z2/Q
      ZT=SQRT(Q**2+WP**2)/Q
      Z2=SQRT(Z2+WN)
      Z1=SQRT(SQRT(ZK**2+WN**2)+WN)
      GG=1000.0*GN/W
      USCL=(W+WN)/2.0/Z1/Z2
      AA(1)=0.0
      AA(2)=0.0
      AA(3)=GG*Z2/Z1
      AA(4)=-GG*Z1*Q/Z2/ZK
      BB(1)=-GG*W/Q/USCL/2.0
      BB(2)=GG*W/Z2**2/2.0/USCL
      BB(3)=-AA(3)
      BB(4)=-AA(4)
      GG=GG*USCL
      CC(1)=-GG*Z2**2/Q
      CC(2)=GG
      CC(3)=-GG*Z2**2/WN
      CC(4)=-GG*Q/WN
      CALL QJOFX(QT,ZT,8)
      CALL QJOFX(QU,-ZU,8)
      S=-1.0
      DO 11 L=1,8
      QU(L)=S*QU(L)
   11 S=-S
      M=0
      DO 2 I=1,3
      DO 3 K=1,4
      A=AA(K)
      IF(I.GT.1) A=2.0*A/3.0
      IF(I.NE.2) A=-A
      IF(I.EQ.1) B=CC(K)*(UN-UP)-BB(K)
      IF(I.EQ.2)  B=-(CC(K)*(2.0*UN+UP)+BB(K))/3.0
      IF(I.EQ.3) B=-(CC(K)*(2.0*UP+UN)+2.0*BB(K))/3.0
      DO 3 L=1,8
    3 F(L,K)=(A*QT(L)+B*QU(L))/2.0
      M=M+1
      L=2
    4 L=L+1
      IF(L.GT.6)  GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      EM(M,1,L)=E/ZL
      GO TO 4
    5 L=0
    6 L=L+1
      IF(L.GT.6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
      EE=ZL/(2.0*ZL+1.0)*(F(L-1,3)-F(L+1,3))
      E=E+EE
      EM(M,2,L)=E/(ZL+1.0)
      GO TO 6
    7 M=M+1
      L=1
    8 L=L+1
      IF(L.GT.6) GO TO 9
      ZL=L-1
      EM(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      GO TO 8
    9 L=1
   10 L=L+1
      IF(L.GT.6)  GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      EM(M,2,L)=E/(ZL+1.0)
      GO TO 10
    2 CONTINUE
   99 RETURN
      END
C **************************************************************
      SUBROUTINE PRNPOL(EL,EM)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EM(6,2,6)
      DATA SQ2,WP,WN,UP,UN,GN/1.41421,135.04,938.256,1.793,-1.913,62.51/
C GN=SQRT(ALFA*G2)*HBARC = SQRT(13.75/137)*197.32
C  ADD NUCLEON POLE TERMS TO OPEC EMS
      DATA GPI2M/13.75/
      SAVE
      IF(GPI2.EQ.GPI2M) GO TO 20
      IF(GPI2.EQ.0.0) GPI2=GPI2M
      GN=197.32*SQRT(GPI2/137.0)
      GPI2M=GPI2
   20 CONTINUE
      S=WN*(WN+2.0*EL)
      W=SQRT(S)
      ZK=EL/SQRT(1.0+2.0*EL/WN)
      Q=SQRT((S-(WN+WP)**2)*(S-(WN-WP)**2)/4.0/S)
      IF(Q.LT.0.0)  GO TO 99
      Z2=SQRT(Q**2+WN**2)
      ZU=-Z2/Q
      ZT=SQRT(Q**2+WP**2)/Q
      Z2=SQRT(Z2+WN)
      Z1=SQRT(SQRT(ZK**2+WN**2)+WN)
      GG=1000.0*GN/W/4.0/WN
      ZM=Z1*Z2
      ZD=Z1/Z2
      EM(1,2,1)=EM(1,2,1)+GG*ZM*(UP-UN)
      EM(2,1,2)=EM(2,1,2)+GG*(UP-UN)*Q*ZK/ZM
      UB=(2.0*UN+UP)/3.0
      EM(3,2,1)=EM(3,2,1)+GG*(2.0*ZM*WN/(W+WN)-UP*ZK/ZD+UB*ZM)
      EM(4,1,2)=EM(4,1,2)-GG*(2.0*Q*ZD*WN/(W+WN)+UP*Q*ZD-UB*Q*ZK/ZM)
      UB=(2.0*UP+UN)/3.0
      EM(5,2,1)=EM(5,2,1)+GG*(UB*ZM-UN*ZK/ZD)
      EM(6,1,2)=EM(6,1,2)+GG*(UB*Q*ZK/ZM-UN*Q*ZD)
   99 RETURN
      END
C ************************************************************
      SUBROUTINE PREPV(EL,EM)
C ADD EXTRA TERM FOR PV COUPLING
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      REAL MN,MPI,MUN,MUP,G
      DATA MN,MPI,PI,GPI2M/938.256,135.04,3.1415927,13.75/
      DATA MUP,MUN,CE,GN/1.793,-1.913,315.65,62.51/
C  G=SQRT(4*PI*G**2)   CE=1000*SQRT(ALFA*G**2)=1000*SQRT(13.65/137)
      DIMENSION EM(6,2,6)
      SAVE
      IF(GPI2.EQ.GPI2M) GO TO 20
      IF(GPI2.EQ.0.0) GPI2=GPI2M
      GN=197.32*SQRT(GPI2/137.0)
      GPI2M=GPI2
   20 CONTINUE
      CE=1000.0*GN
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      EM(1,2,1)=EM(1,2,1)+CE*(MUP-MUN)*Z1*Z2*(WC-MN)/8.0/WC/MN**2
      EM(3,2,1)=EM(3,2,1)
     &          +CE*(2.0*MUP+MUN)*Z1*Z2*(WC-MN)/12.0/WC/MN**2
      EM(5,2,1)=EM(5,2,1)
     &          +CE*(MUP+2.0*MUN)*Z1*Z2*(WC-MN)/12.0/WC/MN**2
      EM(2,1,2)=EM(2,1,2)
     &           -CE*(MUP-MUN)*(WC-MN)*RQ*Z1/Z2/8.0/WC/MN**2
      EM(4,1,2)=EM(4,1,2)
     &           -CE*(2.0*MUP+MUN)*(WC-MN)*RQ*Z1/Z2/12.0/WC/MN**2
      EM(6,1,2)=EM(6,1,2)
     &          -CE*(MUP+2.0*MUN)*(WC-MN)*RQ*Z1/Z2/12.0/WC/MN**2
      RETURN
      END
C **************************************************
      SUBROUTINE PROMEGA(EL,EM)
C*
C* PROGRAM TO CALCULATE T-CHANEL OMEGA CHANGE TERM
C* THE CORRESPONDING EXEC FILE IS GOOS
      IMPLICIT REAL (A-H,O-Z)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),ET(2,2,6),EM(6,2,6),F(12,4),WI(3)
      REAL QL(12),MN,MPI,C,BB(4),ZL,ZZ,MW
      REAL EL,E1,E2,PI,LW,G1W,G2W,G1,G2,E
      DATA PI,FG/3.1415926,154.05/
C FG=0.36*SQRT(4*PI*ALPHA)*HBARC/MPI = 0.36*197.32/0.13965*0.30286 (MFM?
C NOTE: FOR RHO EXCHANGE THE LR/LW=1/3 FACTOR IS IN WI
C OMEGA MULTIPOLES ARE FROM BERENDS, DONNACHIE, WEAVER, NPB4, 103(67)
C RHO MULTIPOLES ARE FROM OLSSON, PRD17, 174(78); USES T IN A1
      DATA WI/1.0,0.33333,-0.33333/
C     DATA WI/0.0,0.333333,0.333333/
      DATA MN,MPI,MW/938.256,135.04,782.6/
      DATA B/0/
      SAVE
      G1W=GOM1
      G2W=GOM2
      IF(G1W.EQ.0.0.AND.G2W.EQ.0.0) GO TO 99
      G1=FG*G1W
      G2=FG*G2W
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      C=(WC-MN)/(8.0*PI*WC)
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      BW=(MW**2+2.0*RK*EPI-MPI**2)/(2.0*RK*RQ)
      C1=C*G1
      C2=C*G2
      BB(1)=C1*Z1*Z2/RK/RQ*(WC-MN-RK*EPI/(WC-MN)+RK*RQ*BW/(WC-MN))
      BB(2)=C1*Z1/Z2/RK*(WC+MN-RK*EPI/(WC+MN)+RK*RQ*BW/(WC+MN))
      BB(3)=-C1*Z1*Z2/RK
      BB(4)=-C1*Z1/Z2*RQ/RK
      BB(1)=BB(1)-C2*Z1*Z2*MW**2/(2.0*RQ*RK*MN)
      BB(2)=BB(2)+C2*(Z1/Z2)*MW**2/(2.0*RK*MN)
      BB(3)=BB(3)+C2*Z1*Z2*(WC-MN)/(RK*2.0*MN)
      BB(4)=BB(4)-C2*Z1/Z2*RQ*(WC+MN)/(2.0*MN*RK)
      CALL QJOFX(QL,BW,8)
      DO 1, M=1,2
      DO 1, J=1,2
      DO 1, L=1,6
    1 ET(M,J,L)=0.0
      M=1
      DO 3 K=1,4
      DO 3 L=1,8
    3 F(L,K)=QL(L)/2.0*BB(K)
      L=2
    4 L=L+1
      IF (L .GT. 6) GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      ET(M,1,L)=E/ZL
      GO TO 4
    5 L=0
    6 L=L+1
      IF (L .GT. 6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
      EE=ZL/(2.0*ZL+1.0)*(F(L-1,3)-F(L+1,3))
      E=E+EE
      ET(M,2,L)=E/(ZL+1.0)
      GO TO 6
    7 M=2
      L=1
    8 L=L+1
      IF (L .GT. 6) GO TO 9
      ZL=L-1
      ET(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      GO TO 8
    9 L=1
   10 L=L+1
      IF (L .GT. 6) GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      ET(M,2,L)=E/(ZL+1.0)
      GO TO 10
    2 ET(1,2,1)=ET(1,2,1)-0.5*C1*Z1*Z2/(WC-MN)
      ET(2,1,2)=ET(2,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)
C THE FOLLOWING DELTA-FUNCTIONS CHANGE FORM OF A1 -> A1*T/MW**2
C     ET(1,2,1)=ET(1,2,1)+C2*Z1*Z2/MN/2.0
C     ET(2,1,2)=ET(2,1,2)-C2*Z1/Z2*RQ/MN/2.0
      M=0
      DO 12 I=1,3
      Z=WI(I)
      DO 13 K=1,2
      M=M+1
      DO 13 L=1,6
      DO 13 J=1,2
   13 EM(M,J,L)=EM(M,J,L)+Z*ET(K,J,L)
   12 CONTINUE
   99 RETURN
      END
C ********************************************************
      SUBROUTINE PRRHO(EL,EM)
C*
C* PROGRAM TO CALCULATE T-CHANEL OMEGA CHANGE TERM
C* THE CORRESPONDING EXEC FILE IS GOOS
      IMPLICIT REAL (A-H,O-Z)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),ET(2,2,6),EM(6,2,6),F(12,4),WI(3)
      REAL QL(12),MN,MPI,C,BB(4),ZL,ZZ,MW
      REAL EL,E1,E2,PI,LW,G1W,G2W,G1,G2,E
      DATA PI,FG/3.1415926,154.05/
C FG=0.36*SQRT(4*PI*ALPHA)*HBARC/MPI = 0.36*197.32/0.13965*0.30286 (MFM?
C NOTE: FOR RHO EXCHANGE THE LR/LW=1/3 FACTOR IS IN WI
C OMEGA MULTIPOLES ARE FROM BERENDS, DONNACHIE, WEAVER, NPB4, 103(67)
C RHO MULTIPOLES ARE FROM OLSSON, PRD17, 174(78); USES T IN A1
C     DATA WI/1.0,0.33333,-0.33333/
      DATA WI/0.0,0.333333,0.333333/
      DATA MN,MPI,MW/938.256,135.04,770.0/
      DATA B/0/
      SAVE
      G1W=GP1
      G2W=GP2
      IF(G1W.EQ.0.0.AND.G2W.EQ.0.0) GO TO 99
      G1=FG*G1W
      G2=FG*G2W
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      C=(WC-MN)/(8.0*PI*WC)
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      BW=(MW**2+2.0*RK*EPI-MPI**2)/(2.0*RK*RQ)
      C1=C*G1
      C2=C*G2
      BB(1)=C1*Z1*Z2/RK/RQ*(WC-MN-RK*EPI/(WC-MN)+RK*RQ*BW/(WC-MN))
      BB(2)=C1*Z1/Z2/RK*(WC+MN-RK*EPI/(WC+MN)+RK*RQ*BW/(WC+MN))
      BB(3)=-C1*Z1*Z2/RK
      BB(4)=-C1*Z1/Z2*RQ/RK
      BB(1)=BB(1)-C2*Z1*Z2*MW**2/(2.0*RQ*RK*MN)
      BB(2)=BB(2)+C2*(Z1/Z2)*MW**2/(2.0*RK*MN)
      BB(3)=BB(3)+C2*Z1*Z2*(WC-MN)/(RK*2.0*MN)
      BB(4)=BB(4)-C2*Z1/Z2*RQ*(WC+MN)/(2.0*MN*RK)
      CALL QJOFX(QL,BW,8)
      DO 1, M=1,2
      DO 1, J=1,2
      DO 1, L=1,6
    1 ET(M,J,L)=0.0
      M=1
      DO 3 K=1,4
      DO 3 L=1,8
    3 F(L,K)=QL(L)/2.0*BB(K)
      L=2
    4 L=L+1
      IF (L .GT. 6) GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      ET(M,1,L)=E/ZL
      GO TO 4
    5 L=0
    6 L=L+1
      IF (L .GT. 6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
      EE=ZL/(2.0*ZL+1.0)*(F(L-1,3)-F(L+1,3))
      E=E+EE
      ET(M,2,L)=E/(ZL+1.0)
      GO TO 6
    7 M=2
      L=1
    8 L=L+1
      IF (L .GT. 6) GO TO 9
      ZL=L-1
      ET(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      GO TO 8
    9 L=1
   10 L=L+1
      IF (L .GT. 6) GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      ET(M,2,L)=E/(ZL+1.0)
      GO TO 10
    2 ET(1,2,1)=ET(1,2,1)-0.5*C1*Z1*Z2/(WC-MN)
      ET(2,1,2)=ET(2,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)
C THE FOLLOWING DELTA-FUNCTIONS CHANGE FORM OF A1 -> A1*T/MW**2
      ET(1,2,1)=ET(1,2,1)+C2*Z1*Z2/MN/2.0
      ET(2,1,2)=ET(2,1,2)-C2*Z1/Z2*RQ/MN/2.0
      M=0
      DO 12 I=1,3
      Z=WI(I)
      DO 13 K=1,2
      M=M+1
      DO 13 L=1,6
      DO 13 J=1,2
   13 EM(M,J,L)=EM(M,J,L)+Z*ET(K,J,L)
   12 CONTINUE
   99 RETURN
      END
C **********************************************************
      SUBROUTINE PROMEGX(EL,EM)
C*
C* PROGRAM TO CALCULATE T-CHANEL OMEGA CHANGE TERM
C* THE CORRESPONDING EXEC FILE IS GOOS
      IMPLICIT REAL (A-H,O-Z)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),ET(6,2,6),EM(6,2,6),F(12,4)
      REAL QL(12),MN,MPI,C,BB(4),ZL,ZZ,MW
      REAL EL,E1,E2,PI,LW,G1W,G2W,G1,G2,E
      DATA PI/3.1415926/
      DATA MN,MPI,MW/938.256,135.04,782.6/
      DATA G1W,G2W/16.0,0.0/
      DATA B/0/
      SAVE
C     IF(GOMS.NE.27) GO TO 20
C     GOMS=28.0
      G1W=GOM1
      G2W=GOM2
   20 IF(G1W.EQ.0.0) GO TO 99
      LW=0.36
      LW=LW*SQRT(4.0*PI*1.0/137.0)
      G1=197.3*LW*G1W/0.1395
      G2=197.3*LW*G2W/0.1395
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      C=(WC-MN)/(8.0*PI*WC)
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      BW=(MW**2+2.0*RK*EPI-MPI**2)/(2.0*RK*RQ)
      C1=C*G1
      C2=C*G2
      BB(1)=C1*Z1*Z2/RK/RQ*(WC-MN-RK*EPI/(WC-MN)+RK*RQ*BW/(WC-MN))
      BB(2)=C1*Z1/Z2/RK*(WC+MN-RK*EPI/(WC+MN)+RK*RQ*BW/(WC+MN))
      BB(3)=-C1*Z1*Z2/RK
      BB(4)=-C1*Z1/Z2*RQ/RK
      BB(1)=BB(1)-C2*Z1*Z2*MW**2/(2.0*RQ*RK*MN)
      BB(2)=BB(2)+C2*(Z1/Z2)*MW**2/(2.0*RK*MN)
      BB(3)=BB(3)+C2*Z1*Z2*(WC-MN)/(RK*2.0*MN)
      BB(4)=BB(4)-C2*Z1/Z2*RQ*(WC+MN)/(2.0*MN*RK)
      CALL QJOFX(QL,BW,8)
      DO 1, M=1,6
      DO 1, J=1,2
      DO 1, L=1,6
    1 ET(M,J,L)=0.0
      M=0
      DO 2 I=1,3
      DO 3 K=1,4
      IF (I .EQ. 1) B=BB(K)
      IF (I .EQ. 2) B=BB(K)/3.0
      IF (I .EQ. 3) B=-BB(K)/3.0
      DO 3 L=1,8
    3 F(L,K)=B*QL(L)/2.0
      M=M+1
      L=2
    4 L=L+1
      IF (L .GT. 6) GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      ET(M,1,L)=E/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 4
    5 L=0
    6 L=L+1
      IF (L .GT. 6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
      EE=ZL/(2.0*ZL+1.0)*(F(L-1,3)-F(L+1,3))
      E=E+EE
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 6
    7 M=M+1
      L=1
    8 L=L+1
      IF (L .GT. 6) GO TO 9
      ZL=L-1
      ET(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 8
    9 L=1
   10 L=L+1
      IF (L .GT. 6) GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 10
    2 CONTINUE
      EM(1,2,1)=EM(1,2,1)-0.5*C1*Z1*Z2/(WC-MN)
      EM(2,1,2)=EM(2,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)
      EM(3,2,1)=EM(3,2,1)-0.5*C1*Z1*Z2/(WC-MN)/3.0
      EM(4,1,2)=EM(4,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)/3.0
      EM(5,2,1)=EM(5,2,1)+0.5*C1*Z1*Z2/(WC-MN)/3.0
      EM(6,1,2)=EM(6,1,2)+0.5*C1*Z1/Z2*RQ/(WC+MN)/3.0
   99 RETURN
      END
C *****************************************
      SUBROUTINE PRRHX(EL,EM)
      IMPLICIT REAL (A-H,O-Z)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),ET(6,2,6),EM(6,2,6),F(12,4)
      REAL QL(12),MN,MPI,C,BB(4),ZL,ZZ,MP
      REAL EL,E1,E2,PI,LP,GP1,GP2,G1,G2,E
      DATA MN,MPI,MP,FG/938.256,135.04,770.0,51.35/
C FG=0.12*SQRT(4*pi*alpha)*Hbarc/mpi = 0.12*197.32/0.13965*0.30286 (mFm?
      DATA B,LP,PI/0,0.12,3.1415927/
      SAVE
      IF(GP1.EQ.0.0.AND.GP2.EQ.0.0) GO TO 99
C     PI=4.0*ATAN(1.0)
C     LP=0.12
C     LP=LP*SQRT(4.0*PI*1.0/137.0)
C     G1=197.3*LP*GP1/0.1395
C     G2=197.3*LP*GP2/0.1395
C     G2=G1*GP2
      G1=FG*GP1
      G2=FG*GP2
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      C=(WC-MN)/(8.0*PI*WC)
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      BW=(MP**2+2.0*RK*EPI-MPI**2)/(2.0*RK*RQ)
      C1=C*G1
      C2=C*G2
      BB(1)=C1*Z1*Z2/RK/RQ*(WC-MN-RK*EPI/(WC-MN)+RK*RQ*BW/(WC-MN))
      BB(2)=C1*Z1/Z2/RK*(WC+MN-RK*EPI/(WC+MN)+RK*RQ*BW/(WC+MN))
      BB(3)=-C1*Z1*Z2/RK
      BB(4)=-C1*Z1/Z2*RQ/RK
      BB(1)=BB(1)+C2*Z1*Z2*MP**2/(2.0*RQ*RK*MN)
      BB(2)=BB(2)-C2*(Z1/Z2)*MP**2/(2.0*RK*MN)
      BB(3)=BB(3)+C2*Z1*Z2*(WC-MN)/(RK*2.0*MN)
      BB(4)=BB(4)-C2*Z1/Z2*RQ*(WC+MN)/(2.0*MN*RK)
      CALL QJOFX(QL,BW,8)
      DO 1, M=1,6
      DO 1, J=1,2
      DO 1, L=1,6
    1 ET(M,J,L)=0.0
      M=0
      DO 2 I=1,3
      DO 3 K=1,4
      IF (I .EQ. 1) B=0.0
      IF (I .EQ. 2) B=BB(K)
      IF (I .EQ. 3) B=BB(K)
      DO 3 L=1,8
    3 F(L,K)=B*QL(L)/2.0
      M=M+1
      L=2
    4 L=L+1
      IF (L .GT. 6) GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      ET(M,1,L)=E/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 4
    5 L=0
    6 L=L+1
      IF (L .GT. 6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
      EE=ZL/(2.0*ZL+1.0)*(F(L-1,3)-F(L+1,3))
      E=E+EE
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 6
    7 M=M+1
      L=1
    8 L=L+1
      IF (L .GT. 6) GO TO 9
      ZL=L-1
      ET(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 8
    9 L=1
   10 L=L+1
      IF (L .GT. 6) GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 10
    2 CONTINUE
      EM(3,2,1)=EM(3,2,1)-0.5*C1*Z1*Z2/(WC-MN)-0.5*C2*Z1*Z2/MN
      EM(4,1,2)=EM(4,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)+0.5*C2*Z1/Z2*RQ/MN
      EM(5,2,1)=EM(5,2,1)-0.5*C1*Z1*Z2/(WC-MN)-0.5*C2*Z1*Z2/MN
      EM(6,1,2)=EM(6,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)+0.5*C2*Z1/Z2*RQ/MN
   99 RETURN
      END
C ********************************************************
      FUNCTION RFM(J,A,HRX,HIX)
C get Mainz response functions from those of Knochlein 11/99 RAA
      DIMENSION HIX(6),HRX(6),N(36)
      DATA N/0,0,0,6,7,-4,-5,0,0,11,-10,0,0,0,0,17,-16
     C,19,-18,0,0,0,24,-23,0,0,0,0,30,-29,32,-31,0,0,36,-35/
      RFM=RF(J,HRX,HIX)
      IF(N(J).EQ.0) GO TO 99
      C=COS(0.0174532*A)
      S=SQRT(1.0-C**2)
      RFM=RFM*C
      M=N(J)
      IF(M.LT.0) S=-S
      IF(M.LT.0) M=-M
      RFM=RFM+S*RF(M,HRX,HIX)
   99 RETURN
      END
C ********************************************************
      FUNCTION RF(J,HR,HI)
C get Response functions from Knochlein, Drechsel, Tiator
      DIMENSION HR(6),HI(6),I0(36),IJK(146)
      DATA IJK/0,11,22,33,44,1,-12,-34,1,13,24,0,14,23,0,13,-24,0,12,-34
     C,0,11,-22,-33,44,0,55,66,1,-56,0,-55,66,0,56,0,51,-54,62,63
     C,1,-52,53,-61,-64,1,-52,-53,61,-64,1,-51,-54,62,-63,1,52,-53,-61
     C,-64,1,-51,-54,-62,63,0,-51,54,62,63,0,52,53,61,-64,0,-14,23
     C,1,13,-24,1,-14,-23,1,12,-34,1,-14,23,1,-51,54,-62,-63,0,52,-53
     C,61,64,0,-52,-53,61,-64,0,51,54,-62,63,0,-52,53,61,64,0,51,54,62
     C,-63,1,51,-54,-62,-63,1,-52,-53,-61,64,0,12,34,0,11,-22,33,-44
     C,0,13,24,0,11,22,-33,-44/
      DATA I0/1,6,9,12,15,18,21,26,29,31,34,36,41,46,51,56,61,66,71,76
     C,79,82,85,88,91,96,101,106,111,116,121,126,131,134,139,142/
      RF=0.0
      IF(J.LT.1.OR.J.GT.36) GO TO 99
      I=I0(J)
      IR=IJK(I)
    1 I=I+1
      MN=IJK(I)
      S=1.0
      IF(MN.LT.0) S=-1.0
      IF(MN.LT.0) MN=-MN
      IF(MN.LT.10) GO TO 3
      M=MN/10
      N=MN-10*M
      IF(IR.EQ.1) GO TO 2
      RF=RF+S*(HR(M)*HR(N)+HI(M)*HI(N))
C     write(*,222) j,i,ir,mn,s,hr(m),hi(m),hr(n),hi(n),rf
C 222 format(' j,i,ir,mn=',4i3,' s=',f3.0,5f9.4)
      GO TO 1
    2 RF=RF+S*(HR(M)*HI(N)-HR(N)*HI(M))
      GO TO 1
    3 IF(J.EQ.1.OR.J.EQ.7) RF=RF/2.0
      IF(J.EQ.34.OR.J.EQ.36) RF=RF/2.0
      IF(J.EQ.9.OR.J.EQ.11) RF=2.0*RF
      IF(J.GT.11.AND.J.LT.20) RF=RF/SQRT(2.0)
      IF(J.GT.24.AND.J.LT.33) RF=RF/SQRT(2.0)
   99 RETURN
      END
C ********************************************************
      FUNCTION IMJL(MJL,M,J,L)
C DECODE MJL FOR PARTIAL WAVE IDENTIFIERS
      N=MJL/100
      IMJL=0
      IF(MJL.GT.1000) IMJL=MJL-100*N
      IF(MJL.LT.1000) N=MJL
      M=N/100
      JL=N-100*M
      J=JL/10
      L=JL-10*J
      IF(M.LT.1.OR.M.GT.9) IMJL=-1
      IF(J.NE.1.AND.J.NE.2) IMJL=-1
      IF(L.LT.0.OR.L.GT.6) IMJL=-1
      IF(L.EQ.0.AND.J.EQ.1) IMJL=-1
      IF(M.GT.6) GO TO 99
      MEO=M/2
      MEO=M-2*MEO
      IF(L.EQ.0.AND.MEO.EQ.0) IMJL=-1
      IF(L.NE.1) GO TO 99
      IF(J.EQ.1.AND.MEO.EQ.1) IMJL=-1
   99 RETURN
      END
C *******************************************************************
      SUBROUTINE HBAS(TH,B,LMX)
C GET EXPANSION BASES FOR PION-PHOTOPRODUCTION PROGRAM
      DIMENSION B(4,20)
      SAVE
      THR=0.0174532*TH
      S=SIN(THR)
      Z=COS(THR)
      SH=SIN(THR/2.0)/SQRT(2.0)
      CH=COS(THR/2.0)/SQRT(2.0)
      PM=1.0
      P=Z
      B(1,1)=0.0
      B(2,1)=-CH
      B(3,1)=0.0
      B(4,1)=SH
      PP=0.0
      PPP=0.0
      ZL=0.0
      DO 1 L=2,LMX
      PPP=Z*PPP+(ZL+2.0)*PP
      PP=Z*PP+(ZL+1.0)*PM
      ZL=ZL+1.0
      B(1,L)=((1.0-Z)*PPP-(ZL+2.0)*PP)*S*CH
      B(2,L)=((1.0-Z)*PP-(ZL+1.0)*P)*CH
      B(3,L)=((1.0+Z)*PPP+(ZL+2.0)*PP)*S*SH
      B(4,L)=((1.0+Z)*PP+(ZL+1.0)*P)*SH
      X=P
      P=((2.0*ZL+1.0)*Z*P-ZL*PM)/(ZL+1.0)
    1 PM=X
      RETURN
      END
C ****************************************
      FUNCTION DTROMB(NL,IS,T)
C DO QUADRATIC TABLE LOOKUP OF TROMBERG PHASES K=1(S31),2(P31),3(P33)
C 4(S11),5(S31),6(P31),7(P33)   1-3 FOR PI+P AND 4-7 FOR PI-P/CXS
C TI=10(25)350 MEV
      DIMENSION F(105)
      DATA F/ 0.110, 0.093, 0.093, 0.100, 0.100, 0.110, 0.120, 0.121
     C, 0.130, 0.130, 0.130, 0.129, 0.130, 0.133, 0.137, 0.010, 0.012
     C, 0.027, 0.039, 0.049, 0.068, 0.075, 0.090, 0.093, 0.105, 0.116
     C, 0.123, 0.129, 0.135, 0.141,-0.043,-0.120,-0.277,-0.516,-0.871
     C,-1.287,-1.449,-1.115,-0.612,-0.230, 0.007, 0.143, 0.233, 0.274
     C, 0.299, 0.119, 0.088, 0.064, 0.048, 0.034, 0.021, 0.008, 0.000
     C,-0.005,-0.011,-0.015,-0.021,-0.028,-0.035,-0.040,-0.206,-0.145
     C,-0.112,-0.098,-0.090,-0.084,-0.082,-0.080,-0.075,-0.074,-0.071
     C,-0.069,-0.067,-0.065,-0.063,-0.003,-0.011,-0.019,-0.029,-0.036
     C,-0.046,-0.052,-0.057,-0.064,-0.070,-0.075,-0.081,-0.088,-0.098
     C,-0.108, 0.154, 0.346, 0.542, 0.747, 0.945, 1.021, 0.737, 0.232
     C,-0.151,-0.340,-0.402,-0.403,-0.385,-0.353,-0.318/
      SAVE
      DTROMB=0.0
      K=0
      IF(IS.LT.1.OR.IS.GT.3) GO TO 99
      IF(T.GT.400.0) GO TO 99
      IF(NL.EQ.40) K=1
      IF(NL.EQ.40.AND.IS.GT.1) K=5
      IF(NL.EQ.20) K=4
      IF(NL.EQ.31.AND.IS.EQ.1) K=2
      IF(NL.EQ.41) K=3
      IF(NL.EQ.21.AND.IS.GT.1) K=6
      IF(NL.EQ.41.AND.IS.GT.1) K=7
      IF(K.EQ.0) GO TO 99
      I=(T-22.5)/25.0
      I=I+2
      IF(I.GT.14) I=14
      TI=25*I-15
      I=15*(K-1)+I
      F0=F(I)
      FM=F(I-1)-F0
      FP=F(I+1)-F0
C     WRITE(*,222) T,I,K,TI,F0,FM,FP
C 222 FORMAT(' T=',F5.1,' I,K,TI=',2I3,F6.1,' F0,FM,FP=',3F7.3)
      ZM=-25.0
      ZP=25.0
      Z=T-TI
      D=ZM*ZP*(ZP-ZM)
      A=(ZP**2*FM-ZM**2*FP)/D
      B=(ZM*FP-ZP*FM)/D
      DTROMB=F0+Z*(A+Z*B)
      IF(K.LT.4) GO TO 99
      IF(K.EQ.4.OR.K.EQ.6) DTROMB=2.0*DTROMB
   99 RETURN
      END
C ******************************************************
      SUBROUTINE QJOFX(QS,Y,LMAX)
C  VERWEST ALGORITHMS, MOD 6/86 FOR LARGE X ARNDT
      IMPLICIT REAL*8(A-H,O-Z)
       REAL*4 QS,Y,CN,ZN,ZL
       DIMENSION QL(03),QS(03)
       X=Y
        IWRIT=6
       IF(LMAX.LT.2) LMAX=1
      LL=LMAX+1
      DO 1 L=1,LL
    1 QS(L)=0.
      IF(X.LT.2.) GO TO 5
C POWER SERIES IN 1/X ARNDT 6/17/86
      Z=1./X
      ZZ=Z
      ZN=-1.
   20 ZN=ZN+2.
      CN=ZZ/ZN
      DO 21 L=1,LL
      ZL=L
      QS(L)=QS(L)+CN
      CN=CN*Z*(ZN+ZL-1.)/(ZN+2.*ZL)
      IF(CN.LT.1.E-30) GO TO 22
   21 CONTINUE
   22 ZZ=ZZ*Z**2
      IF(ZZ.GT.1.E-30) GO TO 20
      GO TO 2
    5 L=LMAX
       DO 100 II=1,2
       IF (X.LT.1.030) GO TO 600
C   ** ENTERING LARGE X EXPANSION
       Z=1./(X+DSQRT(X*X-1.))
         ALF=2.*Z
         DO 3 I=1,L
   3     ALF=DFLOAT(I)/DFLOAT(2*I+1)*ALF*2.*Z
        CTOT=1.
        CNOW=1.
         DO 4 I=1,100
         CNOW=DFLOAT((2*I-1)*(I+L))/DFLOAT(2*I*I+2*I*L+I)*CNOW*Z*Z
         CTOT=CTOT+CNOW
         IF (CNOW/CTOT.LT.1.E-7  ) GO TO 99
   4     CONTINUE
         WRITE(IWRIT,333)
   99    QLOFX=ALF*CTOT
         GO TO 601
C   ** ENTERING SMALL X EXPANSION
  600    Z=1.-1./X/X
         SUM=0.
         CNOW=1.
         FNOW=DLOG(4.D0)-DLOG(Z)
         IF (L.EQ.0) GO TO299
         DO 18 JJ=1,L
   18    FNOW=FNOW-2./DFLOAT(JJ)
 299     DO 48 I=01,100
         SUM=SUM+CNOW*FNOW
        IF (DABS(CNOW*FNOW/SUM ).LT.1.E-7  ) GO TO 199
         CNOW=CNOW*DFLOAT((L+2*I)*(L+2*I-1))/4./DFLOAT(I*I)*Z
 48      FNOW=FNOW+2.*(1./DFLOAT(I)-1./DFLOAT(L+2*I-1)-1./DFLOAT(2*I+L))
         WRITE(IWRIT,333)
  199    QLOFX=.5/(X**(L+1))*SUM
  601    QL(II)=QLOFX
  100    L=L-1
        QJ1=QL(2)
        QJ2=QL(1)
        QS(LMAX+1)=QJ2
        QS(LMAX)=QJ1
        DO 999 IOP=2,LMAX
        J=LMAX-IOP+1
      QJ=(DFLOAT(2*J+1)*QJ1*X-DFLOAT(J+1)*QJ2)/DFLOAT(J)
       QS(J)=QJ
       QJ2=QJ1
 999   QJ1=QJ
  333  FORMAT (' WARNING **** QLS MAY NOT BE RIGHT *** SUM ENDED')
    2   RETURN
          END
C *********************************************************
      SUBROUTINE CMFN(WR,WI,WZ,WTR,WTI,LMX,CR,CI)
C  CHEW-MANDELSTAM FUNCTIONS 7/17/80 ARNDT
C  INT(0,1) OF X**(L+1/2)/PI/(X-Z)
C  Z=(W-WT)/(W-WZ)
      DIMENSION CR(20),CI(20)
      DATA PI/3.1415927/
      DATA ZC/2./
      IF(LMX.GT.10) LMX=10
      DO 10 L=1,LMX
      CR(L)=0.
   10 CI(L)=0.
      DR=WR-ABS(WZ)
      D2=DR**2+WI**2
      IF(D2.LT.1.) GO TO 99
      ZR=((WR-WTR)*DR+WI*(WI-WTI))/D2
      ZI=(DR*(WI-WTI)-WI*(WR-WTR))/D2
      AR=ZR
      AI=ZI
      CALL SQZ(AR,AI)
      IF(WZ.LT.0..AND.ZI.LT.0.) AR=-AR
      IF(WZ.LT.0..AND.ZI.LT.0.) AI=-AI
      Z2=ZR**2+ZI**2
      IF(Z2.LT.ZC**2) GO TO 11
C  USE POWER SERIES FOR Z GTO INF
      RR=ZR/Z2
      RI=-ZI/Z2
      L=0
   12 TL=2*L
      TL=(TL+3.)/2.
      SR=-RR/PI/TL
      SI=-RI/PI/TL
      TR=SR
      TI=SI
      RT=SQRT(TR**2+TI**2)
      DO 13 N=1,20
      R=TL/(TL+1.)
      Z=TR
      TR=R*(RR*Z-RI*TI)
      TI=R*(RI*Z+RR*TI)
      SR=SR+TR
      SI=SI+TI
      IF(R*RT.LT.1.E-6) GO TO 14
   13 TL=TL+1.
   14 L=L+1
      CR(L)=SR
      CI(L)=SI
      IF(L.GE.LMX) GO TO 99
      GO TO 12
   11 A2=AR**2+AI**2
      D=1.+A2+2.*AR
      ZZR=(1.-A2)/D
      ZZI=-2.*AI/D
      CALL ALG(ZZR,ZZI)
      BR=2./PI-AI+(AR*ZZR-AI*ZZI)/PI
      BI=AR+(AR*ZZI+AI*ZZR)/PI
      ZL=.5
      L=0
    1 L=L+1
      CR(L)=BR
      CI(L)=BI
      IF(L.GE.LMX) GO TO 99
      ZL=ZL+1.
      ZZ=BR
      BR=ZR*BR-ZI*BI+1./PI/ZL
      BI=ZI*ZZ+ZR*BI
      GO TO 1
   99 RETURN
      END
C **********************************************************
      SUBROUTINE ALG(ZR,ZI)
C  TAKE NATURAL LOG OF Z  BRANCH CUT AT Z=0 TAKEN TO LEFT
      DATA PI/3.1415927/
      IF(ZR.EQ.0.) ZR=1.E-10
      ZM=ZR**2+ZI**2
      PHI=ATAN(ZI/ZR)
      IF(ZR.GT.0.) GO TO 1
      IF(ZI.GE.0.) PHI=PHI+PI
      IF(ZI.LT.0.) PHI=PHI-PI
    1 ZR=ALOG(ZM)/2.
      ZI=PHI
      RETURN
      END
C **************************************************************
      SUBROUTINE SQZ(ZR,ZI)
C  SQRT(Z)  BRANCH CUT TAKEN TO LEFT OF Z=0
      DATA PI/3.1415927/
      ZM=SQRT(SQRT(ZR**2+ZI**2))
      IF(ZR.EQ.0.) ZR=1.E-10
      PHI=ATAN(ZI/ZR)
      IF(ZR.GT.0.) GO TO 1
      IF(ZI.LT.0.) PHI=PHI-PI
      IF(ZI.GT.0.) PHI=PHI+PI
    1 PHI=PHI/2.
      ZR=ZM*COS(PHI)
      ZI=ZM*SIN(PHI)
      RETURN
      END
C ***********************************************
      SUBROUTINE SAIDI(TTL,MJL,IR,Q2,WCM,DER,DEI)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION CR(91,6,2,6,41),CI(91,6,2,6,41),PWQ(6),TTL(18),TTX(16)
      DATA NOPEN,SOL,W0,WP,BLNK/0,4HGWSI,1073.3,1079.21,4H    /
      SAVE
      H=TTL(1)
      IF(H.NE.SOL) GO TO 99
      II=IMJL(MJL,M,J,L)
      IF(II.LT.0) GO TO 99
      IF(M.EQ.5.OR.M.EQ.6) GO TO 99
      IF(M.EQ.7) M=5
      IF(M.EQ.8) M=6
      IF(M.EQ.9) GO TO 99
      IF(NOPEN.EQ.1) GO TO 1
      NOPEN=1
      NF=64
      OPEN(NF,FILE='GWSI.DAT',STATUS='OLD')
      READ(NF,101) TTX,GOM1,GOM2,GOMS,GPI2,GP1,GP2
  101 FORMAT(16A4,/6F10.4)
      TTL(2)=BLNK
      DO 3 I=1,16
    3 TTL(I+2)=TTX(I)
      READ(NF,102) WMN,DW,WMX,DQ2,QMX,NW,NQ2
  102 FORMAT(5F10.4,2I4)
      WRITE(*,103) TTX,WMN,DW,WMX,DQ2,QMX,NW,NQ2
     C,GOM1,GOM2,GOMS,GPI2,GP1,GP2
  103 FORMAT(//' Interpolating for solution ',/16A4
     C,/' W=',F7.1,'(',F6.2,')',F7.1,'  Q2= 0.0(',F5.2,')',F6.2
     C,'  NW, NQ2 =',2I4,/6F10.4,/)
      DO 2 NQ=1,NQ2
      DO 2 LL=1,6
      DO 2 JJ=1,2
      DO 2 MM=1,6
      READ(NF,110) (CR(I,MM,JJ,LL,NQ),I=1,NW)
  110 FORMAT(9F8.4)
      READ(NF,110) (CI(I,MM,JJ,LL,NQ),I=1,NW)
    2 CONTINUE
c     write(*,224) (cr(i,2,2,2,8),i=11,22)
c 224 format(6f10.4)
    1 DER=0.0
      DEI=0.0
      IF(WCM.GT.2000.0) GO TO 99
      IF(Q2.GT.4.0) GO TO 99
      L=L+1
      NQ=Q2/DQ2+1.0
      QM=NQ-1
      QM=DQ2*QM
      Z=(Q2-QM)/DQ2
      IF(NQ.GT.NQ2-1) NQ=NQ2-1
      ZM=ZDWI(CR(1,M,J,L,NQ),WCM,WMN,DW,NW)
      ZP=ZDWI(CR(1,M,J,L,NQ+1),WCM,WMN,DW,NW)
      DER=ZM+Z*(ZP-ZM)
      ZM=ZDWI(CI(1,M,J,L,NQ),WCM,WMN,DW,NW)
      ZP=ZDWI(CI(1,M,J,L,NQ+1),WCM,WMN,DW,NW)
      DEI=ZM+Z*(ZP-ZM)
c     if(mjl.ne.221) go to 99
c     write(*,222) nq,nq2,q2,dq2,zm,zp,z,dei
  222 format(' nq=',i4,' nq2 q2 dq2 =',i4,2f8.3
     ,/' zm, zp, z, dei =',4f9.4)
C Modification of threshold factor doesn't do much
      Z=1.0
      IF(IR.EQ.2) Z=(WCM-WP)/(WCM-W0)
      IF(Z.LT.0.0) Z=0.0
      DER=Z*DER
      DEI=Z*DEI
c     write(*,223) mjl,ir,z,der,dei
  223 format(i4,i3,' z, der, dei =',3f9.4)
   99 RETURN
      END
C *******************************************************
      FUNCTION ZDWI(F,W,WMN,DW,NW)
      DIMENSION F(91)
      I=(W-WMN)/DW+1.0
      IF(I.LT.2) I=2
      IF(I.GT.NW-1) I=NW-1
      Z=I-1
      WI=WMN+DW*Z
      Z=(W-WI)/DW
      A=(F(I+1)-F(I-1))/2.0
      B=(F(I+1)+F(I-1)-2.0*F(I))/2.0
      ZDWI=F(I)+Z*(A+Z*B)
      RETURN
      END

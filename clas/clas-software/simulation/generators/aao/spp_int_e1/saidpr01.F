      subroutine GWEPR01(qq,wc,eps,a,phi,irx,crs,sltp)

c These routines were sent by Igor Strakovski and Dick Arndt Feb 20 2002
c Solution XF18K includes preliminary CLAS data from Joo (pi0) and Egiyan (pi+) 
c Modified this routine for my purposes (LCS)
c User must call EPRSET first time before calling GWEPR
c This routine can be modified to output whatever observable is desired
c The routine EPROUT below can be used to create multipole tables
      
c Solution XF18K 1800 SP01(33787/17047)+ 98761/55349 (78 PRM)  

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
 
      IR=IRX
      Q2=QQ
      WCM=WC
      THTA=A
      EPSG=EPS
      PHIG=PHI
      CALL EPRHA
      
c You have to hunt through the routine EPROBS to find the index for each observable.
c The list below is from the original code.      
c Observables are numbered starting with DSG
c (DSG )  (DT  )  (DL  )  (DTT )  (DTL )  (DTLP)  (DSTL)
c (SGT )  (SGTT)  (SGTL)  (T   )  (P   )  (S   )  (E   )
c (F   )  (G   )  (H   )  (OX  )  (OZ  )  (CX  )  (CZ  )
c (TX  )  (TZ  )  (LX  )  (LZ  )  (T00 )  (T0Y )  (TY0 )
c (TXX )  (TXZ )  (TZX )  (TZZ )  (L00 )  (L0Y )  (LXX )
c (LZX )  (TL00)  (TL0X)  (TL0Y)  (TL0Z)  (TLX0)  (TLZ0)
c (TLXX)  (TLZX)  (TT00)  (TT0X)  (TT0Z)  (TTX0)  (TTZ0)
c (LP00)  (LP0X)  (LP0Y)  (LP0Z)  (LPX0)  (LPZ0)  (LPXX)
c (LPZX)  (TP0X)  (TP0Z)  (TPX0)  (TPZ0)  (A0Y )  (PY  )
c (PX  )  (PZ  )  (EMRI)  (ST1 )  (ST3 )  (ST13)  (A0YP)
c (AXYZ)

      CALL EPROBS(1)
      crs=OBSPRD
      CALL EPROBS(6)
      sltp=OBSPRD
      RETURN
      END
      
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
      WCM = WC
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
        write(j,330) ww,q2
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
      CHARACTER HTL*52
      DIMENSION CCS(5),TTL(13),PP1(70),PP2(70),PP3(70),PP4(70),PP5(70)
     C,PP6(70),PP7(70),PP8(70),PP9(47),PP(607)
      EQUIVALENCE (HTL,TTL),(PP1,PP),(PP2,PP(71)),(PP3,PP(141))
     C,(PP4,PP(211)),(PP5,PP(281)),(PP6,PP(351)),(PP7,PP(421))
     C,(PP8,PP(491)),(PP9,PP(561))
      DATA BLNK,TEST/4H    ,4HTEST/
      DATA CCS/  22.500,   0.000,  13.750,   0.000,   0.000/
      DATA IPRKX/  1/
      DATA HTL/'XF18K 1800 SP01(33787/17047)+ 98761/55349 (78 PRM)  '/
      DATA IMX/607/
      DATA PP1/ 0.125121E+14, 0.441080E+01,-0.547123E+02, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.138890E+02, 0.000000E+00,-0.895965E+02,
     C 0.143323E+03, 0.219600E+00, 0.121321E+14,-0.857640E+01,
     C 0.980500E+00,-0.758000E-01, 0.000000E+00, 0.659227E+02,
     C-0.889690E+01, 0.270100E+00, 0.000000E+00,-0.823820E+01,
     C 0.000000E+00, 0.559710E+01, 0.000000E+00,-0.597300E+00,
     C 0.125521E+14, 0.138000E-01, 0.188011E+02, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.495680E+01, 0.000000E+00,-0.875070E+01,
     C 0.125721E+14,-0.186455E+02, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.234790E+03, 0.405218E+03, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.921000E+00, 0.125821E+14, 0.697600E+00,
     C 0.846730E+01, 0.000000E+00, 0.000000E+00,-0.241455E+02,
     C 0.124921E+14, 0.326013E+02,-0.402928E+02, 0.000000E+00,
     C 0.000000E+00,-0.246265E+02, 0.125212E+14,-0.189375E+03,
     C 0.756065E+03,-0.738824E+03/
      DATA PP2/ 0.000000E+00,-0.302803E+03, 0.148996E+04,-0.145095E+04,
     C 0.000000E+00,-0.687580E+01, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.427800E+00, 0.125412E+14, 0.603167E+02,
     C-0.288547E+03, 0.291622E+03, 0.000000E+00, 0.648404E+02,
     C-0.957195E+02, 0.000000E+00, 0.000000E+00, 0.361670E+01,
     C 0.000000E+00,-0.195330E+01, 0.000000E+00,-0.121630E+01,

     C 0.125612E+14, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.193101E+02, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.106479E+02, 0.125712E+14, 0.122054E+03,
     C-0.205855E+03, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00,-0.442282E+03,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00,-0.519600E+00,
     C 0.125812E+14,-0.876440E+01, 0.915145E+02,-0.155539E+03,
     C 0.000000E+00, 0.484950E+01, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.363550E+01, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.758640E+01, 0.124912E+14, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00,-0.486205E+02,
     C 0.124122E+14,-0.420730E+01/
      DATA PP3/ 0.171920E+02,-0.131628E+02, 0.000000E+00,-0.142130E+01,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00,-0.115720E+01,
     C 0.124222E+14,-0.330597E+02, 0.188387E+02,-0.314817E+03,
     C 0.346079E+03, 0.543833E+02, 0.190011E+02,-0.125420E+01,
     C 0.137000E-01, 0.000000E+00, 0.000000E+00,-0.282409E+02,
     C 0.000000E+00,-0.329000E-01, 0.125322E+14, 0.245789E+02,
     C-0.781094E+02, 0.601228E+02, 0.000000E+00, 0.609144E+02,
     C-0.517449E+02, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.264570E+01, 0.000000E+00,-0.106360E+01,
     C 0.125422E+14,-0.273449E+02, 0.100729E+03,-0.102010E+03,
     C 0.000000E+00,-0.318370E+02, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.862050E+01,
     C 0.000000E+00,-0.560600E+00, 0.125522E+14,-0.235870E+01,
     C 0.476440E+01, 0.125622E+14, 0.151950E+01, 0.125722E+14,
     C 0.942932E+02,-0.175382E+03, 0.000000E+00, 0.000000E+00,
     C-0.703254E+02, 0.329860E+03, 0.000000E+00, 0.000000E+00,
     C 0.263147E+02, 0.000000E+00/
      DATA PP4/ 0.000000E+00, 0.000000E+00,-0.128500E+00, 0.125822E+14,
     C-0.246780E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.302847E+02,-0.163820E+03, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.178433E+03, 0.329287E+03,
     C 0.995200E+00, 0.124922E+14,-0.831000E+01, 0.125113E+14,
     C 0.172849E+03,-0.674376E+03, 0.563359E+03, 0.000000E+00,
     C-0.545995E+03, 0.556153E+03, 0.000000E+00, 0.000000E+00,
     C 0.354120E+02, 0.000000E+00, 0.294872E+02, 0.125213E+14,
     C-0.380740E+02, 0.151528E+03,-0.167112E+03, 0.000000E+00,
     C 0.334938E+02, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.218220E+01, 0.125313E+14, 0.126137E+03,-0.500551E+03,
     C 0.442135E+03, 0.000000E+00, 0.884736E+02,-0.103935E+03,
     C 0.000000E+00, 0.000000E+00, 0.557270E+01, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.877300E+00, 0.125413E+14,
     C-0.164937E+02, 0.155827E+02, 0.000000E+00, 0.000000E+00,
     C 0.122601E+03,-0.356564E+03, 0.272622E+03, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.974200E+00, 0.000000E+00,

     C 0.648700E+00, 0.125513E+14/
      DATA PP5/ 0.438656E+02,-0.464686E+02, 0.000000E+00, 0.000000E+00,
     C-0.179961E+03, 0.301082E+03, 0.125613E+14,-0.579680E+01,
     C 0.185104E+02, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.378400E+01,
     C 0.125713E+14, 0.888846E+03,-0.321120E+04, 0.253920E+04,
     C 0.000000E+00,-0.391386E+04, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.668716E+04,-0.134279E+05, 0.790972E+03,
     C 0.618179E+03, 0.125813E+14, 0.176810E+03,-0.300445E+03,
     C 0.000000E+00, 0.000000E+00,-0.839439E+03, 0.284635E+04,
     C-0.215524E+04, 0.000000E+00,-0.124522E+02, 0.000000E+00,
     C-0.333010E+01, 0.125913E+14, 0.343020E+01, 0.125123E+14,
     C-0.215509E+02, 0.731915E+02,-0.590112E+02, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.209680E+01, 0.125223E+14,-0.210930E+01, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.693910E+01, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00/
      DATA PP6/-0.215260E+01, 0.125323E+14, 0.470000E-01, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.395300E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.891580E+01, 0.125423E+14,
     C 0.414390E+01,-0.101468E+02, 0.000000E+00, 0.000000E+00,
     C 0.151840E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.511800E+00, 0.000000E+00, 0.934200E+00, 0.000000E+00,
     C 0.105220E+01, 0.125523E+14, 0.226000E-01, 0.125623E+14,
     C 0.342300E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.179881E+02, 0.125723E+14, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.154021E+03, 0.000000E+00,
     C-0.161318E+03, 0.125823E+14, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.288480E+01, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.788480E+01, 0.124923E+14, 0.928880E+01, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.119248E+02, 0.125114E+14,
     C 0.142116E+02,-0.204037E+02/
      DATA PP7/ 0.000000E+00, 0.000000E+00, 0.234435E+02, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.335210E+01, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.613090E+01, 0.125214E+14,
     C-0.167020E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.210165E+02, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.539900E+00, 0.125314E+14,-0.166215E+02, 0.211368E+02,
     C 0.000000E+00, 0.000000E+00, 0.489029E+02,-0.623247E+02,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.387600E+00, 0.125414E+14,
     C-0.199660E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,

     C 0.107423E+02, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.126930E+01, 0.125514E+14, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.432680E+01, 0.125614E+14,
     C 0.142240E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.594080E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.139010E+01, 0.125714E+14/
      DATA PP8/-0.853660E+01, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C-0.187973E+04, 0.327118E+04, 0.125814E+14,-0.373048E+02,
     C 0.742637E+02, 0.000000E+00, 0.000000E+00,-0.559210E+01,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.641570E+01, 0.000000E+00, 0.737700E+00,
     C 0.125124E+14,-0.342200E+01, 0.518930E+01, 0.000000E+00,
     C 0.000000E+00, 0.506500E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.218020E+01, 0.125224E+14,-0.460540E+02,
     C 0.190679E+03,-0.190992E+03, 0.000000E+00,-0.433504E+02,
     C 0.997752E+02, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.166250E+01, 0.125324E+14, 0.873900E+01,
     C-0.411467E+02, 0.478775E+02, 0.000000E+00, 0.152809E+03,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.258801E+02, 0.125424E+14,-0.413800E+00,
     C 0.125524E+14, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.169085E+02, 0.125624E+14, 0.123268E+02,
     C-0.236829E+02, 0.000000E+00/
      DATA PP9/ 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.261588E+02, 0.000000E+00,-0.102274E+02,
     C 0.125724E+14, 0.000000E+00, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00,-0.312240E+01, 0.125824E+14, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.000000E+00,-0.737337E+03,
     C 0.137737E+04, 0.125115E+14, 0.763500E+00, 0.125215E+14,
     C-0.780800E+00, 0.125415E+14,-0.150300E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.446300E+01, 0.125615E+14,
     C 0.138990E+01, 0.125225E+14, 0.000000E+00, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00, 0.311749E+02, 0.000000E+00,
     C 0.000000E+00, 0.000000E+00,-0.496560E+01, 0.125425E+14,
     C-0.362700E+00, 0.125625E+14, 0.298800E+00/
      SAVE
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
      DO 4 M=1,9
      DO 4 J=1,2
      NF(M,J,L)=0
      DO 4 I=1,50
    4 PEM(I,M,J,L)=0.0
    3 ZL=ZL+1.0
      IPRK=IPRKX
      GOM1=CCS(1)
      GOM2=CCS(2)
      GPI2=CCS(3)
      GP1=CCS(4)
      GP2=CCS(5)
      DO 51 M=1,6
      DO 51 J=1,2
      DO 51 L=1,6
      NF(M,J,L)=0
      IF(L.GT.2) NF(M,J,L)=3
      EMR(M,J,L)=0.0
      EMI(M,J,L)=0.0
   51 EMPI(M,J,L)=0.0
      DO 54 N=1,13
      TTLPNS(N)=BLNK
   54 TITLE(N)=TTL(N)
      TTLPNS(1)=TEST
      I=1
   52 Z=PP(I)/1.E8
      NL=Z+0.1
      NFM=NL/1000
      NL=NL-1000*NFM
      M=NL/100
      NL=NL-100*M
      J=NL/10
      L=NL-10*J
      NF(M,J,L)=NFM
      K=0
   53 I=I+1
      IF(I.GT.IMX) GO TO 99
      IF(PP(I).GT.1.E8) GO TO 52
      K=K+1
      PEM(K,M,J,L)=PP(I)
      GO TO 53
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
      if(mjl.eq.790) write(*,222) mjl,thta,der,dei

  222 format(' mjl, thta, der, dei=',i4,f7.2,2f9.3)
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
      SUBROUTINE EPRDLT
      COMMON/EPRE/IR,WCM,Q2,EPSG,PHIG,QCM,ZKCM,EMR(9,2,6),EMI(9,2,6)
     C,PEM(50,9,2,6),NF(9,2,6),CIS(4,2),CH(4,4,6),TITLE(18),TTLPNS(18)
     C,EPI,HR(6),HI(6),NNL,IPRK,MM,JJ,LL,THTA,DER,DEI,OBSPRD
     C,EMPI(9,2,6),HRX(6),HIX(6)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION QL(10),EMRX(9,2,6),EMIX(9,2,6)
      DATA WPI,WPIC,TLBM,ZXM,WN,NNFM/135.04,139.65,0.0,0.0,938.256,-1/
      SAVE
C  FORM  1  A=ZB*(1+i*Te)+ZR*Te  ZB=P1+Z*P2+Z**2*P3+Z**3*P4
C  ZR=same  using P5..P8 ; Z=Epi/Mpi ZB=ZB*Q**l/K**(l+1)
C  ZR=ZR*K**l/Q**(l+1) Units are [10**-3/Mpi]
      SAVE
      NNF=NF(MM,JJ,LL)
      NQ2=NNF/100
      NNF=NNF-100*NQ2
      NROT=NNF/10
      NNF=NNF-10*NROT
      MJL=100*MM+10*JJ+LL-1
      DER=0.
      DEI=0.
      H=TITLE(1)
      WT=WN
      IF(IR.GT.2) WT=939.65
      EG=(WCM**2-WT**2)/2.0/WT
      IF(QCM.LE.0.0) GO TO 98
      IF(NNF.LT.1)  GO TO 99
      IF(NNF.EQ.9) GO TO 10
      ZKX=(WCM**2-WN**2)/2.0/WCM
      EPX=EPI
      Z=EPX/WPI
      IF(NNF.GT.4) Z=EPX/(800.0+EPX)
      IF(EPX.LT.0.0) EPX=0.0
      WGX=1000.0*SQRT(Q2)
      QB=WN*SQRT(EPX*(EPX+2.0*WPIC)/((WPIC+WN)**2+2.0*WN*EPX))
      CALL EPRBORN(WCM,WGX,EMPI,2)
      N=2
      IF(MM.GT.2) N=0
      IF(MM.EQ.7) N=2
      N=N+JJ
      IF(N.EQ.1.AND.LL.EQ.1) GO TO 99
      IF(N.EQ.3.AND.LL.EQ.1) GO TO 99
      CALL PNPWI(N,LL-1,EPX,TER,TEI,TTLPNS)
   11 ZR=0.0
      QK=QB/ZKX
      IF(QB.LE.0.0) GO TO 24
      ZR=Z*(PEM(6,MM,JJ,LL)+Z*(PEM(7,MM,JJ,LL)+Z*PEM(8,MM,JJ,LL)))
      ZR=(ZR+PEM(5,MM,JJ,LL))*WPI/QB
      IF(LL.EQ.1) GO TO 24
      ZZ=1.0/QK
      IF(IPRK.EQ.1) ZZ=QCM*ZKX/QB**2
      ZR=ZR*ZZ**(LL-1)
   24 IF(NNF.GT.2) GO TO 23
      ZB=Z*(PEM(2,MM,JJ,LL)+Z*(PEM(3,MM,JJ,LL)+Z*PEM(4,MM,JJ,LL)))
      ZB=ZB+PEM(1,MM,JJ,LL)
      IF(LL.GT.1) ZB=ZB*QK**(LL-1)
      GO TO 16
   23 ZZ=2.0*WPI
      ZX=SQRT(QCM**2+ZZ**2)/QCM
      IF(ZX.NE.ZXM)  CALL QJOFX(QL,ZX,8)
      ZXM=ZX
      ZB=PEM(1,MM,JJ,LL)*QL(LL)+PEM(2,MM,JJ,LL)*QL(LL+1)
      ZB=ZX*(ZB+PEM(3,MM,JJ,LL)*QL(LL+2)+PEM(4,MM,JJ,LL)*QL(LL+3))
   16 NEO=0
      IF(NNF.EQ.2) NEO=2
      IF(NNF.EQ.3) NEO=2
      IF(NNF.EQ.5) NEO=4
      IF(NEO.GT.0) ZR=ZR*QK**NEO
      IF(NEO.GT.0) ZB=ZB*QK**NEO
      BRN=EMPI(MM,JJ,LL)
      IF(LL.EQ.1.OR.IPRK.NE.1) GO TO 33
      ZZ=1.0
      CALL EPRKIN(WCM,WGX,1,EPZ,ZKZ,Q0)
      ZZ=QCM/Q0
      BRN=BRN*ZZ**(LL-1)
      ZB=ZB*ZZ**(LL-1)
   33 CONTINUE
      CALL EPRQ2(NQ2,ZFX,ZBX,Q2,WCM,ZKCM,PEM(11+NROT,MM,JJ,LL))
      BRN=BRN*ZBX
      ZB=ZB*ZFX
      ZR=ZR*ZFX
      ZB=ZB+BRN
      DER=ZB*(1.0-TEI)+ZR*TER
      DEI=ZB*TER+ZR*TEI
      SGR=TEI-TEI**2-TER**2
      IF(SGR.LE.0.0) GO TO 98
      ZPR=PEM(9,MM,JJ,LL)+Z*PEM(10,MM,JJ,LL)
      IF(NROT.NE.2) GO TO 32
      ZPI=PEM(11,MM,JJ,LL)+Z*PEM(12,MM,JJ,LL)
      ZFX=ZFX*SGR
      DER=DER+ZPR*ZFX
      DEI=DEI+ZPI*ZFX
      GO TO 98
   32 IF(ZPR.EQ.0.0) GO TO 98
      ROT=0.0174532*ZPR*SGR
      S=SIN(ROT)
      C=COS(ROT)
      Z=DEI
      DEI=Z*C+DER*S
      DER=C*DER-S*Z

      GO TO 98
   10 CONTINUE
C FORM 9 FOR SINGLE-ENERGY Mod-Phase
      TLB=WCM
      ZM=PEM(1,MM,JJ,LL)+PEM(2,MM,JJ,LL)*(TLB-PEM(5,MM,JJ,LL))
      PHS=PEM(3,MM,JJ,LL)+PEM(4,MM,JJ,LL)*(TLB-PEM(5,MM,JJ,LL))
      PHS=0.0174532*PHS
      DER=ZM*COS(PHS)
      DEI=ZM*SIN(PHS)
      IF(IR.LT.2) GO TO 98
      IF(IR.GT.4) GO TO 98
      DER=DER+PEM(4+IR,MM,JJ,LL)
      DEI=DEI+PEM(7+IR,MM,JJ,LL)
   98 ZZ=SQRT(DER**2+DEI**2)
      Z=1.0
      IF(ZZ.GT.0.0) Z=1.0+PEM(15,MM,JJ,LL)/ZZ
      DER=DER*Z-EMR(MM,JJ,LL)
      DEI=DEI*Z-EMI(MM,JJ,LL)
      IF(LL.GT.1) GO TO 99
   99 RETURN
      END
C *************************************************************
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
      WGX=1000.0*SQRT(Q2)
      CALL EPRKIN(WCM,WGX,IR,EPI,ZKCM,QCM)
      WGX=1000.0*SQRT(Q2)
      CALL EPROPEC(WCM,WGX,EMR)

      DO 3 M=1,9
      MM=M
      DO 3 J=1,2
      JJ=J
      DO 3 L=1,NNL
      LL=L
      EMI(M,J,L)=0.0
      NFX=NF(M,J,L)

      IF(Q2.EQ.0.0.AND.M.GT.6) GO TO 3
      IF(NFX.EQ.0) NF(M,J,L)=3
      EMR(M,J,L)=0.0
      CALL EPRDLT
      EMR(M,J,L)=DER

      EMI(M,J,L)=DEI
    3 NF(M,J,L)=NFX
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
      IF(IT.EQ.6) OBSPRD=DTLP*SQRT(EPSG*(1-EPSG)*2.0)*SIN(0.0174532*PHIG)
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
      SUBROUTINE EPRQ2(NQ2,ZFX,ZBX,Q2,WCM,ZKCM,P)
C get Q2 structure functions
      DIMENSION P(30)
      DATA WN/938.256/
      DATA PE0/0.383/
      ZFX=1.0
      ZBX=1.0
      IF(Q2.EQ.0.0) GO TO 99
      ZQ0=2.0*WCM*ZKCM/(WCM**2-WN**2)/(1.0+Q2/.71)**2
      PE=P(1)
      IF(NQ2.EQ.1) PE=PE0+P(2)
      ZZ=ABS(PE)*Q2
      IF(ZZ.GT.30.0) ZZ=30.0
      ZQF=EXP(-ZZ/2.0)
      IF(NQ2.EQ.1) ZQF=ZQF*(1.0+P(1)*Q2)
      IF(NQ2.GT.1) GO TO 2
      ZFX=ZQF*ZQ0
C     IF(NQ2.EQ.1) ZBX=ZFX
      GO TO 99
    2 ZQF=1.0/(1.0+ZZ)**2
      ZQF=ZQF*ZQ0
      ZZ=ABS(P(2))*Q2
      IF(ZZ.GT.30.0) ZZ=30.0
      ZQF=ZQF*EXP(-ZZ)
      ZFX=ZQF
   99 RETURN
      END
C *************************************************************
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
      SUBROUTINE PNPWI(NN,LL,EIN,TR,TI,NTL)
C INTERPOLATE FOR PI-N, OR K-N AMPLITUDES 3/22/83 ARNDT
      DIMENSION NTL(18),PT(3000),PH(3000)
      DIMENSION EL(4,8),AL(4,8),BLX(4,8),TRZ(4,8),TIZ(4,8)
      DATA DLOW/0.002/
      DATA E1,DE,DE2,NE,NE2,EPI,E2,NAM1,NAMX/0,0,0,0,0,0,0,0,0/
      DATA PI,NAMM,NOWN,IRM,INTP/3.1415927,4HXXXX,4HOWN ,0,4HINTP/
      DATA QR,WI,WT,LMX/1.,139.,938.,4/
      DATA LIST,NCON,NW94,NS93,NF93/4HLIST,4HCONT,4HWI94,4HSP93,4HFA93/
      DATA NS95,NM95,NS98,NTST/4HSP95,4HSM95,4HSP98,4HTEST/
      SAVE
      TR=0.0
      TI=0.0
      E=EIN
      IF(E.LE.0.0) GO TO 98
   90 IF(E.LT.2.0) E=2.0
      N=0
      IF(NTL(1).EQ.NW94) N=1
      IF(NTL(1).EQ.NF93) N=2
      IF(NTL(1).EQ.NS93) N=3
      IF(NTL(1).EQ.NS95) N=4
      IF(NTL(1).EQ.NM95) N=5
      IF(NTL(1).EQ.NS98) N=6
      IF(NTL(1).EQ.NTST) N=7
      IF(N.EQ.0) N=7   
      IF(N.EQ.0) GO TO 55
C     IF(N.EQ.1) CALL PNWI94(E,0,TRZ,TIZ,NTL)
C     IF(N.EQ.2) CALL PNFA93(E,0,TRZ,TIZ,NTL)
C     IF(N.EQ.3) CALL PNSP93(E,0,TRZ,TIZ,NTL)
C     IF(N.EQ.4) CALL PNSP95(E,0,TRZ,TIZ,NTL)
      IF(N.EQ.5) CALL PNSM95(E,0,TRZ,TIZ,NTL)
C     IF(N.EQ.6) CALL PNSP98(E,0,TRZ,TIZ,NTL)
      IF(N.EQ.7) CALL PNTEST(E,0,TRZ,TIZ,NTL)
      TR=TRZ(NN,LL+1)
      TI=TIZ(NN,LL+1)
      GO TO 99
   55 NAME=NTL(1)
      IF(NAME.EQ.NAMX) GO TO 1
C     IF(NN.NE.-1.AND.NAME.NE.NAMX) GO TO 1
C LOOK FOR INTERPOLATING ARRAY ON FOR15
      NTRY=0
   20 REWIND 15
      DO 21 J=1,21
      PH(J)=0.0
   21 PT(J)=0.
      EM=1.E20
    2 READ(15,100,END=3) NAMM
      IF(NAMM.NE.INTP) GO TO 2
  100 FORMAT(20A4)
      READ(15,100,END=3) NTL
      NAMM=NTL(1)
      IF(NAM1.EQ.NAMX) NAM1=NAMM
      READ(15,102,END=3) WI,WT,QR,E1,DE,NE
  102 FORMAT(5F10.3,I4)
      NAMX=NTL(1)
      WRITE(*,110) (NTL(J),J=1,14),WI,WT,QR,E1,DE,NE
      WRITE(7,110) (NTL(J),J=1,14),WI,WT,QR,E1,DE,NE
      IF(NAME.NE.NAMM) GO TO 2
   11 J0=1
      LMX=4
      DO 4 L=1,8
      DO 4 N=1,4
      IF(WI.GT.200..AND.N.LT.3) GO TO 4
      IF(L.EQ.1.AND.N.EQ.1) GO TO 4
      IF(L.EQ.1.AND.N.EQ.3) GO TO 4
      JMX=J0+2*NE-1
      READ(15,106,END=3) (PT(J),J=J0,JMX)
  106 FORMAT(10F8.4)
      IF(PT(J0+NE-1).NE.0.) LMX=L
      EL(N,L)=0.0
      EI=E1+DE
      I=1
      DO 28 II=2,NE
      IF(ABS(PT(J0+I)).GT.DLOW) GO TO 29
      I=I+1

   28 EI=EI+DE
      GO TO 26
   29 EL(N,L)=EI
      PWR=L-1
      PWR=PWR+0.5
      AL(N,L)=PT(J0+I)
      BLX(N,L)=PT(J0+I-1)*(EI/(EI-DE))**PWR-PT(J0+I)
      BLX(N,L)=-BLX(N,L)/DE
C     WRITE(*,225) N,L,EI,AL(N,L),BLX(N,L)
C 225 FORMAT(' N,L,EL=',2I3,F9.2,' A,B=',2E12.4)
   26 J0=JMX+1
    4 CONTINUE
      EPI=((WI+WT+139.)**2-(WI+WT)**2)/2./WT
      WRITE(*,110) (NTL(J),J=1,14),WI,WT,QR,E1,DE,NE,LMX
  110 FORMAT(1X,14A4,/' WI,WT,QR,E1,DE,NE,LMX=',2F8.2,F5.0,2F6.2,2I3)
      E2=1.E6
      DE2=0.0
      READ(15,100,END=1) NAMC
      READ(15,100,END=1) NAMC
      IF(NAMC.NE.NCON) GO TO 1
      READ(15,102,END=3) WI,WT,QR,E2,DE2,NE2
      WRITE(*,110) (NTL(J),J=1,14),WI,WT,QR,E2,DE2,NE2
      J0=1
      DO 9 L=1,8
      DO 9 N=1,4
      IF(WI.GT.200..AND.N.LT.3) GO TO 9
      IF(L.EQ.1.AND.N.EQ.1) GO TO 9
      IF(L.EQ.1.AND.N.EQ.3) GO TO 9
      JMX=J0+2*NE2-1
      READ(15,106,END=3) (PH(J),J=J0,JMX)
      IF(PH(J0+NE2-1).NE.0.) LMX=L
      IF(EL(N,L).NE.0.0) GO TO 46
      EI=E2+DE2
      I=1
      DO 48 II=2,NE2
      IF(ABS(PH(J0+I)).GT.DLOW) GO TO 49
      I=I+1
   48 EI=EI+DE2
      GO TO 46
   49 EL(N,L)=EI
      PWR=L-1
      PWR=PWR+0.5
      AL(N,L)=PH(J0+I)
      BLX(N,L)=PH(J0+I-1)*(EI/(EI-DE))**PWR-PH(J0+I)
      BLX(N,L)=-BLX(N,L)/DE2
   46 J0=JMX+1
    9 CONTINUE
      GO TO 1
    3 NTRY=NTRY+1
      IF(NAME.EQ.LIST) GO TO 99
      NAME=NAM1
      IF(NTRY.EQ.1) GO TO 20
      WRITE(*,101) NAME
  101 FORMAT(' ',A4,' was not found on unit 15')
      NTL(1)=NS98
      IF(NAME.NE.NS98) GO TO 90
      CALL EXIT
    1 L=LL+1
      N=NN
      IF(E.LE.0.0) GO TO 99
      IF(L.LT.1.OR.L.GT.LMX) GO TO 99
      IF(N.LT.1.OR.N.GT.4) GO TO 99
      K=4*LL+N
      IF(K.EQ.2) K=1
      IF(K.EQ.4) K=2
      IF(K.GT.4) K=K-2
      IF(E.GT.E2) GO TO 12
      I=(E-E1)/DE+1.5
      IF(I.LT.2) I=2
      IF(I.GT.NE-1) I=NE-1
      E0=I-1

      E0=E1+DE*E0
      ZE=(E-E0)/DE
      Z=E
      ZM=SQRT(ABS(Z/(E0-DE)))
      ZP=SQRT(ABS(Z/(E0+DE)))
      Z0=SQRT(ABS(Z/E0))
      Z=E-EPI
      IF(Z.LT.0.) Z=0.
      ZPM=SQRT(ABS(Z/(E0-DE-EPI)))
      ZPP=SQRT(ABS(Z/(E0+DE-EPI)))
      ZP0=SQRT(ABS(Z/(E0-EPI)))
      J=I+2*NE*(K-1)
      D=PT(J-1)*ZM*(ZE-1.)+PT(J+1)*ZP*(1.+ZE)
      D=(D*ZE/2.+PT(J)*Z0*(1.-ZE**2))
      IF(E.GE.EL(N,L)) GO TO 25
      Z=E/EL(N,L)
      PWR=L-1
      PWR=PWR+.5
      D=AL(N,L)+BLX(N,L)*(E-EL(N,L))
      D=D*Z**PWR
   25 J=J+NE
      R=PT(J-1)*ZPM*(ZE-1.)+PT(J+1)*ZPP*(1.+ZE)
      R=(R*ZE/2.+PT(J)*ZP0*(1.-ZE**2))
      ET=COS(R)
      TR=ET*SIN(2.*D)/2.
      TI=(1.-ET*COS(2.*D))/2.
      GO TO 99
   12 I=(E-E2)/DE2+1.5
      IF(I.LT.2) I=2
      IF(I.GT.NE2-1) I=NE2-1
      E0=I-1
      E0=E2+DE2*E0
      ZE=(E-E0)/DE2

      Z=E
      ZM=SQRT(ABS(Z/(E0-DE2)))
      ZP=SQRT(ABS(Z/(E0+DE2)))
      Z0=SQRT(ABS(Z/E0))
      Z=E-EPI
      IF(Z.LT.0.) Z=0.
      ZPM=SQRT(ABS(Z/(E0-DE2-EPI)))
      ZPP=SQRT(ABS(Z/(E0+DE2-EPI)))
      ZP0=SQRT(ABS(Z/(E0-EPI)))
      J=I+2*NE2*(K-1)

      D=PH(J-1)*ZM*(ZE-1.)+PH(J+1)*ZP*(1.+ZE)
      D=(D*ZE/2.+PH(J)*Z0*(1.-ZE**2))
      J=J+NE2
      R=PH(J-1)*ZPM*(ZE-1.)+PH(J+1)*ZPP*(1.+ZE)
      R=(R*ZE/2.+PH(J)*ZP0*(1.-ZE**2))
      ET=COS(R)
      TR=ET*SIN(2.*D)/2.
      TI=(1.-ET*COS(2.*D))/2.
   99 IF(EIN.GE.2.0) GO TO 98
      Z=EIN/2.0
      TR=TR*SQRT(Z)
      TI=TI*Z
      IF(LL.EQ.0) GO TO 98
      TR=TR*Z**LL
      TI=TI*Z**(2*LL)
   98 RETURN
      END
C ***************************************
      SUBROUTINE PNTEST(E,IRR,TR,TI,NTL)
C Get SAID partial waves. Parameters are in DATA statements
C E is Tlab(MeV), IR=0(PiN),1(Pi+P),2(Pi-P),3(Cxs)
C T(N,L) is PW for l=L-1, and N=1(I=1/2,J-), 2(I=1/2,J+),3(I=3/2,J-)
C and 4(I=3/2,J+) eg (N,L)=(2,1) for S11, (4,1) for S31, (4,2) for P33
C (1,4) for F15 .......
C NTL is set on 1st call and is a "title" for the SAID solution encoded
      DIMENSION PP(280),NNTL(13),PP1(70),PP2(70),PP3(70),PP4(65)
      DIMENSION TR(4,8),TI(4,8),NFM(4,8),P(30,4,8)
      DIMENSION NTL(13),W1(3),W2(3),DW2(3),V(8,3),VI(8,3),BPL(8)
      CHARACTER HTL*52
      EQUIVALENCE (HTL,NNTL),(PP,PP1),(PP(71),PP2),(PP(141),PP3)
     C,(PP(211),PP4)
      DATA HTL/'SP00 2/00 g2=13.74, a(-)=254, a(pi-)=250, Eth=1.83  '/
      DATA IMX/275/
      DATA PP1/ 0.62100E+11, 0.35051E+00,-0.87942E+00, 0.52405E+00,
     C 0.63167E+00, 0.17017E+04, 0.00000E+00, 0.19495E+01,-0.16350E+01,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,

     C 0.00000E+00, 0.37489E+01,-0.11890E+02, 0.31830E+02, 0.00000E+00,
     C 0.24667E+00,-0.13940E+03,-0.17314E+04,-0.27545E+01, 0.33100E+11,
     C 0.55000E+03, 0.34100E+11,-0.25834E+00,-0.31169E+01,-0.46620E+01,
     C 0.17207E+02, 0.00000E+00,-0.82959E+01, 0.22004E+02,-0.15346E+02,
     C 0.31620E+02, 0.51200E+11,-0.16151E+01, 0.16660E+02,-0.28846E+02,
     C 0.15325E+02, 0.00000E+00, 0.76741E+01,-0.81435E+01,-0.29631E+01,
     C 0.69585E+01, 0.15847E+01, 0.00000E+00,-0.15920E+01, 0.20390E+01,
     C 0.52200E+11,-0.44787E+00,-0.77323E+00, 0.52370E+00, 0.42957E+01,
     C 0.00000E+00,-0.33619E+01, 0.87787E+01,-0.33058E+01, 0.99125E+01,
     C 0.00000E+00, 0.20217E+00, 0.53200E+11,-0.88859E+00, 0.43201E-01,
     C-0.13507E+02, 0.17596E+02, 0.00000E+00,-0.65156E+01, 0.10960E+02,
     C 0.00000E+00/
      DATA PP2/ 0.36922E+01, 0.54200E+11, 0.20897E+01,-0.36313E+01,
     C 0.54904E+00, 0.00000E+00, 0.13814E+04, 0.22295E+01,-0.16502E+01,
     C-0.17098E+01, 0.58129E+01, 0.97030E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.36090E+01,
     C-0.21143E+01, 0.51300E+11, 0.53234E+00, 0.16330E+01,-0.97091E+00,
     C 0.00000E+00, 0.00000E+00, 0.80335E+01, 0.00000E+00,-0.94892E+02,
     C 0.19196E+03, 0.97538E+00, 0.52300E+11, 0.61372E+00,-0.10384E+01,
     C 0.21515E+01,-0.31527E+00, 0.00000E+00,-0.35036E+00, 0.72494E+01,
     C-0.42106E+02, 0.69307E+02, 0.10341E+01, 0.53300E+11, 0.50665E+00,
     C-0.23410E+01, 0.23902E+01, 0.00000E+00, 0.00000E+00, 0.85935E+00,
     C 0.21495E+01,-0.92927E+01, 0.16152E+02, 0.72560E+00, 0.54300E+11,
     C-0.20391E+00,-0.66255E+00, 0.91923E+00, 0.00000E+00, 0.00000E+00,
     C-0.95322E+00, 0.21004E+01,-0.12854E+02, 0.16085E+02, 0.00000E+00,
     C-0.66541E+00, 0.31400E+11, 0.34848E+00, 0.43482E+00, 0.12755E+01,
     C-0.14375E+01/
      DATA PP3/ 0.00000E+00, 0.15580E+02,-0.10602E+02,-0.65152E+02,
     C 0.93462E+02, 0.00000E+00, 0.71466E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.12697E+00,-0.98335E+02,-0.18099E+04, 0.32400E+11,-0.82025E-01,
     C 0.27362E+00,-0.22371E+00, 0.00000E+00, 0.00000E+00, 0.87566E+00,
     C 0.00000E+00, 0.99948E+01,-0.84440E+01, 0.33400E+11, 0.42307E-01,
     C-0.31408E+00,-0.10828E+01, 0.10426E+01, 0.00000E+00, 0.27848E+01,
     C-0.13953E+01,-0.11369E+02, 0.14621E+02, 0.80499E+00, 0.34400E+11,
     C 0.56529E+00,-0.34225E+00, 0.14489E+01, 0.00000E+00, 0.00000E+00,
     C 0.13756E+01, 0.18187E+01,-0.20487E+02, 0.23076E+02, 0.31500E+11,
     C 0.17589E+00, 0.10078E+01,-0.39184E+00, 0.00000E+00, 0.00000E+00,
     C 0.15683E+01, 0.00000E+00,-0.82232E+01, 0.75576E+01, 0.32500E+11,
     C 0.26253E+00,-0.80422E+00, 0.80969E+00, 0.00000E+00, 0.00000E+00,
     C 0.97735E+00, 0.00000E+00,-0.54683E+01, 0.56671E+01, 0.33500E+11,
     C 0.16723E+00/
      DATA PP4/-0.61498E+00, 0.35634E+00, 0.00000E+00, 0.00000E+00,
     C 0.75010E+00, 0.34500E+11,-0.70340E-01, 0.51945E-01, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.94912E+00, 0.31600E+11,
     C 0.21463E+00,-0.85833E+00, 0.11915E+01, 0.23905E+01, 0.00000E+00,
     C 0.35584E+01, 0.00000E+00, 0.26232E+02,-0.18680E+02, 0.32600E+11,
     C 0.11633E+00,-0.17134E+01, 0.30410E+01,-0.14964E+01, 0.33600E+11,
     C 0.13614E+00,-0.36977E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.67933E+00, 0.34600E+11, 0.25812E+00, 0.12399E+01,-0.29321E+01,
     C 0.17824E+01, 0.00000E+00, 0.66588E+00, 0.31700E+11, 0.21191E+00,

     C 0.10896E+00, 0.32700E+11, 0.18656E+00,-0.40983E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.62788E+00, 0.33700E+11,
     C 0.17214E+00, 0.70675E+00,-0.96510E+00, 0.00000E+00, 0.00000E+00,
     C 0.89652E+00, 0.34700E+11, 0.11727E+00, 0.82277E+00,-0.63800E+00,
     C 0.0/
      DATA W1/139.65,139.65,938.256/
      DATA W2/938.256,1212.0,549.0/
      DATA DW2/1.0,102.0,2.5/
      DATA NNL,NCH,WI,WT/8,3,139.65,938.256/
      DATA IRM,EM,S11L,NSTRT/-1,0.0,0.0,0/
      DATA WSUB/150.0/
      SAVE
      IF(NSTRT.EQ.1) GO TO 1
C     write(*,222) NTL(1)
C 222 format('  NTL=',A4)
      DO 51 L=1,8
      DO 51 N=1,4
      NFM(N,L)=0
      TR(N,L)=0.0
      TI(N,L)=0.0
      DO 51 J=1,30
   51 P(J,N,L)=0.0
      NSTRT=1
      DO 54 N=1,13
   54 NTL(N)=NNTL(N)
      I=1
   52 Z=PP(I)/1.E8
      NL=Z+0.1
      NF=NL/100
      NL=NL-100*NF
      N=NL/10
      L=NL-10*N
C     IF(N.EQ.3.AND.L.EQ.1) NF=0
      NFM(N,L)=NF
      J=0
   53 I=I+1
      IF(I.GT.IMX) GO TO 1
      IF(PP(I).GT.1.E8) GO TO 52
      J=J+1

      P(J,N,L)=PP(I)
      IF(J.EQ.1.AND.N.EQ.3.AND.L.EQ.1) WSUB=PP(I)
      GO TO 53
    1 IF(EM.NE.E) IRM=-27
      IF(TR(2,1).NE.S11L) IRM=-27
      IF(IRR.EQ.IRM) GO TO 99
      IRM=IRR
      IR=IRR
      IF(IR.GT.3) IR=IR-3
      EM=E
      TLB=E
      DO 55 L=1,8
      DO 55 N=1,4
      TR(N,L)=0.0
   55 TI(N,L)=0.0
      IF(IR.LT.0.OR.IR.GT.3) GO TO 99
C SMALLEST ENERGIES SEEM TO BREED TROUBLE
      IF(TLB.LT.0.2) GO TO 99
      W=SQRT((WI+WT)**2+2.*TLB*WT)
      QPQ=(W**2-1074.7**2)*(W**2-804.6**2)
      QPQ=SQRT(QPQ/(W**2-(WI+WT)**2)/(W**2-(WT-WI)**2))
      DO 7 N=1,NCH
      WSU=W1(N)+W2(N)
      WC=WI+WT+140.0
      IF(N.EQ.1) WC=WSU-WSUB
      GU=-DW2(N)/2.
      IF(N.EQ.1.) GU=0.
    7 CALL CMFN(W,0.,WC,WSU,GU,NNL,V(1,N),VI(1,N))
      ETA=IR
      IF(ETA.GT.1.0) ETA=-1.0
      XKMEV=SQRT(TLB*(TLB+2.0*WI)/((1.0+WI/WT)**2+2.0*TLB/WT))
      IF(IR.EQ.3) XKMEV=XKMEV*SQRT(QPQ)
      XKM=197.32/XKMEV
      PZR=SQRT(XKMEV**2+W2(1)**2)
      QZR=SQRT(XKMEV**2+W1(1)**2)
      ETA=ETA*.007297348*(QZR*PZR+XKMEV**2)/XKMEV/(PZR+QZR)
C  PUT COULOMB BARRIER FACTORS INTO VI(L,5) 8/26/82 ARNDT
      IF(ETA.GT.100.) ETA=100.
      Z=2.*3.1415927*ETA
      BL=1.
      IF(ETA.NE.0.) BL=Z/(EXP(Z)-1.)
      Z=0.
      ZZ=SQRT(QPQ)
      DO 8 L=1,NNL
      BPL(L)=BL
      Z=Z+1.
      IF(IR.NE.3) GO TO 8
      BPL(L)=SQRT(BL)*ZZ
      ZZ=ZZ*QPQ
    8 BL=BL*(1.+(ETA/Z)**2)
      DO 9 LL=1,NNL
      DO 9 NN=1,4
      NNF=NFM(NN,LL)
      TRX=0.
      TIX=0.
      NL=10*NN+LL-1
      IF(NN.EQ.1.AND.LL.EQ.1) GO TO 9
      IF(NN.EQ.3.AND.LL.EQ.1) GO TO 9
      IF(NNF.LT.3.OR.NNF.GT.6) GO TO 10
      BL=BPL(LL)
      IF(BL.EQ.0.0) BL=1.0
C  ENCODE C-M K-MTX FIT FOR FORM 4 9/23/81 ARNDT
      CER=V(LL,1)
      CEI=VI(LL,1)
      LI=LL
      IF(NN.EQ.1.OR.NN.EQ.3) LI=LL-2
      IF(LI.LT.1) LI=LI+2
      IF(LL.EQ.1) LI=3
      NIL=2
      IF(NIL.GT.NCH) NIL=NCH
      CIR=V(LI,NIL)
      CII=VI(LI,NIL)
      WTH=W1(1)+W2(1)
      WPITH=WTH+140.
      WCM=SQRT(WTH**2+2.*W2(1)*TLB)
      Z=(WCM-WPITH)/1000.
      ZZ=1.
      ZE=0.0
      WKP=P(5,NN,LL)
      DRL=1.0
C MASS-SPLIT K-MTX POLE PIECE FOR P33 1/95 ARNDT
      IF(WKP.EQ.0.0) GO TO 34
      DWK=P(18,NN,LL)/2.0
      IF(NL.NE.41) DWK=0.0
      IF(BL.GT.1.0001) WKP=WKP+DWK
      IF(BL.LT.0.9999) WKP=WKP-DWK
      ZZ=WKP-WTH
      DRL=WKP-WCM
      DGK=P(17,NN,LL)/2.0
      IF(DWK.EQ.0.0) DGK=0.0
      IF(DGK.EQ.0.0) GO TO 34
      IF(BL.GT.1.0001) ZE=DGK
      IF(BL.LT.0.9999) ZE=-DGK
   34 CONTINUE
      IF(NNF.GT.2) Z=(WCM-WTH)/1000.0
      LIP=LI+2
      IF(LIP.GT.8) LIP=LIP-2
      DO 12 J=1,4
      ZE=ZE+P(J,NN,LL)*ZZ
   12 ZZ=ZZ*Z
      ZEI=0.
      DIM=0.
      DO 31 J=1,3
      IF(J.NE.3) GO TO 33
      CIR=V(LIP,2)
      CII=VI(LIP,2)
      IF(NNF.EQ.3.OR.NNF.EQ.5) GO TO 33
      CIR=V(LL,3)
      CII=VI(LL,3)
   33 CONTINUE
C     IF(CII.LT.0.0) CII=0.0
      K=2+4*J
      Z0=Z*P(K,NN,LL)+Z**2*P(K+1,NN,LL)
      IF(Z0.EQ.0.0) GO TO 31
      ZZ=P(K+2,NN,LL)+Z*P(K+3,NN,LL)
      IF(NNF.GT.4) ZZ=ZZ*Z
      DIR=1.0-CIR*ZZ
      DII=-CII*ZZ
      Z2R=Z0**2*CIR
      Z2I=CII*Z0**2
      ZZ=Z2R
      Z2R=ZZ*DRL-Z2I*DIM
      Z2I=ZZ*DIM+Z2I*DRL
      ZZ=ZE
      ZE=ZE*DIR-ZEI*DII+Z2R
      ZEI=ZZ*DII+ZEI*DIR+Z2I
      ZZ=DRL
      DRL=DRL*DIR-DIM*DII
      DIM=ZZ*DII+DIM*DIR
   31 CONTINUE
      DRL=DRL-CER*ZE+CEI*ZEI
      DIM=DIM-CER*ZEI-CEI*ZE
      D2=DRL**2+DIM**2
      Z=CEI/D2
      TRX=Z*(ZE*DRL+ZEI*DIM)
      TIX=Z*(ZEI*DRL-ZE*DIM)
      IF(IRR.GT.3) BL=1.0
      IF(BL.EQ.1.0) GO TO 11
      IF(BL.GT.1.0.AND.NL.EQ.41) CALL ETA33(TLB,TRX,TIX)
      IF(NFM(3,1).NE.4) GO TO 13
      IF(LL.GT.2) GO TO 13
      IF(TLB.GT.400.0) GO TO 13
      DD=DTROMB(NL,IR,TLB)*0.0174532
      Z=TRX
      TRX=TRX+DD*(1.0-2.0*TIX)
      TIX=TIX+2.0*Z*DD
      GO TO 11
   13 DR=1.-TIX*(1.-BL)
      DI=TRX*(1.-BL)
      D2=(DR**2+DI**2)/BL
      Z=TRX
      TRX=(Z*DR+TIX*DI)/D2
      TIX=(TIX*DR-Z*DI)/D2
   11 IF(TIX.GE.1.0) TRX=0.0
      IF(TIX.GT.1.0) TIX=1.0
      CALL ADDRES(E,TRX,TIX,NL,P(19,NN,LL))
      IF(TIX.GE.TRX**2+TIX**2) GO TO 10
      D2=1.0+TRX**2+TIX**2-2.0*TIX
      IF(D2.LT.1.E-20) WRITE(7,224) TLB,WCM,TRX,TIX
  224 FORMAT(' TLB, WCM=',2F8.2,' TR,TI=',2F9.5)
      IF(D2.LT.1.E-20) GO TO 10
      Z=TRX/D2
      TRX=Z/(1.+Z**2)
      TIX=Z*TRX
   10 TR(NN,LL)=TRX
    9 TI(NN,LL)=TIX
      S11L=TR(2,1)
   99 RETURN
      END
C *******************************************************************
      SUBROUTINE PNSM95(E,IRR,TR,TI,NTL)
C Get SAID partial waves. Parameters are in DATA statements
C E is Tlab(MeV), IR=0(PiN),1(Pi+P),2(Pi-P),3(Cxs)
C T(N,L) is PW for l=L-1, and N=1(I=1/2,J-), 2(I=1/2,J+),3(I=3/2,J-)
C and 4(I=3/2,J+) eg (N,L)=(2,1) for S11, (4,1) for S31, (4,2) for P33
C (1,4) for F15 .......
C NTL is set on 1st call and is a "title" for the SAID solution encoded
      DIMENSION PP(278),NNTL(13),PP1(70),PP2(70),PP3(70),PP4(65)
      DIMENSION TR(4,8),TI(4,8),NFM(4,8),P(30,4,8)
      DIMENSION NTL(13),W1(3),W2(3),DW2(3),V(8,3),VI(8,3),BPL(8)
      CHARACTER HTL*52
      EQUIVALENCE (HTL,NNTL),(PP,PP1),(PP(71),PP2),(PP(141),PP3)
     C,(PP(211),PP4)
      DATA HTL/'SM95 A55J05 46261/21739 P+=22616/10197 P-=18883/ 942'/
      DATA IMX/275/
      DATA PP1/ 0.62100E+11, 0.34241E+00,-0.82362E+00, 0.56554E+00,
     C 0.37428E+00, 0.16985E+04, 0.00000E+00, 0.16698E+01,-0.13308E+01,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.38617E+01,-0.10823E+02, 0.29502E+02, 0.00000E+00,
     C 0.27453E+00,-0.93934E+02,-0.17123E+04,-0.11347E+01, 0.33100E+11,
     C 0.50000E+03, 0.34100E+11,-0.26078E+00,-0.30559E+01,-0.28895E+01,
     C 0.13324E+02, 0.00000E+00,-0.76275E+01, 0.20539E+02,-0.18236E+02,
     C 0.36539E+02, 0.51200E+11,-0.13805E+01, 0.14733E+02,-0.25454E+02,
     C 0.13616E+02, 0.00000E+00, 0.80740E+01,-0.84446E+01,-0.57069E+01,
     C 0.12607E+02, 0.17406E+01, 0.00000E+00,-0.97398E+00, 0.15162E+01,
     C 0.52200E+11,-0.36497E+00,-0.70043E+00, 0.16508E+01, 0.00000E+00,
     C 0.00000E+00, 0.17908E+00, 0.20029E+01,-0.57498E+01, 0.93514E+01,
     C 0.00000E+00, 0.73363E+00, 0.53200E+11,-0.71838E+00,-0.93605E+00,
     C-0.87571E+01, 0.12615E+02, 0.00000E+00,-0.56270E+01, 0.93192E+01,
     C 0.00000E+00/
      DATA PP2/ 0.29990E+01, 0.54200E+11, 0.19268E+01,-0.30171E+01,
     C 0.00000E+00, 0.00000E+00, 0.13704E+04, 0.20703E+01,-0.14533E+01,
     C-0.71776E+01, 0.13848E+02, 0.98079E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.64645E+01,
     C-0.47284E+01, 0.51300E+11, 0.49193E+00, 0.12901E+01,-0.62085E+00,
     C 0.00000E+00, 0.00000E+00, 0.73900E+01, 0.00000E+00,-0.79177E+02,
     C 0.16114E+03, 0.98604E+00, 0.52300E+11, 0.44512E+00,-0.28501E+00,
     C 0.53144E+00, 0.00000E+00, 0.00000E+00, 0.20529E+01, 0.31498E+01,
     C-0.40774E+02, 0.67388E+02, 0.00000E+00, 0.92560E+00, 0.53300E+11,
     C 0.38425E+00,-0.17794E+01, 0.15045E+01, 0.00000E+00, 0.00000E+00,
     C 0.14622E+01, 0.88328E+00,-0.11594E+02, 0.19323E+02, 0.77120E+00,
     C 0.54300E+11,-0.17791E+00,-0.59504E+00, 0.85740E+00, 0.00000E+00,
     C 0.00000E+00,-0.12851E+01, 0.24762E+01,-0.13286E+02, 0.16567E+02,
     C 0.00000E+00,-0.63714E+00, 0.31400E+11, 0.20010E+00, 0.88710E+00,
     C 0.16018E+01/
      DATA PP3/-0.13851E+01, 0.00000E+00, 0.98711E+01,-0.32074E+01,
     C-0.58316E+02, 0.84871E+02, 0.00000E+00, 0.65896E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.10499E+00,-0.87146E+02,-0.18133E+04, 0.32400E+11,
     C-0.51667E-01,-0.12703E+00, 0.20324E+00, 0.00000E+00, 0.00000E+00,
     C 0.86363E+00, 0.00000E+00, 0.11874E+02,-0.96999E+01, 0.33400E+11,
     C 0.27643E-01,-0.24479E+00,-0.10670E+01, 0.91484E+00, 0.00000E+00,
     C 0.29018E+01,-0.16815E+01,-0.10830E+02, 0.13986E+02, 0.69098E+00,
     C 0.34400E+11, 0.38957E+00,-0.85090E-02, 0.98085E+00, 0.00000E+00,
     C 0.00000E+00, 0.18144E+01, 0.13785E+01,-0.22141E+02, 0.25106E+02,
     C 0.31500E+11, 0.10280E+00, 0.97309E+00,-0.35444E+00, 0.00000E+00,
     C 0.00000E+00, 0.15411E+01, 0.00000E+00,-0.74684E+01, 0.70572E+01,
     C 0.32500E+11, 0.14852E+00,-0.26806E+00, 0.35804E+00, 0.00000E+00,
     C 0.00000E+00, 0.96271E+00, 0.00000E+00,-0.45971E+01, 0.49392E+01,
     C 0.33500E+11/
      DATA PP4/ 0.83171E-01,-0.31272E+00, 0.11725E+00, 0.00000E+00,
     C 0.00000E+00, 0.73736E+00, 0.34500E+11,-0.44713E-01, 0.71551E-01,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.87168E+00,
     C 0.31600E+11, 0.10282E+00, 0.24820E+01,-0.58524E+01, 0.63150E+01,

     C 0.00000E+00, 0.40418E+01, 0.00000E+00, 0.28576E+02,-0.20224E+02,
     C 0.32600E+11, 0.49839E-01, 0.60770E+00,-0.12734E+01, 0.58251E+00,
     C 0.33600E+11, 0.63075E-01,-0.26005E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.64517E+00, 0.34600E+11, 0.14069E+00, 0.99109E+00,
     C-0.22627E+01, 0.14635E+01, 0.00000E+00, 0.60794E+00, 0.31700E+11,
     C 0.10074E+00, 0.24254E+00, 0.32700E+11, 0.10121E+00,-0.23164E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.44086E+00,
     C 0.33700E+11, 0.75125E-01, 0.45317E+00,-0.60161E+00, 0.00000E+00,
     C 0.00000E+00, 0.77820E+00, 0.34700E+11, 0.56023E-01, 0.59477E+00,
     C-0.46785E+00/
      DATA W1/139.65,139.65,938.256/
      DATA W2/938.256,1212.0,549.0/
      DATA DW2/1.0,102.0,2.5/
      DATA NNL,NCH,WI,WT/8,3,139.65,938.256/
      DATA IRM,EM,S11L,NSTRT/-1,0.0,0.0,0/
      DATA WSUB/150.0/
      SAVE
      IF(NSTRT.EQ.1) GO TO 1
      DO 51 L=1,8
      DO 51 N=1,4
      NFM(N,L)=0
      TR(N,L)=0.0
      TI(N,L)=0.0
      DO 51 J=1,30
   51 P(J,N,L)=0.0
      NSTRT=1
      DO 54 N=1,13
   54 NTL(N)=NNTL(N)
      I=1
   52 Z=PP(I)/1.E8
      NL=Z+0.1
      NF=NL/100
      NL=NL-100*NF

      N=NL/10
      L=NL-10*N
C     IF(N.EQ.3.AND.L.EQ.1) NF=0
      NFM(N,L)=NF
      J=0
   53 I=I+1
      IF(I.GT.IMX) GO TO 1
      IF(PP(I).GT.1.E8) GO TO 52
      J=J+1
      P(J,N,L)=PP(I)
      IF(J.EQ.1.AND.N.EQ.3.AND.L.EQ.1) WSUB=PP(I)
      GO TO 53
    1 IF(EM.NE.E) IRM=-27
      IF(TR(2,1).NE.S11L) IRM=-27
      IF(IRR.EQ.IRM) GO TO 99
      IRM=IRR
      IR=IRR
      IF(IR.GT.3) IR=IR-3
      EM=E
      TLB=E
      DO 55 L=1,8
      DO 55 N=1,4
      TR(N,L)=0.0
   55 TI(N,L)=0.0
      IF(IR.LT.0.OR.IR.GT.3) GO TO 99
C SMALLEST ENERGIES SEEM TO BREED TROUBLE
      IF(TLB.LT.0.2) GO TO 99
      W=SQRT((WI+WT)**2+2.*TLB*WT)
      QPQ=(W**2-1074.7**2)*(W**2-804.6**2)
      QPQ=SQRT(QPQ/(W**2-(WI+WT)**2)/(W**2-(WT-WI)**2))
      DO 7 N=1,NCH
      WSU=W1(N)+W2(N)
      WC=WI+WT+140.0
      IF(N.EQ.1) WC=WSU-WSUB
      GU=-DW2(N)/2.
      IF(N.EQ.1.) GU=0.
    7 CALL CMFN(W,0.,WC,WSU,GU,NNL,V(1,N),VI(1,N))
      ETA=IR
      IF(ETA.GT.1.0) ETA=-1.0
      XKMEV=SQRT(TLB*(TLB+2.0*WI)/((1.0+WI/WT)**2+2.0*TLB/WT))
      IF(IR.EQ.3) XKMEV=XKMEV*SQRT(QPQ)
      XKM=197.32/XKMEV
      PZR=SQRT(XKMEV**2+W2(1)**2)
      QZR=SQRT(XKMEV**2+W1(1)**2)
      ETA=ETA*.007297348*(QZR*PZR+XKMEV**2)/XKMEV/(PZR+QZR)
C  PUT COULOMB BARRIER FACTORS INTO VI(L,5) 8/26/82 ARNDT
      IF(ETA.GT.100.) ETA=100.
      Z=2.*3.1415927*ETA
      BL=1.
      IF(ETA.NE.0.) BL=Z/(EXP(Z)-1.)
      Z=0.
      ZZ=SQRT(QPQ)
      DO 8 L=1,NNL
      BPL(L)=BL
      Z=Z+1.
      IF(IR.NE.3) GO TO 8
      BPL(L)=SQRT(BL)*ZZ

      ZZ=ZZ*QPQ
    8 BL=BL*(1.+(ETA/Z)**2)
      DO 9 LL=1,NNL
      DO 9 NN=1,4
      NNF=NFM(NN,LL)
      TRX=0.
      TIX=0.
      NL=10*NN+LL-1
      IF(NN.EQ.1.AND.LL.EQ.1) GO TO 9
      IF(NN.EQ.3.AND.LL.EQ.1) GO TO 9
      IF(NNF.LT.3.OR.NNF.GT.6) GO TO 10
      BL=BPL(LL)
      IF(BL.EQ.0.0) BL=1.0
C  ENCODE C-M K-MTX FIT FOR FORM 4 9/23/81 ARNDT
      CER=V(LL,1)
      CEI=VI(LL,1)
      LI=LL
      IF(NN.EQ.1.OR.NN.EQ.3) LI=LL-2
      IF(LI.LT.1) LI=LI+2
      IF(LL.EQ.1) LI=3
      NIL=2
      IF(NIL.GT.NCH) NIL=NCH
      CIR=V(LI,NIL)
      CII=VI(LI,NIL)
      WTH=W1(1)+W2(1)
      WPITH=WTH+140.
      WCM=SQRT(WTH**2+2.*W2(1)*TLB)
      Z=(WCM-WPITH)/1000.
      ZZ=1.
      ZE=0.0
      WKP=P(5,NN,LL)
      DRL=1.0
C MASS-SPLIT K-MTX POLE PIECE FOR P33 1/95 ARNDT
      IF(WKP.EQ.0.0) GO TO 34
      DWK=P(18,NN,LL)/2.0
      IF(NL.NE.41) DWK=0.0
      IF(BL.GT.1.0001) WKP=WKP+DWK
      IF(BL.LT.0.9999) WKP=WKP-DWK
      ZZ=WKP-WTH
      DRL=WKP-WCM
      DGK=P(17,NN,LL)/2.0
      IF(DWK.EQ.0.0) DGK=0.0
      IF(DGK.EQ.0.0) GO TO 34
      IF(BL.GT.1.0001) ZE=DGK
      IF(BL.LT.0.9999) ZE=-DGK
   34 CONTINUE
      IF(NNF.GT.2) Z=(WCM-WTH)/1000.0
      LIP=LI+2
      IF(LIP.GT.8) LIP=LIP-2
      DO 12 J=1,4
      ZE=ZE+P(J,NN,LL)*ZZ
   12 ZZ=ZZ*Z
      ZEI=0.
      DIM=0.
      DO 31 J=1,3
      IF(J.NE.3) GO TO 33
      CIR=V(LIP,2)
      CII=VI(LIP,2)
      IF(NNF.EQ.3.OR.NNF.EQ.5) GO TO 33
      CIR=V(LL,3)
      CII=VI(LL,3)
   33 CONTINUE
C     IF(CII.LT.0.0) CII=0.0
      K=2+4*J
      Z0=Z*P(K,NN,LL)+Z**2*P(K+1,NN,LL)
      IF(Z0.EQ.0.0) GO TO 31
      ZZ=P(K+2,NN,LL)+Z*P(K+3,NN,LL)
      IF(NNF.GT.4) ZZ=ZZ*Z
      DIR=1.0-CIR*ZZ
      DII=-CII*ZZ
      Z2R=Z0**2*CIR
      Z2I=CII*Z0**2
      ZZ=Z2R
      Z2R=ZZ*DRL-Z2I*DIM
      Z2I=ZZ*DIM+Z2I*DRL
      ZZ=ZE
      ZE=ZE*DIR-ZEI*DII+Z2R
      ZEI=ZZ*DII+ZEI*DIR+Z2I
      ZZ=DRL
      DRL=DRL*DIR-DIM*DII
      DIM=ZZ*DII+DIM*DIR
   31 CONTINUE
      DRL=DRL-CER*ZE+CEI*ZEI
      DIM=DIM-CER*ZEI-CEI*ZE
      D2=DRL**2+DIM**2
      Z=CEI/D2
      TRX=Z*(ZE*DRL+ZEI*DIM)
      TIX=Z*(ZEI*DRL-ZE*DIM)
      IF(IRR.GT.3) BL=1.0
      IF(BL.EQ.1.0) GO TO 11
      IF(BL.GT.1.0.AND.NL.EQ.41) CALL ETA33(TLB,TRX,TIX)
      IF(NFM(3,1).NE.4) GO TO 13
      IF(LL.GT.2) GO TO 13
      IF(TLB.GT.400.0) GO TO 13
      DD=DTROMB(NL,IR,TLB)*0.0174532
      Z=TRX
      TRX=TRX+DD*(1.0-2.0*TIX)
      TIX=TIX+2.0*Z*DD
      GO TO 11

   13 DR=1.-TIX*(1.-BL)
      DI=TRX*(1.-BL)
      D2=(DR**2+DI**2)/BL
      Z=TRX
      TRX=(Z*DR+TIX*DI)/D2
      TIX=(TIX*DR-Z*DI)/D2
   11 IF(TIX.GE.1.0) TRX=0.0
      IF(TIX.GT.1.0) TIX=1.0
      CALL ADDRES(E,TRX,TIX,NL,P(19,NN,LL))
      IF(TIX.GE.TRX**2+TIX**2) GO TO 10
      D2=1.0+TRX**2+TIX**2-2.0*TIX
      IF(D2.LT.1.E-20) WRITE(7,224) TLB,WCM,TRX,TIX
  224 FORMAT(' TLB, WCM=',2F8.2,' TR,TI=',2F9.5)
      IF(D2.LT.1.E-20) GO TO 10
      Z=TRX/D2
      TRX=Z/(1.+Z**2)
      TIX=Z*TRX
   10 TR(NN,LL)=TRX
    9 TI(NN,LL)=TIX
      S11L=TR(2,1)
   99 RETURN
      END
C ****************************************************
      SUBROUTINE ADDRES(E,TR,TI,NL,P)
C  add resonance "bump" to partial-wave 8/94 Arndt
      DIMENSION P(30)
      DATA WPI,WN/139.65,938.256/
      SAVE
      IF(P(2).GT.0.0) GO TO 10
      DRL=1.0
      DIM=0.0
      WR=SQRT((WPI+WN)**2+2.0*WN*E)
      WI=0.0
      CALL ADDRES2(WR,WI,DRL,DIM,TR,TI,NL,P)
      GO TO 99
   10 GT=P(2)
      GE=P(1)*GT
      IF(GE.EQ.0.0) GO TO 99
      GI=GT-GE
      IF(GI.LE.0.0) GI=0.0
      ER=P(3)
      IF(ER.EQ.0.0) GO TO 99
C     Z=SQRT(2.0*E**2/(E**2+ER**2))
      Z=2.0*E/(ER+E)
      RE=SQRT(Z)
      N=NL/10
      LE=NL-10*N
      IF(LE.LT.0.OR.LE.GT.8) LE=1
      IF(LE.GT.0) RE=RE*Z**LE
      RI=(E-150.0)/(ER-150.0)
      IF(RI.LT.0.0) RI=0.0
      RI=RE*RI**3
      GE=GE*RE
      GI=GI*RI
      GT=GE+GI
      Z=ER-E
      D=Z**2+GT**2
      ZR=GE*Z/D
      ZI=GE*GT/D
      IF(P(4).EQ.0.0) GO TO 1
      SR=1.0-2.0*ZI
      SI=2.0*ZR
      Z2=ZR**2+ZI**2
      ZE=P(4)*Z2
      D2=1.0+ZE**2
      ZR=ZR+(SR*ZE-SI*ZE**2)/D2
      ZI=ZI+(SR*ZE**2+SI*ZE)/D2
    1 SR=1.0-2.0*TI
      SI=2.0*TR
      TR=TR+SR*ZR-SI*ZI
      TI=TI+SR*ZI+SI*ZR
   99 RETURN
      END
C ******************************************************
      SUBROUTINE ADDRES2(WR,WI,DRL,DIM,TR,TI,NL,P)
C ADD RESONANCE FOR COMPLEX(WR,WI) ENERGY 10/18/94 ARNDT
      DIMENSION P(4)
      DATA WE,WIN/1078.0,1218.0/
      SAVE
      GT=-P(2)
      IF(GT.LT.20.0) GO TO 99
      WRES=ABS(P(3))
      GE=P(1)*GT
      GI=GT-GE
      IF(GI.LT.0.0) GI=0.0
      GE=GT-GI
      N=NL/10
      LE=NL-10*N
      D=1.0/(WR**2+WI**2)
      ZR=1.0-D*WE*WR
      ZIR=1.0-D*WIN*WR
      Z=1.0-WE/WRES
      ZZ=1.0-WIN/WRES
      ZR=ZR/Z
      ZIR=ZIR/ZZ
      IF(WR.LT.WIN) ZIR=0.0
      IF(WI.EQ.0.0) GO TO 1
      ZI=D*WE*WI/Z
      ZII=D*WIN*WI/ZZ
      RER=ZR
      REI=ZI
      CALL SQZ(RER,REI)
      RIR=ZIR**2-ZII**2
      RII=ZIR*ZII*2.0
      IF(LE.LT.1) GO TO 2
      DO 3 L=1,LE
      Z=RER
      RER=Z*ZR-REI*ZI
    3 REI=Z*ZI+REI*ZR

      GO TO 2
    1 REI=0.0
      IF(ZR.LE.0.0) ZR=0.0
      RER=SQRT(ZR)
      IF(LE.GT.0) RER=RER*ZR**LE
      RIR=ZIR**2
      RII=0.0
    2 GER=GE*RER
      GEI=GE*REI
      IF(P(3).GT.0.0) GO TO 8
      ZR=(WR-WE)/(WRES-WE)
      ZI=WI/(WRES-WE)
      Z=GER
      GER=Z*ZR-GI*ZI
      GEI=ZR*GEI+Z*ZI
    8 GIR=GI*RIR
      GII=GI*RII
      GTR=GER+GIR
      GTI=GEI+GII
      DR=WRES-WR+GTI
      DI=-WI-GTR
      Z=DRL
      DRL=Z*DR-DI*DIM
      DIM=Z*DI+DR*DIM
      D2=DR**2+DI**2
      TRR=(GER*DR+GEI*DI)/D2
      TRI=(GEI*DR-GER*DI)/D2
      IF(P(4).EQ.0.0) GO TO 5
      SR=1.0-2.0*TRI
      SI=2.0*TRR
      ZB=P(4)*GER**2/((WRES-WR)**2+GTR**2)
      TBR=ZB/(1.0+ZB**2)
      TBI=TBR*ZB
      TRR=TRR+SR*TBR-SI*TBI
      TRI=TRI+SR*TBI+SI*TBR
    5 SR=1.0-2.0*TI
      SI=2.0*TR
      TR=TR+SR*TRR-SI*TRI
      TI=TI+SR*TRI+SI*TRR
   99 RETURN
      END
C ****************************************************
      SUBROUTINE ETA33(X,TR,TI)
      DIMENSION P(4)
      DATA P/70.71,160.1,221.0,0.0307/
C CORRECTS PI-P,CXS P33 FOR N-G CROSS SECTION X=TLAB 11/91 ARNDT
      SAVE
      BW=P(1)**2/((X-P(2))**2+P(1)**2)
      Z=X**2/(X**2+P(3)**2)
      YS=P(4)*BW*Z
      ETA=1.0-YS
      TR=ETA*TR
      TI=ETA*TI+(1.0-ETA)/2.0
      RETURN
      END
C *******************************************************************
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

C      ********************************************************
C      *                                                      *
C      *                   P E P S I  2                       *
C      *                                                      *
C      *   MONTE CARLO PROGRAM FOR POLARIZED LEPTOPRODUCTION  *
C      *   october 1991                                       *
C      ********************************************************
C
C      PEPSI2 is a modification of the LEPTO6.5 code written
C      by G. Ingelman. The authors have modified LEPTO to the extent
C      necessary to simulate scattering off a polarized target.
C
C      PEPSI code is written by:
C
C      L. Mankiewicz Institut fuer Theoretische Physik
C                    der Universitaet Heidelberg
C                    Philosephenweg 16, W-6900 Heidelberg, Germany
C
C      A. Schaefer   Institut fuer Theoretische Physik
C                    Universitaet Frankfurt
C                    PO Box 111932 60054 Frankfurt, Germany
C
C      M. Veltri     Max-Planck Institut fuer Kernphysik
C                    PO Box 103980 W-6900 Heidelberg, Germany
C                    vlt@dxnhd1.mpi-hd.mpg.de - 13616::VLT
C
C       H. Ihssen    HERMES group, DESY, Notkestrasse 85,
C                    D-22603 Hamburg 52, Germany
C
C       M. Maul      Institut fuer Theoretische Physik
C                    Universitaet Frankfurt
C
C
C***************************************************************
C
C  SUBROUTINES FROM LEPTO
C ltimex 
C leptod 
C linit  
C lepto  
C leptox 
C lkinem 
C lqcdpr 
C lqev   
C lqgev  
C lqqbev 
C lxp   
C lzp   
C lqmcut 
C lazimu 
C dsigma 
C dsigm2 
C dqcd   
C dqcdi  
C lflav  
C lremh  
C lprikt 
C lframe 
C lwbb   
C lweits 
C lprwts 
C lsigmx 
C lxsect 
C riwibd 
C dvnopt 
C dfun   
C riwfun 
C dcross 
C dlower 
C dupper 
C fltabl 
C flipol 
C flintg 
C flqint 
C flgint 
C fltint 
C lsci   
C leaswi 
C lecswi 
C lsmall 
C lshowr 
C lmeps  
C lscale 
C lysspa 
C lyremn 
C lyspli 
C lmcmnd 
C lminto 
C lmidat 
C lminew 
C lmprin 
C lmpint 
C lmrazz 
C lmsimp 
C gadap  
C gadap2 
C gadapf 
C lnstrf 
C lystfu 
C
C SUBROUTINES UNIQUE IN PEPSI
C
C poldqcd 
C poldqcdi
C pollazimu
C pollzp  
C poldsigma
C polleptox
C pollinit 
C dstfu  
C dnstrf 
C parton 
C polpar 
C rdarry 
C polini 
C grv94lo
C grv94ho
C grv94di
C fv 
C fw 
C fws
C parpol 
C dfint
C polnlo
C rdarrynlo
C nloini      
c inideflo
      
C######################################################################C
C                                                                      C
C                          L E P T O                                   C
C                                                                      C
C                  A Monte Carlo Generator for                         C
C            Deep Inelastic Lepton-Nucleon Scattering                  C
C                                                                      C
C                  version 6.5,  April 20, 1996                        C
C                                                                      C
C   Authors:                                                           C
C   Gunnar Ingelman, DESY theory group     Dept. of Radiation Sciences C
C                    (room 202  bldg 2a)   Uppsala University          C
C                    D-22603 Hamburg, FRG  S-751 21 Uppsala, Sweden    C
C             phone: +49(40)8998-2795      +46(18)18-3884              C
C               fax:            -2777               -3833              C
C            e-mail: ingelman@desy.de      (ingelman@tsl.uu.se)        C
C   Anders Edin, Dept. of Radiation Sciences, edin@tsl.uu.se           C
C   Johan Rathsman, Dept. of Radiation Sciences, rathsman@tsl.uu.se    C
C                                                                      C
C   Contributions on parton cascades: M. Bengtsson, T. Sjostrand       C
C                                                                      C
C   Home page: http: 
C                                                                      C
C   Manual: G.Ingelman, A.Edin, J.Rathsman, DESY 96-057                C
C                                                                      C
C   Please report any problems or suggestions for improvements.        C
C                                                                      C
C######################################################################C




      SUBROUTINE LTIMEX(TIME)
      
      IMPLICIT NONE
      REAL TIME      
C...Interface routine to transfer a call to some machine-dependent
C...routine to get the execution time used since job started.
C...Nice, but not necessary information. Can also be called by user.

      TIME=0.
C...Use of CERN library routine Z007, replace/delete if not available.
C     CALL TIMEX(TIME)
      RETURN
      END

C **********************************************************************

C **********************************************************************

      BLOCK DATA LEPTOD
      
      IMPLICIT NONE
C...Give sensible default values to switches and parameters.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LFLMIX/ CABIBO(4,4)
      REAL CABIBO
*DEC Station gave a warning      SAVE /CABIBO/

      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/


      COMMON /FLGRID/ NFX,NFQ,XR(2),QR(2),FLQT(41,16),FLGT(41,16),
     &FLMT(41,16)
      REAL    XR,QR,FLQT,FLGT,FLMT
      INTEGER NFX,NFQ
      SAVE /FLGRID/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      REAL XKIN,UKIN,WKIN,AIN,BIN,RELERR,RELUP,RELER2,FCNMAX
      INTEGER MAXFIN
      SAVE /LMINUI/

      COMMON /LMINUC/ NAMKIN(4),NAM(30)
      CHARACTER*10 NAMKIN,NAM
      SAVE /LMINUC/

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      INTEGER I
      
C...LEPTOU: Cuts, basic switches and parameters.
      DATA CUT/1.E-04,1.,0.,1.,4.,1.E+08,5.,1.E+08,1.,1.E+08,1.,1.E+08,
     &0.,3.1416/
C...            0    1    2    3    4    5    6    7    8    9
      DATA LST/      0,   1,   5,   1,   3,   1,   1,  12,   5,
     1          1,   0,   4,   5,   4,   9,   1,   0,   2, -10,
     2          5,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     3          0,   0,   0,   0,   1,   1,   0,   0,   1,   0,
     4          0 /
C...            0    1    2    3    4    5    6    7    8    9
      DATA PARL/     1.,  1.,  0.44,0.75,.2319,0., 0.5, .04, 4.,
     1         0.0, 0.01, 0.01,0.1, 0.35,0.01,7.29735E-03,
     &                                   1.16639E-05,0.044,0.03,
     2         0.1,10*0./
C...Internally used variables.
      DATA PARI/50*0./
      DATA QC/-.33333,.66667,-.33333,.66667,-.33333,.66667,
     &        -.33333,.66667/
      DATA CABIBO/.95,.05,2*0.,.05,.948,.002,2*0.,.002,.998,4*0.,1./
      DATA OPTX/1.,3*0./,OPTY/1.,3*0./,OPTQ2/1.,3*0./,OPTW2/1.,3*0./
      DATA NXX,NWW/31,21/
      DATA PQG,PQQB,QGMAX,QQBMAX/6510*0./,YCUT/651*0./,XTOT/651*1.0E-12/
      DATA NFX,NFQ/41,16/,FLQT,FLGT,FLMT/1968*0./
      DATA XKIN/1.,2.,3.,4./,UKIN,WKIN,AIN,BIN/16*0./,MAXFIN/2000/
      DATA RELUP,RELERR,RELER2/0.1,0.05,0.05/
      DATA NAMKIN/'         x','          ','          ','          '/
      DATA IPY/
     1 0,     0,     2,     2,     6,     1,     1,     6,     3,     1,
     2 3,     1,     1,     2,     1,     1,     4,     1,     1,     1,
     3 0,     1,     1,     1,     1,     1,     1,     0,     0,     0,
     4 1,     2,     1,     1,    30,    33,     1,     1,     7,     0,
     5 0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     6 0,     0,     0,     1,   100,     0,     0,     0,     0,     0,
     7 0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     8 0,     0,     0,     0,     0,     0,     0,     0,     0,     0/
      DATA (PYPAR(I),I=1,40)/
     1   7.299E-03,   2.290E-01,   2.000E-01,   2.500E-01,   4.000E+00,
     1   1.000E+00,   4.400E-01,   4.400E-01,   7.500E-02,   0.000E+00,
     2   2.000E+00,   0.000E+00,   1.000E+00,   0.000E+00,   3.000E+00,
     2   1.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   1.000E+00,
     3   2.500E-01,   1.000E+00,   2.000E+00,   1.000E-03,   1.000E+00,
     3   1.000E+00,   1.000E+00,  -2.000E-02,  -1.000E-02,   0.000E+00,
     4   0.000E+00,   1.600E+00,   0.500E+00,   0.200E+00,   3.894E-01,
     4   1.000E+00,   3.300E-01,   6.600E-01,   0.000E+00,   1.000E+00/
      DATA (PYPAR(I),I=41,80)/
     5   2.260E+00,   1.000E+04,   1.000E-04,   0.000E+00,   0.000E+00,
     5   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     6   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     6   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     7   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     7   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     8   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,
     8   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00,   0.000E+00/
      DATA PYVAR/80*0./

      DATA IPLST/0,11,12,7*0/
      DATA CPDFNAM /
     1     './pdf/MRSa1.dat','./pdf/polnloA.dat', 
     2     './pdf/MRSa1.dat','./pdf/polnloB.dat',
     3     './pdf/MRSa1.dat','./pdf/polnloC.dat',
     4     ' ','./pdf/polparA.dat',' ','./pdf/polparB.dat',
     6     ' ','./pdf/polparC.dat',
     7     ' ','./pdf/STDNLO.GRID',' ','./pdf/VALNLO.GRID',
     9     ' ','./pdf/STDLO.GRID',' ','./pdf/VALLO.GRID',
     1     ' ','./pdf/S1NLO.GRID',
     2     ' ','./pdf/S2NLO.GRID',
     3     ' ','./pdf/S3NLO.GRID',
     4     ' ','./pdf/S1LO.GRID',
     5     ' ','./pdf/S2LO.GRID',
     6     ' ','./pdf/S3LO.GRID',
     7     './pdf/cteq4lq.tbl',' ',
     8     './pdf/MRSasq.dat','./pdf/MRSalq.dat',
****** grsv with grv98 a unpol refenrence |bs>*********************
     9     './pdf/grv98lo.grid','./pdf/std2000_lo_g1.grid',           
     1     './pdf/grv98lo.grid','./pdf/val2000_lo_g1.grid',
     2     './pdf/grv98nlo.grid','./pdf/std2000_nlo_g1.grid',
     3     './pdf/grv98nlo.grid','./pdf/std2000_nlo_g1.grid',
****** CTEQ 5 unpol ***********************************************
     4     './pdf/cteq5l.tbl',' ',
     5     './pdf/cteq5m.tbl',' ',
     6     './pdf/cteq5m1.tbl',' ',
****** MRS 99 unpol ***********************************************
     7     './pdf/cor01.dat',' ',
     8     './pdf/cor02.dat',' ',
     9     './pdf/cor03.dat',' ',
     1     './pdf/cor04.dat',' ',
     2     './pdf/cor05.dat',' ',
     3     './pdf/cor06.dat',' ',
     4     './pdf/cor07.dat',' ',
     5     './pdf/cor08.dat',' ',
     6     './pdf/cor09.dat',' ',
     7     './pdf/cor10.dat',' ',
     8     './pdf/cor11.dat',' ',
     9     './pdf/cor12.dat',' ', 
     1     './pdf/cteq6l.tbl',' ',
     2     './pdf/cteq6d.tbl',' ',
     3     './pdf/cteq6m.tbl',' ' /
      DATA IPDFNAM /
     1     107,1,108,2,109,3,
     4     113,4,114,5,115,6,
     7     116,7,117,8,118,9,119,10,
     1     124,11,125,12,126,13,127,14,128,15,129,16,
     7     133,17,137,18,144,19,145,20,146,21,
     1     147,22,150,23,151,24,152,25,161,26,
     2     162,27,163,28,164,29,165,30,166,31,167,32,
     3     168,33,169,34,170,35,171,36,172,37,173,38,174,39,175,40          /

      END


C **********************************************************************

      SUBROUTINE LINIT(LFILE,LEPIN,PLZ,PPZ,INTER)

      IMPLICIT NONE
      
C...Initialize for an incoming lepton (type LEPIN, momentum pz=PLZ)
C...and target nucleon (momentum pz=PPZ) to interact via INTER.
C...Find maximum of differential cross section, calculate QCD event
C...probabilities or read them from logical file LFILE (if >0).
C...Numerical integration to obtain total cross-section.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/


      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/

      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DBETA
      REAL STHETA,SPHI,PB,PHIR
      SAVE /LBOOST/

      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      REAL XKIN,UKIN,WKIN,AIN,BIN,RELERR,RELUP,RELER2,FCNMAX
      INTEGER MAXFIN
      SAVE /LMINUI/

      COMMON /LMINUC/ NAMKIN(4),NAM(30)
      CHARACTER*10 NAMKIN,NAM
      SAVE /LMINUC/

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARIPY(200) 
      INTEGER MSTP,MSTI
      REAL PARP,PARIPY
      SAVE /PYPARS/

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

      COMMON/MAXFROMHAND/ FMAXFH
      REAL FMAXFH
      SAVE/MAXFROMHAND/

      INTEGER INTER,LFILE,LEPIN,NCALL,I,J,IFL
      REAL PLZ,PPZ,PI,PM2,ROOTS
      REAL ULMASS,ULANGL,UMIN,UMAX,S
      INTEGER LQCD,LTM,IPMAX,IP,IW,IX
      REAL TI1,TI2

      INTEGER LSTW(40)
      REAL PARLW(30)
      DATA PI/3.1415927/,NCALL/0/

*PEPSI>>
      external leptod   ! MB, 15-Nov-2002, Load default PDF names

      DO I=1,IMXPDF
      IF(LST(15).eq.IPDFNAM(1,I)) THEN
         CUNPOL=CPDFNAM(1,IPDFNAM(2,I))
         CPOL  =CPDFNAM(2,IPDFNAM(2,I))
      ENDIF
      ENDDO

      IF (LST(15).EQ.150) CALL  SETCTQ5(3) 
      IF (LST(15).EQ.151) CALL  SETCTQ5(1) 
      IF (LST(15).EQ.152) CALL  SETCTQ5(8) 
      IF (LST(15).EQ.173) CALL  SETCTQ6(1)
      IF (LST(15).EQ.174) CALL  SETCTQ6(2)
      IF (LST(15).EQ.175) CALL  SETCTQ6(3)
      CALL POLLINIT(LFILE,LEPIN,PLZ,PPZ,INTER)
      IF (LST(40).NE.0) RETURN
*PEPSI<<       

      NCALL=NCALL+1
      LST3=LST(3)
      IF(LST(18).GE.1) THEN
C...W, Z masses from theta-Weinberg, Fermi constant GF and rad. corr.
        PMAS(24,1)=SQRT(PI*PARL(16)/(SQRT(2.)*PARL(17)*PARL(5)*
     &  (1.-PARL(18))))
        PMAS(23,1)=PMAS(24,1)/SQRT(1.-PARL(5))
      ENDIF
C...Couplings between Z0 and left/right-handed leptons and quarks.
      ZL(1,1)=-.5+PARL(5)
      ZL(1,2)=PARL(5)
      ZL(2,1)=ZL(1,2)
      ZL(2,2)=ZL(1,1)
      ZL(1,3)=0.5
      ZL(2,3)=0.
      ZL(1,4)=0.
      ZL(2,4)=0.5
      DO 10 IFL=1,8
      ZQ(1,IFL)=SIGN(0.5,QC(IFL))-QC(IFL)*PARL(5)
   10 ZQ(2,IFL)=-QC(IFL)*PARL(5)

C...Set initial state.
      LST(23)=INTER
      KSAVE(1)=LEPIN
      KSAVE(2)=2212
      K(1,1)=21
      K(1,2)=KSAVE(1)
      K(1,3)=0
      K(1,4)=0
      K(1,5)=0
      K(2,1)=21
      K(2,2)=KSAVE(2)
      K(2,3)=0
      K(2,4)=0
      K(2,5)=0
      P(1,1)=0.
      P(1,2)=0.
      P(1,3)=PLZ
      P(1,5)=ULMASS(KSAVE(1))
      P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
      P(2,1)=0.
      P(2,2)=0.
      P(2,3)=PPZ
      P(2,5)=ULMASS(KSAVE(2))
      P(2,4)=SQRT(P(2,3)**2+P(2,5)**2)
      N=2
      LST(28)=3
C...Save momentum vectors of incoming particles
      DO 20 I=1,2
      DO 20 J=1,5
   20 PSAVE(3,I,J)=P(I,J)
C...Dot-product of initial particles, cms energy
      PARL(21)=2.*(DBLE(P(1,4))*DBLE(P(2,4))-DBLE(P(1,3))*DBLE(P(2,3)))
      ROOTS=SQRT((DBLE(P(1,4))+DBLE(P(2,4)))**2
     &          -(DBLE(P(1,3))+DBLE(P(2,3)))**2)
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1000)
     & LEPIN,(P(1,J),J=1,3),PARL(1),PARL(2),(P(2,J),J=1,3),INTER,ROOTS
      write(*,*) 'INTER,ROOT',INTER,ROOTS
      IF(PLZ*PPZ.GT.0.1) THEN
        WRITE(6,1010)
        STOP
      ENDIF

C...Reduced header for Jetset/Pythia
      MSTU(12)=0
      MSTP(122)=0
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) 
     &WRITE(6,1020) MSTU(181),MSTU(182),MSTP(181),MSTP(182)
C...If JETSET version before 7.402, problem with azimuthal dependence
C...in LUSHOW solved by chosing flat azimuthal dependence.
      IF(MSTU(181).LE.7.AND.MSTU(182).LT.402) THEN
        MSTJ(46)=0
        WRITE(6,1030) MSTJ(46)
      ENDIF
C...Initialize PYTHIA for parton densities.
      IF(LST(15).GT.0) THEN
C...Set switches and parameters for parton densities in PYSTFU.
        MSTP(51)=LST(15)
        MSTP(52)=LST(16)
        MSTP(58)=LST(12)
      ENDIF

      write(*,*) 'PYINIT not included !'
*HI      CALL PYINIT('NONE','e-','p',ROOTS)      



      PARL(26)=PARP(1)
CAE--     use Lambda from parton densities in initial cascade
      PYPAR(21)=PARP(1)
C...Reset PYTHIA 4.8 parameters from LEPTO parameters.
      IF(MOD(LST(8),10).EQ.3.OR.MOD(LST(8),10).EQ.5) IPY(13)=0
      IF(LST(35).NE.1.AND.
     &(MOD(LST(8),10).EQ.4.OR.MOD(LST(8),10).EQ.5)) IPY(14)=0
      IPY(8)=LST(12)

      IF(PSAVE(3,1,3).LT.0.) THEN
C...Flip event to have initial lepton along +z axis
        P(1,3)=-P(1,3)
        P(2,3)=-P(2,3)
      ENDIF
C...Boost parameters to cms of incoming particles
      DBETA(1,1)=0.D0
      DBETA(1,2)=0.D0
      DBETA(1,3)=(DBLE(P(1,3))+DBLE(P(2,3)))/(DBLE(P(1,4))+DBLE(P(2,4)))
      SPHI(1)=0.D0
      STHETA(1)=0.D0
      IF(LST(17).NE.0) THEN
C...For varying beam energies, transform to cms, lepton along +z axis.
        CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,-DBETA(1,3))
        SPHI(1)=ULANGL(P(1,1),P(1,2))
        CALL LUDBRB(0,0,0.,-SPHI(1),0.D0,0.D0,0.D0)
        STHETA(1)=ULANGL(P(1,3),P(1,1))
        CALL LUDBRB(0,0,-STHETA(1),0.,0.D0,0.D0,0.D0)
        LST(28)=2
      ENDIF

C...Effective limits on kinematic variables x, y, Q**2, W**2
      PM2=P(2,5)**2
      S=PARL(21)
      XMIN=MAX(CUT(1),0.)
      XMAX=MIN(CUT(2),1.)
      YMIN=MAX(CUT(3),0.)
      YMAX=MIN(CUT(4),1.)
      Q2MIN=MAX(CUT(5),0.)
      Q2MAX=MIN(CUT(6),S)
      W2MIN=MAX(CUT(7),0.)
      W2MAX=MIN(CUT(8),S+PM2)
      UMIN=MAX(CUT(9),0.)
      UMAX=MIN(CUT(10),S/(2.*P(2,5)))
      DO 40 I=1,2
      XMIN=MAX(XMIN,Q2MIN/(S*YMAX),Q2MIN/(2.*P(2,5)*UMAX),
     &1.-(W2MAX-PM2)/MAX(S*YMIN,1.E-22),
     &1.-(W2MAX-PM2)/MAX(2.*P(2,5)*UMIN,1.E-22))
      XMAX=MIN(XMAX,Q2MAX/MAX(S*YMIN,1.E-22),
     &Q2MAX/MAX(2.*P(2,5)*UMIN,1.E-22),
     &1.-(W2MIN-PM2)/(S*YMAX),1.-(W2MIN-PM2)/(2.*P(2,5)*UMAX))
      YMIN=MAX(YMIN,Q2MIN/(S*XMAX),(W2MIN-PM2)/(S*(1.-XMIN)),
     &(W2MIN-PM2+Q2MIN)/S,2.*P(2,5)*UMIN/S)
      YMAX=MIN(YMAX,Q2MAX/MAX(S*XMIN,1.E-22),
     &(W2MAX-PM2)/MAX(S*(1.-XMAX),1.E-22),
     &(W2MAX-PM2+Q2MAX)/S,2.*P(2,5)*UMAX/S)
      Q2MIN=MAX(Q2MIN,S*XMIN*YMIN,S*YMIN-W2MAX+PM2,
     &2.*P(2,5)*UMIN*XMIN,(W2MIN-PM2)*XMIN/(1.-XMIN))
      Q2MAX=MIN(Q2MAX,S*XMAX*YMAX,S*YMAX-W2MIN+PM2,
     &2.*P(2,5)*UMAX*XMAX,(W2MAX-PM2)*XMAX/MAX(1.-XMAX,1.E-22))
      W2MIN=MAX(W2MIN,S*(1.-XMAX)*YMIN+PM2,Q2MIN*(1.-XMAX)/XMAX+PM2,
     &S*YMIN-Q2MAX+PM2,2.*P(2,5)*UMIN*(1.-XMAX)+PM2)
      W2MAX=MIN(W2MAX,S*(1.-XMIN)*YMAX+PM2,
     &Q2MAX*(1.-XMIN)/MAX(XMIN,1.E-22)+PM2,
     &S*YMAX-Q2MIN+PM2,2.*P(2,5)*UMAX*(1.-XMIN)+PM2)
C     UMIN=MAX(UMIN,....)
C     UMAX=MIN(UMAX,....)
   40 CONTINUE
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1050)
     &CUT,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX,UMIN,UMAX
      IF(XMAX.LT.XMIN.OR.YMAX.LT.YMIN.OR.Q2MAX.LT.Q2MIN.OR.
     &W2MAX.LT.W2MIN) THEN
        IF(LST(3).GE.1) WRITE(6,1100)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF
      IF(XMIN.LT.1.E-10.OR.Q2MIN.LT.1.E-01) THEN
        IF(LST(3).GE.1) WRITE(6,1110)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF

      PARI(11)=(PARL(1)-PARL(2))/PARL(1)
      KSAVE(4)=LEPIN
      ILEP=1
      IF(LEPIN.LT.0) ILEP=2
      INU=0
      IF(IABS(LEPIN).EQ.12.OR.IABS(LEPIN).EQ.14
     &.OR.IABS(LEPIN).EQ.16) INU=1
      IF(INU.EQ.1) THEN
C...Set full polarisation for incoming neutrino.
        PARL(6)=-1.
        IF(LEPIN.LT.0) PARL(6)=1.
      ENDIF
      IF(LST(23).EQ.1.AND.INU.EQ.0) THEN
C...Electromagnetic interaction.
        KSAVE(3)=22
        IG=1
        IZ=0
      ELSEIF(LST(23).EQ.2) THEN
C...Weak charged current, only one helicity state contributes.
        IF(KSAVE(1).LT.0.AND.PARL(6).LT.-0.99
     &  .OR.KSAVE(1).GT.0.AND.PARL(6).GT.0.99) THEN
          IF(LST(3).GE.1) WRITE(6,1150) LEPIN,PARL(6)
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
        ENDIF
        IF(MOD(IABS(LEPIN),2).EQ.0) THEN
          KSAVE(3)=ISIGN(24,LEPIN)
          KSAVE(4)=ISIGN(IABS(LEPIN)-1,LEPIN)
        ELSE
          KSAVE(3)=ISIGN(24,-LEPIN)
          KSAVE(4)=ISIGN(IABS(LEPIN)+1,LEPIN)
        ENDIF
      ELSEIF(LST(23).EQ.3.OR.(LST(23).EQ.4.AND.INU.EQ.1)) THEN
C...Weak neutral current.
        KSAVE(3)=23
        IG=0
        IZ=1
      ELSEIF(LST(23).EQ.4.AND.INU.EQ.0) THEN
C...Neutral current, electromagnetic and weak with interference.
        KSAVE(3)=23
        IG=1
        IZ=1
      ELSE
        IF(LST(3).GE.1) WRITE(6,1200) INTER,LEPIN
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF

C...Choice of independent variables.
      IF(LST(1).EQ.0) THEN
        LST(31)=1
        IF(INTER.EQ.2.OR.INTER.EQ.3) LST(31)=2
      ELSE
        LST(31)=IABS(LST(1))
      ENDIF
      IF(LST(31).LT.1.OR.LST(31).GT.3) THEN
        IF(LST(3).GE.1) WRITE(6,1210) LST(1),LST(31)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF
      IF(LST(1).LT.0) THEN
C...User-defined optimization parameters.
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1220) OPTX,OPTY,OPTQ2,OPTW2
      ELSE
C...Set optimization parameters.
        DO 50 I=1,4
        OPTX(I)=0.
        OPTY(I)=0.
        OPTQ2(I)=0.
   50   OPTW2(I)=0.
        IF(INTER.EQ.1) THEN
          OPTX(2)=1.
          OPTY(1)=1.
          OPTQ2(3)=1.
          OPTW2(3)=1.
        ELSEIF(INTER.EQ.4) THEN
          OPTX(1)=0.1
          OPTX(2)=1.
          OPTY(1)=1.
          OPTQ2(1)=0.5
          OPTQ2(2)=0.5
          OPTQ2(3)=1.
          OPTW2(1)=0.5
          OPTW2(2)=0.5
          OPTW2(3)=1.
        ELSE
          OPTX(1)=1.
          OPTY(1)=1.
          OPTQ2(1)=1.
          OPTW2(1)=1.
        ENDIF
      ENDIF

C...Initialize Monte Carlo estimate of cross section.
      PARL(24)=0.
      PARI(27)=0.
      PARI(28)=0.
      PARI(29)=0.
      PARI(30)=0.
      PARI(32)=0.
      IF(LST(23).EQ.2) THEN
C...Constant factor GF**2/pi for CC, transformation to picobarn.
        PARI(31)=PARL(17)**2/PI*0.39E+09
      ELSE
C...Constant factor 2*pi*alpha**2 for NC, transformation to picobarn.
        PARI(31)=2.*PI*PARL(16)**2*0.39E+09
      ENDIF
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &WRITE(6,1250) (I,LST(I),LST(I+10),PARL(I),PARL(I+10),I=1,10)

C...Set up grid with longitudinal structure function, QCD & target mass;
C...only when photon exchange is included
      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      IF(LST(11).NE.0.AND.(INTER.EQ.1.OR.INTER.EQ.4)) CALL FLTABL

C...Get integrated cross-section.
      PARL(23)=0.
      IF(LST(10).GT.0) CALL LXSECT
      IF(LQCD.EQ.2.OR.LTM.EQ.2) THEN
        WRITE(6,1300)
        IF(LQCD.EQ.2) WRITE(6,1310)
        IF(LTM .EQ.2) WRITE(6,1320)
        WRITE(6,1330)
      ENDIF

      IF(LST(2).EQ.1) THEN
C...Find max value of differential cross section for rejection.
        UKIN(1)=(XMAX+XMIN)/2.
        WKIN(1)=0.8*(XMAX-XMIN)/2.
        AIN(1)=XMIN
        BIN(1)=XMAX
        IF(LST(31).EQ.1) THEN
          UKIN(2)=(Q2MAX+Q2MIN)/2.
          WKIN(2)=0.8*(Q2MAX-Q2MIN)/2.
          AIN(2)=Q2MIN
          BIN(2)=Q2MAX
          NAMKIN(2)='      Q**2'
        ELSEIF(LST(31).EQ.2) THEN
          UKIN(2)=(YMAX+YMIN)/2.
          WKIN(2)=0.8*(YMAX-YMIN)/2.
          AIN(2)=YMIN
          BIN(2)=YMAX
          NAMKIN(2)='         y'
        ELSEIF(LST(31).EQ.3) THEN
          UKIN(2)=(W2MAX+W2MIN)/2.
          WKIN(2)=0.8*(W2MAX-W2MIN)/2.
          AIN(2)=W2MIN
          BIN(2)=W2MAX
          NAMKIN(2)='      W**2'
        ENDIF
C...Maximum obtained by minimizing -(diff. x-section).
        CALL LTIMEX(TI1)
        CALL LMINEW
        CALL LTIMEX(TI2)
        PARI(LST(23))=FCNMAX*1.1
CMM.. Maximum inserted by hand if MINUIT fails
        IF(PARI(LST(23)).EQ.0.0) PARI(LST(23))=FMAXFH
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1400) PARI(LST(23)),TI2-TI1
      ENDIF

      IF(LFILE.GT.0.AND.LST(19).GE.0) THEN
C...Read QCD weights from file.
        READ(LFILE) LSTW,PARLW,NXX,NWW,NP,XX,WW
        IPMAX=2
        IF(LSTW(17).NE.0) IPMAX=3
        READ(LFILE) (((PQG(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((PQQB(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((QGMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,IPMAX),
     &  (((QQBMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,MIN(2,IPMAX)),
     &  YCUT
        IF(NP.NE.1) READ(LFILE) XTOT
        CLOSE(LFILE)
C...Reset parameters for matrix element integration.
        PARL(8)=PARLW(8)
        PARL(9)=PARLW(9)
        PARL(11)=PARLW(11)
        PARL(12)=PARLW(12)
        PARL(13)=PARLW(13)
C...Check current parameter values against those used when
C...calculating weights.
        IF(LST(12).NE.LSTW(12).OR.LST(13).NE.LSTW(13)
     &  .OR.LST(15).NE.LSTW(15).OR.LST(16).NE.LSTW(16)
     &  .OR.LST(17).NE.LSTW(17).OR.LST(23).NE.LSTW(23)
     &  .OR.ABS(PARL(1)-PARLW(1)).GT.0.1.OR.ABS(PARL(2)-PARLW(2)).GT.0.1
     &  .OR.ABS(PARL(5)-PARLW(5)).GT.0.01
     &  .OR.ABS(PARL(6)-PARLW(6)).GT.0.1) THEN
         IF(LST(3).GE.1)
     &    WRITE(6,1500) LST(12),LSTW(12),LST(13),LSTW(13),LST(15),
     &    LSTW(15),LST(16),LSTW(16),LST(17),LSTW(17),LST(23),LSTW(23),
     &    PARL(1),PARLW(1),PARL(2),PARLW(2),PARL(5),PARLW(5),PARL(6),
     &    PARLW(6)
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
        ENDIF
      ELSEIF((LST(19).GE.0.OR.LST(19).EQ.-10).AND.
     &(LST(8).EQ.1.OR.LST(8)/10.EQ.1.OR.MOD(LST(8),10).EQ.9)) THEN
C...Calculate weights if 1st order QCD from grid is requested.
        CALL LTIMEX(TI1)
        CALL LWEITS(LFILE)
        CALL LTIMEX(TI2)
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1510) TI2-TI1
      ENDIF


C...Reset counters to zero for Monte Carlo estimate of cross section.
      PARI(27)=0.
      PARI(28)=0.
      PARI(29)=0.
      PARI(30)=0.
      LST(32)=0
      RETURN
c
c
 1000 FORMAT(' ', 
     &'A MONTE CARLO GENERATOR FOR DEEP INELASTIC LEPTON-'
     &,'NUCLEON SCATTERING',/,5X,68('='), 
     &25X,'LEPTO version 6.5, April 20, 1996', 
C    &25X,'PRELIMINARY VERSION, DO NOT CIRCULATE', 
     &' Lepton: type =',I3,5X,'momentum (px,py,pz) =',5F9.2,
     &' GeV', 
     &'momentum (px,py,pz) =',3F9.2,' GeV', 
     &' Interaction :',I3,14X,' CMS energy =',1PG12.4,' GeV',/)
 1010 FORMAT(' Warning: lepton and nucleon momenta in same direction',
     &' not allowed.',/,10X,'Execution stopped.')
 1020 FORMAT(/,' JETSET version ',I3,'.',I3,' is used.',/,
     &' Parton densities in PYTHIA version ',I3,'.',I3,' are used.',/)
 1030   FORMAT(' Warning (LINIT): JETSET version before 7.402, MSTJ(46)'
     &  ,' set to',I4,/,18X,'to avoid mismatch LEPTO<-->LUSHOW.',/)
 1050 FORMAT(/,' User applied cuts (+ phase space) : ',1P,
     &      G12.4,' <   x   < ',G12.4,
     &/,37X,G12.4,' <   y   < ',G12.4,
     &/,37X,G12.4,' < Q**2  < ',G12.4,
     &/,37X,G12.4,' < W**2  < ',G12.4,
     &/,37X,G12.4,' <  nu   < ',G12.4,
     &/,37X,G12.4,' <  E''   < ',G12.4,
     &/,37X,G12.4,' < theta < ',G12.4,/,
     &/,       ' Effective ranges (from above cuts): ',
     &      G12.4,' <   x   < ',G12.4,
     &/,37X,G12.4,' <   y   < ',G12.4,
     &/,37X,G12.4,' < Q**2  < ',G12.4,
     &/,37X,G12.4,' < W**2  < ',G12.4,
     &/,37X,G12.4,' <  nu   < ',G12.4)
 1100 FORMAT(' Warning: effective upper limit of kinematical ',
     &'variable(s) smaller than corresponding lower limit.')
 1110 FORMAT(' Warning: lower limit in x and/or Q2 too small for ',
     &'DIS formalism.')
 1150 FORMAT(' Warning: weak charged current cross section zero for ',
     &'specified lepton helicity; LEPIN, PARL(6) =',I3,F5.2)
 1200 FORMAT(' Warning: unrecognized interaction in LINIT call: ',
     &'INTER = ',I5,'  for lepton LEPIN =',I5)
 1210 FORMAT(' Warning: unallowed value of LST(1) =',I3,
     &' and/or LST(31) =',I3)
 1220 FORMAT(/,' User-defined optimization parameters:',
     &/,5X,'OPTX(1...4)  =',4G11.3,/,5X,'OPTY(1...4)  =',4G11.3,
     &/,5X,'OPYQ2(1...4) =',4G11.3,/,5X,'OPTW2(1...4) =',4G11.3,/)
 1250 FORMAT(/,' Parameter values:', 
     &'LST(I+10)',8X,'PARL(I)',5X,'PARL(I+10)',1P,
     &/,5X,55('-'),10(/,3I10,2G15.4),/)
 1300 FORMAT(' Warning: cross section, PARL(23), excludes FL (see ',
     &'LST(11)) from:')
 1310 FORMAT(10X,'QCD, since evaluated event by event for LQCD=2')
 1320 FORMAT(10X,'TM , since evaluated event by event for LTM =2')
 1330 FORMAT(' Cross section in PARL(24) includes these contributions.')
 1400 FORMAT(' Max of differential cross section (for weighting) =',
     &E12.4,/,' obtained in ',F7.2,' seconds.',/)
 1500 FORMAT( 
     &'with those used when calculating QCD weights.', 
     &'current value     value for weights',/,
     &/,'     LST(12)   ',I12,10X,I12,
     &/,'     LST(13)   ',I12,10X,I12,
     &/,'     LST(15)   ',I12,10X,I12,
     &/,'     LST(16)   ',I12,10X,I12,
     &/,'     LST(17)   ',I12,10X,I12,
     &/,'     LST(23)   ',I12,10X,I12,
     &/,'     PARL(1)   ',E12.4,10X,E12.4,
     &/,'     PARL(2)   ',E12.4,10X,E12.4,
     &/,'     PARL(5)   ',E12.4,10X,E12.4,
     &/,'     PARL(6)   ',E12.4,10X,E12.4)
 1510 FORMAT(/,' Time for calculating QCD weights =',F5.1,' seconds',/)
 1900 FORMAT(' Execution stopped ',/)
      END


C **********************************************************************

      SUBROUTINE LEPTO

      IMPLICIT NONE

C...Administer the generation of an event.
C...Note: if error flag LST(21) is non-zero, no proper event generated.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DBETA
      REAL STHETA,SPHI,PB,PHIR
      SAVE /LBOOST/


* if ARIADNE is used to simulate the parton shower evolution, the
* ARDAT1 Common block is neccessary for a proper interface.
* if ARIADNE is used to simulate the parton shower evolution, the
* ARDAT1 Common block is neccessary for a proper interface.
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      REAL PARA
      INTEGER MSTA
      SAVE /ARDAT1/ 


      INTEGER NUMMIS,NWARN,I,J,NS,L17
      REAL ULMASS,ULANGL,RLU,ULALPS
      REAL QG,QQB,SPQ,SRLU,PLU,PT,PHI,ENERGY,P2,ATAN
      DOUBLE PRECISION DETOT,DARI29,DARI30
      DIMENSION SPQ(17)
      DATA NUMMIS,NWARN/0,10/,DARI29,DARI30/2*0.D0/

      L17=0
    1 LST(21)=0
      DO 10 I=1,10
      DO 10 J=1,5
      K(I,J)=0
   10 V(I,J)=0.
      DO 15 I=1,4
      K(I,1)=21
   15 K(I,2)=KSAVE(I)
      K(4,1)=1
      N=2

      IF(LST(17).NE.0.AND.LST(2).GT.0) THEN
C...Lepton and/or nucleon energy may vary from event to event,
        IF(L17.EQ.0) THEN
C...Momentum vectors from P(i,j) i=1,2 j=1,2,3 on entry in LEPTO
          DO 20 I=1,2
          P(I,5)=ULMASS(K(I,2))
          P(I,4)=SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2+P(I,5)**2)
          DO 20 J=1,5
   20     PSAVE(3,I,J)=P(I,J)
        ELSE
C...Momentum vectors from PSAVE if new try, i.e. jump back to 1
          DO 25 I=1,2
          DO 25 J=1,5
   25     P(I,J)=PSAVE(3,I,J)
        ENDIF
        L17=1
C...Transform to cms of incoming particles, lepton along +z axis.
        DO 30 J=1,3
   30   DBETA(1,J)=(DBLE(P(1,J))+DBLE(P(2,J)))/
     &             (DBLE(P(1,4))+DBLE(P(2,4)))
        CALL LUDBRB(0,0,0.,0.,-DBETA(1,1),-DBETA(1,2),-DBETA(1,3))
        SPHI(1)=ULANGL(P(1,1),P(1,2))
        CALL LUDBRB(0,0,0.,-SPHI(1),0.D0,0.D0,0.D0)
        STHETA(1)=ULANGL(P(1,3),P(1,1))
        CALL LUDBRB(0,0,-STHETA(1),0.,0.D0,0.D0,0.D0)
        LST(28)=2
        PARL(21)=2.*(P(1,4)*P(2,4)-P(1,3)*P(2,3))
      ELSE
C...Initial state momenta fixed from LINIT call.
        DO 42 I=1,2
        DO 40 J=1,5
   40   P(I,J)=PSAVE(3,I,J)
   42   IF(PSAVE(3,1,3).LT.0.) P(I,3)=-PSAVE(3,I,3)
        LST(28)=3
      ENDIF
      CALL LEPTOX
C...Return if error or if no event to be generated.
      IF(LST(21).NE.0.OR.LST(2).LE.0.OR.LST(7).EQ.-1) RETURN

      IF(PARI(29).LT.0.5) THEN
C...For first call, reset double precision counters.
        DARI29=0.D0
        DARI30=0.D0
      ENDIF
      DARI29=DARI29+1.D0
      PARI(29)=DARI29
C     CALL GULIST(-3,2)
C...Scattered lepton and exchanged boson added to event record in LKINEM
C...Transform to lepton-nucleon cms if not made earlier
      IF(LST(17).EQ.0) THEN
        DO 46 I=3,4
        DO 45 J=1,5
   45   PSAVE(3,I,J)=P(I,J)
   46   IF(PSAVE(3,1,3).LT.0.) PSAVE(3,I,3)=-P(I,3)
        CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,-DBETA(1,3))
        LST(28)=2
      ENDIF
      DO 50 I=1,4
      DO 50 J=1,5
   50 PSAVE(2,I,J)=P(I,J)
C     CALL GULIST(-2,2)

C...Prepare for parton cascade.
      IF(LST(8).GE.2.AND.MOD(LST(8),10).NE.9) CALL LSHOWR(0)

C...Transform to hadronic cms, boost parameters in double precision.
      DETOT=DBLE(P(1,4))-DBLE(P(4,4))+DBLE(P(2,4))
      DBETA(2,1)=-DBLE(P(4,1))/DETOT
      DBETA(2,2)=-DBLE(P(4,2))/DETOT
      DBETA(2,3)=(DBLE(P(1,3))-DBLE(P(4,3))+DBLE(P(2,3)))/DETOT
      CALL LUDBRB(0,0,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
      SPHI(2)=0.
      STHETA(2)=ULANGL(P(3,3),P(3,1))
      CALL LUDBRB(0,0,-STHETA(2),0.,0.D0,0.D0,0.D0)
      LST(28)=1
      DO 60 I=1,4
      DO 60 J=1,5
   60 PSAVE(1,I,J)=P(I,J)
C...Save momentum of exchanged boson (used in subroutine LFRAME).
      DO 70 J=1,5
   70 PB(J)=P(3,J)
C     CALL GULIST(-1,2)

   90 N=4
      MSTU(1)=N+1
      LST(26)=N+1
      LST(27)=0
      PARL(25)=ULALPS(Q2)
      IF(LST(8).EQ.1.OR.LST(8)/10.EQ.1.OR.MOD(LST(8),10).EQ.9) THEN
C...Probabilities for hard, first order QCD events.
CAE...Corrected what to do when LQGEV or LQQBEV fail. Now make LQEV.
         CALL LQCDPR(QG,QQB)

        DO 100 I=1,17
  100   SPQ(I)=PQ(I)
  200   SRLU=RLU(0)

        IF(SRLU.GT.QQB+QG) THEN
          CALL LQEV
        ELSEIF(SRLU.GT.QQB) THEN
          IF(LST(8).EQ.9) THEN
            DO 211 I=1,17
  211         PQ(I)=SPQ(I)
            CALL LQEV
          ELSE
            CALL LQGEV
            DO 212 I=1,17
              PQ(I)=SPQ(I)
  212       CONTINUE
          ENDIF
        ELSE
          CALL LQQBEV
          DO 213 I=1,17
            PQ(I)=SPQ(I)
  213     CONTINUE
          IF(LST(8).EQ.9.AND.LST(21).EQ.0) THEN
            IF(PLU(5,11).LT.Q2*PARA(20)) THEN
              DO 220 I=1,17
  220         PQ(I)=SPQ(I)
              CALL LQEVAR(K(5,2),K(7,2))
            ENDIF
          ENDIF
        ENDIF
        IF(LST(21).NE.0) THEN
  230     CALL LQEV
          IF(LST(21).NE.0) GOTO 230
        ENDIF   
      ELSE
C...QPM model without QCD corrections (cascade applied later).
  300   CALL LQEV
        IF(LST(21).NE.0) GOTO 300
      ENDIF

      NS=MSTU(1)
      MSTU(1)=0
C     CALL GULIST(-3,2)
C     WRITE(6,*) ' LST(24)=',LST(24)
CJR--       no preclustering of small systems
          MSTJ(14)=-1
CJR--            
      IF(LST(8).LE.1.OR.MOD(LST(8),10).EQ.9) THEN
C...No parton cascade, introduce primordial kt.
        IF(PARL(3).GT.1.E-03) THEN
          CALL LPRIKT(PARL(3),PT,PHI)
          CALL LUDBRB(NS,N,0.,-PHI,0.D0,0.D0,0.D0)
          CALL LUDBRB(NS,N,ATAN(2.*PT/SQRT(W2)),PHI,0.D0,0.D0,0.D0)
        ENDIF
        IF(MOD(LST(8),10).NE.9) THEN
C...Check system against fragmentation cuts.
          MSTU(24)=0
          CALL LUPREP(0)
          IF(MSTU(24).NE.0) THEN
            IF(LST(3).GE.1) WRITE(6,*)'LUPREP error MSTU(24)=',MSTU(24),
     &                                ', New event generated'
            LST(21)=1
            GOTO 1
          ENDIF
        ENDIF
      ELSEIF(LST(24).EQ.1) THEN
C...Include parton cascades (+ remnant & kt) on q-event
        CALL LSHOWR(1)
      ELSE
C...Include parton cascades (+ remnant & kt) on qg- or qqbar-event
        CALL LMEPS
      ENDIF
      IF(LST(21).NE.0) THEN
C        IF(LST(3).GE.1)
C     &     WRITE(6,*)'Cascade error LST(21)= ',LST(21),
C     &               ', New event generated'
        GOTO 1
      ENDIF

CJR--       Soft colour interactions
      IF(LST(34).EQ.1) CALL LSCI(PARL(7))
      IF(LST(21).NE.0) GOTO 1
CJR--       take care of small systems
      CALL LSMALL
      IF(LST(21).NE.0) THEN
        IF(LST(3).GE.1) WRITE(6,*)' LSMALL error LST(21)= ',LST(21),
     &                            ', New event generated'
        GOTO 1
      ENDIF
      MSTJ(14)=1
      CALL LUPREP(0)
      IF(MSTU(24).NE.0) THEN
         IF(LST(3).GE.1) WRITE(6,*)' LUPREP error MSTU(24)= ',MSTU(24),
     &                             ', New event generated'
         LST(21)=1
      ENDIF
CJR--            
      IF(LST(21).NE.0) GOTO 1

      DO 400 I=1,N
C...Correct energy-momentum-mass mismatch for real particle
      IF(P(I,5).LT.0.) GOTO 400
      ENERGY=SQRT(DBLE(P(I,5))**2+DBLE(P(I,1))**2+DBLE(P(I,2))**2+
     &DBLE(P(I,3))**2)
      P2=DBLE(P(I,4))**2-DBLE(P(I,1))**2-DBLE(P(I,2))**2-DBLE(P(I,3))**2
      IF(ABS(ENERGY-P(I,4))/(PSAVE(3,1,4)+PSAVE(3,2,4)).GT.PARU(11))THEN
        NUMMIS=NUMMIS+1
C...For testing purposes
C       IF(LST(3).GE.1.AND.NUMMIS.LE.NWARN) THEN
C         WRITE(6,1000) I,(K(I,J),J=1,2),(P(I,J),J=1,5),
C    &    SIGN(SQRT(ABS(P2)),P2),ENERGY,INT(DARI29),NWARN
C         IF(ABS(P2-P(I,5)**2).GT.400.) CALL LULIST(2)
C       ENDIF
CAE        WRITE(6,*) 'Energy mismatch',LST(24),PARL(28),PARL(29),NUMMIS
        GOTO 90
      ENDIF
      P(I,4)=ENERGY
  400 CONTINUE

      DARI30=DARI30+1.D0
      PARI(30)=DARI30
Ctest IF(LST(23).EQ.2) PARL(24)=PARL(24)*DARI30/DARI29

      DO 500 I=1,N
      DO 500 J=1,5
  500 V(I,J)=0.
      IF(LST(7).EQ.1) THEN
        CALL LUEXEC
        IF(MSTU(24).NE.0) THEN
          WRITE(6,*) ' Error from JETSET, new event made'
          GOTO 90
        ENDIF
      ENDIF

C     CALL GULIST(-1,2)
C...Transform to desired frame
C     LST(28)=1
      LST(29)=0
      PHIR=6.2832*RLU(0)
      IF(LST(17).EQ.0) THEN
        IF(LST(5).GE.2) CALL LFRAME(LST(5),0)
C...Restore momenta (e,p,boson,L) due to numerical errors from boosts
        DO 600 I=1,4
        DO 600 J=1,5
  600   P(I,J)=PSAVE(LST(28),I,J)
        IF(LST(6).EQ.1.AND.LST(28).GE.2) THEN
C...Random rotation in azimuthal angle
          CALL LUDBRB(0,0,0.,PHIR,0.D0,0.D0,0.D0)
          LST(29)=1
        ENDIF
      ELSE
        IF(LST(5).GE.2) CALL LFRAME(LST(5),LST(6))
      ENDIF
C...Deactivate scattered lepton
      IF(MOD(LST(4),10).EQ.0) K(4,1)=21
C     CALL GULIST(0,2)

      RETURN
 1000 FORMAT(' Warning: too large numerical mismatch in ',
     &'particle energy-momentum-mass',
     &/,3X,'I K(I,1) ..2)  P(I,1)  P(I,2)  P(I,3)',
     &'  P(I,4)  P(I,5)    mass  energy',/,I4,2I6,7F8.3,/,
     &' Event no.',I8,' regenerated. Only first',I5,' warnings printed')
      END

C **********************************************************************

      SUBROUTINE LEPTOX

      IMPLICIT NONE

C...Select process and choose kinematical variables (x,y; x,Q2; x,W2)
C...according to the differential cross section.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

* this Common block appears only in S LEPTOX 
      COMMON /FLINFO/ RFLQ,RFLG,RFLM,RFLT
      REAL RFLQ,RFLG,RFLM,RFLT
      SAVE /FLINFO/ 

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/


      INTEGER IH,I,NCUT,II
      INTEGER LQCD,LTM,LHT,LKINEM
      REAL W2LOW,W2UPP,YLOW,YUPP,Q2LOW,Q2UPP,PNT,PQH,S,PM2,
     +XPQ,GFQ2,AEMCOR,ULALEM,ZLEP,A,B,FLG,FLQ,
     +FLM,FLT,F2EM,PQH17,WEIGHT,HX,XFACT,Q2FACT,HY,YFACT,HQ2,
     +W2FACT,SIGL,SIGR,SIGMA,VIOL,YQ,YQB,FYQ,HW2,WHICH
      REAL RLU
*...Added array XDPQ to store delta parton distributions
      DIMENSION PQH(17,2),PNT(2,2),XPQ(-6:6)
*---
CGI-001122...d- & u-quark weights for p & n target & helicity state
      REAL PNTDU(2,2,2)
CMM-010306...anti d- & anti u-quark weights for p & n target & helicity state
      REAL PNTDUS(2,2,2)     
      DOUBLE PRECISION DARI27,DARI28
      DATA DARI27,DARI28/2*0.D0/
      DATA W2LOW,W2UPP,YLOW,YUPP,Q2LOW,Q2UPP/6*0./

*PEPSI>>
      IF (LST(40).NE.0) THEN
        CALL POLLEPTOX
        RETURN
      ENDIF 
*PEPSI<<
      DO 10 IH=1,2
      DO 5 I=1,2
CGI-001122
      PNTDU(I,1,IH)=0.
      PNTDU(I,2,IH)=0.
CMM-010306
      PNTDUS(I,1,IH)=0.
      PNTDUS(I,2,IH)=0.      
    5 PNT(I,IH)=0.
      DO 6 I=1,8
      EWQC(1,IH,I)=0.
    6 EWQC(2,IH,I)=0.
      DO 10 I=1,17
   10 PQH(I,IH)=0.
      DO 20 I=1,17
   20 PQ(I)=0.

      LST(21)=0
      NCUT=0
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      IF(LST(2).NE.1) THEN
       Q2LOW=MAX(Q2MIN,X*YMIN*S,(W2MIN-PM2)*X/MAX(1.-X,1.E-22))
       Q2UPP=MIN(Q2MAX,X*YMAX*S,(W2MAX-PM2)*X/MAX(1.-X,1.E-22))
       YLOW=MAX(YMIN,Q2MIN/MAX(S*X,1.E-22),
     & (W2MIN-PM2)/MAX(S*(1.-X),1.E-22))
       YUPP=MIN(YMAX,Q2MAX/MAX(S*X,1.E-22),
     & (W2MAX-PM2)/MAX(S*(1.-X),1.E-22))
       W2LOW=MAX(W2MIN,(1.-X)*YMIN*S+PM2,Q2MIN*(1.-X)/MAX(X,1.E-22)+PM2)
       W2UPP=MIN(W2MAX,(1.-X)*YMAX*S+PM2,Q2MAX*(1.-X)/MAX(X,1.E-22)+PM2)
       GOTO 110
      ENDIF

      IF(PARI(28).LT.0.5) THEN
C...For first call, reset double precision counters.
        DARI27=0.D0
        DARI28=0.D0
      ENDIF
  100 DARI28=DARI28+1.D0
      PARI(28)=DARI28
  101 CONTINUE
C...Choose x according to the distribution
C...hx(x) =  a + b/x + c/x**2 + d/x**3. In detail
C...hq=OPTX(1)/(XMAX-XMIN) + 1/ln(XMAX/XMIN)*OPTX(2)/X
C...   +XMIN*XMAX/(XMAX-XMIN)*OPTX(3)/X**2
C...   +2*(XMIN*XMAX)**2/(XMAX**2-XMIN**2)*OPTX(4)/X**3
      WHICH=(OPTX(1)+OPTX(2)+OPTX(3)+OPTX(4))*RLU(0)
      IF(WHICH.LE.OPTX(1)) THEN
        X=XMIN+RLU(0)*(XMAX-XMIN)
      ELSEIF(WHICH.LE.(OPTX(1)+OPTX(2))) THEN
        X=XMIN*(XMAX/XMIN)**RLU(0)
      ELSEIF(WHICH.LE.(OPTX(1)+OPTX(2)+OPTX(3))) THEN
        X=XMIN*XMAX/(XMAX+RLU(0)*(XMIN-XMAX))
      ELSE
        X=SQRT((XMIN*XMAX)**2/(XMAX**2+RLU(0)*(XMIN**2-XMAX**2)))
      ENDIF
      IF(LST(31).EQ.1) THEN
C...Choose Q**2 according to the distribution
C...hq(Q2) =  a + b/(Q2) + c/(Q2)**2 + d/(Q2)**3. In detail
C...hq=OPTQ2(1)/(Q2MAX-Q2MIN) + 1/ln(Q2MAX/Q2MIN)*OPTQ2(2)/Q2
C...   +Q2MIN*Q2MAX/(Q2MAX-Q2MIN)*OPTQ2(3)/Q2**2
C...   +2*(Q2MIN*Q2MAX)**2/(Q2MAX**2-Q2MIN**2)*OPTQ2(4)/Q2**3
        Q2LOW=MAX(Q2MIN,X*YMIN*S,(W2MIN-PM2)*X/(1.-X))
        Q2UPP=MIN(Q2MAX,X*YMAX*S,(W2MAX-PM2)*X/(1.-X))
        IF(Q2UPP.LT.Q2LOW) GOTO 101
        WHICH=(OPTQ2(1)+OPTQ2(2)+OPTQ2(3)+OPTQ2(4))*RLU(0)
        IF(WHICH.LE.OPTQ2(1)) THEN
          Q2=Q2LOW+RLU(0)*(Q2UPP-Q2LOW)
        ELSEIF(WHICH.LE.(OPTQ2(1)+OPTQ2(2))) THEN
          Q2=Q2LOW*(Q2UPP/Q2LOW)**RLU(0)
        ELSEIF(WHICH.LE.(OPTQ2(1)+OPTQ2(2)+OPTQ2(3))) THEN
          Q2=Q2LOW*Q2UPP/(Q2UPP+RLU(0)*(Q2LOW-Q2UPP))
        ELSE
         Q2=SQRT((Q2LOW*Q2UPP)**2/(Q2UPP**2+RLU(0)*(Q2LOW**2-Q2UPP**2)))
        ENDIF
        Y=Q2/(PARL(21)*X)
        IF(Y.LT.YMIN.OR.Y.GT.YMAX) GOTO 100
      ELSEIF(LST(31).EQ.2) THEN
C...Choose y according to the distribution
C...hy(y) =  a + b/y + c/y**2 + d/y**3. In detail
C...hy=OPTY(1)/(YMAX-YMIN) + 1/ln(YMAX/YMIN)*OPTY(2)/Y
C...   +YMIN*YMAX/(YMAX-YMIN)*OPTY(3)/Y**2
C...   +2*(YMIN*YMAX)**2/(YMAX**2-YMIN**2)*OPTY(4)/Y**3
        YLOW=MAX(YMIN,Q2MIN/(S*X),(W2MIN-PM2)/(S*(1.-X)))
        YUPP=MIN(YMAX,Q2MAX/(S*X),(W2MAX-PM2)/(S*(1.-X)))
        IF(YUPP.LT.YLOW) GOTO 101
        WHICH=(OPTY(1)+OPTY(2)+OPTY(3)+OPTY(4))*RLU(0)
        IF(WHICH.LE.OPTY(1)) THEN
          Y=YLOW+RLU(0)*(YUPP-YLOW)
        ELSEIF(WHICH.LE.(OPTY(1)+OPTY(2))) THEN
          Y=YLOW*(YUPP/YLOW)**RLU(0)
        ELSEIF(WHICH.LE.(OPTY(1)+OPTY(2)+OPTY(3))) THEN
          Y=YLOW*YUPP/(YUPP+RLU(0)*(YUPP-YLOW))
        ELSE
          Y=SQRT((YLOW*YUPP)**2/(YUPP**2+RLU(0)*(YLOW**2-YUPP**2)))
        ENDIF
        Q2=X*Y*PARL(21)
        IF(Q2.LT.Q2MIN.OR.Q2.GT.Q2MAX) GOTO 100
      ELSEIF(LST(31).EQ.3) THEN
C...Choose W**2 according to the distribution
C...hw(W2) =  a + b/(W2) + c/(W2)**2 + d/(W2)**3. In detail
C...hw=OPTW2(1)/(W2MAX-W2MIN) + 1/ln(W2MAX/W2MIN)*OPTW2(2)/W2
C...   +W2MIN*W2MAX/(W2MAX-W2MIN)*OPTW2(3)/W2**2
C...   +2*(W2MIN*W2MAX)**2/(W2MAX**2-W2MIN**2)*OPTW2(4)/W2**3
        W2LOW=MAX(W2MIN,(1.-X)*YMIN*S+PM2,Q2MIN*(1.-X)/X+PM2)
        W2UPP=MIN(W2MAX,(1.-X)*YMAX*S+PM2,Q2MAX*(1.-X)/X+PM2)
        IF(W2UPP.LT.W2LOW) GOTO 101
        WHICH=(OPTW2(1)+OPTW2(2)+OPTW2(3)+OPTW2(4))*RLU(0)
        IF(WHICH.LE.OPTW2(1)) THEN
          W2=W2LOW+RLU(0)*(W2UPP-W2LOW)
        ELSEIF(WHICH.LE.(OPTW2(1)+OPTW2(2))) THEN
          W2=W2LOW*(W2UPP/W2LOW)**RLU(0)
        ELSEIF(WHICH.LE.(OPTW2(1)+OPTW2(2)+OPTW2(3))) THEN
          W2=W2LOW*W2UPP/(W2UPP+RLU(0)*(W2LOW-W2UPP))
        ELSE
         W2=SQRT((W2LOW*W2UPP)**2/(W2UPP**2+RLU(0)*(W2LOW**2-W2UPP**2)))
        ENDIF
        Y=(W2-PM2)/((1.-X)*PARL(21))
        Q2=X*Y*PARL(21)
        IF(Y.LT.YMIN.OR.Y.GT.YMAX) GOTO 100
        IF(Q2.LT.Q2MIN.OR.Q2.GT.Q2MAX) GOTO 100
      ENDIF

  110 IF(LKINEM(LST(2)).NE.0) THEN
        NCUT=NCUT+1
        IF(LST(2).EQ.1) THEN
          IF(NCUT.LE.9999) GOTO 100
          IF(LST(3).GE.1) WRITE(6,1200)
        ENDIF
        LST(21)=2
        RETURN
      ENDIF
      PARI(24)=(1.+(1.-Y)**2)/2.
      PARI(25)=1.-Y
      PARI(26)=(1.-(1.-Y)**2)/2.
      CALL LNSTRF(X,Q2,XPQ)
C...Lepton helicity state, only one contributes in some cases.
      IH=1
      IF(PARL(6).GT.+0.99) IH=2
  200 LST(30)=SIGN(1.,IH-1.5)
      PQH(17,IH)=0.
      PNT(1,IH)=0.
      PNT(2,IH)=0.
CGI-001122
      PNTDU(1,1,IH)=0.
      PNTDU(1,2,IH)=0.
      PNTDU(2,1,IH)=0.
      PNTDU(2,2,IH)=0.
CMM-010306
      PNTDUS(1,1,IH)=0.
      PNTDUS(1,2,IH)=0.
      PNTDUS(2,1,IH)=0.
      PNTDUS(2,2,IH)=0.
      IF(LST(23).EQ.2) THEN
C...Charged current: zero cross-section for one helicity state.
        IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 240
        YQ=PARI(24)-LST(30)*PARI(26)
        YQB=PARI(24)+LST(30)*PARI(26)
        IF(PARI(11).GT.1.E-06) THEN
          IF(K(3,2).LT.0) THEN
            PNT(1,IH)=(1.-PARI(11))*PARI(13)*YQ
            PNT(2,IH)=PARI(11)*PARI(12)*YQ
CGI-001122...Save u-quark weights for p & n target
            PNTDU(1,2,IH)=PARI(13)*YQ
            PNTDU(2,2,IH)=PARI(12)*YQ
CMM-010306...Save anti u-quark weights for p & n target
            PNTDUS(1,2,IH)=PARI(43)*YQ
            PNTDUS(2,2,IH)=PARI(42)*YQ
          ELSE
            PNT(1,IH)=(1.-PARI(11))*PARI(12)*YQ
            PNT(2,IH)=PARI(11)*PARI(13)*YQ
CGI-001122...Save d-quark weights for p & n target
            PNTDU(1,1,IH)=PARI(12)*YQ
            PNTDU(2,1,IH)=PARI(13)*YQ
CMM-010306...Save anti d-quark weights for p & n target
            PNTDUS(1,1,IH)=PARI(42)*YQ
            PNTDUS(2,1,IH)=PARI(43)*YQ
           ENDIF
        ENDIF
        DO 220 I=1,LST(12)
        IF(K(3,2)*QC(I).LT.0) THEN
          PQH(I,IH)=XPQ(I)*YQ
        ELSE
          PQH(I+LST(12),IH)=XPQ(-I)*YQB
        ENDIF
  220   CONTINUE
      ELSE
C...Neutral current: electromagnetic or weak or both with interference.
        GFQ2=Q2/(PMAS(23,1)**2+Q2)*SQRT(2.)*PARL(17)*PMAS(23,1)**2/
     &  (3.1415927*PARL(16))
C...Correction to obtain Q**2 dependent alpha-em, if desired.
        AEMCOR=1.
        IF(LST(18).GE.2) AEMCOR=ULALEM(Q2)/PARL(16)
        II=3-IH
        ZLEP=ZL(IH,ILEP+2*INU)
        DO 230 I=1,MAX(LST(12),LST(13))
        A=(-IG*QC(I)*AEMCOR+IZ*GFQ2*ZLEP*ZQ(IH,I))**2
        B=(-IG*QC(I)*AEMCOR+IZ*GFQ2*ZLEP*ZQ(II,I))**2
C...Save helicity-dependent electroweak quark couplings for later use.
        EWQC(1,IH,I)=A
        EWQC(2,IH,I)=B
        IF(I.GT.LST(12)) GOTO 230
        FYQ=(A+B)*PARI(24)+(A-B)*PARI(26)
        PQH(I,IH)=XPQ(I)*FYQ
        IF(I.LE.2.AND.PARI(11).GT.1.E-06) THEN
          PNT(1,IH)=PNT(1,IH)+(1.-PARI(11))*PARI(11+I)*FYQ
          PNT(2,IH)=PNT(2,IH)+PARI(11)*PARI(14-I)*FYQ
CGI-001122...Save d- & u-quark weights for p & n target
          PNTDU(1,I,IH)=PARI(11+I)*FYQ
          PNTDU(2,I,IH)=PARI(14-I)*FYQ
CMM-010306...Save anti d- & u-quark weights for p & n target
          PNTDUS(1,I,IH)=PARI(41+I)*FYQ
          PNTDUS(2,I,IH)=PARI(44-I)*FYQ
        ENDIF
        PQH(I+LST(12),IH)=XPQ(-I)*((A+B)*PARI(24)-(A-B)*PARI(26))
  230   CONTINUE
      ENDIF
  240 CONTINUE
      DO 300 I=1,LST(12)
  300 PQH(17,IH)=PQH(17,IH)+PQH(I,IH)+PQH(I+LST(12),IH)

      IF(ABS(PARL(6)).LT.0.99.AND.IH.EQ.1) THEN
        IH=2
        GOTO 200
      ENDIF

      FLQ=0.
      FLG=0.
      FLM=0.
      FLT=0.
      IF(LST(11).NE.0.AND.(LST(23).EQ.1.OR.LST(23).EQ.4)
     &.AND.LST(2).NE.-3) THEN
C...Include F_L for photon exchange (unless QCD grid being set up)
        LQCD=MOD(LST(11),10)
        LTM=MOD(LST(11)/10,10)
        LHT=LST(11)/100
C...Include QCD, target mass and/or higher twist contr. to long. str fcn
C...FL from interpolation.
        IF(LQCD.EQ.1.OR.LTM.EQ.1) CALL FLIPOL(FLQ,FLG,FLM)
C...Event simulation: if requested, get FL by event-by-event integration
        IF(LST(2).GT.0.AND.
     &  (LQCD.EQ.2.OR.LTM.EQ.2)) CALL FLINTG(FLQ,FLG,FLM)
        IF(LTM.GE.1.OR.LHT.GE.1) THEN
          F2EM=0.
          DO 301 I=1,LST(12)
  301     F2EM=F2EM+QC(I)**2*(XPQ(I)+XPQ(-I))
          IF(LTM.GE.1) FLM=FLM-2.*X**2*PSAVE(3,2,5)**2/Q2*F2EM
          IF(LHT.GE.1) FLT=8.*PARL(19)/Q2*F2EM
        ENDIF
        DO 305 IH=1,2
        PQH17=PQH(17,IH)
C...Note factor 2 at the end, since PQH(IH,17) contains overall factor 2
        PQH(17,IH)=PQH(17,IH)-Y**2*(FLQ+FLG+FLM+FLT)
        DO 305 I=1,16
  305   PQH(I,IH)=PQH(I,IH)*PQH(17,IH)/PQH17
      ENDIF

      DO 310 I=1,17
  310 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)

C...Relative contribution from longitudinal str. fcn. and higher twist.
       RFLQ=-Y**2*FLQ/MAX(PQ(17),1.E-33)
       RFLG=-Y**2*FLG/MAX(PQ(17),1.E-33)
       RFLM=-Y**2*FLM/MAX(PQ(17),1.E-33)
       RFLT=-Y**2*FLT/MAX(PQ(17),1.E-33)

C...Common factor for matrix elements.
      IF(LST(31).EQ.1) THEN
        IF(LST(23).EQ.2) THEN
          COMFAC=1./X/(1.+Q2/PMAS(24,1)**2)**2
        ELSE
          COMFAC=1./X/Q2**2
        ENDIF
      ELSEIF(LST(31).EQ.2) THEN
        IF(LST(23).EQ.2) THEN
          COMFAC=1./(1.+Q2/PMAS(24,1)**2)**2*PARL(21)
        ELSE
          COMFAC=1./Q2**2*PARL(21)
        ENDIF
      ELSEIF(LST(31).EQ.3) THEN
        IF(LST(23).EQ.2) THEN
          COMFAC=1./X/(1.+Q2/PMAS(24,1)**2)**2  * X/(1.-X)
        ELSE
          COMFAC=1./X/Q2**2 * X/(1.-X)
        ENDIF
      ENDIF
C-check: Move change of COMFAC to below??
C...Prepare for Q2 weighting.
C     WEIGHT=1/Q2**2
      WEIGHT=1.D0
      COMFAC=COMFAC/WEIGHT
      IF(LST(2).LE.-2) RETURN
      HX=OPTX(1)/(XMAX-XMIN) + 1./ALOG(XMAX/XMIN)*OPTX(2)/X
     &+XMIN*XMAX/(XMAX-XMIN)*OPTX(3)/X**2
     &+2*(XMIN*XMAX)**2/(XMAX**2-XMIN**2)*OPTX(4)/X**3
      XFACT=OPTX(1)+OPTX(2)+OPTX(3)+OPTX(4)
      IF(LST(31).EQ.1) THEN
        HQ2=OPTQ2(1)/(Q2UPP-Q2LOW)
     &  +1./ALOG(Q2UPP/Q2LOW)*OPTQ2(2)/Q2
     &  +Q2LOW*Q2UPP/(Q2UPP-Q2LOW)*OPTQ2(3)/Q2**2
     &  +2*(Q2LOW*Q2UPP)**2/(Q2UPP**2-Q2LOW**2)*OPTQ2(4)/Q2**3
        Q2FACT=OPTQ2(1)+OPTQ2(2)+OPTQ2(3)+OPTQ2(4)
        COMFAC=COMFAC*XFACT*Q2FACT/HX/HQ2
      ELSEIF(LST(31).EQ.2) THEN
        HY=OPTY(1)/(YUPP-YLOW)+1./ALOG(YUPP/YLOW)*OPTY(2)/Y
     &  +YLOW*YUPP/(YUPP-YLOW)*OPTY(3)/Y**2
     &  +2*(YLOW*YUPP)**2/(YUPP**2-YLOW**2)*OPTY(4)/Y**3
        YFACT=OPTY(1)+OPTY(2)+OPTY(3)+OPTY(4)
        COMFAC=COMFAC*XFACT*YFACT/HX/HY
      ELSEIF(LST(31).EQ.3) THEN
        HW2=OPTW2(1)/(W2UPP-W2LOW)
     &  +1./ALOG(W2UPP/W2LOW)*OPTW2(2)/W2
     &  +W2LOW*W2UPP/(W2UPP-W2LOW)*OPTW2(3)/W2**2
     &  +2*(W2LOW*W2UPP)**2/(W2UPP**2-W2LOW**2)*OPTW2(4)/W2**3
        W2FACT=OPTW2(1)+OPTW2(2)+OPTW2(3)+OPTW2(4)
        COMFAC=COMFAC*XFACT*W2FACT/HX/HW2
      ENDIF
      IF(LST(2).LE.0) RETURN

C-check: Move change of COMFAC to here?
      SIGL=(1.-PARL(6))/2.*PQH(17,1)
      SIGR=(1.+PARL(6))/2.*PQH(17,2)
      SIGMA=SIGL+SIGR
      IF(LST(2).EQ.1) THEN
C...When chosing (x,y), reject according to maximum of "cross-section",
C...update cross-section estimate.
        DARI27=DARI27+DBLE(SIGMA)*DBLE(COMFAC)*WEIGHT
        PARI(27)=DARI27
        VIOL=SIGMA*COMFAC/PARI(LST(23))
        IF(VIOL.GT.PARI(32)) THEN
          PARI(32)=VIOL
          IF(PARI(32).GT.1.) THEN
            PARI(LST(23))=PARI(LST(23))*PARI(32)
            IF(LST(3).GE.1) WRITE(6,1300) PARI(32),INT(PARI(30)+1),
     &      PARI(LST(23)),X,Y,Q2,W2
            PARI(32)=1.
          ENDIF
        ENDIF
        IF(VIOL.LT.RLU(0)) GOTO 100
        PARL(24)=PARI(31)*DARI27/DARI28
      ENDIF

      IF(ABS(PARL(6)).LT.0.99) THEN
C...Choose helicity of incoming lepton.
        IH=1
        IF(RLU(0)*SIGMA.GT.SIGL) IH=2
      ENDIF
      LST(30)=SIGN(1.,IH-1.5)
CGI-001122...Save parton density weights for chosen helicity
      DO 510 I=1,17
  510 PQ(I)=PQH(I,IH)

C...Choose target nucleon, proton or neutron.
      LST(22)=1
      K(2,2)=2212
      IF(PARI(11).GT.1.E-06) THEN
        IF(RLU(0).LT.(PARI(11)*(PQH(17,IH)-PNT(1,IH)-PNT(2,IH))+
     &  PNT(2,IH))/PQH(17,IH)) THEN
          LST(22)=2
          K(2,2)=2112
        ENDIF
CGI-001122...Save parton density weights for chosen target nucleon
CMM-010306...and take also care of the anti u- and d-quarks.
        PQ(17)=PQ(17)-PQ(1)-PQ(2)-PQ(1+LST(12))-PQ(2+LST(12))
        PQ(1)=PNTDU(LST(22),1,IH)
        PQ(2)=PNTDU(LST(22),2,IH)
        PQ(1+LST(12))=PNTDUS(LST(22),1,IH)
        PQ(2+LST(12))=PNTDUS(LST(22),2,IH)      
        PQ(17)=PQ(17)+PQ(1)+PQ(2)+PQ(1+LST(12))+PQ(2+LST(12))
      ENDIF

      RETURN
 1200 FORMAT(' Warning: LEPTOX is looping, cannot find allowed ',
     &'phase space point due to cuts,',/,
     &10X,'check, in particular, CUT(11) to CUT(14)')
 1300 FORMAT(' Warning: maximum violated by a factor ',F7.3,
     &' in event ',I7,/,' maximum increased by this factor to ',E12.3,
     &/,' Point of violation: x, y, Q**2, W**2 = ',4G10.3)
      END

C **********************************************************************

      FUNCTION LKINEM(L)
      
      IMPLICIT NONE
      
C...Calculate kinematical variables and reject (optionally) if outside
C...required limits.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DBETA
      REAL STHETA,SPHI,PB,PHIR
      SAVE /LBOOST/


      INTEGER LKINEM,L,J
      REAL THETAL,PLU
      REAL ULMASS
      DOUBLE PRECISION DE,DPZ,DPT

      LKINEM=1
      IF(L.EQ.-3) THEN
C...x,W known from LWEITS, no cuts applied.
        U=(W2-P(2,5)**2)/(2.*P(2,5)*(1.-X))
        Q2=2.*P(2,5)*U*X
        Y=Q2/(PARL(21)*X)
        GOTO 200
      ENDIF
C...x,y given.
      PARL(22)=Y*PARL(21)
      Q2=X*PARL(22)
      U=PARL(22)/(2.*P(2,5))
      W2=PARL(22)*(1.-X)+P(2,5)**2
      P(4,5)=ULMASS(K(4,2))
      IF(P(4,5)/SQRT(PARL(21)).LT.0.001) THEN
C...Simpler formulae for effectively massless scattered lepton.
        DE=DBLE(P(1,4))*(1.-DBLE(Y))+DBLE(X)*DBLE(Y)*DBLE(ABS(P(2,3)))
        DPZ=DE-DBLE(X)*DBLE(Y)*(DBLE(P(2,4))+DBLE(ABS(P(2,3))))
      ELSE
C...Formulae for massive scattered lepton.
        DE=DBLE(P(1,4))+(DBLE(ABS(P(2,3)))*(DBLE(Q2)+DBLE(P(1,5))**2+
     &  DBLE(P(4,5))**2)/(2.D0*DBLE(P(1,4)))-DBLE(PARL(22))/2.D0)/
     &  (DBLE(P(2,4))+DBLE(ABS(P(2,3))))
        DPZ=DBLE(P(1,4))-(DBLE(P(2,4))*(DBLE(Q2)+DBLE(P(1,5))**2+
     &  DBLE(P(4,5))**2)/(2.D0*DBLE(P(1,4)))+DBLE(PARL(22))/2.D0)/
     &  (DBLE(P(2,4))+DBLE(ABS(P(2,3))))
      ENDIF
      DPT=DE**2-DPZ**2-DBLE(P(4,5))**2
      IF(DPT.LT.0.D0) RETURN
      DPT=SQRT(DPT)
      P(4,1)=DPT
      P(4,2)=0.
      P(4,3)=DPZ
      P(4,4)=DE
      P(3,1)=-DPT
      P(3,2)=0.
      P(3,3)=DBLE(P(1,3))-DPZ
      P(3,4)=DBLE(P(1,4))-DE
      P(3,5)=-SQRT(Q2)
      K(3,3)=1
      K(4,3)=1
      N=4
      IF(L.EQ.3) GOTO 200

      IF(X.LT.XMIN.OR.X.GT.XMAX) RETURN
      IF(Y.LT.YMIN.OR.Y.GT.YMAX) RETURN
      IF(Q2.LT.Q2MIN.OR.Q2.GT.Q2MAX) RETURN
      IF(W2.LT.W2MIN.OR.W2.GT.W2MAX) RETURN
C-check: CUT(9),CUT(10) --> UMIN,UMAX needs change in /LINTRL/ --> next update 
      IF(U.LT.CUT(9).OR.U.GT.CUT(10)) RETURN
      IF(LST(17).EQ.0) THEN
        IF(P(4,4).LT.CUT(11).OR.P(4,4).GT.CUT(12))  RETURN
        THETAL=PLU(4,13)
C       THETAL=ACOS((P(1,1)*P(4,1)+P(1,2)*P(4,2)+P(1,3)*P(4,3))
C    &  /SQRT(P(1,1)**2+P(1,2)**2+P(1,3)**2)/
C    &  SQRT(P(4,1)**2+P(4,2)**2+P(4,3)**2))
      ELSE
C...No cuts on energy, angle for initialisation of varying energy mode
        IF(LST(32).NE.0) GOTO 200
C...Transform scattered lepton back to lab system to make cut
C...in energy and angle (defined as space angle to incoming lepton).
        DO 110 J=1,5
        K(6,J)=K(4,J)
  110   P(6,J)=P(4,J)
        CALL LUDBRB(6,6,STHETA(1),SPHI(1),0.D0,0.D0,0.D0)
        CALL LUDBRB(6,6,0.,0.,DBETA(1,1),DBETA(1,2),DBETA(1,3))
        IF(P(6,4).LT.CUT(11).OR.P(6,4).GT.CUT(12))  RETURN
        THETAL=ACOS((PSAVE(3,1,1)*P(6,1)+PSAVE(3,1,2)*P(6,2)+
     &  PSAVE(3,1,3)*P(6,3))
     &  /SQRT(PSAVE(3,1,1)**2+PSAVE(3,1,2)**2+PSAVE(3,1,3)**2)/
     &  SQRT(P(6,1)**2+P(6,2)**2+P(6,3)**2))
      ENDIF
      IF(THETAL.LT.CUT(13).OR.THETAL.GT.CUT(14))  RETURN
  200 LKINEM=0
      RETURN
      END
C **********************************************************************

      SUBROUTINE LQCDPR(QG,QQB)

      IMPLICIT NONE

C...Probabilities for hard QCD events, qg or qqb, from integration of
C...QCD matrix elements event-by event or interpolation on x-W grid.
      
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/



      common/debug1/iw,ix,ih

      
      DIMENSION PQSAVE(17)
      EXTERNAL DSIGMA,DSIGM2
      INTEGER NOUT,NABOVE,NWARN,IH,IX,IW,IP
      INTEGER I,IYCUT
      REAL EPS,XPMAX,RQ,PQSAVE,RTOT,RQQB,RQG,ULALPS
      REAL XPMIN 
      REAL QG,QQB,W,XP,WD,XD,X1P,X2P,QGIP,PQ17,QTOT,QQBIP,QGQQB
      REAL  P27MAX,YCMIN,YCMAX
      DATA NOUT,NABOVE/2*0/,NWARN/10/

      LOGICAL ZOOM

C...Get ycut from grid
      IF(LST(19).GE.0.OR.LST(19).EQ.-10) THEN
C...
C...qg and qqb event probabilities from interpolation on grid
      QG=0.
      QQB=0.
C...QCD weight zero for x->1 above grid and W small below grid
      IF(X.GT.XX(NXX).AND.X.GT.0.999) RETURN
      IF(LST(19).LT.10.AND.SQRT(W2).LT.WW(1).AND.WW(1).LT.6.) RETURN

      XP=X
C...Local variable W is W or y
      W=SQRT(W2)
      IF(LST(19).GE.10.OR.LST(19).EQ.-10) W=Y
      IF(X.LT.XX(1).OR.X.GT.XX(NXX).OR.
     &W.LT.WW(1).OR.W.GT.WW(NWW)) THEN
C...x and/or W/y outside limits of grid, write warning NWARN first times
        NOUT=NOUT+1
        IF(LST(3).GE.1.AND.NOUT.LE.NWARN)
     &  WRITE(6,1000) X,W,INT(PARI(29)),NWARN
        IF(X.LT.XX(1)) XP=XX(1)
        IF(X.GT.XX(NXX)) XP=XX(NXX)
        IF(W.LT.WW(1)) W=WW(1)
        IF(W.GT.WW(NWW)) W=WW(NWW)
      ENDIF

      IH=1
      IF(LST(30).EQ.1) IH=2
*Pepsi>>      
      IF(LST(40).NE.0) IH=1
*Pepsi<<            
      IX=0
  100 IX=IX+1
      IF(XP.GT.XX(IX+1)) GOTO 100
      IW=0
  200 IW=IW+1
      IF(W.GT.WW(IW+1)) GOTO 200
      WD=(W-WW(IW))/(WW(IW+1)-WW(IW))
      XD=(XP-XX(IX))/(XX(IX+1)-XX(IX))

      DO 500 IP=1,NP
      X1P=(PQG(IX+1,IW,IP)-PQG(IX,IW,IP))*XD+PQG(IX,IW,IP)
      X2P=(PQG(IX+1,IW+1,IP)-PQG(IX,IW+1,IP))*XD+PQG(IX,IW+1,IP)
      QGIP=(X2P-X1P)*WD+X1P
      IF(NP.EQ.1) THEN
        QG=QGIP

        PARI(15)=MAX(QGMAX(IX,IW,IH),QGMAX(IX+1,IW+1,IH),
     &  QGMAX(IX+1,IW,IH),QGMAX(IX,IW+1,IH))
      ELSE
        QG=QG+PARI(23+IP)*QGIP
        PARI(14+IP)=MAX(QGMAX(IX,IW,IP),QGMAX(IX+1,IW+1,IP),
     &  QGMAX(IX+1,IW,IP),QGMAX(IX,IW+1,IP))
      ENDIF
      IF(IP.EQ.3) GOTO 500
      X1P=(PQQB(IX+1,IW,IP)-PQQB(IX,IW,IP))*XD+PQQB(IX,IW,IP)
      X2P=(PQQB(IX+1,IW+1,IP)-PQQB(IX,IW+1,IP))*XD+PQQB(IX,IW+1,IP)
      QQBIP=(X2P-X1P)*WD+X1P
      IF(NP.EQ.1) THEN
        QQB=QQBIP
        PARI(18)=MAX(QQBMAX(IX,IW,IH),QQBMAX(IX+1,IW+1,IH),
     &  QQBMAX(IX+1,IW,IH),QQBMAX(IX,IW+1,IH))
      ELSE
        QQB=QQB+PARI(23+IP)*QQBIP
        PARI(17+IP)=MAX(QQBMAX(IX,IW,IP),QQBMAX(IX+1,IW+1,IP),
     &  QQBMAX(IX+1,IW,IP),QQBMAX(IX,IW+1,IP))
      ENDIF
  500 CONTINUE

      IF(NP.NE.1) THEN
C...Get total x-section from interpolation to be used for normalization.
        X1P=(XTOT(IX+1,IW)-XTOT(IX,IW))*XD+XTOT(IX,IW)
        X2P=(XTOT(IX+1,IW+1)-XTOT(IX,IW+1))*XD+XTOT(IX,IW+1)
        PQ17=(X2P-X1P)*WD+X1P
        QG=QG/PQ17
        QQB=QQB/PQ17
      ENDIF

C..Interpolate in the grid
      X1P=(YCUT(IX+1,IW)-YCUT(IX,IW))*XD+YCUT(IX,IW)
      X2P=(YCUT(IX+1,IW+1)-YCUT(IX,IW+1))*XD+YCUT(IX,IW+1)
      PARL(27)=(X2P-X1P)*WD+X1P
C...Include alpha-strong in weight.
      QG=QG*PARL(25)
      QQB=QQB*PARL(25)
C...Get value of y-cut,
      IF(LST(19).GE.0) THEN
        IF(LST(33).EQ.-91) THEN
C...Include 3-jet cross section in denominator
          QTOT=1.+QG+QQB
          QG =QG/QTOT
          QQB=QQB/QTOT
        ENDIF
        IF(QG+QQB.GT.1) THEN
C...Sum of QCD event probabilities larger than unity, rescale to unity
C...and print warning for first NWARN cases.
          NABOVE=NABOVE+1
          IF(LST(3).GE.1.AND.NABOVE.LE.NWARN)
     &    WRITE(6,1100) QG,QQB,X,W,INT(PARI(29)),NWARN
          QGQQB=QG+QQB
          QG=QG/QGQQB
          QQB=QQB/QGQQB
        ENDIF
      ELSE
        IF(MAX(YCUT(IX,IW),YCUT(IX+1,IW+1),
     &         YCUT(IX+1,IW),YCUT(IX,IW+1))-
     &     MIN(YCUT(IX,IW),YCUT(IX+1,IW+1),
     &         YCUT(IX+1,IW),YCUT(IX,IW+1)).EQ.0.0) THEN
          RETURN
        ELSE
C...Get the minimum from the grid
          PARL(27)=MIN(YCUT(IX,IW),YCUT(IX+1,IW+1),
     &                 YCUT(IX+1,IW),YCUT(IX,IW+1))
        ENDIF
      ENDIF

C...Grid
      ENDIF
      
C...Calculate probabilities directly or refine value from grid
      IF(LST(19).LE.0) THEN
      
C...qg and qqbar event probabilities (and max values for simulation)
C...obtained by integrating QCD matrix elements for each event.
C     LST2=LST(2)
C     LST(2)=-3
C     NP=1
      LST(32)=1

      DO 1 I=1,17
    1 PQSAVE(I)=PQ(I)

      PARL(25)=ULALPS(Q2)
      PARI(20)=PQ(17)
      IF(LST(19).GT.-10) THEN
        IF(LST(20).LE.1) THEN
          PARL(27)=MAX(PARL(9)**2/W2,PARL(8))
          P27MAX=1.0
        ELSEIF(LST(20).EQ.2) THEN
          PARL(27)=MAX(PARL(9)**2/Q2,PARL(8))
          P27MAX=W2/Q2
        ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
          PARL(27)=PARL(8)
          P27MAX=0.5
        ELSEIF(LST(20).EQ.6) THEN
          PARL(27)=PARL(9)
          P27MAX=W2
        ENDIF
      ELSE
        IF(LST(20).LE.1) THEN
          P27MAX=1.0
        ELSEIF(LST(20).EQ.2) THEN
          P27MAX=W2/Q2
        ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
          P27MAX=0.5
        ELSEIF(LST(20).EQ.6) THEN
          P27MAX=W2
        ENDIF
      ENDIF

      ZOOM=.FALSE.
      IYCUT=0
      YCMIN=PARL(27)
      YCMAX=PARL(27)
   10 IYCUT=IYCUT+1
      RQG=0.
      RQQB=0.
CAE.Scheme for ME cutoff: W2, Q2, mixed
      IF(LST(20).LE.1) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*(1.D0-DBLE(X))*DBLE(PARL(27)))
        XPMAX=DBLE(X)/(DBLE(X)+(1.D0-DBLE(X))*DBLE(PARL(27)))
      ELSEIF(LST(20).EQ.2) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*DBLE(X)*DBLE(PARL(27)))
        XPMAX=1.D0/(1.D0+DBLE(PARL(27)))
      ELSEIF(LST(20).EQ.3.OR.LST(20).EQ.4) THEN
        XPMIN=X
        XPMAX=1./(1.+PARL(9))
      ELSEIF(LST(20).EQ.5) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(9))
      ELSEIF(LST(20).EQ.6) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(27))
      ELSE
        WRITE(6,*) 'LQCDPR: No such jet scheme!'
      ENDIF
CAE
      IF(XPMIN.LT.X.OR.XPMIN.GT.1.) GOTO 40
      IF(XPMIN.GE.XPMAX) GOTO 40

      PARI(15)=0.
      PARI(16)=0.
      PARI(18)=0.
      PARI(19)=0.
C...QCD-Compton -> qg-event
      LST(24)=2
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RQG)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RQG)
C...QCD-fusion  -> qq-event
      LST(24)=3
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RQQB)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RQQB)
C...q-event
      RQ=1.-RQG-RQQB
CAE      WRITE(6,*) IYCUT,RQ,PARL(27),YCMIN,YCMAX
      IF(.NOT.ZOOM) THEN
CAE.First find interval so that RQ>0
        IF(RQ.LT.0.AND.IYCUT.LT.10) THEN
          PARL(27)=MIN(1.1*EXP(-2.0*RQ)*PARL(27),P27MAX)
          YCMIN=YCMAX
          YCMAX=PARL(27)
        ELSEIF(RQ.LT.0.AND.IYCUT.GE.10) THEN
C...Terminate procedure after some iterations
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/RTOT
          RQQB=RQQB/RTOT
          RQ=1.-RQG-RQQB
C          IF(LST(3).GE.1) THEN
C            WRITE(6,*) 'Warning! sigma>tot for x,q2,cut=',X,Q2,PARL(27)
C            WRITE(6,*) 'Weights set to=',RQ,RQG,RQQB
C          ENDIF
C...Break loop
          GOTO 40
        ELSEIF(IYCUT.GE.2.AND.RQ.GT.PARL(13)) THEN
C...If RQ>PARL(13), then ycut was increased to much
          ZOOM=.TRUE.
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSE
C...correct ycut found
          GOTO 40
        ENDIF
      ELSE
C...Zoom in on ycut so that 0<RQ<PARL(13)
        IF(RQ.LT.0.AND.IYCUT.LT.40) THEN
          YCMIN=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSEIF(RQ.GT.PARL(13).AND.IYCUT.LT.40) THEN
          YCMAX=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
C...Catch infinite loop
        ELSEIF(IYCUT.GE.40) THEN
          IF(LST(3).GE.1) THEN
            WRITE(6,*) 'LQCDPR: Warning, PARL(27) not found.'
          ENDIF
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/MAX(1.0,RTOT)
          RQQB=RQQB/MAX(1.0,RTOT)
          RQ=1.-RQG-RQQB
C...Break loop
          GOTO 40
        ELSE
C...ycut found, break loop
          GOTO 40
        ENDIF
      ENDIF
C...Loop until correct weights found
      GOTO 10

   40 CONTINUE
CAE
      IF(LST(33).EQ.-91) THEN
C...Include 3-jet cross section in denominator
        QTOT=1.+RQG+RQQB
        RQG =RQG/QTOT
        RQQB=RQQB/QTOT
        RQ=1.-RQG-RQQB
      ENDIF

C     LST(2)=LST2
      LST(32)=0
      DO 90 I=1,17
   90 PQ(I)=PQSAVE(I)
      QG=RQG
      QQB=RQQB

C...Refine      
      ENDIF
      
 1000 FORMAT(' Warning: x=',F7.4,' or W/y=',F10.4,' outside QCD grid',
     &' in event no.',I8,/,10X,
     &'weight on limit of grid used. Only first',I5,' warnings printed')
 1100 FORMAT(' Warning: Sum of QCD probabilities larger than unity ',
     &' QG, QQB =',2F8.4,/10X,'occurs at x, W/y =',2F10.4,
     &' in event no.',I8,/,10X,
     &'Weights rescaled to unit sum. Only first',I5,' warnings printed')
      RETURN
      END

C **********************************************************************

      SUBROUTINE LQEV

      IMPLICIT NONE

C...Generate an ordinary 2-jet event, q-event.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/

      
      INTEGER IFL,IFLR,IFLRAR,IFLRO,NREMH,K2,IFLAR
      REAL W,AMIFL,XT,AMK2,AMIFLR,PT,PHI,PT2,TM2K2,EK2,PZK2,EPZ,WT,
     +TMIFLR,EIFL,EIFLR,THER,THEK2
      REAL ULMASS,ULANGL
      INTEGER KFIFLR,LUCOMP,KFK2

      LST(24)=1
      W=SQRT(W2)

C...Choose flavour of scattered quark and target remnant.
  200 CALL LFLAV(IFL,IFLR)
      IF(LST(21).NE.0) GOTO 200

      GOTO 210
C...Entry used for Ariadne
      ENTRY LQEVAR(IFLAR,IFLRAR)
      IFL=IFLAR
      IFLR=IFLRAR
      LST(24)=1
      W=SQRT(W2)

  210 CONTINUE
      MSTJ(93)=1
      AMIFL=ULMASS(IFL)
      MSTJ(93)=1
      AMIFLR=ULMASS(IFLR)
      IF(LST(14).EQ.0.OR.IFLR.GT.10
     &.OR.(LST(8).GE.2.AND.MOD(LST(8),10).NE.9)) THEN
C...Check if energy in jet system is enough for fragmentation.
        IF(W.LT.AMIFL+AMIFLR+PARJ(32)) GOTO 200
        CALL LU2ENT(MSTU(1),IFL,IFLR,W)
        K(MSTU(1)+1,3)=2
      ELSE
C...Target remnant is not a simple diquark, special treatment needed.
        IF(W.LT.AMIFL+AMIFLR+0.9+PARJ(32)) GOTO 200
        IFLRO=IFLR
        NREMH=0
  300   NREMH=NREMH+1
        IF(NREMH.GT.100) GOTO 999
C...Give balancing pt to IFLQ and IFLQQ.
        CALL LPRIKT(PARL(14),PT,PHI)
        CALL LREMH(IFLRO,PT,IFLR,K2,XT)
        MSTJ(93)=1
        AMIFLR=ULMASS(IFLR)
CJR--
        KFIFLR=LUCOMP(IFLR)
        IF (KFIFLR.EQ.90) THEN
          AMIFLR=AMIFLR-2.*PARL(20)
        ELSEIF (1.LE.KFIFLR .AND. KFIFLR.LE.6) THEN
          AMIFLR=AMIFLR-PARL(20)
        ENDIF
        MSTJ(93)=1
        AMK2=ULMASS(K2)
        KFK2=LUCOMP(K2)
        IF (KFK2.EQ.90) THEN
          AMK2=AMK2-2.*PARL(20)
        ELSEIF (1.LE.KFK2 .AND. KFK2.LE.6) THEN
          AMK2=AMK2-PARL(20)
        ENDIF
CJR--
        PT2=PT**2
        TM2K2=AMK2**2+PT2
        EK2=.5*(XT*W+TM2K2/XT/W)
        PZK2=-.5*(XT*W-TM2K2/XT/W)
        EPZ=W-TM2K2/XT/W
        WT=(1.-XT)*W*EPZ-PT2
C...Check if energy in jet system is enough for fragmentation.
        IF(WT.LT.(AMIFL+AMIFLR+PARJ(32))**2) GOTO 300
        WT=SQRT(WT+PT2)
        TMIFLR=AMIFLR**2+PT2
        EIFL=.5*(WT+(AMIFL**2-TMIFLR)/WT)
        EIFLR=.5*(WT+(-AMIFL**2+TMIFLR)/WT)
        THER=ULANGL(-SQRT(EIFLR**2-TMIFLR),PT)
C...Form jet system.
        CALL LU1ENT(-MSTU(1),IFL,EIFL,0.,0.)
        CALL LU1ENT(MSTU(1)+1,IFLR,EIFLR,THER,PHI)
        CALL LUDBRB(MSTU(1),0,0.,0.,0.D0,0.D0,
     &  (DBLE(EPZ)-(1.D0-DBLE(XT))*DBLE(W))/
     &  (DBLE(EPZ)+(1.D0-DBLE(XT))*DBLE(W)))
        THEK2=ULANGL(PZK2,PT)
C...Add formed "target" particle.
        MSTU(10)=1
        P(MSTU(1)+2,5)=AMK2
        CALL LU1ENT(MSTU(1)+2,K2,EK2,THEK2,PHI+3.1415927)
        MSTU(10)=2
        K(MSTU(1)+1,3)=2
        K(MSTU(1)+2,3)=2
CIC...Target remnants required to go backwards in hadronic cms
        IF(P(MSTU(1)+1,3).GT.0..OR.P(MSTU(1)+2,3).GT.0.) GOTO 300
      ENDIF
      
CAE...Set reasonable values to the ME variables xp,zq and phi
      PARL(28)=1.0
      PARL(29)=1.0
      PARL(30)=0.0

      LST(21)=0
      RETURN

  999 LST(21)=3
      RETURN
      END

C **********************************************************************

      SUBROUTINE LQGEV

      IMPLICIT NONE

C...Generate quark-gluon jet event, choose xp and zp according to QCD
C...matrix elements and apply cuts for soft and collinear gluons.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/

      
      INTEGER J1,J2,J3,J4,IFAIL,IFL,IFLR,LQMCUT,IFLRO,K2,NREMH
      REAL W,XP,ZP,AMIFL,XT,AMK2,PT,PHI,PT2,TM2K2,TMIFLR,EPZ,AMIFLR
      REAL ULMASS
      INTEGER LUCOMP,KFK2,KFIFLR

      LST(24)=2
      W=SQRT(W2)
      J1=MSTU(1)
      J2=MSTU(1)+1
      J3=MSTU(1)+2
      J4=MSTU(1)+3
      CALL LXP(XP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999

C...Choose flavour of scattered quark and target remnant.
  200 CALL LFLAV(IFL,IFLR)
      IF(LST(21).NE.0) RETURN
      CALL LZP(XP,ZP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999
      MSTJ(93)=1
      AMIFL=ULMASS(IFL)
      MSTJ(93)=1
      AMIFLR=ULMASS(IFLR)

      IF(LST(14).EQ.0.OR.IFLR.GT.10
     &.OR.(LST(8).GE.2.AND.MOD(LST(8),10).NE.9)) THEN
        IF(W.LT.AMIFL+AMIFLR+PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL,0.,AMIFLR).NE.0) GOTO 999
        CALL LU3ENT(J1,IFL,21,IFLR,W,PARI(21),PARI(23))
        K(MSTU(1)+2,3)=2
        CALL LUROBO(ACOS(-P(J3,3)/SQRT(P(J3,3)**2+P(J3,1)**2)),
     &  0.,0.,0.,0.)
      ELSE
C...Target remnant is not a simple diquark, special treatment needed.
        IF(W.LT.AMIFL+AMIFLR+1.+PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL,0.,1.).NE.0) GOTO 999
        IFLRO=IFLR
        NREMH=0
  300   NREMH=NREMH+1
        IF(NREMH.GT.100) GOTO 999
        CALL LPRIKT(PARL(14),PT,PHI)
        CALL LREMH(IFLRO,PT,IFLR,K2,XT)
        MSTJ(93)=1
        AMIFLR=ULMASS(IFLR)
CJR--
        KFIFLR=LUCOMP(IFLR)
        IF (KFIFLR.EQ.90) THEN
          AMIFLR=AMIFLR-2.*PARL(20)
        ELSEIF (1.LE.KFIFLR .AND. KFIFLR.LE.6) THEN
          AMIFLR=AMIFLR-PARL(20)
        ENDIF
        MSTJ(93)=1
        AMK2=ULMASS(K2)
        KFK2=LUCOMP(K2)
        IF (KFK2.EQ.90) THEN
          AMK2=AMK2-2.*PARL(20)
        ELSEIF (1.LE.KFK2 .AND. KFK2.LE.6) THEN
          AMK2=AMK2-PARL(20)
        ENDIF
CJR--
        P(J1,5)=AMIFL
        P(J2,5)=0.
        PT2=PT**2
        TM2K2=AMK2**2+PT2
        TMIFLR=AMIFLR**2+PT2
        P(J3,5)=SQRT(TM2K2/XT+TMIFLR/(1.-XT))
        IF(LQMCUT(XP,ZP,AMIFL,0.,P(J3,5)).NE.0) GOTO 300
        MSTU(10)=1
        CALL LU3ENT(J1,IFL,21,IFLR,W,PARI(21),PARI(23))
        K(MSTU(1)+2,3)=2
        MSTU(10)=2
        CALL LUROBO(ACOS(-P(J3,3)/SQRT(P(J3,3)**2+P(J3,1)**2)),
     &  0.,0.,0.,0.)
        EPZ=P(J3,4)-P(J3,3)
        P(J3,1)=PT*COS(PHI)
        P(J3,2)=PT*SIN(PHI)
        P(J3,3)=-0.5*((1.-XT)*EPZ-TMIFLR/(1.-XT)/EPZ)
        P(J3,4)= 0.5*((1.-XT)*EPZ+TMIFLR/(1.-XT)/EPZ)
        P(J3,5)=AMIFLR
        P(J4,1)=-P(J3,1)
        P(J4,2)=-P(J3,2)
        P(J4,3)=-0.5*(XT*EPZ-TM2K2/XT/EPZ)
        P(J4,4)= 0.5*(XT*EPZ+TM2K2/XT/EPZ)
        P(J4,5)=AMK2
        K(J4,1)=1
        K(J4,2)=K2
        K(J4,3)=2
        K(J4,4)=0
        K(J4,5)=0
        N=J4
        IF((P(J3,4)+P(J2,4)/2.)**2-(P(J3,1)+P(J2,1)/2.)**2-P(J3,2)**2
     &  -(P(J3,3)+P(J2,3)/2.)**2.LT.(AMIFLR+2.5*PARJ(32))**2) GOTO 300
      ENDIF
      CALL LAZIMU(XP,ZP)
      LST(21)=0
      RETURN

  999 LST(21)=4
      RETURN
      END

C **********************************************************************

      SUBROUTINE LQQBEV

      IMPLICIT NONE

C...Generate boson-gluon fusion event, choose xp and zp according to
C...QCD matrix elements and apply cuts for softness and collinearness.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/

      
      INTEGER J1,J2,J3,J4,IFAIL,LQMCUT
      REAL W,XP,ZP,XT,PT,PHI,PT2,EPZ,
     +AMIFL1,AMIFL3,AMR1,AMR2,TM2R1,TM2R2
      REAL ULMASS,RLU,PLU
      INTEGER IFL1,IFLR2,IFLR1,IFL3,IFL1A,IFL3A,NREMH,IR1,IR2,
     +     I,J,IFL1S
      INTEGER LUCOMP,KFIFL1,KFIFL2

      LST(24)=3
      W=SQRT(W2)
      J1=MSTU(1)
      J2=MSTU(1)+1
      J3=MSTU(1)+2
      J4=MSTU(1)+3
       
      CALL LXP(XP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999

C...Choose flavour of produced quark-antiquark pair.
  200 CALL LFLAV(IFL1,IFL3)

      IF(LST(21).NE.0) RETURN
      IF(IFL1.LT.0) THEN
C...Put quark in first position
        IFL1S=IFL1
        IFL1=IFL3
        IFL3=IFL1S
      ENDIF
      CALL LZP(XP,ZP,IFAIL)
      IF(IFAIL.NE.0) GOTO 999
      IFL1A=IABS(IFL1)
      IFL3A=IABS(IFL3)
      MSTJ(93)=1
      AMIFL1=ULMASS(IFL1)
      MSTJ(93)=1
      AMIFL3=ULMASS(IFL3)


      IF(LST(14).EQ.0.OR.(LST(8).GE.2.AND.MOD(LST(8),10).NE.9)) THEN
C...If baryon production from target remnant is neglected the
C...target remnant is approximated by a gluon.
        IF(W.LT.AMIFL1+AMIFL3+PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL1,0.,AMIFL3).NE.0) GOTO 999
        CALL LU3ENT(J1,IFL1,21,IFL3,W,PARI(21),PARI(23))
        K(MSTU(1)+1,3)=2
C...Align target remnant (gluon) along -z axis
        CALL LUROBO(-ACOS(-P(J2,3)/SQRT(P(J2,3)**2+P(J2,1)**2)),
     &  0.,0.,0.,0.)
C...Phi-rotation to bring quark to phi=0.
        CALL LUROBO(0.,-PLU(J1,15),0.,0.,0.)
      ELSE

        IF(W.LT.AMIFL1+AMIFL3+0.9+2.*PARJ(32)) GOTO 999
        IF(LQMCUT(XP,ZP,AMIFL1,1.,AMIFL3).NE.0) GOTO 999
        P(J1,5)=AMIFL1
        P(J3,5)=AMIFL3
C...Choose target valence quark/diquark to form jet system with
C...produced antiquark/quark.
        IFLR2=INT(1.+LST(22)/3.+RLU(0))
        IF(IFLR2.EQ.LST(22)) THEN
          IFLR1=2101
          IF(RLU(0).GT.PARL(4)) IFLR1=2103
        ELSE
          IFLR1=1000*IFLR2+100*IFLR2+3
        ENDIF
        IFLR2=3-IFLR2
        MSTJ(93)=1
        AMR1=ULMASS(IFLR1)
CJR--
        KFIFL1=LUCOMP(IFLR1)
        IF (KFIFL1.EQ.90) THEN
          AMR1=AMR1-2*PARL(20)
        ELSEIF (1.LE.KFIFL1 .AND. KFIFL1.LE.6) THEN
          AMR1=AMR1-PARL(20)
        ENDIF
        MSTJ(93)=1
        AMR2=ULMASS(IFLR2)
        KFIFL2=LUCOMP(IFLR2)
        IF (KFIFL2.EQ.90) THEN
          AMR2=AMR2-2.*PARL(20)
        ELSEIF (1.LE.KFIFL2 .AND. KFIFL2.LE.6) THEN
          AMR2=AMR2-PARL(20)
        ENDIF
CJR--

        NREMH=0
  310   NREMH=NREMH+1
        IF(NREMH.GT.100) GOTO 999
        CALL LPRIKT(PARL(14),PT,PHI)
        CALL LREMH(0,PT,IFLR1,IFLR2,XT)
        PT2=PT**2
        TM2R1=AMR1**2+PT2
        TM2R2=AMR2**2+PT2
        P(J2,5)=SQRT(TM2R1/(1.-XT)+TM2R2/XT)
        IF(LQMCUT(XP,ZP,AMIFL1,P(J2,5),AMIFL3).NE.0) GOTO 310
        MSTU(10)=1
        CALL LU3ENT(J1,IFL1,21,IFL3,W,PARI(21),PARI(23))
        MSTU(10)=2
C...Align target remnant (effective gluon) along -z axis
        CALL LUROBO(-ACOS(-P(J2,3)/SQRT(P(J2,3)**2+P(J2,1)**2)),
     &0.,0.,0.,0.)
C...Phi-rotation to bring quark to phi=0.
        CALL LUROBO(0.,-PLU(J1,15),0.,0.,0.)
        EPZ=P(J2,4)-P(J2,3)
        IF(IFL1.GT.0) THEN
          IR1=J2
          IR2=J4
        ELSE
          IR1=J4
          IR2=J2
        ENDIF
        P(IR1,1)=PT*COS(PHI)
        P(IR1,2)=PT*SIN(PHI)
        P(IR1,3)=-0.5*((1.-XT)*EPZ-TM2R1/(1.-XT)/EPZ)
        P(IR1,4)= 0.5*((1.-XT)*EPZ+TM2R1/(1.-XT)/EPZ)
        P(IR1,5)=AMR1
        P(IR2,1)=-P(IR1,1)
        P(IR2,2)=-P(IR1,2)
        P(IR2,3)=-0.5*(XT*EPZ-TM2R2/XT/EPZ)
        P(IR2,4)= 0.5*(XT*EPZ+TM2R2/XT/EPZ)
        P(IR2,5)=AMR2
        K(IR1,1)=1
        K(IR1,2)=IFLR1
        K(IR2,1)=1
        K(IR2,2)=IFLR2
        K(J3,1)=2
        DO 320 I=J1,J4
          DO 320 J=3,5
  320       K(I,J)=0
        N=J4
        K(IR1,3)=2
        K(IR2,3)=2
       IF((P(J1,4)+P(J2,4))**2-(P(J1,1)+P(J2,1))**2-(P(J1,3)+P(J2,3))**2
     &  -P(J2,2)**2.LT.(P(J1,5)+P(J2,5)+PARJ(32))**2) GOTO 310
       IF((P(J3,4)+P(J4,4))**2-(P(J3,1)+P(J4,1))**2-(P(J3,3)+P(J4,3))**2
     &  -P(J4,2)**2.LT.(P(J3,5)+P(J4,5)+PARJ(32))**2) GOTO 310

      ENDIF

      CALL LAZIMU(XP,ZP)
      LST(21)=0
      RETURN

  999 LST(21)=5
      RETURN
      END

C **********************************************************************

      SUBROUTINE LXP(XP,IFAIL)

      IMPLICIT NONE

C...Choose value of XP according to QCD matrix elements weighted by
C...structure functions.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/


      common/debug1/iw,ix,ip
      integer iw,ix,ip
      
      INTEGER IFAIL,LOOP
      REAL XP,XPMIN,XPMAX,AP,BP,QXPMAX,XPWEIT
      REAL RLU,DSIGMA
      DOUBLE PRECISION DXPMAX

      IFAIL=1
CAE.Scheme for ME cutoff: W2, Q2, mixed, z-shat
      IF(LST(20).LE.1) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*(1.D0-DBLE(X))*DBLE(PARL(27)))
        DXPMAX=DBLE(X)/(DBLE(X)+(1.D0-DBLE(X))*DBLE(PARL(27)))
        XPMAX=SNGL(DXPMAX)
      ELSEIF(LST(20).EQ.2) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*DBLE(X)*DBLE(PARL(27)))
        DXPMAX=1.D0/(1.D0+DBLE(PARL(27)))
        XPMAX=SNGL(DXPMAX)
      ELSEIF(LST(20).EQ.3.OR.LST(20).EQ.4) THEN
        XPMIN=X
        DXPMAX=1.D0/(1.D0+DBLE(PARL(9)))
        XPMAX=DXPMAX
      ELSEIF(LST(20).EQ.5) THEN
        XPMIN=X
        DXPMAX=DBLE(Q2)/(DBLE(Q2)+DBLE(PARL(9)))
        XPMAX=DXPMAX
      ELSEIF(LST(20).EQ.6) THEN
        XPMIN=X
        DXPMAX=DBLE(Q2)/(DBLE(Q2)+DBLE(PARL(27)))
        XPMAX=DXPMAX
      ELSE
        WRITE(6,*) 'LXP: No such jet scheme!'
      ENDIF
CAE
      IF(XPMIN.LT.X.OR.XPMIN.GT.1.) RETURN
      IF(XPMIN.GE.XPMAX) RETURN
      AP=1.-XPMIN
      BP=(1.D0-DXPMAX)/AP
      IF(LST(24).EQ.2) THEN
        QXPMAX=PARI(15)
        IF(LST(17).NE.0.AND.LST(19).GE.0) QXPMAX=
     &  PARI(24)*PARI(15)+PARI(25)*PARI(16)+PARI(26)*PARI(17)
      ELSE
        QXPMAX=PARI(18)
        IF(LST(17).NE.0.AND.LST(19).GE.0) QXPMAX=
     &  PARI(24)*PARI(18)+PARI(25)*PARI(19)
      ENDIF

C...Safety factor on max value.
      QXPMAX=QXPMAX*1.05
      LOOP=0
  100 LOOP=LOOP+1
      IF(LOOP.GT.1000) RETURN
      XP=1.-AP*BP**RLU(0)
      XPWEIT=DSIGMA(XP)/QXPMAX
      IF(XPWEIT.LT.RLU(0)) GOTO 100
      IFAIL=0
      RETURN
      END

C **********************************************************************

      SUBROUTINE LZP(XP,ZP,IFAIL)

      IMPLICIT NONE

C...Choose value of ZP according to QCD matrix elements.
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

*...
      INTEGER IFAIL,LOOP,IPART
      REAL XP,AP,BP
      REAL RLU,DZPMAX
      REAL D,E,DP,DQCD,ZPWEIT
      REAL DZ,DX,DA,DB,DC,DD,DE
      INTEGER IH,I
      REAL ZP,C1,C2,SZP,FQG,FQQ,ZPMIN,ZPMAX,A,B,
     +CSIGN,C,CP,AA,BB,Z1

      DATA C1,C2/0.2122066,0.0795775/,DZPMAX,SZP,CP/3*0./
      FQG(DZ,DX,DA,DB,DC)=DA*(DZ**2+DX**2)/(1.-DX)+2.*DA*DX*DZ*(1.-DZ)
     &+2.*DA*(1.-DZ)+4.*DB*DX*DZ*(1.-DZ)+DC*(DZ**2+DX**2)/(1.-DX)+
     &2.*DC*(DX+DZ)*(1.-DZ)
      FQQ(DZ,DX,DA,DB,DC,DD,DE)=DA*DD*(DZ**2+(1.-DZ)**2)+DB*DE*DZ*
     &(1.-DZ)+DC*DD*(2.*DZ-1.)

*PEPSI>>
      IF(LST(40).NE.0) THEN
       CALL POLLZP(XP,ZP,IFAIL)
       RETURN
      ENDIF
*PEPSI<<

      IFAIL=1
      IH=1
      IF(LST(30).EQ.1) IH=2
CAE.Scheme for ME cutoff: W2, Q2, mixed, z-shat
      IF(LST(20).LE.1) THEN
        ZPMIN=(1.-X)*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).EQ.2) THEN
        ZPMIN=X*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).GE.3.OR.LST(20).LE.5) THEN
        ZPMIN=PARL(27)
      ELSEIF(LST(20).EQ.6) THEN
        ZPMIN=PARL(8)
      ELSE
        WRITE(6,*) 'LZP: No such jet scheme!'
      ENDIF
CAE
      IF(ZPMIN.LE.0..OR.ZPMIN.GE.0.5) RETURN
      ZPMAX=1.-ZPMIN
      I=IABS(LST(25))
      AP=1.-ZPMIN
      BP=ZPMIN/AP
      IF(LST(23).EQ.2) THEN
        A=PARI(24)
        B=PARI(25)
        CSIGN=-LST(30)*ISIGN(1,LST(25))*PARI(26)
      ELSE
        A=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(24)
        B=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(25)
        C=(EWQC(1,IH,I)-EWQC(2,IH,I))*PARI(26)
        CSIGN=-C*LST(30)*ISIGN(1,LST(25))
      ENDIF
      IF(LST(24).EQ.2) THEN
        DZPMAX=MAX(FQG(ZPMIN,XP,A,B,CSIGN),FQG(ZPMAX,XP,A,B,CSIGN))
        AA=2.*(A+CSIGN)/(1.-XP)-4.*A*XP-8.*B*XP-4.*CSIGN
        IF(ABS(AA).GT.1.E-20) THEN
          BB=2.*A*(XP-1.)+4.*B*XP+2.*CSIGN*(1.-XP)
          Z1=-BB/AA
          IF(Z1.GT.ZPMIN.AND.Z1.LT.ZPMAX)
     &    DZPMAX=MAX(DZPMAX,FQG(Z1,XP,A,B,CSIGN))
        ENDIF
        DZPMAX=DZPMAX*C1*1.05
      ELSEIF(LST(24).EQ.3) THEN
        CP=1./BP**2
        D=XP**2+(1.-XP)**2
        E=8.*XP*(1-XP)
        DZPMAX=MAX(FQQ(ZPMIN,XP,A,B,CSIGN,D,E),
     &  FQQ(ZPMAX,XP,A,B,CSIGN,D,E))
        AA=4.*A*D-2.*B*E
        IF(ABS(AA).GT.1.E-20) THEN
          BB=B*E-2.*A*D+2.*CSIGN*D
          Z1=-BB/AA
          IF(Z1.GT.ZPMIN.AND.Z1.LT.ZPMAX)
     &    DZPMAX=MAX(DZPMAX,FQQ(Z1,XP,A,B,CSIGN,D,E))
        ENDIF
        DZPMAX=DZPMAX*C2*1.05
      ENDIF
      IPART=LST(24)-1
      LOOP=0
  100 LOOP=LOOP+1
      IF(LOOP.GT.1000) RETURN
      IF(LST(24).EQ.2) THEN
        ZP=1.-AP*BP**RLU(0)
        SZP=1.-ZP
      ELSEIF(LST(24).EQ.3) THEN
        DP=BP*CP**RLU(0)
        ZP=DP/(1.+DP)
        SZP=ZP*(1.-ZP)
      ENDIF
      ZPWEIT=SZP*(A*DQCD(0,IPART,1,XP,ZP,0.)+B*DQCD(0,IPART,2,XP,ZP,0.)
     &+CSIGN*DQCD(0,IPART,3,XP,ZP,0.))/DZPMAX
      IF(ZPWEIT.LT.RLU(0)) GOTO 100
      IFAIL=0
      RETURN
      END

C **********************************************************************

      FUNCTION LQMCUT(XP,ZP,AM1,AM2,AM3)

C...Apply cuts, if necessary, on the event configuration
C...obtained from QCD matrix elements.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/


      INTEGER LQMCUT
      REAL XP,ZP,AM1,AM2,AM3,S12,S23,S13
      REAL W,X1,X2,X3,PA1,PA2,PA3
      DATA S12,S23,S13/3*0./

      IF(LST(24).EQ.2) THEN
        S12=Q2*(1.-XP)/XP
        S23=Q2*(XP-X)*(1.-ZP)/X/XP+AM2**2+AM3**2
        S13=Q2*(XP-X)*ZP/X/XP+AM1**2+AM3**2
      ELSEIF(LST(24).EQ.3) THEN
        S13=Q2*(1.-XP)/XP
        S23=Q2*(XP-X)*(1.-ZP)/X/XP+AM2**2+AM3**2
        S12=Q2*(XP-X)*ZP/X/XP+AM1**2+AM2**2
        IF(S13.LT.(AM1+AM3)**2) GOTO 900
      ENDIF

      W=SQRT(W2)
      X1=1.-(S23-AM1**2)/W2
      X3=1.-(S12-AM3**2)/W2
      X2=2.-X1-X3
      PARI(21)=X1
      PARI(22)=X2
      PARI(23)=X3
                
      IF(X1.GT.1..OR.X2.GT.1..OR.X3.GT.1.) GOTO 900
      IF(X1*W/2..LT.AM1.OR.X2*W/2..LT.AM2.OR.X3*W/2..LT.AM3) GOTO 900
      PA1=SQRT((0.5*X1*W)**2-AM1**2)
      PA2=SQRT((0.5*X2*W)**2-AM2**2)
      PA3=SQRT((0.5*X3*W)**2-AM3**2)
      IF(ABS((PA3**2-PA1**2-PA2**2)/(2.*PA1*PA2)).GE.1.) GOTO 900
      IF(ABS((PA2**2-PA1**2-PA3**2)/(2.*PA1*PA3)).GE.1.) GOTO 900
      LQMCUT=0
      RETURN

  900 LQMCUT=1
      RETURN
      END

C **********************************************************************

      SUBROUTINE LAZIMU(XP,ZP)

      IMPLICIT NONE

C...Choose azimuthal angle (PHI) according to QCD matrix elements.
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

            
      INTEGER J,IFL,I,IH
      REAL XP,ZP,DQCD,A,B,C,PHIMAX,PHI,SGN
      REAL RLU

*PEPSI>>
      IF(LST(40).NE.0) THEN
         CALL POLLAZIMU(XP,ZP)
         RETURN
      ENDIF
*PEPSI<<

      J=LST(24)-1
      SGN=SIGN(1.,2.5-LST(24))
      IFL=LST(25)
      I=IABS(IFL)
      IH=1
      IF(LST(30).EQ.1) IH=2

      IF(LST(23).EQ.2) THEN
        A=PARI(24)*DQCD(0,J,1,XP,ZP,Y)+PARI(25)*DQCD(0,J,2,XP,ZP,Y)
     &  -LST(30)*ISIGN(1,IFL)*PARI(26)*DQCD(0,J,3,XP,ZP,Y)
        B=DQCD(1,J,1,XP,ZP,Y)
     &  +SGN*LST(30)*ISIGN(1,IFL)*DQCD(1,J,3,XP,ZP,Y)
        C=DQCD(2,J,1,XP,ZP,Y)
      ELSE
        A=(EWQC(1,IH,I)+EWQC(2,IH,I))*(PARI(24)*DQCD(0,J,1,XP,ZP,Y)+
     &    PARI(25)*DQCD(0,J,2,XP,ZP,Y))
     &    -LST(30)*ISIGN(1,IFL)*(EWQC(1,IH,I)-EWQC(2,IH,I))
     &    *PARI(26)*DQCD(0,J,3,XP,ZP,Y)
        B=(EWQC(1,IH,I)+EWQC(2,IH,I))*DQCD(1,J,1,XP,ZP,Y)
     &    +SGN*LST(30)*ISIGN(1,IFL)*(EWQC(1,IH,I)-EWQC(2,IH,I))
     &    *DQCD(1,J,3,XP,ZP,Y)
        C=(EWQC(1,IH,I)+EWQC(2,IH,I))*DQCD(2,J,1,XP,ZP,Y)
      ENDIF

      PHIMAX=ABS(A)+ABS(B)+ABS(C)
  100 PHI=6.2832*RLU(0)
      IF(A+B*COS(PHI)+C*COS(2.*PHI).LT.RLU(0)*PHIMAX) GOTO 100
      CALL LUROBO(0.,PHI,0.,0.,0.)

CAE.Store ME variables
      PARL(28)=XP
      PARL(29)=ZP
      PARL(30)=PHI
CAE
      RETURN
      END

C **********************************************************************

      FUNCTION DSIGMA(XP)
      
      IMPLICIT NONE
      
C...Differential cross section for first order QCD processes.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      
      REAL DSIGMA,XP,DQCDI,ULMASS,POLDSIGMA
      INTEGER I,IH,IL,IU,IP
      REAL PQH,AMU,XI,ZPMIN,ZPMAX,XPQ,WQ,WQB,TQ,TQB,T1,S13,SGN,SIG
      DIMENSION XPQ(-6:6),PQH(17,2)

*PEPSI>>
      IF (LST(40).NE.0) THEN
         DSIGMA = POLDSIGMA(XP)
         RETURN
      ENDIF
*PEPSI<<


      DSIGMA=0.
      DO 10 I=1,17
      PQH(I,1)=0.
      PQH(I,2)=0.
   10 PQ(I)=0.

      MSTJ(93)=1
      AMU=ULMASS(2)
      IF(LST(32).EQ.1.AND.LST(19).GE.0.AND.LST(17).EQ.1) THEN
        IL=LST(32)
        IU=LST(32)
      ELSE
        IL=1
        IU=3
        IF(LST(23).EQ.1.OR.LST(24).EQ.3) IU=2
      ENDIF
      XI=X/XP
C...Scheme for ME cutoff: W2, Q2, mixed
      IF(LST(20).LE.1) THEN
        ZPMIN=(1.-X)*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).EQ.2) THEN
        ZPMIN=X*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
        ZPMIN=PARL(27)
      ELSEIF(LST(20).GE.6) THEN
        ZPMIN=PARL(8)
      ENDIF

      IF(ZPMIN.LE.0..OR.ZPMIN.GE.0.5) RETURN
      ZPMAX=1.D0-DBLE(ZPMIN)
      CALL LNSTRF(XI,Q2,XPQ)
      IF(LST(24).EQ.3) GOTO 3000

C...Gluon bremsstrahlung process, i.e. qg-event.
 2000 DO 2500 IP=IL,IU
      SIG=DQCDI(1,IP,XP,ZPMIN,ZPMAX)
      SGN=SIGN(1.,5.-2.*IP)
      DO 2300 IH=1,2
      IF(IH.EQ.1) THEN
        IF(PARL(6).GT.0.99) GOTO 2300
        IF(LST(32).EQ.0.AND.LST(30).NE.-1) GOTO 2300
      ELSEIF(IH.EQ.2) THEN
        IF(PARL(6).LT.-0.99) GOTO 2300
        IF(LST(32).EQ.0.AND.LST(30).NE.1) GOTO 2300
      ENDIF
      IF(LST(32).NE.0) LST(30)=SIGN(1.,IH-1.5)
      IF(LST(23).NE.2) THEN
        DO 2100 I=1,LST(12)
        WQ=XPQ(I)*SIG*(EWQC(1,IH,I)+SGN*EWQC(2,IH,I))
        WQB=XPQ(-I)*SIG*SGN*(EWQC(1,IH,I)+SGN*EWQC(2,IH,I))
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(12),IH)=PQH(I+LST(12),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 2100   CONTINUE
      ELSEIF(LST(23).EQ.2) THEN
C...Zero CC cross-section for one helicity state.
        IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 2300
        IF(IP.EQ.3) THEN
          TQ=-LST(30)
          TQB=-TQ
        ELSE
          TQ=1.
          TQB=1.
        ENDIF
        DO 2200 I=1,LST(12)
        T1=-K(3,2)*QC(I)
        IF(T1.GT.0) THEN
          WQ=XPQ(I)*SIG*TQ
          WQB=0.
        ELSE
          WQB=XPQ(-I)*SIG*TQB
          WQ=0.
        ENDIF
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(12),IH)=PQH(I+LST(12),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 2200   CONTINUE
        ENDIF
 2300 CONTINUE
 2500 CONTINUE
      DO 2600 I=1,17
 2600 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)
      IH=1
      IF(LST(30).EQ.1) IH=2
      IF(LST(32).EQ.0) THEN
C...Simulation: cross section for chosen helicity state only.
        DSIGMA=PQH(17,IH)
      ELSEIF(LST(19).EQ.-1) THEN
C...Integration event-by-event: normalize and include alpha_s*1/(1-xp)
        DSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
C...Max of dsigma/dxp for L- and R-handed lepton.
        IF(PQH(17,1).GT.PARI(15)) PARI(15)=PQH(17,1)
        IF(PQH(17,2).GT.PARI(16)) PARI(16)=PQH(17,2)
      ELSE
C...Integration for grid: normalize and include alpha_s*1/(1-xp)
        DSIGMA=PQ(17)/PARI(20)*PARL(25)/(1.-XP)
        IF(LST(17).EQ.0) THEN
C...Fixed beam energy, max of dsigma/dxp for L- and R-handed lepton.
          IF(PQH(17,1).GT.PARI(15)) PARI(15)=PQH(17,1)
          IF(PQH(17,2).GT.PARI(16)) PARI(16)=PQH(17,2)
        ELSE
C...Variable beam energy, max of dsigma/dxp for S, T, I contributions.
          IF(PQ(17)/PARI(23+LST(32)).GT.PARI(14+LST(32)))
     &    PARI(14+LST(32))=PQ(17)/PARI(23+LST(32))
        ENDIF
      ENDIF
      RETURN

C...Boson-gluon fusion, i.e. qq-event.
 3000 S13=Q2*(1.-XP)/XP
      IF(S13.LT.4.*AMU**2) RETURN
      DO 3500 IP=IL,IU
      SIG=XPQ(0)*DQCDI(2,IP,XP,ZPMIN,ZPMAX)
      DO 3300 IH=1,2
      IF(IH.EQ.1) THEN
        IF(PARL(6).GT.0.99) GOTO 3300
        IF(LST(32).EQ.0.AND.LST(30).NE.-1) GOTO 3300
      ELSEIF(IH.EQ.2) THEN
        IF(PARL(6).LT.-0.99) GOTO 3300
        IF(LST(32).EQ.0.AND.LST(30).NE.1) GOTO 3300
      ENDIF
      IF(LST(32).NE.0) LST(30)=SIGN(1.,IH-1.5)
      IF(LST(23).NE.2) THEN
        DO 3100 I=1,LST(13)
        MSTJ(93)=1
        IF(S13.LT.4.*ULMASS(I)**2) GOTO 3100
        WQ=SIG/2.*(EWQC(1,IH,I)+EWQC(2,IH,I))
        WQB=WQ
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(13),IH)=PQH(I+LST(13),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 3100   CONTINUE
      ELSEIF(LST(23).EQ.2) THEN
C...Zero CC cross-section for one helicity state.
        IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 3300
        DO 3200 I=1,LST(13)
        MSTJ(93)=1
        IF(S13.LT.(AMU+ULMASS(I))**2) GOTO 3200
        IF(K(3,2)*QC(I).LT.0) THEN
          WQ=SIG
          WQB=0.
        ELSE
          WQB=SIG
          WQ=0.
        ENDIF
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(13),IH)=PQH(I+LST(13),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 3200   CONTINUE
      ENDIF
 3300 CONTINUE
 3500 CONTINUE
      DO 3600 I=1,17
 3600 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)
      IH=1
      IF(LST(30).EQ.1) IH=2
      IF(LST(32).EQ.0) THEN
C...Simulation: cross section for chosen helicity state only.
        DSIGMA=PQH(17,IH)
      ELSEIF(LST(19).EQ.-1) THEN
C...Integration event-by-event: normalize and include alpha_s*1/(1-xp)
        DSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
C...Max of dsigma/dxp for L- and R-handed lepton.
        IF(PQH(17,1).GT.PARI(18)) PARI(18)=PQH(17,1)
        IF(PQH(17,2).GT.PARI(19)) PARI(19)=PQH(17,2)
      ELSE
C...Integration for grid: normalize and include alpha_s*1/(1-xp)
        DSIGMA=PQ(17)/PARI(20)*PARL(25)/(1.-XP)
        IF(LST(17).EQ.0) THEN
C...Fixed beam energy, max of dsigma/dxp for L- and R-handed lepton.
          IF(PQH(17,1).GT.PARI(18)) PARI(18)=PQH(17,1)
          IF(PQH(17,2).GT.PARI(19)) PARI(19)=PQH(17,2)
        ELSE
C...Variable beam energy, max of dsigma/dxp for S, T, I contributions.
          IF(PQ(17)/PARI(23+LST(32)).GT.PARI(17+LST(32)))
     &    PARI(17+LST(32))=PQ(17)/PARI(23+LST(32))
        ENDIF
      ENDIF
      RETURN
      END
C **********************************************************************
      
      FUNCTION DSIGM2(X)
      
      IMPLICIT NONE
      
CAE...Modified version of DSIGMA with a factor 1/(1-XP) is removed
C...by a variable transformation. This makes the integration of the 
C...cross section faster.

      REAL X,XP,DSIGMA,DSIGM2
      
      XP=1.0-EXP(X)
      DSIGM2=(1.0-XP)*DSIGMA(XP)

      RETURN
      END

C **********************************************************************

      FUNCTION DQCD(ICOSFI,IPART,IP,XP,ZP,YY)
      
      IMPLICIT NONE
      
C...First order QCD matrix elements from R.D. Peccei and R. Ruckl:
C...Nucl. Phys. B162 (1980) 125

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/


      INTEGER ICOSFI,IP,IPART
      REAL DQCD,XP,ZP,YY,C1,C2,C3,C4,C5
      
C...Constants C1 to C5 are resp. 2/3/pi, 1/4/pi, 4/3/pi, 1/2/pi, 1/pi
      DATA C1,C2,C3,C4,C5/0.2122066,0.0795775,0.4244132,0.1591549,
     &     0.3183099/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        DQCD=0.0
        RETURN
      ENDIF

      IF(ICOSFI.EQ.0) THEN
        IF(IPART.EQ.1) THEN
          IF(IP.EQ.1) THEN
            DQCD=C1*((ZP**2+XP**2)/(1.-XP)/(1.-ZP)+2.*(XP*ZP+1.))
          ELSEIF(IP.EQ.2) THEN
            DQCD=C1*4.*XP*ZP
          ELSEIF(IP.EQ.3) THEN
            DQCD=C1*((ZP**2+XP**2)/(1.-XP)/(1.-ZP)+2.*(XP+ZP))
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ELSEIF(IPART.EQ.2) THEN
          IF(IP.EQ.1) THEN
            DQCD=C2*(XP**2+(1.-XP)**2)*(ZP**2+(1.-ZP)**2)/(1.-ZP)/ZP
          ELSEIF(IP.EQ.2) THEN
            DQCD=C2*8.*XP*(1.-XP)
          ELSEIF(IP.EQ.3) THEN
            DQCD=C2*(XP**2+(1.-XP)**2)*(ZP-(1.-ZP))/(1.-ZP)/ZP
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ELSE
          WRITE(6,1000) ICOSFI,IPART,IP
        ENDIF

      ELSEIF(ICOSFI.EQ.1) THEN
        IF(IPART.EQ.1) THEN
          IF(IP.EQ.1) THEN
            DQCD=C3*YY*SQRT((1.-YY)*XP*ZP/(1.-XP)/(1.-ZP))*
     &      (1.-2./YY)*(1.-ZP-XP+2.*XP*ZP)
          ELSEIF(IP.EQ.3) THEN
            DQCD=C3*YY*SQRT((1.-YY)*XP*ZP/(1.-XP)/(1.-ZP))*
     &      (1.-XP-ZP)
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ELSEIF(IPART.EQ.2) THEN
          IF(IP.EQ.1) THEN
            DQCD=C4*YY*SQRT((1.-YY)*XP*(1.-XP)/ZP/(1.-ZP))*
     &      (1.-2./YY)*(1.-2.*ZP)*(1.-2.*XP)
          ELSEIF(IP.EQ.3) THEN
            DQCD=C4*YY*SQRT((1.-YY)*XP*(1.-XP)/ZP/(1.-ZP))*
     &      (1.-2.*XP)
          ELSE
            WRITE(6,1000) ICOSFI,IPART,IP
          ENDIF
        ENDIF

      ELSEIF(ICOSFI.EQ.2) THEN
        IF(IPART.EQ.1) THEN
          DQCD=C3*(1.-YY)*XP*ZP
        ELSEIF(IPART.EQ.2) THEN
          DQCD=C5*(1.-YY)*XP*(1.-XP)
        ELSE
          WRITE(6,1000) ICOSFI,IPART,IP
        ENDIF

      ELSE
        WRITE(6,1000) ICOSFI,IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine DQCD     ',
     &' ICOSFI, IPART, IP = ',3I10)
      END
C **********************************************************************

      FUNCTION DQCDI(IPART,IP,XP,ZPMIN,ZPMAX)

      IMPLICIT NONE

C...First order QCD matrix elements as in function DQCD but analytically
C...integrated over ZP from ZPMIN to ZPMAX, also a factor 1/(1-XP) is
C...factored out (since XP chosen randomly according to 1/(1-XP) distr.)

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      
      INTEGER IPART,IP
      REAL XP,ZPMIN,ZPMAX,C1,C2,ZLOG,DQCDI

      DATA C1,C2/0.2122066,0.0795775/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        DQCDI=0.0
        RETURN
      ENDIF

      IF(IPART.EQ.1) THEN
        IF(IP.EQ.1) THEN
          ZLOG=ALOG(ZPMAX/ZPMIN)
          DQCDI=C1*(XP**2*ZLOG+ZPMIN-ZPMAX+(ZPMIN**2-ZPMAX**2)/2.+ZLOG+
     &    XP*(1.-XP)*(ZPMAX**2-ZPMIN**2)+2.*(1.-XP)*(ZPMAX-ZPMIN))
        ELSEIF(IP.EQ.2) THEN
          DQCDI=C1*2.*XP*(1.-XP)*(ZPMAX**2-ZPMIN**2)
        ELSEIF(IP.EQ.3) THEN
          ZLOG=ALOG(ZPMAX/ZPMIN)
          DQCDI=C1*(XP**2*ZLOG+ZPMIN-ZPMAX+(ZPMIN**2-ZPMAX**2)/2.+ZLOG+
     &    2.*XP*(1.-XP)*(ZPMAX-ZPMIN)+(1.-XP)*(ZPMAX**2-ZPMIN**2))
        ELSE
          WRITE(6,1000) IPART,IP
        ENDIF

      ELSEIF(IPART.EQ.2) THEN
        IF(IP.EQ.1) THEN
          DQCDI=C2*(1.-XP)*(XP**2+(1.-XP)**2)*(2.*(ZPMIN-ZPMAX)+
     &    ALOG(ZPMAX**2/ZPMIN**2))
        ELSEIF(IP.EQ.2) THEN
          DQCDI=C2*8.*XP*(1.-XP)**2*(ZPMAX-ZPMIN)
        ELSEIF(IP.EQ.3) THEN
          DQCDI=0.
        ELSE
          WRITE(6,1000) IPART,IP
        ENDIF

      ELSE
        WRITE(6,1000) IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine DQCDI     ',
     &' IPART, IP = ',2I10)
      END

C **********************************************************************

      SUBROUTINE LREMH(IFLRO,PT,IFLR,K2,Z)

      IMPLICIT NONE

C...Gives flavour and energy-momentum fraction Z for the particle
C...to be produced out of the target remnant when that is not a
C...simple diquark.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      INTEGER IFLRO,IFLR,K2,IDUM,KSP,KC2,K2A
      INTEGER IFLQ,IFLQQ,LUCOMP
      REAL RLU,ULMASS,TM2,PT
      REAL Z,AMSP,AMK2,A,FC

C...Flavours fixed when calling from LQQBEV OR LYREMN
      IF(IFLRO.EQ.0) GOTO 200

C...Split target remnant qqqQ -> qqQ + q or qqqQbar -> qQbar + qq
C...Q (Qbar) is the partner to the struck sea quark
C...qqq are the nucleon valence quarks from which a quark q or a
C...diquark qq is chosen at random to form a jet system with the
C...scattered sea antiquark or quark, respectively, the other parton
C...forms a baryon qqQ or meson qQbar, respectively.
  100 IFLQ=INT(1.+LST(22)/3.+RLU(0))
      IF(IFLQ.EQ.LST(22)) THEN
        IFLQQ=2101
        IF(RLU(0).GT.PARL(4)) IFLQQ =2103
      ELSE
        IFLQQ=1000*IFLQ+100*IFLQ+3
      ENDIF
      IFLQ=3-IFLQ

C...Choose flavour of hadron and parton for jet system
      IF(IFLRO.GT.0) THEN
        CALL LUKFDI(IFLQQ,IFLRO,IDUM,K2)
        IF(K2.EQ.0) GOTO 100
        IFLR=IFLQ
      ELSE
        CALL LUKFDI(IFLQ,IFLRO,IDUM,K2)
        IF(K2.EQ.0) GOTO 100
        IFLR=IFLQQ
      ENDIF

C...Entry from LQQBEV & LYREMN with flavours given, choose E-p fraction
  200 KSP=IFLR
C...Split energy-momentum of target remnant according to functions P(z)
C...z=E-pz fraction for qq (q) forming jet-system with struck Q (Qbar)
C...1-z=E-pz fraction for qQbar (qqQ) hadron
C...mq=mass of (light) parton remnant q (qq) in jet system
C...mQ=mass of produced (heavy flavour) hadron
      MSTJ(93)=1
      AMSP=ULMASS(KSP)
      MSTJ(93)=1
      AMK2=ULMASS(K2)
      IF(LST(14).EQ.1) THEN
C...P(z)=(a+1)(1-z)**a with <z>=1/(a+2)=1/3 since a=1 fixed
        Z=1.-SQRT(RLU(0))
C...Flip if baryon produced
        KC2=IABS(LUCOMP(K2))
        IF(KC2.GE.301.AND.KC2.LE.400) Z=1.-Z
      ELSEIF(LST(14).EQ.2) THEN
C...P(z)=(a+1)(1-z)**a with <z>=1/(a+2)=mq/(mq+mQ) --> a=a(mq,mQ)
        A=(AMSP+AMK2)/AMSP - 2.
        Z=RLU(0)**(1./(A+1.))
      ELSEIF(LST(14).EQ.3) THEN
C...Using Peterson fragmentation function
C...P(z)=N/(z(1-1/z-c/(1-z))**2)  where c=(mq/mQ)**2  (FC=-c)
        FC=-(AMSP/AMK2)**2
  300   Z=RLU(0)
        IF(-4.*FC*Z*(1.-Z)**2.LT.RLU(0)*((1.-Z)**2-FC*Z)**2) GOTO 300
      ELSEIF(LST(14).EQ.4) THEN
C...Using chosen fragmentation function in JETSET
        TM2=AMK2**2+PT**2
        CALL LUZDIS(1,0,TM2,Z)
      ENDIF
      LST(27)=1
      K2A=IABS(K2)
      IF((K2A.GE.1.AND.K2A.LE.8).OR.K2A.EQ.21.OR.LUCOMP(K2A).EQ.90)
     &LST(27)=2

      RETURN
      END

C **********************************************************************

      SUBROUTINE LPRIKT(S,PT,PHI)

      IMPLICIT NONE

C...Size (PT) and azimuthal angle (PHI) of primordial kt according
C...to a Gaussian distribution.
      REAL S,PT,PHI,RLU
      
      PT=S*SQRT(-ALOG(RLU(0)))
      PHI=6.2832*RLU(0)
      RETURN
      END

C **********************************************************************

      SUBROUTINE LFRAME(IFR,IPH)

      IMPLICIT NONE

C...Make transformation from hadronic CM frame to lab frame.

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DBETA
      REAL STHETA,SPHI,PB,PHIR
      SAVE /LBOOST/


      INTEGER IFR,IPH,IFRAME,IPHI,I,J
      REAL THEBOS,PHIBOS,PLU

      IFRAME=IFR
      IPHI=IPH
      IF(IFRAME.LT.1.OR.IFRAME.GT.4.OR.IPHI.LT.0.OR.IPHI.GT.1) GOTO 999
      IF(IFRAME.EQ.1) IPHI=0

CGI -- Also boost lines up to N+MSTU(3)
      N=N+1+MSTU(3)
      DO 5 J=1,5
    5 P(N,J)=PB(J)

   10 CONTINUE
      IF(IPHI.NE.LST(29)) THEN
         IFRAME=2
      ELSE
         IFRAME=IFR
      ENDIF
      IF((IFRAME.EQ.LST(28)).AND.(IPHI.EQ.LST(29))) THEN
        DO 15 J=1,5
   15   PB(J)=P(N,J)
        N=N-1-MSTU(3)
        RETURN
      ENDIF

      GOTO(100,200,300,400), LST(28)
      GOTO 999

  100 IF(IFRAME.GE.2) THEN
        CALL LUDBRB(0,0,STHETA(2),SPHI(2),0.D0,0.D0,0.D0)
        CALL LUDBRB(0,0,0.,0.,DBETA(2,1),DBETA(2,2),DBETA(2,3))
        LST(28)=2
      ELSE
        GOTO 999
      ENDIF
      GOTO 10

  200 IF(IPHI.NE.LST(29)) THEN
       CALL LUDBRB(0,0,0.,SIGN(PHIR,FLOAT(IPHI-LST(29))),0.D0,0.D0,0.D0)
       LST(29)=IPHI
      ENDIF

      IF(IFRAME.EQ.1) THEN
        CALL LUDBRB(0,0,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
        CALL LUDBRB(0,0,-STHETA(2),0.,0.D0,0.D0,0.D0)
        LST(28)=1
      ELSEIF(IFRAME.GE.3) THEN
        IF(LST(17).EQ.0) THEN
          CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,DBETA(1,3))
          IF(PSAVE(3,1,3).LT.0.) THEN
            DO 210 I=1,N
            V(I,3)=-V(I,3)
  210       P(I,3)=-P(I,3)
          ENDIF
        ELSE
          CALL LUDBRB(0,0,STHETA(1),SPHI(1),0.D0,0.D0,0.D0)
          CALL LUDBRB(0,0,0.,0.,DBETA(1,1),DBETA(1,2),DBETA(1,3))
        ENDIF
        LST(28)=3
      ENDIF
      GOTO 10

  300 IF(IFRAME.LE.2) THEN
        IF(LST(17).EQ.0) THEN
          IF(PSAVE(3,1,3).LT.0.) THEN
            DO 310 I=1,N
            V(I,3)=-V(I,3)
  310       P(I,3)=-P(I,3)
          ENDIF
          CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,-DBETA(1,3))
        ELSE
          CALL LUDBRB(0,0,0.,0.,-DBETA(1,1),-DBETA(1,2),-DBETA(1,3))
          CALL LUDBRB(0,0,0.,-SPHI(1),0.D0,0.D0,0.D0)
          CALL LUDBRB(0,0,-STHETA(1),0.,0.D0,0.D0,0.D0)
        ENDIF
        LST(28)=2
      ELSEIF(IFRAME.EQ.4) THEN
        THEBOS=PLU(N,13)
        PHIBOS=PLU(N,15)
        CALL LUDBRB(0,0,0.,-PHIBOS,0.D0,0.D0,0.D0)
        CALL LUDBRB(0,0,-THEBOS,0.,0.D0,0.D0,0.D0)
        LST(28)=4
      ENDIF
      GOTO 10

  400 IF(IFRAME.LE.3) THEN
        CALL LUDBRB(0,0,THEBOS,PHIBOS,0.D0,0.D0,0.D0)
        LST(28)=3
      ENDIF
      GOTO 10

  999 WRITE(6,1000) IFRAME,IPHI,LST(28),LST(29)
 1000 FORMAT(' BAD VARIABLES IN SUBROUTINE LFRAME: IFRAME,IPHI,',      
     &'LST(28),LST(29) =',4I5)
      RETURN
      END

C ********************************************************************

      SUBROUTINE LWBB(ENU)

      IMPLICIT NONE
      REAL EMEAN,SLOPE,EMIN,EMAX,ENU 
      REAL A1,A2,E
      REAL RLU

C...Gives energy (ENU) of a (anti-)neutrino chosen from a simple
C...parametrization of a wide band beam.

      DATA EMEAN,SLOPE,EMIN,EMAX/30.,0.02,12.,300./
      A1=1./(EMEAN-12.)
      A2=EXP(EMEAN*SLOPE)
  100 ENU=300.*RLU(0)
      IF(ENU.LT.EMEAN)THEN
      E=A1*(ENU-12.)
      ELSE
      E=A2*EXP(-ENU*SLOPE)
      ENDIF
      IF(ENU.LT.EMIN.OR.ENU.GT.EMAX) GOTO 100
      IF(E.LT.RLU(0)) GOTO 100
      RETURN
      END

C **********************************************************************

      SUBROUTINE LWEITS(LFILE)

      IMPLICIT NONE

C...Integrates the QCD matrix elements to obtain probabilities for
C...qg- and qq-events as a function of (x,W). Also finds various
C...maximum values to be used for the QCD simulation. Results stored
C...in common LGRID and optionally written to logical file LFILE.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/


      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/


      INTEGER LFILE,NCALL,LST2,IPMAX,IW,IX,LW,LX,IYCUT,IP
      REAL WWI,XXI,WMAX,PQCOM,RQG,XPMIN,XPMAX,EPS,RQ,
     +QTOT,RQQB,W,RESULT
      REAL YSPLIT,XSPLIT,RTOT,ULALPS
      INTEGER NY,NX,NL,I
      REAL  P27MAX,YCMIN,YCMAX
      DIMENSION WWI(21,4),XXI(31,4)
      EXTERNAL DSIGMA,DSIGM2
      LOGICAL ZOOM
      
       DATA WWI/
     1  5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,19.,
     1  20.,21.,22.,23.,24.,25.,
     2     5.,5.2,5.3,5.4,5.5,5.6,5.7,5.8,5.9,
     2     6.,6.1,6.2,6.3,6.4,6.5,6.6,6.7,6.8,6.9,7.,7.3,
*     2     16.,17.,18.,19.,20.,21.,22.,23.,24.,25.,
*     2  5.,7.,9.,11.,13.,15.,17.,19.,21.,23.,25.,27.,29.,31.,33.,
*     2  35.,37.,39.,41.,43.,45.,
     3  5.,7.,10.,15.,20.,25.,30.,40.,50.,75.,100.,125.,150.,175.,
     3  200.,225.,250.,275.,300.,325.,350.,
     4  5.,10.,15.,20.,30.,50.,75.,100.,150.,200.,250.,300.,400.,
     4  500.,700.,900.,1200.,1500.,1800.,2100.,2500./
      DATA XXI/
     1  1.E-3,2.E-3,3.E-3,4.E-3,5.E-3,6.E-3,8.E-3,
     1  1.E-2,2.E-2,3.E-2,4.E-2,5.E-2,6.E-2,8.E-2,
     1  .1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.72,.8,.87,.94,.999,
C.HI     1  1.E-2,2.E-2,2.5E-2,3.E-2,3.5E-2,4.E-2,4.5E-2,5.E-2,
C.HI     1  5.5E-2, 6.E-2,6.5E-2,7.E-2, 7.5E-2,8.E-2, 8.5E-2,
C.HI     1  .1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.72,.8,.9,.999,
     2  1.E-3,2.E-3,3.E-3,4.E-3,5.E-3,6.E-3,8.E-3,
     2  1.E-2,2.E-2,3.E-2,4.E-2,5.E-2,6.E-2,8.E-2,
     2  .1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.72,.8,.87,.94,.999,
     3  1.E-5,2.E-5,4.E-5,6.E-5,8.E-5,1.E-4,2.E-4,4.E-4,6.E-4,8.E-4,
     3  1.E-3,2.E-3,4.E-3,6.E-3,8.E-3,1.E-2,2.E-2,4.E-2,6.E-2,8.E-2,
     3  .1,.2,.3,.4,.5,.6,.7,.8,.87,.94,.999,
     4  1.E-5,2.E-5,4.E-5,6.E-5,8.E-5,1.E-4,2.E-4,4.E-4,6.E-4,8.E-4,
     4  1.E-3,2.E-3,4.E-3,6.E-3,8.E-3,1.E-2,2.E-2,4.E-2,6.E-2,8.E-2,
     4  .1,.2,.3,.4,.5,.6,.7,.8,.87,.94,.999/
      DATA NCALL/0/,XSPLIT/0.1/,YSPLIT/2./

      NCALL=NCALL+1
      LST2=LST(2)
      LST(2)=-3
      WMAX=SQRT(PARL(21))

      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &WRITE(6,1000) PARL(11),LST(13),MSTU(112),PARU(112),
     &PARL(8),PARL(9),PARL(12),PARL(13)
      IF(LST(17).EQ.0) THEN
        NP=1
        IPMAX=2
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1010)
      ELSE
        NP=3
        IF(LST(23).EQ.1) NP=2
        IPMAX=3
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) THEN
          IF(LST(19).GE.0.AND.LST(19).LT.10) WRITE(6,1020)
          IF(LST(19).GE.10.OR.LST(19).EQ.-10) WRITE(6,2020)
        ENDIF
      ENDIF

      IF(LST(19).GE.0.AND.LST(19).LT.10) THEN
C...Fixed grid in x and W
        IF(LST(19).EQ.0)THEN
C...Grid specified by user.
          READ(5,*) NWW,NXX
          READ(5,*) (WW(IW),IW=1,NWW)
          READ(5,*) (XX(IX),IX=1,NXX)
          IF(XX(NXX).GT..99) XX(NXX)=.99
        ELSEIF(LST(19).GE.1.AND.LST(19).LE.4) THEN
C...Grid taken from data in arrays WWI, XXI.
          DO 10 IW=1,NWW
   10     WW(IW)=WWI(IW,LST(19))
          DO 20 IX=1,NXX
   20     XX(IX)=XXI(IX,LST(19))
        ENDIF
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1030) LST(19),NXX,NWW,XX,WW
        IF(WMAX.GT.WW(NWW)) THEN
          IF(LST(3).GE.1) WRITE(6,1040) WMAX,WW(NWW)
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
        ENDIF
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1100)
      ELSEIF(LST(19).GE.10.OR.LST(19).EQ.-10) THEN
C...Grid in x,y automatically defined from available x,y-ranges
        NX=NXX
        NY=NWW
        IF(XMIN.GE.XSPLIT) THEN
          DO 30 I=1,NX
   30     XX(I)=MIN(0.999,XMIN+(XMAX-XMIN)*(I-1)/FLOAT(NX-1))
        ELSEIF(XMAX.GT.XSPLIT) THEN
          NL=MIN(2.*NX/3.,MAX(NX/3.,NX*LOG(XSPLIT/XMIN)/LOG(XMAX/XMIN)))
          DO 40 I=1,NL
   40     XX(I)=MIN(0.999,
     &   10.**(LOG10(XMIN)+(LOG10(XSPLIT)-LOG10(XMIN))*(I-1)/FLOAT(NL)))
          DO 41 I=NL+1,NX
   41     XX(I)=MIN(0.999,XSPLIT+(XMAX-XSPLIT)*(I-NL-1)/FLOAT(NX-NL-1))
        ELSE
          DO 50 I=1,NX
   50     XX(I)=MIN(0.999,
     &   10.**(LOG10(XMIN)+(LOG10(XMAX)-LOG10(XMIN))*(I-1)/FLOAT(NX-1)))
        ENDIF
C...y-variable stored in same array as W
        IF(YMIN.GE.YSPLIT) THEN
          DO 60 I=1,NY
   60     WW(I)=MIN(0.999,YMIN+(YMAX-YMIN)*(I-1)/FLOAT(NY-1))
        ELSEIF(YMAX.GT.YSPLIT) THEN
          NL=MIN(2.*NY/3.,MAX(NY/3.,NY*LOG(YSPLIT/YMIN)/LOG(YMAX/YMIN)))
          DO 70 I=1,NL
   70     WW(I)=MIN(0.999,
     &   10.**(LOG10(YMIN)+(LOG10(YSPLIT)-LOG10(YMIN))*(I-1)/FLOAT(NL)))
          DO 71 I=NL+1,NY
   71     WW(I)=MIN(0.999,YSPLIT+(YMAX-YSPLIT)*(I-NL-1)/FLOAT(NY-NL-1))
        ELSE
          DO 80 I=1,NY
   80     WW(I)=MIN(0.999,
     &   10.**(LOG10(YMIN)+(LOG10(YMAX)-LOG10(YMIN))*(I-1)/FLOAT(NY-1)))
        ENDIF
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,2030) LST(19),NXX,NWW,XX,WW
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,2100)
      ENDIF

      LW=0
      DO 500 IW=1,NWW
      W=WW(IW)
      Y=WW(IW)
      IF(LW.GT.0) GOTO 600
      IF(LST(19).LT.10.AND.W.GT.WMAX) LW=LW+1
      LX=0
      DO 400 IX=1,NXX
      X=XX(IX)
      IF(LST(19).GE.0.AND.LST(19).LT.10) THEN
C...x,W given.
        W2=W**2
        U=(W2-PSAVE(3,2,5)**2)/(2.*PSAVE(3,2,5)*(1.-X))
        Q2=2.*PSAVE(3,2,5)*U*X
        Y=Q2/(PARL(21)*X)
      ELSEIF(LST(19).GE.10.OR.LST(19).EQ.-10) THEN
C...x,y given.
        PARL(22)=Y*PARL(21)
        Q2=X*PARL(22)
        U=PARL(22)/(2.*PSAVE(3,2,5))
        W2=PARL(22)*(1.-X)+PSAVE(3,2,5)**2
        W=SQRT(W2)
      ENDIF
C...Protection against too small Q2 in structure functions
Ctest IF(Q2.LT.2.098) GOTO 400
      IF(LX.GT.0) GOTO 500
      IF(LST(19).GE.0.AND.LST(19).LT.10.AND.X.GT.1.-W2/WMAX**2) LX=LX+1
      CALL LEPTO
      PQCOM=PARI(31)*PQ(17)*COMFAC
      PARL(25)=ULALPS(Q2)
      PARI(20)=PQ(17)
      XTOT(IX,IW)=PQ(17)
      IF(LST(20).LE.1) THEN
        PARL(27)=MAX(PARL(9)**2/W2,PARL(8))
        P27MAX=1.0
      ELSEIF(LST(20).EQ.2) THEN
        PARL(27)=MAX(PARL(9)**2/Q2,PARL(8))
        P27MAX=W2/Q2
      ELSEIF(LST(20).EQ.3.OR.LST(20).EQ.4) THEN
        PARL(27)=PARL(8)
        P27MAX=0.5
      ELSEIF(LST(20).EQ.5) THEN
        PARL(27)=PARL(8)
        P27MAX=0.5
      ELSEIF(LST(20).EQ.6) THEN
        PARL(27)=PARL(9)
        P27MAX=W2
      ENDIF

      ZOOM=.FALSE.
      YCMIN=PARL(27)
      YCMAX=PARL(27)
      IYCUT=0
  100 IYCUT=IYCUT+1
      RQG=0.
      RQQB=0.
CAE.Scheme for ME cutoff: W2, Q2, mixed
      IF(LST(20).LE.1) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*(1.D0-DBLE(X))*DBLE(PARL(27)))
        XPMAX=DBLE(X)/(DBLE(X)+(1.D0-DBLE(X))*DBLE(PARL(27)))
      ELSEIF(LST(20).EQ.2) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*DBLE(X)*DBLE(PARL(27)))
        XPMAX=1.D0/(1.D0+DBLE(PARL(27)))
      ELSEIF(LST(20).EQ.3.OR.LST(20).EQ.4) THEN
        XPMIN=X
        XPMAX=1.D0/(1.D0+DBLE(PARL(9)))
      ELSEIF(LST(20).EQ.5) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(9))
      ELSEIF(LST(20).EQ.6) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(27))
      ELSE
        WRITE(6,*) 'LWEITS: No such jet scheme!'
      ENDIF
CAE
      IF(XPMIN.GE.XPMAX.OR.XPMIN.LE.0.) GOTO 210
      DO 200 IP=1,NP
      IF(LST(17).EQ.0) THEN
        PARI(15)=0.
        PARI(16)=0.
        PARI(18)=0.
        PARI(19)=0.
      ELSE
        PARI(14+IP)=0.
        IF(IP.LE.2) PARI(17+IP)=0.
      ENDIF
      LST(32)=IP
      LST(24)=2
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RESULT)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RESULT)
      RQG=RQG+RESULT
      PQG(IX,IW,IP)=RESULT/PARL(25)
      IF(LST(17).EQ.0) THEN
        QGMAX(IX,IW,1)=PARI(15)
        QGMAX(IX,IW,2)=PARI(16)
      ELSE
        PQG(IX,IW,IP)=RESULT*PARI(20)/PARI(23+IP)/PARL(25)
        QGMAX(IX,IW,IP)=PARI(14+IP)
      ENDIF
      IF(IP.EQ.3) GOTO 200
      LST(24)=3
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RESULT)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RESULT)
      RQQB=RQQB+RESULT
      PQQB(IX,IW,IP)=RESULT/PARL(25)
      IF(LST(17).EQ.0) THEN
        QQBMAX(IX,IW,1)=PARI(18)
        QQBMAX(IX,IW,2)=PARI(19)
      ELSE
        PQQB(IX,IW,IP)=RESULT*PARI(20)/PARI(23+IP)/PARL(25)
        QQBMAX(IX,IW,IP)=PARI(17+IP)
      ENDIF
  200 CONTINUE
  210 CONTINUE
      RQ=1.-RQG-RQQB
*HI>>
      IF(RQ.lt.-20)  RQ=-20
*HI<<      
      IF(.NOT.ZOOM) THEN
CAE.First find interval so that RQ>0
        IF(RQ.LT.0.AND.IYCUT.LT.10) THEN
          PARL(27)=MIN(1.1*EXP(-2.0*RQ)*PARL(27),P27MAX)
          YCMIN=YCMAX
          YCMAX=PARL(27)
        ELSEIF(RQ.LT.0.AND.IYCUT.GE.10) THEN
C...Terminate procedure after some iterations
          WRITE(6,*) 'Warning! sigma>tot for x,q2,cut=',X,Q2,PARL(27)
          WRITE(6,*) 'Weights=',RQ,RQG,RQQB
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/RTOT
          RQQB=RQQB/RTOT
          RQ=1.-RQG-RQQB
          DO 220 IP=1,3
            PQG(IX,IW,IP)=PQG(IX,IW,IP)/RTOT
            QGMAX(IX,IW,IP)=QGMAX(IX,IW,IP)/RTOT
220       CONTINUE
          DO 230 IP=1,2
            PQQB(IX,IW,IP)=PQQB(IX,IW,IP)/RTOT
            QQBMAX(IX,IW,IP)=QQBMAX(IX,IW,IP)/RTOT
230       CONTINUE
C...Break loop
          GOTO 250
        ELSEIF(IYCUT.GE.2.AND.RQ.GT.PARL(13)) THEN
C...If RQ>PARL(13), then ycut was increased to much
          ZOOM=.TRUE.
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSE
C...correct ycut found
          GOTO 250
        ENDIF
      ELSE
C...Zoom in on ycut so that 0<RQ<PARL(13)
        IF(RQ.LT.0.AND.IYCUT.LT.100) THEN
          YCMIN=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSEIF(RQ.GT.PARL(13).AND.IYCUT.LT.100) THEN
          YCMAX=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSEIF(IYCUT.GE.100) THEN
          IF(LST(3).GE.1) THEN
            WRITE(6,*) 'LWEITS: Warning, PARL(27) not found.'
          ENDIF
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/MAX(1.0,RTOT)
          RQQB=RQQB/MAX(1.0,RTOT)
          RQ=1.-RQG-RQQB
C...Break loop
          GOTO 250        
        ELSE
C...ycut found, break loop
          GOTO 250
        ENDIF
      ENDIF
C...Loop until correct weights found
      GOTO 100

 250  CONTINUE
CAE
      YCUT(IX,IW)=PARL(27)
      IF(LST(33).EQ.-91) THEN
Ctest...Include 3-jet cross section in denominator
        QTOT=1.+RQG+RQQB
        RQG =RQG/QTOT
        RQQB=RQQB/QTOT
        RQ=1.-RQG-RQQB
      ENDIF
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) THEN
        IF(LST(19).LT.10) WRITE(6,1200) W,X,Y,Q2,PARL(25),PQCOM,
     &  PARL(27),IYCUT,RQ,RQG,RQQB,(QGMAX(IX,IW,IP),IP=1,IPMAX),
     &  (QQBMAX(IX,IW,IP),IP=1,MIN(2,IPMAX))
        IF(LST(19).GE.10) WRITE(6,2200) X,Y,Q2,W,PARL(25),PQCOM,
     &  PARL(27),IYCUT,RQ,RQG,RQQB,(QGMAX(IX,IW,IP),IP=1,IPMAX),
     &  (QQBMAX(IX,IW,IP),IP=1,MIN(2,IPMAX))
      ENDIF
  400 CONTINUE
  500 CONTINUE
  600 CONTINUE

      LST(2)=LST2
      IF(LFILE.LT.0) THEN
C...Write results on logical file number IABS(LFILE)
        WRITE(IABS(LFILE)) LST,PARL,NXX,NWW,NP,XX,WW
        WRITE(IABS(LFILE))(((PQG(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((PQQB(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((QGMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,IPMAX),
     &  (((QQBMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,MIN(2,IPMAX)),
     &  YCUT
        IF(NP.NE.1) WRITE(IABS(LFILE)) XTOT
        CLOSE(IABS(LFILE))
      ENDIF
      RETURN

 1000 FORMAT('1',/,5X,'Integration of 1st order QCD matrix elements',
     &           /,5X,'============================================',
     &/,' for gluon radiation (qg-event) and boson-gluon fusion ',
     &'(qq-event) probability.',
     & 
     & 
     &F6.3,2I5, 
     &/,25X,' QCD lambda, PARU(112) =',F6.3,' GeV',
     & 
     &/,' PARL(8), PARL(9), PARL(12), PARL(13) =',4F8.4,/)
 1010 FORMAT(' Lepton energy not allowed to vary in simulation.',/)
 1020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' y in table below calculated assuming max energy.',/)
 1030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, W =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' W-values in array WW:',/,10F7.1,/,11F7.1,/)
 1040 FORMAT(' Warning: max W outside grid, Wmax, grid-max =',2F12.1)
 1100 FORMAT( 
     &9X,'cut',' it',2X,'q-event',1X,'qg-event',
     &1X,'qq-event',' max of matrix elements qg & qq; L,R or T,S,I',
     &/,1X,132(1H-),/)
 1200 FORMAT(F7.1,2F8.4,1PG10.3,0PF6.2,1PG11.3,0PF8.4,I3,3F9.4,1P,5E9.2)
 1900 FORMAT(' Execution stopped ',/)
 2020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' W in table below calculated assuming max energy.',/)
 2030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, y =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' y-values in array WW:',/,10F7.4,/,11F7.4,/)
 2100 FORMAT( 
     &9X,'cut',' it',2X,'q-event',1X,'qg-event',
     &1X,'qq-event',' max of matrix elements qg & qq; L,R or T,S,I',
     &/,1X,132(1H-),/)
 2200 FORMAT(2F8.5,1PG10.3,0PF7.1,F6.2,1PG11.3,0PF8.4,I3,3F9.4,1P,5E9.2)
      END

C **********************************************************************

      SUBROUTINE LPRWTS(NSTEP)

      IMPLICIT NONE

C...Prints probabilities for q-, qg- and qqbar-events using the present
C...QCD weights stored in common block LGRID.


      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/


      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/


      INTEGER NSTEP,LW,IW,LX,IX,IP
      REAL WMAX,W,RQG,RQQB,QTOT,RQ,ULALPS

      WMAX=SQRT(PARL(21)+PSAVE(3,1,5)**2+PSAVE(3,2,5)**2)
      WRITE(6,1000) PARL(11),LST(13),MSTU(112),PARU(112),
     &PARL(8),PARL(9),PARL(12),PARL(13)
      IF(NP.EQ.1) THEN
        WRITE(6,1010)
      ELSE
        IF(LST(19).LT.10) WRITE(6,1020)
        IF(LST(19).GE.10) WRITE(6,2020)
      ENDIF
      IF(LST(19).LT.10) THEN
        WRITE(6,1030) LST(19),NXX,NWW,XX,WW
        IF(WMAX.GT.WW(NWW)) WRITE(6,1040) WMAX,WW(NWW)
        WRITE(6,1100)
      ELSEIF(LST(19).GE.10) THEN
        WRITE(6,2030) LST(19),NXX,NWW,XX,WW
        WRITE(6,2100)
      ENDIF

      LW=0
      DO 500 IW=1,NWW,MAX(1,NSTEP)
      W=WW(IW)
      Y=WW(IW)
      IF(LW.GT.0) GOTO 600
      IF(LST(19).LT.10.AND.W.GT.WMAX) LW=LW+1
      W2=W**2
      LX=0
      DO 400 IX=1,NXX,MAX(1,NSTEP)
      X=XX(IX)
      IF(LX.GT.0) GOTO 500
      IF(LST(19).LT.10) THEN
C...x,W2 given.
        U=(W2-PSAVE(3,2,5)**2)/(2.*PSAVE(3,2,5)*(1.-X))
        Q2=2.*PSAVE(3,2,5)*U*X
        Y=Q2/(PARL(21)*X)
      ELSEIF(LST(19).GE.10) THEN
C...x,y given.
        PARL(22)=Y*PARL(21)
        Q2=X*PARL(22)
        U=PARL(22)/(2.*PSAVE(3,2,5))
        W2=PARL(22)*(1.-X)+PSAVE(3,2,5)**2
      ENDIF
      PARI(24)=(1.+(1.-Y)**2)/2.
      PARI(25)=1.-Y
      PARI(26)=(1.-(1.-Y)**2)/2.
      PARL(25)=ULALPS(Q2)
      IF(Y.GT.1.) LX=LX+1
      RQG=0.
      RQQB=0.
      DO 200 IP=1,NP
      IF(NP.EQ.1) THEN
        RQG=PQG(IX,IW,IP)
        RQQB=PQQB(IX,IW,IP)
      ELSE
        RQG=RQG+PQG(IX,IW,IP)*PARI(23+IP)/XTOT(IX,IW)
        IF(IP.LT.3) RQQB=RQQB+PQQB(IX,IW,IP)*PARI(23+IP)/XTOT(IX,IW)
      ENDIF
  200 CONTINUE
C...Include alpha-strong in weight.
      RQG=RQG*PARL(25)
      RQQB=RQQB*PARL(25)
      IF(LST(33).EQ.-91) THEN
C...Include 3-jet cross section in denominator
        QTOT=1.+RQG+RQQB
        RQG =RQG/QTOT
        RQQB=RQQB/QTOT
      ENDIF
      RQ=1.-RQG-RQQB
      IF(LST(19).LT.10) THEN
        WRITE(6,1200) W,X,Y,Q2,PARL(25),YCUT(IX,IW),RQ,RQG,RQQB
      ELSEIF(LST(19).GE.10) THEN
        WRITE(6,2200) X,Y,Q2,W,PARL(25),YCUT(IX,IW),RQ,RQG,RQQB
      ENDIF
  400 CONTINUE
  500 CONTINUE
  600 CONTINUE
      RETURN

 1000 FORMAT('1',/,5X,'SUMMARY OF QCD MATRIX ELEMENT INTEGRATION',
     &           /,5X,'-----------------------------------------', 
     &/,' for gluon radiation (qg-event) and boson-gluon fusion ',
     &'(qq-event) probability.',
     & 
     & 
     &I5, 
     &'  QCD lambda, PARU(112) =',F6.3,' GeV',
     & 
     &/,' PARL(8), PARL(9), PARL(12), PARL(13) =',4F8.4,/)
 1010 FORMAT(' Lepton energy not allowed to vary in simulation.',/)
 1020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' y in table below calculated assuming max energy.',/)
 1030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, W =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' W-values in array WW:',/,10F7.1,/,11F7.1,/)
 1040 FORMAT(' Max W outside grid, execution stopped ! Wmax, grid-max ='
     &,2F12.1)
 1100 FORMAT( 
     &5X,'cut',2X,'q-event',1X,'qg-event',1X,'qq-event',
     &/,1X,77(1H-),/)
 1200 FORMAT(F7.1,2F8.4,1PG12.3,0PF8.2,F8.4,3F9.4)
 2020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' W in table below calculated assuming max energy.',/)
 2030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, y =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' y-values in array WW:',/,10F7.4,/,11F7.4,/)
 2100 FORMAT( 
     &5X,'cut',2X,'q-event',1X,'qg-event',1X,'qq-event',
     &/,1X,77(1H-),/)
 2200 FORMAT(2F8.5,1PG12.3,0PF7.1,F8.2,F8.4,3F9.4)
      END
C **********************************************************************

      SUBROUTINE LSIGMX(NPAR,DERIV,DIFSIG,XF,IFLAG)

      IMPLICIT NONE

C...Calculates the negative of the differential cross-section.
C...In the generation procedure the maximum of the differential cross-
C...section is needed for weighting purposes. This maximum is found by
C...minimizing the negative differential cross-section using the MINUIT
C...routines which are then calling this routine.
C...More precisly, only the part of the cross-section formula which is
C...needed for the weighting procedure is included here.
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/


      INTEGER NPAR,IFLAG,NCALLS,LST2
      REAL DERIV,DIFSIG,XF,DUMMY,S,PM2,Q2LOW,Q2UPP,YLOW,YUPP,W2LOW,
     +W2UPP

      DIMENSION DERIV(30),XF(30)
      DATA NCALLS/0/

      DUMMY=NPAR+DERIV(1)
      IF(IFLAG.EQ.1) NCALLS=0
      IF(IFLAG.EQ.2) WRITE(6,1000)

      DIFSIG=1.E+12
      NCALLS=NCALLS+1
      X=XF(1)
      IF(X.LT.XMIN) THEN
        DIFSIG=(XMIN-X)**2*1.E+11 
        RETURN
      ELSEIF(X.GT.XMAX) THEN 
        DIFSIG=(X-XMAX)**2*1.E+11 
        RETURN
      ENDIF
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      Q2LOW=MAX(Q2MIN,X*YMIN*S,(W2MIN-PM2)*X/(1.-X))
      Q2UPP=MIN(Q2MAX,X*YMAX*S,(W2MAX-PM2)*X/(1.-X))
      YLOW=MAX(YMIN,Q2MIN/(S*X),(W2MIN-PM2)/(S*(1.-X)))
      YUPP=MIN(YMAX,Q2MAX/(S*X),(W2MAX-PM2)/(S*(1.-X)))
      W2LOW=MAX(W2MIN,(1.-X)*YMIN*S+PM2,Q2MIN*(1.-X)/X+PM2)
      W2UPP=MIN(W2MAX,(1.-X)*YMAX*S+PM2,Q2MAX*(1.-X)/X+PM2)
      IF(LST(31).EQ.1) THEN
        Q2=XF(2)
        IF(Q2.LT.Q2LOW) THEN
          DIFSIG=(Q2LOW-Q2)**2*1.E+11 
          RETURN
        ELSEIF(Q2.GT.Q2UPP) THEN 
          DIFSIG=(Q2-Q2UPP)**2*1.E+11 
          RETURN
        ENDIF
        Y=Q2/(PARL(21)*X)
        W2=(1.-X)*Y*PARL(21)+PM2
      ELSEIF(LST(31).EQ.2) THEN
        Y=XF(2)
        IF(Y.LT.YLOW) THEN
          DIFSIG=(YLOW-Y)**2*1.E+11 
          RETURN
        ELSEIF(Y.GT.YUPP) THEN 
          DIFSIG=(Y-YUPP)**2*1.E+11 
          RETURN
        ENDIF
        Q2=Y*X*PARL(21)
        W2=(1.-X)*Y*PARL(21)+PM2
      ELSEIF(LST(31).EQ.3) THEN
        W2=XF(2)
        IF(W2.LT.W2LOW) THEN
          DIFSIG=(W2LOW-W2)**2*1.E+11 
          RETURN
        ELSEIF(W2.GT.W2UPP) THEN 
          DIFSIG=(W2-W2UPP)**2*1.E+11 
          RETURN
        ENDIF
        Y=(W2-PM2)/((1.-X)*PARL(21))
        Q2=X*Y*PARL(21)
      ENDIF
      IF(Q2.LT.Q2LOW) THEN
        DIFSIG=(Q2LOW-Q2)**2*1.E+11 
        RETURN
      ELSEIF(Q2.GT.Q2UPP) THEN 
        DIFSIG=(Q2-Q2UPP)**2*1.E+11 
        RETURN
      ENDIF
      IF(Y.LT.YLOW) THEN
        DIFSIG=(YLOW-Y)**2*1.E+11 
        RETURN
      ELSEIF(Y.GT.YUPP) THEN 
        DIFSIG=(Y-YUPP)**2*1.E+11 
        RETURN
      ENDIF
      IF(W2.LT.W2LOW) THEN
        DIFSIG=(W2LOW-W2)**2*1.E+11 
        RETURN
      ELSEIF(W2.GT.W2UPP) THEN 
        DIFSIG=(W2-W2UPP)**2*1.E+11 
        RETURN
      ENDIF
      LST2=LST(2)
      LST(2)=-1
      CALL LEPTO
      LST(2)=LST2
      DIFSIG=0. 
      IF(LST(21).NE.0) RETURN
      DIFSIG=-PQ(17)*COMFAC

      IF(LST(3).GE.4.AND.IFLAG.EQ.3)
     &WRITE(6,1100) NCALLS,DIFSIG,X,Y,Q2,W2
      RETURN

 1000 FORMAT(' Warning: IFLAG = 2 in call to LSIGMX, which does not '
     &,'calculate derivatives.')
 1100 FORMAT(/,5X,'Terminating entry in LSIGMX after ',I5,' calls.',/,
     &5X,'Best estimate of minimum found to be ',E12.4,/,
     &5X,'located at x, y, Q**2, W**2 = ',4G10.3,/)

      END

C **********************************************************************

      SUBROUTINE LXSECT

      IMPLICIT NONE

C...Integrate differential cross-section using GADAP, RIWIAD or DIVONNE

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

* this Common block appears only in Subroutine LXSECT 
      COMMON /PARAMS/ ACC,NDIM,NSUB,ITER
      DOUBLE PRECISION ACC
      INTEGER NDIM,NSUB,ITER
      SAVE /PARAMS/

* this Common block appears only in Subroutine LXSECT 
      COMMON /ANSWER/ VALUE,ERRIW
      DOUBLE PRECISION VALUE,ERRIW
      SAVE /ANSWER/ 

* this Common block appears only in Subroutine LXSECT 
      COMMON /BNDLMT/ FLOW,FHIGH
      DOUBLE PRECISION FLOW,FHIGH
      SAVE /BNDLMT/

* this Common block appears only in Subroutine LXSECT
      COMMON /SAMPLE/ NPOINT
      INTEGER NPOINT
      SAVE /SAMPLE/


      INTEGER NCALL,I,MAXNUM,MAXPTS,NPT,IT,JDEG
      REAL XMINUS,XPLUS,TI1,SIGMA,EPS,SPRDMX,ACCUR,TI2,ERREST
      DIMENSION XMINUS(2),XPLUS(2)
      EXTERNAL DCROSS,DLOWER,DUPPER,RIWFUN
      DATA NCALL/0/

      NCALL=NCALL+1
      CALL LTIMEX(TI1)
      NTOT=0
      NPASS=0
      SIGMA=0.
      ERREST=0.
      NDIM=2
C...Parameters for RIWIAD integration.
      ACC=PARL(15)
      NSUB=100
      ITER=100
C...Parameters for DIVON integration.
      DO 20 I=1,2
      XMINUS(I)=0.
   20 XPLUS(I)=1.
      EPS=PARL(15)
      MAXNUM=50000
      FLOW=-1.D0
      FHIGH=1.D+20
      NPOINT=100
C...Additional parameters for detailed DIVON integration.
      SPRDMX=2.
      MAXPTS=50000
      JDEG=0
      NPT=1000

      SIGMA=0.
      ERREST=0.
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1000)
      IF(LST(10).EQ.1) THEN
C...Integration using GADAP.
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1100)
        ACCUR=PARL(15)
        IT=0
  100   IT=IT+1
        ERREST=ACCUR
        CALL GADAP2(XMIN,XMAX,DLOWER,DUPPER,DCROSS,ERREST,SIGMA)
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1110) IT,NTOT,NPASS,SIGMA
        IF(SIGMA.GT.1.) THEN
          IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &    WRITE(6,1120) ACCUR
        ELSE
          IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &    WRITE(6,1130) ACCUR,ACCUR/MAX(1.E-22,SIGMA),PARL(15)
          ACCUR=MAX(1.E-22,SIGMA*PARL(15))
          IF(IT.LT.2) GOTO 100
        ENDIF
      ELSEIF(LST(10).EQ.2) THEN
C...Integration using RIWIAD. When RIWIAD cannot be loaded:
C...activate next two lines and deactivate RIWIAD call.
       WRITE(6,*) ' RIWIAD not available, execution stopped.'
       STOP
C.HI        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
C.HI     &  WRITE(6,1200) SNGL(ACC),NSUB,ITER
C        CALL RIWIAD(RIWFUN)
C.HI        SIGMA=SNGL(VALUE)
C.HI        ERREST=SNGL(ERRIW)
      ELSEIF(LST(10).EQ.3) THEN
C...Integration using simple DIVONNE. When DIVONNE cannot be loaded:
C...activate next two lines and deactivate DIVONNE call.
       WRITE(6,*) ' DIVONNE not available, execution stopped.'
       STOP
C.HI        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
C.HI     &  WRITE(6,1300) EPS,MAXNUM,SNGL(FLOW),SNGL(FHIGH),NPOINT
C        CALL DIVON(NDIM,XMINUS,XPLUS,EPS,MAXNUM,SIGMA,ERREST)
      ELSEIF(LST(10).EQ.4) THEN
C...Integration using detailed DIVONNE. When DIVONNE cannot be loaded:
C...activate next two lines and deactivate PARTN and INTGRL calls.
       WRITE(6,*) ' DIVONNE not available, execution stopped.'
       STOP
C.HI        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
C.HI     &  WRITE(6,1400) EPS,MAXNUM,
C.HI     &  SNGL(FLOW),SNGL(FHIGH),NPOINT,SPRDMX,MAXPTS,JDEG,NPT
C        CALL PARTN(NDIM,XMINUS,XPLUS,SPRDMX,MAXPTS)
C        CALL INTGRL(NDIM,JDEG,NPT,SIGMA,ERREST)
      ELSE
        IF(LST(3).GE.1) WRITE(6,*) ' Warning: LST(10) = ',LST(10),
     &  ' not allowed.'
      ENDIF
      CALL LTIMEX(TI2)
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)
     &.OR.(LST(3).GE.1.AND.NPASS.EQ.0)) THEN
        WRITE(6,1500) SIGMA,ERREST,NTOT,NPASS,TI2-TI1
        IF(LST(3).GE.1.AND.NPASS.EQ.0) WRITE(6,1600)
      ENDIF
      PARL(23)=SIGMA

      RETURN
 1000 FORMAT(/,' Integration of cross section:',/,1X,28('-'))
 1100 FORMAT(5X,'using GADAP = adaptive Gaussian integration')
 1110 FORMAT(5X,'Iteration #',I3,/,
     &10X,'# function evaluations; total & non-zero =',2I8,/,
     &10X,'sigma =',G10.2,' pb')
 1120 FORMAT(10X,'required  relative error = ',G10.2)
 1130 FORMAT(10X,'effective absolute error = ',G10.2,/,
     &       10X,'effective relative error = ',G10.2,/,
     &       10X,'required  relative error = ',G10.2)
 1200 FORMAT(5X,'using RIWIAD with parameters: rel. acc. = ',F10.4,
     &/,5X,'# of subvolumes = ',I5,5X,'max # iterations = ',I5)
 1300 FORMAT(5X,'using automatic DIVONNE with parameters: ',
     &'rel. acc. = ',F10.4,/,5X,'max # function calls = ',I5,
     &/,5X,'lower and upper bound on integrand =',2E12.4,
     &/,5X,'# sample points/region =',I5)
 1400 FORMAT(5X,'using detailed DIVONNE with parameters: ',
     &'rel. acc. = ',F10.4,/,5X,'max # function calls = ',I5,
     &/,5X,'lower and upper bound on integrand =',2E12.4,
     &/,5X,'# sample points/region =',I5,
     &/,5X,'SPRDMX, MAXPTS, JDEG, NPT =',F5.2,3I10)
 1500 FORMAT(/,' ===> Cross-section =',1P,G12.3,
     &' pb,  error estimate = ',G12.3,/,
     &6X,'# of integrand evaluations; total & non-zero =',2I8,/,
     &6X,'cpu time for integration =',G12.3,' seconds',/)
 1600 FORMAT(' Warning: integrand always zero, probably no allowed',
     &' phase space due to cuts',/,
     &10X,'check, in particular, CUT(11) to CUT(14)')
      END

C **********************************************************************

      SUBROUTINE RIWIBD

      IMPLICIT NONE
      
C   BLOCK DATA SUBSTITUTE from RIWIAD
**HI      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/STORE/XA(11),XB(11),XC(11),XD(11),MA(11),MB(11),MC(11)
      DOUBLE PRECISION XA,XB,XC,XD
      INTEGER MA,MB,MC
      COMMON/STORE1/R(10000),LR
      DOUBLE PRECISION R
      INTEGER LR
      COMMON/OPTION/IPRRIW,ICONV,IRESET
      INTEGER IPRRIW,ICONV,IRESET 
      COMMON/RANDOM/NSHOTS
      INTEGER NSHOTS
      COMMON/INTERN/FACTOR,ALFA,BETA,GAMMA,DELTA,LEVEL,NMIN
      INTEGER LEVEL,NMIN
      DOUBLE PRECISION FACTOR,ALFA,BETA,GAMMA,DELTA

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      INTEGER INIT
      DATA INIT/0/

      IF(INIT.EQ.1) RETURN
      INIT=1
      MA(1)=0
      LR=10000
      ICONV=1
      IRESET=0
      NSHOTS=2
      FACTOR=1.65
      LEVEL=90
      ALFA=0.3
      BETA=0.3
      GAMMA=0.3
      DELTA=.7
      NMIN=2
C...Print flag to be changed here.
      IPRRIW=0
      IF(LST3.GE.4) WRITE(6,1000) IPRRIW
      RETURN
 1000 FORMAT(5X,'RIWIAD print flag changed: IPRRIW =',I5)
      END

C **********************************************************************

      SUBROUTINE DVNOPT

      IMPLICIT NONE

C...Change of default options in DIVONNE

      COMMON /PRINT/ IPRDIV
      INTEGER IPRDIV

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/


      IPRDIV=0
      IF(LST3.GE.2) IPRDIV=1000
      IF(LST3.GE.4) IPRDIV=10
      IF(LST3.GE.4) WRITE(6,1000) IPRDIV
      RETURN
 1000 FORMAT(5X,'DIVON4 print flag changed: IPRDIV =',I5)
      END

C **********************************************************************

      DOUBLE PRECISION FUNCTION DFUN(NDIM,X)
      IMPLICIT NONE
      INTEGER NDIM
      DOUBLE PRECISION X(NDIM),RIWFUN

      DFUN=RIWFUN(X)
      RETURN
      END

C **********************************************************************

      DOUBLE PRECISION FUNCTION RIWFUN(V)
      
      IMPLICIT NONE
      
      DOUBLE PRECISION V(2)
      
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/


      REAL V2MIN,V2MAX,V1MIN,V1MAX,V1,V2,DCROSS
      DATA V2MIN,V2MAX/2*0./

      RIWFUN=0.D0
      V1MIN=XMIN
      V1MAX=XMAX
      IF(LST(31).EQ.1) THEN
        V2MIN=Q2MIN
        V2MAX=Q2MAX
      ELSEIF(LST(31).EQ.2) THEN
        V2MIN=YMIN
        V2MAX=YMAX
      ELSEIF(LST(31).EQ.3) THEN
        V2MIN=W2MIN
        V2MAX=W2MAX
      ENDIF
      V1=V1MIN+V(1)*(V1MAX-V1MIN)
      V2=V2MIN+V(2)*(V2MAX-V2MIN)
      RIWFUN=DCROSS(V1,V2)*(V1MAX-V1MIN)*(V2MAX-V2MIN)

      RETURN
      END

C **********************************************************************

      FUNCTION DCROSS(V1,V2)
      
      IMPLICIT NONE
      
C...Differential cross-section dsigma/dv1dv2; v1=x, v2=Q2 or y or W2.
C...Used for numerical integration etc.
C...Note, non-zero result only for region defined by cuts through CUT.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/


      INTEGER LST2
      REAL V1,V2,DCROSS,S,PM2,Q2LOW,Q2UPP,YLOW,YUPP,W2LOW,W2UPP

      DCROSS=0.
      NTOT=NTOT+1
C...Variable V1 is x, variable V2 is either Q**2, y or W**2
      X=V1
      IF(X.LT.XMIN.OR.X.GT.XMAX) RETURN
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      IF(LST(31).EQ.1) THEN
        Q2=V2
        Y=Q2/(PARL(21)*X)
        W2=(1.-X)*Y*PARL(21)+PSAVE(3,2,5)**2
      ELSEIF(LST(31).EQ.2) THEN
        Y=V2
        Q2=Y*X*PARL(21)
        W2=(1.-X)*Y*PARL(21)+PSAVE(3,2,5)**2
      ELSEIF(LST(31).EQ.3) THEN
        W2=V2
        Y=(W2-PSAVE(3,2,5)**2)/((1.-X)*PARL(21))
        Q2=X*Y*PARL(21)
      ENDIF
      Q2LOW=MAX(Q2MIN,X*YMIN*S,(W2MIN-PM2)*X/(1.-X))
      Q2UPP=MIN(Q2MAX,X*YMAX*S,(W2MAX-PM2)*X/(1.-X))
      YLOW=MAX(YMIN,Q2MIN/(S*X),(W2MIN-PM2)/(S*(1.-X)))
      YUPP=MIN(YMAX,Q2MAX/(S*X),(W2MAX-PM2)/(S*(1.-X)))
      W2LOW=MAX(W2MIN,(1.-X)*YMIN*S+PM2,Q2MIN*(1.-X)/X+PM2)
      W2UPP=MIN(W2MAX,(1.-X)*YMAX*S+PM2,Q2MAX*(1.-X)/X+PM2)
      IF(Q2.LT.Q2LOW.OR.Q2.GT.Q2UPP) RETURN
      IF(Y.LT.YLOW.OR.Y.GT.YUPP) RETURN
      IF(W2.LT.W2LOW.OR.W2.GT.W2UPP) RETURN
      LST2=LST(2)
      LST(2)=-2
      CALL LEPTO
      LST(2)=LST2
      IF(LST(21).NE.0) RETURN
      NPASS=NPASS+1
      DCROSS=PARI(31)*PQ(17)*COMFAC

      RETURN
      END

C **********************************************************************

      FUNCTION DLOWER(V1)

C...Lower limit on second variable (y, Q**2 or W**2) depending on first
C...variable x=V1. Used for integrating differential cross-section.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      REAL DLOWER,V1,S,PM2

C...CMS energy squared and target nucleon mass.
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      IF(LST(31).EQ.1) THEN
        DLOWER=MAX(Q2MIN,V1*YMIN*S,(W2MIN-PM2)*V1/MAX(1.-V1,1.E-22))
      ELSEIF(LST(31).EQ.2) THEN
        DLOWER=MAX(YMIN,Q2MIN/(S*V1),(W2MIN-PM2)/MAX(S*(1.-V1),1.E-22))
      ELSEIF(LST(31).EQ.3) THEN
        DLOWER=MAX(W2MIN,(1.-V1)*YMIN*S+PM2,
     &  Q2MIN*(1.-V1)/MAX(V1,1.E-22)+PM2)
      ENDIF
      RETURN
      END

C **********************************************************************

      FUNCTION DUPPER(V1)
      
      IMPLICIT NONE
      
C...Upper limit on second variable (y, Q**2 or W**2) depending on first
C...variable x=V1. Used for integrating differential cross-section.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      REAL DUPPER,V1,S,PM2
      
C...CMS energy squared and target nucleon mass.
      S=PARL(21)
      PM2=PSAVE(3,2,5)**2
      IF(LST(31).EQ.1) THEN
        DUPPER=MIN(Q2MAX,V1*YMAX*S,(W2MAX-PM2)*V1/MAX(1.-V1,1.E-22))
      ELSEIF(LST(31).EQ.2) THEN
        DUPPER=MIN(YMAX,Q2MAX/(S*V1),(W2MAX-PM2)/MAX(S*(1.-V1),1.E-22))
      ELSEIF(LST(31).EQ.3) THEN
        DUPPER=MIN(W2MAX,(1.-V1)*YMAX*S+PM2,
     &  Q2MAX*(1.-V1)/MAX(V1,1.E-22)+PM2)
      ENDIF
      RETURN
      END

C **********************************************************************

      SUBROUTINE FLTABL

      IMPLICIT NONE

C...Integrates the longitudinal structure function, store on grid
C...  in x, Q**2.
      
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/

      COMMON /FLGRID/ NFX,NFQ,XR(2),QR(2),FLQT(41,16),FLGT(41,16),
     &FLMT(41,16)
      REAL    XR,QR,FLQT,FLGT,FLMT
      INTEGER NFX,NFQ
      SAVE /FLGRID/


      INTEGER LQCD,LTM,LHT,IX,IQ,LQ,IT
      REAL T1,FLQ,ACCUR,EPS,T2,FLG,FLM
      real ULALPS
      
      EXTERNAL FLQINT,FLGINT,FLTINT

      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      LHT=LST(11)/100
      IF(LST(3).GE.3) WRITE(6,1000) LST(11),LQCD,LTM,LHT
      IF(LQCD.LT.1.AND.LTM.LT.1) RETURN
      CALL LTIMEX(T1)
      DO 10 IX=1,NFX
      DO 10 IQ=1,NFQ
      FLQT(IX,IQ)=0.
      FLGT(IX,IQ)=0.
   10 FLMT(IX,IQ)=0.
      QR(1)=Q2MIN
      XR(1)=XMIN
      XR(2)=XMAX
      DO 500 IX=1,NFX
      X=10**(ALOG10(XR(1))+(ALOG10(XR(2))-ALOG10(XR(1)))*(IX-1)/(NFX-1))
      QR(2)=X*PARL(21)
      IF(QR(1).GT.QR(2)) GOTO 500
      LQ=0
      DO 400 IQ=1,NFQ
      Q2=10**(ALOG10(QR(1))+(ALOG10(QR(2))-ALOG10(QR(1)))*
     &(IQ-1)/(NFQ-1))
Ctest IF(LQ.GT.0) GOTO 500
      IF(Q2.GT.PARL(21)) LQ=LQ+1
      Y=Q2/(PARL(21)*X)
      IF(Y.LT.0.0.OR.Y.GT.1.0) LQ=LQ+1
      PARL(25)=ULALPS(Q2)
      IF(LQCD.EQ.1) THEN
C...Quark part.
        ACCUR=PARL(11)
        IT=0
  100   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLQINT,EPS,FLQ)
        IF(FLQ.LT.1) THEN
          ACCUR=FLQ*PARL(11)
          IF(IT.LT.2) GOTO 100
        ENDIF
        FLQT(IX,IQ)=FLQ
C...Gluon part.
        ACCUR=PARL(11)
        IT=0
  200   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLGINT,EPS,FLG)
        IF(FLG.LT.1.) THEN
          ACCUR=FLG*PARL(11)
          IF(IT.LT.2) GOTO 200
        ENDIF
        FLGT(IX,IQ)=FLG
      ENDIF
      IF(LTM.EQ.1) THEN
C...Target mass  part.
        ACCUR=PARL(11)
        IT=0
  300   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLTINT,EPS,FLM)
        IF(FLM.LT.1) THEN
          ACCUR=FLM*PARL(11)
          IF(IT.LT.2) GOTO 300
        ENDIF
        FLMT(IX,IQ)=FLM
      ENDIF
  400 CONTINUE
  500 CONTINUE
  600 CONTINUE
      CALL LTIMEX(T2)
      IF(LST(3).GE.3) WRITE(6,1100) T2-T1
      RETURN

 1000 FORMAT(' Initialisation for FL; QCD, target mass, higher twist: ',
     &/,' LST(11) =',I5,' --> LQCD, LTM, LHT =',3I3)
 1100 FORMAT(' FL integrations performed if LQCD=1 and/or LTM=1, ',
     &'results on grid.'/,' Time for FL integrations is ',F7.1,' sec.')
      END

C **********************************************************************

      SUBROUTINE FLIPOL(FLQ,FLG,FLM)

      IMPLICIT NONE

C...QCD and target mass contributions to longitudinal structure function
C...from interpolation on x,Q2 grid.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /FLGRID/ NFX,NFQ,XR(2),QR(2),FLQT(41,16),FLGT(41,16),
     &FLMT(41,16)
      REAL    XR,QR,FLQT,FLGT,FLMT
      INTEGER NFX,NFQ
      SAVE /FLGRID/


      INTEGER NOUT,NWARN,LQCD,LTM,LHT,IX,IQ
      REAL FLQ,FLG,FLM,XP,Q2P,Q2L,Q2H,XL,XH,QD,XD,X1P,X2P
      
      DATA NOUT/0/,NWARN/10/

      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      LHT=LST(11)/100
      XP=X
      Q2P=Q2
C...NOTE: tiny mismatch between present x-value and those on grid.
      QR(2)=X*PARL(21)
      IF(QR(1).GT.QR(2)) RETURN
      IF(X.LT.XR(1).OR.X.GT.XR(2).OR.
     &Q2.LT.QR(1).OR.Q2.GT.QR(2)) THEN
C...x and/or Q2 outside grid limits, write warning for first NWARN cases
        IF(LST(2).GE.0) THEN
          NOUT=NOUT+1
          IF(LST(3).GE.1.AND.NOUT.LE.NWARN) WRITE(6,1000) X,Q2,NWARN
        ENDIF
        IF(X.LT.XR(1)) XP=XR(1)
        IF(X.GT.XR(2)) XP=XR(2)
        IF(Q2.LT.QR(1)) Q2P=QR(1)
        IF(Q2.GT.QR(2)) Q2P=QR(2)
      ENDIF

      IX=(ALOG10(XP)-ALOG10(XR(1)))/
     &(ALOG10(XR(2))-ALOG10(XR(1)))*(NFX-1)+1
      IQ=(ALOG10(Q2P)-ALOG10(QR(1)))/
     &(ALOG10(QR(2))-ALOG10(QR(1)))*(NFQ-1)+1
      IX=MIN(IX,NFX-1)
      IQ=MIN(IQ,NFQ-1)
      Q2L=10**(ALOG10(QR(1))+(ALOG10(QR(2))-ALOG10(QR(1)))*
     &(IQ-1)/(NFQ-1))
      Q2H=10**(ALOG10(QR(1))+(ALOG10(QR(2))-ALOG10(QR(1)))*
     &(IQ  )/(NFQ-1))
      XL=10**(ALOG10(XR(1))+(ALOG10(XR(2))-ALOG10(XR(1)))*
     &(IX-1)/(NFX-1))
      XH=10**(ALOG10(XR(1))+(ALOG10(XR(2))-ALOG10(XR(1)))*
     &(IX  )/(NFX-1))
      QD=(Q2P-Q2L)/(Q2H-Q2L)
      XD=(XP-XL)/(XH-XL)

      IF(LQCD.EQ.1) THEN
        X1P=(FLQT(IX+1,IQ)-FLQT(IX,IQ))*XD+FLQT(IX,IQ)
        X2P=(FLQT(IX+1,IQ+1)-FLQT(IX,IQ+1))*XD+FLQT(IX,IQ+1)
        FLQ=(X2P-X1P)*QD+X1P
        X1P=(FLGT(IX+1,IQ)-FLGT(IX,IQ))*XD+FLGT(IX,IQ)
        X2P=(FLGT(IX+1,IQ+1)-FLGT(IX,IQ+1))*XD+FLGT(IX,IQ+1)
        FLG=(X2P-X1P)*QD+X1P
      ENDIF
      IF(LTM.EQ.1) THEN
        X1P=(FLMT(IX+1,IQ)-FLMT(IX,IQ))*XD+FLMT(IX,IQ)
        X2P=(FLMT(IX+1,IQ+1)-FLMT(IX,IQ+1))*XD+FLMT(IX,IQ+1)
        FLM=(X2P-X1P)*QD+X1P
      ENDIF

      RETURN
 1000 FORMAT(' Warning: x=',F7.4,' or Q2=',F6.1,' outside grid,',
     &' for FL interpolation',/,10X,'value on grid limit used.',
     &' Only first',I5,' warnings printed.',/)
      END

C **********************************************************************

      SUBROUTINE FLINTG(CFLQ,CFLG,CFLM)

      IMPLICIT NONE
      
C...Event-by-event calculation of contribution to longitudinal
C...structure function from QCD and target mass effects.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/


      INTEGER LTM,LHT,IT,LQCD
      REAL FLQINT,FLGINT,FLTINT,CFLQ,CFLG,CFLM,EPS,ACCUR
      REAL ULALPS
      EXTERNAL FLQINT,FLGINT,FLTINT

      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      LHT=LST(11)/100
      PARL(25)=ULALPS(Q2)
      IF(LQCD.EQ.2) THEN
C...FL from QCD, quark and gluon contributions.
        ACCUR=PARL(11)
        IT=0
  100   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLQINT,EPS,CFLQ)
        IF(CFLQ.LT.1) THEN
          ACCUR=CFLQ*PARL(11)
          IF(IT.LT.2) GOTO 100
        ENDIF
        ACCUR=PARL(11)
        IT=0
  200   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLGINT,EPS,CFLG)
        IF(CFLG.LT.1.) THEN
          ACCUR=CFLG*PARL(11)
          IF(IT.LT.2) GOTO 200
        ENDIF
      ENDIF
      IF(LTM.EQ.2) THEN
        ACCUR=PARL(11)
        IT=0
  300   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLTINT,EPS,CFLM)
        IF(CFLM.LT.1.) THEN
          ACCUR=CFLM*PARL(11)
          IF(IT.LT.2) GOTO 300
        ENDIF
      ENDIF

      RETURN
      END

C **********************************************************************

      FUNCTION FLQINT(Z)

C...Quark contribution integrand to QCD longitudinal structure function.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/


      INTEGER I
      REAL XPQ,Z,FLQINT,PI
      DIMENSION XPQ(-6:6)
      DATA PI/3.14159/
      NTOT=NTOT+1
      CALL LNSTRF(Z,Q2,XPQ)
      FLQINT=0.
      DO 100 I=-LST(12),LST(12)
      IF(I.EQ.0) GOTO 100
      FLQINT=FLQINT+QC(IABS(I))**2*XPQ(I)
  100 CONTINUE
      FLQINT=4./3.*PARL(25)/PI*(X/Z)**2*FLQINT/Z
      NPASS=NPASS+1

      RETURN
      END

C **********************************************************************

      FUNCTION FLGINT(Z)

C...Gluon contribution integrand to QCD longitudinal structure function.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/


      REAL FLGINT,Z,XPQ,PI
      DIMENSION XPQ(-6:6)
      DATA PI/3.14159/
      NTOT=NTOT+1
      CALL LNSTRF(Z,Q2,XPQ)
      FLGINT=20./9.*PARL(25)/PI*(X/Z)**2*(1.-X/Z)/Z*XPQ(0)
      NPASS=NPASS+1

      RETURN
      END

C **********************************************************************

      FUNCTION FLTINT(Z)

C...Integrand for target mass correction contribution to
C...quark longitudinal structure function

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/


      INTEGER I
      REAL FLTINT,Z,XPQ,PM2
      DIMENSION XPQ(-6:6)
      DATA PM2/0.8804/
      NTOT=NTOT+1
      CALL LNSTRF(Z,Q2,XPQ)
      FLTINT=0.
      DO 100 I=-LST(12),LST(12)
      IF(I.EQ.0) GOTO 100
      FLTINT=FLTINT+QC(IABS(I))**2*XPQ(I)
  100 CONTINUE
      FLTINT=4.*PM2/Q2*(X/Z)**2*X*FLTINT
      NPASS=NPASS+1

      RETURN
      END
C **********************************************************************

      SUBROUTINE LSCI(PROB)

C--                                                                   --C
C--   Created:        950319                                          --C
C--   Last update:    950731                                          --C
C--   Purpose:        to generate random switches of parton           --C
C--                   colours in the partonic final state             --C

      IMPLICIT NONE

C--   global variables
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      
C--       functions
      REAL RLU
C--       local variables
      INTEGER I,J,LUCOMP,NS,NEXT,THIS,INIT
      LOGICAL QUARK,QUARK1,QUARK2,AQUARK1,AQUARK2,GLUON1,GLUON2,FIRST
      REAL PROB

C--       Assign colour and anticolour pointers to all partons. Colour
C--       pointers are in K(I,4) and anticolour pointers are in K(I,5).
C--       The pointer points to the row where the respective anticolour
C--       and colour is.

      FIRST=.TRUE.
      DO 10 I=5,N
         IF (K(I,1).LT.10 .AND. K(I,1).GT.0) THEN
C--       check if parton is a quark, antiquark or diquark
            IF (ABS(K(I,2)).LT.10 .OR. LUCOMP(K(I,2)).EQ.90) THEN
               IF (K(I,2).LT.10 .AND. K(I,2).GT.0 .OR.
     &             K(I,2).LT.-1000) THEN
        	  QUARK=.TRUE.
               ELSE
        	  QUARK=.FALSE.
               ENDIF
C--       reset pointers
              K(I,4)=0
              K(I,5)=0
C--       the first quark, antiquark or diquark in a string points
C--       to the parton in the next line
               IF (FIRST) THEN
        	  IF (QUARK) THEN
                     K(I,4)=(I+1)
        	  ELSE
                     K(I,5)=(I+1)
        	  ENDIF
        	  FIRST=.FALSE.
C--       the last quark, antiquark or diquark in a string points
C--       to the parton in the previous line
               ELSE
        	  IF (QUARK) THEN
                     K(I,4)=(I-1)
        	  ELSE
                     K(I,5)=(I-1)
        	  ENDIF
        	  FIRST=.TRUE.
               ENDIF
               K(I,1)=3
C--       check if parton gluon 
            ELSEIF (K(I,2).EQ.21) THEN
C--       if the previous colour points to this gluon then its anticolour
C--       should point back and its colour should point to the next line 
               IF(K(I-1,4).EQ.I) THEN
        	  K(I,4)=(I+1)
        	  K(I,5)=(I-1)
               ELSE
        	  K(I,4)=(I-1)
        	  K(I,5)=(I+1)
               ENDIF
               K(I,1)=3
            ENDIF
         ENDIF
10    CONTINUE

C--       find first parton in colour switch
      DO 20 I=5,N
         QUARK1=.FALSE.
         AQUARK1=.FALSE.
         GLUON1=.FALSE.
         IF (K(I,1).EQ.3) THEN 
C--       check if parton quark or antidiquark
            IF (K(I,4).NE.0 .AND. K(I,5).EQ.0) THEN
               QUARK1=.TRUE.
C--       check if parton antiquark or diquark
            ELSEIF (K(I,4).EQ.0 .AND. K(I,5).NE.0) THEN
               AQUARK1=.TRUE.
C--       check if parton gluon 
            ELSEIF (K(I,2).EQ.21) THEN
               GLUON1=.TRUE.
            ENDIF
C--       find second parton in colour switch
            DO 30 J=I+1,N
               QUARK2=.FALSE.
               AQUARK2=.FALSE.
               GLUON2=.FALSE.
               IF (K(J,1).EQ.3) THEN
C--       check if second parton quark or antidiquark
                  IF (K(J,4).NE.0 .AND. K(J,5).EQ.0) THEN
                     QUARK2=.TRUE.
C--       check if second parton antquark or diquark
                  ELSEIF (K(J,4).EQ.0 .AND. K(J,5).NE.0) THEN
                     AQUARK2=.TRUE.
C--       check if second parton gluon 
                  ELSEIF (K(J,2).EQ.21) THEN
        	     GLUON2=.TRUE.
                  ENDIF
C--       switch colour pointers
                  IF (QUARK1.AND.QUARK2) THEN
                     IF (RLU(0).LT.PROB) CALL LECSWI(I,J)
                  ELSEIF (K(I,4).NE.J .AND. K(J,4).NE.I .AND. 
     &            (QUARK1.AND.GLUON2 .OR. GLUON1.AND.QUARK2)) THEN
                     IF (RLU(0).LT.PROB) CALL LECSWI(I,J)
                  ELSEIF (AQUARK1.AND.AQUARK2) THEN
                     IF (RLU(0).LT.PROB) CALL LEASWI(I,J)
                  ELSEIF (K(I,5).NE.J .AND. K(J,5).NE.I .AND. 
     &            (AQUARK1.AND.GLUON2 .OR. GLUON1.AND.AQUARK2)) THEN
                     IF (RLU(0).LT.PROB) CALL LEASWI(I,J)
                  ELSEIF (K(I,4).NE.J .AND. K(J,4).NE.I .AND.
     &                 GLUON1.AND.GLUON2) THEN
                     IF (RLU(0).LT.PROB) CALL LECSWI(I,J)
                     IF (RLU(0).LT.PROB) CALL LEASWI(I,J)
                  ENDIF
               ENDIF
30          CONTINUE
         ENDIF
20    CONTINUE

C--       restore colour order in strings ready for hadronisation
      NS=N
      DO 40 I=5,NS
C--       find first quark (or anti diquark) string end
         IF (K(I,1).EQ.3 .AND. K(I,4).NE.0 .AND. K(I,5).EQ.0 )THEN
            NEXT=I
50          CONTINUE
            N=N+1
            IF(N.GT.4000) THEN 
              IF(LST(3).GE.1) WRITE(6,*) 'LSCI: N>4000!'
              LST(21)=101
              RETURN
            ENDIF
            THIS=NEXT
C--       copy to last row in event-record and update K-vector
            DO 60 J=1,5
               P(N,J)=P(THIS,J)
               V(N,J)=V(THIS,J)
               K(N,J)=K(THIS,J)
60          CONTINUE
            K(THIS,1)=13
            K(N,1)=2
            K(N,3)=THIS
            K(N,4)=0
            K(N,5)=0
C--       find next parton in string in row K(THIS,4)
            NEXT=K(THIS,4)
            IF (NEXT.NE.0) GOTO 50
C--       this is the last parton in string
            K(N,1)=1    
         ENDIF
40    CONTINUE
      DO 70 I=5,NS
C--       find first gluon string end
         IF (K(I,1).EQ.3 .AND. K(I,2).EQ.21) THEN
            INIT=I
            NEXT=I
80          CONTINUE
            N=N+1
            IF(N.GT.4000) THEN 
              IF(LST(3).GE.1) WRITE(6,*) 'LSCI: N>4000!'
              LST(21)=101
              RETURN
            ENDIF
            THIS=NEXT
C--       copy to last row in event-record and update K-vector
            DO 90 J=1,5
               P(N,J)=P(THIS,J)
               V(N,J)=V(THIS,J)
               K(N,J)=K(THIS,J)
90          CONTINUE
            K(THIS,1)=13
            K(N,1)=2
            K(N,3)=THIS
            K(N,4)=0
            K(N,5)=0
C--       find next parton in string in row K(THIS,4)
            NEXT=K(THIS,4)
            IF (NEXT.NE.INIT) GOTO 80
C--       this is the last parton in string
            K(N,1)=1    
         ENDIF
70    CONTINUE

      END


C ********************************************************************

      SUBROUTINE LEASWI(I,J)

C--       switch anticolour pointers for partons in rows I and J
C--       and colour pointers for the partons pointing back

      IMPLICIT NONE

C--   global variables
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/


C--       local variables
      INTEGER I,J,KI5,KJ5

      KI5=K(I,5)
      KJ5=K(J,5)
      K(KI5,4)=J
      K(KJ5,4)=I
      K(I,5)=KJ5
      K(J,5)=KI5         

      END

C ********************************************************************

      SUBROUTINE LECSWI(I,J)

C--       switch colour pointers for partons in rows I and J 
C--       and anticolour pointers for the partons pointing back

      IMPLICIT NONE

C--       global variables
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/


C--       local variables
      INTEGER I,J,KI4,KJ4

      KI4=K(I,4)
      KJ4=K(J,4)
      K(KI4,5)=J
      K(KJ4,5)=I
      K(I,4)=KJ4
      K(J,4)=KI4

      END
C ********************************************************************

      SUBROUTINE LSMALL
C--                                                                  --C
C--   Created:     951031                                            --C
C--   Last update: 980722                                            --C
C--   Purpose:     Take care of small mass systems with one diquark, --C  
C--                charmonium or bottonium                           --C

      IMPLICIT NONE

C--       arg for spec. version
      INTEGER ILON
      REAL WEIGHT

C--       global variables
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/


C--       functions
      REAL RLU,ULMASS
      INTEGER LUCOMP
C--       local variables
      INTEGER I,J,IFIRST,I1,I2,IAQ,IQ,NTRY1,NTRY2,KFB,KFH1,KFH2
      INTEGER KIAQ,KIQ,KDUMMY,NPARTON
      REAL*8 ENERGY,MAXENERGY,INVMASS,THRESHOLD
      REAL*8 PSUM(5)
      REAL*8 TOT2,M1,M2,ROTARG,PABS,COSTHE,PTEMP,PHI,PI
      REAL*8 PCPS,PC2,PN2,PS2,A,B,C,EPS2,EPS1
      REAL*8 W441,W443,W445,W10441,W20443,W30443,TMP
      REAL*8 W551,W553,W555,W10551,W20553,W30553
      LOGICAL FIRST,CHARMONIUM,BOTTONIUM,DIQUARK
      DATA PI/3.14159265359D0/
            
C--       do not use spec. version
      ILON=0      

C--       find light singlet system and hadronize them
      FIRST=.TRUE.
      NPARTON=N
      CHARMONIUM=.FALSE.
      BOTTONIUM=.FALSE.
      DIQUARK=.FALSE.
      ENERGY=2.D0*PARJ(32)
      DO 20 I=1,NPARTON
        IF (K(I,1).EQ.2) THEN
          IF (FIRST) THEN
            PSUM(1)=P(I,1)
            PSUM(2)=P(I,2)
            PSUM(3)=P(I,3)
            PSUM(4)=P(I,4)
            MSTJ(93)=1
            PSUM(5)=ULMASS(K(I,2))
            FIRST=.FALSE.
            IFIRST=I
          ELSE
            PSUM(1)=PSUM(1)+P(I,1)
            PSUM(2)=PSUM(2)+P(I,2)
            PSUM(3)=PSUM(3)+P(I,3)
            PSUM(4)=PSUM(4)+P(I,4)
            MSTJ(93)=1
            PSUM(5)=PSUM(5)+ULMASS(K(I,2))
          ENDIF
        ELSEIF (K(I,1).EQ.1 .AND. .NOT. FIRST) THEN
          PSUM(1)=PSUM(1)+P(I,1)
          PSUM(2)=PSUM(2)+P(I,2)
          PSUM(3)=PSUM(3)+P(I,3)
          PSUM(4)=PSUM(4)+P(I,4)
          MSTJ(93)=1
          PSUM(5)=PSUM(5)+ULMASS(K(I,2))
          INVMASS=PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-PSUM(3)**2
          ENERGY=SQRT(MAX(0.D0,INVMASS))-PSUM(5)
          FIRST=.TRUE.
C--   only light systems with two c- or b-quarks or one diquark are of interest
          IF (ENERGY.LT.DBLE(PARJ(32)) .AND. 
     &       ((ABS(K(IFIRST,2)).EQ.4 .AND. ABS(K(I,2)).EQ.4 
     &       .OR. ABS(K(IFIRST,2)).EQ.5 .AND. ABS(K(I,2)).EQ.5) .OR.  
     /      (LUCOMP(K(IFIRST,2)).EQ.90 .OR. LUCOMP(K(I,2)).EQ.90))) THEN

            I1=IFIRST
            I2=I
C--       add system as cluster
            N=N+1
            DO 30 J=1,4
              P(N,J)=PSUM(J)
 30         CONTINUE
            P(N,5)=SQRT(MAX(0.D0,INVMASS))
            K(N,1)=11
            K(N,2)=91
            K(N,3)=I1
            K(N,4)=N+1
            K(N,5)=N+2
            IF(ABS(K(I1,2)).EQ.4) THEN
               CHARMONIUM=.TRUE.
               THRESHOLD=2.*ULMASS(421)
            ELSEIF(ABS(K(I1,2)).EQ.5) THEN
               BOTTONIUM=.TRUE.
               THRESHOLD=2.*ULMASS(521)
            ELSEIF(LUCOMP(K(I1,2)).EQ.90  .OR.
     /             LUCOMP(K(I2,2)).EQ.90) THEN
               DIQUARK=.TRUE.
               THRESHOLD=ULMASS(2212)+ULMASS(111)
            ENDIF
C--       inactivate old system
            DO 40 J=I1,I2
              K(J,1)=K(J,1)+10
              K(J,4)=N
 40         CONTINUE
	 
C--       try to make two particles
            NTRY1=0
            NTRY2=0
 50         CONTINUE
C--       take quark or diquark end first
            IF (K(I1,2).EQ.4 .OR. K(I1,2).EQ.5 .OR.
     /          LUCOMP(K(I1,2)).EQ.90) THEN
              IQ=I1
              IAQ=I2
            ELSE
              IQ=I2
              IAQ=I1
            ENDIF
            KIQ=K(IQ,2)
            KIAQ=K(IAQ,2)
            KDUMMY=0
            KFB=0
            KFH1=0
            KFH2=0
            CALL LUKFDI(KIQ,KDUMMY,KFB,KFH1)
            KDUMMY=0
            CALL LUKFDI(KIAQ,-KFB,KDUMMY,KFH2)
            IF (KFH1.EQ.0 .OR. KFH2.EQ.0) THEN
              NTRY1=NTRY1+1
              IF (NTRY1.GE.100) THEN
                LST(21)=200
                RETURN
              ENDIF
              GOTO 50
            ENDIF
C--       consistency checks
            IF (LUCOMP(KFH1).EQ.0 .OR. LUCOMP(KFH2).EQ.0) THEN
              LST(21)=201
              RETURN
            ENDIF
            IF (KCHG(LUCOMP(KFH1),2)*ISIGN(1,KFH1)+
     +          KCHG(LUCOMP(KFH2),2)*ISIGN(1,KFH2) .NE.0) THEN
              LST(21)=202
              RETURN
            ENDIF
            P(N+1,5)=ULMASS(KFH1)
            P(N+2,5)=ULMASS(KFH2)
            IF (P(N,5).LE.P(N+1,5)+P(N+2,5)+PARJ(64) .AND.
     &          P(N,5).GE.THRESHOLD+PARJ(64) .AND.
     &          NTRY2.LE.100) THEN
              NTRY2=NTRY2+1
              GOTO 50
            ENDIF
            IF (P(N,5).GE.P(N+1,5)+P(N+2,5)+PARJ(64)) THEN
C--       make two particles
C--       isotropic decay in cms (dcostheta*dphi)
              TOT2=INVMASS
              M1=DBLE(P(N+1,5))
              M2=DBLE(P(N+2,5))            
              ROTARG=(TOT2-M1**2-M2**2)**2-4.D0*M1**2*M2**2
              IF (ROTARG.LT.0.D0) THEN
                LST(21)=203
                RETURN
              ENDIF
              PABS=0.5D0*SQRT(ROTARG/TOT2)
              COSTHE=-1.D0+2.D0*RLU(0)
              PTEMP=PABS*SQRT(1.D0-COSTHE**2)
              PHI=2.D0*PI*RLU(0)
              P(N+1,4)=SQRT(PABS**2+M1**2)
              P(N+1,3)=PABS*COSTHE
              P(N+1,2)=PTEMP*COS(PHI)
              P(N+1,1)=PTEMP*SIN(PHI)
              P(N+2,4)=SQRT(PABS**2+M2**2)
              P(N+2,3)=-P(N+1,3)
              P(N+2,2)=-P(N+1,2)
              P(N+2,1)=-P(N+1,1)
C--       K-vector
              K(N+1,1)=1
              K(N+1,2)=KFH1
              K(N+1,3)=N
              K(N+1,4)=0
              K(N+1,5)=0
              K(N+2,1)=1
              K(N+2,2)=KFH2
              K(N+2,3)=N
              K(N+2,4)=0
              K(N+2,5)=0
C--       boost to lab
              MSTU(33)=1
              CALL LUDBRB(N+1,N+2,0.,0.,PSUM(1)/PSUM(4),
     &                    PSUM(2)/PSUM(4),PSUM(3)/PSUM(4)) 
C--       V-vector
              DO 60 J=1,5 
                V(N,J)=V(IQ,J) 
                V(N+1,J)=V(IQ,J) 
                V(N+2,J)=V(IAQ,J) 
 60           CONTINUE 
              V(N,5)=0. 
              V(N+1,5)=0. 
              V(N+2,5)=0.
              N=N+2 
            ELSE


C--       make one particle instead
              IF(CHARMONIUM) THEN

                W441=1.D0
                W443=3.D0
                W445=5.D0
                W10441=1.D0
                W20443=3.D0
C--       extra suppression of psiprime from radial exitation
                W30443=3.D0/2.D0

                TMP=RLU(0)*(W441+W443+W445+W10441+W20443+W30443)
                IF (TMP .LT. W441) THEN
                  KFH1=441
                ELSEIF (TMP .LT. W441+W443) THEN
                  KFH1=443
                ELSEIF (TMP .LT. W441+W443+W445) THEN
                  KFH1=445
                ELSEIF (TMP .LT. W441+W443+W445+W10441) THEN
                  KFH1=10441
                ELSEIF (TMP .LT. W441+W443+W445+W10441+W20443) THEN
                  KFH1=20443
                ELSE
                  KFH1=30443
                ENDIF
                
C--       code for spec. version
                IF (ILON.EQ.1) THEN
                  KFH1=441
                  WEIGHT=W441/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.2) THEN
                  KFH1=443
                  WEIGHT=W443/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.3) THEN
                  KFH1=445
                  WEIGHT=W445/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.4) THEN
                  KFH1=10441
                  WEIGHT=W10441/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.5) THEN
                  KFH1=20443
                  WEIGHT=W20443/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.6) THEN
                  KFH1=30443
                  WEIGHT=W30443/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                
              ELSEIF(BOTTONIUM) THEN

                W551=1.D0
                W553=3.D0
                W555=5.D0
                W10551=1.D0
                W20553=3.D0
                W30553=3.D0/2.D0

                TMP=RLU(0)*(W551+W553+W555+W10551+W20553+W30553)
                IF (TMP .LT. W551) THEN
                  KFH1=551
                ELSEIF (TMP .LT. W551+W553) THEN
                  KFH1=553
                ELSEIF (TMP .LT. W551+W553+W555) THEN
                  KFH1=555
                ELSEIF (TMP .LT. W551+W553+W555+W10551) THEN
                  KFH1=10551
                ELSEIF (TMP .LT. W551+W553+W555+W10551+W20553) THEN
                  KFH1=20553
                ELSE
                  KFH1=30553
                ENDIF

C--       code for spec. version
                IF (ILON.EQ.1) THEN
                  KFH1=551
                  WEIGHT=W551/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.2) THEN
                  KFH1=553
                  WEIGHT=W553/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.3) THEN
                  KFH1=555
                  WEIGHT=W555/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.4) THEN
                  KFH1=10551
                  WEIGHT=W10551/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.5) THEN
                  KFH1=20553
                  WEIGHT=W20553/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.6) THEN
                  KFH1=30553
                  WEIGHT=W30553/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF

              ELSEIF(DIQUARK) THEN
70              CONTINUE
                KIQ=K(IQ,2)
                KIAQ=K(IAQ,2)
                KDUMMY=0
                KFH1=0
                CALL LUKFDI(KIQ,KIAQ,KDUMMY,KFH1)
                IF (KFH1.EQ.0) GOTO 70
C--       isospin conservation
                IF (KFH1.EQ.2214) KFH1=2212
                IF (KFH1.EQ.2114) KFH1=2112
                IF (KFH1.EQ.-2214) KFH1=-2212
                IF (KFH1.EQ.-2114) KFH1=-2112
	         
              ENDIF

              K(N,5)=N+1
              K(N+1,1)=1
              K(N+1,2)=KFH1
              K(N+1,3)=N
              K(N+1,4)=0
              K(N+1,5)=0
              P(N+1,5)=ULMASS(KFH1)
C--       find particle to shuffle energy & momentum to and from
              MAXENERGY=0.D0
              I1=0
              DO 80 J=1,N-1
                IF (0.LT.K(J,1) .AND. K(J,1).LT.10 .AND.
     &              LUCOMP(K(J,2)).NE.0 .AND.
     &              (K(J,2).EQ.21 .OR. ABS(K(J,2)).LT.10 .OR.
     &               ABS(K(J,2)).GT.100) ) THEN
                  INVMASS=(P(N,4)+P(J,4))**2-(P(N,1)+P(J,1))**2-
     -                    (P(N,2)+P(J,2))**2-(P(N,3)+P(J,3))**2
                  ENERGY=SQRT(MAX(0.D0,INVMASS))-P(N,5)-P(J,5)
                  IF (ENERGY.GT.MAXENERGY ) THEN
                    I1=J
                    MAXENERGY=ENERGY
                  ENDIF
                ENDIF
 80           CONTINUE
C--       shuffle energy & momentum
              IF (I1.NE.0) THEN
                PCPS=DBLE(P(N,4))*DBLE(P(I1,4))
     -              -DBLE(P(N,1))*DBLE(P(I1,1))
     -              -DBLE(P(N,2))*DBLE(P(I1,2))
     -              -DBLE(P(N,3))*DBLE(P(I1,3))
                PC2=DBLE(P(N,5))**2
                PN2=DBLE(P(N+1,5))**2
                PS2=DBLE(P(I1,5))**2
                A=PC2+PS2+2.D0*PCPS
                B=PC2+PN2+2.D0*PCPS
                C=(PN2-PC2)*(4.D0*PCPS*(PCPS+PC2)-PC2*(PN2-PC2))/
     /            4.D0/(PCPS**2-PC2*PS2)
                IF (B**2-4.D0*C*A.LT.0.D0) THEN
                  LST(21)=204
                  RETURN
                ENDIF
                EPS2=(-B+SQRT(MAX(0.D0,B**2-4.D0*C*A)))/2.D0/A
                EPS1=(PN2-PC2+2.D0*EPS2*(PS2+PCPS))/2.D0/(PC2+PCPS)
                DO 90 J=1,4 
                  P(N+1,J)=(1.+EPS1)*P(N,J)-EPS2*P(I1,J) 
                  P(I1,J)=(1.+EPS2)*P(I1,J)-EPS1*P(N,J) 
                  V(N,J)=V(I1,J) 
                  V(N+1,J)=V(I1,J) 
 90             CONTINUE 
                V(N,5)=0. 
                V(N+1,5)=0. 
                N=N+1
              ELSE
                LST(21)=205
                RETURN
              ENDIF
            ENDIF
            CHARMONIUM=.FALSE.
            BOTTONIUM=.FALSE.
            DIQUARK=.FALSE.
	  ENDIF
        ENDIF
 20   CONTINUE
          
      RETURN
      
      END

C#######################################################################
C
C  The following routines for parton cascades were made together
C  with M. Bengtsson and T. Sjostrand (Z. Phys. C37 (1988) 465,
C  Nucl. Phys. B301 (1988) 554). Contain modifications of
C  routines in PYTHIA 4.8 (Sjostrand, Bengtsson, CPC 46 (1987) 43).
C
C **********************************************************************

      SUBROUTINE LSHOWR(ICALL)

      IMPLICIT NONE

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DBETA
      REAL STHETA,SPHI,PB,PHIR
      SAVE /LBOOST/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LYPRO2)
*
      COMMON /LYPROC/ ISUB,KFL(3,2),X(2),SH,TH,UH,Q2,XSEC(0:40)
      REAL    X,SH,TH,UH,Q2,XSEC
      INTEGER ISUB,KFL
      SAVE /LYPROC/ 

      COMMON /LYINT1/ XQ(2,-6:6),DSIG(-6:6,-6:6,5),FSIG(10,10,3)
      REAL    XQ,DSIG,FSIG
      SAVE /LYINT1/ 
 


      INTEGER ICALL,I,J,NS,IFL,IFL1,IFL2,IT,IPU1,IPU2
      REAL XR,PMA1,PMA2,ULMASS,QMAX,ULANGL,T1,T2
      
      DOUBLE PRECISION DPQ2,DPB(3),DPA(3),DCTHET,DROBO(5)

      INTEGER KS(9,5)
      REAL PS(9,5),ROBO(5),XPQ(-6:6)
      SAVE KS,PS

      IF(ICALL.EQ.0) THEN
C...Initialize cascade for each event, save event record in overall cms.
        DO 10 I=1,9
        DO 10 J=1,5
        KS(I,J)=0
   10   PS(I,J)=0.
        DO 20 J=1,5
        KS(1,J)=K(1,J)
        PS(1,J)=P(1,J)
        KS(2,J)=K(2,J)
        PS(2,J)=P(2,J)
        KS(5,J)=K(3,J)
        PS(5,J)=P(3,J)
        KS(7,J)=K(4,J)
   20   PS(7,J)=P(4,J)
        KS(5,3)=3
        KS(7,1)=21
        KS(7,3)=5
C       CALL GULIST(0,2)
        RETURN
      ENDIF

C     CALL GULIST(1,2)
C...Apply parton cascade on QPM event.
C...Save incoming and outgoing quark as well as scattered lepton.
      KS(6,1)=21
      KS(6,2)=LST(25)
      KS(6,3)=4
      KS(8,1)=21
      KS(8,2)=K(5,2)
      KS(8,3)=6
      KS(9,1)=0
      KS(9,2)=K(4,2)
      KS(9,3)=5
      DO 110 J=1,5
      PS(6,J)=0.
      PS(8,J)=P(5,J)
  110 PS(9,J)=P(4,J)
      XR=XLP
      DPQ2=DBLE(Q2LP)
      PMA1=0.
      PS(6,5)=PMA1
      PMA2=PS(8,5)
      DPB(1)=0.5D0*(DPQ2*(1D0/XR-1D0)+DBLE(PS(1,5))**2-
     &ULMASS(IABS(KS(7,2)))**2)/(PS(1,4)+PS(2,4))
      DPB(2)=DSQRT(DPB(1)**2+DPQ2)
      DCTHET=(DBLE(PS(2,4))*DPB(1)-DPQ2/(2D0*XR))/(DBLE(PS(2,3))*
     &DPB(2))
      DPA(1)=(DPB(2)*DCTHET)**2-DPB(1)**2
      DPA(2)=DPQ2-DBLE(PMA1)**2+DBLE(PMA2)**2
      PS(6,4)=-(DPA(2)*DPB(1)-DPB(2)*DCTHET*DSQRT(DPA(2)**2+4D0*
     &DBLE(PMA1)**2*DPA(1)))/(2D0*DPA(1))
      PS(6,3)=-SQRT((PS(6,4)+PMA1)*(PS(6,4)-PMA1))
C...Partons with colour information in hadronic cms frame.
      DO 120 I=10,26
      DO 120 J=1,5
      K(I,J)=0
      P(I,J)=0.
  120 V(I,J)=0.
      NS=20
      K(NS+1,1)=21
      K(NS+1,2)=K(3,2)
      K(NS+1,3)=3
      K(NS+2,1)=-1
      K(NS+2,3)=NS+1
      K(NS+3,2)=KS(6,2)
      DO 130 J=1,5
  130 P(NS+1,J)=P(3,J)
      K(NS+3,1)=13
      K(NS+3,3)=2
      P(NS+3,5)=0.
      K(NS+4,1)=-1
      K(NS+4,3)=NS+3
      K(NS+3,4)=NS+5
      K(NS+3,5)=NS+5
      P(NS+4,3)=NS+5
      P(NS+4,4)=NS+5
      K(NS+5,1)=3
      K(NS+5,3)=8
      K(NS+5,2)=KS(8,2)
      K(NS+6,1)=-1
      K(NS+6,3)=NS+5
      DO 140 J=1,4
      P(NS+5,J)=P(5,J)
      P(NS+3,J)=P(NS+5,J)-P(NS+1,J)
  140 CONTINUE
      P(NS+5,5)=PMA2
      P(NS+6,1)=NS+3
      P(NS+6,2)=NS+3
      K(NS+5,4)=(NS+3)*MSTU(5)
      K(NS+5,5)=(NS+3)*MSTU(5)
      N=NS+6
C     CALL GULIST(2,2)
C...Copy saved record in overall cms to line 1 through 9.
C...Lines 1,2,5,6,7 in ep cms, 8,9 in hadronic cms
      DO 150 I=1,9
      DO 150 J=1,5
      K(I,J)=KS(I,J)
  150 P(I,J)=PS(I,J)
C     CALL GULIST(3,2)
C...Scale for bremsstrahlung etc.
      Q2=Q2LP
      IPY(40)=8
      IPY(47)=N
C...Save quantities for later use.
      X(1)=1.
      X(2)=XR
      CALL LYSTFU(K(2,2),XR,Q2LP,XPQ)
      DO 160 IFL=-6,6
  160 XQ(2,IFL)=XPQ(IFL)
      IF(LST(23).EQ.1) THEN
        ISUB=39
        IPY(11)=1
      ELSEIF(LST(23).EQ.3) THEN
        ISUB=39
        IPY(11)=2
      ELSEIF(LST(23).EQ.4) THEN
        ISUB=39
        IPY(11)=3
      ELSEIF(LST(23).EQ.2) THEN
        ISUB=40
      ENDIF
      KFL(2,1)=K(5,2)
      IFL1=K(6,2)
      IFL2=K(8,2)
      KFL(2,2)=IFL1
      KFL(1,1)=KFL(2,1)
      KFL(1,2)=KFL(2,2)
      IF(ISUB.EQ.39) KFL(3,1)=K(1,2)
      IF(ISUB.EQ.40) KFL(3,1)=K(1,2)+ISIGN(1,K(1,2))
      KFL(3,2)=IFL2
      PYVAR(2)=(P(1,4)+P(2,4))**2
      PYVAR(1)=SQRT(PYVAR(2))
      PYVAR(3)=P(1,5)
      PYVAR(4)=P(2,5)
      PYVAR(5)=P(1,3)
      IPY(41)=K(1,2)
      IPY(42)=K(2,2)
      IPY(48)=0

C...Generate timelike parton shower (if required)
      IF(IPY(13).EQ.1) THEN
        CALL LSCALE(1,QMAX)
        QMAX=MIN(QMAX,P(25,4))
        CALL LUSHOW(25,0,QMAX)
      ENDIF
      IT=25
      IF(N.GE.27) IT=27
      NS=N
C     CALL GULIST(4,2)

C...Generate spacelike parton shower (if required)
      IPU1=0
      IPU2=23
      IF(X(2)*(1.+(P(IT,5)**2+PYPAR(22))/P(21,5)**2).GT.0.999) THEN
        LST(21)=7
        RETURN
      ENDIF
      IF(IPY(14).GE.1) THEN
        CALL LYSSPA(IPU1,IPU2)
        IF(LST(21).NE.0) RETURN
      ENDIF
      IF(N.EQ.NS) THEN 
        DO 220 I=NS+1,NS+4
        DO 220 J=1,5
        K(I,J)=0
        P(I,J)=0.
  220   V(I,J)=0.
        K(NS+1,1)=11
        K(NS+1,2)=KFL(2,1)
        K(NS+1,3)=21
        DO 230 J=1,5
  230   P(NS+1,J)=P(21,J)
        K(NS+2,1)=-1
        K(NS+2,3)=NS+1
        K(NS+3,1)=13
        K(NS+3,2)=KFL(2,2)
        K(NS+3,3)=23
        K(NS+3,4)=23
        K(NS+3,5)=23
        P(NS+3,3)=(P(IT,5)**2+Q2)*(P(21,4)-P(21,3))/(2.*Q2)
        P(NS+3,4)=-P(NS+3,3)
        K(NS+4,1)=-1
        K(NS+4,3)=NS+3
        P(NS+4,3)=23
        P(NS+4,4)=23
        P(24,1)=NS+3
        P(24,2)=NS+3
        K(23,4)=K(23,4)+(NS+3)*MSTU(5)
        K(23,5)=K(23,5)+(NS+3)*MSTU(5)
        IPU1=0
        IPU2=NS+3
        N=N+4
      ENDIF
C     CALL GULIST(5,2)

      IF(ABS(P(IT,1)).GT.0.1.OR.ABS(P(IT,2)).GT.0.1) THEN
C       WRITE(6,*) 'Warning: non-zero pt on final shower initiator'
C       WRITE(6,*) '0:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
        LST(21)=8
        RETURN
      ENDIF
      P(IT,1)=0.
      P(IT,2)=0.

C...Rotate and boost outgoing parton shower
      IF(N.GT.30) THEN
        K(N+1,1)=0
        DO 210 J=1,4
  210   P(N+1,J)=P(NS+1,J)+P(NS+3,J)
        IF(P(N+1,4).LE.1.01*P(IT,5)) THEN
          LST(21)=9
          RETURN
        ENDIF
        ROBO(1)=ULANGL(P(IT,3),SQRT(P(IT,1)**2+P(IT,2)**2))
        ROBO(2)=ULANGL(P(IT,1),P(IT,2))
      IF(ABS(ROBO(1)).GT.0.001.OR.ABS(ROBO(2)).GT.0.001) THEN
      WRITE(6,*) '0:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
      WRITE(6,*) '   ROBO(1-2)=',ROBO(1),ROBO(2)
      ENDIF
        CALL LUDBRB(25,NS,0.,-ROBO(2),0.D0,0.D0,0.D0)
        CALL LUDBRB(25,NS,-ROBO(1),0.,0.D0,0.D0,0.D0)
        DROBO(5)=-(P(IT,3)*P(IT,4)-P(N+1,4)*SQRT(P(N+1,4)**2-
     &  P(IT,4)**2+P(IT,3)**2))/(P(IT,3)**2+P(N+1,4)**2)
        CALL LUDBRB(25,NS,0.,0.,0.D0,0.D0,DROBO(5))
        ROBO(1)=ULANGL(P(N+1,3),SQRT(P(N+1,1)**2+P(N+1,2)**2))
        ROBO(2)=ULANGL(P(N+1,1),P(N+1,2))
        CALL LUDBRB(25,NS,ROBO(1),ROBO(2),0.D0,0.D0,0.D0)
      ENDIF
C     CALL GULIST(6,2)

      Q2=Q2LP
C...Hadron remnant and primordial kt
      IPY(47)=N
      CALL LYREMN(IPU1,IPU2)
      IF(IPY(48).EQ.1) THEN
        LST(21)=10
        RETURN
      ENDIF
C     CALL GULIST(7,2)

C...Transform line 1,2 and 5-7 to hadronic cms frame.
      CALL LUDBRB(1,2,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
      CALL LUDBRB(1,2,-STHETA(2),0.,0.D0,0.D0,0.D0)
      CALL LUDBRB(5,7,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
      CALL LUDBRB(5,7,-STHETA(2),0.,0.D0,0.D0,0.D0)
C     CALL GULIST(8,2)

C...Rearrange partons along strings
      MSTU(24)=0
      CALL LUPREP(0)
      IF(MSTU(24).NE.0) THEN
C       CALL GULIST(88,2)
        IF(LST(3).GE.1) WRITE(6,*) ' LUPREP error MSTU(24)= ',MSTU(24)
        LST(21)=11
        RETURN
      ENDIF
C     CALL GULIST(9,2)

C...Clean up event record -> order:
C...1=inc. lepton; 2=inc. nucleon; 3=exch boson; 4=scat. lepton;
C...5=inc. parton before initial shower; 6=inc. quark at boson vertex
C...after shower; 7=scat. quark at boson vertex before final shower
      LST(26)=7
      DO 510 J=1,5
      K(N+1,J)=K(4,J)
  510 P(N+1,J)=P(4,J)
      DO 520 J=1,5
      K(3,J)=K(5,J)
      P(3,J)=P(5,J)
      K(4,J)=K(9,J)
      P(4,J)=P(9,J)
      K(5,J)=K(N+1,J)
      P(5,J)=P(N+1,J)
C     K(7,J)=K(8,J)
C     P(7,J)=P(8,J)
      K(6,J)=K(NS+3,J)
      P(6,J)=P(NS+3,J)
      K(7,J)=K(IT,J)
      P(7,J)=P(IT,J)
  520 CONTINUE
      K(3,3)=1
      K(4,3)=1
      K(6,1)=21
      K(6,3)=5
      K(6,4)=0
      K(6,5)=0
      K(7,1)=21
      K(7,3)=6
      K(7,4)=0
      K(7,5)=0
C...Activate line with scattered lepton.
      K(4,1)=1
C...Deactivate obsolete lines 8, 9 and 21, NS+1 (extra lines with boson)
      K(8,1)=0
      K(9,1)=0
      K(21,1)=0
      IF(K(NS+1,2).EQ.K(3,2)) K(NS+1,1)=0
C...Zero irrelevant lines with K(I,1)<0
      DO 540 I=1,N
      IF(K(I,1).LT.0) THEN
        DO 530 J=1,5
        K(I,J)=0
  530   P(I,J)=0.
      ENDIF
  540 CONTINUE
C     CALL GULIST(10,2)
C...Delete internal parton lines, i.e. with K(I,1)=13,14
      IF(MOD(LST(4)/10,10).EQ.0) THEN
        CALL LTIMEX(T1)
        CALL LUEDIT(14)
        CALL LTIMEX(T2)
C       CALL GULIST(11,2)
      ENDIF
C...Delete empty lines
      CALL LTIMEX(T1)
      CALL LUEDIT(12)
      CALL LTIMEX(T2)
C     CALL GULIST(12,2)

      RETURN
      END

C **********************************************************************

      SUBROUTINE LMEPS

      IMPLICIT NONE


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DBETA
      REAL STHETA,SPHI,PB,PHIR
      SAVE /LBOOST/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LYPRO2)
*
      COMMON /LYPROC/ ISUB,KFL(3,2),X(2),SH,TH,UH,Q2,XSEC(0:40)
      REAL    X,SH,TH,UH,Q2,XSEC
      INTEGER ISUB,KFL
      SAVE /LYPROC/ 

      COMMON /LYINT1/ XQ(2,-6:6),DSIG(-6:6,-6:6,5),FSIG(10,10,3)
      REAL    XQ,DSIG,FSIG
      SAVE /LYINT1/ 
 


      INTEGER I,J,KS,IP2,NS,IFL,IT,IPU1,IPU2
      REAL PS,XR,ROBO,XPQ,QMAX,T1,T2
      REAL ULANGL
      DOUBLE PRECISION DPQ2,DROBO(5)
      DOUBLE PRECISION DELTAP(4),DPLONG,DBTOT,DGAMMA,DROOT
      DIMENSION KS(9,5),PS(9,5),ROBO(5),XPQ(-6:6)
**HI      SAVE KS,PS

C     CALL GULIST(100,2)
C...Save event record in hadronic cms
      DO 10 I=1,7
      DO 10 J=1,5
      KS(I,J)=K(I,J)
   10 PS(I,J)=P(I,J)
C...Rearrange event record to PYSSPA standard
      IP2=6
      IF(LST(24).EQ.3) IP2=7
      DO 20 J=1,5
      K(3,J)=0.
      P(3,J)=0.
      K(4,J)=0
      P(4,J)=0.
      K(5,J)=KS(3,J)
      P(5,J)=PS(3,J)
      K(7,J)=KS(4,J)
      P(7,J)=PS(4,J)
      K(8,J)=KS(5,J)
      P(8,J)=PS(5,J)
      K(9,J)=KS(4,J)
      P(9,J)=PS(4,J)
      K(10,J)=KS(IP2,J)
   20 P(10,J)=PS(IP2,J)
      K(5,3)=3
      K(6,3)=4
      K(7,3)=5
      K(8,3)=6
      K(9,3)=5
      K(10,3)=6
      DO 30 I=5,10
   30 K(I,1)=21
      K(9,1)=0
C...Incoming parton = outgoing 2 parton - boson fourvectors
      DO 40 J=1,4
   40 P(6,J)=P(8,J)+P(10,J)-P(5,J)
      P(6,5)=0.
      K(6,2)=LST(25)
      IF(LST(24).EQ.3) K(6,2)=21
      N=10
C     CALL GULIST(101,2)

      XR=XLP
      DPQ2=DBLE(Q2LP)
C...Partons with colour information in hadronic cms frame.
      DO 120 I=11,27
      DO 120 J=1,5
      K(I,J)=0
      P(I,J)=0.
  120 V(I,J)=0.
      NS=20
      DO 130 J=1,5
      K(NS+1,J)=K(5,J)
      P(NS+1,J)=P(5,J)
      K(NS+3,J)=K(6,J)
      P(NS+3,J)=P(6,J)
      K(NS+5,J)=K(8,J)
      P(NS+5,J)=P(8,J)
      K(NS+6,J)=K(10,J)
  130 P(NS+6,J)=P(10,J)
C...Old standard continuation lines
      K(NS+2,1)=-1
      K(NS+2,3)=NS+1
      K(NS+4,1)=-1
      K(NS+4,3)=NS+3
      P(NS+4,3)=27
      P(NS+4,4)=27
C...Origin and colour info for incoming parton
      K(NS+3,1)=13
      K(NS+3,3)=2
      K(NS+3,4)=27
      K(NS+3,5)=27
C...Colour info for two outgoing partons
      K(NS+5,1)=3
      K(NS+6,1)=3
      IF(K(NS+6,2).EQ.21) THEN
C...qg-event
        IF(K(NS+5,2).GT.0) THEN
          K(NS+5,4)=(NS+6)*MSTU(5)
          K(NS+5,5)=(NS+7)*MSTU(5)
          K(NS+6,4)=(NS+7)*MSTU(5)
          K(NS+6,5)=(NS+5)*MSTU(5)
        ELSE
          K(NS+5,4)=(NS+7)*MSTU(5)
          K(NS+5,5)=(NS+6)*MSTU(5)
          K(NS+6,4)=(NS+5)*MSTU(5)
          K(NS+6,5)=(NS+7)*MSTU(5)
        ENDIF
      ELSE
C...qqbar-event
        K(NS+5,4)=(NS+7)*MSTU(5)
        K(NS+5,5)=(NS+7)*MSTU(5)
        K(NS+6,4)=(NS+7)*MSTU(5)
        K(NS+6,5)=(NS+7)*MSTU(5)
      ENDIF
C...Effective outgoing parton = sum of both outgoing partons
      K(NS+7,1)=14
      K(NS+7,3)=3
      IF(LST(24).EQ.2) THEN
        K(NS+7,2)=K(NS+5,2)
        IF(K(NS+7,2).EQ.21) WRITE(6,*) ' Warning: K(NS+7,2)=',K(NS+7,2)
        IF(K(NS+7,2).GT.0) THEN
          K(NS+7,4)=(NS+3)*MSTU(5)+26
          K(NS+7,5)=(NS+3)*MSTU(5)+25
        ELSE
          K(NS+7,4)=(NS+3)*MSTU(5)+25
          K(NS+7,5)=(NS+3)*MSTU(5)+26
        ENDIF
      ELSE
        K(NS+7,2)=21
        IF(K(NS+5,2).GT.0) THEN
          K(NS+7,4)=(NS+3)*MSTU(5)+25
          K(NS+7,5)=(NS+3)*MSTU(5)+26
        ELSE
          K(NS+7,4)=(NS+3)*MSTU(5)+26
          K(NS+7,5)=(NS+3)*MSTU(5)+25
        ENDIF
      ENDIF
      DO 140 J=1,4
  140 P(NS+7,J)=P(8,J)+P(10,J)

      IT=NS+7
      IF(ABS(P(IT,1)).GT.0.1.OR.ABS(P(IT,2)).GT.0.1) THEN
C       WRITE(6,*) 'Warning: non-zero pt on final shower initiator'
C       WRITE(6,*) '1:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
C       WRITE(6,*) '1:',8 ,K( 8,2),P( 8,1),P( 8,2),P( 8,3),P( 8,4),P( 8,5)
C       WRITE(6,*) '1:',10,K(10,2),P(10,1),P(10,2),P(10,3),P(10,4),P(10,5)
        LST(21)=12
        RETURN
      ENDIF
      P(IT,1)=0.
      P(IT,2)=0.

      P(NS+7,5)=SQRT(MAX(0.,P(NS+7,4)**2-P(NS+7,1)**2-P(NS+7,2)**2-
     &P(NS+7,3)**2))
      N=NS+7
C     CALL GULIST(103,2)

C...Scale for bremsstrahlung etc.
      Q2=Q2LP
      IPY(40)=10
      IPY(47)=N
C...Save quantities for later use.
      X(1)=1.
      X(2)=XR
      CALL LYSTFU(K(2,2),XR,Q2LP,XPQ)
      DO 160 IFL=-6,6
  160 XQ(2,IFL)=XPQ(IFL)
      IF(LST(23).EQ.1) THEN
        ISUB=39
        IPY(11)=1
      ELSEIF(LST(23).EQ.3) THEN
        ISUB=39
        IPY(11)=2
      ELSEIF(LST(23).EQ.4) THEN
        ISUB=39
        IPY(11)=3
      ELSEIF(LST(23).EQ.2) THEN
        ISUB=40
      ENDIF
      KFL(2,1)=K(5,2)
      KFL(2,2)=K(6,2)
      KFL(1,1)=KFL(2,1)
      KFL(1,2)=KFL(2,2)
      IF(ISUB.EQ.39) KFL(3,1)=K(1,2)
      IF(ISUB.EQ.40) KFL(3,1)=K(1,2)+ISIGN(1,K(1,2))
      KFL(3,2)=K(27,2)
      PYVAR(2)=PARL(21)
      PYVAR(1)=SQRT(PYVAR(2))
      PYVAR(3)=P(1,5)
      PYVAR(4)=P(2,5)
      PYVAR(5)=PYVAR(1)/2.
      IPY(41)=K(1,2)
      IPY(42)=K(2,2)
      IPY(48)=0

C...Generate timelike parton shower (if required)
      IF(IPY(13).EQ.1) THEN
        CALL LSCALE(1,QMAX)
        CALL LUSHOW(25,26,QMAX)
      ENDIF
      IT=25
      IF(N.GE.27) IT=27
      NS=N
C     CALL GULIST(104,2)

C...Generate spacelike parton shower (if required)
      IPU1=0
      IPU2=23
      IF(X(2)*(1.+(P(IT,5)**2+PYPAR(22))/P(21,5)**2).GT.0.999) THEN
        LST(21)=13
        RETURN
      ENDIF
      IF(IPY(14).GE.1) THEN
        CALL LYSSPA(IPU1,IPU2)
        IF(LST(21).NE.0) RETURN
      ENDIF
      IF(N.EQ.NS) THEN 
        DO 220 I=NS+1,NS+4
        DO 220 J=1,5
        K(I,J)=0
        P(I,J)=0.
  220   V(I,J)=0.
        K(NS+1,1)=11
        K(NS+1,2)=KFL(2,1)
        K(NS+1,3)=21
        DO 230 J=1,5
  230   P(NS+1,J)=P(21,J)
        K(NS+2,1)=-1
        K(NS+2,3)=NS+1
        K(NS+3,1)=13
        K(NS+3,2)=KFL(2,2)
        K(NS+3,3)=23
        K(NS+3,4)=23
        K(NS+3,5)=23
        P(NS+3,3)=(P(IT,5)**2+Q2LP)*(P(21,4)-P(21,3))/(2.*Q2LP)
        P(NS+3,4)=-P(NS+3,3)
        K(NS+4,1)=-1
        K(NS+4,3)=NS+3
        P(NS+4,3)=23
        P(NS+4,4)=23
        P(24,1)=NS+3
        P(24,2)=NS+3
        K(23,4)=K(23,4)+(NS+3)*MSTU(5)
        K(23,5)=K(23,5)+(NS+3)*MSTU(5)
        IPU1=0
        IPU2=NS+3
        N=N+4
      ENDIF
C     CALL GULIST(105,2)

C...Rotate and boost outgoing parton shower
      IF(N.GT.31) THEN
        K(N+1,1)=0
        DO 210 J=1,4
  210   P(N+1,J)=P(NS+1,J)+P(NS+3,J)
        IF(P(N+1,4).LE.1.01*P(IT,5)) THEN
          LST(21)=14
          RETURN
        ENDIF
        ROBO(1)=ULANGL(P(IT,3),SQRT(P(IT,1)**2+P(IT,2)**2))
        ROBO(2)=ULANGL(P(IT,1),P(IT,2))
      IF(ABS(ROBO(1)).GT.0.001.OR.ABS(ROBO(2)).GT.0.001) THEN
      WRITE(6,*) '1:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
      WRITE(6,*) '   ROBO(1-2)=',ROBO(1),ROBO(2)
      ENDIF
        CALL LUDBRB(25,NS,0.,-ROBO(2),0.D0,0.D0,0.D0)
        CALL LUDBRB(25,NS,-ROBO(1),0.,0.D0,0.D0,0.D0)
C...Replace old rotation method with x,y,z-boost to preserve QCD phi-dep
        DELTAP(1)=DBLE(P(N+1,1))
        DELTAP(2)=DBLE(P(N+1,2))
        DELTAP(3)=DBLE(P(N+1,3)) - DBLE(P(IT,3))
        DELTAP(4)=SQRT(DELTAP(1)**2+DELTAP(2)**2+DELTAP(3)**2)
        IF(DELTAP(4).LT.1.D-11) GOTO 410
        DPLONG=-(DBLE(P(IT,3))*DELTAP(3))/DELTAP(4)
        DROOT=MAX(0.D0,DBLE(P(N+1,4))**2-DBLE(P(IT,4))**2+DPLONG**2)
        DBTOT=-(DPLONG*DBLE(P(IT,4))-DBLE(P(N+1,4))*SQRT(DROOT))/
C    &  SQRT(DBLE(P(N+1,4))**2-DBLE(P(IT,4))**2+DPLONG**2))/
     &  (DPLONG**2+DBLE(P(N+1,4))**2)
        DGAMMA=1.D0/SQRT(1.D0-DBTOT**2)
        DO 400 I = 1,3
  400   DROBO(I+2)=DELTAP(I)/(DGAMMA/(DGAMMA+1.D0)*
     &  (DBLE(P(N+1,4))-DGAMMA*DBLE(P(IT,4)))+DGAMMA*DBLE(P(IT,4)))
        CALL LUDBRB(25,NS,0.,0.,DROBO(3),DROBO(4),DROBO(5))
  410   CONTINUE
C...End phi-correction
      ENDIF
C     CALL GULIST(106,2)

      Q2=Q2LP
C...Hadron remnant and primordial kt
      IPY(47)=N
      CALL LYREMN(IPU1,IPU2)
      IF(IPY(48).EQ.1) THEN
        LST(21)=15
        RETURN
      ENDIF
C     CALL GULIST(107,2)

C...Rearrange partons along strings
      MSTU(24)=0
      CALL LUPREP(0)
      IF(MSTU(24).NE.0) THEN
C       CALL GULIST(188,2)
        IF(LST(3).GE.1) WRITE(6,*) ' LUPREP error MSTU(24)= ',MSTU(24)
        LST(21)=16
        RETURN
      ENDIF
C     CALL GULIST(109,2)

C...Clean up event record -> order:
C...1=inc. lepton; 2=inc. nucleon; 3=exch boson; 4=scat. lepton;
C...5=inc. parton before initial shower; 6=inc. parton at hard scattering
C...after shower; 7,8=first,second parton from hard scattering
C...before final shower
      LST(26)=7
      DO 510 J=1,5
      K(N+1,J)=K(4,J)
  510 P(N+1,J)=P(4,J)
      DO 520 J=1,5
      K(3,J)=K(5,J)
      P(3,J)=P(5,J)
      K(4,J)=K(9,J)
      P(4,J)=P(9,J)
      K(5,J)=K(N+1,J)
      P(5,J)=P(N+1,J)
      K(6,J)=K(NS+3,J)
      P(6,J)=P(NS+3,J)
C     K(7,J)=K(IT,J)
C     P(7,J)=P(IT,J)
      K(7,J)=K(25,J)
      P(7,J)=P(25,J)
      K(8,J)=K(26,J)
      P(8,J)=P(26,J)
  520 CONTINUE
      K(3,3)=1
      K(4,3)=1
      K(6,1)=21
      K(6,3)=5
      K(6,4)=0
      K(6,5)=0
      K(7,1)=21
      K(7,3)=6
      K(7,4)=0
      K(7,5)=0
      K(8,1)=21
      K(8,3)=6
      K(8,4)=0
      K(8,5)=0
C...Activate line with scattered lepton.
      K(4,1)=1
C...Deactivate obsolete lines 9, 10, 21, NS+1 (extra lines with boson)
      K(9,1)=0
      K(10,1)=0
      K(21,1)=0
      IF(K(NS+1,2).EQ.K(3,2)) K(NS+1,1)=0
C...Zero irrelevant lines with K(I,1)<0
      DO 540 I=1,N
      IF(K(I,1).LT.0) THEN
        DO 530 J=1,5
        K(I,J)=0
  530   P(I,J)=0.
      ENDIF
  540 CONTINUE
C     CALL GULIST(110,2)
C...Delete internal parton lines, i.e. with K(I,1)=13,14
      IF(MOD(LST(4)/10,10).EQ.0) THEN
        CALL LTIMEX(T1)
        CALL LUEDIT(14)
        CALL LTIMEX(T2)
C       CALL GULIST(111,2)
      ENDIF
C...Delete empty lines
      CALL LTIMEX(T1)
      CALL LUEDIT(12)
      CALL LTIMEX(T2)
C     CALL GULIST(112,2)

      RETURN
      END

C **********************************************************************

      SUBROUTINE LSCALE(INFIN,QMAX)

      IMPLICIT NONE

C...Give maximum virtuality of partons in parton showers.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

C...  Power in f(x0)=(1-x0)**power used for scale x0*W2
      INTEGER INFIN,I,J
      REAL POWER,QMAX,FOUR,X0,RLU
      DATA POWER/3./
      FOUR(I,J)=P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)

      QMAX=0.1
      IF(LST(8).GE.2.AND.LST(8).LE.5) THEN
C...Parton showers without matrix elements matching
        IF(LST(9).EQ.1) THEN
          QMAX=Q2
        ELSEIF(LST(9).EQ.2) THEN
          QMAX=W2
        ELSEIF(LST(9).EQ.3) THEN
          QMAX=SQRT(W2*Q2)
        ELSEIF(LST(9).EQ.4) THEN
          QMAX=Q2*(1.-X)
        ELSEIF(LST(9).EQ.5) THEN
          QMAX=Q2*(1.-X)*MAX(1.,LOG(1./MAX(1.E-06,X)))
        ELSEIF(LST(9).EQ.6) THEN
          X0=1.D0-(1.D0-DBLE(X))*RLU(0)**(1./(POWER+1.))
          QMAX=X0*W2
        ELSEIF(LST(9).EQ.9) THEN
          QMAX=W2**(2./3.)
        ELSE
          WRITE(6,*) ' Warning, LSCALE: LST(9)=',LST(9),' not allowed'
        ENDIF
      ELSEIF(LST(8).GT.10.AND.LST(24).EQ.1.AND.MOD(LST(8),10).NE.9) THEN
C...Parton showers added to q-event from 1st order matrix elements
        IF(LST(20).LE.1) THEN
          QMAX=PARL(27)*W2
        ELSEIF(LST(20).EQ.2) THEN
          QMAX=PARL(27)*Q2
        ELSEIF(LST(20).EQ.3) THEN
          QMAX=PARL(9)*Q2
        ELSEIF(LST(20).EQ.4) THEN
          QMAX=PARL(9)*Q2
          IF(INFIN.LT.0) QMAX=PARL(27)*Q2/X
        ELSEIF(LST(20).EQ.5) THEN
          QMAX=PARL(9)
          IF(INFIN.LT.0) QMAX=PARL(27)*Q2/X
        ELSEIF(LST(20).EQ.6) THEN
          QMAX=PARL(27)
          IF(INFIN.LT.0) QMAX=PARL(8)*Q2/X
        ELSE
          WRITE(6,*) 'LSCALE: No such jet scheme!'
        ENDIF
      ELSEIF(LST(8).GT.10.AND.MOD(LST(8),10).NE.9) THEN
C...Parton showers added to qg-/qqbar-event from 1st order matrix elements
C...Scale given by invariant mass of final parton pair
        QMAX=P(27,5)**2
        IF(INFIN.LT.0)
     &  QMAX=MAX(ABS(-Q2-2.*FOUR(25,21)),ABS(-Q2-2.*FOUR(26,21)))
      ENDIF
      QMAX=SQRT(QMAX)

      RETURN
      END
C ********************************************************************

      SUBROUTINE LYSSPA(IPU1,IPU2)

      IMPLICIT NONE

C...NEW X REDEFINITION
C...GENERATES SPACELIKE PARTON SHOWERS
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LYPRO2)
*
      COMMON /LYPROC/ ISUB,KFL(3,2),X(2),SH,TH,UH,Q2,XSEC(0:40)
      REAL    X,SH,TH,UH,Q2,XSEC
      INTEGER ISUB,KFL
      SAVE /LYPROC/ 

      COMMON /LYINT1/ XQ(2,-6:6),DSIG(-6:6,-6:6,5),FSIG(10,10,3)
      REAL    XQ,DSIG,FSIG
      SAVE /LYINT1/ 
 


      INTEGER IPU1,IPU2,IFLS,IS,NQ,ILEP,IFLA,NS,IFL,JT,IHFC,IHFX,
     +I,J,JR,IFLB,IHFT,IPO,I1,I2,ITEMP,IT,IKIN,ID1,KN1,KD1,ID2,
     +IR,JB,NTRY,N145, NJR
      REAL XS,ZS,Q2S,TEVS,ROBO,XFS,XFA,XFB,WTAP,WTSF,Z,XE0,XA,
     +TMAX,QMAX,Q2E,B0,XBMIN,XB,Q2B,TEVB,QMASS,XE,WTAPQ,WTSUM,TEVXP,
     +Q2REF,WTRAN,WTZ,XBNEW,RSOFT,ZU,Q2MAX,ALPRAT,THE
      REAL ULMASS,RLU,ULANGL,XT,SEARATIO
 
      DIMENSION IFLS(4),IS(2),XS(2),ZS(2),Q2S(2),TEVS(2),ROBO(5),
     &XFS(2,-6:6),XFA(-6:6),XFB(-6:6),WTAP(-6:6),WTSF(-6:6)
      DOUBLE PRECISION DQ2(3),DSH,DSHZ,DSHR,DPLCM,DPC(3),DPD(4),DMS,
     &DMSMA,DPT2,DPB(4),DBE1(4),DBE2(4),DBEP,DGABEP,DPQ(4),DPQS(2),
     &DM2,DQ2B,DROBO(5),DBEZ,DTEMP
C-GI &DQ23,DPH(4),DM2,DQ2B,DQM2
CJR--begin
      LOGICAL SEAQUARK,SPLIT
      REAL XFT(-6:6)
      REAL XQUARK,XGLUON,XSEA,ZSPLIT,ZSOFT,ZMAX
      COMMON /SEAQTE/ XQUARK,XGLUON,XSEA,ZSPLIT,ZSOFT,ZMAX,SPLIT
      INTEGER LASTFL,SEAFL
      COMMON /FLAVOR/ LASTFL,SEAFL
CJR--end
      DATA IFLA,NQ/0,0/,Z,XE0,XA/3*0./,DSHZ,DMSMA,DPT2,DSHR/4*0.D0/

C...COMMON CONSTANTS, SET UP INITIAL VALUES
      ILEP=0
      IF(IPU1.EQ.0) ILEP=1
      IF(IPU2.EQ.0) ILEP=2
      Q2E=Q2
C-GI  IF(ISET(ISUB).EQ.2.OR.ISET(ISUB).EQ.3) Q2E=Q2E/PYPAR(26)
      IF(ISUB.EQ.27) Q2E=PMAS(23,1)**2
      IF(ISUB.EQ.28) Q2E=PMAS(24,1)**2
      TMAX=ALOG(PYPAR(26)*PYPAR(27)*Q2E/PYPAR(21)**2)
      IF(ILEP.GE.1) THEN
        SH=P(25,5)**2
        IF(N.GE.27) SH=P(27,5)**2
        CALL LSCALE(-1,QMAX)
        Q2E=QMAX**2
        Q2E=MAX(PYPAR(21)**2,MIN(Q2E,(0.95/X(3-ILEP)-1.)*Q2-SH,
     &  Q2/2.+SH))
        TMAX=ALOG(Q2E/PYPAR(21)**2)
      ENDIF
CJR--begin
      IF (MOD(LST(8),10).EQ.4 .OR. MOD(LST(8),10).EQ.5) THEN
         Q2E=PYPAR(22)
         TMAX=ALOG(Q2E/PYPAR(21)**2)
      ELSEIF(PYPAR(26)*Q2E.LT.MAX(PYPAR(22),2.*PYPAR(21)**2).OR.
     &TMAX.LT.0.2) THEN
         RETURN
      ENDIF
CJR--end
      IF(ILEP.EQ.0) XE0=2.*PYPAR(23)/PYVAR(1)
      B0=(33.-2.*IPY(8))/6.
      NS=N
      MSTU(2)=0
CJR--begin
      NTRY=0
  100 N=NS
      NTRY=NTRY+1
      IF (NTRY.GT.100) THEN
         LST(21)=17
         RETURN
      ENDIF
CJR--end
CJR  100 N=NS
CJR--begin
      SEAQUARK=.FALSE.
      SPLIT=.FALSE.
CJR--end
      IF(ILEP.GE.1) THEN
        NQ=IPU2-2
        IF(ILEP.EQ.2) NQ=IPU1+2
        DPQS(1)=DBLE(P(NQ,3))
        DPQS(2)=DBLE(P(NQ,4))
        XBMIN=X(3-ILEP)*MAX(0.5,SH/Q2)
        CALL LYSTFU(IPY(43-ILEP),XBMIN,Q2,XFB)
        DO 110 IFL=-6,6
  110   XQ(3-ILEP,IFL)=XFB(IFL)
      ENDIF
      DO 120 JT=1,2
      IFLS(JT)=KFL(2,JT)
      IF(KFL(2,JT).EQ.21) IFLS(JT)=0
      IFLS(JT+2)=IFLS(JT)
      XS(JT)=X(JT)
      ZS(JT)=1.
      IF(ILEP.EQ.0) Q2S(JT)=PYPAR(26)*Q2E
      TEVS(JT)=TMAX
      DO 120 IFL=-6,6
  120 XFS(JT,IFL)=XQ(JT,IFL)
      IF(ILEP.GE.1) THEN
        Q2S(ILEP)=P(NQ,5)**2
        DQ2(ILEP)=Q2S(ILEP)
        Q2S(3-ILEP)=Q2E
      ENDIF
      DSH=SH
      IHFC=0
      IHFX=0

C...PICK UP LEG WITH HIGHEST VIRTUALITY
  130 CONTINUE
      IF(N.GT.MSTU(4)-10) THEN
        WRITE(6,*) ' LYSSPA: no more memory in LUJETS'
        LST(21)=18
        RETURN
      ENDIF
      DO 133 I=N+1,N+8
      DO 133 J=1,5
      K(I,J)=0
  133 P(I,J)=0.
C     CALL GULIST(21,2)
      N=N+2
      JT=1
      IF((N.GT.NS+2.AND.Q2S(2).GT.Q2S(1).AND.ILEP.EQ.0).OR.ILEP.EQ.1)
     &JT=2
      JR=3-JT
      IFLB=IFLS(JT)
      XB=XS(JT)
      IF(ILEP.GE.1.AND.N.EQ.NS+2) XB=XS(JT)*MAX(SH/Q2,0.5)
      DO 140 IFL=-6,6
  140 XFB(IFL)=XFS(JT,IFL)
      Q2B=Q2S(JT)
      TEVB=TEVS(JT)
      IF(IPY(14).GE.9.AND.N.GT.NS+4) THEN
        Q2B=0.5*(1./ZS(JT)+1.)*Q2S(JT)+0.5*(1./ZS(JT)-1.)*(Q2S(3-JT)-
     &  SNGL(DSH)+SQRT((SNGL(DSH)+Q2S(1)+Q2S(2))**2+8.*Q2S(1)*Q2S(2)*
     &  ZS(JT)/(1.-ZS(JT))))
        TEVB=ALOG(PYPAR(27)*Q2B/PYPAR(21)**2)
      ENDIF
      IF(ILEP.EQ.0) THEN
        DSHR=2.*DSQRT(DSH)
        DSHZ=DSH/DBLE(ZS(JT))
      ELSEIF(ILEP.GE.1) THEN
        DSHZ=DSH
        IF(N.GT.NS+4) DSHZ=(DSH+DQ2(JR)-DQ2(JT))/ZS(JT)-DQ2(JR)+
     &  PYPAR(22)
        DPD(2)=DSHZ+DQ2(JR)+DBLE(PYPAR(22))

        MSTJ(93)=1
        QMASS=ULMASS(IABS(IFLB))
        IF(IABS(IFLB).EQ.0) QMASS=ULMASS(21)
C...CHECK IF QUARK PAIR CREATION ONLY POSSIBILITY
        IF(DQ2(JR).LT.4.*QMASS**2) THEN
          DM2=QMASS**2
          DPC(1)=DQ2(JR)*(DBLE(PYPAR(22))+DM2)**2
          DPC(2)=DPD(2)*(DPD(2)-2D0*PYPAR(22))*(PYPAR(22)+DM2)
          DPC(3)=PYPAR(22)*(DPD(2)-2D0*PYPAR(22))**2
          XE0=1D0-(DPC(2)-DSQRT(DPC(2)**2-4D0*DPC(1)*DPC(3)))/
     &    (2D0*DPC(1))
CJR--begin
          ZMAX=(DPC(2)-DSQRT(DPC(2)**2-4D0*DPC(1)*DPC(3)))/(2D0*DPC(1))
          XE0=XB*(1./ZMAX-1.)
CJR--end
        ELSE
          XE0=1D0-(DPD(2)-2D0*DBLE(PYPAR(22)))*(DPD(2)-DSQRT(DPD(2)**2-
     &    4D0*DQ2(JR)*DBLE(PYPAR(22))))/(2D0*DQ2(JR)*DBLE(PYPAR(22)))
CJR--begin
          ZMAX=(DPD(2)-2D0*DBLE(PYPAR(22)))*(DPD(2)-DSQRT(DPD(2)**2-
     &    4D0*DQ2(JR)*DBLE(PYPAR(22))))/(2D0*DQ2(JR)*DBLE(PYPAR(22)))
          XE0=XB*(1./ZMAX-1.)
CJR--end
        ENDIF
CJR--begin
CJR--          radiated parton energy cut
C        XE0=MAX(XE0,2.*PYPAR(23)/SQRT(W2LP))
CJR--end
      ENDIF
CJR  145 XE=MAX(XE0,XB*(1./(1.-PYPAR(24))-1.))
CJR--begin
      N145=0
145   CONTINUE
      N145=N145+1
      IF (N145.GT.100) THEN
CJR         WRITE(*,*) '145'
         GOTO 100
      ENDIF
      XE=MAX(XE0,XB*(1./(1.-PYPAR(24))-1.))
        ZMAX=XB/(XB+XE)
CJR--end
      IF(XB+XE.GE.0.999) THEN
        Q2B=0.
        GOTO 210
      ENDIF

C...CALCULATE ALTARELLI-PARISI AND STRUCTURE FUNCTION WEIGHTS
      DO 150 IFL=-6,6
      WTAP(IFL)=0.
  150 WTSF(IFL)=0.
      IF(IFLB.EQ.0) THEN
        WTAPQ=16.*(1.-SQRT(XB+XE))/(3.*SQRT(XB))
        DO 160 IFL=-IPY(8),IPY(8)
        IF(IFL.EQ.0) WTAP(IFL)=6.*ALOG((1.-XB)/XE)
  160   IF(IFL.NE.0) WTAP(IFL)=WTAPQ
      ELSE
        WTAP(0)=0.5*XB*(1./(XB+XE)-1.)
        WTAP(IFLB)=8.*ALOG((1.-XB)*(XB+XE)/XE)/3.
      ENDIF
  170 WTSUM=0.
      IF(IHFC.EQ.0) THEN
        DO 180 IFL=-IPY(8),IPY(8)
        WTSF(IFL)=XFB(IFL)/MAX(1E-10,XFB(IFLB))
  180   WTSUM=WTSUM+WTAP(IFL)*WTSF(IFL)
        IF(IABS(IFLB).GE.4.AND.WTSUM.GT.1E3) THEN
          IHFX=1
          DO 185 IFL=-IPY(8),IPY(8)
  185     WTSF(IFL)=WTSF(IFL)*1E3/WTSUM
          WTSUM=1E3
        ENDIF
      ENDIF

C...CHOOSE NEW T AND FLAVOUR
CJR  190 IF(IPY(14).LE.6.OR.IPY(14).GE.9) THEN
CJR--begin
      NJR=0
190   CONTINUE
      SEAQUARK=.FALSE.
      NJR=NJR+1
      IF (NJR.GT.100) THEN
CJR         WRITE(*,*) '190'
         GOTO 100
      ENDIF
CJR--end
      IF(IPY(14).LE.6.OR.IPY(14).GE.9) THEN
        TEVXP=B0/MAX(0.0001,WTSUM)
      ELSE
        TEVXP=B0/MAX(0.0001,5.*WTSUM)
      ENDIF
      TEVB=TEVB*EXP(MAX(-100.,ALOG(RLU(0))*TEVXP))
      Q2REF=PYPAR(21)**2*EXP(TEVB)/PYPAR(27)
      Q2B=Q2REF/PYPAR(27)
      DQ2B=Q2B
      IF(ILEP.GE.1) THEN
        DSHZ=DSH
        IF(N.GT.NS+4) DSHZ=(DSH+DQ2(JR)-DQ2(JT))/DBLE(ZS(JT))-DQ2(JR)+
     &  DQ2B
      ENDIF
CJR--begin -- 
CAE--seaquarks up to LST(12), if the quark density != 0
      IF( LST(35).EQ.1) THEN
         IF( Q2B.LT.PYPAR(22) .AND.
     &       (ABS(IFLB).LE.LST(12).AND.ABS(IFLB).GE.1)) THEN
            Q2REF=MIN(PYPAR(22),Q2S(JT))
            Q2B=MIN(PYPAR(22),Q2S(JT))
            IF(ILEP.GE.1.AND.N.EQ.NS+2) THEN
               XT=X(JT)*(1.+(DSH-Q2B)/DQ2(JR))
            ELSE
               XT=XB
            ENDIF
            CALL LYSTFU(IPY(40+JT),XT,Q2REF,XFT)
            IF(XFT(IFLB).EQ.0.0.AND.XFT(-IFLB).EQ.0.0) THEN
              SEARATIO=1.0
            ELSEIF(XFT(ABS(IFLB)).EQ.0.0) THEN
              SEARATIO=1.0
            ELSE
              SEARATIO=XFT(-ABS(IFLB))/XFT(ABS(IFLB))
            ENDIF
CJR-- (protons only)
            IF (RLU(0).LT.SEARATIO) THEN
               SEAQUARK=.TRUE.
               XQUARK=XT
            ELSE
               Q2B=0.
               SEAQUARK=.FALSE.
            ENDIF
         ENDIF
      ENDIF
CJR--end
      IF(Q2B.LT.PYPAR(22)) THEN
         Q2B=0.
CJR--begin
         SEAQUARK=.FALSE.
CJR--end
      ELSE
        WTRAN=RLU(0)*WTSUM
        IFLA=-IPY(8)-1
  200   IFLA=IFLA+1
        WTRAN=WTRAN-WTAP(IFLA)*WTSF(IFLA)
        IF(IFLA.LT.IPY(8).AND.WTRAN.GT.0.) GOTO 200

CJR--begin
        IF (SEAQUARK) THEN
           SEAFL=-IFLB
           IFLA=0
CT           XE=XB*(1./(1.-0.001)-1.)
        ENDIF
CJR--end

C...CHOOSE Z VALUE AND CORRECTIVE WEIGHT
        IF(IFLB.EQ.0.AND.IFLA.EQ.0) THEN
          Z=1./(1.+((1.-XB)/XB)*(XE/(1.-XB))**RLU(0))
          WTZ=(1.-Z*(1.-Z))**2
        ELSEIF(IFLB.EQ.0) THEN
          Z=XB/(1.-RLU(0)*(1.-SQRT(XB+XE)))**2
          WTZ=0.5*(1.+(1.-Z)**2)*SQRT(Z)
        ELSEIF(IFLA.EQ.0) THEN
          Z=XB*(1.+RLU(0)*(1./(XB+XE)-1.))
          WTZ=1.-2.*Z*(1.-Z)
        ELSE
          Z=1.-(1.-XB)*(XE/((XB+XE)*(1.-XB)))**RLU(0)
          WTZ=0.5*(1.+Z**2)
        ENDIF
CJR--begin
C        IF (SEAQUARK) THEN
C           XSEA=LEXSEA(0.15*XT,Q2B)
C           XE=MIN(XE,XSEA)
C           Z=XT/(XSEA+XT)
C        ENDIF
CJR--end
C...REWEIGHT FIRST LEG BECAUSE OF MODIFIED XB OR CHECK PHASE SPACE
        IF(ILEP.GE.1.AND.N.EQ.NS+2) THEN
          XBNEW=X(JT)*(1.+(DSH-Q2B)/DQ2(JR))
          IF(XBNEW.GT.MIN(Z,0.999)) GOTO 190
          XB=XBNEW
        ENDIF
C...SUM UP SOFT GLUON EMISSION AS EFFECTIVE Z SHIFT
CJR--       should this realy always be done ??
        IF(IPY(15).GE.1) THEN
          RSOFT=6.
          IF(IFLB.NE.0) RSOFT=8./3.
          Z=Z*(TEVB/TEVS(JT))**(RSOFT*XE/((XB+XE)*B0))
          IF(Z.LE.XB) GOTO 190
CJR--begin
          ZSOFT=(TEVB/TEVS(JT))**(RSOFT*XE/((XB+XE)*B0))
          ZMAX=XB/(XB+XE)
CJR--end
        ENDIF
C...CHECK IF HEAVY FLAVOUR BELOW THRESHOLD
        IHFT=0
CIC...Skip for intrinsic charm/bottom simulation, charm quark should
CIC...not come from gluon but is non-perturbative part of proton.
        IF(LST(15).EQ.-4.OR.LST(15).EQ.-5) GOTO 205
        MSTJ(93)=1
        IF(ILEP.GE.1.AND.IABS(IFLB).GE.4.AND.(XFB(IFLB).LT.1E-10.OR.
     &    Q2B.LT.5.*ULMASS(IABS(IFLB))**2)) THEN
          IHFT=1
          IFLA=0
        ENDIF
  205   CONTINUE

C...FOR LEPTOPRODUCTION, CHECK Z AGAINST NEW LIMIT
        IF(ILEP.GE.1) THEN
          DPD(2)=DSHZ+DQ2(JR)+DQ2B
          MSTJ(93)=1
          DM2=ULMASS(IABS(IFLA-IFLB))**2
          IF(IABS(IFLA-IFLB).EQ.0) DM2=ULMASS(21)**2
          DPC(1)=DQ2(JR)*(DQ2B+DM2)**2
          DPC(2)=DPD(2)*(DPD(2)-2D0*DQ2B)*(DQ2B+DM2)
          DPC(3)=DQ2B*(DPD(2)-2D0*DQ2B)**2
          ZU=(DPC(2)-DSQRT(DPC(2)**2-4D0*DPC(1)*DPC(3)))/(2D0*DPC(1))
          IF(Z.GE.ZU) GOTO 190
        ENDIF

C...OPTION WITH EVOLUTION IN KT2=(1-Z)Q2:
        IF(IPY(14).GE.5.AND.IPY(14).LE.6.AND.N.LE.NS+4) THEN
C...CHECK THAT (Q2)LAST BRANCHING < (Q2)HARD
          IF(Q2B/(1.-Z).GT.PYPAR(26)*Q2) GOTO 190
        ELSEIF(IPY(14).GE.3.AND.IPY(14).LE.6.AND.N.GE.NS+6) THEN
C...CHECK THAT Z,Q2 COMBINATION IS KINEMATICALLY ALLOWED
          Q2MAX=0.5*(1./ZS(JT)+1.)*DQ2(JT)+0.5*(1./ZS(JT)-1.)*
     &    (DQ2(3-JT)-DSH+SQRT((DSH+DQ2(1)+DQ2(2))**2+8.*DQ2(1)*DQ2(2)*
     &    ZS(JT)/(1.-ZS(JT))))
          IF(Q2B/(1.-Z).GE.Q2MAX) GOTO 190

        ELSEIF(IPY(14).EQ.7.OR.IPY(14).EQ.8) THEN
C...OPTION WITH ALPHAS((1-Z)Q2): DEMAND KT2 > CUTOFF, REWEIGHT
          IF((1.-Z)*Q2B.LT.PYPAR(22)) GOTO 190
          ALPRAT=TEVB/(TEVB+ALOG(1.-Z))
          IF(ALPRAT.LT.5.*RLU(0)) GOTO 190
          IF(ALPRAT.GT.5.) WTZ=WTZ*ALPRAT/5.
        ENDIF

C...WEIGHTING WITH NEW STRUCTURE FUNCTIONS
        CALL LYSTFU(IPY(40+JT),XB,Q2REF,XFB)
        XA=XB/Z
        CALL LYSTFU(IPY(40+JT),XA,Q2REF,XFA)
        IF(IHFT.EQ.1.OR.IHFX.EQ.1) THEN
           IF(XFA(IFLA).LT.1E-10) IHFC=1
           GOTO 210
        ELSEIF(XFB(IFLB).LT.1E-20) THEN
          GOTO 100
        ENDIF
        IF(WTZ*XFA(IFLA)/XFB(IFLB).LT.RLU(0)*WTSF(IFLA)) THEN
          IF(ILEP.GE.1.AND.N.EQ.NS+2) GOTO 145
          GOTO 170
        ENDIF
CJR--begin
        IF (SEAQUARK) THEN
           SPLIT=.TRUE.
           XGLUON=XA
           XSEA=XA-XB
           ZSPLIT=Z
           SEAQUARK=.FALSE.
        ENDIF
CJR--end
      ENDIF

210   CONTINUE
      IF(N.EQ.NS+4-2*MIN(1,ILEP)) THEN
C...DEFINE TWO HARD SCATTERERS IN THEIR CM-FRAME
        DQ2(JT)=Q2B
        IF(IPY(14).GE.3.AND.IPY(14).LE.6) DQ2(JT)=Q2B/(1.-Z)
        IF(ILEP.EQ.0) THEN
          DPLCM=DSQRT((DSH+DQ2(1)+DQ2(2))**2-4.*DQ2(1)*DQ2(2))/DSHR
          DO 220 JR=1,2
          I=NS+2*JR-1
          IPO=19+2*JR
          K(I,1)=14
          K(I,2)=IFLS(JR+2)
          IF(IFLS(JR+2).EQ.0) K(I,2)=21
          K(I,3)=0
          K(I,4)=IPO
          K(I,5)=IPO
          P(I,1)=0.
          P(I,2)=0.
          P(I,3)=DPLCM*(-1)**(JR+1)
          P(I,4)=(DSH+DQ2(3-JR)-DQ2(JR))/DSHR
          P(I,5)=-SQRT(SNGL(DQ2(JR)))
          K(I+1,1)=-1
          K(I+1,2)=K(IPO+1,2)
          K(I+1,3)=I
          K(I+1,4)=0
          K(I+1,5)=0
          P(I+1,1)=0.
          P(I+1,2)=0.
          P(I+1,3)=IPO
          P(I+1,4)=IPO
          P(I+1,5)=0.
          P(IPO+1,1)=I
          P(IPO+1,2)=I
          K(IPO,4)=MOD(K(IPO,4),MSTU(5))+I*MSTU(5)
          K(IPO,5)=MOD(K(IPO,5),MSTU(5))+I*MSTU(5)
  220     CONTINUE
        ELSE
C..LEPTOPRODUCTION EVENTS: BOSON AND HADRON REST FRAME
          I1=NS+2*ILEP-1
          I2=NS-2*ILEP+5
          DO 225 ITEMP=NS+1,NS+4
          DO 225 J=1,5
          K(ITEMP,J)=0
  225     P(ITEMP,J)=0.
          DO 230 J=1,5
  230     P(I1,J)=P(NQ,J)
          K(NS+1,1)=11
          K(NS+3,1)=14
          IF(ILEP.EQ.2) THEN
            K(NS+1,1)=14
            K(NS+3,1)=11
          ENDIF
          K(NS+2,1)=-1
          K(NS+4,1)=-1
          K(NS+1,3)=0
          K(NS+2,3)=NS+1
          K(NS+3,3)=0
          K(NS+4,3)=NS+3
          K(I1,2)=KFL(2,ILEP)
          K(I2,2)=KFL(2,3-ILEP)
          DPD(1)=DSH+DQ2(1)+DQ2(2)
          DPD(3)=(3-2*ILEP)*DSQRT(DPD(1)**2-4D0*DQ2(1)*DQ2(2))
          P(I2,3)=(DPQS(2)*DPD(3)-DPQS(1)*DPD(1))/
     &    (2D0*DQ2(JR))
          P(I2,4)=(DPQS(1)*DPD(3)-DPQS(2)*DPD(1))/
     &    (2D0*DQ2(JR))
          P(I2,5)=-SQRT(SNGL(DQ2(3-ILEP)))
          P(I2+1,3)=MAX(IPU1,IPU2)
          P(I2+1,4)=MAX(IPU1,IPU2)
          K(I2,4)=K(I2,4)-MOD(K(I2,4),MSTU(5))+MAX(IPU1,IPU2)
          K(I2,5)=K(I2,5)-MOD(K(I2,5),MSTU(5))+MAX(IPU1,IPU2)
          P(26-2*ILEP,1)=I2
          P(26-2*ILEP,2)=I2
          K(25-2*ILEP,4)=MOD(K(25-2*ILEP,4),MSTU(5))+I2*MSTU(5)
          K(25-2*ILEP,5)=MOD(K(25-2*ILEP,5),MSTU(5))+I2*MSTU(5)
          N=N+2
        ENDIF

      ELSEIF(N.GT.NS+4) THEN
C...FIND MAXIMUM ALLOWED MASS OF TIMELIKE PARTON
        DQ2(3)=Q2B
        IF(IPY(14).GE.3.AND.IPY(14).LE.6) DQ2(3)=Q2B/(1.-Z)
        IF(IS(1).GE.1.AND.IS(1).LE.MSTU(4)) THEN
          DPC(1)=P(IS(1),4)
          DPC(3)=0.5*(ABS(P(IS(1),3))+ABS(P(IS(2),3)))
        ELSE
C...IS(1) not initialized
          DPC(1)=0.
          DPC(3)=0.5*(       0.      +ABS(P(IS(2),3)))
        ENDIF
        DPC(2)=P(IS(2),4)
        DPD(1)=DSH+DQ2(JR)+DQ2(JT)
        DPD(2)=DSHZ+DQ2(JR)+DQ2(3)
        DPD(3)=DSQRT(DPD(1)**2-4.*DQ2(JR)*DQ2(JT))
        DPD(4)=DSQRT(DPD(2)**2-4.*DQ2(JR)*DQ2(3))
        IKIN=0
        IF((Q2S(JR).GE.0.5*PYPAR(22).AND.DPD(1)-DPD(3).GE.1D-10*DPD(1))
     &  .OR.ILEP.GE.1) IKIN=1
        IF(IKIN.EQ.0) DMSMA=(DQ2(JT)/DBLE(ZS(JT))-DQ2(3))*(DSH/
     &  (DSH+DQ2(JT))-DSH/(DSHZ+DQ2(3)))
        IF(IKIN.EQ.1) DMSMA=(DPD(1)*DPD(2)-DPD(3)*DPD(4))/(2.*
     &  DQ2(JR))-DQ2(JT)-DQ2(3)

C...GENERATE TIMELIKE PARTON SHOWER (IF REQUIRED)
        IT=N-1
        K(IT,1)=3
        K(IT,2)=IFLB-IFLS(JT+2)
        IF(IFLB-IFLS(JT+2).EQ.0) K(IT,2)=21
        MSTJ(93)=1
        P(IT,5)=ULMASS(K(IT,2))
        IF(SNGL(DMSMA).LE.P(IT,5)**2) GOTO 100
        P(IT,2)=0.
        DO 240 J=1,5
        K(IT+1,J)=0
  240   P(IT+1,J)=0.
        K(IT+1,1)=-1
        K(IT+1,2)=K(IS(JT)+1,2)
        K(IT+1,3)=IT
        IF(MOD(IPY(14),2).EQ.0) THEN
          P(IT,1)=0.
          IF(ILEP.EQ.0) P(IT,4)=(DSHZ-DSH-P(IT,5)**2)/DSHR
          IF(ILEP.GE.1) P(IT,4)=0.5*(P(IS(JT),3)*DPD(2)+
     &    DPQS(1)*(DQ2(JT)+DQ2(3)+P(IT,5)**2))/(P(IS(JT),3)*DPQS(2)-
     &    P(IS(JT),4)*DPQS(1))-DPC(JT)
          P(IT,3)=SQRT(MAX(0.,P(IT,4)**2-P(IT,5)**2))
          CALL LUSHOW(IT,0,SQRT(MIN(SNGL(DMSMA),PYPAR(25)*Q2)))
          IF(N.GE.IT+2) P(IT,5)=P(IT+2,5)
          IF(N.GT.MSTU(4)-10) THEN
            WRITE(6,*) ' LYSSPA: no more memory in LUJETS'
            LST(21)=19
            RETURN
          ENDIF
          DO 243 I=N+1,N+8
          DO 243 J=1,5
          K(I,J)=0
  243     P(I,J)=0.
        ENDIF

C...RECONSTRUCT KINEMATICS OF BRANCHING: TIMELIKE PARTON SHOWER
        DMS=P(IT,5)**2
        IF(IKIN.EQ.0.AND.ILEP.EQ.0) DPT2=(DMSMA-DMS)*(DSHZ+DQ2(3))/
     &  (DSH+DQ2(JT))
        IF(IKIN.EQ.1.AND.ILEP.EQ.0) DPT2=(DMSMA-DMS)*(0.5*DPD(1)*
     &  DPD(2)+0.5*DPD(3)*DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)+DMS))/
     &  (4.*DSH*DPC(3)**2)
        IF(IKIN.EQ.1.AND.ILEP.GE.1) DPT2=(DMSMA-DMS)*(0.5*DPD(1)*
     &  DPD(2)+0.5*DPD(3)*DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)+DMS))/
     &  DPD(3)**2
        IF(DPT2.LT.0.) GOTO 100
        K(IT,3)=N+1
        P(IT,1)=SQRT(SNGL(DPT2))
        IF(ILEP.EQ.0) THEN
          DPB(1)=(0.5*DPD(2)-DPC(JR)*(DSHZ+DQ2(JR)-DQ2(JT)-DMS)/
     &    DSHR)/DPC(3)-DPC(3)
          P(IT,3)=DPB(1)*(-1)**(JT+1)
          P(IT,4)=(DSHZ-DSH-DMS)/DSHR
        ELSE
          DPC(3)=DQ2(JT)+DQ2(3)+DMS
          DPB(2)=DPQS(2)*DBLE(P(IS(JT),3))-DPQS(1)*DPC(JT)
          DPB(1)=0.5D0*(DPC(JT)*DPD(2)+DPQS(2)*DPC(3))/DPB(2)-
     &    DBLE(P(IS(JT),3))
          P(IT,3)=DPB(1)
          P(IT,4)=0.5D0*(DBLE(P(IS(JT),3))*DPD(2)+
     &    DPQS(1)*DPC(3))/DPB(2)-DPC(JT)
        ENDIF
        IF(N.GE.IT+2) THEN
          MSTU(1)=IT+2
          DPB(1)=DSQRT(DPB(1)**2+DPT2)
          DPB(2)=DSQRT(DPB(1)**2+DMS)
          DPB(3)=P(IT+2,3)
          DPB(4)=DSQRT(DPB(3)**2+DMS)
          DBEZ=(DPB(4)*DPB(1)-DPB(3)*DPB(2))/(DPB(4)*DPB(2)-DPB(3)*
     &    DPB(1))
          CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,0.D0,0.D0,DBEZ)
          THE=ULANGL(P(IT,3),P(IT,1))
          CALL LUDBRB(MSTU(1),MSTU(2),THE,0.,0.D0,0.D0,0.D0)
        ENDIF

C...RECONSTRUCT KINEMATICS OF BRANCHING: SPACELIKE PARTON
        K(N+1,1)=14
        K(N+1,2)=IFLB
        IF(IFLB.EQ.0) K(N+1,2)=21
        K(N+1,3)=0
CJR--begin 
CJR--       give all radiated partons 5 as mother particle 
        K(N+1,3)=5
CJR--end
        P(N+1,1)=P(IT,1)
        P(N+1,2)=0.
        P(N+1,3)=P(IT,3)+P(IS(JT),3)
        P(N+1,4)=P(IT,4)+P(IS(JT),4)
        P(N+1,5)=-SQRT(SNGL(DQ2(3)))
        DO 250 J=1,5
        K(N+2,J)=0
  250   P(N+2,J)=0.
        K(N+2,1)=-1
        K(N+2,2)=K(IS(JT)+1,2)
        K(N+2,3)=N+1

C...DEFINE COLOUR FLOW OF BRANCHING
        K(IS(JT),1)=14
        K(IS(JT),3)=N+1
        ID1=IT
      KN1=ISIGN(500+IABS(K(N+1,2)),2*K(N+1,2)+1)
      KD1=ISIGN(500+IABS(K(ID1,2)),2*K(ID1,2)+1)
      IF(K(N+1,2).EQ.21) KN1=500
      IF(K(ID1,2).EQ.21) KD1=500
        IF((KN1.GE.501.AND.KD1.GE.501).OR.(KN1.LT.0.AND.
     &  KD1.EQ.500).OR.(KN1.EQ.500.AND.KD1.EQ.500.AND.
     &  RLU(0).GT.0.5).OR.(KN1.EQ.500.AND.KD1.LT.0))
     &  ID1=IS(JT)
        ID2=IT+IS(JT)-ID1
        P(N+2,3)=ID1
        P(N+2,4)=ID2
        P(ID1+1,1)=N+1
        P(ID1+1,2)=ID2
        P(ID2+1,1)=ID1
        P(ID2+1,2)=N+1
        K(N+1,4)=K(N+1,4)-MOD(K(N+1,4),MSTU(5))+ID1
        K(N+1,5)=K(N+1,5)-MOD(K(N+1,5),MSTU(5))+ID2
        K(ID1,4)=MOD(K(ID1,4),MSTU(5))+(N+1)*MSTU(5)
        K(ID1,5)=MOD(K(ID1,5),MSTU(5))+ID2*MSTU(5)
        K(ID2,4)=MOD(K(ID2,4),MSTU(5))+ID1*MSTU(5)
        K(ID2,5)=MOD(K(ID2,5),MSTU(5))+(N+1)*MSTU(5)
        N=N+2
C     CALL GULIST(22,2)

C...BOOST TO NEW CM-FRAME
        MSTU(1)=NS+1
        IF(ILEP.EQ.0) THEN
          CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,
     &    -DBLE(P(N-1,1)+P(IS(JR),1))/DBLE(P(N-1,4)+P(IS(JR),4)),
     &    0.D0,-DBLE(P(N-1,3)+P(IS(JR),3))/DBLE(P(N-1,4)+P(IS(JR),4)))
          IR=N-1+(JT-1)*(IS(1)-N+1)
          CALL LUDBRB(MSTU(1),MSTU(2),
     &    -ULANGL(P(IR,3),P(IR,1)),PARU(2)*RLU(0),0.D0,0.D0,0.D0)
        ELSE
C...REORIENTATE EVENT WITHOUT CHANGING THE BOSON FOUR MOMENTUM
          DO 260 J=1,4
  260     DPQ(J)=P(NQ,J)
          DBE1(4)=DPQ(4)+DBLE(P(N-1,4))
          DO 270 J=1,3,2
  270     DBE1(J)=-(DPQ(J)+DBLE(P(N-1,J)))/DBE1(4)
          DTEMP=1.D0-DBE1(1)**2-DBE1(3)**2
          IF(DTEMP.LE.0.D0) THEN
            LST(21)=20
            IF(LST(3).GE.1) WRITE(6,*) ' Warning from LYSSPA: sqrt of',
     &      DTEMP,' New event generated.'
            RETURN
          ENDIF
          DBE1(4)=1.D0/DSQRT(DTEMP)
          DBEP=DBE1(1)*DPQ(1)+DBE1(3)*DPQ(3)
          DGABEP=DBE1(4)*(DBE1(4)*DBEP/(1D0+DBE1(4))+DPQ(4))
          DO 280 J=1,3,2
  280     DPQ(J)=DPQ(J)+DGABEP*DBE1(J)
          DPQ(4)=DBE1(4)*(DPQ(4)+DBEP)
          DPC(1)=DSQRT(DPQ(1)**2+DPQ(3)**2)
          DBE2(4)=-(DPQ(4)*DPC(1)-DPQS(2)*DSQRT(DPQS(2)**2+DPC(1)**2-
     &    DPQ(4)**2))/(DPC(1)**2+DPQS(2)**2)
          THE=ULANGL(SNGL(DPQ(3)),SNGL(DPQ(1)))
          DBE2(1)=DBE2(4)*DSIN(DBLE(THE))
          DBE2(3)=DBE2(4)*DCOS(DBLE(THE))
          DBE2(4)=1D0/(1D0-DBE2(1)**2-DBE2(3)*2)

C...CONSTRUCT THE COMBINED BOOST
          DPB(1)=DBE1(4)**2*DBE2(4)/(1D0+DBE1(4))
          DPB(2)=DBE1(1)*DBE2(1)+DBE1(3)*DBE2(3)
          DPB(3)=DBE1(4)*DBE2(4)*(1D0+DPB(2))
          DO 290 JB=1,3,2
  290     DROBO(JB+2)=(DBE1(4)*DBE2(4)*DBE1(JB)+DBE2(4)*DBE2(JB)+
     &    DPB(1)*DBE1(JB)*DPB(2))/DPB(3)
          CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,DROBO(3),0.D0,DROBO(5))
          IF(ILEP.EQ.1) THE=ULANGL(P(NS+1,3),P(NS+1,1))
          IF(ILEP.EQ.2) THE=PARU(1)+ULANGL(P(NS+3,3),P(NS+3,1))
          CALL LUDBRB(MSTU(1),MSTU(2),-THE,PARU(2)*RLU(0),0D0,0D0,0D0)
        ENDIF
        MSTU(1)=0
      ENDIF

C...SAVE QUANTITIES, LOOP BACK
      IS(JT)=N-1
      IF(ILEP.EQ.2.AND.N.EQ.NS+4) IS(JT)=N-3
      Q2S(JT)=Q2B
      DQ2(JT)=Q2B
      IF(IPY(14).GE.3.AND.IPY(14).LE.6) DQ2(JT)=Q2B/(1.-Z)
      DSH=DSHZ
      IF(Q2B.GE.0.5*PYPAR(22)) THEN
        IFLS(JT+2)=IFLS(JT)
        IFLS(JT)=IFLA
        XS(JT)=XA
        ZS(JT)=Z
        DO 300 IFL=-6,6
  300   XFS(JT,IFL)=XFA(IFL)
        TEVS(JT)=TEVB
      ELSE
        IF(JT.EQ.1) IPU1=N-1
        IF(JT.EQ.2) IPU2=N-1
      ENDIF
      IF(MAX(IABS(1-ILEP)*Q2S(1),MIN(1,2-ILEP)*Q2S(2)).GE.0.5*PYPAR(22)
     &.OR.N.LE.NS+2) GOTO 130

      IF(ILEP.EQ.0) THEN
C...BOOST HARD SCATTERING PARTONS TO FRAME OF SHOWER INITIATORS
        DO 310 J=1,3
  310   DROBO(J+2)=(P(NS+1,J)+P(NS+3,J))/(P(NS+1,4)+P(NS+3,4))
        DO 320 J=1,5
  320   P(N+2,J)=P(NS+1,J)
        MSTU(1)=N+2
        MSTU(2)=N+2
        CALL LUDBRB(N+2,N+2,0.,0.,-DROBO(3),-DROBO(4),-DROBO(5))
        ROBO(2)=ULANGL(P(N+2,1),P(N+2,2))
        ROBO(1)=ULANGL(P(N+2,3),SQRT(P(N+2,1)**2+P(N+2,2)**2))
        MSTU(1)=4
        MSTU(2)=NS
        CALL LUDBRB(4,NS,ROBO(1),ROBO(2),DROBO(3),DROBO(4),DROBO(5))
        MSTU(1)=0
        MSTU(2)=0
      ENDIF

C...STORE USER INFORMATION
      K(21,1)=14
      IF(ILEP.NE.0) K(21,1)=11
      K(23,1)=14
      K(21,3)=NS+1
      K(23,3)=NS+3
      DO 330 JT=1,2
      KFL(1,JT)=IFLS(JT)
      IF(IFLS(JT).EQ.0) KFL(1,JT)=21
  330 PYVAR(30+JT)=XS(JT)

      DO 340 I=NS+1,N
      DO 340 J=1,5
  340 V(I,J)=0.
  
CJR--begin
      LASTFL=IFLA
CJR--end

      RETURN
      END

C **********************************************************************

      SUBROUTINE LYREMN(IPU1,IPU2)

      IMPLICIT NONE

C...ADDS ON TARGET REMNANTS (ONE OR TWO FROM EACH SIDE) AND
C...INCLUDES PRIMORDIAL KT.
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LYPRO2)
*
      COMMON /LYPROC/ ISUB,KFL(3,2),X(2),SH,TH,UH,Q2,XSEC(0:40)
      REAL    X,SH,TH,UH,Q2,XSEC
      INTEGER ISUB,KFL
      SAVE /LYPROC/ 


      INTEGER IPU1,IPU2,KFLCH,KFLSP,IPU,IQ,ILEP,IP,NS,I,J,
     +JT,IFLS,IMIN,IMAX,IS,KFI2,NTRY
      INTEGER LUCOMP
      REAL CHI,PMS,ROBO,PEI,PE,PZI,PZ,SHS,PZH,PEH,PTSPL,
     +PHISPL,SHR,PMMIN,PW1,PEF,PZF,PT2,SHH,PHIPT,RQP,SINTH,
     +PEM,PZM
      REAL ULMASS,ULANGL,AMK42,AMK32
      DIMENSION KFLCH(2),KFLSP(2),CHI(2),PMS(6),IS(2),ROBO(5)
      DOUBLE PRECISION DBETAX,DBETAZ,DROBO(5)
      DATA IPU,IQ/0,0/,PEI,PE,PZI,PZ,SHS,PZH,PEH/7*0./

C...FIND EVENT TYPE, SET POINTERS
      IF(IPU1.EQ.0.AND.IPU2.EQ.0) RETURN
      ILEP=0
      IF(IPU1.EQ.0) ILEP=1
      IF(IPU2.EQ.0) ILEP=2
      IF(ISUB.EQ.7) ILEP=-1
      IF(ILEP.EQ.1) IQ=21
      IF(ILEP.EQ.2) IQ=23
      IP=MAX(IPU1,IPU2)
      NS=N

C...DEFINE INITIAL PARTONS, INCLUDING PRIMORDIAL KT
  100 DO 120 I=3,4
      IF(I.EQ.3) IPU=IPU1
      IF(I.EQ.4) IPU=IPU2
      K(I,1)=21
      K(I,3)=I-2
      DO 110 J=1,5
  110 P(I,J)=0.
      IF(ISUB.EQ.7) THEN
        K(I,2)=21
        SHS=0.
      ELSEIF(IPU.NE.0) THEN
        K(I,2)=K(IPU,2)
        P(I,5)=P(IPU,5)
        CALL LPRIKT(PARL(3),PTSPL,PHISPL)
        P(I,1)=PTSPL*COS(PHISPL)
        P(I,2)=PTSPL*SIN(PHISPL)
        PMS(I-2)=P(I,5)**2+P(I,1)**2+P(I,2)**2
      ELSE
        K(I,2)=K(IQ,2)
        P(I,5)=-SQRT(Q2)
        PMS(I-2)=-Q2
        SHS=(1.-X(5-I))*Q2/X(5-I)+PYVAR(7-I)**2
      ENDIF
  120 CONTINUE

C...KINEMATICS CONSTRUCTION FOR INITIAL PARTONS
      IF(ILEP.EQ.0) SHS=PYVAR(31)*PYVAR(32)*PYVAR(2)+
     &(P(3,1)+P(4,1))**2+(P(3,2)+P(4,2))**2
      SHR=SQRT(MAX(0.,SHS))
      IF(ILEP.EQ.0) THEN
        IF((SHS-PMS(1)-PMS(2))**2-4.*PMS(1)*PMS(2).LE.0.) GOTO 100
        P(3,4)=0.5*(SHR+(PMS(1)-PMS(2))/SHR)
        P(3,3)=SQRT(MAX(0.,P(3,4)**2-PMS(1)))
        P(4,4)=SHR-P(3,4)
        P(4,3)=-P(3,3)
      ELSEIF(ILEP.EQ.1) THEN
        P(3,4)=P(IQ,4)
        P(3,3)=P(IQ,3)
        P(4,4)=P(IP,4)
        P(4,3)=P(IP,3)
      ELSEIF(ILEP.EQ.2) THEN
        P(3,4)=P(IP,4)
        P(3,3)=P(IP,3)
        P(4,4)=P(IQ,4)
        P(4,3)=P(IQ,3)
      ENDIF

C...TRANSFORM PARTONS TO OVERALL CM-FRAME (NOT FOR LEPTOPRODUCTION)
      IF(ILEP.EQ.0) THEN
        MSTU(1)=3
        MSTU(2)=4
        DROBO(3)=(P(3,1)+P(4,1))/SHR
        DROBO(4)=(P(3,2)+P(4,2))/SHR
        CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,-DROBO(3),-DROBO(4),0.D0)
        ROBO(2)=ULANGL(P(3,1),P(3,2))
        CALL LUDBRB(MSTU(1),MSTU(2),0.,-ROBO(2),0.D0,0.D0,0.D0)
        ROBO(1)=ULANGL(P(3,3),P(3,1))
        CALL LUDBRB(MSTU(1),MSTU(2),-ROBO(1),0.,0.D0,0.D0,0.D0)
        MSTU(2)=MAX(IPY(47),IPU1,IPU2)
        CALL LUDBRB(MSTU(1),MSTU(2),
     &  ROBO(1),ROBO(2),DROBO(3),DROBO(4),0.D0)
        DROBO(5)=MAX(-0.999999,MIN(0.999999,(PYVAR(31)-PYVAR(32))/
     &  (PYVAR(31)+PYVAR(32))))
        CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,0.D0,0.D0,DROBO(5))
        MSTU(1)=0
        MSTU(2)=0
      ENDIF

C...CHECK INVARIANT MASS OF REMNANT SYSTEM:
C...HADRONIC EVENTS OR LEPTOPRODUCTION
      IF(ILEP.LE.0) THEN
        IF(IPY(12).LE.0.OR.ISUB.EQ.7) PYVAR(33)=0.
        IF(IPY(12).LE.0.OR.ISUB.EQ.7) PYVAR(34)=0.
        PEH=P(3,4)+P(4,4)+0.5*PYVAR(1)*(PYVAR(33)+PYVAR(34))
        PZH=P(3,3)+P(4,3)+0.5*PYVAR(1)*(PYVAR(33)-PYVAR(34))
        SHH=(PYVAR(1)-PEH)**2-(P(3,1)+P(4,1))**2-(P(3,2)+P(4,2))**2-
     &  PZH**2
        MSTJ(93)=1
        AMK32=ULMASS(K(3,2))
        MSTJ(93)=1
        AMK42=ULMASS(K(4,2))
        PMMIN=P(1,5)+P(2,5)+AMK32+AMK42
        IF(SHR.GE.PYVAR(1).OR.SHH.LE.(PMMIN+PYPAR(12))**2) THEN
          IPY(48)=1
          RETURN
        ENDIF
        SHR=SQRT(SHH+(P(3,1)+P(4,1))**2+(P(3,2)+P(4,2))**2)
      ELSE
        PEI=P(IQ,4)+P(IP,4)
        PZI=P(IQ,3)+P(IP,3)
        PMS(ILEP)=MAX(0.,PEI**2-PZI**2+P(5-ILEP,1)**2+P(5-ILEP,2)**2)
        MSTJ(93)=1
        PMMIN=P(3-ILEP,5)+ULMASS(K(5-ILEP,2))+SQRT(PMS(ILEP))
        IF(SHR.LE.PMMIN+PYPAR(12)) THEN
          IPY(48)=1
          RETURN
        ENDIF
      ENDIF

C...  SUBDIVIDE REMNANT IF NECESSARY, STORE FIRST PARTON
CJR--     begin
CJR--     try to find kinematically allowed solution 
CJR--     no more than 100 times
      NTRY=0
  130 CONTINUE
      NTRY=NTRY+1
      IF(NTRY.GT.100) THEN
        IPY(48)=1
        RETURN
      ENDIF
CJR--     end 
      I=NS-1
      DO 160 JT=1,2
      IF(JT.EQ.ILEP) GOTO 160
      IF(JT.EQ.1) IPU=IPU1
      IF(JT.EQ.2) IPU=IPU2
      CALL LYSPLI(IPY(40+JT),KFL(1,JT),KFLCH(JT),KFLSP(JT))
      I=I+2
      IS(JT)=I
      K(I,1)=3
      K(I,2)=KFLSP(JT)
      K(I,3)=JT
      MSTJ(93)=1
      P(I,5)=ULMASS(K(I,2))
CJR--
      KFI2=LUCOMP(K(I,2))
      IF (KFI2.EQ.90) THEN
         P(I,5)=P(I,5)-2.*PARL(20)
      ELSEIF (1.LE.KFI2 .AND. KFI2.LE.6) THEN
         P(I,5)=P(I,5)-PARL(20)
      ENDIF
CJR--
C...FIRST PARTON COLOUR CONNECTIONS AND TRANSVERSE MASS
      K(I+1,1)=-1
      K(I+1,3)=I
      K(I+1,2)=1000
      IF(IPY(34).GE.1) K(I+1,2)=1000+JT
      DO 140 J=1,5
  140 P(I+1,J)=0.
      IF(KFLSP(JT).EQ.21) THEN
        P(I+1,3)=IPU
        P(I+1,4)=IPU
        P(IPU+1,1)=I
        P(IPU+1,2)=I
        K(I,4)=IPU+IPU*MSTU(5)
        K(I,5)=IPU+IPU*MSTU(5)
        K(IPU,4)=MOD(K(IPU,4),MSTU(5))+I*MSTU(5)
        K(IPU,5)=MOD(K(IPU,5),MSTU(5))+I*MSTU(5)
      ELSE
        IFLS=(3-ISIGN(1,KFLSP(JT)*(1102-IABS(KFLSP(JT)))))/2
        P(I+1,IFLS+2)=IPU
        P(IPU+1,3-IFLS)=I
        K(I,IFLS+3)=IPU
        K(IPU,6-IFLS)=MOD(K(IPU,6-IFLS),MSTU(5))+I*MSTU(5)
      ENDIF
      IF(KFLCH(JT).EQ.0) THEN
        P(I,1)=-P(JT+2,1)
        P(I,2)=-P(JT+2,2)
        PMS(JT)=P(I,5)**2+P(I,1)**2+P(I,2)**2
      ELSE
C...WHEN EXTRA REMNANT PARTON OR HADRON: FIND RELATIVE PT, STORE
C...PRIMORDIAL KT SPLIT SHARED BETWEEN REMNANTS
        CALL LPRIKT(PARL(14),PTSPL,PHISPL)
C...RELATIVE DISTRIBUTION OF ENERGY; EXTRA PARTON COLOUR CONNECTION
        CALL LREMH(0,PTSPL,KFLSP(JT),KFLCH(JT),CHI(JT))
        P(I,1)=-P(JT+2,1)*(1.-CHI(JT))+PTSPL*COS(PHISPL)
        P(I,2)=-P(JT+2,2)*(1.-CHI(JT))+PTSPL*SIN(PHISPL)
        PMS(JT+2)=P(I,5)**2+P(I,1)**2+P(I,2)**2
        I=I+2
        DO 150 J=1,5
        K(I,J)=0
        K(I+1,J)=0
        P(I,J)=0.
  150   P(I+1,J)=0.
        K(I,1)=1
        K(I,2)=KFLCH(JT)
        K(I,3)=JT
        MSTJ(93)=1
        P(I,5)=ULMASS(K(I,2))
CJR--
        KFI2=LUCOMP(K(I,2))
        IF (KFI2.EQ.90) THEN
           P(I,5)=P(I,5)-2.*PARL(20)
        ELSEIF (1.LE.KFI2 .AND. KFI2.LE.6) THEN
           P(I,5)=P(I,5)-PARL(20)
        ENDIF
CJR--
        P(I,1)=-P(JT+2,1)*CHI(JT)-PTSPL*COS(PHISPL)
        P(I,2)=-P(JT+2,2)*CHI(JT)-PTSPL*SIN(PHISPL)
        PMS(JT+4)=P(I,5)**2+P(I,1)**2+P(I,2)**2
C...end of update
        PMS(JT)=PMS(JT+4)/CHI(JT)+PMS(JT+2)/(1.-CHI(JT))
        K(I+1,1)=-1
        K(I+1,3)=I
        K(I+1,2)=1000
        IF(IPY(34).GE.1) K(I+1,2)=1000+JT
        IF((IABS(KFLCH(JT)).GE.1.AND.IABS(KFLCH(JT)).LE.8).OR.
     &  IABS(KFLCH(JT)).EQ.21.OR.LUCOMP(IABS(KFLCH(JT))).EQ.90) THEN
          IFLS=(3-ISIGN(1,KFLCH(JT)*(1102-IABS(KFLCH(JT)))))/2
          P(I+1,IFLS+2)=IPU
          P(IPU+1,3-IFLS)=I
          K(I,1)=3
          K(I,IFLS+3)=IPU
          K(IPU,6-IFLS)=MOD(K(IPU,6-IFLS),MSTU(5))+I*MSTU(5)
        ELSE
          IF(IPY(34).GE.1) THEN
            K(I,1)=1
            K(I,3)=JT
          ENDIF
        ENDIF
      ENDIF
  160 CONTINUE
      IF(SHR.LE.SQRT(PMS(1))+SQRT(PMS(2))) GOTO 130
      N=I+1

C...RECONSTRUCT KINEMATICS OF REMNANTS
      DO 170 JT=1,2
      IF(JT.EQ.ILEP) GOTO 170
      PE=0.5*(SHR+(PMS(JT)-PMS(3-JT))/SHR)
      PZ=SQRT(PE**2-PMS(JT))
      IF(KFLCH(JT).EQ.0) THEN
        P(IS(JT),4)=PE
        P(IS(JT),3)=PZ*(-1)**(JT-1)
      ELSE
        PW1=CHI(JT)*(PE+PZ)
        P(IS(JT)+2,4)=0.5*(PW1+PMS(JT+4)/PW1)
        P(IS(JT)+2,3)=0.5*(PW1-PMS(JT+4)/PW1)*(-1)**(JT-1)
        P(IS(JT),4)=PE-P(IS(JT)+2,4)
        P(IS(JT),3)=PZ*(-1)**(JT-1)-P(IS(JT)+2,3)
      ENDIF
  170 CONTINUE

C     CALL GULIST(31,2)
C...HADRONIC EVENTS: BOOST REMNANTS TO CORRECT LONGITUDINAL FRAME
      IF(ILEP.LE.0) THEN
        MSTU(1)=NS+1
        CALL LUDBRB(MSTU(1),MSTU(2),
     &  0.,0.,0.D0,0.D0,-DBLE(PZH)/(DBLE(PYVAR(1))-DBLE(PEH)))
        MSTU(1)=0
C...LEPTOPRODUCTION EVENTS: BOOST COLLIDING SUBSYSTEM
      ELSE
        IMIN=21
        IMAX=MAX(IP,IPY(47))
        PEF=SHR-PE
        PZF=PZ*(-1)**(ILEP-1)
        PT2=P(5-ILEP,1)**2+P(5-ILEP,2)**2
        PHIPT=ULANGL(P(5-ILEP,1),P(5-ILEP,2))
        CALL LUDBRB(IMIN,IMAX,0.,-PHIPT,0.D0,0.D0,0.D0)
        RQP=P(IQ,3)*(PT2+PEI**2)-P(IQ,4)*PEI*PZI
        SINTH=P(IQ,4)*SQRT(PT2*(PT2+PEI**2)/(RQP**2+PT2*
     &  P(IQ,4)**2*PZI**2))*SIGN(1.,-RQP)
        CALL LUDBRB(IMIN,IMAX,ASIN(SINTH),0.,0.D0,0.D0,0.D0)
        DBETAX=(-DBLE(PEI)*PZI*SINTH+
     &  SQRT(DBLE(PT2)*(PT2+PEI**2-(PZI*SINTH)**2)))/
     &  (DBLE(PT2)+PEI**2)
        CALL LUDBRB(IMIN,IMAX,0.,0.,DBETAX,0.D0,0.D0)
        CALL LUDBRB(IMIN,IMAX,0.,PHIPT,0.D0,0.D0,0.D0)
        PEM=P(IQ,4)+P(IP,4)
        PZM=P(IQ,3)+P(IP,3)
        DBETAZ=(-DBLE(PEM)*PZM+
     &  PZF*SQRT(DBLE(PZF)**2+PEM**2-PZM**2))/(DBLE(PZF)**2+PEM**2)
        CALL LUDBRB(IMIN,IMAX,0.,0.,0.D0,0.D0,DBETAZ)
C...Avoid double application of kt
        P(4,1)=0.
        P(4,2)=0.
        CALL LUDBRB(3,4,ASIN(SINTH),0.,DBETAX,0.D0,0.D0)
        CALL LUDBRB(3,4,0.,PHIPT,0.D0,0.D0,DBETAZ)
      ENDIF

      RETURN
      END

C **********************************************************************

      SUBROUTINE LYSPLI(KPART,KFLIN,KFLCH,KFLSP)

      IMPLICIT NONE

C...IN CASE OF A HADRON REMNANT WHICH IS MORE COMPLICATED THAN JUST A
C...QUARK OR A DIQUARK, SPLIT IT INTO TWO (PARTONS OR HADRON + PARTON).
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      
      INTEGER KPART,KFLIN,KFLCH,KFLSP,IFLIN,KSIGN,IFL,IDUM
      REAL RLU,R
      
      IFLIN=KFLIN
      KSIGN=ISIGN(1,KPART)
      IFL=KFLIN*KSIGN
      KFLCH=0
      IDUM=0

      IF(LST(14).EQ.0) THEN
C...If baryon production from remnant excluded, remnant is antiflavour
        KFLSP=-KFLIN
        IF(KFLIN.EQ.21) KFLSP=21
        RETURN
      ENDIF

      IF(IABS(KPART).EQ.211) THEN
C...DECOMPOSE PI+ (PI-).
        IF(IFL.EQ.2) THEN
C...VALENCE U (UBAR) REMOVED.
          KFLSP=-1*KSIGN
        ELSEIF(IFL.EQ.-1) THEN
C...VALENCE D (DBAR) REMOVED.
          KFLSP=2*KSIGN
        ELSEIF(KFLIN.EQ.21) THEN
C...GLUON REMOVED.
          R=2.*RLU(0)
          IF(R.LT.1.) THEN
            KFLCH=2*KSIGN
            KFLSP=-1*KSIGN
          ELSE
            KFLCH=-1*KSIGN
            KFLSP=2*KSIGN
          ENDIF
        ELSEIF((IFL.GE.1.AND.IFL.LE.8).AND.IFL.NE.2) THEN
C...SEA QUARK (ANTIQUARK) REMOVED.
          CALL LUKFDI(-IFLIN,2*KSIGN,IDUM,KFLCH)
          KFLSP=-1*KSIGN
        ELSEIF((IFL.GE.-8.AND.IFL.LE.-1).AND.IFL.NE.-1) THEN
C...SEA ANTIQUARK (QUARK) REMOVED.
          CALL LUKFDI(-IFLIN,-1*KSIGN,IDUM,KFLCH)
          KFLSP=2*KSIGN
        ENDIF

      ELSEIF(IABS(KPART).EQ.2212) THEN
C...DECOMPOSE PROTON (ANTIPROTON).
        IF(IFL.EQ.2) THEN
C...VALENCE U (UBAR) REMOVED.
          R=4.*RLU(0)
          IF(R.LT.3.) THEN
            KFLSP=2101*KSIGN
          ELSE
            KFLSP=2103*KSIGN
          ENDIF
        ELSEIF(IFL.EQ.1) THEN
C...VALENCE D (DBAR) REMOVED.
          KFLSP=2203*KSIGN
        ELSEIF(KFLIN.EQ.21) THEN
C...GLUON REMOVED.
          R=6.*RLU(0)
          IF(R.LT.3.) THEN
            KFLCH=2*KSIGN
            KFLSP=2101*KSIGN
          ELSEIF(R.LT.4.) THEN
            KFLCH=2*KSIGN
            KFLSP=2103*KSIGN
          ELSE
            KFLCH=1*KSIGN
            KFLSP=2203*KSIGN
          ENDIF
        ELSEIF(IFL.GT.2) THEN
C...SEA QUARK (ANTIQUARK) REMOVED.
          R=6*RLU(0)
          IF(R.LT.3.) THEN
            CALL LUKFDI(-IFLIN,2*KSIGN,IDUM,KFLCH)
            KFLSP=2101*KSIGN
          ELSEIF(R.LT.4.) THEN
            CALL LUKFDI(-IFLIN,2*KSIGN,IDUM,KFLCH)
            KFLSP=2103*KSIGN
          ELSE
            CALL LUKFDI(-IFLIN,1*KSIGN,IDUM,KFLCH)
            KFLSP=2203*KSIGN
          ENDIF
        ELSEIF(IFL.LT.0) THEN
C...SEA ANTIQUARK (QUARK) REMOVED.
  100     R=6*RLU(0)
          IF(R.LT.3.) THEN
            CALL LUKFDI(2101*KSIGN,-IFLIN,IDUM,KFLCH)
            KFLSP=2*KSIGN
          ELSEIF(R.LT.4.) THEN
            CALL LUKFDI(2103*KSIGN,-IFLIN,IDUM,KFLCH)
            KFLSP=2*KSIGN
          ELSE
            CALL LUKFDI(2203*KSIGN,-IFLIN,IDUM,KFLCH)
            KFLSP=1*KSIGN
          ENDIF
          IF(KFLCH.EQ.0) GOTO 100
        ENDIF

      ELSEIF(IABS(KPART).EQ.2112) THEN
C...DECOMPOSE NEUTRON (ANTINEUTRON).
        IF(IFL.EQ.2) THEN
C...VALENCE U (UBAR) REMOVED.
          KFLSP=1103*KSIGN
        ELSEIF(IFL.EQ.1) THEN
C...VALENCE D (DBAR) REMOVED.
          R=4.*RLU(0)
          IF(R.LT.3.) THEN
            KFLSP=2101*KSIGN
          ELSE
            KFLSP=2103*KSIGN
          ENDIF
        ELSEIF(KFLIN.EQ.21) THEN
C...GLUON REMOVED.
          R=6.*RLU(0)
          IF(R.LT.2.) THEN
            KFLCH=2*KSIGN
            KFLSP=1103*KSIGN
          ELSEIF(R.LT.5.) THEN
            KFLCH=1*KSIGN
            KFLSP=2101*KSIGN
          ELSE
            KFLCH=1*KSIGN
            KFLSP=2103*KSIGN
          ENDIF
        ELSEIF(IFL.GT.2) THEN
C...SEA QUARK (ANTIQUARK) REMOVED.
          R=6*RLU(0)
          IF(R.LT.2.) THEN
            CALL LUKFDI(-IFLIN,2*KSIGN,IDUM,KFLCH)
            KFLSP=1103*KSIGN
          ELSEIF(R.LT.5.) THEN
            CALL LUKFDI(-IFLIN,1*KSIGN,IDUM,KFLCH)
            KFLSP=2101*KSIGN
          ELSE
            CALL LUKFDI(-IFLIN,1*KSIGN,IDUM,KFLCH)
            KFLSP=2103*KSIGN
          ENDIF
        ELSEIF(IFL.LT.0) THEN
C...SEA ANTIQUARK (QUARK) REMOVED.
  110     R=6*RLU(0)
          IF(R.LT.2.) THEN
            CALL LUKFDI(1103*KSIGN,-IFLIN,IDUM,KFLCH)
            KFLSP=2*KSIGN
          ELSEIF(R.LT.5.) THEN
            CALL LUKFDI(2101*KSIGN,-IFLIN,IDUM,KFLCH)
            KFLSP=1*KSIGN
          ELSE
            CALL LUKFDI(2103*KSIGN,-IFLIN,IDUM,KFLCH)
            KFLSP=1*KSIGN
          ENDIF
          IF(KFLCH.EQ.0) GOTO 110
        ENDIF
      ENDIF

      RETURN
      END

C#######################################################################
C
C   The following routines are slightly modified minimization routines
C   from the MINUIT program package.
C
C **********************************************************************

      SUBROUTINE LMCMND

      IMPLICIT NONE

C...This is the MINUIT routine COMAND.
CC        GETS IFORMATION FROM /LMINUI/  AND TAKES APPROPRIATE ACTION,
CC        EITHER DIRECTLY BY SKIPPING TO THE CORRESPONDING CODE IN
CC        LMCMND, OR BY SETTING UP A CALL TO A SUBROUTINE
CC
      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      REAL XKIN,UKIN,WKIN,AIN,BIN,RELERR,RELUP,RELER2,FCNMAX
      INTEGER MAXFIN
      SAVE /LMINUI/

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/


      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC
      
      REAL ERP,ERN
      INTEGER MAXINT,NPAR
      REAL  X,XT,DIRIN
      INTEGER MAXEXT     ,NU
      REAL U,WERR
      INTEGER LCODE,LCORSP,LIMSET
      REAL ALIM,BLIM
      REAL V
      INTEGER IPFIX,NPFIX
      REAL XS,XTS,DIRINS
      REAL GRDS,G2S,GSTEPS,ABERFS
      REAL Y
      INTEGER JH,JL
      REAL GIN,GRD,G2,GSTEP,ABERF
      REAL P,PSTAR,PSTST,PBAR,PRHO
      REAL VT
*****      COMMON
      INTEGER ISYSRD   ,ISYSWR   ,ISYSPU
      REAL TITLE,DATE
      INTEGER ISW,NBLOCK
      REAL EPSI     ,APSI     ,VTEST
      INTEGER NSTEPQ     ,NFCN ,NFCNMX
      REAL CWORD    ,CWORD2   ,CWORD3    ,WORD7
      REAL AMIN     ,UP      ,SIGMA,EPSMAC
      INTEGER NEWMIN    ,ITAUR
      INTEGER IT,IFLAG
      REAL FVAL3,FMAX,F  

      FVAL3 = 2.0*AMIN+1.0
C                                        . . . . . . . . . . ERROR DEF
      WORD7(1)=RELUP*ABS(AMIN)
      UP = WORD7(1)
      IF (UP .LE. 0.)  UP = 1.0
      IF (ISW(2) .GE. 1)  CALL LMPRIN(1,AMIN)
      WORD7(1)=MAXFIN
      WORD7(2)=RELERR*UP
      NFCNMX = WORD7(1) + 0.5
      IF (NFCNMX .LE. 0)  NFCNMX = 1000
      EPSI = WORD7(2)
      IF (EPSI .LE. 0.)  EPSI = 0.1 * UP
      NEWMIN = 0
      ITAUR = 0
      ISW(1) = 0
      CALL LMSIMP
      IF(ABS(DIRIN(1)).LE.ABS(EPSMAC*X(1)).AND.
     &   ABS(DIRIN(2)).LE.ABS(EPSMAC*X(2))) THEN
        IF(LST3.GE.1) WRITE(6,2100)
        GOTO 500
      ENDIF
      WORD7(1)=MAXFIN
      RELERR=RELER2*RELERR
      WORD7(2)=RELERR*UP
      NFCNMX = WORD7(1) + 0.5
      IF (NFCNMX .LE. 0)  NFCNMX = 1000
      EPSI = WORD7(2)
      IF (EPSI .LE. 0.)  EPSI = 0.1 * UP
      CALL LMSIMP
  500 FCNMAX=ABS(AMIN)
      IF(ISW(1).GE.1) THEN
        IF(LST3.GE.1) WRITE(6,2200)
        FCNMAX=FCNMAX*1.25
      ENDIF
      FMAX=ABS(AMIN)
C                                        . . . . . . . . . . END, EXIT
      WORD7(1)=0.
 1100 IT = WORD7(1) + 0.5
      IF (FVAL3 .EQ. AMIN .OR. IT .GT. 0) RETURN
      IFLAG = 3
      CALL LSIGMX(NPAR,GIN,F,U,IFLAG)
      NFCN = NFCN + 1
      IF(LST3.GE.1.AND.ABS(F).GT.FMAX) WRITE(6,2300) F
      RETURN

 2100 FORMAT(' Warning: Stepsizes are less than machine accuracy ',
     &'times variable values. No further minimization attempted.')
 2200 FORMAT(' Warning: Simplex minimization has not converged ',
     &'properly.',/,10X,'Returned maximum increased by a factor 1.25.')
 2300 FORMAT(' Warning from LMCMND: function at minimum, ',E12.4,
     &'  is smaller than stored minimum.')

      END

C **********************************************************************

      SUBROUTINE LMINTO(PINT)

      IMPLICIT NONE

C...This is the MINUIT routine INTOEX.
CC        TRANSFORMS FROM INTERNAL COORDINATES (PINT) TO EXTERNAL
CC        PARAMETERS (U).   THE MINIMIZING ROUTINES WHICH WORK IN
CC        INTERNAL COORDINATES CALL THIS ROUTINE BEFORE CALLING FCN.

      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC

      REAL ERP,ERN
      INTEGER MAXINT,NPAR
      REAL  X,XT,DIRIN
      INTEGER MAXEXT     ,NU
      REAL U,WERR
      INTEGER LCODE,LCORSP,LIMSET
      REAL ALIM,BLIM
      REAL V
      INTEGER IPFIX,NPFIX
      REAL XS,XTS,DIRINS
      REAL GRDS,G2S,GSTEPS,ABERFS
      REAL Y
      INTEGER JH,JL
      REAL GIN,GRD,G2,GSTEP,ABERF
      REAL P,PSTAR,PSTST,PBAR,PRHO
      REAL VT
*****      COMMON
      INTEGER ISYSRD   ,ISYSWR   ,ISYSPU
      REAL TITLE,DATE
      INTEGER ISW,NBLOCK
      REAL EPSI     ,APSI     ,VTEST
      INTEGER NSTEPQ     ,NFCN ,NFCNMX
      REAL CWORD    ,CWORD2   ,CWORD3    ,WORD7
      REAL AMIN     ,UP      ,SIGMA,EPSMAC
      INTEGER NEWMIN    ,ITAUR
      INTEGER I,J
      REAL PINT,AL
      
      DIMENSION PINT(2)
      
      DO 100 I= 1, NU
      J = LCORSP(I)
      IF ( J )  100,100,50
   50 CONTINUE
      IF (LCODE(I) .EQ. 1)  GO TO 80
      AL = ALIM(I)
      U(I) = AL + 0.5 *(SIN(PINT(J)) +1.0) * (BLIM(I) -AL)
      GO TO 100
   80 U(I) = PINT(J)
  100 CONTINUE
      RETURN
      END

C **********************************************************************

      SUBROUTINE LMIDAT

      IMPLICIT NONE

C...This is the MINUIT routine MIDATA.
CC        GETS PARAMETERS FROM /LMINUI/  AND /LMINUC/
CC        AND SETS UP THE STARTING PARAMETER LISTS.
CC        CONTROL THEN PASSES TO LMCMND FOR READING THE COMMAND "CARDS".
CC

      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC
      
      REAL ERP,ERN
      INTEGER MAXINT,NPAR
      REAL  X,XT,DIRIN
      INTEGER MAXEXT     ,NU
      REAL U,WERR
      INTEGER LCODE,LCORSP,LIMSET
      REAL ALIM,BLIM
      REAL V
      INTEGER IPFIX,NPFIX
      REAL XS,XTS,DIRINS
      REAL GRDS,G2S,GSTEPS,ABERFS
      REAL Y
      INTEGER JH,JL
      REAL GIN,GRD,G2,GSTEP,ABERF
      REAL P,PSTAR,PSTST,PBAR,PRHO
      REAL VT
*****      COMMON
      INTEGER ISYSRD   ,ISYSWR   ,ISYSPU
      REAL TITLE,DATE
      INTEGER ISW,NBLOCK
      REAL EPSI     ,APSI     ,VTEST
      INTEGER NSTEPQ     ,NFCN ,NFCNMX
      REAL CWORD    ,CWORD2   ,CWORD3    ,WORD7
      REAL AMIN     ,UP      ,SIGMA,EPSMAC
      INTEGER NEWMIN    ,ITAUR

      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      REAL XKIN,UKIN,WKIN,AIN,BIN,RELERR,RELUP,RELER2,FCNMAX
      INTEGER MAXFIN
      SAVE /LMINUI/

      COMMON /LMINUC/ NAMKIN(4),NAM(30)
      CHARACTER*10 NAMKIN,NAM
      SAVE /LMINUC/

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      

      REAL XK,UK,WK,A,B,VERSN,TIME,SAV,SAV2,VPLU,VMINU
      INTEGER MNINIT,IFATAL,NINT,I,IUNIT,K
      CHARACTER*10 NAMK,BLANK
      CHARACTER XTITLE*60
      REAL LMPINT
      DATA BLANK/'          '/
      DATA XTITLE/' FIND MINIMUM OF -(DIFFERENTIAL CROSS SECTION)'/
      DATA MNINIT/0/,IFATAL,NINT/0,0/
C                                        . INITIALIZE NEW DATA BLOCK . .
      IF (MNINIT .EQ. 0)  NBLOCK=0
      MNINIT = 1
      NBLOCK = NBLOCK + 1
      VERSN = 11.79
      IF(LST3.GE.5) THEN
        WRITE (ISYSWR,1004)  MAXINT,MAXEXT,VERSN,NBLOCK
        WRITE (ISYSWR,1005)
      ENDIF
      DO 50 I= 1, 7
   50 ISW(I) = 0
      SIGMA = 0.
      CALL LTIMEX(TIME)
      IF(LST3.GE.5) THEN
        WRITE (ISYSWR,1110)  XTITLE,TIME,EPSMAC
        WRITE (ISYSWR,1005)
      ENDIF
      NPFIX = 0
      NINT = 0
      NU = 0
      NPAR = 0
      IFATAL = 0
      IF(LST3.GE.5) WRITE (ISYSWR,1005)
      DO 100 I= 1, MAXEXT
      U(I) = 0.0
      NAM(I) = BLANK
      ERP(I) = 0.0
      ERN(I) = 0.0
      LCODE(I) = 0
  100 LCORSP (I) = 0
      UP = 1.0
      ISW(5) = 1
      IUNIT = ISYSRD
C                                        . . . READ PARAMETER CARDS . .
      ENTRY LMIDA2
      DO 200 I= 1, 200
      IF(I.GE.5) GOTO 250
      XK=XKIN(I)
      NAMK=NAMKIN(I)
      UK=UKIN(I)
      WK=WKIN(I)
      A=AIN(I)
      B=BIN(I)
      K = XK + 0.1
      NU = MAX0(NU,K)
      IF (K .LE. 0)  GO TO 250
      IF (K .LE. MAXEXT)  GO TO 115
      IFATAL = IFATAL + 1
      IF(LST3.GE.1) THEN
        WRITE (ISYSWR,1009)  K,MAXEXT
        WRITE (ISYSWR,1002)  K,NAMK,UK,WK,A,B
      ENDIF
      GO TO 200
  115 CONTINUE
      IF(NAM(K).EQ.BLANK) GO TO 120
C         PREVIOUSLY DEFINED PARAMETER IS BEING REDEFINED
      IF(LST3.GE.1) WRITE(ISYSWR,1007)
      IF(WERR(K).GT..0) NINT=NINT-1
  120 CONTINUE
      NAM(K) = NAMK
      U(K) = UK
      WERR(K) = WK
      IF (WK .GT. 0.0)  GO TO 122
C                                        . . . FIXED PARAMETER . . . .
      IF(LST3.GE.5) WRITE (ISYSWR, 1002)  K,NAMK,UK
      LCODE(K) = 0
      GO TO 160
C                                        . . . VARIABLE PARAMETER . . .
  122 IF(LST3.GE.5) WRITE (ISYSWR, 1002)  K,NAMK,UK,WK,A,B
      NINT = NINT + 1
      ISW(2) = 0
      IF  (A)  140,130,140
  130 IF  (B)  140,135,140
  135 LCODE(K) = 1
      GO TO 160
  140 IF (B-A)  145,142,150
  142 IFATAL = IFATAL + 1
      IF(LST3.GE.1) WRITE (ISYSWR,1010)
      GO TO 150
  145 SAV = B
      B = A
      A = SAV
      IF(LST3.GE.1) WRITE (ISYSWR,1003)
  150 ALIM(K) = A
      BLIM(K) = B
      LCODE(K) = 4
      IF ((B-U(K))*(U(K)-A))  153,155,160
  153 IFATAL = IFATAL + 1
      IF(LST3.GE.1) WRITE (ISYSWR,1011)
      GO TO 160
  155 IF(LST3.GE.1) WRITE (ISYSWR,1006)
  160 CONTINUE
  200 CONTINUE
      IFATAL = IFATAL + 1
      IF(LST3.GE.1) WRITE (ISYSWR,1012)
C                                       . . . END PARAMETER CARDS
C                                       . .   . STOP IF FATAL ERROR
  250 IF(LST3.GE.5) WRITE (ISYSWR,1005)
      IF (NINT .LE. MAXINT)  GO TO 253
      IF(LST3.GE.1) WRITE (ISYSWR,1008)  NINT,MAXINT
      IFATAL = IFATAL + 1
  253 IF (IFATAL .LE. 0)  GO TO 280
      IF(LST3.GE.1) WRITE (ISYSWR,1013)  IFATAL
      IF(LST3.GE.2) STOP
C                                       CALCULATE STEP SIZES DIRIN
  280 NPAR = 0
      DO 300 K= 1, NU
      IF (LCODE(K) .LE. 0)  GO TO 300
      NPAR = NPAR + 1
      LCORSP(K) = NPAR
      SAV = U(K)
      X(NPAR) = LMPINT(SAV,K)
      XT(NPAR) = X(NPAR)
      SAV2 = SAV + WERR(K)
      VPLU = LMPINT(SAV2,K) - X(NPAR)
      SAV2 = SAV - WERR(K)
      VMINU = LMPINT(SAV2,K) - X(NPAR)
      DIRIN(NPAR) = 0.5 * (ABS(VPLU) +ABS(VMINU))
      G2(NPAR) = 2.0 / DIRIN(NPAR)**2
      GSTEP(NPAR) = DIRIN(NPAR)
      IF (LCODE(K) .GT. 1)  GSTEP(NPAR) = -GSTEP(NPAR)
  300 CONTINUE
      SIGMA = 1.0E10
      IUNIT = ISYSRD
      RETURN
C...     THE FORMAT BELOW IS MACHINE-DEPENDENT. (A10) , (A6,4X) , ETC.
 1002 FORMAT (I10,2X,A10,2X,2G12.6,2X,2G12.6)
 1003 FORMAT(' WARNING           - ABOVE LIMITS HAVE BEEN REVERSED.')
 1004 FORMAT (1H1/42X,21(1H*)/42X,21H*   D506   MINUIT   */42X,
     112H* DIMENSIONS, I3, 1H/, I3, 2H */   42X,
     1'*  MODIFICATION OF  *',/,42X,
     111H*   VERSION ,F6.2,4H   */42X,16H* DATA BLOCK NO. ,I3,2H *)
 1005 FORMAT (4X,96(1H*))
 1006 FORMAT(' WARNING           - ABOVE PARAMETER IS AT LIMIT ')
 1007 FORMAT(' WARNING  *******  - PARAMETER REQUESTED ON FOLLOWING',
     1' CARD HAS ALREADY APPEARED.  PREVIOUS VALUES IGNORED.')
 1008 FORMAT('0   TOO MANY VARIABLE PARAMETERS.  YOU REQUEST',I5/,
     +'   THIS VERSION OF MINUIT IS ONLY DIMENSIONED FOR',I4//)
 1009 FORMAT('0FATAL ERROR. PARAMETER NUMBER',I11,' GREATER THAN ',
     +'ALLOWED MAXIMUM',I4)
 1010 FORMAT(' FATAL ERROR. UPPER AND LOWER LIMITS ARE EQUAL.')
 1011 FORMAT(' FATAL ERROR. PARAMETER OUTSIDE LIMITS',/)
 1012 FORMAT('0FATAL ERROR. MORE THAN 200 PARAMETER CARDS',/)
 1013 FORMAT(/I5,' FATAL ERRORS ON PARAMETER CARDS.  ABORT.',//)
 1110 FORMAT(5X,A60,5X,'TIME',F8.3,' SECONDS',/,70X,'MACH. PREC.=',
     &E10.2)
      END

C **********************************************************************

      SUBROUTINE LMINEW

      IMPLICIT NONE

C...This is the MINUIT routine MINNEW.
C     C        THIS IS THE MAIN PROGRAM, DISGUISED AS A SUBROUTINE FOR

CC        REASONS OF COMPATIBILITY BETWEEN SYSTEMS.    IT INITIALIZES
CC        SOME CONSTANTS IN COMMON INCLUDING THE LOGICAL I/O UNIT NOS.)
CC        THEN VERIFIES THAT FCN GIVES THE SAME VALUE WHEN CALLED
CC        TWICE WITH THE SAME ARGUMENTS, AND PASSES CONTROL TO LMCMND.
CC

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC

      REAL ERP,ERN
      INTEGER MAXINT,NPAR
      REAL  X,XT,DIRIN
      INTEGER MAXEXT     ,NU
      REAL U,WERR
      INTEGER LCODE,LCORSP,LIMSET
      REAL ALIM,BLIM
      REAL V
      INTEGER IPFIX,NPFIX
      REAL XS,XTS,DIRINS
      REAL GRDS,G2S,GSTEPS,ABERFS
      REAL Y
      INTEGER JH,JL
      REAL GIN,GRD,G2,GSTEP,ABERF
      REAL P,PSTAR,PSTST,PBAR,PRHO
      REAL VT
*****      COMMON
      INTEGER ISYSRD   ,ISYSWR   ,ISYSPU
      REAL TITLE,DATE
      INTEGER ISW,NBLOCK
      REAL EPSI     ,APSI     ,VTEST
      INTEGER NSTEPQ     ,NFCN ,NFCNMX
      REAL CWORD    ,CWORD2   ,CWORD3    ,WORD7
      REAL AMIN     ,UP      ,SIGMA,EPSMAC
      INTEGER NEWMIN    ,ITAUR
      REAL F
      INTEGER I

C     UNIT NUMBERS FOR CARD READER, PRINTER, PUNCH
C
      ISYSRD = 5
      ISYSWR = 6
      ISYSPU = 7
      MAXINT=15
      MAXEXT=30
C                   DETERMINE MACHINE ACCURACY EPSMAC
      EPSMAC = 0.5
      DO 33 I= 1, 100
      EPSMAC = EPSMAC * 0.5
      IF ((1.0+EPSMAC) .EQ. 1.0)  GO TO 35
   33 CONTINUE
      EPSMAC = 1.0E-6
   35 EPSMAC = 2.0 * EPSMAC
C                             . . . . . . . . .
  110 CONTINUE
      NFCN = 1
      CALL LMIDAT
      CALL LMINTO(X)
      IF(LST3.GE.5) WRITE (ISYSWR,120)
  120 FORMAT (/,'0FIRST ENTRY TO FCN ')
      CALL LSIGMX(NPAR,GIN,AMIN,U,1)
      CALL LSIGMX(NPAR,GIN,AMIN,U,4)
      CALL LMPRIN(1,AMIN)
      CALL LSIGMX(NPAR,GIN,F   ,U,4)
      IF  (F .NE. AMIN)  GO TO 160
      NFCN = 3
      CALL LMCMND
      RETURN
  160 CONTINUE
      IF(LST3.GE.1) WRITE (ISYSWR,880) AMIN, F
      IF(LST3.GE.2) STOP
  880 FORMAT('0FOR THE ABOVE VALUES OF THE PARAMETERS, FCN IS TIME-',
     +'DEPENDENT',/,'0F = ',E22.14,' FOR FIRST CALL',/,' F =',E22.14,
     +' FOR SECOND')
      END

C **********************************************************************

      SUBROUTINE LMPRIN  (IKODE,FVAL)

      IMPLICIT NONE

C...This is the MINUIT routine MPRINT.
CC        PRINTS THE VALUES OF THE PARAMETERS AT THE TIME OF THE CALL.
CC        ALSO PRINTS OTHER RELEVANT INFORMATION SUCH AS FUNCTION VALUE,
CC        ESTIMATED DISTANCE TO MINIMUM, PARAMETER ERRORS, STEP SIZES.
CC        ACCORDING TO THE VALUE OF IKODE,THE PRINTOUT IS LONG FORMAT,
CC        SHORT FORMAT, OR MINOS FORMAT (0,1,2)
CC

      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC

      REAL ERP,ERN
      INTEGER MAXINT,NPAR
      REAL  X,XT,DIRIN
      INTEGER MAXEXT     ,NU
      REAL U,WERR
      INTEGER LCODE,LCORSP,LIMSET
      REAL ALIM,BLIM
      REAL V
      INTEGER IPFIX,NPFIX
      REAL XS,XTS,DIRINS
      REAL GRDS,G2S,GSTEPS,ABERFS
      REAL Y
      INTEGER JH,JL
      REAL GIN,GRD,G2,GSTEP,ABERF
      REAL P,PSTAR,PSTST,PBAR,PRHO
      REAL VT
*****      COMMON
      INTEGER ISYSRD   ,ISYSWR   ,ISYSPU
      REAL TITLE,DATE
      INTEGER ISW,NBLOCK
      REAL EPSI     ,APSI     ,VTEST
      INTEGER NSTEPQ     ,NFCN ,NFCNMX
      REAL CWORD    ,CWORD2   ,CWORD3    ,WORD7
      REAL AMIN     ,UP      ,SIGMA,EPSMAC
      INTEGER NEWMIN    ,ITAUR

      COMMON /LMINUC/ NAMKIN(4),NAM(30)
      CHARACTER*10 NAMKIN,NAM
      SAVE /LMINUC/

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      
      INTEGER IKODE,I,L,KOUNT
      REAL FVAL,TI,E,DX,AL,BA,DU1,DU2,X1,X2
      
C                                        . GET TIME AND PRINT HEADINGS .
      CALL LTIMEX(TI)
      IF(LST3.GE.5) WRITE (ISYSWR,1000)
      E = SIGMA
      KOUNT = 0
C                                        . . . LOOP OVER PARAMETERS . .
      DO 200 I= 1, NU
      IF(NAM(I).EQ.'          ') GOTO 200
   20 L = LCORSP(I)
      IF (L .EQ. 0)  GO TO 55
C              VARIABLE PARAMETER.  CALCULATE EXTERNAL ERROR IF V EXISTS
      IF (ISW(2) .LT. 1)  GO TO 27
      DX = SQRT(ABS(V(L,L)*UP))
      IF (LCODE(I) .LE. 1)  GO TO 26
      AL = ALIM(I)
      BA = BLIM(I) - AL
      DU1 = AL + 0.5 *(SIN(X(L)+DX) +1.0) * BA - U(I)
      DU2 = AL + 0.5 *(SIN(X(L)-DX) +1.0) * BA - U(I)
      IF (DX .GT. 1.0)  DU1 = BA
      DX = 0.5 * (ABS(DU1) + ABS(DU2))
   26 WERR(I) = DX
   27 X1 = X(L)
      X2 = DIRIN(L)
      IF (IKODE .LT. 2)  GO TO 29
      X1 = ERP(I)
      X2 = ERN(I)
   29 IF (KOUNT)  30,30,40
   30 KOUNT = 1
      IF(LST3.GE.5)
     &WRITE (ISYSWR,1001) FVAL,NFCN,TI,E, L,I,NAM(I),U(I),WERR(I),X1,X2
      GO TO 45
   40 IF(LST3.GE.5) WRITE (ISYSWR,1002)   L,I,NAM(I),U(I),WERR(I),X1,X2
   45 IF (LCODE(I) .LE. 1)  GO TO 200
      IF(LST3.GE.1.AND. ABS(COS(X(L))) .LT. 0.001)  WRITE (ISYSWR,1004)
      GO TO 200
C                           FIXED PARAMETER.  PRINT ONLY IF IKODE .GT.0
   55 IF (IKODE .EQ. 0)  GO TO 200
      IF (KOUNT)  60,60,70
   60 KOUNT = 1
      IF(LST3.GE.5) WRITE (ISYSWR,1001) FVAL,NFCN,TI,E, L,I,NAM(I),U(I)
      GO TO 200
   70 IF(LST3.GE.5) WRITE (ISYSWR,1003)                   I,NAM(I),U(I)
  200 CONTINUE
      IF(LST3.GE.5.AND.
     &IKODE.GE.1 .AND.ISW(2).GE.1)  WRITE (ISYSWR,1005)  UP
      RETURN
 1000 FORMAT(/ 4X,'FCN VALUE',5X,'CALLS',4X,'TIME',4X,' EDM  ',4X ,
     +'INT.EXT. PARAMETER     VALUE         ERROR      INTERN.VALUE  ',
     +'INT.STEP SIZE')
 1001 FORMAT(E15.7,I7,F9.2,E11.2,I6,I4,1X,A10,4E14.5)
 1002 FORMAT(1H ,41X,I6,I4,1X,A10,4E14.5)
 1003 FORMAT(1H  ,47X  ,I4,1X,A10,4E14.5)
 1004 FORMAT(1H ,52X  ,'WARNING -   - ABOVE PARAMETER IS AT LIMIT.')
 1005 FORMAT(/45X,'ERRORS CORRESPOND TO FUNCTION CHANGE OF ',E12.4)
      END

C **********************************************************************

      REAL FUNCTION LMPINT(PEXTI,I)

C...This is the MINUIT routine PINTF.
CC        CALCULATES THE INTERNAL PARAMETER VALUE LMPINT CORRESPONDING
CC        TO THE EXTERNAL VALUE PEXTI FOR PARAMETER I.
CC
      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC
      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      DATA BIG, SMALL / 1.570796326795 , -1.570796326795 /
      IGO = LCODE(I)
      GO TO (100,200,300,400),IGO
C--       IGO = 1  MEANS NO LIMITS
  100 LMPINT = PEXTI
      GO TO 800
  200 CONTINUE
  300 CONTINUE
C--       IGO = 4  MEANS THERE ARE TWO LIMITS
  400 ALIMI = ALIM(I)
      BLIMI = BLIM(I)
      IF (PEXTI-ALIMI)  440,500,460
  440 A = SMALL
  450 LMPINT = A
      PEXTI = ALIMI + 0.5* (BLIMI-ALIMI) *(SIN(A) +1.0)
      LIMSET=1
      IF(LST3.GE.1) WRITE (ISYSWR,241)  I
      GO TO 800
  460 IF (BLIMI-PEXTI)  470,520,480
  470 A = BIG
      GO TO 450
  480 YY=2.0*(PEXTI-ALIMI)/(BLIMI-ALIMI) - 1.0
      LMPINT = ATAN(YY/SQRT(1.0- YY**2) )
      GO TO 800
  500 LMPINT = SMALL
      GO TO 800
  520 LMPINT = BIG
  800 RETURN
  241 FORMAT(' WARNING - VARIABLE',I3,' HAS BEEN BROUGHT BACK IN',
     +'SIDE LIMITS BY LMPINT.')
      END

C **********************************************************************

      SUBROUTINE LMRAZZ(YNEW,PNEW)

      IMPLICIT NONE

C...This is the MINUIT routine RAZZIA.
CC        CALLED ONLY BY SIMPLEX (AND IMPROV) TO ADD A NEW POINT
CC        AND REMOVE AN OLD ONE FROM THE CURRENT SIMPLEX, AND GET THE
CC        ESTIMATED DISTANCE TO MINIMUM.
CC
      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC

      REAL ERP,ERN
      INTEGER MAXINT,NPAR
      REAL  X,XT,DIRIN
      INTEGER MAXEXT     ,NU
      REAL U,WERR
      INTEGER LCODE,LCORSP,LIMSET
      REAL ALIM,BLIM
      REAL V
      INTEGER IPFIX,NPFIX
      REAL XS,XTS,DIRINS
      REAL GRDS,G2S,GSTEPS,ABERFS
      REAL Y
      INTEGER JH,JL
      REAL GIN,GRD,G2,GSTEP,ABERF
      REAL P,PSTAR,PSTST,PBAR,PRHO
      REAL VT
*****      COMMON
      INTEGER ISYSRD   ,ISYSWR   ,ISYSPU
      REAL TITLE,DATE
      INTEGER ISW,NBLOCK
      REAL EPSI     ,APSI     ,VTEST
      INTEGER NSTEPQ     ,NFCN ,NFCNMX
      REAL CWORD    ,CWORD2   ,CWORD3    ,WORD7
      REAL AMIN     ,UP      ,SIGMA,EPSMAC
      INTEGER NEWMIN    ,ITAUR
      
      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/


      INTEGER I,J,NPARP1
      REAL YNEW,PNEW,US,PBIG,PLIT
      
      DIMENSION PNEW(15)
      DO 10 I=1,NPAR
10    P(I,JH)=PNEW(I)
      Y(JH)=YNEW
      IF(YNEW.GE.AMIN) GO TO 18
      DO 15 I=1,NPAR
15    X(I)=PNEW(I)
      CALL LMINTO(X)
      AMIN=YNEW
      JL=JH
18    CONTINUE
      JH=1
      NPARP1=NPAR+1
20    DO 25 J=2,NPARP1
      IF (Y(J) .GT. Y(JH))  JH = J
25    CONTINUE
      SIGMA = Y(JH) - Y(JL)
      IF (SIGMA .LE. 0.)  GO TO 45
      US = 1.0/SIGMA
      DO 35 I= 1, NPAR
      PBIG = P(I,1)
      PLIT = PBIG
      DO 30 J= 2, NPARP1
      IF (P(I,J) .GT. PBIG)  PBIG = P(I,J)
      IF (P(I,J) .LT. PLIT)  PLIT = P(I,J)
   30 CONTINUE
      DIRIN(I) = PBIG - PLIT
      IF (ITAUR .LT. 1 )  V(I,I) = 0.5*(V(I,I) +US*DIRIN(I)**2)
   35 CONTINUE
   40 RETURN
CMM changed
C   45 IF(LST3.GE.1) WRITE (ISYSWR, 1000)  NPAR
   45 IF(LST3.GE.6) WRITE (ISYSWR, 1000)  NPAR

      GO TO 40
 1000 FORMAT('0***** FUNCTION VALUE DOES NOT SEEM TO DEPEND ON ANY ',
     +'OF THE',I3,' VARIABLE PARAMETERS',/15X ,'VERIFY THAT STEP SIZES',
     +' ARE BIG ENOUGH AND CHECK FCN LOGIC.',/1X,81(1H*)/1X,81(1H*)//)
      END

C **********************************************************************

      SUBROUTINE LMSIMP

      IMPLICIT NONE

C...This is the MINUIT routine SIMPLEX.
CC        PERFORMS A MINIMIZATION USING THE SIMPLEX METHOD OF NELDER
CC        AND MEAD (REF. -- COMP. J. 7,308 (1965)).
      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      REAL XKIN,UKIN,WKIN,AIN,BIN,RELERR,RELUP,RELER2,FCNMAX
      INTEGER MAXFIN
      SAVE /LMINUI/

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      COMMON
     1/LMMINE/ ERP(30)  ,ERN(30)
     2/LMPARI/ X(15)    ,XT(15)   ,DIRIN(15) ,MAXINT     ,NPAR
     3/LMPARE/ U(30)              ,WERR(30)  ,MAXEXT     ,NU
     4/LMLIMI/ ALIM(30) ,BLIM(30) ,LCODE(30) ,LCORSP(30) ,LIMSET
     5/LMVARI/ V(15,15)
     7/LMFIX / IPFIX(15),XS(15)   ,XTS(15)   ,DIRINS(15) ,NPFIX
     7/LMFIX2/ GRDS(15) ,G2S(15)  ,GSTEPS(15),ABERFS(15)
     C/LMCASC/ Y(16)    ,JH       ,JL
     F/LMDERI/ GIN(30)  ,GRD(15)  ,G2(15)    ,GSTEP(15)  ,ABERF(15)
     G/LMSIMV/ P(15,16) ,PSTAR(15),PSTST(15) ,PBAR(15)   ,PRHO(15)
     J/LMVART/ VT(15,15)
      COMMON
     6/LMUNIT/ ISYSRD   ,ISYSWR   ,ISYSPU
     8/LMTITL/ TITLE(13),DATE(2)  ,ISW(7)    ,NBLOCK
     9/LMCONV/ EPSI     ,APSI     ,VTEST     ,NSTEPQ     ,NFCN ,NFCNMX
     A/LMCARD/ CWORD    ,CWORD2   ,CWORD3    ,WORD7(7)
     B/LMMINI/ AMIN     ,UP       ,NEWMIN    ,ITAUR      ,SIGMA,EPSMAC

      REAL ERP,ERN
      INTEGER MAXINT,NPAR
      REAL  X,XT,DIRIN
      INTEGER MAXEXT     ,NU
      REAL U,WERR
      INTEGER LCODE,LCORSP,LIMSET
      REAL ALIM,BLIM
      REAL V
      INTEGER IPFIX,NPFIX
      REAL XS,XTS,DIRINS
      REAL GRDS,G2S,GSTEPS,ABERFS
      REAL Y
      INTEGER JH,JL
      REAL GIN,GRD,G2,GSTEP,ABERF
      REAL P,PSTAR,PSTST,PBAR,PRHO
      REAL VT
*****      COMMON
      INTEGER ISYSRD   ,ISYSWR   ,ISYSPU
      REAL TITLE,DATE
      INTEGER ISW,NBLOCK
      REAL EPSI     ,APSI     ,VTEST
      INTEGER NSTEPQ     ,NFCN ,NFCNMX
      REAL CWORD    ,CWORD2   ,CWORD3    ,WORD7
      REAL AMIN     ,UP      ,SIGMA,EPSMAC
      INTEGER NEWMIN    ,ITAUR
      INTEGER I,IFLAG,NPARP1,NPFN,KG,NS,NF,K,NCYCL,J,JHOLD
      REAL ALPHA,BETA,GAMMA,RHOMIN,RHOMAX,RHO1,RHO2,WG,YNPP1,ABSMIN,
     +     AMING,BESTX,F,SIG2,PB,YSTAR,YSTST,Y1,Y2,RHO,YRHO,YPBAR

      DATA ALPHA,BETA,GAMMA,RHOMIN,RHOMAX / 1.0, 0.5, 2.0, 4.0, 8.0/
      IF (NPAR .LE. 0)  RETURN
      NPFN=NFCN
      NPARP1=NPAR+1
      RHO1 = 1.0 + ALPHA
      RHO2 = RHO1 + ALPHA*GAMMA
      WG = 1.0/FLOAT(NPAR)
      IFLAG=4
      IF(LST3.GE.5) WRITE(ISYSWR,100) EPSI
      DO 2 I= 1, NPAR
      IF (ISW(2) .GE. 1)  DIRIN(I) = SQRT(V(I,I)*UP)
      IF (ABS(DIRIN(I)) .LT. 1.0E-10*ABS(X(I))) DIRIN(I)=1.0E-8*X(I)
      IF(ITAUR.LT. 1)  V(I,I) =    DIRIN(I)**2/UP
    2 CONTINUE
      IF (ITAUR .LT. 1)  ISW(2) = 1
C**       CHOOSE THE INITIAL SIMPLEX USING SINGLE-PARAMETER SEARCHES
    1 CONTINUE
      YNPP1 = AMIN
      JL = NPARP1
      Y(NPARP1) = AMIN
      ABSMIN = AMIN
      DO 10 I= 1, NPAR
      AMING = AMIN
      PBAR(I) = X(I)
      BESTX = X(I)
      KG = 0
      NS = 0
      NF = 0
    4 X(I) = BESTX + DIRIN(I)
      CALL LMINTO(X)
      CALL LSIGMX(NPAR,GIN, F, U, 4)
      NFCN = NFCN + 1
      IF (F .LE. AMING)  GO TO 6
C         FAILURE
      IF (KG .EQ. 1)  GO TO 8
      KG = -1
      NF = NF + 1
      DIRIN(I) = DIRIN(I) * (-0.4)
      IF (NF .LT. 3)  GO TO 4
      NS = 6
C         SUCCESS
    6 BESTX = X(I)
      DIRIN(I) = DIRIN(I) * 3.0
      AMING = F
      KG = 1
      NS = NS + 1
      IF (NS .LT. 6)  GO TO 4
C         LOCAL MINIMUM FOUND IN ITH DIRECTION
    8 Y(I) = AMING
      IF (AMING .LT. ABSMIN)  JL = I
      IF (AMING .LT. ABSMIN)  ABSMIN = AMING
      X(I) = BESTX
      DO 9 K= 1, NPAR
    9 P(K,I) = X(K)
   10 CONTINUE
      JH = NPARP1
      AMIN=Y(JL)
      CALL LMRAZZ(YNPP1,PBAR)
      DO 20 I= 1, NPAR
   20 X(I) = P(I,JL)
      CALL LMINTO(X)
      DO 30 I=1,NPAR
   30 IF(ABS(DIRIN(I)).LE.ABS(EPSMAC*X(I))) DIRIN(I)=4.*EPSMAC*X(I)
      IF (ISW(5) .GE. 1)  CALL LMPRIN(0,AMIN)
      SIGMA = SIGMA * 10.
      SIG2 = SIGMA
      NCYCL=0
C                                        . . . . .  START MAIN LOOP
   50 CONTINUE
C...Change in SIMPLX; error redefined for second call to LMSIMP.
      UP=RELUP*ABS(AMIN)
      EPSI=RELERR*UP
      IF (SIG2 .LT. EPSI .AND. SIGMA.LT.EPSI)     GO TO 76
      SIG2 = SIGMA
      IF ((NFCN-NPFN) .GT. NFCNMX)  GO TO 78
C         CALCULATE NEW POINT * BY REFLECTION
      DO 60 I= 1, NPAR
      PB = 0.
      DO 59 J= 1, NPARP1
   59 PB = PB + WG * P(I,J)
      PBAR(I) = PB - WG * P(I,JH)
   60 PSTAR(I)=(1.+ALPHA)*PBAR(I)-ALPHA*P(I,JH)
      CALL LMINTO(PSTAR)
      CALL LSIGMX(NPAR,GIN,YSTAR,U,4)
      NFCN=NFCN+1
      IF(YSTAR.GE.AMIN) GO TO 70
C         POINT * BETTER THAN JL, CALCULATE NEW POINT **
      DO 61 I=1,NPAR
   61 PSTST(I)=GAMMA*PSTAR(I)+(1.-GAMMA)*PBAR(I)
      CALL LMINTO(PSTST)
      CALL LSIGMX(NPAR,GIN,YSTST,U,4)
      NFCN=NFCN+1
C         TRY A PARABOLA THROUGH PH, PSTAR, PSTST.  MIN = PRHO
      Y1 = (YSTAR-Y(JH)) * RHO2
      Y2 = (YSTST-Y(JH)) * RHO1
      RHO = 0.5 * (RHO2*Y1 -RHO1*Y2) / (Y1 -Y2)
      IF (RHO .LT. RHOMIN)  GO TO 66
      IF (RHO .GT. RHOMAX)  RHO = RHOMAX
      DO 64 I= 1, NPAR
   64 PRHO(I) = RHO*PBAR(I) + (1.0-RHO)*P(I,JH)
      CALL LMINTO(PRHO)
      CALL LSIGMX(NPAR,GIN,YRHO, U,4)
      NFCN = NFCN + 1
      IF (YRHO .LT. Y(JL) .AND. YRHO .LT. YSTST)  GO TO 65
      IF (YSTST .LT. Y(JL))  GO TO 67
      IF (YRHO .GT. Y(JL))  GO TO 66
C         ACCEPT MINIMUM POINT OF PARABOLA, PRHO
   65 CALL LMRAZZ (YRHO,PRHO)
      GO TO 68
   66 IF (YSTST .LT. Y(JL))  GO TO 67
      CALL LMRAZZ(YSTAR,PSTAR)
      GO TO 68
   67 CALL LMRAZZ(YSTST,PSTST)
   68 NCYCL=NCYCL+1
      IF (ISW(5) .LT. 2)  GO TO 50
      IF (ISW(5) .GE. 3 .OR. MOD(NCYCL, 10) .EQ. 0) CALL LMPRIN(0,AMIN)
      GO TO 50
C         POINT * IS NOT AS GOOD AS JL
   70 IF (YSTAR .GE. Y(JH))  GO TO 73
      JHOLD = JH
      CALL LMRAZZ(YSTAR,PSTAR)
      IF (JHOLD .NE. JH)  GO TO 50
C         CALCULATE NEW POINT **
   73 DO 74 I=1,NPAR
   74 PSTST(I)=BETA*P(I,JH)+(1.-BETA)*PBAR(I)
      CALL LMINTO (PSTST)
      CALL LSIGMX(NPAR,GIN,YSTST,U,4)
      NFCN=NFCN+1
      IF(YSTST.GT.Y(JH)) GO TO 1
C     POINT ** IS BETTER THAN JH
      IF (YSTST .LT. AMIN)  GO TO 67
      CALL LMRAZZ(YSTST,PSTST)
      GO TO 50
C                                        . . . . . .  END MAIN LOOP
   76 IF(LST3.GE.5) WRITE(ISYSWR,120)
      GO TO 80
   78 IF(LST3.GE.5) WRITE(ISYSWR,130)
      ISW(1) = 1
   80 DO 82 I=1,NPAR
      PB = 0.
      DO 81 J=1,NPARP1
   81 PB = PB + WG * P(I,J)
   82 PBAR(I) = PB - WG * P(I,JH)
      CALL LMINTO(PBAR)
      CALL LSIGMX(NPAR,GIN,YPBAR,U,IFLAG)
      NFCN=NFCN+1
      IF (YPBAR .LT. AMIN)  CALL LMRAZZ(YPBAR,PBAR)
      CALL LMINTO(X)
      IF (NFCNMX+NPFN-NFCN .LT. 3*NPAR)  GO TO 90
      IF (SIGMA .GT. 2.0*EPSI)  GO TO 1
   90 CALL LMPRIN(1-ITAUR, AMIN)
      RETURN
  100 FORMAT(' START SIMPLEX MINIMIZATION          ',8X   ,'CON',
     +'VERGENCE CRITERION -- ESTIMATED DISTANCE TO MINIMUM (EDM) .LT.',
     +E10.2 )
120   FORMAT(1H ,'SIMPLEX MINIMIZATION HAS CONVERGED')
130   FORMAT(1H ,'SIMPLEX TERMINATES WITHOUT CONVERGENCE')
      END

C#######################################################################
C
C   One- and two-dimensional adaptive Gaussian integration routines.
C
C **********************************************************************

      SUBROUTINE GADAP(A0,B0,F,EPS,SUM)

      IMPLICIT NONE
C
C   PURPOSE           - INTEGRATE A FUNCTION F(X)
C   METHOD            - ADAPTIVE GAUSSIAN
C   USAGE             - CALL GADAP(A0,B0,F,EPS,SUM)
C   PARAMETERS  A0    - LOWER LIMIT (INPUT,REAL)
C               B0    - UPPER LIMIT (INPUT,REAL)
C               F     - FUNCTION F(X) TO BE INTEGRATED. MUST BE
C                       SUPPLIED BY THE USER. (INPUT,REAL FUNCTION)
C               EPS   - DESIRED RELATIVE ACCURACY. IF SUM IS SMALL EPS
C                       WILL BE ABSOLUTE ACCURACY INSTEAD. (INPUT,REAL)
C               SUM   - CALCULATED VALUE FOR THE INTEGRAL (OUTPUT,REAL)
C   PRECISION         - SINGLE
C   REQ'D PROG'S      - F
C   AUTHOR            - T. JOHANSSON, LUND UNIV. COMPUTER CENTER, 1973
C   REFERENCE(S)      - THE AUSTRALIAN COMPUTER JOURNAL,3 P.126 AUG. -71
C
      COMMON/GADAP1/ NUM,IFU
      INTEGER NUM,IFU
      SAVE /GADAP1/

      INTEGER N,I,L
      REAL AA,BB,F1F,F2F,F3F
      REAL A0,B0,F,EPS,SUM,A,B,F1,F2,F3,S
      REAL DSUM,C,RED,W1,U2,SS,SOLD
      EXTERNAL F
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5./18.*(BB-AA)*(F1F+1.6*F2F+F3F)
      IF(EPS.LT.1.0E-8) EPS=1.0E-8
      RED=1.3
      L=1
      I=1
      SUM=0.
      C=SQRT(15.)/5.
      A(1)=A0
      B(1)=B0
      F1(1)=F(0.5*(1+C)*A0+0.5*(1-C)*B0)
      F2(1)=F(0.5*(A0+B0))
      F3(1)=F(0.5*(1-C)*A0+0.5*(1+C)*B0)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5.
      U2=2.*W1-(A(I)+A(I+2))/2.
      F1(I+1)=F(A(I)+B(I)-W1)
      F2(I+1)=F3(I)
      F3(I+1)=F(B(I)-A(I+2)+W1)
      F1(I+2)=F(U2)
      F2(I+2)=F2(I)
      F3(I+2)=F(B(I+2)+A(I+2)-U2)
      F1(I+3)=F(A(I)+A(I+2)-W1)
      F2(I+3)=F1(I)
      F3(I+3)=F(W1)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
      IF(ABS(SOLD-SS).GT.EPS*(1.+ABS(SS))/2.) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
  130 RETURN
      END

C **********************************************************************

      SUBROUTINE GADAP2(A0,B0,FL,FU,F,EPS,SUM)

      IMPLICIT NONE
C
C   PURPOSE           - INTEGRATE A FUNCTION F(X,Y) OF TWO VARIABLES
C   METHOD            - ADAPTIVE GAUSSIAN IN BOTH DIRECTIONS
C   USAGE             - CALL GADAP2(A0,B0,FL,FU,F,EPS,SUM)
C   PARAMETERS  A0    - LOWER X-LIMIT (INPUT,REAL)
C               B0    - UPPER X-LIMIT (INPUT,REAL)
C               FL    - USER SUPPLIED FUNCTION FL(X) GIVING THE LOWER
C                       Y-LIMIT FOR A GIVEN X-VALUE
C                       (INPUT,REAL FUNCTION)
C               FU    - USER SUPPLIED FUNCTION FU(X) GIVING THE UPPER
C                       Y-LIMIT FOR A GIVEN X-VALUE
C                       (INPUT,REAL FUNCTION)
C               F     - USER SUPPLIED FUNCTION F(X,Y) TO BE INTEGRATED
C                       (INPUT,REAL FUNCTION)
C               EPS   - DESIRED ACCURACY (INPUT,REAL)
C               SUM   - CALCULATED VALUE FOR THE INTEGRAL (OUTPUT,REAL)
C   PRECISION         - SINGLE
C   REQ'D PROG'S      - FL,FU,F,GADAPF
C   AUTHOR            - THOMAS JOHANSSON, LDC,1973
C
      COMMON/GADAP1/ NUM,IFU
      INTEGER NUM,IFU
      SAVE /GADAP1/

      INTEGER N,L,I
      REAL AA,BB,F1F,F2F,F3F      
      REAL A0,B0,FL,FU,F,EPS,SUM,A,B,F1,F2,F3,S
      REAL DSUM,RED,C,X,AY,BY,W1,U2,SS,SOLD
      REAL GADAPF 
      EXTERNAL F,FL,FU
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5./18.*(BB-AA)*(F1F+1.6*F2F+F3F)
      IF(EPS.LT.1.0E-8) EPS=1.0E-8
      RED=1.4
      L=1
      I=1
      SUM=0.
      C=SQRT(15.)/5.
      A(1)=A0
      B(1)=B0
      X=0.5*(1+C)*A0+0.5*(1-C)*B0
      AY=FL(X)
      BY=FU(X)
      F1(1)=GADAPF(X,AY,BY,F,EPS)
      X=0.5*(A0+B0)
      AY=FL(X)
      BY=FU(X)
      F2(1)=GADAPF(X,AY,BY,F,EPS)
      X=0.5*(1-C)*A0+0.5*(1+C)*B0
      AY=FL(X)
      BY=FU(X)
      F3(1)=GADAPF(X,AY,BY,F,EPS)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5.
      U2=2.*W1-(A(I)+A(I+2))/2.
      X=A(I)+B(I)-W1
      AY=FL(X)
      BY=FU(X)
      F1(I+1)=GADAPF(X,AY,BY,F,EPS)
      F2(I+1)=F3(I)
      X=B(I)-A(I+2)+W1
      AY=FL(X)
      BY=FU(X)
      F3(I+1)=GADAPF(X,AY,BY,F,EPS)
      X=U2
      AY=FL(X)
      BY=FU(X)
      F1(I+2)=GADAPF(X,AY,BY,F,EPS)
      F2(I+2)=F2(I)
      X=B(I+2)+A(I+2)-U2
      AY=FL(X)
      BY=FU(X)
      F3(I+2)=GADAPF(X,AY,BY,F,EPS)
      X=A(I)+A(I+2)-W1
      AY=FL(X)
      BY=FU(X)
      F1(I+3)=GADAPF(X,AY,BY,F,EPS)
      F2(I+3)=F1(I)
      X=W1
      AY=FL(X)
      BY=FU(X)
      F3(I+3)=GADAPF(X,AY,BY,F,EPS)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
      IF(ABS(SOLD-SS).GT.EPS*(1.+ABS(SS))/2.) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
 130  RETURN
      END

C **********************************************************************


      FUNCTION GADAPF(X,A0,B0,F,EPS)
      IMPLICIT NONE
      COMMON/GADAP1/ NUM,IFU
      INTEGER NUM,IFU
      SAVE /GADAP1/


      INTEGER N,L,I
      REAL GADAPF,X,A0,B0,F,EPS,A,B,F1,F2,F3,S,RED,C,W1,U2,SS,SOLD,SUM
      REAL DSUM
      REAL AA,BB,F1F,F2F,F3F
      
      EXTERNAL F
      DIMENSION A(300),B(300),F1(300),F2(300),F3(300),S(300),N(300)
    1 FORMAT(16H GADAP:I TOO BIG)
      DSUM(F1F,F2F,F3F,AA,BB)=5./18.*(BB-AA)*(F1F+1.6*F2F+F3F)
      IF(EPS.LT.1.0E-8) EPS=1.0E-8
      RED=1.4
      L=1
      I=1
      SUM=0.
      C=SQRT(15.)/5.
      A(1)=A0
      B(1)=B0
      F1(1)=F(X,0.5*(1+C)*A0+0.5*(1-C)*B0)
      F2(1)=F(X,0.5*(A0+B0))
      F3(1)=F(X,0.5*(1-C)*A0+0.5*(1+C)*B0)
      IFU=3
      S(1)=  DSUM(F1(1),F2(1),F3(1),A0,B0)
  100 CONTINUE
      L=L+1
      N(L)=3
      EPS=EPS*RED
      A(I+1)=A(I)+C*(B(I)-A(I))
      B(I+1)=B(I)
      A(I+2)=A(I)+B(I)-A(I+1)
      B(I+2)=A(I+1)
      A(I+3)=A(I)
      B(I+3)=A(I+2)
      W1=A(I)+(B(I)-A(I))/5.
      U2=2.*W1-(A(I)+A(I+2))/2.
      F1(I+1)=F(X,A(I)+B(I)-W1)
      F2(I+1)=F3(I)
      F3(I+1)=F(X,B(I)-A(I+2)+W1)
      F1(I+2)=F(X,U2)
      F2(I+2)=F2(I)
      F3(I+2)=F(X,B(I+2)+A(I+2)-U2)
      F1(I+3)=F(X,A(I)+A(I+2)-W1)
      F2(I+3)=F1(I)
      F3(I+3)=F(X,W1)
      IFU=IFU+6
      IF(IFU.GT.5000) GOTO 130
      S(I+1)=  DSUM(F1(I+1),F2(I+1),F3(I+1),A(I+1),B(I+1))
      S(I+2)=  DSUM(F1(I+2),F2(I+2),F3(I+2),A(I+2),B(I+2))
      S(I+3)=  DSUM(F1(I+3),F2(I+3),F3(I+3),A(I+3),B(I+3))
      SS=S(I+1)+S(I+2)+S(I+3)
      I=I+3
      IF(I.GT.300)GOTO 120
      SOLD=S(I-3)
      IF(ABS(SOLD-SS).GT.EPS*(1.+ABS(SS))/2.) GOTO 100
      SUM=SUM+SS
      I=I-4
      N(L)=0
      L=L-1
  110 CONTINUE
      IF(L.EQ.1) GOTO 130
      N(L)=N(L)-1
      EPS=EPS/RED
      IF(N(L).NE.0) GOTO 100
      I=I-1
      L=L-1
      GOTO 110
  120 WRITE(6,1)
  130 GADAPF=SUM
      EPS=EPS/RED
      RETURN
      END

C ********************************************************************

      SUBROUTINE LNSTRF(X,Q2,XPQ)

      IMPLICIT NONE

C...Structure function per nucleon for a proton/neutron mixture
C...according to defined nucleus.
CGI-001122 Updated to save and mix total d- and u-quark distributions
C...instead of only the valence parts.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/


      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      REAL X,Q2,XPQ,XDV,XUV,XD,XU,XUB,XDB
      DIMENSION XPQ(-6:6)

      CALL LYSTFU(2212,X,Q2,XPQ)

      IF(PARI(11).LE.1.E-06) RETURN
C...Total d- and u-quark densities in proton, save for use in LEPTOX
      XD=XPQ(1)
      XU=XPQ(2)
      PARI(12)=XD
      PARI(13)=XU
CMM-010306 Saving the anti d- and anti u-quark densities
Cstep1
C...anti-d- and anti-u-quark densities in proton, save for use in LEPTOX
      XDB=XPQ(-1)
      XUB=XPQ(-2)
      PARI(42)=XDB
      PARI(43)=XUB
      IF(LST(39).EQ.0) THEN
C...For nuclear target, mix d- and u-quark distributions.
        XPQ(1)=(1.-PARI(11))*XD+PARI(11)*XU
        XPQ(2)=(1.-PARI(11))*XU+PARI(11)*XD
C...mix anti-d- and anti-u-quark distributions.
        XPQ(-1)=(1.-PARI(11))*XDB+PARI(11)*XUB    
        XPQ(-2)=(1.-PARI(11))*XUB+PARI(11)*XDB
      ENDIF
      RETURN
      END

C **********************************************************************

      SUBROUTINE LYSTFU(KF,X,Q2,XPQ)

      IMPLICIT NONE

C...Interface to PYSTFU in PYTHIA 5.7 to get parton density distributions,
C...i.e. momentum weighted probability distributions xq(x,Q2), xg(x,Q2).
C...Also gives intrinsic charm and beauty distributions.
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200) 
      INTEGER MSTP,MSTI
      REAL PARP,PARI
      SAVE /PYPARS/

      COMMON /ARSTRF/ KFSAVE(2),XSAVE(2),XQ2SAV(2),
     +		XPQSAV(2,-6:6),XDPQSAV(2,-6:6)
      INTEGER KFSAVE
      REAL XSAVE,XQ2SAV,XPQSAV,XDPQSAV
      SAVE /ARSTRF/


      REAL XCORR,XPYST,X,Q2,XPQ,XDPQ
      INTEGER KFL,KF
      DIMENSION XDPQ(-6:6),XPQ(-6:6),XPYST(-25:25)

C...Reset arrays etc.
      DO 100 KFL=-6,6
      XPQ(KFL)=0.0
  100 XPQSAV(1,KFL)=0.
      XSAVE(1)=X
      XQ2SAV(1)=Q2
      KFSAVE(1)=KF
C...Check x and particle species.
      IF(X.LE.0..OR.X.GE.1.) THEN
        WRITE(MSTU(11),5000) X
        RETURN
      ENDIF

      IF(LST(15).EQ.-4.OR.LST(15).EQ.-5) THEN
C...Intrinsic charm/bottom quark distribution in the proton...
        IF(Q2.LT.1.) RETURN
C...from Phys. Lett 93B (1980) 451, 
C...Amount of intrinsic charm PARL(12)=BETA^2
        XPQ(4)=X**3*1800.*PARL(12)*
     &         ((1.-X)/3.*(1.+10.*X+X**2)+2.*X*(1.+X)*LOG(X))
C...plus first order QCD-correction parametrized with polynomia
        IF(X.LT.0.9) THEN
          XCORR=0.22024E-1*X-0.77833E-1*X**2-0.47292*X**3+
     &          2.104*X**4-2.1698*X**5-0.84891*X**6+1.8882*X**7+
     &          0.8989*X**8-2.1072*X**9+0.76351*X**10
        ELSE
          XCORR=-1.
        ENDIF
C...and a Q2 dependence on that
CJR        XCORR=1.125*XCORR*0.190424*EXP(1.15*LOG(LOG(Q2)))
        IF(Q2.GT.1) THEN
           XCORR=1.125*XCORR*0.190424*EXP(1.15*LOG(LOG(Q2)))
        ELSE
           XCORR=1.125*XCORR*0.190424
        ENDIF
C...smooth cut-off of the structure function !
        XPQ(4)=MAX(XPQ(4)+XCORR,XPQ(4)/Q2)
        XPQ(-4)=XPQ(4)
        IF(LST(15).EQ.-5) THEN
C...  Intrinsic bottom assumed to have the same shape as zeroth
C...  approximation but suppressed by (mc/mb)**2=0.1 approximately
           XPQ(5)=XPQ(4)*0.1
           XPQ(-5)=XPQ(5)
           XPQ(4)=0.
           XPQ(-4)=0.
        ENDIF
      ELSE
C...  Parton densities from PYSTFU in PYTHIA 5.7
*     PEPSI>>
         IF(LST(15).LT.100) THEN

            write(*,*) 'PYSTFU not included !'
*HI            CALL PYSTFU(KF,X,Q2,XPYST)



            DO 110 KFL=-6,6
 110           XPQ(KFL)=XPYST(KFL)
         ELSE 
            CALL PARTON(X,Q2,XPQ,XDPQ)
         ENDIF
*     PEPSI<<
      ENDIF
         
      DO 120 KFL=-6,6
 120     XPQSAV(1,KFL)=XPQ(KFL)
C...  Formats for error printouts.
 5000 FORMAT(' Error in LYSTFU: x =',1P,E12.4,' outside physical range')
            
      RETURN
      END
*72*********************************************************************
      FUNCTION POLDQCD(ICOSFI,IPART,IP,XP,ZP,YY)

      IMPLICIT NONE

*...Added common block LEPTOU to access to polarization state,

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER ICOSFI,IP,IPART
      REAL POLDQCD,XP,ZP,YY,C1,C2,C3,C4

C...First order QCD matrix elements from R.D. Peccei and R. Ruckl:
C...Nucl. Phys. B162 (1980) 125
*...Polarized part: notes of L. Mankiewicz

C...Constants C1 to C5 are resp. 2/3/pi, 1/4/pi, 4/3/pi, 1/2/pi, 1/pi
      DATA C1,C2,C3,C4/0.2122066,0.0795775,0.4244132,0.1591549/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        POLDQCD=0.0
        RETURN  
      ENDIF     

      IF(ICOSFI.EQ.0) THEN
	IF(IPART.EQ.1) THEN
	  IF(IP.EQ.1) THEN
	    POLDQCD=0
	  ELSEIF(IP.EQ.2) THEN
	    POLDQCD=0
	  ELSEIF(IP.EQ.3) THEN
              POLDQCD=C1*((1.-XP)/(1.-ZP) + (1.-ZP)/(1.-XP) + 
     1             2.*XP*ZP/((1.-XP)*(1.-ZP)) + 2.*(ZP+XP-1.))
	  ELSE
	    WRITE(6,1000) ICOSFI,IPART,IP
	  ENDIF
	ELSEIF(IPART.EQ.2) THEN
	  IF(IP.EQ.1) THEN
	    POLDQCD= 0.
	  ELSEIF(IP.EQ.2) THEN
	    POLDQCD=0.
	  ELSEIF(IP.EQ.3) THEN
            POLDQCD=C2*(2.*XP-1)*(ZP/(1.-ZP) + (1.-ZP)/ZP)
	  ELSE
	    WRITE(6,1000) ICOSFI,IPART,IP
	  ENDIF
	ELSE
	  WRITE(6,1000) ICOSFI,IPART,IP
	ENDIF

      ELSEIF(ICOSFI.EQ.1) THEN
	IF(IPART.EQ.1) THEN
	  IF(IP.EQ.1) THEN
	   POLDQCD= 0.
	  ELSEIF(IP.EQ.3) THEN
              POLDQCD = C3*YY*
     &             SQRT((1.-YY)*XP*ZP/(1.-XP)/(1.-ZP))* 
     &             (1.-XP-ZP)
 	  ELSE
	    WRITE(6,1000) ICOSFI,IPART,IP
	  ENDIF
	ELSEIF(IPART.EQ.2) THEN
	  IF(IP.EQ.1) THEN
	    POLDQCD= 0.
	  ELSEIF(IP.EQ.3) THEN
	      POLDQCD=C4*YY*SQRT((1.-YY)*XP*(1.-XP)/ZP/(1.-ZP))*
     &        (1.-2.*ZP)
	  ELSE
	    WRITE(6,1000) ICOSFI,IPART,IP
	  ENDIF
	ENDIF

      ELSEIF(ICOSFI.EQ.2) THEN
	IF(IPART.EQ.1) THEN
	  POLDQCD= 0.
	ELSEIF(IPART.EQ.2) THEN
	  POLDQCD= 0.
	ELSE
	  WRITE(6,1000) ICOSFI,IPART,IP
	ENDIF

      ELSE
	WRITE(6,1000) ICOSFI,IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine POLDQCD     ',
     &' ICOSFI, IPART, IP = ',3I10)
      END

*72*********************************************************************
      FUNCTION POLDQCDI(IPART,IP,XP,ZPMIN,ZPMAX)

       IMPLICIT NONE
C...First order QCD matrix elements as in function DQCD but analytically
C...integrated over ZP from ZPMIN to ZPMAX, also a factor 1/(1-XP) is
C...factored out (since XP chosen randomly according to 1/(1-XP) distr.)

*...Added common block LEPTOU to access to polarization state.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER IPART,IP
      REAL XP,ZPMIN,ZPMAX,C1,C2,ZLOG,POLDQCDI
      DATA C1,C2/0.2122066,0.0795775/

      IF(LST(8).EQ.19.AND.IPART.EQ.1) THEN
C...No QCD Compton for Ariadne
        POLDQCDI=0.0
        RETURN   
      ENDIF      

      IF(IPART.EQ.1) THEN
	IF(IP.EQ.1) THEN
	  POLDQCDI = 0.
	ELSEIF(IP.EQ.2) THEN
	  POLDQCDI = 0.
	ELSEIF(IP.EQ.3) THEN
	  ZLOG=ALOG(ZPMAX/ZPMIN)  
	  POLDQCDI=C1*(XP**2*ZLOG+ZPMIN-ZPMAX+(ZPMIN**2-ZPMAX**2)/2.+ZLOG+
     &    2.*XP*(1.-XP)*(ZPMAX-ZPMIN)+(1.-XP)*(ZPMAX**2-ZPMIN**2))
	ELSE
	  WRITE(6,1000) IPART,IP
	ENDIF

      ELSEIF(IPART.EQ.2) THEN
	IF(IP.EQ.1) THEN
	  POLDQCDI = 0.
	ELSEIF(IP.EQ.2) THEN
	  POLDQCDI = 0.
	ELSEIF(IP.EQ.3)  THEN
	  ZLOG=ALOG(ZPMAX/ZPMIN)  
	  POLDQCDI=C2*(2.*XP-1.)*(1.-XP)*(2.*ZLOG+2.*ZPMIN-2.*ZPMAX)
	ELSE
	  WRITE(6,1000) IPART,IP
	ENDIF

      ELSE
	WRITE(6,1000) IPART,IP
      ENDIF
      RETURN

 1000 FORMAT(' Error in routine POLDQCDI     ',
     &' IPART, IP = ',2I10)
      END
*72*********************************************************************
      SUBROUTINE POLLAZIMU(XP,ZP)

      IMPLICIT NONE

C...Choose azimuthal angle (PHI) according to QCD matrix elements.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/


*...Two additional (local) arrays to store the values of the
*...parton distriutions at XI=X/XP
      INTEGER J,IFL,I,IH,IEVFL
      REAL XPQ1,XP,ZP,DQCD,POLDQCD,A,B,C,XDPQ1,PHIMAX,PHI,
     +XI,SGN
      REAL RLU

      DIMENSION XPQ1(-6:6),XDPQ1(-6:6)
*---

      J=LST(24)-1
      SGN=SIGN(1.,2.5-LST(24))
      IFL=LST(25)
      I=IABS(IFL)
      IH=1
      IF(LST(30).EQ.1) IH=2
      
      IF(LST(23).EQ.2) THEN
        A=PARI(24)*DQCD(0,J,1,XP,ZP,Y)+PARI(25)*DQCD(0,J,2,XP,ZP,Y)
     &  -LST(30)*ISIGN(1,IFL)*PARI(26)*DQCD(0,J,3,XP,ZP,Y)
        B=DQCD(1,J,1,XP,ZP,Y)
     &  +SGN*LST(30)*ISIGN(1,IFL)*DQCD(1,J,3,XP,ZP,Y)
        C=DQCD(2,J,1,XP,ZP,Y)

      ELSEIF(LST(23).EQ.1) THEN
	XI=X/XP
	CALL LNSTRF(XI,Q2,XPQ1)
        CALL DNSTRF(XI,Q2,XDPQ1)

	IF(LST(24).EQ.2)THEN
	  IEVFL=IFL
	ELSEIF(LST(24).EQ.3)THEN
	  IEVFL=0
	ENDIF

        A= XPQ1(IEVFL)* 
     &     (PARI(24)*DQCD(0,J,1,XP,ZP,Y)+PARI(25)*DQCD(0,J,2,XP,ZP,Y))
     &  + (FLOAT(LST(40))*XDPQ1(IEVFL)*PARI(26)*POLDQCD(0,J,3,XP,ZP,Y))
	B= XPQ1(IEVFL)   *DQCD(1,J,1,XP,ZP,Y)
     &   +FLOAT(LST(40))*XDPQ1(IEVFL)*POLDQCD(1,J,3,XP,ZP,Y)
	C= XPQ1(IEVFL)   *DQCD(2,J,1,XP,ZP,Y)

      ENDIF

      PHIMAX=ABS(A)+ABS(B)+ABS(C)
   
      IF(XDPQ1(IEVFL).gt. XPQ1(IEVFL).and.
     &     .NOT.(LST(15).ge.107.and.LST(15).le.109)) THEN
         WRITE(*,*) 'Trouble in POLLAZIMU!'
         STOP 
      ENDIF
      
 100  PHI=6.2832*RLU(0)
      IF(A+B*COS(PHI)+C*COS(2.*PHI).LT.RLU(0)*PHIMAX) GOTO 100
      CALL LUROBO(0.,PHI,0.,0.,0.)

CAE.Store ME variables
      PARL(28)=XP
      PARL(29)=ZP
      PARL(30)=PHI
CAE
      RETURN
      END


C ********************************************************************

      SUBROUTINE POLLZP(XP,ZP,IFAIL)

      IMPLICIT NONE

C...Choose value of ZP according to QCD matrix elements.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

*...
      INTEGER IFAIL,LOOP,IPART
      REAL XP,AP,BP,XI
      REAL RLU,DZPMAX
      REAL D,E,F,DP,POLDQCD,DQCD,ZPWEIT
      REAL DZ,DX,DA,DB,DC,DD,DE,DF
      INTEGER IH,I,IFL
      REAL ZP,C1,C2,SZP,FQG,FQQP,ZPMIN,ZPMAX,A,B,
     +CSIGN,C,XPQ,XDPQ,CP,AA,BB,Z1
*HI      REAL FQQ
      DIMENSION XPQ(-6:6),XDPQ(-6:6)
*---
      DATA C1,C2/0.2122066,0.0795775/,DZPMAX,SZP,CP/3*0./
      FQG(DZ,DX,DA,DB,DC)=DA*(DZ**2+DX**2)/(1.-DX)+2.*DA*DX*DZ*(1.-DZ)
     &+2.*DA*(1.-DZ)+4.*DB*DX*DZ*(1.-DZ)+DC*(DZ**2+DX**2)/(1.-DX)+
     &2.*DC*(DX+DZ)*(1.-DZ)
*HI      FQQ(DZ,DX,DA,DB,DC,DD,DE)=DA*DD*(DZ**2+(1.-DZ)**2)+DB*DE*DZ*
*HI     &(1.-DZ)+DC*DD*(2.*DZ-1.)
*...Polarized case:
      FQQP(DZ,DX,DA,DB,DC,DD,DE,DF)=DA*DD*(DZ**2+(1.-DZ)**2)+DB*DE*DZ*
     &(1.-DZ)+DC*DF*((1.-DZ)**2+DZ**2)
*---
      IFAIL=1
      IH=1
      IF(LST(30).EQ.1) IH=2
CAE.Scheme for ME cutoff: W2, Q2, mixed, z-shat
      IF(LST(20).LE.1) THEN
        ZPMIN=(1.-X)*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).EQ.2) THEN
        ZPMIN=X*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).GE.3.OR.LST(20).LE.5) THEN
        ZPMIN=PARL(27)
      ELSEIF(LST(20).EQ.6) THEN
        ZPMIN=PARL(8)
      ELSE
        WRITE(6,*) 'LZP: No such jet scheme!'
      ENDIF
CAE
      IF(ZPMIN.LE.0..OR.ZPMIN.GE.0.5) RETURN
      ZPMAX=1.-ZPMIN
      I=IABS(LST(25))
      AP=1.-ZPMIN
      BP=ZPMIN/AP
      IF(LST(23).EQ.2) THEN
        A=PARI(24)
        B=PARI(25)
        CSIGN=-LST(30)*ISIGN(1,LST(25))*PARI(26)
      ELSE
         
         XI=X/XP
         CALL LNSTRF(XI,Q2,XPQ)
         CALL DNSTRF(XI,Q2,XDPQ)
         
         IF (LST(24).EQ.2) THEN
            IFL=LST(25)
            A=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(24)*XPQ(IFL)
            B=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(25)*XPQ(IFL)
            C=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(26)*LST(40)*XDPQ(IFL)
         ELSE IF (LST(24).EQ.3) THEN
            A=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(24)*XPQ(0)
            B=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(25)*XPQ(0)
            C=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(26)*LST(40)*XDPQ(0)
         ENDIF
         CSIGN=C 

      ENDIF
      IF(LST(24).EQ.2) THEN
        DZPMAX=MAX(FQG(ZPMIN,XP,A,B,CSIGN),FQG(ZPMAX,XP,A,B,CSIGN))
        AA=2.*(A+CSIGN)/(1.-XP)-4.*A*XP-8.*B*XP-4.*CSIGN
        IF(ABS(AA).GT.1.E-20) THEN
          BB=2.*A*(XP-1.)+4.*B*XP+2.*CSIGN*(1.-XP)
          Z1=-BB/AA
          IF(Z1.GT.ZPMIN.AND.Z1.LT.ZPMAX)
     &    DZPMAX=MAX(DZPMAX,FQG(Z1,XP,A,B,CSIGN))
        ENDIF
        DZPMAX=DZPMAX*C1*1.05
      ELSEIF(LST(24).EQ.3) THEN
         CP=1./BP**2
         E=8.*XP*(1-XP)
         D=XP**2+(1.-XP)**2
         F=(2.*XP-1.)
         DZPMAX=MAX(FQQP(ZPMIN,XP,A,B,CSIGN,D,E,F),
     &            FQQP(ZPMAX,XP,A,B,CSIGN,D,E,F))
         AA=4.*(A*D+CSIGN*F)-2.*B*E
         IF(ABS(AA).GT.1.E-20) THEN
            BB=B*E-2.*(A*D+CSIGN*F)
            Z1=-BB/AA
            IF(Z1.GT.ZPMIN.AND.Z1.LT.ZPMAX) THEN
               DZPMAX=MAX(DZPMAX,FQQP(Z1,XP,A,B,CSIGN,D,E,F))
            ENDIF
         ENDIF
         DZPMAX=DZPMAX*C2*1.05
      ENDIF
      IPART=LST(24)-1
         
      LOOP=0
  100 LOOP=LOOP+1
      IF(LOOP.GT.1000) RETURN
      IF(LST(24).EQ.2) THEN
        ZP=1.-AP*BP**RLU(0)
        SZP=1.-ZP
      ELSEIF(LST(24).EQ.3) THEN
        DP=BP*CP**RLU(0)
        ZP=DP/(1.+DP)
        SZP=ZP*(1.-ZP)
      ENDIF
      ZPWEIT=SZP*(A*DQCD(0,IPART,1,XP,ZP,0.)+B*DQCD(0,IPART,2,XP,ZP,0.)
     &+CSIGN*POLDQCD(0,IPART,3,XP,ZP,0.))/DZPMAX
      IF(ZPWEIT.LT.RLU(0)) GOTO 100
      IFAIL=0
      RETURN
      END

*72*********************************************************************

       FUNCTION POLDSIGMA(XP)

       IMPLICIT NONE
C...Differential cross section for first order QCD processes.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/


      REAL POLDSIGMA,XP,DQCDI,POLDQCDI,ULMASS
      INTEGER I,IH,IL,IU,IP
      REAL PQH,AMU,XI,ZPMIN,ZPMAX,XPQ,WQ,WQB,TQ,TQB,T1,S13,SGN,XDPQ,SIG
*...Added array XDPQ to store delta parton distributions.      
      DIMENSION XPQ(-6:6),XDPQ(-6:6),PQH(17,2)
*___
      POLDSIGMA=0.
      DO 10 I=1,17
      PQH(I,1)=0.
      PQH(I,2)=0.
   10 PQ(I)=0.

      MSTJ(93)=1
      AMU=ULMASS(2)
*PEPSI>>
        IL=1
        IU=3
*PEPSI<<
      XI=X/XP
C...Scheme for ME cutoff: W2, Q2, mixed
      IF(LST(20).LE.1) THEN
        ZPMIN=(1.-X)*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).EQ.2) THEN
        ZPMIN=X*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
        ZPMIN=PARL(27)
      ELSEIF(LST(20).GE.6) THEN
        ZPMIN=PARL(8)
      ENDIF
      IF(ZPMIN.LE.0..OR.ZPMIN.GE.0.5) RETURN
      ZPMAX=1.D0-DBLE(ZPMIN)
      CALL LNSTRF(XI,Q2,XPQ)
      CALL DNSTRF(XI,Q2,XDPQ)
      IF(LST(24).EQ.3) GOTO 3000

C...Gluon bremsstrahlung process, i.e. qg-event.
 2000 DO 2500 IP=IL,IU
      SIG=DQCDI(1,IP,XP,ZPMIN,ZPMAX)
*PEPSI>>
      IF (IP.EQ.3) THEN
      SIG=POLDQCDI(1,IP,XP,ZPMIN,ZPMAX)
      ENDIF
*PEPSI<<
      SGN=SIGN(1.,5.-2.*IP)
      DO 2300 IH=1,2
      IF(IH.EQ.1) THEN
        IF(PARL(6).GT.0.99) GOTO 2300
        IF(LST(32).EQ.0.AND.LST(30).NE.-1) GOTO 2300
      ELSEIF(IH.EQ.2) THEN
        IF(PARL(6).LT.-0.99) GOTO 2300
        IF(LST(32).EQ.0.AND.LST(30).NE.1) GOTO 2300
      ENDIF
      IF(LST(32).NE.0) LST(30)=SIGN(1.,IH-1.5)
      IF(LST(23).NE.2) THEN
        DO 2100 I=1,LST(12)
        WQ=XPQ(I)*SIG*(EWQC(1,IH,I)+SGN*EWQC(2,IH,I))
        WQB=XPQ(-I)*SIG*SGN*(EWQC(1,IH,I)+SGN*EWQC(2,IH,I))
*...EM polarized case: if IP=3 use proper formula for
*...the interference part of cross section.
*PEPSI>>
        IF(IP.EQ.3)THEN
          WQ=LST(40)*XDPQ(I)*SIG*(EWQC(1,IH,I)+EWQC(2,IH,I))
          WQB=LST(40)*XDPQ(-I)*SIG*(EWQC(1,IH,I)+EWQC(2,IH,I))
        ENDIF
*PEPSI<<
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(12),IH)=PQH(I+LST(12),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 2100   CONTINUE
      ELSEIF(LST(23).EQ.2) THEN
C...Zero CC cross-section for one helicity state.
        IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 2300
        IF(IP.EQ.3) THEN
          TQ=-LST(30)
          TQB=-TQ
        ELSE
          TQ=1.
          TQB=1.
        ENDIF
        DO 2200 I=1,LST(12)
        T1=-K(3,2)*QC(I)
        IF(T1.GT.0) THEN
          WQ=XPQ(I)*SIG*TQ
          WQB=0.
        ELSE
          WQB=XPQ(-I)*SIG*TQB
          WQ=0.
        ENDIF
C...Include y-dependence.
        WQ=WQ*PARI(23+IP)
        WQB=WQB*PARI(23+IP)
        PQH(I,IH)=PQH(I,IH)+WQ
        PQH(I+LST(12),IH)=PQH(I+LST(12),IH)+WQB
        PQH(17,IH)=PQH(17,IH)+WQ+WQB
 2200   CONTINUE
        ENDIF
 2300 CONTINUE
 2500 CONTINUE
      DO 2600 I=1,17
 2600 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)
      IH=1
      IF(LST(30).EQ.1) IH=2
      IF(LST(32).EQ.0) THEN
C...Simulation: cross section for chosen helicity state only.
        POLDSIGMA=PQH(17,IH)
      ELSEIF(LST(19).EQ.-1) THEN
C...Integration event-by-event: normalize and include alpha_s*1/(1-xp)
        POLDSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
C...Max of dsigma/dxp for L- and R-handed lepton.  
        IF(PQH(17,1).GT.PARI(15)) PARI(15)=PQH(17,1)
        IF(PQH(17,2).GT.PARI(16)) PARI(16)=PQH(17,2)
      ELSE
C...Integration: normalize and include alpha_s*1/(1-xp)
        POLDSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
        IF(LST(17).EQ.0.AND.LST(40).EQ.0) THEN
C...Fixed beam energy, max of dsigma/dxp for L- and R-handed lepton.
          IF(PQH(17,1).GT.PARI(15)) PARI(15)=PQH(17,1)
          IF(PQH(17,2).GT.PARI(16)) PARI(16)=PQH(17,2)
	ELSE
C...Variable beam energy or polarizzed case, max of dsigma/dxp 
*...for S, T, I contributions.
	  IF(PQ(17)/PARI(23+LST(32)).GT.PARI(14+LST(32)))
     &    PARI(14+LST(32))=PQ(17)/PARI(23+LST(32))
	ENDIF
      ENDIF
      RETURN
C...Boson-gluon fusion, i.e. qq-event.
 3000 S13=Q2*(1.-XP)/XP
      IF(S13.LT.4.*AMU**2) RETURN
      DO 3500 IP=IL,IU
*PEPSI>>
*HI      SIG=XPQ(0)*DQCDI(2,IP,XP,ZPMIN,ZPMAX)
      IF(IP.EQ.3.) THEN
         SIG=FLOAT(LST(40))*XDPQ(0)*POLDQCDI(2,IP,XP,ZPMIN,ZPMAX)
      ELSE
         SIG=XPQ(0)*DQCDI(2,IP,XP,ZPMIN,ZPMAX)
      ENDIF
*PEPSI<<
      DO 3300 IH=1,2
      IF(IH.EQ.1) THEN
	IF(PARL(6).GT.0.99) GOTO 3300
	IF(LST(32).EQ.0.AND.LST(30).NE.-1) GOTO 3300
      ELSEIF(IH.EQ.2) THEN
	IF(PARL(6).LT.-0.99) GOTO 3300
	IF(LST(32).EQ.0.AND.LST(30).NE.1) GOTO 3300
      ENDIF
      IF(LST(32).NE.0) LST(30)=SIGN(1.,IH-1.5)
      IF(LST(23).NE.2) THEN
	DO 3100 I=1,LST(13)
        MSTJ(93)=1
	IF(S13.LT.4.*ULMASS(I)**2) GOTO 3100
	WQ=SIG/2.*(EWQC(1,IH,I)+EWQC(2,IH,I))
	WQB=WQ
C...Include y-dependence.
	WQ=WQ*PARI(23+IP)
	WQB=WQB*PARI(23+IP)
	PQH(I,IH)=PQH(I,IH)+WQ
	PQH(I+LST(13),IH)=PQH(I+LST(13),IH)+WQB
	PQH(17,IH)=PQH(17,IH)+WQ+WQB
 3100   CONTINUE
      ELSEIF(LST(23).EQ.2) THEN
C...Zero CC cross-section for one helicity state.
	IF(KSAVE(1).LT.0.AND.IH.EQ.1
     &  .OR.KSAVE(1).GT.0.AND.IH.EQ.2) GOTO 3300
	DO 3200 I=1,LST(13)
        MSTJ(93)=1
	IF(S13.LT.(AMU+ULMASS(I))**2) GOTO 3200
	IF(K(3,2)*QC(I).LT.0) THEN
	  WQ=SIG
	  WQB=0.
	ELSE
	  WQB=SIG
	  WQ=0.
	ENDIF
C...Include y-dependence.
	WQ=WQ*PARI(23+IP)
	WQB=WQB*PARI(23+IP)
	PQH(I,IH)=PQH(I,IH)+WQ
	PQH(I+LST(13),IH)=PQH(I+LST(13),IH)+WQB
	PQH(17,IH)=PQH(17,IH)+WQ+WQB
 3200   CONTINUE
      ENDIF
 3300 CONTINUE
 3500 CONTINUE
      DO 3600 I=1,17
 3600 PQ(I)=(1.-PARL(6))/2.*PQH(I,1)+(1.+PARL(6))/2.*PQH(I,2)
      IH=1
      IF(LST(30).EQ.1) IH=2
      IF(LST(32).EQ.0) THEN
C...Simulation: cross section for chosen helicity state only.
        POLDSIGMA=PQH(17,IH)
      ELSEIF(LST(19).EQ.-1) THEN
C...Integration event-by-event: normalize and include alpha_s*1/(1-xp)
        POLDSIGMA=PQH(17,IH)/PARI(20)*PARL(25)/(1.-XP)
C...Max of dsigma/dxp for L- and R-handed lepton.  
        IF(PQH(17,1).GT.PARI(18)) PARI(18)=PQH(17,1)
        IF(PQH(17,2).GT.PARI(19)) PARI(19)=PQH(17,2)
      ELSE
C...Integration for grid: normalize and include alpha_s*1/(1-xp)
	POLDSIGMA=PQ(17)/PARI(20)*PARL(25)/(1.-XP)
	IF(LST(17).EQ.0.AND.LST(40).EQ.0) THEN
*---
C...Fixed beam energy, max of dsigma/dxp for L- and R-handed lepton.
	  IF(PQH(17,1).GT.PARI(18)) PARI(18)=PQH(17,1)
	  IF(PQH(17,2).GT.PARI(19)) PARI(19)=PQH(17,2)
	ELSE
C...Variable beam energy, max of dsigma/dxp for S, T, I contributions.
	  IF(PQ(17)/PARI(23+LST(32)).GT.PARI(17+LST(32)))
     &    PARI(17+LST(32))=PQ(17)/PARI(23+LST(32))
	ENDIF

      ENDIF
      RETURN
      END



*72*********************************************************************
       SUBROUTINE POLLINIT(LFILE,LEPIN,PLZ,PPZ,INTER)

       IMPLICIT NONE

C...Initialize for an incoming lepton (type LEPIN, momentum pz=PLZ)
C...and target nucleon (momentum pz=PPZ) to interact via INTER.
C...Find maximum of differential cross section, calculate QCD event
C...probabilities or read them from logical file LFILE (if >0).
C...  Numerical integration to obtain total cross-section.
       
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      REAL PSAVE,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX
      INTEGER KSAVE,ILEP,INU,IG,IZ
      SAVE /LINTRL/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/

      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2), 
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      REAL    XX,WW,PQG,PQQB,QGMAX,QQBMAX,YCUT,XTOT
      INTEGER NXX,NWW,NP
      SAVE /LGRID/


      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      REAL OPTX,OPTY,OPTQ2,OPTW2,COMFAC
      SAVE /LOPTIM/

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/

      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DBETA
      REAL STHETA,SPHI,PB,PHIR
      SAVE /LBOOST/

      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      REAL XKIN,UKIN,WKIN,AIN,BIN,RELERR,RELUP,RELER2,FCNMAX
      INTEGER MAXFIN
      SAVE /LMINUI/

      COMMON /LMINUC/ NAMKIN(4),NAM(30)
      CHARACTER*10 NAMKIN,NAM
      SAVE /LMINUC/

      COMMON /LPFLAG/ LST3
      INTEGER LST3
      SAVE /LPFLAG/

      COMMON /PYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      REAL PYPAR,PYVAR
      INTEGER IPY
      SAVE /PYPARA/ 

      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARIPY(200) 
      INTEGER MSTP,MSTI
      REAL PARP,PARIPY
      SAVE /PYPARS/

      COMMON/MAXFROMHAND/ FMAXFH
      REAL FMAXFH
      SAVE/MAXFROMHAND/


*
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      INTEGER INTER,LFILE,LEPIN,NCALL,I,J,IFL
      REAL PLZ,PPZ,PI,PM2,S,ROOTS
      REAL ULMASS,ULANGL,UMIN,UMAX
      INTEGER LQCD,LTM,IPMAX,IP,IW,IX
      REAL TI1,TI2

      INTEGER LSTW(40)
      REAL PARLW(30)

      DATA PI/3.1415927/,NCALL/0/

      NCALL=NCALL+1
      LST3=LST(3)

*MM,HI   >>
      DO I=1,IMXPDF
         IF(LST(15).eq.IPDFNAM(1,I)) THEN
            CUNPOL=CPDFNAM(1,IPDFNAM(2,I))
            CPOL  =CPDFNAM(2,IPDFNAM(2,I))
         ENDIF
      ENDDO
      IF (LST(15).GE.113.AND.LST(15).LE.115) CALL POLINI
      IF (LST(15).GE.107.AND.LST(15).LE.109) CALL NLOINI
      IF ((LST(15).GE.124).AND.(LST(15).LE.129)) CALL INIDEFLO
      IF (LST(40).EQ.0) RETURN 
*MM,HI<<

      IF(LST(18).GE.1) THEN
C...W, Z masses from theta-Weinberg, Fermi constant GF and rad. corr.
        PMAS(24,1)=SQRT(PI*PARL(16)/(SQRT(2.)*PARL(17)*PARL(5)*
     &  (1.-PARL(18))))
        PMAS(23,1)=PMAS(24,1)/SQRT(1.-PARL(5))
      ENDIF
C...Couplings between Z0 and left/right-handed leptons and quarks.
      ZL(1,1)=-.5+PARL(5)
      ZL(1,2)=PARL(5)
      ZL(2,1)=ZL(1,2)
      ZL(2,2)=ZL(1,1)
      ZL(1,3)=0.5
      ZL(2,3)=0.
      ZL(1,4)=0.
      ZL(2,4)=0.5
      DO 10 IFL=1,8
      ZQ(1,IFL)=SIGN(0.5,QC(IFL))-QC(IFL)*PARL(5)
   10 ZQ(2,IFL)=-QC(IFL)*PARL(5)

C...Set initial state.
      LST(23)=INTER
      KSAVE(1)=LEPIN
      KSAVE(2)=2212
      K(1,1)=21
      K(1,2)=KSAVE(1)
      K(1,3)=0
      K(1,4)=0
      K(1,5)=0
      K(2,1)=21
      K(2,2)=KSAVE(2)
      K(2,3)=0
      K(2,4)=0
      K(2,5)=0
      P(1,1)=0.
      P(1,2)=0.
      P(1,3)=PLZ
      P(1,5)=ULMASS(KSAVE(1))
      P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
      P(2,1)=0.
      P(2,2)=0.
      P(2,3)=PPZ
      P(2,5)=ULMASS(KSAVE(2))
      P(2,4)=SQRT(P(2,3)**2+P(2,5)**2)
      N=2
      LST(28)=3
C...Save momentum vectors of incoming particles
      DO 20 I=1,2
      DO 20 J=1,5
   20 PSAVE(3,I,J)=P(I,J)
C...Dot-product of initial particles, cms energy
      PARL(21)=2.*(DBLE(P(1,4))*DBLE(P(2,4))-DBLE(P(1,3))*DBLE(P(2,3)))
      ROOTS=SQRT((DBLE(P(1,4))+DBLE(P(2,4)))**2
     &          -(DBLE(P(1,3))+DBLE(P(2,3)))**2)
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1000)
     &LEPIN,(P(1,J),J=1,3),PARL(1),PARL(2),(P(2,J),J=1,3),INTER,ROOTS
*...Write out also polarization state and structure function      
      IF(LST(40).NE.0)WRITE(6,1005) LST(40),LST(15) 
*---      
      IF(PLZ*PPZ.GT.0.1) THEN
        WRITE(6,1010)
        STOP
      ENDIF

C...Reduced header for Jetset/Pythia
      MSTU(12)=0
      MSTP(122)=0
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) 
     &WRITE(6,1020) MSTU(181),MSTU(182),MSTP(181),MSTP(182)
C...If JETSET version before 7.402, problem with azimuthal dependence
C...in LUSHOW solved by chosing flat azimuthal dependence.
      IF(MSTU(181).LE.7.AND.MSTU(182).LT.402) THEN
        MSTJ(46)=0
        WRITE(6,1030) MSTJ(46)
      ENDIF
C...Initialize PYTHIA for parton densities.
      IF(LST(15).GT.0) THEN
C...Set switches and parameters for parton densities in LYSTFU.
        MSTP(51)=LST(15)
        MSTP(52)=LST(16)
        MSTP(58)=LST(12)
      ENDIF
C     CALL PYINIT('NONE','e-','p',ROOTS)
      PARL(26)=PARP(1)
CAE--     use Lambda from parton densities in initial cascade
      PYPAR(21)=PARP(1)
C...Reset PYTHIA 4.8 parameters from LEPTO parameters.
      IF(MOD(LST(8),10).EQ.3.OR.MOD(LST(8),10).EQ.5) IPY(13)=0
      IF(LST(35).NE.1.AND.
     &(MOD(LST(8),10).EQ.4.OR.MOD(LST(8),10).EQ.5)) IPY(14)=0
      IPY(8)=LST(12)


*...Special restrictions apply for polarized version: Only first
*...order QCD. Only EM interaction. Beam energy not allowed to vary.
*...Parton cascades, longitudinal SF and higher twist not allowed.
      IF((LST(40).NE.0.AND.LST(23).NE.1).OR.
     &(LST(40).NE.0.AND.LST(8).GE.2).OR.
     &(LST(40).NE.0.AND.LST(17).NE.0).OR.
*     &(LST(40).NE.0.AND.LST(11).NE.0).OR.
     &(LST(40).NE.0.AND.IABS(LEPIN).NE.11)) THEN
	WRITE(6,1025)
	STOP
      ENDIF
*---
      IF(PSAVE(3,1,3).LT.0.) THEN
C...Flip event to have initial lepton along +z axis
        P(1,3)=-P(1,3)
        P(2,3)=-P(2,3)
      ENDIF
C...Boost parameters to cms of incoming particles
      DBETA(1,1)=0.D0
      DBETA(1,2)=0.D0
      DBETA(1,3)=(DBLE(P(1,3))+DBLE(P(2,3)))/(DBLE(P(1,4))+DBLE(P(2,4)))
      SPHI(1)=0.D0
      STHETA(1)=0.D0
      IF(LST(17).NE.0) THEN
C...For varying beam energies, transform to cms, lepton along +z axis.
        CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,-DBETA(1,3))
        SPHI(1)=ULANGL(P(1,1),P(1,2))
        CALL LUDBRB(0,0,0.,-SPHI(1),0.D0,0.D0,0.D0)
        STHETA(1)=ULANGL(P(1,3),P(1,1))
        CALL LUDBRB(0,0,-STHETA(1),0.,0.D0,0.D0,0.D0)
        LST(28)=2
      ENDIF

C...Effective limits on kinematic variables x, y, Q**2, W**2
      PM2=P(2,5)**2
      S=PARL(21)
      XMIN=MAX(CUT(1),0.)
      XMAX=MIN(CUT(2),1.)
      YMIN=MAX(CUT(3),0.)
      YMAX=MIN(CUT(4),1.)
      Q2MIN=MAX(CUT(5),0.)
      Q2MAX=MIN(CUT(6),S)
      W2MIN=MAX(CUT(7),0.)
      W2MAX=MIN(CUT(8),S+PM2)
      UMIN=MAX(CUT(9),0.)
      UMAX=MIN(CUT(10),S/(2.*P(2,5)))
      DO 40 I=1,2
      XMIN=MAX(XMIN,Q2MIN/(S*YMAX),Q2MIN/(2.*P(2,5)*UMAX),
     &1.-(W2MAX-PM2)/MAX(S*YMIN,1.E-22),
     &1.-(W2MAX-PM2)/MAX(2.*P(2,5)*UMIN,1.E-22))
      XMAX=MIN(XMAX,Q2MAX/MAX(S*YMIN,1.E-22),
     &Q2MAX/MAX(2.*P(2,5)*UMIN,1.E-22),
     &1.-(W2MIN-PM2)/(S*YMAX),1.-(W2MIN-PM2)/(2.*P(2,5)*UMAX))
      YMIN=MAX(YMIN,Q2MIN/(S*XMAX),(W2MIN-PM2)/(S*(1.-XMIN)),
     &(W2MIN-PM2+Q2MIN)/S,2.*P(2,5)*UMIN/S)
      YMAX=MIN(YMAX,Q2MAX/MAX(S*XMIN,1.E-22),
     &(W2MAX-PM2)/MAX(S*(1.-XMAX),1.E-22),
     &(W2MAX-PM2+Q2MAX)/S,2.*P(2,5)*UMAX/S)
      Q2MIN=MAX(Q2MIN,S*XMIN*YMIN,S*YMIN-W2MAX+PM2,
     &2.*P(2,5)*UMIN*XMIN,(W2MIN-PM2)*XMIN/(1.-XMIN))
      Q2MAX=MIN(Q2MAX,S*XMAX*YMAX,S*YMAX-W2MIN+PM2,
     &2.*P(2,5)*UMAX*XMAX,(W2MAX-PM2)*XMAX/MAX(1.-XMAX,1.E-22))
      W2MIN=MAX(W2MIN,S*(1.-XMAX)*YMIN+PM2,Q2MIN*(1.-XMAX)/XMAX+PM2,
     &S*YMIN-Q2MAX+PM2,2.*P(2,5)*UMIN*(1.-XMAX)+PM2)
      W2MAX=MIN(W2MAX,S*(1.-XMIN)*YMAX+PM2,
     &Q2MAX*(1.-XMIN)/MAX(XMIN,1.E-22)+PM2,
     &S*YMAX-Q2MIN+PM2,2.*P(2,5)*UMAX*(1.-XMIN)+PM2)
C     UMIN=MAX(UMIN,....)
C     UMAX=MIN(UMAX,....)
   40 CONTINUE
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1050)
     &CUT,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX,UMIN,UMAX
      IF(XMAX.LT.XMIN.OR.YMAX.LT.YMIN.OR.Q2MAX.LT.Q2MIN.OR.
     &W2MAX.LT.W2MIN) THEN
        IF(LST(3).GE.1) WRITE(6,1100)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF
      IF(XMIN.LT.1.E-10.OR.Q2MIN.LT.1.E-01) THEN
        IF(LST(3).GE.1) WRITE(6,1110)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF

      PARI(11)=(PARL(1)-PARL(2))/PARL(1)
      KSAVE(4)=LEPIN
      ILEP=1
      IF(LEPIN.LT.0) ILEP=2
      INU=0
      IF(IABS(LEPIN).EQ.12.OR.IABS(LEPIN).EQ.14
     &.OR.IABS(LEPIN).EQ.16) INU=1
      IF(INU.EQ.1) THEN
C...Set full polarisation for incoming neutrino.
        PARL(6)=-1.
        IF(LEPIN.LT.0) PARL(6)=1.
      ENDIF
      IF(LST(23).EQ.1.AND.INU.EQ.0) THEN
C...Electromagnetic interaction.
        KSAVE(3)=22
        IG=1
        IZ=0
      ELSEIF(LST(23).EQ.2) THEN
C...Weak charged current, only one helicity state contributes.
        IF(KSAVE(1).LT.0.AND.PARL(6).LT.-0.99
     &  .OR.KSAVE(1).GT.0.AND.PARL(6).GT.0.99) THEN
          IF(LST(3).GE.1) WRITE(6,1150) LEPIN,PARL(6)
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
        ENDIF
        IF(MOD(IABS(LEPIN),2).EQ.0) THEN
          KSAVE(3)=ISIGN(24,LEPIN)
          KSAVE(4)=ISIGN(IABS(LEPIN)-1,LEPIN)
        ELSE
          KSAVE(3)=ISIGN(24,-LEPIN)
          KSAVE(4)=ISIGN(IABS(LEPIN)+1,LEPIN)
        ENDIF
      ELSEIF(LST(23).EQ.3.OR.(LST(23).EQ.4.AND.INU.EQ.1)) THEN
C...Weak neutral current.
        KSAVE(3)=23
        IG=0
        IZ=1
      ELSEIF(LST(23).EQ.4.AND.INU.EQ.0) THEN
C...Neutral current, electromagnetic and weak with interference.
        KSAVE(3)=23
        IG=1
        IZ=1
      ELSE
        IF(LST(3).GE.1) WRITE(6,1200) INTER,LEPIN
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF

C...Choice of independent variables.
      IF(LST(1).EQ.0) THEN
        LST(31)=1
        IF(INTER.EQ.2.OR.INTER.EQ.3) LST(31)=2
      ELSE
        LST(31)=IABS(LST(1))
      ENDIF
      IF(LST(31).LT.1.OR.LST(31).GT.3) THEN
        IF(LST(3).GE.1) WRITE(6,1210) LST(1),LST(31)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF
      IF(LST(1).LT.0) THEN
C...User-defined optimization parameters.
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1220) OPTX,OPTY,OPTQ2,OPTW2
      ELSE
C...Set optimization parameters.
        DO 50 I=1,4
        OPTX(I)=0.
        OPTY(I)=0.
        OPTQ2(I)=0.
   50   OPTW2(I)=0.
        IF(INTER.EQ.1) THEN
          OPTX(2)=1.
          OPTY(1)=1.
          OPTQ2(3)=1.
          OPTW2(3)=1.
        ELSEIF(INTER.EQ.4) THEN
          OPTX(1)=0.1
          OPTX(2)=1.
          OPTY(1)=1.
          OPTQ2(1)=0.5
          OPTQ2(2)=0.5
          OPTQ2(3)=1.
          OPTW2(1)=0.5
          OPTW2(2)=0.5
          OPTW2(3)=1.
        ELSE
          OPTX(1)=1.
          OPTY(1)=1.
          OPTQ2(1)=1.
          OPTW2(1)=1.
        ENDIF
      ENDIF

C...Initialize Monte Carlo estimate of cross section.
      PARL(24)=0.
      PARI(27)=0.
      PARI(28)=0.
      PARI(29)=0.
      PARI(30)=0.
      PARI(32)=0.
      IF(LST(23).EQ.2) THEN
C...Constant factor GF**2/pi for CC, transformation to picobarn.
        PARI(31)=PARL(17)**2/PI*0.39E+09
      ELSE
C...Constant factor 2*pi*alpha**2 for NC, transformation to picobarn.
        PARI(31)=2.*PI*PARL(16)**2*0.39E+09
      ENDIF
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &WRITE(6,1250) (I,LST(I),LST(I+10),PARL(I),PARL(I+10),I=1,10)

C...Set up grid with longitudinal structure function, QCD & target mass;
C...only when photon exchange is included
      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      IF(LST(11).NE.0.AND.(INTER.EQ.1.OR.INTER.EQ.4)) CALL FLTABL

C...Get integrated cross-section.
      PARL(23)=0.
      IF(LST(10).GT.0) CALL LXSECT
      IF(LQCD.EQ.2.OR.LTM.EQ.2) THEN
        WRITE(6,1300)
        IF(LQCD.EQ.2) WRITE(6,1310)
        IF(LTM .EQ.2) WRITE(6,1320)
        WRITE(6,1330)
      ENDIF

      IF(LST(2).EQ.1) THEN
C...Find max value of differential cross section for rejection.
        UKIN(1)=(XMAX+XMIN)/2.
        WKIN(1)=0.8*(XMAX-XMIN)/2.
        AIN(1)=XMIN
        BIN(1)=XMAX
        IF(LST(31).EQ.1) THEN
          UKIN(2)=(Q2MAX+Q2MIN)/2.
          WKIN(2)=0.8*(Q2MAX-Q2MIN)/2.
          AIN(2)=Q2MIN
          BIN(2)=Q2MAX
          NAMKIN(2)='      Q**2'
        ELSEIF(LST(31).EQ.2) THEN
          UKIN(2)=(YMAX+YMIN)/2.
          WKIN(2)=0.8*(YMAX-YMIN)/2.
          AIN(2)=YMIN
          BIN(2)=YMAX
          NAMKIN(2)='         y'
        ELSEIF(LST(31).EQ.3) THEN
          UKIN(2)=(W2MAX+W2MIN)/2.
          WKIN(2)=0.8*(W2MAX-W2MIN)/2.
          AIN(2)=W2MIN
          BIN(2)=W2MAX
          NAMKIN(2)='      W**2'
        ENDIF
C...Maximum obtained by minimizing -(diff. x-section).
        CALL LTIMEX(TI1)
        CALL LMINEW
        CALL LTIMEX(TI2)
        PARI(LST(23))=FCNMAX*1.1
CMM.. Maximum inserted by hand if MINUIT fails
        IF(PARI(LST(23)).EQ.0.0) PARI(LST(23))=FMAXFH
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1400) PARI(LST(23)),TI2-TI1
      ENDIF

      IF(LFILE.GT.0.AND.LST(19).GE.0) THEN
C...Read QCD weights from file.
        READ(LFILE) LSTW,PARLW,NXX,NWW,NP,XX,WW
        IPMAX=2
        IF(LSTW(17).NE.0) IPMAX=3
        READ(LFILE) (((PQG(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((PQQB(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((QGMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,IPMAX),
     &  (((QQBMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,MIN(2,IPMAX)),
     &  YCUT
        IF(NP.NE.1) READ(LFILE) XTOT
        CLOSE(LFILE)
C...Reset parameters for matrix element integration.
        PARL(8)=PARLW(8)
        PARL(9)=PARLW(9)
        PARL(11)=PARLW(11)
        PARL(12)=PARLW(12)
        PARL(13)=PARLW(13)
C...Check current parameter values against those used when
C...calculating weights.
*...Added check also for polarization flag
        IF(LST(12).NE.LSTW(12).OR.LST(13).NE.LSTW(13)
     &  .OR.LST(15).NE.LSTW(15).OR.LST(16).NE.LSTW(16)
     &  .OR.LST(17).NE.LSTW(17).OR.LST(23).NE.LSTW(23)
     &  .OR.LST(40).NE.LSTW(40)
     &  .OR.ABS(PARL(1)-PARLW(1)).GT.0.1.OR.ABS(PARL(2)-PARLW(2)).GT.0.1
     &  .OR.ABS(PARL(5)-PARLW(5)).GT.0.01
     &  .OR.ABS(PARL(6)-PARLW(6)).GT.0.1) THEN
         IF(LST(3).GE.1)
     &    WRITE(6,1500) LST(12),LSTW(12),LST(13),LSTW(13),LST(15),
     &    LSTW(15),LST(16),LSTW(16),LST(17),LSTW(17),LST(23),LSTW(23),
     &    LST(40),LSTW(40),
     &    PARL(1),PARLW(1),PARL(2),PARLW(2),PARL(5),PARLW(5),PARL(6),
     &    PARLW(6)
*---
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
	ENDIF
      ELSEIF((LST(19).GE.0.OR.LST(19).EQ.-10).AND.
     &(LST(8).EQ.1.OR.LST(8)/10.EQ.1.OR.MOD(LST(8),10).EQ.9)) THEN
C...Calculate weights if 1st order QCD from grid is requested.
        CALL LTIMEX(TI1)
        CALL LWEITS(LFILE)
        CALL LTIMEX(TI2)
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1510) TI2-TI1
      ENDIF

C...Reset counters to zero for Monte Carlo estimate of cross section.
      PARI(27)=0.
      PARI(28)=0.
      PARI(29)=0.
      PARI(30)=0.
      LST(32)=0
      RETURN

 1000 FORMAT(' ', 
     &'A MONTE CARLO GENERATOR FOR DEEP INELASTIC LEPTON-'
     &,'NUCLEON SCATTERING',/,5X,68('='), 
     &25X,'LEPTO version 6.5, April 20, 1996', 
C    &25X,'PRELIMINARY VERSION, DO NOT CIRCULATE', 
     &' Lepton: type =',I3,5X,'momentum (px,py,pz) =',3F8.1,
     &' GeV', 
     &'momentum (px,py,pz) =',3F8.1,' GeV', 
     &' Interaction :',I3,14X,' CMS energy =',1PG12.4,' GeV',/)
*...Write also relative polarization state
 1005 FORMAT(' Realtive e-p polarization state:',I3,/,
     &' Polarized structure function set:',I3,/)
*---
 1010 FORMAT(' Warning: lepton and nucleon momenta in same direction',
     &' not allowed.',/,10X,'Execution stopped.')
*...Stop execution if inconsistency is detected in polarized case
 1020 FORMAT(/,' JETSET version ',I3,'.',I3,' is used.',/,
     &' Parton densities in PYTHIA version ',I3,'.',I3,' are used.',/)
 1025 FORMAT(' Warning: unallowed type of interaction.',/,10X,   
     &' Execution stopped.')
 1030   FORMAT(' Warning (LINIT): JETSET version before 7.402, MSTJ(46)'
     &  ,' set to',I4,/,18X,'to avoid mismatch LEPTO<-->LUSHOW.',/)
*---
 1050 FORMAT(/,' User applied cuts (+ phase space) : ',1P,
     &      G12.4,' <   x   < ',G12.4,
     &/,37X,G12.4,' <   y   < ',G12.4,
     &/,37X,G12.4,' < Q**2  < ',G12.4,
     &/,37X,G12.4,' < W**2  < ',G12.4,
     &/,37X,G12.4,' <  nu   < ',G12.4,
     &/,37X,G12.4,' <  E''   < ',G12.4,
     &/,37X,G12.4,' < theta < ',G12.4,/,
     &/,       ' Effective ranges (from above cuts): ',
     &      G12.4,' <   x   < ',G12.4,
     &/,37X,G12.4,' <   y   < ',G12.4,
     &/,37X,G12.4,' < Q**2  < ',G12.4,
     &/,37X,G12.4,' < W**2  < ',G12.4,
     &/,37X,G12.4,' <  nu   < ',G12.4)
 1100 FORMAT(' Warning: effective upper limit of kinematical ',
     &'variable(s) smaller than corresponding lower limit.')
 1110 FORMAT(' Warning: lower limit in x and/or Q2 too small for ',
     &'DIS formalism.')
 1150 FORMAT(' Warning: weak charged current cross section zero for ',
     &'specified lepton helicity; LEPIN, PARL(6) =',I3,F5.2)
 1200 FORMAT(' Warning: unrecognized interaction in LINIT call: ',
     &'INTER = ',I5,'  for lepton LEPIN =',I5)
 1210 FORMAT(' Warning: unallowed value of LST(1) =',I3,
     &' and/or LST(31) =',I3)
 1220 FORMAT(/,' User-defined optimization parameters:',
     &/,5X,'OPTX(1...4)  =',4G11.3,/,5X,'OPTY(1...4)  =',4G11.3,
     &/,5X,'OPYQ2(1...4) =',4G11.3,/,5X,'OPTW2(1...4) =',4G11.3,/)
 1250 FORMAT(/,' Parameter values:', 
     &'LST(I+10)',8X,'PARL(I)',5X,'PARL(I+10)',1P,
     &/,5X,55('-'),10(/,3I10,2G15.4),/)
 1300 FORMAT(' Warning: cross section, PARL(23), excludes FL (see ',
     &'LST(11)) from:')
 1310 FORMAT(10X,'QCD, since evaluated event by event for LQCD=2')
 1320 FORMAT(10X,'TM , since evaluated event by event for LTM =2')
 1330 FORMAT(' Cross section in PARL(24) includes these contributions.')
 1400 FORMAT(' Max of differential cross section (for weighting) =',
     &E12.4,/,' obtained in ',F7.2,' seconds.',/)
*...Added also LST(40)
 1500 FORMAT( 
     &'with those used when calculating QCD weights.', 
     &'current value     value for weights',/,
     &/,'     LST(12)   ',I12,10X,I12,
     &/,'     LST(13)   ',I12,10X,I12,
     &/,'     LST(15)   ',I12,10X,I12,
     &/,'     LST(16)   ',I12,10X,I12,
     &/,'     LST(17)   ',I12,10X,I12,
     &/,'     LST(23)   ',I12,10X,I12,
     &/,'     LST(40)   ',I12,10X,I12,
     &/,'     PARL(1)   ',E12.4,10X,E12.4,
     &/,'     PARL(2)   ',E12.4,10X,E12.4,
     &/,'     PARL(5)   ',E12.4,10X,E12.4,
     &/,'     PARL(6)   ',E12.4,10X,E12.4)
*---
 1510 FORMAT(/,' Time for calculating QCD weights =',F5.1,' seconds',/)
 1900 FORMAT(' Execution stopped ',/)
      END

*72*********************************************************************
      SUBROUTINE DSTFU(KF,X,Q2,XDPQ)

       IMPLICIT NONE

C...Gives proton and neutron structure functions according to a few
C...different polarized parametrizations.
C...Note that what is coded is x times the probability distribution,
C...i.e. xdq(x,Q2) etc.

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /ARSTRF/ KFSAVE(2),XSAVE(2),XQ2SAV(2),
     +		XPQSAV(2,-6:6),XDPQSAV(2,-6:6)
      INTEGER KFSAVE
      REAL XSAVE,XQ2SAV,XPQSAV,XDPQSAV
      SAVE /ARSTRF/


      INTEGER KF,KFL,KFA
      REAL X,Q2,XPQ,XDPQ,XDPS
      DIMENSION XPQ(-6:6),XDPQ(-6:6)

C...Reset structure functions.
      DO 100 KFL=-6,6
      XDPQ(KFL)=0.
 100  XDPQSAV(1,KFL)=0.
      XSAVE(1)=X
      XQ2SAV(1)=Q2
      KFSAVE(1)=KF

C...Check x and particle species.
      IF(X.LE.0..OR.X.GE.1.) THEN
	WRITE(6,5000) X
	RETURN
      ENDIF
      KFA=IABS(KF)
      IF(KFA.NE.2112.AND.KFA.NE.2212) THEN
	WRITE(6,5100) KF
	RETURN
      ENDIF

*...  Put into output array  
      CALL PARTON(X,Q2,XPQ,XDPQ)


C...Check positivity and reset above maximum allowed flavour.
      DO 180 KFL=-6,6
      IF(IABS(KFL).GT.LST(12)) THEN 
        XDPQ(KFL)=0.
      ENDIF
 180  CONTINUE

C...  Isospin conjugation for neutron.
*HI>>      
      IF(KFA.EQ.2112.AND.LST(39).EQ.0) THEN
*     IF(KFA.EQ.2112) THEN
*HI<<         
	XDPS = XDPQ(1)
	XDPQ(1) = XDPQ(2)
	XDPQ(2) = XDPS
	XDPS = XDPQ(-1)
	XDPQ(-1) = XDPQ(-2)
	XDPQ(-2) = XDPS
      ENDIF

C...Charge conjugation for antiparticle.
      IF(KF.LT.0) THEN
	DO 170 KFL=1,6
	XDPS = XDPQ(KFL)
	XDPQ(KFL) = XDPQ(-KFL)
	XDPQ(-KFL) = XDPS
  170   CONTINUE
        ENDIF

      DO 120 KFL=-6,6
  120 XDPQSAV(1,KFL)=XDPQ(KFL)

C...Formats for error printouts.
 5000 FORMAT(' Error: x value outside physical range; x =',1P,E12.3)
 5100 FORMAT(' Error: illegal particle code for structure function;',
     &' KF =',I5)
 5200 FORMAT(' Error: unknown structure function; KF, library, set =',
     &3I5)

      RETURN
      END


*72*********************************************************************

      SUBROUTINE POLPAR(IFLAG,X,Q2,UVAL,DVAL,GLUE,QBAR,STR)

      IMPLICIT NONE

      INTEGER IFLAG
      DOUBLE PRECISION X,Q2,UVAL,DVAL,GLUE,QBAR,STR,AUX(5)

c ---- LO polarized parton distributions as described in
c ----    T. Gehrmann and W.J. Stirling:
c ----    "Polarized Parton Distributions of the Nucleon"
c ----    Durham preprint DTP/95/82
c ---- uval = u-ubar
c ---- dval = d-dbar 
c ---- glue 
c ---- qbar = ubar = dbar = 1/2 usea = 1/2 dsea
c ---- str = sbar = 1/2 strsea
 
      call rdarry(x,q2,aux,iflag)
      uval=aux(1)*(x**0.6d0*(1.d0-x)**3)
      dval=aux(2)*(x**0.75d0*(1.d0-x)**4)
      glue=aux(3)*(x**0.5d0*(1.d0-x)**5)
      qbar=aux(4)*(x**0.5d0*(1.d0-x)**6)
      str=aux(5)*(x**0.5d0*(1.d0-x)**6)
 
      return
      end

*72*********************************************************************
      subroutine rdarry(x,q2,aux,iflag)
      implicit real*8(a-h,o-z)
      implicit integer(i-n)
      data icount /0/
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      
      dimension aux(5)
      common/pdist/arraya(151,20,6)

      nx=151
      ndata=nx-1
      nq2pts=20
      nq2inv=nq2pts-1
      q2sta=1.d0
      q2fin=1.d6
      ymin=5.d0
      xmin=10.d0**(-ymin)
      xmax=1.d0

      if (q2.lt.q2sta) then
         q2=q2sta
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
            WRITE(*,*) 'WARNING : rdarry : Q^2 set to minimal value !',Q2
         ENDIF   
      endif
      if (q2.gt.q2fin) then
         q2=q2fin
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
            WRITE(*,*) 'WARNING : rdarry : Q^2 set to maximal value !',Q2
         ENDIF
      endif
      if (x.lt.xmin) then
         x=xmin
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
            WRITE(*,*) 'WARNING : rdarry : X set to minimal value !',X
         ENDIF   
      endif
      if (x.gt.xmax) then
         x=xmax
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
            WRITE(*,*) 'WARNING : rdarry : X set to maximal value !',X
         ENDIF         
      endif
      
      y=dlog10(x)
      ram=(y+ymin)*ndata/ymin+1.d0
      iram=int(ram)
      fraci=ram-dble(iram)
      ram=dlog(q2/q2sta)*nq2inv/dlog(q2fin/q2sta)+1.d0
      jram=int(ram)
      fracj=ram-dble(jram)
      
      do i=1,5
         aux(i)=(arraya(iram,jram,i)*(1.d0-fraci)
     .        +arraya(iram+1,jram,i)*fraci)*(1.d0-fracj)+
     .        (arraya(iram,jram+1,i)*(1.d0-fraci)
     .        +arraya(iram+1,jram+1,i)*fraci)*fracj
      enddo
      
      return
      end

*72*********************************************************************
      subroutine polini
      implicit real*8(a-h,o-z)
      common/pdist/arraya(151,20,6)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      open(IPLST(3),file=CPOL,status='UNKNOWN')
      do i=1,20
         do j=1,151
            read(IPLST(3),901) arraya(j,i,1),arraya(j,i,2),
     &           arraya(j,i,3),arraya(j,i,4),arraya(j,i,5)
         enddo
      enddo
      close(IPLST(3))
      
 901  format(5f14.9)
      
      return
      end
*72*********************************************************************
      SUBROUTINE MRSEB
     &                  (X,SCALE,MODE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
C***************************************************************C
C								C
C     This is a package for the new MRS(A prime,G) parton       C
C     distributions. The minimum Q^2  value is 5 GeV^2 and the  C
C     x range is, as before 10^-5 < x < 1. MSbar factorization  C
C     is used. The package reads 2 grids, which are in separate C
C     files (A prime=for020.dat/ftn20, G=for021.dat/ftn21).     C  
C     Note that x times the parton distribution is returned,    C
C     Q is the scale in GeV,                                    C
C     and Lambda(MSbar,nf=4) = 231/255 MeV for A prime/G.       C
C								C
C	MODE=20 for MRS(A prime)                                C
C	MODE=21 for MRS(G)                                      C
C								C
C         The reference is :                                    C
C         A.D. Martin, R.G. Roberts and W.J. Stirling,          C
C         Phys. Lett. B354 (1995) 155-162                       C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C             >>>>>>>>  CROSS CHECK  <<<<<<<<                   C
C                                                               C
C         THE FIRST NUMBER IN THE 20 GRID IS 0.00341            C
C         THE FIRST NUMBER IN THE 21 GRID IS 0.00269            C
C                                                               C
C         HI: changed SCALE to Q^2                              C
C***************************************************************C
      IMPLICIT REAL*8(A-H,O-Z)
      DATA ICOUNT /0/
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      Q2=SCALE
*     Q2STA= .5D0
      Q2STA = .625D0
      Q2FIN = 1310720.D0
      Xmin = 1D-5
      Xmax = 1D0

      if (q2.lt.q2sta) then
         q2=q2sta
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to minimal value !',Q2
         ENDIF   
      endif
      if (q2.gt.q2fin) then
         q2=q2fin
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to maximal value !',Q2
         ENDIF
      endif
      if (x.lt.xmin) then
         x=xmin
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to minimal value !',X
         ENDIF   
      endif
      if (x.gt.xmax) then
         x=xmax
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to maximal value !',X
         ENDIF         
      endif

      IF(MODE.EQ.20) 
     .   CALL STRC20(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.21) 
     .   CALL STRC21(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.10) THEN 
         IF(Q2.LT.0.625D0.OR.Q2.GT.1310720.D0) PRINT 99
      IF(Q2.GT.5D0) CALL STRC10(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,
     X        GLU)
      IF(Q2.LE.5D0) CALL STRC11(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,
     X        GLU)
 99      FORMAT('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ')
      ENDIF
*  check for positive definiteness

      IF(UPV.LT.0.0) UPV=0.0
      IF(DNV.LT.0.0) DNV=0.0
      IF(USEA.LT.0.0) USEA=0.0
      IF(DSEA.LT.0.0) DSEA=0.0
      IF(GLU.LT.0.0) GLU=0.0
      IF(STR.LT.0.0) STR=0.0
      IF(CHM.LT.0.0) CHM=0.0
      IF(BOT.LT.0.0) BOT=0.0

      RETURN
      END
*72*********************************************************************
      
      SUBROUTINE STRC20(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
C     THIS IS THE NEW  "Aprime" FIT -- Feb 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      save F
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      OPEN(IPLST(2),FILE=CUNPOL,STATUS='OLD')
      DO 20 N=1,nx-1
         DO 20 M=1,19
            READ(IPLST(2),50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),
     &           F(5,N,M),F(7,N,M),F(6,N,M),F(8,N,M)
C     1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
            DO 25 I=1,8
 25         F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
 20   CONTINUE
      CLOSE(IPLST(2))
      DO 31 J=1,NTENTH-1
         XX(J)=DLOG10(XX(J))+1.1D0
         DO 31 I=1,8
            IF(I.EQ.7) GO TO 31
            DO 30 K=1,19
 30            F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)
     &              /DLOG(F(I,ntenth,K))
 31   CONTINUE
 50   FORMAT(8F10.5)
      DO 40 I=1,8
         DO 40 M=1,19
 40   F(I,nx,M)=0.D0
 10   CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
         G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .        + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
         IF(N.GE.ntenth) GOTO 65
         IF(I.EQ.7) GOTO 65
         FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
         G(I)=FAC**(G(I)/FAC)
 65      CONTINUE
         G(I)=G(I)*(1.D0-X)**N0(I)
 60   CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE STRC21(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE NEW  "G" FIT -- Feb 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      save F
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      OPEN(IPLST(2),FILE='./pdf/MRSa1.dat',STATUS='OLD')
      DO 20 N=1,nx-1
         DO 20 M=1,19
            READ(IPLST(2),50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),
     &           F(5,N,M),F(7,N,M),F(6,N,M),F(8,N,M)
C     1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
            DO 25 I=1,8
 25         F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
 20   CONTINUE
      CLOSE(IPLST(2))
      DO 31 J=1,NTENTH-1
         XX(J)=DLOG10(XX(J))+1.1D0
         DO 31 I=1,8
            IF(I.EQ.7) GO TO 31
            DO 30 K=1,19
 30         F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
 50   FORMAT(8F10.5)
      DO 40 I=1,8
         DO 40 M=1,19
 40   F(I,nx,M)=0.D0
 10   CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
         G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     &        + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
         IF(N.GE.ntenth) GOTO 65
         IF(I.EQ.7) GOTO 65
         FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
         G(I)=FAC**(G(I)/FAC)
 65      CONTINUE
         G(I)=G(I)*(1.D0-X)**N0(I)
 60   CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE STRC11(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE NEW  "A" FIT -- May 1994 -- low Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,8),G(8),XX(NX),N0(8)
      save F
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,0.625D0,5.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/

      xsave=x  ! don't let x be altered if it's out of range!!

      IF(INIT.NE.0) GOTO 10
      INIT=1
      OPEN(IPLST(3),FILE=CPOL,STATUS='OLD')
      DO 20 N=1,nx-1
         DO 20 M=1,7
            READ(IPLST(3),50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),
     &           F(5,N,M),F(7,N,M),F(6,N,M),F(8,N,M)
C     1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
            DO 25 I=1,8
  25        F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CLOSE(IPLST(3))
      DO 31 J=1,NTENTH-1
         XX(J)=DLOG10(XX(J))+1.1D0
         DO 31 I=1,8
            IF(I.EQ.7.or.i.eq.5) GO TO 31
            DO 30 K=1,7
 30         F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)
     &              /DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,7
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)*2D0
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7.or.i.eq.5) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)

      x=xsave  !restore x

      RETURN
      END      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      
      SUBROUTINE STRC10(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE NEW  "A" FIT -- May 1994 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      save F
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      OPEN(IPLST(2),FILE=CUNPOL,STATUS='OLD')
      DO 20 N=1,nx-1
         DO 20 M=1,19
            READ(IPLST(2),50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),
     &           F(5,N,M),F(7,N,M),F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
            DO 25 I=1,8
 25         F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
 20   CONTINUE
      CLOSE(IPLST(2))
      DO 31 J=1,NTENTH-1
         XX(J)=DLOG10(XX(J))+1.1D0
         DO 31 I=1,8
            IF(I.EQ.7) GO TO 31
            DO 30 K=1,19
 30         F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C============================================================================
C                CTEQ Parton Distribution Functions: Version 4.6
C                             June 21, 1996
C                   Modified: 10/17/96, 1/7/97, 1/15/97
C                             2/17/97, 2/21/97
C                   Last Modified on April 2, 1997
C
C   Ref[1]: "IMPROVED PARTON DISTRIBUTIONS FROM GLOBAL ANALYSIS OF RECENT DEEP
C         INELASTIC SCATTERING AND INCLUSIVE JET DATA"
C   By: H.L. Lai, J. Huston, S. Kuhlmann, F. Olness, J. Owens, D. Soper
C       W.K. Tung, H. Weerts
C       Phys. Rev. D55, 1280 (1997)
C
C   Ref[2]: "CHARM PRODUCTION AND PARTON DISTRIBUTIONS"
C   By: H.L. Lai and W.K. Tung
C       MSU-HEP-61222, CTEQ-622, e-Print Archive: hep-ph/9701256
C       to appear in Z. Phys.
C
C   This package contains 13 sets of CTEQ4 PDFs. Details are:
C ---------------------------------------------------------------------------
C  Iset   PDF        Description       Alpha_s(Mz)  Lam4  Lam5   Table_File
C ---------------------------------------------------------------------------
C Ref[1]
C   1    CTEQ4M   Standard MSbar scheme   0.116     298   202    cteq4m.tbl
C   2    CTEQ4D   Standard DIS scheme     0.116     298   202    cteq4d.tbl
C   3    CTEQ4L   Leading Order           0.132     236   181    cteq4l.tbl
C   4    CTEQ4A1  Alpha_s series          0.110     215   140    cteq4a1.tbl
C   5    CTEQ4A2  Alpha_s series          0.113     254   169    cteq4a2.tbl
C   6    CTEQ4A3            ( same as CTEQ4M )
C   7    CTEQ4A4  Alpha_s series          0.119     346   239    cteq4a4.tbl
C   8    CTEQ4A5  Alpha_s series          0.122     401   282    cteq4a5.tbl
C   9    CTEQ4HJ  High Jet                0.116     303   206    cteq4hj.tbl
C   10   CTEQ4LQ  Low Q0                  0.114     261   174    cteq4lq.tbl
C ---------------------------------------------------------------------------
C Ref[2]
C   11   CTEQ4HQ  Heavy Quark             0.116     298   202    cteq4hq.tbl
C   12   CTEQ4HQ1 Heavy Quark:Q0=1,Mc=1.3 0.116     298   202    cteq4hq1.tbl
C        (Improved version of CTEQ4HQ, recommended)
C   13   CTEQ4F3  Nf=3 FixedFlavorNumber  0.106     (Lam3=385)   cteq4f3.tbl
C   14   CTEQ4F4  Nf=4 FixedFlavorNumber  0.111     292   XXX    cteq4f4.tbl
C ---------------------------------------------------------------------------
C   
C   The available applied range is 10^-5 < x < 1 and 1.6 < Q < 10,000 (GeV) 
C   except CTEQ4LQ(4HQ1) for which Q starts at a lower value of 0.7(1.0) GeV.  
C   Lam5 (Lam4, Lam3) represents Lambda value (in MeV) for 5 (4,3) flavors. 
C   The matching alpha_s between 4 and 5 flavors takes place at Q=5.0 GeV,  
C   which is defined as the bottom quark mass, whenever it can be applied.
C
C   The Table_Files are assumed to be in the working directory.
C   
C   Before using the PDF, it is necessary to do the initialization by
C       Call SetCtq4(Iset) 
C   where Iset is the desired PDF specified in the above table.
C   
C   The function Ctq4Pdf (Iparton, X, Q)
C   returns the parton distribution inside the proton for parton [Iparton] 
C   at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                            for (b, c, s, d, u, g, u_bar, ..., b_bar),
C      whereas CTEQ4F3 has, by definition, only 3 flavors and gluon;
C              CTEQ4F4 has only 4 flavors and gluon.
C   
C   For detailed information on the parameters used, e.q. quark masses, 
C   QCD Lambda, ... etc.,  see info lines at the beginning of the 
C   Table_Files.
C
C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single 
C   precision.
C   
C   If you have detailed questions concerning these CTEQ4 distributions, 
C   or if you find problems/bugs using this package, direct inquires to 
C   Hung-Liang Lai(Lai_H@pa.msu.edu) or Wu-Ki Tung(Tung@pa.msu.edu).
C   
C===========================================================================

      Function Ctq4Pdf (Iparton, X, Q2)
      Implicit Double Precision (A-H,O-Z)

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/


      Logical Warn
      Common
     > / CtqPar2 / Nx, Nt, NfMx
     > / QCDtable /  Alambda, Nfl, Iorder
     > / XQrange / Qini, Qmax, Xmin

      Data Warn /.true./
      save Warn
      Q2STA = Qini
      Q2FIN = Qmax
      Xmax = 1D0
 

      if (q2.lt.q2sta) then
         q2=q2sta
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : Q^2 set to minimal value !',Q2
         ENDIF
      endif
      if (q2.gt.q2fin) then
         q2=q2fin
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : Q^2 set to maximal value !',Q2
         ENDIF
      endif
      if (x.lt.xmin) then
         x=xmin
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : X set to minimal value !',X
         ENDIF
      endif
      if (x.gt.xmax) then
         x=xmax
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : CTEQ : X set to maximal value !',X
         ENDIF
      endif
      
      Q=SQRT(Q2)

      If (X .lt. 0D0 .or. X .gt. 1D0) Then
        Print *, 'X out of range in Ctq4Pdf: ', X
        Stop
      Endif
      If (Q .lt. Alambda) Then
        Print *, 'Q out of range in Ctq4Pdf: ', Q
        Stop
      Endif
      If ((Iparton .lt. -NfMx .or. Iparton .gt. NfMx)) Then
         If (Warn) Then
C        put a warning for calling extra flavor.
             Warn = .false.
             Print *, 'Warning: Iparton out of range in Ctq4Pdf: '
     &              , Iparton
         Endif
         Ctq4Pdf = 0D0
         Return
      Endif

      Ctq4Pdf = PartonX (Iparton, X, Q)
      if(Ctq4Pdf.lt.0.D0)  Ctq4Pdf = 0.D0

      Return

C                             ********************
      End

C============================================================================
C                CTEQ Parton Distribution Functions: Version 5.0
C                             Nov. 1, 1999
C
C   Ref: "GLOBAL QCD ANALYSIS OF PARTON STRUCTURE OF THE NUCLEON:
C         CTEQ5 PPARTON DISTRIBUTIONS"
C
C  hep-ph/9903282; to be published in Eur. Phys. J. C 1999.
C
C  These PDF's use quadratic interpolation of attached tables. A parametrized 
C  version of the same PDF's without external tables is under construction.  
C  They will become available later.
C
C   This package contains 7 sets of CTEQ5 PDF's; plus two updated ones.
C   The undated CTEQ5M1 and CTEQHQ1 use an improved evolution code.
C   Both the original and the updated ones fit current data with comparable
C   accuracy.  The CTEQHQ1 set also involve a different choice of scale,
C   hence differs from CTEQHQ slightly more.  It is preferred over CTEQ5HQ.

C   Details are:
C ---------------------------------------------------------------------------
C  Iset   PDF        Description       Alpha_s(Mz)  Lam4  Lam5   Table_File
C ---------------------------------------------------------------------------
C x 1    CTEQ5M   Standard MSbar scheme   0.118     326   226    cteq5m.tbl
C   2    CTEQ5D   Standard DIS scheme     0.118     326   226    cteq5d.tbl
C x 3    CTEQ5L   Leading Order           0.127     192   146    cteq5l.tbl
C   4    CTEQ5HJ  Large-x gluon enhanced  0.118     326   226    cteq5hj.tbl
C   5    CTEQ5HQ  Heavy Quark             0.118     326   226    cteq5hq.tbl
C   6    CTEQ5F3  Nf=3 FixedFlavorNumber  0.106     (Lam3=395)   cteq5f3.tbl
C   7    CTEQ5F4  Nf=4 FixedFlavorNumber  0.112     309   XXX    cteq5f4.tbl
C         --------------------------------------------------------
C x 8    CTEQ5M1  Improved CTEQ5M         0.118     326   226    cteq5m1.tbl
C   9    CTEQ5HQ1 Improved CTEQ5HQ        0.118     326   226    ctq5hq1.tbl
C ---------------------------------------------------------------------------
C   
C  The available applied range is 10^-5 << x << 1 and 1.0 << Q << 10,000 (GeV).
C   Lam5 (Lam4, Lam3) represents Lambda value (in MeV) for 5 (4,3) flavors. 
C   The matching alpha_s between 4 and 5 flavors takes place at Q=4.5 GeV,  
C   which is defined as the bottom quark mass, whenever it can be applied.
C
C   The Table_Files are assumed to be in the working directory.
C   
C   Before using the PDF, it is necessary to do the initialization by
C       Call SetCtq5(Iset) 
C   where Iset is the desired PDF specified in the above table.
C   
C   The function Ctq5Pdf (Iparton, X, Q)
C   returns the parton distribution inside the proton for parton [Iparton] 
C   at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                            for (b, c, s, d, u, g, u_bar, ..., b_bar),
C      whereas CTEQ5F3 has, by definition, only 3 flavors and gluon;
C              CTEQ5F4 has only 4 flavors and gluon.
C   
C   For detailed information on the parameters used, e.q. quark masses, 
C   QCD Lambda, ... etc.,  see info lines at the beginning of the 
C   Table_Files.
C
C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single 
C   precision.
C   
C   If you have detailed questions concerning these CTEQ5 distributions, 
C   or if you find problems/bugs using this package, direct inquires to 
C   Hung-Liang Lai(lai@phys.nthu.edu.tw) or Wu-Ki Tung(Tung@pa.msu.edu).
C   
C===========================================================================

      Function Ctq5Pdf (Iparton, X, Q)
      Implicit Double Precision (A-H,O-Z)

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      Logical Warn
      Common
     > / CtqPar2 / Nx, Nt, NfMx
     > / QCDtable /  Alambda, Nfl, Iorder
     > / XQrange / Qini, Qmax, Xmin

      Data Warn /.true./
      save Warn

      If (X .lt. 0D0 .or. X .gt. 1D0) Then
	Print *, 'X out of range in Ctq5Pdf: ', X
	Stop
      Endif
      If (Q .lt. Alambda) Then
	Print *, 'Q out of range in Ctq5Pdf: ', Q
	Stop
      Endif
      If ((Iparton .lt. -NfMx .or. Iparton .gt. NfMx)) Then
         If (Warn) Then
C        put a warning for calling extra flavor.
	     Warn = .false.
	     Print *, 'Warning: Iparton out of range in Ctq5Pdf: '
     >              , Iparton
         Endif
         Ctq5Pdf = 0D0
         Return
      Endif

      Ctq5Pdf = PartonX5 (Iparton, X, Q)
      if(Ctq5Pdf.lt.0.D0)  Ctq5Pdf = 0.D0

      Return

C                             ********************
      End

C============================================================================
C                CTEQ Parton Distribution Functions: Version 6.0
C                             January 24, 2002
C
C   Ref: "New Generation of Parton Distributions with
C         Uncertainties from Global QCD Analysis"
C   By: J. Pumplin, D.R. Stump, J.Huston, H.L. Lai, P. Nadolsky, W.K. Tung
C       hep-ph/0201195
C
C   This package contains 3 standard sets of CTEQ6 PDF's and 40 up/down sets
C   with respect to CTEQ6M PDF's. Details are:
C ---------------------------------------------------------------------------
C  Iset   PDF        Description       Alpha_s(Mz)**Lam4  Lam5   Table_File
C ---------------------------------------------------------------------------
C   1    CTEQ6M   Standard MSbar scheme   0.118     326   226    cteq6m.tbl
C   2    CTEQ6D   Standard DIS scheme     0.118     326   226    cteq6d.tbl
C   3    CTEQ6L   Leading Order           0.118**   326** 226    cteq6l.tbl
C     ------------------------------
C   1xx  CTEQ6M1xx  +/- w.r.t. CTEQ6M     0.118     326   226    cteq6m1xx.tbl
C    (where xx=01--40)
C ---------------------------------------------------------------------------
C   ** ALL fits are obtained by using the same coupling strength \alpha_s(Mz)=0.118;
C   and the NLO running \alpha_s formula.  For the LO fit, the evolution of the PDF
C   and the hard cross sections are calculated at LO.  More detailed discussions are
C   given in hep-ph/0201195.
C
C   The table grids are generated for 10^-6 < x < 1 and 1.3 < Q < 10,000 (GeV).
C   PDF values outside of the above range are returned using extrapolation.
C   Lam5 (Lam4) represents Lambda value (in MeV) for 5 (4) flavors.
C   The matching alpha_s between 4 and 5 flavors takes place at Q=4.5 GeV,
C   which is defined as the bottom quark mass, whenever it can be applied.
C
C   The Table_Files are assumed to be in the working directory.
C
C   Before using the PDF, it is necessary to do the initialization by
C       Call SetCtq6(Iset)
C   where Iset is the desired PDF specified in the above table.
C
C   The function Ctq6Pdf (Iparton, X, Q)
C   returns the parton distribution inside the proton for parton [Iparton]
C   at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                            for (b, c, s, d, u, g, u_bar, ..., b_bar),
C
C   For detailed information on the parameters used, e.q. quark masses,
C   QCD Lambda, ... etc.,  see info lines at the beginning of the
C   Table_Files.
C
C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single
C   precision.
C
C   If you have detailed questions concerning these CTEQ6 distributions,
C   or if you find problems/bugs using this package, direct inquires to
C   Pumplin@pa.msu.edu or Tung@pa.msu.edu.
C
C===========================================================================

      Function Ctq6Pdf (Iparton, X, Q)
      Implicit Double Precision (A-H,O-Z)
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      Logical Warn
      Common
     > / CtqPar2 / Nx, Nt, NfMx
     > / QCDtable /  Alambda, Nfl, Iorder
     > / XQrange / Qini, Qmax, Xmin

      Data Warn /.true./
      save Warn

      If (X .lt. 0D0 .or. X .gt. 1D0) Then
        Print *, 'X out of range in Ctq6Pdf: ', X
        Stop
      Endif
      If (Q .lt. Alambda) Then
        Print *, 'Q out of range in Ctq6Pdf: ', Q
        Stop
      Endif
      If ((Iparton .lt. -NfMx .or. Iparton .gt. NfMx)) Then
         If (Warn) Then
C        put a warning for calling extra flavor.
             Warn = .false.
             Print *, 'Warning: Iparton out of range in Ctq6Pdf: '
     >              , Iparton,NfMx
         Endif
         Ctq6Pdf = 0D0
         Return
      Endif

      Ctq6Pdf = PartonX6 (Iparton, X, Q)
      if(Ctq6Pdf.lt.0.D0)  Ctq6Pdf = 0.D0

      Return

C                             ********************
      End



      subroutine mrs99(x,q,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
C****************************************************************C
C								 C
C     This is a package for the new **corrected** MRST parton    C
C     distributions. The format is similar to the previous       C
C     (1998) MRST series.                                        C
C								 C
C     NOTE: 7 new sets are added here, corresponding to shifting C
C     the small x HERA data up and down by 2.5%, and by varying  C
C     the charm and strange distributions, and by forcing a      C
C     larger d/u ratio at large x.                               C
C								 C
C     As before, x times the parton distribution is returned,    C
C     q is the scale in GeV, MSbar factorization is assumed,     C
C     and Lambda(MSbar,nf=4) is given below for each set.        C
C								 C
C     NAMING SCHEME:                                             C
C						                 C
C  mode  set    comment             L(4)/MeV  a_s(M_Z)  grid#1   C
C  ----  ---    -------             --------  -------   ------   C
C								 C
C  1     COR01  central gluon, a_s    300      0.1175   0.00537  C
C  2     COR02  higher gluon          300      0.1175   0.00497  C
C  3     COR03  lower gluon           300      0.1175   0.00398  C
C  4     COR04  lower a_s             229      0.1125   0.00585  C
C  5     COR05  higher a_s            383      0.1225   0.00384  C
C  6     COR06  quarks up             303.3    0.1178   0.00497  C
C  7     COR07  quarks down           290.3    0.1171   0.00593  C
C  8     COR08  strange up            300      0.1175   0.00524  C
C  9     COR09  strange down          300      0.1175   0.00524  C
C  10    C0R10  charm up              300      0.1175   0.00525  C
C  11    COR11  charm down            300      0.1175   0.00524  C
C  12    COR12  larger d/u            300      0.1175   0.00515  C
C						                 C
C      The corresponding grid files are called cor01.dat etc.    C
C							  	 C
C      The reference is:                                         C
C      A.D. Martin, R.G. Roberts, W.J. Stirling, R.S Thorne      C
C      Univ. Durham preprint DTP/99/64, hep-ph/9907231 (1999)    C
C                                                                C
C      Comments to : W.J.Stirling@durham.ac.uk                   C
C                                                                C
C								 C
C****************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
      if(q2.lt.qsqmin.or.q2.gt.qsqmax) print 99
      if(x.lt.xmin.or.x.gt.xmax)       print 98
          if(mode.eq.1) then
        call mrs991(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.2) then
        call mrs992(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.3) then
        call mrs993(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.4) then
        call mrs994(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.5) then
        call mrs995(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.6) then
        call mrs996(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.7) then
        call mrs997(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.8) then
        call mrs998(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.9) then
        call mrs999(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.10) then
        call mrs9910(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.11) then
        call mrs9911(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.12) then
        call mrs9912(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      endif 
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ')
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ')
      return
      end
c===============================================================
      subroutine mrs991(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      


      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
      open (unit=IPLST(2),file=CUNPOL,status='old')
cc        open(unit=1,file='./pdf/cor01.dat',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c===============================================================
      
      subroutine mrs992(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open (unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c=============================================================
      
      subroutine mrs993(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c===============================================================
      
      
      subroutine mrs994(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
      open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c=============================================================
      
      subroutine mrs995(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
      open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c======================================================================
      

      subroutine mrs996(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
      open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c===================================================================
      
      subroutine mrs997(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c===============================================================


      subroutine mrs998(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c===================================================================
      
      subroutine mrs999(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
C************************************************************************

      subroutine mrs9910(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs9911(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      
      subroutine mrs9912(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=IPLST(2),file=CUNPOL,status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(IPLST(2),50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end

c==================================================================

      Function NextUn5()
C                                 Returns an unallocated FORTRAN i/o unit.
      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUn5 = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End
c==================================================================

      Function NextUn6()
C                                 Returns an unallocated FORTRAN i/o unit.
      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUn6 = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End
c================================================================

      FUNCTION PartonX (IPRTN, X, Q)
C
C   Given the parton distribution function in the array Upd in
C   COMMON / CtqPar1 / , this routine fetches u(fl, x, q) at any value of
C   x and q using Mth-order polynomial interpolation for x and Ln(Q/Lambda).
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      PARAMETER (M= 2, M1 = M + 1)
C
      Logical First
      Common 
     > / CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / CtqPar2 / Nx, Nt, NfMx
     > / XQrange / Qini, Qmax, Xmin
C
      Dimension Fq(M1), Df(M1)

      Data First /.true./
      save First
C                                                 Work with Log (Q)
      QG  = LOG (Q/AL)

C                           Find lower end of interval containing X
      JL = -1
      JU = Nx+1
 11   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (X .GT. XV(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 11
      Endif

      Jx = JL - (M-1)/2
      If (X .lt. Xmin .and. First ) Then
         First = .false.
         Print '(A, 2(1pE12.4))', 
     >     ' WARNING: X < Xmin, extrapolation used; X, Xmin =', X, Xmin
         If (Jx .LT. 0) Jx = 0
      Elseif (Jx .GT. Nx-M) Then
         Jx = Nx - M
      Endif
C                                    Find the interval where Q lies
      JL = -1
      JU = NT+1
 12   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (QG .GT. QL(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 12
      Endif

      Jq = JL - (M-1)/2
      If (Jq .LT. 0) Then
         Jq = 0
         If (Q .lt. Qini)  Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q < Qini, extrapolation used; Q, Qini =', Q, Qini
      Elseif (Jq .GT. Nt-M) Then
         Jq = Nt - M
         If (Q .gt. Qmax)  Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q > Qmax, extrapolation used; Q, Qmax =', Q, Qmax
      Endif

      If (Iprtn .GE. 3) Then
         Ip = - Iprtn
      Else
         Ip = Iprtn
      EndIf
C                             Find the off-set in the linear array Upd
      JFL = Ip + NfMx
      J0  = (JFL * (NT+1) + Jq) * (NX+1) + Jx
C
C                        Now interpolate in x for M1 Qs
      Do 21 Iq = 1, M1
         J1 = J0 + (Nx+1)*(Iq-1) + 1
         Call Polint (XV(Jx), Upd(J1), M1, X, Fq(Iq), Df(Iq))
 21   Continue
C                                          Finish off by interpolating in Q
      Call Polint (QL(Jq), Fq(1), M1, QG, Ftmp, Ddf)

      PartonX = Ftmp
C
      RETURN
C                        ****************************
      END

      Subroutine SetCtq4 (Iset)
      Implicit Double Precision (A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      Parameter (Isetmax=14)
      Character Flnm(Isetmax)*12, Tablefile*40
      Data (Flnm(I), I=1,Isetmax)
     > / 'cteq4m.tbl', 'cteq4d.tbl', 'cteq4l.tbl'
     > , 'cteq4a1.tbl', 'cteq4a2.tbl', 'cteq4m.tbl', 'cteq4a4.tbl'
     > , 'cteq4a5.tbl', 'cteq4hj.tbl', 'cteq4lq.tbl'
     > , 'cteq4hq.tbl', 'cteq4hq1.tbl', 'cteq4f3.tbl', 'cteq4f4.tbl' /
      Data Tablefile / 'test.tbl' /
      Data Isetold, Isetmin, Isettest / -987, 1, 911 /
      save

C             If data file not initialized, do so.
C    Rewrite file name to fit Pepsi distribution format
C
      If(Iset.ne.Isetold) then
	 IU= NextUn()
         If (Iset .eq. Isettest) then
            Print* ,'Opening ', Tablefile
 21         Open(IU, File=Tablefile, Status='OLD', Err=101)
         ElseIf (Iset.lt.Isetmin .or. Iset.gt.Isetmax) Then
	    Print *, 'Invalid Iset number in SetCtq4 :', Iset
	    Stop
         Else
*            Tablefile=Flnm(Iset)
*            Tablefile='./pdf/' 
            Open(IPLST(2), File=CUNPOL, Status='OLD', Err=100)
            IU=IPLST(2)
	 Endif
         Call ReadTbl (IU)
         Close (IU)
	 Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     > 
      Stop
 101  Print*, Tablefile, ' cannot be opened '
      Print*, 'Please input the .tbl file:'
      Read (*,'(A)') Tablefile
      Goto 21
C                             ********************
      End


      Subroutine SetCtq5 (Iset)
      Implicit Double Precision (A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      Parameter (Isetmax=9)
      Character Flnm(Isetmax)*16, Tablefile*40
      Data (Flnm(I), I=1,Isetmax)
     > / './pdf/cteq5m.tbl', './pdf/cteq5d.tbl', './pdf/cteq5l.tbl', 
     >   './pdf/cteq5hj.tbl'
     > , './pdf/cteq5hq.tbl', './pdf/cteq5f3.tbl', './pdf/cteq5f4.tbl'
     > , './pdf/cteq5m1.tbl', './pdf/ctq5hq1.tbl'  /
      Data Tablefile / 'test.tbl' /
      Data Isetold, Isetmin, Isettest / -987, 1, 911 /
      save

C             If data file not initialized, do so.
      If(Iset.ne.Isetold) then
	 IU= NextUn5()
         If (Iset .eq. Isettest) then
            Print* ,'Opening ', Tablefile
 21         Open(IU, File=Tablefile, Status='OLD', Err=101)
            GoTo 22
 101        Print*, Tablefile, ' cannot be opened '
            Print*, 'Please input the .tbl file:'
            Read (*,'(A)') Tablefile
            Goto 21
 22         Continue
         ElseIf (Iset.lt.Isetmin .or. Iset.gt.Isetmax) Then
	    Print *, 'Invalid Iset number in SetCtq5 :', Iset
	    Stop
         Else
c            Tablefile=Flnm(Iset)
c            Open(IU, File=Tablefile, Status='OLD', Err=100)
            Print*, 'We are here yeah'
            Print*, CUNPOL
            Open(IPLST(2), File=CUNPOL, Status='OLD', Err=100)
            IU=IPLST(2)
	 Endif
         Call ReadTbl5 (IU)
         Close (IU)
	 Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     >//'in SetCtq5!!'
      Stop
C                             ********************
      End


      Subroutine SetCtq6 (Iset)
      Implicit Double Precision (A-H,O-Z)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      Parameter (Isetmax0=3)
      Character Flnm(Isetmax0)*20, nn*3, Tablefile*40
      Data (Flnm(I), I=1,Isetmax0)
     > / './pdf/cteq6l.tbl', './pdf/cteq6d.tbl', './pdf/cteq6m.tbl' /
      Data Isetold, Isetmin0, Isetmin1, Isetmax1 /-987,1,101,140/
      save

C             If data file not initialized, do so.
      If(Iset.ne.Isetold) then
         IU= NextUn6()
         If (Iset.ge.Isetmin0 .and. Iset.le.Isetmax0) Then
C            Tablefile=Flnm(Iset)//'.tbl'
            Tablefile=Flnm(Iset)
         Elseif (Iset.ge.Isetmin1 .and. Iset.le.Isetmax1) Then
            write(nn,'(I3)') Iset
            Tablefile=Flnm(1)//nn//'.tbl'
         Else
            Print *, 'Invalid Iset number in SetCtq6 :', Iset
            Stop
         Endif
         Open(IU, File=Tablefile, Status='OLD', Err=100)
 21      Call ReadTbl6 (IU)
         Close (IU)
         Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     >//'in SetCtq6!!'
      Stop
C                             ********************
      End



      Subroutine ReadTbl (Nu)
      Implicit Double Precision (A-H,O-Z)
      Character Line*80
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      Common 
     > / CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / CtqPar2 / Nx, Nt, NfMx
     > / XQrange / Qini, Qmax, Xmin
     > / QCDtable /  Alambda, Nfl, Iorder
     > / Masstbl / Amass(6)
      
      Read  (Nu, '(A)') Line     
      Read  (Nu, '(A)') Line
      Read  (Nu, *) Dr, Fl, Al, (Amass(I),I=1,6)
      Iorder = Nint(Dr)
      Nfl = Nint(Fl)
      Alambda = Al

      Read  (Nu, '(A)') Line 
      Read  (Nu, *) NX,  NT, NfMx

      Read  (Nu, '(A)') Line
      Read  (Nu, *) QINI, QMAX, (QL(I), I =0, NT)

      Read  (Nu, '(A)') Line
      Read  (Nu, *) XMIN, (XV(I), I =0, NX)

      Do 11 Iq = 0, NT
         QL(Iq) = Log (QL(Iq) /Al)
   11 Continue
C
C                  Since quark = anti-quark for nfl>2 at this stage, 
C                  we Read  out only the non-redundent data points
C     No of flavors = NfMx (sea) + 1 (gluon) + 2 (valence) 

      Nblk = (NX+1) * (NT+1)
      Npts =  Nblk  * (NfMx+3)
      Read  (Nu, '(A)') Line
      Read  (Nu, *, IOSTAT=IRET) (UPD(I), I=1,Npts)

      Return
C                        ****************************
      End



      Subroutine ReadTbl5 (Nu)
      Implicit Double Precision (A-H,O-Z)
      Character Line*80
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      Common 
     > / CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / CtqPar2 / Nx, Nt, NfMx
     > / XQrange / Qini, Qmax, Xmin
     > / QCDtable /  Alambda, Nfl, Iorder
     > / Masstbl / Amass(6)
      
      Read  (Nu, '(A)') Line     
      Read  (Nu, '(A)') Line
      Read  (Nu, *) Dr, Fl, Al, (Amass(I),I=1,6)
      Iorder = Nint(Dr)
      Nfl = Nint(Fl)
      Alambda = Al

      Read  (Nu, '(A)') Line 
      Read  (Nu, *) NX,  NT, NfMx

      Read  (Nu, '(A)') Line
      Read  (Nu, *) QINI, QMAX, (QL(I), I =0, NT)

      Read  (Nu, '(A)') Line
      Read  (Nu, *) XMIN, (XV(I), I =0, NX)

      Do 11 Iq = 0, NT
         QL(Iq) = Log (QL(Iq) /Al)
   11 Continue
C
C                  Since quark = anti-quark for nfl>2 at this stage, 
C                  we Read  out only the non-redundent data points
C     No of flavors = NfMx (sea) + 1 (gluon) + 2 (valence) 

      Nblk = (NX+1) * (NT+1)
      Npts =  Nblk  * (NfMx+3)
      Read  (Nu, '(A)') Line
      Read  (Nu, *, IOSTAT=IRET) (UPD(I), I=1,Npts)

      Return
C                        ****************************
      End


      Subroutine ReadTbl6 (Nu)
      Implicit Double Precision (A-H,O-Z)
      Character Line*80
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      Common
     > / CtqPar1 / Al, XV(0:MXX), TV(0:MXQ), UPD(MXPQX)
     > / CtqPar2 / Nx, Nt, NfMx
     > / XQrange / Qini, Qmax, Xmin
     > / QCDtable /  Alambda, Nfl, Iorder
     > / Masstbl / Amass(6)

      Read  (Nu, '(A)') Line
      Read  (Nu, '(A)') Line
      Read  (Nu, *) Dr, Fl, Al, (Amass(I),I=1,6)
      Iorder = Nint(Dr)
      Nfl = Nint(Fl)
      Alambda = Al

      Read  (Nu, '(A)') Line
      Read  (Nu, *) NX,  NT, NfMx
      
      Read  (Nu, '(A)') Line
      Read  (Nu, *) QINI, QMAX, (TV(I), I =0, NT)

      Read  (Nu, '(A)') Line
      Read  (Nu, *) XMIN, (XV(I), I =0, NX)

      Do 11 Iq = 0, NT
         TV(Iq) = Log(Log (TV(Iq) /Al))
   11 Continue
C
C                  Since quark = anti-quark for nfl>2 at this stage,
C                  we Read  out only the non-redundent data points
C     No of flavors = NfMx (sea) + 1 (gluon) + 2 (valence)

      Nblk = (NX+1) * (NT+1)
      Npts =  Nblk  * (NfMx+3)
      Read  (Nu, '(A)') Line
      Read  (Nu, *, IOSTAT=IRET) (UPD(I), I=1,Npts)

      Return
C                        ****************************
      End


      Function NextUn()
C                                 Returns an unallocated FORTRAN i/o unit.
      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUn = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End
C

      SUBROUTINE POLINT (XA,YA,N,X,Y,DY)
 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C                                        Adapted from "Numerical Recipes" 
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END

C*********************************************************************
      
      SUBROUTINE POLNLO(IFLAG,X,Q2,
     &                  DXDUV,DXDDV,DXDG,DXDUBAR,DXDDBAR,DXDS)
      IMPLICIT NONE      
      DOUBLE PRECISION AUX(6),XD,Q2D
      DOUBLE PRECISION DXDUV,DXDDV,DXDG,DXDUBAR,DXDDBAR,DXDS      
      INTEGER IFLAG
      REAL X,Q2


c ---- NLO polarized parton distributions as described in
c ---- T. Gehrmann and W.J. Stirling:
c ---- Polarized Parton Distributions of the Nucleon
c ---- Durham preprint DTP/95/82
c ----
c ---- DXDUV = u-ubar
c ---- DXDDV = d-dbar 
c ---- DXDG  = gluon
c ---- DXDUBAR = 1/2 usea
c ---- DXDDBAR = 1/2 dsea
c ---- DXDS = sbar = 1/2 strsea

      XD=DBLE(X)
      Q2D=DBLE(Q2)

      call rdarrynlo(XD,Q2D,AUX,IFLAG)
      DXDUV=aux(1)*(XD**0.6d0*(1.d0-XD)**3)
      DXDDV=aux(2)*(XD**0.75d0*(1.d0-XD)**4)
      DXDG=aux(3)*(XD**0.5d0*(1.d0-XD)**5)
      DXDUBAR=aux(4)*(XD**0.5d0*(1.d0-XD)**6)
      DXDDBAR=aux(5)*(XD**0.5d0*(1.d0-XD)**6)
      DXDS=aux(6)*(XD**0.5d0*(1.d0-XD)**6)
      
      return
      end


      subroutine rdarrynlo(x,q2,aux,iflag)
      implicit real*8(a-h,o-z)
      implicit integer(i-n)
      dimension aux(6)
      common/pdist/arraya(151,20,6)
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      
      nx=151
      ndata=nx-1
      nq2pts=20
      nq2inv=nq2pts-1
      q2sta=1.d0
      q2fin=1.d6
      ymin=5.d0
      xmin=10.d0**(-ymin)
      xmax=1.d0

      if (q2.lt.q2sta) then
         q2=q2sta
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to minimal value !',Q2
         ENDIF   
      endif
      if (q2.gt.q2fin) then
         q2=q2fin
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : Q^2 set to maximal value !',Q2
         ENDIF
      endif
      if (x.lt.xmin) then
         x=xmin
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to minimal value !',X
         ENDIF   
      endif
      if (x.gt.xmax) then
         x=xmax
         IF(CUT(1).lt.X.and.CUT(2).gt.X.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
         WRITE(*,*) 'WARNING : MRSEB : X set to maximal value !',X
         ENDIF         
      endif

      y=dlog10(x)
      ram=(y+ymin)*ndata/ymin+1.d0
      iram=int(ram)
      fraci=ram-dble(iram)
      ram=dlog(q2/q2sta)*nq2inv/dlog(q2fin/q2sta)+1.d0
      jram=int(ram)
      fracj=ram-dble(jram)
      
      do i=1,6
         aux(i)=(arraya(iram,jram,i)*(1.d0-fraci)
     .        +arraya(iram+1,jram,i)*fraci)*(1.d0-fracj)+
     .        (arraya(iram,jram+1,i)*(1.d0-fraci)
     .        +arraya(iram+1,jram+1,i)*fraci)*fracj
      enddo

      return
      end

C*********************************************************************
      subroutine nloini
      implicit real*8(a-h,o-z)
      common/pdist/arraya(151,20,6)
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      open(IPLST(3),file=CPOL,status='UNKNOWN')
      do i=1,20
         do j=1,151
            read(IPLST(3),901) arraya(j,i,1),arraya(j,i,2),
     &        arraya(j,i,3),arraya(j,i,4),arraya(j,i,5),arraya(j,i,6)
         enddo
      enddo
      close(IPLST(3))

 901  format(6f14.9)

      return
      end
*72*********************************************************************
** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                 *
*    G R V  -  P R O T O N  - P A R A M E T R I Z A T I O N S     *
*                                                                 *
*                         1994 UPDATE                             *
*                                                                 *
*                 FOR A DETAILED EXPLANATION SEE                  *
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *
*                                                                 *
*   THE PARAMETRIZATIONS ARE FITTED TO THE EVOLVED PARTONS FOR    *
*        Q**2 / GEV**2  BETWEEN   0.4   AND  1.E6                 *
*             X         BETWEEN  1.E-5  AND   1.                  *
*   LARGE-X REGIONS, WHERE THE DISTRIBUTION UNDER CONSIDERATION   *
*   IS NEGLIGIBLY SMALL, WERE EXCLUDED FROM THE FIT.              *
*                                                                 *
*   HEAVY QUARK THRESHOLDS  Q(H) = M(H)  IN THE BETA FUNCTION :   *
*                   M(C)  =  1.5,  M(B)  =  4.5                   *
*   CORRESPONDING LAMBDA(F) VALUES IN GEV FOR  Q**2 > M(H)**2 :   *
*      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.153,                                *
*      NLO :  LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.131.                                *
*   THE NUMBER OF ACTIVE QUARK FLAVOURS IS  NF = 3  EVERYWHERE    *
*   EXCEPT IN THE BETA FUNCTION, I.E. THE HEAVY QUARKS C,B,...    *
*   ARE NOT PRESENT AS PARTONS IN THE Q2-EVOLUTION.               *
*   IF NEEDED, HEAVY QUARK DENSITIES CAN BE TAKEN FROM THE 1991   *
*   GRV PARAMETRIZATION.                                          *
*                                                                 * 
*   NLO DISTRIBUTIONS ARE GIVEN IN MS-BAR FACTORIZATION SCHEME    *
*     (SUBROUTINE GRV94HO) AS WELL AS IN THE DIS SCHEME (GRV94DI),  *
*   THE LEADING ORDER PARAMETRIZATION IS PROVIDED BY "GRV94LO".   *
*                                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*...INPUT PARAMETERS : 
*
*    X   = MOMENTUM FRACTION 
*    Q2  = SCALE Q**2 IN GEV**2
*
*...OUTPUT (ALWAYS X TIMES THE DISTRIBUTION) :
*
*    UV  = U(VAL) = U - U(BAR)
*    DV  = D(VAL) = D - D(BAR)
*    DEL = D(BAR) - U(BAR)
*    UDB = U(BAR) + D(BAR) 
*    SB  = S = S(BAR)
*    GL  = GLUON
*
*...LO PARAMETRIZATION :
*
*72*********************************************************************      
      SUBROUTINE GRV94LO (X, Q2, UV, DV, DEL, UDB, SB, GL)
      IMPLICIT DOUBLE PRECISION (A - Z)
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER ICOUNT
      DATA ICOUNT/0/
      MU2  = 0.23
      LAM2 = 0.2322 * 0.2322
*HI>>
      IF(Q2.le.MU2) THEN
         Q2=MU2
         IF(CUT(5).lt.Q2.and.CUT(6).gt.Q2.AND.ICOUNT.lt.10) THEN
            ICOUNT = ICOUNT + 1
            WRITE(*,*)
     &'WARNING : GRV94LO predictions are not correct for your setup !'
         ENDIF
         S= 0.D0
         DS=S
         S2=S
         S3=S
      ELSE
         S  = DLOG (DLOG(Q2/LAM2) / DLOG(MU2/LAM2))
         DS = DSQRT (S)
         S2 = S * S
         S3 = S2 * S
      ENDIF
*HI<<       
*...  UV :
      NU  =  2.284 + 0.802 * S + 0.055 * S2
      AKU =  0.590 - 0.024 * S
      BKU =  0.131 + 0.063 * S
      AU  = -0.449 - 0.138 * S - 0.076 * S2
      BU  =  0.213 + 2.669 * S - 0.728 * S2 
      CU  =  8.854 - 9.135 * S + 1.979 * S2
      DU  =  2.997 + 0.753 * S - 0.076 * S2 
      UV  = FV (X, NU, AKU, BKU, AU, BU, CU, DU)
*...  DV :
      ND  =  0.371 + 0.083 * S + 0.039 * S2 
      AKD =  0.376 
      BKD =  0.486 + 0.062 * S
      AD  = -0.509 + 3.310 * S - 1.248 * S2
      BD  =  12.41 - 10.52 * S + 2.267 * S2 
      CD  =  6.373 - 6.208 * S + 1.418 * S2
      DD  =  3.691 + 0.799 * S - 0.071 * S2 
      DV  = FV (X, ND, AKD, BKD, AD, BD, CD, DD)
*...  DEL :
      NE  =  0.082 + 0.014 * S + 0.008 * S2 
      AKE =  0.409 - 0.005 * S
      BKE =  0.799 + 0.071 * S
      AE  = -38.07 + 36.13 * S - 0.656 * S2
      BE  =  90.31 - 74.15 * S + 7.645 * S2 
      CE  =  0.0
      DE  =  7.486 + 1.217 * S - 0.159 * S2 
      DEL = FV (X, NE, AKE, BKE, AE, BE, CE, DE)
*...  UDB :
      ALX =  1.451
      BEX =  0.271 
      AKX =  0.410 - 0.232 * S 
      BKX =  0.534 - 0.457 * S
      AGX =  0.890 - 0.140 * S
      BGX = -0.981
      CX  =  0.320 + 0.683 * S
      DX  =  4.752 + 1.164 * S + 0.286 * S2
      EX  =  4.119 + 1.713 * S
      ESX =  0.682 + 2.978 * S
      UDB = FW (X, S, ALX, BEX, AKX, BKX, AGX, BGX, CX, DX, EX, ESX)
*...  SB :
      ALS =  0.914
      BES =  0.577
      AKS =  1.798 - 0.596 * S
      AS  = -5.548 + 3.669 * DS - 0.616 * S
      BS  =  18.92 - 16.73 * DS + 5.168 * S
      DST =  6.379 - 0.350 * S  + 0.142 * S2
      EST =  3.981 + 1.638 * S
      ESS =  6.402 
      SB  = FWS (X, S, ALS, BES, AKS, AS, BS, DST, EST, ESS)
*...  GL :
      ALG =  0.524
      BEG =  1.088
      AKG =  1.742 - 0.930 * S
      BKG =                     - 0.399 * S2
      AG  =  7.486 - 2.185 * S
      BG  =  16.69 - 22.74 * S  + 5.779 * S2
      CG  = -25.59 + 29.71 * S  - 7.296 * S2
      DG  =  2.792 + 2.215 * S  + 0.422 * S2 - 0.104 * S3
      EG  =  0.807 + 2.005 * S  
      ESG =  3.841 + 0.316 * S 
      GL  = FW (X, S, ALG, BEG, AKG, BKG, AG, BG, CG, DG, EG, ESG)
      RETURN
      END
*
*...NLO PARAMETRIZATION (MS(BAR)) :
*
*72*********************************************************************      
      SUBROUTINE GRV94HO (X, Q2, UV, DV, DEL, UDB, SB, GL)

       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.34
       LAM2 = 0.248 * 0.248
       S  = DLOG (DLOG(Q2/LAM2) / DLOG(MU2/LAM2))
       DS = DSQRT (S)
       S2 = S * S
       S3 = S2 * S
*...UV :
       NU  =  1.304 + 0.863 * S 
       AKU =  0.558 - 0.020 * S
       BKU =          0.183 * S
       AU  = -0.113 + 0.283 * S - 0.321 * S2
       BU  =  6.843 - 5.089 * S + 2.647 * S2 - 0.527 * S3
       CU  =  7.771 - 10.09 * S + 2.630 * S2
       DU  =  3.315 + 1.145 * S - 0.583 * S2 + 0.154 * S3
       UV  = FV (X, NU, AKU, BKU, AU, BU, CU, DU)
*...DV :
       ND  =  0.102 - 0.017 * S + 0.005 * S2 
       AKD =  0.270 - 0.019 * S
       BKD =  0.260
       AD  =  2.393 + 6.228 * S - 0.881 * S2
       BD  =  46.06 + 4.673 * S - 14.98 * S2 + 1.331 * S3
       CD  =  17.83 - 53.47 * S + 21.24 * S2
       DD  =  4.081 + 0.976 * S - 0.485 * S2 + 0.152 * S3
       DV  = FV (X, ND, AKD, BKD, AD, BD, CD, DD)
*...DEL :
       NE  =  0.070 + 0.042 * S - 0.011 * S2 + 0.004 * S3 
       AKE =  0.409 - 0.007 * S
       BKE =  0.782 + 0.082 * S
       AE  = -29.65 + 26.49 * S + 5.429 * S2
       BE  =  90.20 - 74.97 * S + 4.526 * S2 
       CE  =  0.0
       DE  =  8.122 + 2.120 * S - 1.088 * S2 + 0.231 * S3
       DEL = FV (X, NE, AKE, BKE, AE, BE, CE, DE)
*...UDB :
       ALX =  0.877
       BEX =  0.561
       AKX =  0.275 
       BKX =  0.0
       AGX =  0.997
       BGX =  3.210 - 1.866 * S
       CX  =  7.300
       DX  =  9.010 + 0.896 * DS + 0.222 * S2
       EX  =  3.077 + 1.446 * S
       ESX =  3.173 - 2.445 * DS + 2.207 * S
       UDB = FW (X, S, ALX, BEX, AKX, BKX, AGX, BGX, CX, DX, EX, ESX)
*...SB :
       ALS =  0.756
       BES =  0.216
       AKS =  1.690 + 0.650 * DS - 0.922 * S
       AS  = -4.329 + 1.131 * S
       BS  =  9.568 - 1.744 * S
       DST =  9.377 + 1.088 * DS - 1.320 * S + 0.130 * S2
       EST =  3.031 + 1.639 * S
       ESS =  5.837 + 0.815 * S
       SB  = FWS (X, S, ALS, BES, AKS, AS, BS, DST, EST, ESS)
*...GL :
       ALG =  1.014
       BEG =  1.738
       AKG =  1.724 + 0.157 * S
       BKG =  0.800 + 1.016 * S
       AG  =  7.517 - 2.547 * S
       BG  =  34.09 - 52.21 * DS + 17.47 * S
       CG  =  4.039 + 1.491 * S
       DG  =  3.404 + 0.830 * S
       EG  = -1.112 + 3.438 * S  - 0.302 * S2
       ESG =  3.256 - 0.436 * S 
       GL  = FW (X, S, ALG, BEG, AKG, BKG, AG, BG, CG, DG, EG, ESG)
       RETURN
       END
*
*...NLO PARAMETRIZATION (DIS) :
*
*72*********************************************************************
      SUBROUTINE GRV94DI (X, Q2, UV, DV, DEL, UDB, SB, GL)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.34
       LAM2 = 0.248 * 0.248
       S  = DLOG (DLOG(Q2/LAM2) / DLOG(MU2/LAM2))
       DS = DSQRT (S)
       S2 = S * S
       S3 = S2 * S
*...UV :
       NU  =  2.484 + 0.116 * S + 0.093 * S2 
       AKU =  0.563 - 0.025 * S
       BKU =  0.054 + 0.154 * S
       AU  = -0.326 - 0.058 * S - 0.135 * S2
       BU  = -3.322 + 8.259 * S - 3.119 * S2 + 0.291 * S3
       CU  =  11.52 - 12.99 * S + 3.161 * S2
       DU  =  2.808 + 1.400 * S - 0.557 * S2 + 0.119 * S3
       UV  = FV (X, NU, AKU, BKU, AU, BU, CU, DU)
*...DV :
       ND  =  0.156 - 0.017 * S 
       AKD =  0.299 - 0.022 * S
       BKD =  0.259 - 0.015 * S
       AD  =  3.445 + 1.278 * S + 0.326 * S2
       BD  = -6.934 + 37.45 * S - 18.95 * S2 + 1.463 * S3
       CD  =  55.45 - 69.92 * S + 20.78 * S2
       DD  =  3.577 + 1.441 * S - 0.683 * S2 + 0.179 * S3
       DV  = FV (X, ND, AKD, BKD, AD, BD, CD, DD)
*...DEL :
       NE  =  0.099 + 0.019 * S + 0.002 * S2  
       AKE =  0.419 - 0.013 * S
       BKE =  1.064 - 0.038 * S
       AE  = -44.00 + 98.70 * S - 14.79 * S2 
       BE  =  28.59 - 40.94 * S - 13.66 * S2 + 2.523 * S3
       CE  =  84.57 - 108.8 * S + 31.52 * S2 
       DE  =  7.469 + 2.480 * S - 0.866 * S2 
       DEL = FV (X, NE, AKE, BKE, AE, BE, CE, DE)
*...UDB :
       ALX =  1.215
       BEX =  0.466
       AKX =  0.326 + 0.150 * S 
       BKX =  0.956 + 0.405 * S
       AGX =  0.272
       BGX =  3.794 - 2.359 * DS
       CX  =  2.014
       DX  =  7.941 + 0.534 * DS - 0.940 * S + 0.410 * S2
       EX  =  3.049 + 1.597 * S
       ESX =  4.396 - 4.594 * DS + 3.268 * S
       UDB = FW (X, S, ALX, BEX, AKX, BKX, AGX, BGX, CX, DX, EX, ESX)
*...SB :
       ALS =  0.175
       BES =  0.344
       AKS =  1.415 - 0.641 * DS 
       AS  =  0.580 - 9.763 * DS + 6.795 * S  - 0.558 * S2
       BS  =  5.617 + 5.709 * DS - 3.972 * S
       DST =  13.78 - 9.581 * S  + 5.370 * S2 - 0.996 * S3
       EST =  4.546 + 0.372 * S2
       ESS =  5.053 - 1.070 * S  + 0.805 * S2
       SB  = FWS (X, S, ALS, BES, AKS, AS, BS, DST, EST, ESS)
*...GL :
       ALG =  1.258
       BEG =  1.846
       AKG =  2.423 
       BKG =  2.427 + 1.311 * S  - 0.153 * S2
       AG  =  25.09 - 7.935 * S
       BG  = -14.84 - 124.3 * DS + 72.18 * S
       CG  =  590.3 - 173.8 * S
       DG  =  5.196 + 1.857 * S
       EG  = -1.648 + 3.988 * S  - 0.432 * S2
       ESG =  3.232 - 0.542 * S 
       GL  = FW (X, S, ALG, BEG, AKG, BKG, AG, BG, CG, DG, EG, ESG)
       RETURN
       END
*
*...FUNCTIONAL FORMS OF THE PARAMETRIZATIONS :
*
       FUNCTION FV (X, N, AK, BK, A, B, C, D)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = DSQRT (X)
       FV = N * X**AK * (1.+ A*X**BK + X * (B + C*DX)) * (1.- X)**D
       RETURN
       END
*
       FUNCTION FW (X, S, AL, BE, AK, BK, A, B, C, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       LX = DLOG (1./X)
       FW = (X**AK * (A + X * (B + X*C)) * LX**BK + S**AL
     1      * DEXP (-E + DSQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END
*
       FUNCTION FWS (X, S, AL, BE, AK, AG, B, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = DSQRT (X)
       LX = DLOG (1./X)
       FWS = S**AL / LX**AK * (1.+ AG*DX + B*X) * (1.- X)**D
     1       * DEXP (-E + DSQRT (ES * S**BE * LX))
       RETURN
       END


c********************************************************************
*********************************************************************
*                                                                   *
*    POLARIZED RADIATIVELY GENERATED LO AND NLO PARTON DENSITIES    *
*                                                                   *
*         M. GLUCK, E. REYA, M. STRATMANN AND W. VOGELSANG,         *
*                   DO-TH 95/13, RAL-TR-95-042                      *
*                  (TO APPEAR IN PHYS. REV. D)                      *       
*          PROBLEMS/QUESTIONS TO VOGELSANG@V2.RL.AC.UK              *
*                 OR  TO STRAT@HAL1.PHYSIK.UNI-DORTMUND.DE          *
*                                                                   *
*   INPUT:   ISET = number of the parton set :                      *
*              ISET = 1  'STANDARD' SCENARIO, NEXT-TO-LEADING ORDER *
*                        (DATA FILE 'STDNLO.GRID' UNIT=11, TO BE    *
*                         DEFINED BY THE USER )                     *
*              ISET = 2  'VALENCE' SCENARIO,  NEXT-TO-LEADING ORDER *
*                        (DATA FILE 'VALNLO.GRID' UNIT=22, TO BE    *
*                         DEFINED BY THE USER )                     *
*              ISET = 3  'STANDARD' SCENARIO, LEADING ORDER         *
*                        (DATA FILE 'STDLO.GRID' UNIT=33, TO BE     *
*                         DEFINED BY THE USER )                     *
*              ISET = 4  'VALENCE' SCENARIO,  LEADING ORDER         *
*                        (DATA FILE 'VALLO.GRID' UNIT=44, TO BE     *
*                         DEFINED BY THE USER )                     *
*                                                                   *
*            X  = Bjorken-x       (between  1.E-4  and  1)          *
*            Q2 = scale in GeV**2 (between  0.4  and   1.E4)        *
*             (for values outside the allowed range the program     *
*              writes a warning and extrapolates to the x and       *
*              Q2 values requested)                                 *
*                                                                   *
*   OUTPUT:  UV = x * ( DELTA u - DELTA u(bar) ),                   *
*            DV = x * ( DELTA d - DELTA d(bar) ),                   *        
*            QB = x * POLARIZED LIGHT SEA,                          *   
*                 QB= x * ( (DELTA U(BAR) + DELTA D(BAR))/2 )       * 
*            ST = x * DELTA STRANGE = x * DELTA STRANGE(BAR)        *     
*            GL = x * DELTA GLUON                                   *
*                                                                   *
*            always x times the distribution is returned            *
*                                                                   *
*            The sets are the result of a combined fit to           *
*            data for the spin asymmetries A_1 (p,n,d)              *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINI / IINI , and  IINI     *
*            has always to be zero when PARPOL is called for the    *
*            first time or when 'ISET' has been changed.            *
*                                                                   *
*********************************************************************
*
      SUBROUTINE PARPOL (ISET, X, Q2, UV, DV, QB, ST, GL)

      IMPLICIT NONE
      
      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      real*8 xb1,dfint,xb0,q2,uv,x,st,gl,dv,qb,xb,qs,
     &       parton,arrf,xt,xqbf,xuvf,xdvf,xgf,xsf
      integer n,iq,m,ix,iini,iset,npart,nx,nq,narg
     &        ,na
cywu<<
      PARAMETER (NPART=5, NX=42, NQ=21, NARG=2)
      DIMENSION XUVF(NX,NQ), XDVF(NX,NQ), XQBF(NX,NQ), 
     1          XSF(NX,NQ), XGF(NX,NQ), PARTON (NPART,NQ,NX-1), 
     2          QS(NQ), XB(NX), XT(NARG), NA(NARG), ARRF(NX+NQ) 
      COMMON / INTINI / IINI
      SAVE XUVF, XDVF, XQBF, XSF, XGF, NA, ARRF
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.4D0, 0.75D0, 1.0D0, 1.5D0, 2.5D0, 
     1           4.0D0, 6.4D0, 1.0D1, 1.5D1, 2.5D1, 4.0D1, 6.4D1,
     2           1.0D2, 1.8D2, 3.2D2, 5.8D2, 1.0D3, 1.8D3,
     3           3.2D3, 5.8D3, 1.0D4 /
       DATA XB / 1.D-4, 1.5D-4, 2.2D-4, 3.2D-4, 4.8D-4, 7.D-4,
     2           1.D-3, 1.5D-3, 2.2D-3, 3.2D-3, 4.8D-3, 7.D-3,
     3           1.D-2, 1.5D-2, 2.2D-2, 3.2D-2, 5.0D-2, 7.5D-2,
     4           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     5           0.3, 0.325, 0.35, 0.375, 0.4, 0.45,  0.5, 0.55,
     6           0.6, 0.65,  0.7,  0.75,  0.8, 0.85,  0.9, 1.0 /
*...CHECK OF X AND Q2 VALUES : 
       IF ( (X.LT.1.0D-4) .OR. (X.GT.1.0D0) ) THEN
           WRITE(6,91) 
  91       FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
C          GOTO 60
       ENDIF
       IF ( (Q2.LT.0.4D0) .OR. (Q2.GT.1.D4) ) THEN
cerwin           WRITE(6,92) 
  92       FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
cerwin           write(6,*)'q2=',q2
C          GOTO 60
       ENDIF
*...INITIALIZATION :
*    SELECTION AND READING OF THE GRID :
*    FILE - NO. = 11 FOR NLO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.040E-03 )     
*    FILE - NO. = 22 FOR NLO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 9.740E-04 )     
*    FILE - NO. = 33 FOR  LO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.731E-03 )     
*    FILE - NO. = 44 FOR  LO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.846E-03 )     
      IF (IINI.NE.0) GOTO 16
      OPEN(IPLST(3),FILE=CPOL,STATUS='OLD')
C
      DO 15 M = 1, NX-1 
         DO 15 N = 1, NQ
       READ(IPLST(3),90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1                 PARTON(4,N,M), PARTON(5,N,M)
 15   CONTINUE
      CLOSE(IPLST(3))
 90   FORMAT (5(1PE10.3))
C
      IINI = 1
*.... ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XB0 = XB(IX) 
        XB1 = 1.D0-XB(IX)
        XUVF(IX,IQ) = PARTON(1,IQ,IX) / (XB1**3 * XB0**0.7)
        XDVF(IX,IQ) = PARTON(2,IQ,IX) / (XB1**4 * XB0**0.6)
        XQBF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**7 * XB0**0.3) 
        XSF(IX,IQ)  = PARTON(4,IQ,IX) / (XB1**7 * XB0**0.3) 
        XGF(IX,IQ)  = PARTON(5,IQ,IX) / (XB1**10 * XB0**0.3)
  20  CONTINUE
        XUVF(NX,IQ) = 0.D0
        XDVF(NX,IQ) = 0.D0
        XQBF(NX,IQ) = 0.D0
        XSF(NX,IQ)  = 0.D0
        XGF(NX,IQ)  = 0.D0
  10  CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
  16  CONTINUE
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      UV = DFINT(NARG,XT,NA,ARRF,XUVF) * (1.D0-X)**3 * X**0.7
      DV = DFINT(NARG,XT,NA,ARRF,XDVF) * (1.D0-X)**4 * X**0.6 
      QB = DFINT(NARG,XT,NA,ARRF,XQBF) * (1.D0-X)**7 * X**0.3
      ST = DFINT(NARG,XT,NA,ARRF,XSF)  * (1.D0-X)**7 * X**0.3
      GL = DFINT(NARG,XT,NA,ARRF,XGF)  * (1.D0-X)**10 * X**0.3
 60   RETURN
      END
*72*********************************************************************
*     
*...CERN LIBRARY ROUTINE E104 (INTERPOLATION) :
*
      FUNCTION DFINT(NARG,ARG,NENT,ENT,TABLE)
c     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
cerw
c     IMPLICIT INTEGER (I-N)
cerw
cywu>>
      DOUBLE PRECISION fac,d,ent,arg,dfint,table
      integer iadr,ifadr,j,jr,ncomb,ient,il,k,jb,nent,narg,
     &        ja,i,m,kd
cywu<<
  
      DIMENSION ARG(5),NENT(5),ENT(63),TABLE(882)
      DIMENSION D(5),NCOMB(5),IENT(5)
      KD=1
      M=1
      JA=1
         DO 5 I=1,NARG
      NCOMB(I)=1
      JB=JA-1+NENT(I)
         DO 2 J=JA,JB
      IF (ARG(I).LE.ENT(J)) GO TO 3
    2 CONTINUE
      J=JB
    3 IF (J.NE.JA) GO TO 4
      J=J+1
    4 JR=J-1
      D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
      IENT(I)=J-JA
      KD=KD+IENT(I)*M
      M=M*NENT(I)
    5 JA=JB+1
      DFINT=0.D0
   10 FAC=1.D0
      IADR=KD
      IFADR=1
         DO 15 I=1,NARG
      IF (NCOMB(I).EQ.0) GO TO 12
      FAC=FAC*(1.D0-D(I))
      GO TO 15
   12 FAC=FAC*D(I)
      IADR=IADR-IFADR
   15 IFADR=IFADR*NENT(I)
      DFINT=DFINT+FAC*TABLE(IADR)
      IL=NARG
   40 IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
         DO 50  K=IL,NARG
   50 NCOMB(K)=1
      GO TO 10
   80 IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C									     C
C       POLARIZED PARTON DISTRIBUTIONS (NLO and LO)			     C
C       FROM CERN-TH/71-97 AND HEP-PH 9711440                                C
C	D. de Florian, R. Sassot and O. Sampayo                              C
C 									     C
C	MODE: 1 SET1 NLO (MSbar)					     C
C             2 SET2 NLO (MSbar)    					     C
C             3 SET3 NLO (MSbar) 					     C
C             4 SET1 LO              					     C
C             5 SET2 LO 						     C
C             6 SET3 LO							     C
C									     C
C                    Q2=Q^2						     C
C                    DUV :    X * U VALENCE DISTRIBUTION		     C
C                    DDV :    X * D VALENCE DISTRIBUTION 		     C
C                    DQBAR :  X * UBAR= X * DBAR DISTRIBUTION   	     C
C                    DSTR :   X * STR= X * STRBAR DISTRIBUTION		     C
C                    DGLU :   X * GLUON DISTRIBUTION 			     C
C                    G1P :    X * POLARIZED STRUCTURE FUNCTION (PROTON)	     C
C                    G1N :    X * POLARIZED STRUCTURE FUNCTION (NEUTRON)     C
C                    G1D :    X * POLARIZED STRUCTURE FUNCTION(DEUTERON)     C
C									     C
C       REMEMBER: ALWAYS X*DISTRIBUTION !!!				     C
C 	BEFORE CALLING THE SUBRUTINE `POLFIT` FOR THE FIRST TIME, THE        C
C       SUBROUTINE `INI` MUST BE CALLED (ONLY ONCE) TO READ THE GRIDS.       C
C              (CALL INI)						     C
C									     C
C	RANGE OF VALIDITY OF THE INTERPOLATION:  			     C
C					10**(-4)< X < 0.9		     C
C					1 < Q**2 < 5*10**4                   C
C									     C
C       IN CASE OF PROBLEMS, DOUBTS, ETC, PLEASE REPORT TO		     C
C        Daniel.de.Florian@cern.ch					     C
C									     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE POLFIT(MODE,X,Q2,DUV,DDV,DQBAR,DSTR,DGLU)
************************      
*HI    Speed optimized *
************************      
       IMPLICIT NONE

      COMMON/ XARRAY / XARRAY
      COMMON/ GRID / GU,GD,GL,GS,GG,GP,GN
      DOUBLE PRECISION XARRAY(102), GU(76,26), GD(76,26), GL(76,26),
     &     GS(76,26), GG(76,26), GP(76,26), GN(76,26)

      DOUBLE PRECISION XQ(2),PERINOLA,X3,X4,X5,X6,X7,X,Q2
      DOUBLE PRECISION DUV,DDV,DQBAR,DSTR,DGLU
      INTEGER MODE
 		
      XQ(1) = X
      XQ(2) = Q2
      X3=(1.D0-XQ(1))**3.D0
      X4=(1.D0-XQ(1))**4.D0
      X5=XQ(1)**0.5D0
      X6=XQ(1)**0.6D0
      X7=XQ(1)**0.7D0
      DUV = PERINOLA(XQ,GU) * X3* X6
      DDV = PERINOLA(XQ,GD) * X4 * X7 
      DQBAR = PERINOLA(XQ,GL) * X3 * X5
      DSTR = PERINOLA(XQ,GS)  * X3 * X5
      DGLU = PERINOLA(XQ,GG)  * X3 * X5
*HI      G1P = PERINOLA(XQ,GP)  * X3 * X5
*HI      G1N = PERINOLA(XQ,GN)  * X4 * X5
*HI      G1D=(G1P+G1N)*0.5D0*(1.D0-1.5D0*0.058D0)
      RETURN
      END
*72*********************************************************************
      DOUBLE PRECISION FUNCTION PERINOLA(ARG,TABLE)
       IMPLICIT NONE

      
      COMMON/ XARRAY / ENT
      DOUBLE PRECISION ENT(102)
      
      INTEGER NENT(2),NCOMB(7),IENT(7),NARG,KD,M,JA,I,JB,J,JR,IADR,IFADR
      INTEGER IL,K
      DOUBLE PRECISION ARG(2),TABLE(*),D(7),FAC

      NARG=2
      NENT(1)=76
      NENT(2)=26
      KD=1
      M=1
      JA=1
      DO 5 I=1,NARG
         NCOMB(I)=1
         JB=JA-1+NENT(I)
         DO 2 J=JA,JB
            IF (ARG(I).LE.ENT(J)) GO TO 3
 2       CONTINUE
         J=JB
 3       IF (J.NE.JA) GO TO 4
         J=J+1
 4       JR=J-1
         D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
         IENT(I)=J-JA
         KD=KD+IENT(I)*M
         M=M*NENT(I)
 5    JA=JB+1
      PERINOLA=0.D0
 10   FAC=1.D0
      IADR=KD
      IFADR=1
      DO 15 I=1,NARG
         IF (NCOMB(I).EQ.0) GO TO 12
         FAC=FAC*(1.D0-D(I))
         GO TO 15
 12      FAC=FAC*D(I)
         IADR=IADR-IFADR
 15   IFADR=IFADR*NENT(I)
      PERINOLA=PERINOLA+FAC*TABLE(IADR)
      IL=NARG
 40   IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
      DO 50  K=IL,NARG
 50   NCOMB(K)=1
      GO TO 10
 80   IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END
*72*********************************************************************
      SUBROUTINE INIDEFLO
************************      
*HI    Speed optimized *
************************      
       IMPLICIT NONE

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      


      INTEGER K,J,LX1,LQ
      DOUBLE PRECISION XX,QQ
      COMMON/ XARRAY / XARRAY
      COMMON/ GRID / GU,GD,GL,GS,GG,GP,GN
      DOUBLE PRECISION XARRAY(102), GU(76,26), GD(76,26), GL(76,26),
     &              GS(76,26), GG(76,26), GP(76,26), GN(76,26) 

      OPEN(IPLST(3),FILE=CPOL,STATUS='OLD')
      DO  K = 1, 76 
         DO  J = 1, 26
            READ(IPLST(3),40)GU(K,J),GD(K,J),GL(K,J),GS(K,J), 
     &                 GG(K,J),GP(K,J),GN(K,J)
 	END DO
      END DO
      CLOSE(IPLST(3))
 40   FORMAT (7(1PE15.7))

      DO LX1=0,25
         XX=10.D0**(-4.D0+LX1/25.D0*3.D0)
         XARRAY(LX1+1) = DLOG(XX)
      ENDDO
      DO LX1=1,50
         XX=0.1D0+LX1/50.D0*0.9D0
         XARRAY(LX1+26) = DLOG(XX)
      ENDDO
      DO LQ=0,25
         QQ=0.6D0*10**(5.D0*LQ/25.D0)
         XARRAY(LQ+77) = DLOG(QQ)
      ENDDO
 
      RETURN
      END 

*72*********************************************************************
      SUBROUTINE FITPARTON(X,Q2,XPQ,XDPQ)
      IMPLICIT NONE
      REAL X,Q2,XPQ(-6:6),XDPQ(-6:6)

      WRITE(*,*) 'FITPARTON : This is a user function'
      WRITE(*,*) 'Program will stop'
      STOP
      
      RETURN
      END

*72********************************************************************* 
      SUBROUTINE PARTON(X,Q2,XPQ,XDPQ)
      IMPLICIT NONE
C *********************************************************************  
C This subroutine gives the parton distributions (polarized 
C and unpolarized) for the polarized leptoproduction. The 
C parton distributions are optimized for polarized scattering.
C The following sets are included
C
C       LST(15) = 101  Schaefer, Phys. Lett. B 208,2 (1988) 175
C                      for comparison with older PEPSI versions
C       LST(15) = 102  free
C       LST(15) = 103  free
C       LST(15) = 104  Schaefer  et al hep-ph/9505306
C                      using Glueck et al. Z. Phys. C 53 (1992) 127
C       LST(15) = 105  Bartelski et al hep-ph/9502271 Set II(p,n)
C       LST(15) = 106  Bartelski et al hep-ph/9502271 Set II(P,n)
C       LST(15) = 107  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP  
C       LST(15) = 108  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C       LST(15) = 109  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C       LST(15) = 110  Gehrmann  et al hep-ph/9512406 Gluon A (LO)
C       LST(15) = 111  Gehrmann  et al hep-ph/9512406 Gluon B (LO)
C       LST(15) = 112  Gehrmann  et al hep-ph/9512406 Gluon C (LO)
C       LST(15) = 113  Gehrmann  et al hep-ph/9512406 Gluon A (LO) + (DGLAP)
C       LST(15) = 114  Gehrmann  et al hep-ph/9512406 Gluon B (LO) + (DGLAP)
C       LST(15) = 115  Gehrmann  et al hep-ph/9512406 Gluon C (LO) + (DGLAP)
C...............................................................................      
C       M. Glueck, E. Reya, M. Stratmann and W. Vogelsang,
C       DO-TH 95/13, RAL-TR-95-042                
C       LST(15) = 116  'standard' scenario, next-to-leading order
C       LST(15) = 117  'valence'  scenario, next-to-leading order 
C       LST(15) = 118  'standard' scenario, leading order
C       LST(15) = 119  'valence'  scenario, leading order
C       LST(15) = 120  Stanley J.Brodsky Nucl.Phys. B441(1995)
C       LST(15) = 121  S.Keler & J.F.Owens  Phys.Lett. B266(1991)
C                                         & Phys.Rev. D19(1994)1199
C       LST(15) = 124  D. de Florian et al., hep-ph/9711440  LO set 1      
C       LST(15) = 125  D. de Florian et al., hep-ph/9711440  LO set 2      
C       LST(15) = 126  D. de Florian et al., hep-ph/9711440  LO set 3      
C       LST(15) = 127  D. de Florian et al., hep-ph/9711440 NLO set 1      
C       LST(15) = 128  D. de Florian et al., hep-ph/9711440 NLO set 2      
C       LST(15) = 129  D. de Florian et al., hep-ph/9711440 NLO set 3      
C       LST(15) = 130  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta u(x) = 0.5 * u(x) and Delta d(x) = 0.
C       LST(15) = 131  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta d(x) = 0.5 * d(x) and Delta u(x) = 0.
C       LST(15) = 132  fit routine. (Is outside the official code.)
C
C   CTEQ4 collaboration: http: 
C       UNPOL: Low Q2 parametrization is the only one used here
C       POL:  BOGUS, du=0.5* u(x) dd=-0.3*d(x)  0.0 all else
C       LST(15) = 133  CTEQ4LQ  
C
C   MRS low Q2
C       UNPOL MRS 95 low Q2
C       POL:  BOGUS, du=0.5* u(x) dd=-0.3*d(x)  0.0 all else
C       LST(15)= 137 MRS low Q2
C       LST(15) = 144   grsv2000 hep-ph/0011215  LO standard scenario     
C       LST(15) = 145   grsv2000 hep-ph/0011215  LO valence scenario     
C       LST(15) = 146   grsv2000 hep-ph/0011215 NLO standard scenario     
C       LST(15) = 147   grsv2000 hep-ph/0011215 NLO valence scenario     
C..............................................................................
C       unpolarized distributions
C
C        LST(15) = 150   cteq5l                   LO 
C        LST(15) = 151   cteq5m                   NLO MSBAR
C        LST(15) = 152   cteq5m1                  NLO MSBAR (update)
C
C        LST(15) = 161  mrs99   cor01  central gluon, a_s    
C        LST(15) = 162  mrs99   cor02  higher gluon        
C        LST(15) = 163  mrs99   cor03  lower gluon       
C        LST(15) = 164  mrs99   cor04  lower a_s      
C        LST(15) = 165  mrs99   cor05  higher a_s     
C        LST(15) = 166  mrs99   cor06  quarks up        
C        LST(15) = 167  mrs99   cor07  quarks down            
C        LST(15) = 168  mrs99   cor08  strange up           
C        LST(15) = 169  mrs99   cor09  strange down          
C        LST(15) = 170  mrs99   cor10  charm up         
C        LST(15) = 171  mrs99   cor11  charm down       
C        LST(15) = 172  mrs99   cor12  larger d/u          
C
C        LST(15) = 173  cteq6l LO
C        LST(15) = 174  cteq6d DIS NLO
C        LST(15) = 175  cteq6m NLO MSBAR              
***********************************************************************

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LEPTO2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                XLP,YLP,W2LP,Q2LP,ULP
      REAL CUT,PARL,XLP,YLP,W2LP,Q2LP,ULP
      INTEGER LST
      SAVE /LEPTOU/

      INTEGER IMXPDF
      PARAMETER (IMXPDF=40)
      COMMON /PEPADM/CPDFNAM(2,IMXPDF),IPDFNAM(2,IMXPDF),
     &       IPLST(10),CUNPOL,CPOL
      CHARACTER*256 CPDFNAM,CUNPOL,CPOL
      INTEGER IPLST,IPDFNAM
      SAVE /PEPADM/

**************************************************************
*      
*   IPLST(1) =  0 (default) : number of PDF warnings
*   IPLST(2) = 11 (default) : unit -1- for pdf files
*   IPLST(3) = 12 (default) : unit -2- for pdf files      
*
**************************************************************      

      
      REAL X,Q2,XPQ(-6:6),XDPQ(-6:6),ULMASS
      DOUBLE PRECISION DX,DQ2      
      REAL XUV, XDV, XS, XG  
      REAL XUB, XDB, XC, XBOT
      REAL XDUV,XDDV,XDS,XDG  
      REAL XDUB,XDDB,XDC

C...Variables for set 101
      REAL VALPHAU,VBETAU,GAMMA
      REAL VALPHAD,VBETAD
      REAL VGAMMA,VC3
      REAL VAU0,VAU1,VAD0,VAD1
      REAL FFU0,FFU1,FFD0,FFD1,FG1P,FG1N

C.. Variables for set 104
      REAL VS,VN,VKA,VSW
      REAL VALPHA,VBETA  
      REAL VKLEINA,VKLEINB
      REAL VA,VB,VC,VD,VE,VF
      REAL VA0,VA10
      REAL MNUCL,WWW2

      REAL XD,XU,XDD,XDU,XDDS,XDUS
      
C...Variables for sets 107-109
      DOUBLE PRECISION MRSUPV,MRSDNV,MRSDSEA,MRSUSEA
      DOUBLE PRECISION MRSSTR,MRSCHM,MRSBOT,MRSGLU
      INTEGER MODE

C...Variables for sets 122-129
      DOUBLE PRECISION DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL,
     &       DXDUV,DXDDV,DXDQBAR,DXDSTR,DXDGLU,
     &       DDXUS,DDXDS
C...Variables for sets 105 and 106
      REAL FDELTA,FM
      REAL VA1,VA2,VA3
      REAL VC1,VC2   

C...Variables for set 110-112
      REAL VETAU ,VETAD, VETAQB, VETAG
      REAL VNORMU,VNORMD,VNORMQB,VNORMG
      REAL VAU,   VAD,   VAQB,   VAG
      REAL VBU,   VBD,   VBQB,   VBG
      REAL VGU,   VGD,   VGQB,   VGG
      REAL VRU,   VRD,   VRQB,   VRG

C...Variables for set 113-115
      INTEGER IFLAG
      DOUBLE PRECISION DXDG,DXDUBAR,DXDDBAR,DXDS
      REAL   XUPDB,XDMUB   

      REAL F2N,F2P
      
C...Variables for sets 133-36
      DOUBLE PRECISION DDXPQ(-6:6)
      INTEGER INITCTEQ,CTEQSET
      DOUBLE PRECISION CTQ4PDF
      
C...Variables for set 116-119
      INTEGER ISET

   
C...Variables for set 120-121
      REAL Y,X1,SS,Z
      REAL AJ(4),BJ(4),CJ(4),RJ(4),W(3),XW(3),WW(3),AAA(6,4)
      REAL CONST0(6,4),CONST1(6,4),CONST2(6,4),CONST3(6,4),
     &     SUMR(2),QG(4),QJ(4)
      REAL AQ(4),BQ(4),CQ(4),DQ(4),ST1(4),ST2(4),
     &     QP(4),QM(4),ST3(4),ST4(4)
      DATA XW/-1.775E-02, -1.E-02, -2.2540E-03/,W/5.556E-03,
     &     8.889E-03, 5.556E-03/,WW/0.3872983, 3.1329002E-02,
     &     4.849505/,AJ/ -0.2, -0.2, -0.3, -0.4/,BJ/ 4.7, 3.7,
     &     8., 8./,CJ/ 0., 2.54, 0., 0./,RJ/-0.9582284, 2.068491,
     &     -0.3953224, 14.54266/,AQ/0.757, 3.784, 0.2897, 2./,
     &     BQ/-0.645, -3.672, -0.2637, -1.25/,CQ/3.23, 2.004,
     &     1., 2./,DQ/-3.118, -1.892, -0.9748, -1.25/,ST1/3., 3.,
     &     5., 4./,ST2/4., 4., 6., 5./,ST3/5., 5., 7., 6./,
     &     ST4/6., 6., 8., 7./      
      DATA CONST0/-0.335  , 3.614 , 0.8673 , 0.0 , 0.0 , 0.0  ,
     &     -0.1612 , 4.667 , 0.0    , 0.0 , 0.0 , 0.0  ,
     &     -1.0   , 7.278 , 0.0    , 0.0 , 0.0 , 0.909,
     &     -1.0   , 5.304 , 0.0    , 0.0 , 0.0 , 3.017/
      DATA CONST1/-0.1097 , .8395 ,-1.6637 , 1.1049, 0.0 , 0.0,
     &     -0.2092 , 0.7951,-1.0232 , 0.8616, 0.0 , 0.0,
     &     -0.3823,-0.7904,-1.6629,-0.0133,0.1211,-0.4023,
     &     -0.9342, 1.4654,-3.9141,9.0176,-5.9602,-4.7347/
      DATA CONST2/-0.002442,-0.02186,0.342,-0.2369,0.0,0.0,
     &     0.02657, 0.1081, 0.05799, 0.153,0.0,0.0,
     &     0.02766, 0.8108,0.5719,0.5299,-0.1739,0.0063,
     &     0.5454,  -1.4292,2.8445,-10.426,7.515,3.3594/
      DATA CONST3/ 0.0    , 0.0 , 0.0 , 0.0  , 0.0 , 0.0  ,
     &     0.0    , 0.0 , 0.0 , 0.0  , 0.0 , 0.0  ,
     &     0.0    , 0.0 , 0.0 , 0.0  , 0.0 , 0.0  ,
     &     -0.1668,0.7569,-0.8411,4.0983,-2.7329,-0.9443/
      DATA INITCTEQ/ 1 /
      DATA CTEQSET/10/
C.. Variables for helium amendment
      REAL PERCS,PERCSP,PERCD
      REAL ADMXS,ADMXSP,ADMXD
      REAL FACTS,FACTSP,FACTD
      REAL PARI11
C
C.. Variables for set 144-147
      DOUBLE PRECISION DXDD,DXDU,DUMMYG1P, DUMMYG1N
      DOUBLE PRECISION DXDUB,DXDDB

C.. Variables for set 150-152
      INTEGER IPARTON
      DOUBLE PRECISION CTQ5PDF

C.. Variable for set 173-175
C.. Use IPARTON from definition above 
      DOUBLE PRECISION CTQ6PDF

      INTEGER I,J,K,ICOUNT,ICOUN1,ICERR
      DATA ICOUNT /0/,ICOUN1/0/,ICERR/0/

      ICOUNT = ICOUNT + 1
      


C  Reset all partondensities
      DO I=-6,6
         XPQ(I)  = 0.
         XDPQ(I) = 0.
      ENDDO

C ***************************************************************************
C
C      set 101 Schaefer, Phys. Lett. B 208,2 (1988) 175
C
C ***************************************************************************


      IF(LST(15).EQ.101) THEN

C    unpolarized part  
            
         VALPHAU = 0.588
         VBETAU  = 2.69 
         XUV = GAMMA(VALPHAU)*GAMMA(VBETAU+1.)/GAMMA(VALPHAU+VBETAU+1.)
         XUV = 2./XUV*X**VALPHAU*(1.-X)**VBETAU
         
         VALPHAD = 1.03
         VBETAD  = 6.87
         XDV = GAMMA(VALPHAD)*GAMMA(VBETAD+1.)/GAMMA(VALPHAD+VBETAD+1.)
         XDV = 1./XDV*X**VALPHAD*(1.-X)**VBETAD
         
         VGAMMA = 14.6  
         VC3    = 0.0147
         XUB    = VC3*(1.+VGAMMA)*(1.-X)**VGAMMA
         
         XDB = XUB
         XS  = XUB
         XC  = 0.02*(1.-X)**19

         XPQ( 1)  = XDV + XDB
         XPQ(-1)  = XDB
         XPQ( 2)  = XUV + XUB
         XPQ(-2)  = XUB
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 
         XPQ( 4)  = XC 
         XPQ(-4)  = XC 

C     polarized part

         VAU0 = 0.12
         VAU1 = 0.3*VAU0
         VAD0 = 7.0*VAU0
         VAD1 = 7.0*VAU1
         VALPHAU = 0.588
         VALPHAD = 1.03 
         
         FFU0 = 1./(1.+ VAU0*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFU1 = 1./(1.+ VAU1*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFD0 = 1./(1.+ VAD0*X**(-VALPHAD)*(1.-X)*(1.-X))
         FFD1 = 1./(1.+ VAD1*X**(-VALPHAD)*(1.-X)*(1.-X))
         FG1P =  0.5*(4*XUV*FFU0/9.
     &        - XDV*(4*FFU0+4*FFU1-FFD0+2*FFD1)/27.)
         FG1N =  0.5*( XUV*FFD0/9.
     &        - XDV*(-4*FFU0+8*FFU1+FFD0+FFD1)/27.)
         
         XDUV = 6*(4*FG1P-FG1N)/5.

         VAU0 = 0.12
         VAU1 = 0.3*VAU0
         VAD0 = 7*VAU0  
         VAD1 = 7*VAU1  
         
         VALPHAU = 0.588
         VALPHAD = 1.03 
         
         FFU0 = 1./(1.+ VAU0*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFU1 = 1./(1.+ VAU1*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFD0 = 1./(1.+ VAD0*X**(-VALPHAD)*(1.-X)*(1.-X))
         FFD1 = 1./(1.+ VAD1*X**(-VALPHAD)*(1.-X)*(1.-X))
         FG1P =  0.5*(4*XUV*FFU0/9.
     &        - XDV*( 4*FFU0+4*FFU1-FFD0+2*FFD1)/27.)
         FG1N =  0.5*(XUV*FFD0/9.
     &        - XDV*(-4*FFU0+8*FFU1+FFD0+FFD1)/27.)

         XDDV = 6*(4*FG1N-FG1P)/5.
         XDUB  = 0.
         XDDB  = 0.
         
         XDPQ( 1) = XDDV + XDDB
         XDPQ(-1) = XDDB
         XDPQ( 2) = XDUV + XDUB
         XDPQ(-2) = XDUB

      ELSEIF(LST(15).EQ.104) THEN      
C ***************************************************************************
C 
C      set  104  Schaefer  et al hep-ph/9505306
C 
C ***************************************************************************

C     unpolarized part
         IF (((Q2.LT.0.25 ).OR.(Q2.GT.1.E+8)).OR.
     &        (( X.LT.1.E-5).OR.( X.GT. 1.  )))    THEN
            ICOUN1 = ICOUN1 + 1
            IF(ICOUN1.lt.10) THEN
               WRITE(*,*) 'WARNING: (Q^2,x) exceeds allowed range'
            ENDIF
            IF(LST(22).eq.0.or.LST(25).eq.0) THEN 
               XPQ(2) = 1.
            ENDIF
            RETURN
         ENDIF
        
         MNUCL = ULMASS(2212)
         WWW2  = MNUCL*MNUCL + Q2*(1./X-1.) 
         
         VS  = LOG(LOG(Q2/(0.232*0.232))/LOG(0.25/(0.232*0.232)))
         
         VN  =  0.579 + 0.283*VS + 0.047*VS*VS
         VKA =  0.523 - 0.015*VS
         VA  =  2.22  - 0.59*VS - 0.27*VS*VS
         VB  =  5.95  - 6.19*VS + 1.55*VS*VS
         VD  =  3.57  + 0.94*VS - 0.16*VS*VS
         XDV = VN*X**VKA*(1.+VA*SQRT(X)+ VB*X)*(1.-X)**VD
         
         VN  =  0.663 + 0.191*VS - 0.041*VS*VS + 0.031*VS*VS*VS   
         VKA =  0.326
         VA  = -1.97  + 6.74*VS - 1.96*VS*VS
         VB  = 24.4   -20.7*VS  + 4.08*VS*VS
         VD  = 2.86   + 0.7*VS  - 0.02*VS*VS
         XUV = VN*X**VKA*(1.+VA*SQRT(X)+ VB*X)*(1.-X)**VD - XDV
         
         VALPHA  =  1.396
         VBETA   =  1.331
         VKLEINA =  0.412 - 0.171*VS
         VKLEINB =  0.566 - 0.496*VS
         VA      =  0.363
         VB      = -1.196
         VC      =  1.029 + 1.785*VS - 0.459*VS*VS
         VD      =  4.696 + 2.109*VS
         VE      =  3.838 + 1.944*VS
         VF      =  2.845
         XUB = ( X**VKLEINA*(VA+VB*X+VC*X*X)*(LOG(1./X))**VKLEINB
     &        +VS**VALPHA*EXP(-VE+SQRT(VF*VS**VBETA*LOG(1./X))))  
     &        *(1.-X)**VD
         XDB = XUB
         
         VSW = 0.
         VALPHA  =  0.803
         VBETA   =  0.563
         VKLEINA =  2.082 -  0.577*VS
         VA      = -3.055 +  1.024*VS**0.67
         VB      = 27.4   - 20.0*VS**0.154 
         VD      = 6.22
         VE      = 4.33 + 1.408*VS
         VF      = 8.27 - 0.437*VS
         
         IF (VS.GT.VSW) THEN
            XS = (VS-VSW)**VALPHA/(LOG(1./X)**VKLEINA)
     &           *(1. + VA*SQRT(X) + VB*X)*(1.-X)**VD
     &           *EXP(-VE + SQRT(VF*VS**VBETA*LOG(1./X)))
         ELSE
            XS = 0
         ENDIF
         
         VSW = 0.888
         VALPHA  =  1.01
         VBETA   =  0.37
         VKLEINA =  0.  
         VA      =  0.  
         VB      = 4.24  - 0.804*VS
         VD      = 3.46  + 1.076*VS
         VE      = 4.61  + 1.49*VS 
         VF      = 2.555 + 1.961*VS
         
*MM   >>    if w > 2* m_c = 3GeV
         IF (WWW2.GT.9.AND.VS.GT.VSW) THEN
            XC = (VS-VSW)**VALPHA/(LOG(1./X)**VKLEINA)
     &           *(1. + VA*SQRT(X) + VB*X)*(1.-X)**VD
     &           *EXP(-VE + SQRT(VF*VS**VBETA*LOG(1./X)))
         ELSE
            XC =0.
         ENDIF
         
         VSW = 1.351
         VALPHA  =  1.00
         VBETA   =  0.51
         VKLEINA =  0.  
         VA      =  0.  
         VB      = 1.848
         VD      = 2.929  + 1.396*VS
         VE      = 4.71   + 1.514*VS
         VF      = 4.02   + 1.239*VS
         
*MM   >>   if w > 2*m_b = 10 GeV
         IF (WWW2.GT.100.AND.VS.GT.VSW) THEN
            XBOT = (VS-VSW)**VALPHA/(LOG(1./X)**VKLEINA)
     &           *(1. + VA*SQRT(X) + VB*X)*(1.-X)**VD
     &           *EXP(-VE + SQRT(VF*VS**VBETA*LOG(1./X)))
         ELSE
            XBOT =0.
         ENDIF
         
         VALPHA  =  0.558
         VBETA   =  1.218
         VKLEINA =  1.00  - 0.17*VS
         VKLEINB =  0.
         VA      =          4.879*VS - 1.383*VS*VS
         VB      =  25.92 - 28.97*VS + 5.596*VS*VS
         VC      = -25.69 + 23.68*VS - 1.975*VS*VS
         VD      =  2.537 + 1.718*VS + 0.353*VS*VS
         VE      =  0.595 + 2.138*VS
         VF      =  4.066
         XG = ( X**VKLEINA*(VA+VB*X+VC*X*X)
     &        +VS**VALPHA*EXP(-VE+SQRT(VF*VS**VBETA*LOG(1./X))))
     &        *(1-X)**VD
         
         XPQ( 0)  = XG
         XPQ( 1)  = XDV + XDB
         XPQ(-1)  = XDB
         XPQ( 2)  = XUV + XUB
         XPQ(-2)  = XUB
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 
         XPQ( 4)  = XC 
         XPQ(-4)  = XC 
         XPQ( 5)  = XBOT
         XPQ(-5)  = XBOT
         
C     polarized part
         
         VA0   =  0.225
         VA10  =  0.15 
         VALPHAU = 0.326
         VALPHAD = 0.505
         
         FFU0  = 1./(1.+VA0*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFU1  = 1./(1.+VA0*VA10*X**(-VALPHAU)*(1.-X)*(1.-X))
         FFD0  = 1./(1.+VA0*X**(-VALPHAD)*(1.-X)*(1.-X))
         FFD1  = 1./(1.+VA0*VA10*X**(-VALPHAD)*(1.-X)*(1.-X))
         
         XDUV  =  XUV*FFU0
     &        -XDV*(FFU0+FFU1)/3.
         XDDV  =  XDV*(FFD0-2*FFD1)/3.
         
         XDUB  = 0.
         XDDB  = 0.
         XDG  =  XUV*(1.-FFU0)/2.
     &        +XDV*(2*FFD1-FFD0+FFU1+FFU0-3.)/6.
         
         XDPQ( 0) = XDG
         XDPQ( 1) = XDDV + XDDB
         XDPQ(-1) = XDDB
         XDPQ( 2) = XDUV + XDUB
         XDPQ(-2) = XDUB
      
      ELSEIF((LST(15).EQ.105).OR.(LST(15).EQ.106)) THEN
C***************************************************************************
C     
C       set 105  Bartelski et al hep-ph/9502271 Set II(p,n)
C       set 106  Bartelski et al hep-ph/9502271 Set II(P,n)
C     
C***************************************************************************
 
         XUV =  1.996*X**(-0.462)*(1.-X)**3.96
     &        *(1. - 0.39*SQRT(X) + 5.13*X)*X
         XDV =  0.296*X**(-0.67)*(1.-X)**4.71
     &        *(1. + 5.03*SQRT(X) + 5.56*X)*X
         FDELTA =   0.099*X**(-0.462)*(1.-X)**9.27*(1.+25*X)
         FM     =   0.411*X**(-1.3  )*(1.-X)**9.27
     &        *(1. - 1.15*SQRT(X) + 15.6*X)  
         XUB  = 0.5*X*(0.392*FM - FDELTA)
         XDB  = 0.5*X*(0.392*FM + FDELTA)
         XS   = 0.5*X*0.196*FM
         XC   = 0.5*X*0.020*FM
         XBOT = 0.
         XG   = 0.775*X**(-1.3)*(1.-X)**5.3*(1.+5.2*X)*X
         
         XPQ( 0)  = XG
         XPQ( 1)  = XDV + XDB
         XPQ(-1)  = XDB
         XPQ( 2)  = XUV + XUB
         XPQ(-2)  = XUB
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 
         XPQ( 4)  = XC 
         XPQ(-4)  = XC 
      
C     polarized part
         
         IF (LST(15).EQ.105) THEN
            VA1 =  0.924
            VA2 = -3.237
            VA3 = 11.3  
            XDUV = X**(-0.462)*(1.-X)**3.96*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VA1 = -0.029
            VA2 = -0.163
            VA3 = -1.644
            XDDV = X**(-0.670)*(1.-X)**4.71*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VC1 = 0.
            VC2 = -0.993
            VD  = -0.015
            FDELTA =   X**(-0.462)*(1.-X)**9.27*VD*(1.+25.*X)
            FM     =   X**(-0.8  )*(1.-X)**9.27
     &           *(VC1 + VC2*SQRT(X))

            XDUB = 0.5*X*(0.392*FM - FDELTA)            
            XDDB = 0.5*X*(0.392*FM + FDELTA)
            XDS = 0.5*X*0.196*FM
            XDC = 0.5*X*0.020*FM
            
         ELSEIF(LST(15).EQ.106) THEN
            
            VA1 =  0.929
            VA2 = -3.005
            VA3 = 10.24
            XDUV = X**(-0.462)*(1.-X)**3.96*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VA1 = -0.066
            VA2 = -0.064
            VA3 = -1.644
            XDDV = X**(-0.670)*(1.-X)**4.71*(VA1+VA2*SQRT(X)+VA3*X)*X
            
            VC1 = 0.
            VC2 = -0.861
            VD  = -0.013
            FDELTA =   X**(-0.462)*(1.-X)**9.27*VD*(1.+25.*X)
            FM     =   X**(-0.8  )*(1.-X)**9.27
     &           *(VC1+ VC2*SQRT(X))
            XDUB = 0.5*X*(0.392*FM - FDELTA)
            XDDB = 0.5*X*(0.392*FM + FDELTA)
            XDS = 0.5*X*0.196*FM
            XDC = 0.5*X*0.020*FM
         ENDIF

         XDPQ( 1) = XDDV + XDDB
         XDPQ(-1) = XDDB
         XDPQ( 2) = XDUV + XDUB
         XDPQ(-2) = XDUB
         XDPQ( 3) = XDS 
         XDPQ(-3) = XDS 
         XDPQ( 4) = XDC 
         XDPQ(-4) = XDC  
      ELSEIF((LST(15).GE.107).AND.(LST(15).LE.109)) THEN
C ***************************************************************************
C 
C       LST(15) = 107  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP  
C       LST(15) = 108  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C       LST(15) = 109  Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP
C         
C ***************************************************************************
         
         IF (LST(15).EQ.107) IFLAG = 0
         IF (LST(15).EQ.108) IFLAG = 1
         IF (LST(15).EQ.109) IFLAG = 2
        
         MODE = 20       ! MSR(A prime) Parametrization


C  unpolarized part 
*     MRS(A prime)
        
         DX=DBLE(X)
         DQ2=DBLE(Q2)
         
         CALL MRSEB(DX,DQ2,MODE,MRSUPV,MRSDNV,MRSUSEA,
     &        MRSDSEA,MRSSTR,MRSCHM,MRSBOT,MRSGLU)

       
         XPQ( 0) = SNGL(MRSGLU)
         XPQ( 1) = SNGL(MRSDNV + MRSDSEA) 
         XPQ(-1) = SNGL(         MRSDSEA)
         XPQ( 2) = SNGL(MRSUPV + MRSUSEA)
         XPQ(-2) = SNGL(         MRSUSEA)
         XPQ( 3) = SNGL(MRSSTR)
         XPQ(-3) = SNGL(MRSSTR)
         XPQ( 4) = SNGL(MRSCHM)  
         XPQ(-4) = SNGL(MRSCHM)
         XPQ( 5) = SNGL(MRSBOT)   
         XPQ(-5) = SNGL(MRSBOT)
         
C     polarized part (evolved set of Gehrmann hep-ph/9512406 Gluon A,B,C (LO))
         CALL POLNLO(IFLAG,X,Q2,DXDUV,DXDDV,DXDG,DXDUBAR,DXDDBAR,DXDS)
         
         XDPQ( 0) = SNGL(DXDG)
         XDPQ( 1) = SNGL(DXDDV+DXDDBAR)
         XDPQ(-1) = SNGL(DXDDBAR)
         XDPQ( 2) = SNGL(DXDUV+DXDUBAR)
         XDPQ(-2) = SNGL(DXDUBAR)
         XDPQ( 3) = SNGL(DXDS) 
         XDPQ(-3) = SNGL(DXDS) 
         
C ***************************************************************************
C 
C       LST(15) = 110  Gehrmann  et al hep-ph/9512406 Gluon A (LO)
C       LST(15) = 111  Gehrmann  et al hep-ph/9512406 Gluon B (LO)
C       LST(15) = 112  Gehrmann  et al hep-ph/9512406 Gluon C (LO)
C
C       LST(15) = 130  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta u(x) = 0.5 * u(x) and Delta d(x) = 0.
C       LST(15) = 131  Fake sample :
C                      unpolarized  Gehrmann  et al hep-ph/9512406
C                      with Delta d(x) = 0.5 * d(x) and Delta u(x) = 0.
C
C ***************************************************************************



      ELSEIF((LST(15).GE.110).AND.(LST(15).LE.112) .or.
     &       (LST(15).GE.130).AND.(LST(15).LE.131) ) THEN

C      unpolarized part

         XUV = 3.221*X**0.564*(1.-X)**3.726
     &        *(1.-0.6889*X**0.200 + 2.254*X + 1.261*SQRT(X)*X)
         XDV = 0.507*X**0.376*(1.-X)**4.476
     &        *(1.+1.615 *X**0.553 + 3.651*X + 1.299*SQRT(X)*X)
         XUPDB = (X**0.158*(0.738 - 0.981*X + 1.063*X*X)
     &          * (-LOG(X))**0.037 
     &          + 0.00285*EXP(SQRT(-4.010*LOG(X))))*(1.-X)**6.356
         XDMUB = 0.1067 * X**0.4036 * (1.+ 0.4680*X**0.8763 + 
     &            X * 18.6760) * (1.- X)**8.6220
          XS = 0.0034*(-LOG(X))**(-1.15)
     &         *(1.-2.392*SQRT(X) + 7.094*X)*(1.-X)**6.166
     &         *EXP(SQRT(-6.719*LOG(X)))
          XG = (X**0.731*(5.110 - 1.204*X - 1.911*X*X)
     &        * (-LOG(X))**(-0.4718)
     &         + 0.0527*EXP(SQRT(-4.584*LOG(X))))*(1.-X)**5.566

         XPQ( 0)  = XG
         XPQ( 1)  = XDV + (XUPDB +  XDMUB)*0.5 
         XPQ(-1)  =       (XUPDB +  XDMUB)*0.5
         XPQ( 2)  = XUV + (XUPDB -  XDMUB)*0.5
         XPQ(-2)  =       (XUPDB -  XDMUB)*0.5
         XPQ( 3)  = XS 
         XPQ(-3)  = XS 

C  polarized part

C  Gluon A        
         IF(LST(15).EQ.110) THEN
            VETAU   =  0.823
            VETAD   = -0.303 
            VETAQB  = -0.0495 
            VETAG   =  1.9
            
            VAU     =  0.578
            VAD     =  0.666
            VAQB    =  0.520
            VAG     =  0.520
            
            VBU     =  3.73
            VBD     =  4.73
            VBQB    = 15.06 
            VBG     =  9.45
            
            VGU     =  9.38
            VGD     = 10.46
            VGQB    =  2.30
            VGG     =  0.0
            
            VRU     = -4.26 
            VRD     = -5.10  
            VRQB    = -2.00
            VRG     =  0.00
            
            VNORMU  =  (1.+ VGU*VAU/(VAU+VBU+1.))
     &           * GAMMA(VAU)*GAMMA(VBU+1.)/GAMMA(VAU+VBU+1.)
     &           + VRU*GAMMA(VAU+0.5)*GAMMA(VBU+1.)
     &           /GAMMA(VAU+VBU+1.5)
            VNORMD  =  (1.+ VGD*VAD/(VAD+VBD+1.))
     &           * GAMMA(VAD)*GAMMA(VBD+1.)/GAMMA(VAD+VBD+1.)
     &           + VRD*GAMMA(VAD+0.5)*GAMMA(VBD+1.)
     &           /GAMMA(VAD+VBD+1.5)
            VNORMQB  =  (1.+ VGQB*VAQB/(VAQB+VBQB+1.))
     &           * GAMMA(VAQB)*GAMMA(VBQB+1.)/GAMMA(VAQB+VBQB+1.)
     &           + VRQB*GAMMA(VAQB+0.5)*GAMMA(VBQB+1.)
     &           /GAMMA(VAQB+VBQB+1.5)
            VNORMG  =  (1.+ VGG*VAG/(VAG+VBG+1.))
     &           * GAMMA(VAG)*GAMMA(VBG+1.)/GAMMA(VAG+VBG+1.)
     &           + VRG*GAMMA(VAG+0.5)*GAMMA(VBG+1.)
     &           /GAMMA(VAG+VBG+1.5)
            VNORMU  = 1./VNORMU
            VNORMD  = 1./VNORMD
            VNORMQB = 1./VNORMQB
            VNORMG  = 1./VNORMG
            
            XDUV  = VETAU*VNORMU*X**VAU*(1.-X)**VBU*(1.+VGU*X
     &           + VRU*SQRT(X))
            XDDV  = VETAD*VNORMD*X**VAD*(1.-X)**VBD*(1.+VGD*X
     &           + VRD*SQRT(X))
            XDUB  = VETAQB*VNORMQB*X**VAQB*(1.-X)**VBQB*
     &           (1.+VGQB*X + VRQB*SQRT(X))       
            XDDB  = XDUB
            XDS   = XDUB
            XDG   = VETAG*VNORMG*X**VAG*(1.-X)**VBG*(1.+VGG*X
     &           + VRG*SQRT(X))

            XDPQ( 0) = XDG
            XDPQ( 1) = XDDV + XDDB
            XDPQ(-1) = XDDB
            XDPQ( 2) = XDUV + XDUB
            XDPQ(-2) = XDUB
            XDPQ( 3) = XDS 
            XDPQ(-3) = XDS 

C Gluon B
         ELSE IF(LST(15).EQ.111) THEN

            VETAU   =  0.823
            VETAD   = -0.303 
            VETAQB  = -0.0495 
            VETAG   =  1.9
            
            VAU     =  0.585
            VAD     =  0.662
            VAQB    =  0.524
            VAG     =  0.524
            
            VBU     =  3.73
            VBD     =  4.73
            VBQB    = 15.96 
            VBG     =  6.87
            
            VGU     =  9.31
            VGD     = 10.91
            VGQB    =  2.42
            VGG     =  1.
            
            VRU     = -4.28 
            VRD     = -5.09  
            VRQB    = -2.00
            VRG     = -2.00
           
            VNORMU  =  (1.+ VGU*VAU/(VAU+VBU+1.))
     &           * GAMMA(VAU)*GAMMA(VBU+1.)/GAMMA(VAU+VBU+1.)
     &             + VRU*GAMMA(VAU+0.5)*GAMMA(VBU+1.)
     &           /GAMMA(VAU+VBU+1.5)
            VNORMD  =  (1.+ VGD*VAD/(VAD+VBD+1.))
     &           * GAMMA(VAD)*GAMMA(VBD+1.)/GAMMA(VAD+VBD+1.)
     &           + VRD*GAMMA(VAD+0.5)*GAMMA(VBD+1.)
     &           /GAMMA(VAD+VBD+1.5)
            VNORMQB  =  (1.+ VGQB*VAQB/(VAQB+VBQB+1.))
     &           * GAMMA(VAQB)*GAMMA(VBQB+1.)/GAMMA(VAQB+VBQB+1.)
     &           + VRQB*GAMMA(VAQB+0.5)*GAMMA(VBQB+1.)
     &           /GAMMA(VAQB+VBQB+1.5)
            VNORMG  =  (1.+ VGG*VAG/(VAG+VBG+1.))
     &           * GAMMA(VAG)*GAMMA(VBG+1.)/GAMMA(VAG+VBG+1.)
     &           + VRG*GAMMA(VAG+0.5)*GAMMA(VBG+1.)
     &           /GAMMA(VAG+VBG+1.5)
            
            VNORMU  = 1./VNORMU
            VNORMD  = 1./VNORMD
            VNORMQB = 1./VNORMQB
            VNORMG  = 1./VNORMG
            
            XDUV  = VETAU*VNORMU*X**VAU*(1.-X)**VBU
     &           *(1.+VGU*X + VRU*SQRT(X))
            XDDV  = VETAD*VNORMD*X**VAD*(1.-X)**VBD
     &           *(1.+VGD*X + VRD*SQRT(X))
            XDUB  = VETAQB*VNORMQB*X**VAQB*(1.-X)**VBQB*
     &           (1.+VGQB*X + VRQB*SQRT(X))       
            XDDB  = XDUB
            XDS   = XDUB
            XDG   = VETAG*VNORMG*X**VAG*(1.-X)**VBG
     &           *(1.+VGG*X + VRG*SQRT(X))

            XDPQ( 0) = XDG
            XDPQ( 1) = XDDV + XDDB
            XDPQ(-1) = XDDB
            XDPQ( 2) = XDUV + XDUB
            XDPQ(-2) = XDUB
            XDPQ( 3) = XDS 
            XDPQ(-3) = XDS 

C Gluon C
         ELSEIF(LST(15).EQ.112) THEN

            VETAU   =  0. 823
            VETAD   = -0.303 
            VETAQB  = -0.0495 
            VETAG   =  1.9
            
            VAU     =  0.582
            VAD     =  0.660
            VAQB    =  0.456
            VAG     =  0.456
            
            VBU     =  3.73
            VBD     =  4.73
            VBQB    = 11.82 
            VBG     =  8.72
            
            VGU     =  9.50
            VGD     = 11.04
            VGQB    =  2.11
            VGG     =  0.0
            
            VRU     = -4.28 
            VRD     = -5.06  
            VRQB    = -1.95
            VRG     = -3.00

            VNORMU  =  (1.+ VGU*VAU/(VAU+VBU+1.))
     &           * GAMMA(VAU)*GAMMA(VBU+1.)/GAMMA(VAU+VBU+1.)
     &           + VRU*GAMMA(VAU+0.5)*GAMMA(VBU+1.)
     &           /GAMMA(VAU+VBU+1.5)
            VNORMD  =  (1.+ VGD*VAD/(VAD+VBD+1.))
     &           * GAMMA(VAD)*GAMMA(VBD+1.)/GAMMA(VAD+VBD+1.)
     &           + VRD*GAMMA(VAD+0.5)*GAMMA(VBD+1.)
     &           /GAMMA(VAD+VBD+1.5)
            VNORMQB  =  (1.+ VGQB*VAQB/(VAQB+VBQB+1.))
     &           * GAMMA(VAQB)*GAMMA(VBQB+1.)/GAMMA(VAQB+VBQB+1.)
     &           + VRQB*GAMMA(VAQB+0.5)*GAMMA(VBQB+1.)
     &           /GAMMA(VAQB+VBQB+1.5)            
            VNORMG  =  (1.+ VGG*VAG/(VAG+VBG+1.))
     &           * GAMMA(VAG)*GAMMA(VBG+1.)/GAMMA(VAG+VBG+1.)
     &           + VRG*GAMMA(VAG+0.5)*GAMMA(VBG+1.)
     &           /GAMMA(VAG+VBG+1.5)
            
            VNORMU  = 1./VNORMU
            VNORMD  = 1./VNORMD
            VNORMQB = 1./VNORMQB
            VNORMG  = 1./VNORMG
            
            XDUV  = VETAU*VNORMU*X**VAU*(1.-X)**VBU
     &           *(1.+VGU*X + VRU*SQRT(X))
            XDDV  = VETAD*VNORMD*X**VAD*(1.-X)**VBD
     &           *(1.+VGD*X + VRD*SQRT(X))
            XDUB  = VETAQB*VNORMQB*X**VAQB*(1.-X)**VBQB*
     &           (1.+VGQB*X + VRQB*SQRT(X))       
            XDDB  = XDUB
            XDS   = XDUB
            XDG   = VETAG*VNORMG*X**VAG*(1.-X)**VBG
     &           *(1.+VGG*X + VRG*SQRT(X))

            XDPQ( 0) = XDG
            XDPQ( 1) = XDDV + XDDB
            XDPQ(-1) = XDDB
            XDPQ( 2) = XDUV + XDUB
            XDPQ(-2) = XDUB
            XDPQ( 3) = XDS 
            XDPQ(-3) = XDS 

	ELSEIF(LST(15).EQ.130) THEN
C unpolarized  Gehrmann  et al hep-ph/9512406
C with Delta u(x) = 0.5 * u(x) and Delta d(x) = 0.
          XDPQ(2) = 0.5 * XPQ(2)

	ELSEIF(LST(15).EQ.131) THEN
C unpolarized  Gehrmann  et al hep-ph/9512406
C with Delta d(x) = 0.5 * d(x) and Delta u(x) = 0. 
          XDPQ(1) = 0.5 * XPQ(1)
            
         ENDIF

C***************************************************************************
C
C       LST(15) = 113  Gehrmann  et al hep-ph/9512406 Gluon A (LO) + (DGLAP)
C       LST(15) = 114  Gehrmann  et al hep-ph/9512406 Gluon B (LO) + (DGLAP)
C       LST(15) = 115  Gehrmann  et al hep-ph/9512406 Gluon C (LO) + (DGLAP)
C
C**************************************************************************

         ELSEIF ((LST(15).GE.113).AND.(LST(15).LE.115)) THEN 
 
         IF (LST(15).EQ.113) IFLAG = 0
         IF (LST(15).EQ.114) IFLAG = 1
         IF (LST(15).EQ.115) IFLAG = 2

C  unpolarized part 
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         CALL GRV94LO (DX,DQ2
     &          ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)
                
         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + (DDXUDB + DDXDEL)*0.5D0)
         XPQ(-1) = SNGL(         (DDXUDB + DDXDEL)*0.5D0)
         XPQ( 2) = SNGL(DDXUV  + (DDXUDB - DDXDEL)*0.5D0)
         XPQ(-2) = SNGL(         (DDXUDB - DDXDEL)*0.5D0)
         XPQ( 3) = SNGL(DDXSB) 
         XPQ(-3) = SNGL(DDXSB) 

C polarized part (evolved set of Gehrmann hep-ph/9512406 Gluon A,B,C (LO))
         CALL POLPAR(IFLAG,DX,DQ2
     &               ,DXDUV,DXDDV,DXDG,DXDQBAR,DXDS)

         XDPQ( 0) = SNGL(DXDG)
         XDPQ( 1) = SNGL(DXDDV + DXDQBAR)
         XDPQ(-1) = SNGL(DXDQBAR)
         XDPQ( 2) = SNGL(DXDUV + DXDQBAR)
         XDPQ(-2) = SNGL(DXDQBAR)
         XDPQ( 3) = SNGL(DXDS) 
         XDPQ(-3) = SNGL(DXDS) 


C ***************************************************************************
C
C       M. Glueck, E. Reya, M. Stratmann and W. Vogelsang,
C       DO-TH 95/13, RAL-TR-95-042
C
C           LST(15) = 116  'standard' scenario, next-to-leading order
C           LST(15) = 117  'valence'  scenario, next-to-leading order
C           LST(15) = 118  'standard' scenario, leading order
C           LST(15) = 119  'valence'  scenario, leading order
C
C ***************************************************************************

      ELSEIF ((LST(15).GE.116).AND.(LST(15).LE.119)) THEN
       IF (LST(15).EQ.116) ISET = 1
       IF (LST(15).EQ.117) ISET = 2
       IF (LST(15).EQ.118) ISET = 3
       IF (LST(15).EQ.119) ISET = 4

C  unpolarized part
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *
         

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         CALL GRV94LO (DX,DQ2
     &          ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)

         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + (DDXUDB + DDXDEL)*0.5D0)
         XPQ(-1) = SNGL(         (DDXUDB + DDXDEL)*0.5D0)
         XPQ( 2) = SNGL(DDXUV  + (DDXUDB - DDXDEL)*0.5D0)
         XPQ(-2) = SNGL(         (DDXUDB - DDXDEL)*0.5D0)
         XPQ( 3) = SNGL(DDXSB)
         XPQ(-3) = SNGL(DDXSB)

C    Polarisierter Teil

      CALL  PARPOL (ISET,DX,DQ2,
     &      DXDUV, DXDDV, DXDQBAR, DXDS, DXDG)

      XDPQ( 0) = SNGL(DXDG)
      XDPQ( 1) = SNGL(DXDDV) + SNGL(DXDQBAR)
      XDPQ(-1) = SNGL(DXDQBAR)
      XDPQ( 2) = SNGL(DXDUV) + SNGL(DXDQBAR)
      XDPQ(-2) = SNGL(DXDQBAR)
      XDPQ( 3) = SNGL(DXDS)
      XDPQ(-3) = SNGL(DXDS)

C*************************************************************************

      ELSEIF(LST(15).EQ.120)THEN

*     This is polarization distribution of Stanley J.Brodsky et al.
*     Nucl.Phys. B441(1995)
         Y=1.-X
         X1=X**(-1.12)
         DO I = 1,4
            QP(I) = X1*(AQ(I)*Y**ST1(I)+BQ(I)*Y**ST2(I))
            QM(I) = X1*(CQ(I)*Y**ST3(I)+DQ(I)*Y**ST4(I))
            QG(I) = QP(I)+QM(I)
            QJ(I) = QP(I)-QM(I)
         ENDDO
         DO I = 1,2
            QG(I) = QG(I)-2.*QG(3)
            QJ(I) = QJ(I)-2.*QJ(3)
         ENDDO
         DO I = 1,4
            XPQ(I-4+INT(I/4.)*3) = QG(3)
            XDPQ(I-4+INT(I/4.)*3) = QJ(3)
         ENDDO
         
         XPQ(0) = QG(4)
         XPQ(1) = QG(1)
         XPQ(2) = QG(2)
         XDPQ(0) = QJ(4)
         XDPQ(1) = QJ(1)
         XDPQ(2) = QJ(2)
         
      ELSEIF(LST(15).EQ.121)THEN

*     This is polarization distribution of S.Keler & J.F.Owens
*     Phys.Lett. B266(1991)126 and Phys.Rev. D19(1994)1199
         
         IF(Q2.LE.4.) Q2 = 4.
         Z=ALOG(ALOG(Q2/WW(2))/WW(3))
         
         DO I = 1,6
            DO J = 1,4
               AAA(I,J) = CONST0(I,J) + CONST1(I,J)*Z +
     &              CONST2(I,J)*Z*Z + CONST3(I,J)*Z*Z*Z
            ENDDO
         ENDDO
         DO I = 1,2
            SUMR(I) = 0.
            DO 1 J = 1,3
               DO 1 K = 1,50
                  X1=XW(J)+K/50.
                  SS=1.+AAA(3,I)*X1+AAA(4,I)*X1*X1
                  SUMR(I)=SUMR(I)+X1**AAA(1,I)*(1.-X1)*
     &                 *AAA(2,I)*SS*W(J)
 1             CONTINUE
               AAA(6,I)=(5.-2.*I)/SUMR(I)
            ENDDO
            DO I = 1,4
               SS = 1.+AAA(3,I)*X+AAA(4,I)*X*X+AAA(5,I)*X*X*X    
               QG(I) = X**AAA(1,I)*(1.-X)**AAA(2,I)*SS*AAA(6,I)
            ENDDO
         SS=QG(2)
         QG(3)=QG(3)/6.
         QG(2)=QG(1)-SS
         QG(1)=SS
         DO I = 1,4
            SS = 1. + CJ(i)*X
            QJ(I) = X**AJ(I)*(1.-X)**BJ(I)*SS*RJ(I)
         ENDDO
         
         DO I = 1,4
            XPQ(I-4+INT(I/4.)*3) = QG(3)
            XDPQ(I-4+INT(I/4.)*3) = QJ(3)
         ENDDO         
         XPQ(0) = QG(4)
         XPQ(1) = QG(1)
         XPQ(2) = QG(2)
         XDPQ(0) = QJ(4)
         XDPQ(1) = QJ(1)
         XDPQ(2) = QJ(2)
C***********************************************************************
C       LST(15) = 124  D. de Florian et al., hep-ph/9711440  LO set 1      
C       LST(15) = 125  D. de Florian et al., hep-ph/9711440  LO set 2      
C       LST(15) = 126  D. de Florian et al., hep-ph/9711440  LO set 3      
C       LST(15) = 127  D. de Florian et al., hep-ph/9711440 NLO set 1      
C       LST(15) = 128  D. de Florian et al., hep-ph/9711440 NLO set 2      
C       LST(15) = 129  D. de Florian et al., hep-ph/9711440 NLO set 3      
C
C       using GRV 1995 as unpolarized reference set
C
C***********************************************************************
      ELSEIF ((LST(15).GE.124).AND.(LST(15).LE.129)) THEN    

c      IF(X.LE.0.0001)  THEN
c      WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0001001'
c      X=0.0001001
c      ENDIF
c      IF(X.GE.0.9)     THEN
c      WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.89999'
c      X=0.89999
c      ENDIF
c      IF(Q2.LE.1.)     THEN
c      WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.0001'
c      Q2 = 1.0001
c      ENDIF
c      IF(Q2.GE.50000.0) THEN
c      WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 49999.9'
c      Q2 = 49999.9
c      ENDIF 

      IF (LST(15).EQ.124) MODE=1
      IF (LST(15).EQ.125) MODE=2
      IF (LST(15).EQ.126) MODE=3
      IF (LST(15).EQ.127) MODE=4
      IF (LST(15).EQ.128) MODE=5
      IF (LST(15).EQ.129) MODE=6
      
C  unpolarized part 
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         IF ((LST(15).GE.124).AND.(LST(15).LE.126)) THEN
         CALL GRV94LO (DX,DQ2
     &          ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)
         ELSEIF ((LST(15).GE.127).AND.(LST(15).LE.129)) THEN
            CALL GRV94HO (DX,DQ2
     &           ,DDXUV,DDXDV,DDXDEL,DDXUDB,DDXSB,DDXGL)
         ENDIF
 
         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + (DDXUDB + DDXDEL)*0.5D0)
         XPQ(-1) = SNGL(         (DDXUDB + DDXDEL)*0.5D0)
         XPQ( 2) = SNGL(DDXUV  + (DDXUDB - DDXDEL)*0.5D0)
         XPQ(-2) = SNGL(         (DDXUDB - DDXDEL)*0.5D0)
         XPQ( 3) = SNGL(DDXSB) 
         XPQ(-3) = SNGL(DDXSB) 

*     polarized part

         CALL  POLFIT(MODE,DX,DQ2,DXDUV,DXDDV,DXDQBAR,DXDSTR,DXDGLU)
         XDPQ( 0) = SNGL(DXDGLU)
         XDPQ( 1) = SNGL(DXDDV) + SNGL(DXDQBAR)
         XDPQ(-1) =               SNGL(DXDQBAR) 
         XDPQ( 2) = SNGL(DXDUV) + SNGL(DXDQBAR)
         XDPQ(-2) =               SNGL(DXDQBAR)
         XDPQ( 3) =               SNGL(DXDSTR)
         XDPQ(-3) =               SNGL(DXDSTR)
      ELSEIF(LST(15).EQ.132)THEN         
*72*********************************************************************
         CALL FITPARTON(X,Q2,XPQ,XDPQ)
*72*********************************************************************
      ELSEIF ((LST(15).GE.133).AND.(LST(15).LE.136)) THEN

C  unpolarized part
*     "IMPROVED PARTON DISTRIBUTIONS FROM GLOBAL ANALYSIS OF
*   RECENT DEEP INELASTIC SCATTERING AND INCLUSIVE JET DATA"
*   By: H.L. Lai, J. Huston, S. Kuhlmann, F. Olness, J. Owens, 
*    D. Soper W.K. Tung, H. Weerts
*    Phys. Rev. D55, 1280 (1997) 
*   ( low Q2  set )
*         

*     from data statement above
*       CTEQSET=10  !  Low Q2
         DX=DBLE(X)
         DQ2=DBLE(Q2)
         IF(INITCTEQ.eq.1) THEN
            INITCTEQ=0
            CALL SETCTQ4(CTEQSET)
         ENDIF

*   Note in CTEQ u and d indices are reversed WRT pepsi order:
*   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
*                            for (b, c, s, d, u, g, u_bar, ..., b_bar),


         DDXPQ(-3) = CTQ4PDF(-3,DX,DQ2)
         DDXPQ(-1) = CTQ4PDF(-2,DX,DQ2)
         DDXPQ(-2) = CTQ4PDF(-1,DX,DQ2)
         DDXPQ(0) =  CTQ4PDF(0,DX,DQ2)
         DDXPQ(2) =  CTQ4PDF(1,DX,DQ2)
         DDXPQ(1) =  CTQ4PDF(2,DX,DQ2)
         DDXPQ(3) =  CTQ4PDF(3,DX,DQ2)

* CTEQ routine double precision q(x).  Make single  xq(x)
         DO I=-3,3
            XPQ( I) = X*SNGL(DDXPQ(I))
         ENDDO
         
         XDPQ( 0) = 0.0
         XDPQ( 1) = -0.3*XPQ(1)
         XDPQ(-1) = 0.0
         XDPQ( 2) = 0.5*XPQ(2)
         XDPQ(-2) = 0.0
         XDPQ( 3) = 0.0
         XDPQ(-3) = 0.0

* require F2 non-zero
         IF(XPQ(2).EQ.0.0) XPQ(2)=0.000001

      ELSEIF((LST(15).GE.137).AND.(LST(15).LE.138)) THEN

         MODE = 10       ! MSR(A prime) Low Q2 Parametrization


C  unpolarized part 
*     MRS(A prime)
        
         DX=DBLE(X)
         DQ2=DBLE(Q2)

         CALL MRSEB(DX,DQ2,MODE,MRSUPV,MRSDNV,MRSUSEA,
     &        MRSDSEA,MRSSTR,MRSCHM,MRSBOT,MRSGLU)

       
         XPQ( 0) = SNGL(MRSGLU)
         XPQ( 1) = SNGL(MRSDNV + MRSDSEA) 
         XPQ(-1) = SNGL(         MRSDSEA)
         XPQ( 2) = SNGL(MRSUPV + MRSUSEA)
         XPQ(-2) = SNGL(         MRSUSEA)
         XPQ( 3) = SNGL(MRSSTR)
         XPQ(-3) = SNGL(MRSSTR)
         XPQ( 4) = SNGL(MRSCHM)  
         XPQ(-4) = SNGL(MRSCHM)
         XPQ( 5) = SNGL(MRSBOT)   
         XPQ(-5) = SNGL(MRSBOT)


         
         XDPQ( 0) = 0.0
         XDPQ( 1) = -0.3*XPQ(1)
         XDPQ(-1) = 0.0
         XDPQ( 2) = 0.5*XPQ(2)
         XDPQ(-2) = 0.0
         XDPQ( 3) = 0.0
         XDPQ(-3) = 0.0



C***********************************************************************
C       LST(15) = 144  grsv2000  hep-ph/0011215   LO standard scenario
C       LST(15) = 145  grsv2000  hep-ph/0011215   LO  valence scenario  
C       LST(15) = 146  grsv2000  hep-ph/0011215  NLO standard scenario
C       LST(15) = 147  grsv2000  hep-ph/0011215  NLO  valence scenario

C
C       using GRV 1998 as unpolarized reference set
C
C***********************************************************************
      ELSEIF ((LST(15).GE.144).AND.(LST(15).LE.147)) THEN    

      IF(X.LE.0.0001)  THEN
c$$$      WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0001001'
      X=0.0001001
      ENDIF
      IF(X.GE.1.0)     THEN
c$$$      WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
      X=0.999999
      ENDIF
      IF(Q2.LE.0.8)     THEN
c$$$      WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 0.80001'
      Q2 = 0.80001
      ENDIF
      IF(Q2.GE.1.0E6) THEN
c$$$      WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 999999.9'
      Q2 = 999999.9
      ENDIF 

      
C  unpolarized part 
*                   M. Glueck, E. Reya, A. Vogt :                   *
*        hep-ph/9806404  =  DO-TH 98/07  =  WUE-ITP-98-019          *
*                  (To appear in Eur. Phys. J. C)                   *

         DX=DBLE(X)
         DQ2=DBLE(Q2)
         IF ((LST(15).GE.144).AND.(LST(15).LE.145)) THEN
            ISET = 1 ! LO
         ELSEIF ((LST(15).GE.146).AND.(LST(15).LE.147)) THEN
            ISET = 2 ! NLO
         ENDIF
         CALL GRV98PA (ISET, DX, DQ2 
     &           ,DDXUV,DDXDV,DDXUS, DDXDS, DDXSB,DDXGL)

  
         XPQ( 0) = SNGL(DDXGL)
         XPQ( 1) = SNGL(DDXDV  + DDXDS)
         XPQ(-1) = SNGL(DDXDS)         
         XPQ( 2) = SNGL(DDXUV  + DDXUS)   
         XPQ(-2) = SNGL(DDXUS)         
         XPQ( 3) = SNGL(DDXSB) 
         XPQ(-3) = SNGL(DDXSB) 

*     polarized part


      IF (LST(15).EQ.144) ISET = 3
      IF (LST(15).EQ.145) ISET = 4
      IF (LST(15).EQ.146) ISET = 1
      IF (LST(15).EQ.147) ISET = 2



         CALL PARPOLNEW (ISET, DX, DQ2, 
     &          DXDU, DXDD, DXDUB, DXDDB, DXDSTR, DXDGLU, 
     &          DUMMYG1P, DUMMYG1N)
         XDPQ( 0) = SNGL(DXDGLU)
         XDPQ( 1) = SNGL(DXDD)
         XDPQ(-1) = SNGL(DXDDB)           
         XDPQ( 2) = SNGL(DXDU)
         XDPQ(-2) = SNGL(DXDUB)           
         XDPQ( 3) = SNGL(DXDSTR)           
         XDPQ(-3) = SNGL(DXDSTR)
             
C*************************************************************************
C        LST(15) = 150   cteq5l                   LO
C        LST(15) = 151   cteq5m                   NLO MSBAR
C        LST(15) = 152   cteq5m1                  NLO MSBAR (update)
C*************************************************************************
  
         ELSEIF((LST(15).GE.150).AND.(LST(15).LE.152)) THEN

         IF(X.LE.0.00001)  THEN
c$$$       WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0000101'
            X=0.0000101
         ENDIF
         IF(X.GE.1.0)     THEN
c$$$       WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
            X=0.999999
         ENDIF
         IF(Q2.LE.1.0)     THEN
c$$$       WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.00001'
            Q2 = 1.00001
         ENDIF
         IF(Q2.GE.1.0E4) THEN
c$$$       WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 9999.9'
            Q2 = 9999.9
         ENDIF 

        XPQ(-5) = X*SNGL( CTQ5PDF (-5, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-4) = X*SNGL( CTQ5PDF (-4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-3) = X*SNGL( CTQ5PDF (-3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-2) = X*SNGL( CTQ5PDF (-1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-1) = X*SNGL( CTQ5PDF (-2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 0) = X*SNGL( CTQ5PDF ( 0, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 1) = X*SNGL( CTQ5PDF ( 2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 2) = X*SNGL( CTQ5PDF ( 1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 3) = X*SNGL( CTQ5PDF ( 3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 4) = X*SNGL( CTQ5PDF ( 4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 5) = X*SNGL( CTQ5PDF ( 5, DBLE(X), DBLE(SQRT(Q2))))

C*****************************************************************************
C        LST(15) = 161  mrs99   cor01  central gluon, a_s    
C        LST(15) = 162  mrs99   cor02  higher gluon        
C        LST(15) = 163  mrs99   cor03  lower gluon       
C        LST(15) = 164  mrs99   cor04  lower a_s      
C        LST(15) = 165  mrs99   cor05  higher a_s     
C        LST(15) = 166  mrs99   cor06  quarks up        
C        LST(15) = 167  mrs99   cor07  quarks down            
C        LST(15) = 168  mrs99   cor08  strange up           
C        LST(15) = 169  mrs99   cor09  strange down          
C        LST(15) = 170  mrs99   cor10  charm up         
C        LST(15) = 171  mrs99   cor11  charm down       
C        LST(15) = 172  mrs99   cor12  larger d/u          
C****************************************************************************

           ELSEIF((LST(15).GE.161).AND.(LST(15).LE.172)) THEN

         IF(X.LE.0.00001)  THEN
c$$$       WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0000101'
            X=0.0000101
         ENDIF
         IF(X.GE.1.0)     THEN
c$$$       WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
            X=0.999999
         ENDIF
         IF(Q2.LE.1.25)     THEN
c$$$        WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.25001'
            Q2 = 1.25001
         ENDIF
        IF(Q2.GE.1.0E4) THEN
c$$$       WRITE(*,*) 'Warning: Q2 exceeds maximum value, ',
c$$$     &                          'Q2 set to 9999999.9'
            Q2 = 9999999.9
         ENDIF 

        MODE = LST(15)-160
        CALL MRS99(DBLE(X),DBLE(SQRT(Q2)),MODE,
     &       MRSUPV,MRSDNV,MRSUSEA,MRSDSEA,MRSSTR,MRSCHM,MRSBOT,
     &       MRSGLU) 

        XPQ(-5) = SNGL(MRSBOT)
        XPQ(-4) = SNGL(MRSCHM)
        XPQ(-3) = SNGL(MRSSTR)
        XPQ(-2) = SNGL(MRSUSEA)
        XPQ(-1) = SNGL(MRSDSEA)
        XPQ( 0) = SNGL(MRSGLU)
        XPQ( 1) = SNGL(MRSDSEA)+SNGL(MRSDNV)
        XPQ( 2) = SNGL(MRSUSEA)+SNGL(MRSUPV)
        XPQ( 3) = SNGL(MRSSTR)
        XPQ( 4) = SNGL(MRSCHM)
        XPQ( 5) = SNGL(MRSBOT)

C Added By Joe Seele for the inclusion of the CTEQ 6 PDF's

      ELSEIF((LST(15).GE.173).AND.(LST(15).LE.175)) THEN

         IF(X.LE.0.00001)  THEN
c$$$       WRITE(*,*) 'Warning: x exceeds minum value, x set to 0.0000101'
            X=0.0000101
         ENDIF
         IF(X.GE.1.0)     THEN
c$$$       WRITE(*,*) 'Warning: x exceeds maxinum value, x set to 0.99999'
            X=0.999999
         ENDIF
         IF(Q2.LE.1.0)     THEN
c$$$       WRITE(*,*)'Warning: Q2 exceeds minimum value, Q2 set to 1.00001'
            Q2 = 1.00001
         ENDIF
         IF(Q2.GE.1.0E4) THEN
c$$$       WRITE(*,*) 'Warning: Q2 exceeds maximum value, Q2 set to 9999.9'
            Q2 = 9999.9
         ENDIF 

        XPQ(-5) = X*SNGL( CTQ6PDF (-5, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-4) = X*SNGL( CTQ6PDF (-4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-3) = X*SNGL( CTQ6PDF (-3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-2) = X*SNGL( CTQ6PDF (-1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ(-1) = X*SNGL( CTQ6PDF (-2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 0) = X*SNGL( CTQ6PDF ( 0, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 1) = X*SNGL( CTQ6PDF ( 2, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 2) = X*SNGL( CTQ6PDF ( 1, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 3) = X*SNGL( CTQ6PDF ( 3, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 4) = X*SNGL( CTQ6PDF ( 4, DBLE(X), DBLE(SQRT(Q2))))
        XPQ( 5) = X*SNGL( CTQ6PDF ( 5, DBLE(X), DBLE(SQRT(Q2))))

C*****************************************************************************



 
C*************************************************************************
      ELSE
         WRITE(*,*) 'NOT KNOWN PARTON DENSITY FUNCTION: STOP',LST(15)
         STOP
      ENDIF
*72*********************************************************************
      DO I=-6,6
        IF(ABS(XDPQ(I)).gt.XPQ(I)) THEN
          IF(ICERR.lt.IPLST(1).and.Q2.gt.1.) THEN 
             WRITE(*,*)'Non proper parton set : ',XDPQ(I),XPQ(I),I,X,Q2
             ICERR=ICERR+1
          ENDIF
          IF(XDPQ(I).gt.0) THEN 
             XDPQ(I)=XPQ(I)
          ELSE
             XDPQ(I)=-XPQ(I)
          ENDIF
       ENDIF
      ENDDO      
*72*********************************************************************      
C   This Amendment is to simulate HeliumIII
      IF(LST(39).NE.0) THEN
         F2P = 4.*(XPQ(2)+XPQ(-2)+XPQ(4)+XPQ(-4))
     &       +XPQ(1)+XPQ(-1)+XPQ(3)+XPQ(-3)
         IF(LST(36).eq.0) THEN 
            XDV=XPQ(1)-XPQ(-1)
            XUV=XPQ(2)-XPQ(-2)
            F2N = 4.*(XDV+2*XPQ(-2)+XPQ(4)+XPQ(-4))
     &          + XUV+2*XPQ(-1)+XPQ(3)+XPQ(-3)
         ELSE
            F2N = 4.*(XPQ(1)+XPQ(-1)+XPQ(4)+XPQ(-4))+
     &           XPQ(2)+XPQ(-2)+XPQ(3)+XPQ(-3)
         ENDIF
         
C     admitxurecoefficient for the unpolarized structure functions 
         PARI11 = 1./3.
 
         IF(LST(39).EQ.3) THEN
C..........................
C  This is a real helium 3.
C..........................
C Percentage of neutron and proton to the helium3 asymmetry
            PERCS   =  0.865
            PERCSP  =  0.
            PERCD   = -0.027
C admixture coefficients for the spin states
            ADMXS   = 1.0
            ADMXSP  = 0.
            ADMXD   = 0.
C factors for the spin state (number of polarized nucleons/total number
C of nucleons
            FACTS   = f2n/(f2n+2*f2p)
            FACTSP  = 0.
            FACTD   = 2*f2p/(f2n+2*f2p)
         ELSEIF(LST(39).EQ.2) THEN
C..........................
C  This is a real helium 3.
C..........................
C Percentage of the spin states
            PERCS   =  0.903
            PERCSP  =  0.014
            PERCD   = -0.083
C admixture coefficients for the spin states
            ADMXS   = 1.0
            ADMXSP  = 1./3.
            ADMXD   = 1./3.
C factors for the spin state (number of polarized nucleons/total number
C of nucleons
            FACTS   = f2n/(f2n+2*f2p)
            FACTSP  = 1./3.
            FACTD   = 1. 
C     admitxurecoefficient for the unpolarized structure functions 
            PARI11 = 1./3.
         ELSEIF(LST(39).EQ.1) THEN
C...........................
C  This is a ideal helium 3.
C...........................
C     Percentage of the spin states
            PERCS   = 1.0
            PERCSP  = 0.0
            PERCD   = 0.0
C     admixturecoefficients for the spin states
            ADMXS   = 1.
            ADMXSP  = 0.
            ADMXD   = 0.
C     factors for the spin state (number of polarized nucleons/total number
C     of nucleons
            FACTS   = f2n/(f2n+2*f2p)
            FACTSP  = 0.
            FACTD   = 0. 
C     admitxurecoefficient for the unpolarized structure functions
            PARI11 = 1./3.
         ENDIF
         
C...  For nuclear target, mix u- and d-valence distributions.
         IF(LST(36).EQ.0) THEN
C...  For nuclear target, mix u- and d-valence distributions.
            XPQ(1)=(1.-PARI11)*XDV+PARI11*XUV+XPQ(-1)
            XPQ(2)=(1.-PARI11)*XUV+PARI11*XDV+XPQ(-2)
C     Admixture in the polarized case         
            XDDV=XDPQ(1)-XDPQ(-1)
            XDUV=XDPQ(2)-XDPQ(-2)
            XDPQ(1)= ((1.-ADMXS )*XDDV+ADMXS *XDUV )
     &           * FACTS * PERCS
     &           +((1.-ADMXSP)*XDDV+ADMXSP*XDUV )
     &           * FACTSP* PERCSP 
     &           +((1.-ADMXD )*XDDV+ADMXD *XDUV )
     &           * FACTD * PERCD
            XDPQ(2)= ((1.-ADMXS )*XDUV+ADMXS *XDDV )
     &           * FACTS * PERCS
     &           +((1.-ADMXSP)*XDUV+ADMXSP*XDDV )
     &           * FACTSP* PERCSP
     &           +((1.-ADMXD )*XDUV+ADMXD *XDDV )
     &           * FACTD * PERCD
            DO I=-6,6
C...  For nuclear target, mix u- and d-valence distributions.
               IF(I.eq.1) THEN
* XDPQ(-1) has to be diluted here by (FACTS*PERCS+FACTSP...
                  XDPQ(1)= XDPQ(1) + XDPQ(-1)
               ELSEIF(I.eq.2) THEN
* XDPQ(-2) has to be diluted here by (FACTS*PERCS+FACTSP...                  
                  XDPQ(2)= XDPQ(2) + XDPQ(-2)
               ELSE
*     now for the sea quarks and gluon distributions                  
                  XDPQ(I)=XDPQ(I)*
     &                 (FACTS*PERCS+FACTSP*PERCSP+FACTD*PERCD)
               ENDIF
            ENDDO            
         ELSEIF(LST(36).EQ.1) THEN
            XD=XPQ(1)
            XU=XPQ(2)
            XPQ(1) =(1.-PARI11)*XD+PARI11*XU
            XPQ(2) =(1.-PARI11)*XU+PARI11*XD
            XD=XPQ(-1)
            XU=XPQ(-2)
            XPQ(-1)=(1.-PARI11)*XD+PARI11*XU
            XPQ(-2)=(1.-PARI11)*XU+PARI11*XD
C     Admixture in the polarized case
*.............................................
*           FOR u and d QUARKS
*.............................................         
            XDD=XDPQ(1)
            XDU=XDPQ(2)
            XDDS=XDPQ(-1)
            XDUS=XDPQ(-2)            
            DO I=-6,6
C...  For nuclear target, mix u- and d-valence distributions.
               IF(I.eq.1) THEN 
                  XDPQ(1)= ((1.-ADMXS )*XDD+ADMXS *XDU )
     &                 * FACTS * PERCS  
     &                 +((1.-ADMXSP)*XDD+ADMXSP*XDU )
     &                 * FACTSP* PERCSP 
     &                 +((1.-ADMXD )*XDD+ADMXD *XDU )
     &                 * FACTD * PERCD
               ELSEIF(I.eq.2) THEN 
                  XDPQ(2)= ((1.-ADMXS )*XDU+ADMXS *XDD )
     &                 * FACTS * PERCS
     &                 +((1.-ADMXSP)*XDU+ADMXSP*XDD )
     &                 * FACTSP* PERCSP
     &                 +((1.-ADMXD )*XDU+ADMXD *XDD )
     &                 * FACTD * PERCD
*     now for the sea quarks and gluon distributions
               ELSEIF(I.eq.-1) THEN 
                  XDPQ(-1)=((1.-ADMXS)*XDDS+ADMXS*XDUS)
     &                 * FACTS * PERCS  
     &                 +((1.-ADMXSP)*XDDS+ADMXSP*XDUS )
     &                 * FACTSP* PERCSP 
     &                 +((1.-ADMXD )*XDDS+ADMXD *XDUS )
     &                 * FACTD * PERCD
               ELSEIF(I.eq.-2) THEN
                  XDPQ(-2)=((1.-ADMXS)*XDUS+ADMXS*XDDS )
     &                 * FACTS * PERCS
     &                 +((1.-ADMXSP)*XDUS+ADMXSP*XDDS )
     &                 * FACTSP* PERCSP
     &                 +((1.-ADMXD )*XDUS+ADMXD *XDDS )
     &                 * FACTD * PERCD
               ELSE
*           now for the sea quarks and gluon distributions                  
                  XDPQ(I)=XDPQ(I)*
     &                 (FACTS*PERCS+FACTSP*PERCSP+FACTD*PERCD)
               ENDIF
            ENDDO
C...  For nuclear target, mix u- and d-valence distributions.
         ENDIF
      ENDIF
         
* polarisation enhancement factor 
      DO I=-6,6
         XDPQ(I) = FLOAT(LST(38))* XDPQ(I)
      ENDDO
      
      RETURN
      END
c=======================================================================


      FUNCTION PartonX5 (IPRTN, X, Q)
C
C   Given the parton distribution function in the array Upd in
C   COMMON / CtqPar1 / , this routine fetches u(fl, x, q) at any value of
C   x and q using Mth-order polynomial interpolation for x and Ln(Q/Lambda).
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      PARAMETER (M= 2, M1 = M + 1)
C
      Logical First
      Common 
     > / CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / CtqPar2 / Nx, Nt, NfMx
     > / XQrange / Qini, Qmax, Xmin
C
      Dimension Fq(M1), Df(M1)

      Data First /.true./
      save First
C                                                 Work with Log (Q)
      QG  = LOG (Q/AL)

C                           Find lower end of interval containing X
      JL = -1
      JU = Nx+1
 11   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (X .GT. XV(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 11
      Endif

      Jx = JL - (M-1)/2
      If (X .lt. Xmin .and. First ) Then
         First = .false.
         Print '(A, 2(1pE12.4))', 
     >     ' WARNING: X << Xmin, extrapolation used; X, Xmin =', X, Xmin
         If (Jx .LT. 0) Jx = 0
      Elseif (Jx .GT. Nx-M) Then
         Jx = Nx - M
      Endif
C                                    Find the interval where Q lies
      JL = -1
      JU = NT+1
 12   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (QG .GT. QL(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 12
      Endif

      Jq = JL - (M-1)/2
      If (Jq .LT. 0) Then
         Jq = 0
         If (Q .lt. Qini)  Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q << Qini, extrapolation used; Q, Qini =', Q, Qini
      Elseif (Jq .GT. Nt-M) Then
         Jq = Nt - M
         If (Q .gt. Qmax)  Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q > Qmax, extrapolation used; Q, Qmax =', Q, Qmax
      Endif

      If (Iprtn .GE. 3) Then
         Ip = - Iprtn
      Else
         Ip = Iprtn
      EndIf
C                             Find the off-set in the linear array Upd
      JFL = Ip + NfMx
      J0  = (JFL * (NT+1) + Jq) * (NX+1) + Jx
C
C                                           Now interpolate in x for M1 Q's
      Do 21 Iq = 1, M1
         J1 = J0 + (Nx+1)*(Iq-1) + 1
         Call Polint5 (XV(Jx), Upd(J1), M1, X, Fq(Iq), Df(Iq))
 21   Continue
C                                          Finish off by interpolating in Q
      Call Polint5 (QL(Jq), Fq(1), M1, QG, Ftmp, Ddf)

      PartonX5 = Ftmp
C
      RETURN
C                        ****************************
      END
c=======================================================================

      Function PartonX6 (IPRTN, XX, QQ)

c  Given the parton distribution function in the array U in
c  COMMON / PEVLDT / , this routine interpolates to find
c  the parton distribution at an arbitray point in x and q.
c
      Implicit Double Precision (A-H,O-Z)

      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)

      Common
     > / CtqPar1 / Al, XV(0:MXX), TV(0:MXQ), UPD(MXPQX)
     > / CtqPar2 / Nx, Nt, NfMx
     > / XQrange / Qini, Qmax, Xmin

      Dimension fvec(4), fij(4)
      Dimension xvpow(0:mxx)
      Data OneP / 1.00001 /
      Data xpow / 0.3d0 /       !**** choice of interpolation variable
      Data nqvec / 4 /
      Data ientry / 0 /
      Save ientry,xvpow

c store the powers used for interpolation on first call...
      if(ientry .eq. 0) then
         ientry = 1

         xvpow(0) = 0D0
         do i = 1, nx
            xvpow(i) = xv(i)**xpow
         enddo
      endif

      X = XX
      Q = QQ
      tt = log(log(Q/Al))

c      -------------    find lower end of interval containing x, i.e.,
c                       get jx such that xv(jx) .le. x .le. xv(jx+1)...
      JLx = -1
      JU = Nx+1
 11   If (JU-JLx .GT. 1) Then
         JM = (JU+JLx) / 2
         If (X .Ge. XV(JM)) Then
            JLx = JM
         Else
            JU = JM
         Endif
         Goto 11
      Endif
C                     Ix    0   1   2      Jx  JLx         Nx-2     Nx
C                           |---|---|---|...|---|-x-|---|...|---|---|
C                     x     0  Xmin               x                 1
C
      If     (JLx .LE. -1) Then
        Print '(A,1pE12.4)', 'Severe error: x <= 0 in PartonX6! x = ', x
        Stop
      ElseIf (JLx .Eq. 0) Then
         Jx = 0
      Elseif (JLx .LE. Nx-2) Then

C                For interrior points, keep x in the middle, as shown above
         Jx = JLx - 1
      Elseif (JLx.Eq.Nx-1 .or. x.LT.OneP) Then

C                  We tolerate a slight over-shoot of one (OneP=1.00001),
C              perhaps due to roundoff or whatever, but not more than that.
C                                      Keep at least 4 points >= Jx
         Jx = JLx - 2
      Else
        Print '(A,1pE12.4)', 'Severe error: x > 1 in PartonX6! x = ', x
        Stop
      Endif
C          ---------- Note: JLx uniquely identifies the x-bin; Jx does not.

C                       This is the variable to be interpolated in
      ss = x**xpow

      If (JLx.Ge.2 .and. JLx.Le.Nx-2) Then

c     initiation work for "interior bins": store the lattice points in s...
      svec1 = xvpow(jx)
      svec2 = xvpow(jx+1)
      svec3 = xvpow(jx+2)
      svec4 = xvpow(jx+3)

      s12 = svec1 - svec2
      s13 = svec1 - svec3
      s23 = svec2 - svec3
      s24 = svec2 - svec4
      s34 = svec3 - svec4

      sy2 = ss - svec2
      sy3 = ss - svec3

c constants needed for interpolating in s at fixed t lattice points...
      const1 = s13/s23
      const2 = s12/s23
      const3 = s34/s23
      const4 = s24/s23
      s1213 = s12 + s13
      s2434 = s24 + s34
      sdet = s12*s34 - s1213*s2434
      tmp = sy2*sy3/sdet
      const5 = (s34*sy2-s2434*sy3)*tmp/s12
      const6 = (s1213*sy2-s12*sy3)*tmp/s34

      EndIf

c         --------------Now find lower end of interval containing Q, i.e.,
c                          get jq such that qv(jq) .le. q .le. qv(jq+1)...
      JLq = -1
      JU = NT+1
 12   If (JU-JLq .GT. 1) Then
         JM = (JU+JLq) / 2
         If (tt .GE. TV(JM)) Then
            JLq = JM
         Else
            JU = JM
         Endif
         Goto 12
       Endif

      If     (JLq .LE. 0) Then
         Jq = 0
      Elseif (JLq .LE. Nt-2) Then
C                                  keep q in the middle, as shown above
         Jq = JLq - 1
      Else
C                         JLq .GE. Nt-1 case:  Keep at least 4 points >= Jq.
        Jq = Nt - 3

      Endif
C                                   This is the interpolation variable in Q

      If (JLq.GE.1 .and. JLq.LE.Nt-2) Then
c                                        store the lattice points in t...
      tvec1 = Tv(jq)
      tvec2 = Tv(jq+1)
      tvec3 = Tv(jq+2)
      tvec4 = Tv(jq+3)

      t12 = tvec1 - tvec2
      t13 = tvec1 - tvec3
      t23 = tvec2 - tvec3
      t24 = tvec2 - tvec4
      t34 = tvec3 - tvec4

      ty2 = tt - tvec2
      ty3 = tt - tvec3

      tmp1 = t12 + t13
      tmp2 = t24 + t34

      tdet = t12*t34 - tmp1*tmp2

      EndIf


c get the pdf function values at the lattice points...

      If (Iprtn .GE. 3) Then
         Ip = - Iprtn
      Else
         Ip = Iprtn
      EndIf
      jtmp = ((Ip + NfMx)*(NT+1)+(jq-1))*(NX+1)+jx+1

      Do it = 1, nqvec

         J1  = jtmp + it*(NX+1)

       If (Jx .Eq. 0) Then
C                          For the first 4 x points, interpolate x^2*f(x,Q)
C                           This applies to the two lowest bins JLx = 0, 1
C            We can not put the JLx.eq.1 bin into the "interrior" section
C                           (as we do for q), since Upd(J1) is undefined.
         fij(1) = 0
         fij(2) = Upd(J1+1) * XV(1)**2
         fij(3) = Upd(J1+2) * XV(2)**2
         fij(4) = Upd(J1+3) * XV(3)**2
C
C                 Use Polint6 which allows x to be anywhere w.r.t. the grid

         Call Polint6 (XVpow(0), Fij(1), 4, ss, Fx, Dfx)

         If (x .GT. 0D0)  Fvec(it) =  Fx / x**2
C                                              Pdf is undefined for x.eq.0
       ElseIf  (JLx .Eq. Nx-1) Then
C                                                This is the highest x bin:

        Call Polint6 (XVpow(Nx-3), Upd(J1), 4, ss, Fx, Dfx)

        Fvec(it) = Fx

       Else
C                       for all interior points, use Jon's in-line function
C                              This applied to (JLx.Ge.2 .and. JLx.Le.Nx-2)
         sf2 = Upd(J1+1)
         sf3 = Upd(J1+2)

         g1 =  sf2*const1 - sf3*const2
         g4 = -sf2*const3 + sf3*const4

         Fvec(it) = (const5*(Upd(J1)-g1)
     &               + const6*(Upd(J1+3)-g4)
     &               + sf2*sy3 - sf3*sy2) / s23

       Endif

      enddo
C                                   We now have the four values Fvec(1:4)
c     interpolate in t...

      If (JLq .LE. 0) Then
C                         1st Q-bin, as well as extrapolation to lower Q
        Call Polint6 (TV(0), Fvec(1), 4, tt, ff, Dfq)

      ElseIf (JLq .GE. Nt-1) Then
C                         Last Q-bin, as well as extrapolation to higher Q
        Call Polint6 (TV(Nt-3), Fvec(1), 4, tt, ff, Dfq)
      Else
C                         Interrior bins : (JLq.GE.1 .and. JLq.LE.Nt-2)
C       which include JLq.Eq.1 and JLq.Eq.Nt-2, since Upd is defined for
C                         the full range QV(0:Nt)  (in contrast to XV)
        tf2 = fvec(2)
        tf3 = fvec(3)

        g1 = ( tf2*t13 - tf3*t12) / t23
        g4 = (-tf2*t34 + tf3*t24) / t23

        h00 = ((t34*ty2-tmp2*ty3)*(fvec(1)-g1)/t12
     &    +  (tmp1*ty2-t12*ty3)*(fvec(4)-g4)/t34)

        ff = (h00*ty2*ty3/tdet + tf2*ty3 - tf3*ty2) / t23
      EndIf

      PartonX6 = ff

      Return
C                                       ********************
      End
c======================================================================
C

      SUBROUTINE POLINT5 (XA,YA,N,X,Y,DY)
 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C                                        Adapted from "Numerical Recipes" 
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
c======================================================================
C

      SUBROUTINE POLINT6 (XA,YA,N,X,Y,DY)

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C                                        Adapted from "Numerical Recipes"
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END

C************************************************************************
      FUNCTION GAMMA(XX)
      REAL GAMMAGAMMLN,XX
      INTEGER J
      REAL SER,STP,TMP,X,Y,COF(6)
      SAVE COF,STP
      DATA COF,STP/76.18009172947146E0,-86.50532032941677E0,
     & 24.01409824083091E0,-1.231739572450155E0,.1208650973866179E-2,
     & -.5395239384953E-5,2.5066282746310005E0/
      X=XX
      Y=X
      TMP=X+5.5E0
      TMP=(X+0.5E0)*LOG(TMP)-TMP
      SER=1.000000000190015E0
      DO J=1,6
         Y=Y+1.E0
         SER=SER+COF(J)/Y
      ENDDO
      GAMMLN=TMP+LOG(STP*SER/X)
      GAMMA = EXP(GAMMLN)
      RETURN
      END


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                   *
*     G R V  -  P R O T O N  - P A R A M E T R I Z A T I O N S      *
*                                                                   *
*                          1998 UPDATE                              *
*                                                                   *
*                  For a detailed explanation see                   *
*                   M. Glueck, E. Reya, A. Vogt :                   *
*        hep-ph/9806404  =  DO-TH 98/07  =  WUE-ITP-98-019          *
*                  (To appear in Eur. Phys. J. C)                   *
*                                                                   *
*   This package contains subroutines returning the light-parton    *
*   distributions in NLO (for the MSbar and DIS schemes) and LO;    * 
*   the respective light-parton, charm, and bottom contributions    *
*   to F2(electromagnetic); and the scale dependence of alpha_s.    *
*                                                                   *
*   The parton densities and F2 values are calculated from inter-   *
*   polation grids covering the regions                             *
*         Q^2/GeV^2  between   0.8   and  1.E6 ( 1.E4 for F2 )      *
*            x       between  1.E-9  and   1.                       *
*   Any call outside these regions stops the program execution.     *
*                                                                   *
*   At Q^2 = MZ^2, alpha_s reads  0.114 (0.125) in NLO (LO); the    *
*   heavy quark thresholds, QH^2 = mh^2, in the beta function are   *
*            mc = 1.4 GeV,  mb = 4.5 GeV,  mt = 175 GeV.            *
*   Note that the NLO alpha_s running is different from GRV(94).    * 
*                                                                   *
*    Questions, comments etc to:  avogt@physik.uni-wuerzburg.de     *
*                                                                   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*
*
*
      SUBROUTINE GRV98PA (ISET, X, Q2, UV, DV, US, DS, SS, GL)
*********************************************************************
*                                                                   *
*   THE PARTON ROUTINE.                                             *
*                                     __                            *
*   INPUT:   ISET =  1 (LO),  2 (NLO, MS), or  3 (NLO, DIS)         *
*            X  =  Bjorken-x        (between  1.E-9 and 1.)         *
*            Q2 =  scale in GeV**2  (between  0.8 and 1.E6)         *
*                                                                   *
*   OUTPUT:  UV = u - u(bar),  DV = d - d(bar),  US = u(bar),       *
*            DS = d(bar),  SS = s = s(bar),  GL = gluon.            *
*            Always x times the distribution is returned.           *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINIP / IINIP , and the     *
*            integer variable  IINIP  has always to be zero when    *
*            GRV98PA is called for the first time or when  ISET     *
*            has been changed.                                      *
*                                                                   *
*   GRIDS:   1. grv98lo.grid, 2. grv98nlm.grid, 3. grv98nld.grid,   *
*            (1+1809 lines with 6 columns, 4 significant figures)   *
*                                                                   *
*******************************************************i*************
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (NPART=6, NX=68, NQ=27, NARG=2)
      DIMENSION XUVF(NX,NQ), XDVF(NX,NQ), XDEF(NX,NQ), XUDF(NX,NQ),
     1          XSF(NX,NQ), XGF(NX,NQ), PARTON (NPART,NQ,NX-1), 
     2          QS(NQ), XB(NX), XT(NARG), NA(NARG), ARRF(NX+NQ) 
      CHARACTER*80 LINE
      COMMON / INTINIP / IINIP
      SAVE XUVF, XDVF, XDEF, XUDF, XSF, XGF, NA, ARRF
*
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.8E0, 
     1           1.0E0, 1.3E0, 1.8E0, 2.7E0, 4.0E0, 6.4E0,
     2           1.0E1, 1.6E1, 2.5E1, 4.0E1, 6.4E1,
     3           1.0E2, 1.8E2, 3.2E2, 5.7E2,
     4           1.0E3, 1.8E3, 3.2E3, 5.7E3,
     5           1.0E4, 2.2E4, 4.6E4,
     6           1.0E5, 2.2E5, 4.6E5, 
     7           1.E6 /
       DATA XB / 1.0E-9, 1.8E-9, 3.2E-9, 5.7E-9, 
     A           1.0E-8, 1.8E-8, 3.2E-8, 5.7E-8, 
     B           1.0E-7, 1.8E-7, 3.2E-7, 5.7E-7, 
     C           1.0E-6, 1.4E-6, 2.0E-6, 3.0E-6, 4.5E-6, 6.7E-6,
     1           1.0E-5, 1.4E-5, 2.0E-5, 3.0E-5, 4.5E-5, 6.7E-5,
     2           1.0E-4, 1.4E-4, 2.0E-4, 3.0E-4, 4.5E-4, 6.7E-4,
     3           1.0E-3, 1.4E-3, 2.0E-3, 3.0E-3, 4.5E-3, 6.7E-3,
     4           1.0E-2, 1.4E-2, 2.0E-2, 3.0E-2, 4.5E-2, 0.06, 0.08,
     5           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     6           0.3, 0.325, 0.35, 0.375, 0.4,  0.45, 0.5, 0.55,
     7           0.6, 0.65,  0.7,  0.75,  0.8,  0.85, 0.9, 0.95, 1. /
*
*...CHECK OF X AND Q2 VALUES : 
      IF ( (X.LT.0.99D-9) .OR. (X.GT.1.D0) ) THEN
         WRITE(6,91) 
  91     FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
         STOP
      ENDIF
      IF ( (Q2.LT.0.799) .OR. (Q2.GT.1.01E6) ) THEN
         WRITE(6,92) 
  92     FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
         STOP
      ENDIF
      IF (IINIP .NE. 0) GOTO 16
*
*...INITIALIZATION, IF REQUIRED :
*
*    SELECTION AND READING OF THE GRID : 
*    (COMMENT: FIRST NUMBER IN THE FIRST LINE OF THE GRID)
      IF (ISET .EQ. 1) THEN
        OPEN (11,FILE='./pdf/grv98lo.grid',STATUS='old')   ! 7.332E-05
      ELSE IF (ISET .EQ. 2) THEN
        OPEN (11,FILE='./pdf/grv98nlm.grid',STATUS='old')  ! 1.015E-04
      ELSE IF (ISET .EQ. 3) THEN
        OPEN (11,FILE='./pdf/grv98nld.grid',STATUS='old')  ! 1.238E-04
      ELSE
        WRITE(6,93)
  93    FORMAT (2X,'NO OR INVALID PARTON SET CHOICE')
        STOP
      END IF
      IINIP = 1
      READ(11,89) LINE
  89  FORMAT(A80)
      DO 15 M = 1, NX-1 
      DO 15 N = 1, NQ
      READ(11,90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1            PARTON(4,N,M), PARTON(5,N,M), PARTON(6,N,M) 
  90  FORMAT (6(1PE10.3))
  15  CONTINUE
      CLOSE(11)
*
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XB0V = XB(IX)**0.5 
        XB0S = XB(IX)**(-0.2) 
        XB1 = 1.-XB(IX)
        XUVF(IX,IQ) = PARTON(1,IQ,IX) / (XB1**3 * XB0V)
        XDVF(IX,IQ) = PARTON(2,IQ,IX) / (XB1**4 * XB0V)
        XDEF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**7 * XB0V) 
        XUDF(IX,IQ) = PARTON(4,IQ,IX) / (XB1**7 * XB0S)
        XSF(IX,IQ)  = PARTON(5,IQ,IX) / (XB1**7 * XB0S)
        XGF(IX,IQ)  = PARTON(6,IQ,IX) / (XB1**5 * XB0S)
  20  CONTINUE
        XUVF(NX,IQ) = 0.E0
        XDVF(NX,IQ) = 0.E0
        XDEF(NX,IQ) = 0.E0
        XUDF(NX,IQ) = 0.E0
        XSF(NX,IQ)  = 0.E0
        XGF(NX,IQ)  = 0.E0
  10  CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
*
*...CONTINUATION, IF INITIALIZATION WAS DONE PREVIOUSLY.
*
  16  CONTINUE
*
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      X1 = 1.- X
      XV = X**0.5
      XS = X**(-0.2)
      UV = FINT(NARG,XT,NA,ARRF,XUVF) * X1**3 * XV
      DV = FINT(NARG,XT,NA,ARRF,XDVF) * X1**4 * XV
      DE = FINT(NARG,XT,NA,ARRF,XDEF) * X1**7 * XV
      UD = FINT(NARG,XT,NA,ARRF,XUDF) * X1**7 * XS
      US = 0.5 * (UD - DE)
      DS = 0.5 * (UD + DE)
      SS = FINT(NARG,XT,NA,ARRF,XSF)  * X1**7 * XS
      GL = FINT(NARG,XT,NA,ARRF,XGF)  * X1**5 * XS 
*
 60   RETURN
      END
*
*
*

*
      FUNCTION FINT(NARG,ARG,NENT,ENT,TABLE)
*********************************************************************
*                                                                   *
*   THE INTERPOLATION ROUTINE (CERN LIBRARY ROUTINE E104)           *
*                                                                   *
*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION ARG(5),NENT(5),ENT(10),TABLE(10)
      DIMENSION D(5),NCOMB(5),IENT(5)
      KD=1
      M=1
      JA=1
         DO 5 I=1,NARG
      NCOMB(I)=1
      JB=JA-1+NENT(I)
         DO 2 J=JA,JB
      IF (ARG(I).LE.ENT(J)) GO TO 3
    2 CONTINUE
      J=JB
    3 IF (J.NE.JA) GO TO 4
      J=J+1
    4 JR=J-1
      D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
      IENT(I)=J-JA
      KD=KD+IENT(I)*M
      M=M*NENT(I)
    5 JA=JB+1
      FINT=0.
   10 FAC=1.
      IADR=KD
      IFADR=1
         DO 15 I=1,NARG
      IF (NCOMB(I).EQ.0) GO TO 12
      FAC=FAC*(1.-D(I))
      GO TO 15
   12 FAC=FAC*D(I)
      IADR=IADR-IFADR
   15 IFADR=IFADR*NENT(I)
      FINT=FINT+FAC*TABLE(IADR)
      IL=NARG
   40 IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
         DO 50  K=IL,NARG
   50 NCOMB(K)=1
      GO TO 10
   80 IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END
*
*
*
*
      FUNCTION ALPHAS (Q2, NAORD)
*********************************************************************
*                                                                   *
*   THE ALPHA_S ROUTINE.                                            *
*                                                                   *
*   INPUT :  Q2    =  scale in GeV**2  (not too low, of course);    *
*            NAORD =  1 (LO),  2 (NLO).                             *
*                                                                   *
*   OUTPUT:  alphas_s/(4 pi) for use with the GRV(98) partons.      *  
*                                                                   *
*******************************************************i*************
*
      IMPLICIT DOUBLE PRECISION (A - Z)
      INTEGER NF, K, I, NAORD
      DIMENSION LAMBDAL (3:6),  LAMBDAN (3:6), Q2THR (3)
*
*...HEAVY QUARK THRESHOLDS AND LAMBDA VALUES :
      DATA Q2THR   /  1.960,  20.25,  30625. /
      DATA LAMBDAL / 0.2041, 0.1750, 0.1320, 0.0665 /
      DATA LAMBDAN / 0.2994, 0.2460, 0.1677, 0.0678 /
*
*...DETERMINATION OF THE APPROPRIATE NUMBER OF FLAVOURS :
      NF = 3
      DO 10 K = 1, 3
      IF (Q2 .GT. Q2THR (K)) THEN
         NF = NF + 1
      ELSE
          GO TO 20
       END IF
  10   CONTINUE
*
*...LO ALPHA_S AND BETA FUNCTION FOR NLO CALCULATION :
  20   B0 = 11.- 2./3.* NF
       B1 = 102.- 38./3.* NF
       B10 = B1 / (B0*B0)
       IF (NAORD .EQ. 1) THEN
         LAM2 = LAMBDAL (NF) * LAMBDAL (NF)
         ALP  = 1./(B0 * DLOG (Q2/LAM2))
         GO TO 1
       ELSE IF (NAORD .EQ. 2) then
         LAM2 = LAMBDAN (NF) * LAMBDAN (NF)
         B1 = 102.- 38./3.* NF
         B10 = B1 / (B0*B0)
       ELSE
         WRITE (6,91)
  91     FORMAT ('INVALID CHOICE FOR ORDER IN ALPHA_S')
         STOP
       END IF
*
*...START VALUE FOR NLO ITERATION :
       LQ2 = DLOG (Q2 / LAM2)
       ALP = 1./(B0*LQ2) * (1.- B10*DLOG(LQ2)/LQ2)
*
*...EXACT NLO VALUE, FOUND VIA NEWTON PROCEDURE :
       DO 2 I = 1, 6
       XL  = DLOG (1./(B0*ALP) + B10)
       XLP = DLOG (1./(B0*ALP*1.01) + B10)
       XLM = DLOG (1./(B0*ALP*0.99) + B10)
       Y  = LQ2 - 1./ (B0*ALP) + B10 * XL
       Y1 = (- 1./ (B0*ALP*1.01) + B10 * XLP
     1       + 1./ (B0*ALP*0.99) - B10 * XLP) / (0.02D0*ALP)
       ALP = ALP - Y/Y1
  2    CONTINUE
*
*...OUTPUT :
  1    ALPHAS = ALP
       RETURN
       END
*
*********************************************************************
*                                                                   *
*    POLARIZED RADIATIVELY GENERATED LO AND NLO PARTON DENSITIES    *
*                                                                   *
*         M. GLUCK, E. REYA, M. STRATMANN AND W. VOGELSANG,         *
*                        hep-ph/0011215                             *
*                                                                   *
*          PROBLEMS/QUESTIONS TO wvogelsang@bnl.gov                 *
*            OR TO marco.stratmann@physik.uni-regensburg.de         *
*                                                                   *
*   INPUT:   ISET = number of the parton set :                      *
*            ISET = 1  'STANDARD' SCENARIO, NEXT-TO-LEADING ORDER   *
*                      (MS-bar)                                     * 
*                      (DATA FILE 'std2000_nlo.grid' UNIT=11, TO BE *
*                       DEFINED BY THE USER )                       *
*            ISET = 2  'VALENCE' SCENARIO,  NEXT-TO-LEADING ORDER   *
*                      (MS-bar)                                     *   
*                      (DATA FILE 'val2000_nlo.grid' UNIT=22, TO BE *
*                       DEFINED BY THE USER )                       *
*            ISET = 3  'STANDARD' SCENARIO, LEADING ORDER           *
*                      (DATA FILE 'std2000_lo.grid' UNIT=33, TO BE  *
*                       DEFINED BY THE USER )                       *
*            ISET = 4  'VALENCE' SCENARIO,  LEADING ORDER           *
*                      (DATA FILE 'val2000_lo.grid' UNIT=44, TO BE  *
*                       DEFINED BY THE USER )                       *
*                                                                   *
*            X  = Bjorken-x       (between  1.E-4  and  1)          *
*            Q2 = scale in GeV**2 (between  0.8  and   1.E6)        *
*                                                                   *
*   OUTPUT:  U = x * DELTA u                                        *
*            D = x * DELTA d                                        *        
*            UB = x * DELTA ubar                                    *   
*            DB = x * DELTA dbar                                    * 
*            ST = x * DELTA STRANGE                                 *     
*            GL = x * DELTA GLUON                                   *
*            G1P = g_1^proton                                       *
*            G1N = g_1^neutron                                      * 
*                                                                   *
*          (  For the parton distributions always x times           *
*                   the distribution is returned .                  *
*                 This is NOT the case for g1(p,n)  )               *
*                                                                   *
*            The sets are the result of a combined fit to           *
*            data for the spin asymmetries A_1 (p,n,d)              *
*                                                                   *
*            Note: No charm is included                             *
*                                                                   *
*   COMMON:  The main program or the calling routine has to have    *
*            a common block  COMMON / INTINI / IINI , and  IINI     *
*            has always to be zero when PARPOL is called for the    *
*            first time or when 'ISET' has been changed.            *
*                                                                   *
*********************************************************************
*
      SUBROUTINE PARPOLNEW (ISET, X, Q2, U, D, UB, DB, ST,GL,G1P,G1N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NPART=8, NX=42, NQ=30, NARG=2)
      DIMENSION XUF(NX,NQ), XDF(NX,NQ), XUBF(NX,NQ), XDBF(NX,NQ), 
     1          XSF(NX,NQ), XGF(NX,NQ), XG1P(NX,NQ), XG1N(NX,NQ),
     2          PARTON (NPART,NQ,NX-1), QS(NQ), XB(NX), XT(NARG), 
     3          NA(NARG), ARRF(NX+NQ) 
      COMMON / INTINI / IINI
      SAVE XUF, XDF, XUBF, XDBF, XSF, XGF, XG1P, XG1N, NA, ARRF
*...BJORKEN-X AND Q**2 VALUES OF THE GRID :
       DATA QS / 0.8D0, 1.0D0, 1.25d0, 1.5D0, 2.d0, 2.5D0, 
     1           4.0D0, 6.4D0, 1.0D1, 1.5D1, 2.5D1, 4.0D1, 6.4D1,
     2           1.0D2, 1.8D2, 3.2D2, 5.8D2, 1.0D3, 1.8D3,
     3           3.2D3, 5.8D3, 1.0D4, 1.8D4, 3.2D4, 5.8D4, 
     4           1.0D5, 1.8D5, 3.2D5, 5.8D5, 1.0D6  /
       DATA XB / 
     1           1.D-4, 1.5D-4, 2.2D-4, 3.2D-4, 4.8D-4, 7.D-4,
     2           1.D-3, 1.5D-3, 2.2D-3, 3.2D-3, 4.8D-3, 7.D-3,
     3           1.D-2, 1.5D-2, 2.2D-2, 3.2D-2, 5.0D-2, 7.5D-2,
     4           0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275,
     5           0.3, 0.325, 0.35, 0.375, 0.4, 0.45,  0.5, 0.55,
     6           0.6, 0.65,  0.7,  0.75,  0.8, 0.85,  0.9, 1.0 /
*...CHECK OF X AND Q2 VALUES : 
       IF ( (X.LT.1.0D-4) .OR. (X.GT.1.0D0) ) THEN
           WRITE(6,91) 
  91       FORMAT (2X,'PARTON INTERPOLATION: X OUT OF RANGE')
           STOP
c          GOTO 60
       ENDIF
       IF ( (Q2.LT.0.8D0) .OR. (Q2.GT.1.D6) ) THEN
           WRITE(6,92) 
  92       FORMAT (2X,'PARTON INTERPOLATION: Q2 OUT OF RANGE')
           STOP
C          GOTO 60
       ENDIF
*...INITIALIZATION :
*    SELECTION AND READING OF THE GRID :
*    FILE - NO. = 11 FOR NLO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.3478E-03 )
*    FILE - NO. = 22 FOR NLO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 1.5146E-05 )
*    FILE - NO. = 33 FOR  LO 'STANDARD' SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 3.4686E-03 )     
*    FILE - NO. = 44 FOR  LO 'VALENCE'  SCENARIO ( FIRST NUMBER IN THE 
*                                                  GRID: 2.4395E-04 )
      IF (IINI.NE.0) GOTO 16
      IF (ISET.EQ.1) THEN
       IIREAD=11       
       OPEN(UNIT=11,FILE='./pdf/std2000_nlo_g1.grid',STATUS='OLD')
      ELSE IF (ISET.EQ.2) THEN
       IIREAD=22
       OPEN(UNIT=22,FILE='./pdf/val2000_nlo_g1.grid',STATUS='OLD')
      ELSE IF (ISET.EQ.3) THEN
       IIREAD=33       
       OPEN(UNIT=33,FILE='./pdf/std2000_lo_g1.grid',STATUS='OLD')
      ELSE IF (ISET.EQ.4) THEN
       IIREAD=44
       OPEN(UNIT=44,FILE='./pdf/val2000_lo_g1.grid',STATUS='OLD')
      ELSE
        WRITE(6,93)
  93    FORMAT (2X,'PARTON INTERPOLATION: ISET OUT OF RANGE')
        GOTO 60
      END IF
C
       DO 15 M = 1, NX-1
       DO 15 N = 1, NQ
       READ(IIREAD,90) PARTON(1,N,M), PARTON(2,N,M), PARTON(3,N,M), 
     1                 PARTON(4,N,M), PARTON(5,N,M), PARTON(6,N,M),
     2                 PARTON(7,N,M), PARTON(8,N,M)
  90   FORMAT (8(1PE12.4))
  15   CONTINUE
C
      IINI = 1
*....ARRAYS FOR THE INTERPOLATION SUBROUTINE :
      DO 10 IQ = 1, NQ
      DO 20 IX = 1, NX-1
        XB0 = XB(IX) 
        XB1 = 1.D0-XB(IX)
        XUF(IX,IQ) = PARTON(1,IQ,IX) / (XB1**3 * XB0)
        XDF(IX,IQ) = PARTON(2,IQ,IX) / (XB1**4 * XB0)
        XUBF(IX,IQ) = PARTON(3,IQ,IX) / (XB1**8 * XB0**0.5) 
        XDBF(IX,IQ) = PARTON(4,IQ,IX) / (XB1**8 * XB0**0.5) 
        XSF(IX,IQ)  = PARTON(5,IQ,IX) / (XB1**8 * XB0**0.5) 
        XGF(IX,IQ)  = PARTON(6,IQ,IX) / (XB1**5 * XB0**2.)
        XG1P(IX,IQ)  = PARTON(7,IQ,IX) / XB1**3
        XG1N(IX,IQ)  = PARTON(8,IQ,IX) / XB1**3
  20  CONTINUE
        XUF(NX,IQ) = 0.D0
        XDF(NX,IQ) = 0.D0
        XUBF(NX,IQ) = 0.D0
        XDBF(NX,IQ) = 0.D0
        XSF(NX,IQ)  = 0.D0
        XGF(NX,IQ)  = 0.D0
        XG1P(NX,IQ)  = 0.D0
        XG1N(NX,IQ)  = 0.D0
  10  CONTINUE  
      NA(1) = NX
      NA(2) = NQ
      DO 30 IX = 1, NX
        ARRF(IX) = DLOG(XB(IX))
  30  CONTINUE
      DO 40 IQ = 1, NQ
        ARRF(NX+IQ) = DLOG(QS(IQ))
  40  CONTINUE
  16  CONTINUE
*...INTERPOLATION :
      XT(1) = DLOG(X)
      XT(2) = DLOG(Q2)
      U = DFINTNEW(NARG,XT,NA,ARRF,XUF) * (1.D0-X)**3 * X
      D = DFINTNEW(NARG,XT,NA,ARRF,XDF) * (1.D0-X)**4 * X
      UB = DFINTNEW(NARG,XT,NA,ARRF,XUBF) * (1.D0-X)**8 * X**0.5
      DB = DFINTNEW(NARG,XT,NA,ARRF,XDBF) * (1.D0-X)**8 * X**0.5
      ST = DFINTNEW(NARG,XT,NA,ARRF,XSF)  * (1.D0-X)**8 * X**0.5
      GL = DFINTNEW(NARG,XT,NA,ARRF,XGF)  * (1.D0-X)**5 * X**2.
      G1P = DFINTNEW(NARG,XT,NA,ARRF,XG1P)  * (1.D0-X)**3
      G1N = DFINTNEW(NARG,XT,NA,ARRF,XG1N)  * (1.D0-X)**3
 60   RETURN
      END
*
*...CERN LIBRARY ROUTINE E104 (INTERPOLATION) :
*
      FUNCTION DFINTNEW(NARG,ARG,NENT,ENT,TABLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION ARG(5),NENT(5),ENT(73),TABLE(1200)
      DIMENSION D(5),NCOMB(5),IENT(5)
      KD=1
      M=1
      JA=1
         DO 5 I=1,NARG
      NCOMB(I)=1
      JB=JA-1+NENT(I)
         DO 2 J=JA,JB
      IF (ARG(I).LE.ENT(J)) GO TO 3
    2 CONTINUE
      J=JB
    3 IF (J.NE.JA) GO TO 4
      J=J+1
    4 JR=J-1
      D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
      IENT(I)=J-JA
      KD=KD+IENT(I)*M
      M=M*NENT(I)
    5 JA=JB+1
      DFINTNEW=0.D0
   10 FAC=1.D0
      IADR=KD
      IFADR=1
         DO 15 I=1,NARG
      IF (NCOMB(I).EQ.0) GO TO 12
      FAC=FAC*(1.D0-D(I))
      GO TO 15
   12 FAC=FAC*D(I)
      IADR=IADR-IFADR
   15 IFADR=IFADR*NENT(I)
      DFINTNEW=DFINTNEW+FAC*TABLE(IADR)
      IL=NARG
   40 IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
         DO 50  K=IL,NARG
   50 NCOMB(K)=1
      GO TO 10
   80 IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END

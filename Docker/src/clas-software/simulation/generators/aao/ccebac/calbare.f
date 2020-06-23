c=====================================================================
c  this part should be improved
c--------------------------------------------------------------------
c  prepare bare amplitude
c
c  bareamp(ires,imul) 
c
c  bareamp = <N*|j.e|N> with imul=1  e=e(+1),   sn=1/2
c                                 2  e=e(+1),   sn=-1/2
c                                 3  e=e(time), sn=1/2
c                                 4  e=e(time), sn=-1/2
c                                 5  e=e(-1),   sn=+1/2
c                                 6  e=e(-1),   sn=-1/2
c   4-6 unused 
c
c
c  for isospin 3/2   <Delta+|j.e|p>
c      isospin p1/2  <N* tz= 1/2|j.e|p>
c              n1/2  <N* tz=-1/2|j.e|n>
      subroutine calbare(egam,egam0,qpio,wcm,jpin,lpin,itpin,bareamp)
      implicit real*8(a-h,o-z)
      PARAMETER (MAXH=100)
      common / const / pi, fm, scale
      common / cmass / fnuc,fpio,fdel,fdelgm,fmrho,fmomg,feta,fsigm
      common/helpar/hpar(MAXH)

      dimension bareamp(5,6)
      data iwrite/1/,icount/0/

c bjd Aug 14 2007
      character numbers*10,wave*9
      character wpw*3

      numbers="0123456789"
      WAVE   ="SPDFGHIJK"
       wpw=wave(Lpin/2+1:Lpin/2+1)//numbers(itpin+1:itpin+1)
     1//numbers(jpin+1:jpin+1)
c bjd<

      q2       = egam**2 - egam0**2
      bareamp  = 0

      
      if (wpw.eq.'S11') then 
c 1st s11
      ams=1535.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=91.00                      ! 91 (2), 66
      ahel1=ahe1*conv/cc
      ahe3=0.
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
 1123 format(a3,' hel amp:',4e12.4)
      bareamp(1,2) = hpar(40)*ahel1
      bareamp(1,3) = hpar(41)*ahel0
c 2nd s11
      ams=1650.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=22.00                    ! 22 (7), 33
      ahel1=ahe1*conv/cc
      ahe3=0.
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(2,2) = hpar(42)*ahel1
      bareamp(2,3) = hpar(43)*ahel0
      endif
      if (wpw.eq.'S31') then 
c 1st s31
      ams=1620.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=50.00                    ! 50 (2)
      ahel1=ahe1*conv/cc
      ahe3=0.
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,2) = hpar(44)*ahel1
      bareamp(1,3) = hpar(45)*ahel0
      endif
      if (wpw.eq.'P11') then 
c 1st p11
      ams=1440.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=-51.00                    ! -51 (2), -61
      ahel1=ahe1*conv/cc
      ahe3=0.
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,2) = hpar(46)*ahel1
      bareamp(1,3) = hpar(47)*ahel0
c 2nd p11
      ams=1710.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=6.00                    ! 6 (27)
      ahel1=ahe1*conv/cc
      ahe3=0.
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(2,2) = hpar(48)*ahel1
      bareamp(2,3) = hpar(49)*ahel0
      endif
      if (wpw.eq.'P13') then 
c 1st p13
      ams=1720.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=97.00                    ! 97 (3), 73
      ahel1=ahe1*conv/cc
      ahe3=-39.0                    !-39 (3), -11
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(50)*ahel3
      bareamp(1,2) = hpar(51)*ahel1
      bareamp(1,3) = hpar(52)*ahel0
      endif
      if (wpw.eq.'P31') then 
c 1st p31
      ams=1750.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=-31.00                    ! -31 (???)
      ahel1=ahe1*conv/cc
      ahe3=0.                        !  ?? (???)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,2) = hpar(53)*ahel1
      bareamp(1,3) = hpar(54)*ahel0
      endif
      if (wpw.eq.'P33') then 

      qs     = -q2
      betyn  = 0.154d0/1.d6*scale**2
      gamyn  = 0.166d0/1.d6*scale**2
      cut   = 0.71d0/scale**2*1.d6
      gdd   = 1.d0/(1.d0 - qs/cut)**2
      gdelt3 = gdd*(1.d0 + betyn*(-qs))*exp(-gamyn*(-qs))

      gc   = -0.238d0*gdelt3
      gm   = 1.85d0*gdelt3
      ge   = 0.025d0*gdelt3
      fdel = 1238
      enuc = sqrt(fnuc**2 + egam**2)
      eee  = sqrt(4.d0*pi/137.d0)
      ahelc  = - 4.d0*eee*gc*(fdel+fnuc)/(2.d0*fnuc)
     &   /((fdel+fnuc)**2 + q2)/((fdel-fnuc)**2 + q2)
     &  *egam**4*wcm/sqrt(2.d0*enuc*(enuc+fnuc))

      fact1 = eee*(fdel+fnuc)/fnuc*wcm*egam/((fdel+fnuc)**2+q2)
     &      *sqrt((enuc+fnuc)/enuc)
      fact2 = egam**2*4.d0*wcm/(enuc+fnuc)/((fdel-fnuc)**2+q2)
      ahel3 = fact1*sqrt(3.d0)/2.d0*(-gm+ge-ge*fact2)
      ahel1 = fact1/2.d0*           (-gm+ge+ge*fact2)
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3
c
c variations with respect to original SL
c firs
      bareamp(1,1) = ahel3*hpar(55)
      bareamp(1,2) = ahel1*hpar(56)
      bareamp(1,3) = ahelc*hpar(57)
c second P33
      ams=1600.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=-26.00                    ! -26 (20)
      ahel1=ahe1*conv/cc
      ahe3=-6.00                     ! -6. (17)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(2,1) = hpar(58)*ahel3
      bareamp(2,2) = hpar(59)*ahel1
      bareamp(2,3) = hpar(60)*ahel0
      end if
      if (wpw.eq.'D13') then 
c 1st d13
      ams=1520.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=-28.00                    ! -28 (2), -27
      ahel1=ahe1*conv/cc
      ahe3=143.00                    ! 143 (2), 161
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(61)*ahel3
      bareamp(1,2) = hpar(62)*ahel1
      bareamp(1,3) = hpar(63)*ahel0
c 2nd d13
      ams=1700.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=17.00                    !  17 (12)
      ahel1=ahe1*conv/cc
      ahe3=2.00                     !   2 (20)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3

      bareamp(2,1) = hpar(64)*ahel3
      bareamp(2,2) = hpar(65)*ahel1
      bareamp(2,3) = hpar(66)*ahel0
      endif
      if (wpw.eq.'D15') then 
c 1st d15
      ams=1675.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=18.00                    ! 18 (2), 15
      ahel1=ahe1*conv/cc
      ahe3=21.00                    ! 21  (1), 22
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(67)*ahel3
      bareamp(1,2) = hpar(68)*ahel1
      bareamp(1,3) = hpar(69)*ahel0
      endif
      if (wpw.eq.'D33') then 
c 1st d33
      ams=1700.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=125.00                    ! 125 (3)
      ahel1=ahe1*conv/cc
      ahe3=105.00                     ! 105 (3)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(70)*ahel3
      bareamp(1,2) = hpar(71)*ahel1
      bareamp(1,3) = hpar(72)*ahel0
      endif
      if (wpw.eq.'D35') then 
c 1st d35
      ams=1935.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=-15.00                    ! -15 (17)
      ahel1=ahe1*conv/cc
      ahe3=-10.00                     ! -10 (22)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(73)*ahel3
      bareamp(1,2) = hpar(74)*ahel1
      bareamp(1,3) = hpar(75)*ahel0
      endif
      if (wpw.eq.'F15') then 
c 1st f15
      ams=1680.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=-17.00                    !  -17 (1), -25
      ahel1=ahe1*conv/cc
      ahe3=134.0                     ! 134  (2), 134
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(76)*ahel3
      bareamp(1,2) = hpar(77)*ahel1
      bareamp(1,3) = hpar(78)*ahel0
      endif
      if (wpw.eq.'F17') then 
c 1st f17
      ams=1990.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=1.00                    !  1 (40)
      ahel1=ahe1*conv/cc
      ahe3=4.00                     ! 4 (26)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(79)*ahel3
      bareamp(1,2) = hpar(80)*ahel1
      bareamp(1,3) = hpar(81)*ahel0
      endif
      if (wpw.eq.'F35') then 
c 1st f35
      ams=1905.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=21.00                    ! 21 (4)
      ahel1=ahe1*conv/cc
      ahe3=-46.00                     !-46 (5)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(82)*ahel3
      bareamp(1,2) = hpar(83)*ahel1
      bareamp(1,3) = hpar(84)*ahel0
      endif
      if (wpw.eq.'F37') then 
c 1st f37
      ams=1950.0
      qq0=(ams**2-fnuc**2)/2./ams
      en=sqrt(fnuc**2+qq0**2)
      cc=sqrt(en/fnuc)/sqrt(2.*qq0)
      conv=sqrt(1./1000.d00)*1.d-03
      ahe1=-85.00                    ! -85 (17)
      ahel1=ahe1*conv/cc
      ahe3=-101.00                     ! -101. (14)
      ahel3=ahe3*conv/cc
      ahe0=0.
      ahel0=ahe0*conv/cc
      if(iwrite.eq.1)write(900,1123)wpw,ahel1,ahel3,ahe1,ahel3
      bareamp(1,1) = hpar(85)*ahel3
      bareamp(1,2) = hpar(86)*ahel1
      bareamp(1,3) = hpar(87)*ahel0
      endif
c        print*,wpw,bareamp(1,1),bareamp(1,2),bareamp(1,3)
      icount=icount+1
      if(icount.eq.15) iwrite=0 
      if(icount.gt.15)icount=100
      return
      end


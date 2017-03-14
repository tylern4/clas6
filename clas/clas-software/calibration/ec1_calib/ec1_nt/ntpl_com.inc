c
      integer*4 maxpages
      
      parameter ( maxpages=20000 )
c      parameter ( mx=24, my=40)
      
      common /QUEST/IQUEST(100)
      common /pawc/ipawc(maxpages*128)
      integer *4 ipawc
      integer *4 IQUEST
      	
      REAL    *4 deltaT,deltaN
      integer *4 Ntime
	      
      integer *4 dtype
      integer *4 Npmt
      integer *4 Ifile
      integer *4 n_events
      integer *4 nstory
      integer *4 spar_ped
      integer *4 ishift
      integer *4 attlen
      logical    ped_substr
      common     /EC1param/ped_substr, dtype, Npmt ,ifile , nstory, n_events
     &                     ,spar_ped, ishift, attlen
      integer *4 id
      LOGICAL    good_sect
      INTEGER *4 Nhits
      REAL    *4 tot_ADC_in, tot_ADC_out , sumE
      
      common /EC1_res/Nhits(2,4),id(2,4),sumE(2,4),
     &                tot_ADC_in(2) ,tot_ADC_out(2), good_sect(2)
      common /EC1time/Ntime,deltaT(200000),deltaN(200000)

      LOGICAL  EOF, FirstTime
      INTEGER*4 firstNum,nevent,nevent0,time,time0,time1,dtime
      
      common /EC1numb/firstNum,nevent,nevent0,time,time0,time1,dtime,EOF, FirstTime
      
      real *4 lambda
      common /EC1attl/lambda(2,4,40)

c
c calibration constatnts      
c
      real *4 A_l , A_r , da_l , da_r ,cT_l ,cT_r , dT_l , dT_r
      common /calib_const/  A_l(2,4,40), A_r(2,4,40),
     &                     da_l(2,4,40),da_r(2,4,40),
     &                     cT_l(2,4,40),cT_r(2,4,40),
     &                     dT_l(2,4,40),dT_r(2,4,40)

c BMPR
      REAL*4   q_tot,q_tot_lv,tl_tu,current
      common   /BMPRnt/q_tot,q_tot_lv,tl_tu,current
c TRGS
      integer*4 clock_ug,fcup_ug,clock_g1,fcup_g1,clock_g2,fcup_g2
      common   /TRGSnt/clock_ug,fcup_ug,clock_g1,fcup_g1,clock_g2,fcup_g2
c HBTR
      REAL*4    x_hbtr     ,y_hbtr     ,z_hbtr ,
     &          px_hbtr    ,py_hbtr    ,pz_hbtr, q_hbtr   ,chi2_hbtr 
      integer*4 nHBTR, itr_hbtr 
      common   /HBTRnt/nHBTR, x_hbtr (20),  y_hbtr(20), z_hbtr(20),
     &                        px_hbtr(20), py_hbtr(20),pz_hbtr(20),
     &          q_hbtr(20), chi2_hbtr(20), itr_hbtr(20)
c TBTR
      REAL*4     x_tbtr  ,y_tbtr  ,z_tbtr  ,
     &          px_tbtr  ,py_tbtr ,pz_tbtr ,q_tbtr ,chi2_tbtr 
      integer*4 nTBTR, itr_tbtr, itr_hbt_tbtr 
      common   /TBTRnt/nTBTR, x_tbtr (20), y_tbtr(20), z_tbtr(20),
     &                        px_tbtr(20),py_tbtr(20),pz_tbtr(20),
     &          q_tbtr(20),chi2_tbtr(20),itr_tbtr(20),itr_hbt_tbtr(20)
c HDPL      
      REAL*4    X_HDPL , Y_HDPL,  Z_HDPL, 
     &          CX_HDPL ,CY_HDPL, CZ_HDPL,  TLEN_HDPL           
      integer*4 nHDPL, sectHDPL , TRK_PLN_HDPL
      common   /HDPLnt/nHDPL,sectHDPL(200),TRK_PLN_HDPL(200), 
     &                 X_HDPL(200),Y_HDPL(200),Z_HDPL(200),
     &                 CX_HDPL(200),CY_HDPL(200),CZ_HDPL(200),
     &                 TLEN_HDPL(200)
c TDPL      
      REAL*4    X_TDPL , Y_TDPL,  Z_TDPL,
     &          CX_TDPL ,CY_TDPL, CZ_TDPL,  TLEN_TDPL           
      integer*4 nTDPL, sectTDPL ,TRK_PLN_TDPL
      common   /TDPLnt/nTDPL,sectTDPL(200),TRK_PLN_TDPL(200), 
     &                 X_TDPL(200),Y_TDPL(200),Z_TDPL(200),
     &                 CX_TDPL(200),CY_TDPL(200),CZ_TDPL(200),
     &                 TLEN_TDPL(200)

c RAW TRACK
      REAL*4    X, Y, Z, V_CX, V_CY, V_CZ,chi2,pmom,th,ph
      integer*4 nTRK,charge,trk_num
      common   /rawtrack/ nTRK,   X(20),    Y(20),    Z(20),
     &                    V_CX(20), V_CY(20), V_CZ(20),pmom(20),charge(20),
     &                    th(20), ph(20), chi2(20), trk_num(20)

c LAC track
      REAL*4     x_lac, y_lac, z_lac, trk_len
      INTEGER*4  lac_track,ntrk_lac,RF1,RF2
      common  /lactrk/lac_track(2) ,ntrk_lac ,RF1,RF2,
     &                x_lac(2), y_lac(2), z_lac(2), trk_len(2)
      
c EVNT track
      REAL*4     pmomEVNT,massEVNT,cxEVNT,cyEVNT,czEVNT,xEVNT,yEVNT,zEVNT
      INTEGER*4  nEVNT,idEVNT,chargeEVNT
      common  /EVNTnt/nEVNT,idEVNT(20),pmomEVNT(20),massEVNT(20)
     &               ,chargeEVNT(20),cxEVNT(20)
     &               ,cyEVNT(20),czEVNT(20),xEVNT(20),yEVNT(20),zEVNT(20)


c MCTK 
      REAL*4     pmomMCTK,chargeMCTK,V_MC_CX,V_MC_CY,V_MC_CZ
      INTEGER*4  nMCTK,idMCTK
      common  /MCTKnt/nMCTK,idMCTK(10),pmomMCTK(10)
     &               ,chargeMCTK(10)
     &               ,V_MC_CX(10),V_MC_CY(10),V_MC_CZ(10)


!
 TABLE  PTDB  ! create write display delete !Polarized Target Data Bank
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
1   PBeam       I    0       10000      ! beam polarization (% x 100)
2   PB_DATE     I   100  100000000      ! date of measurement
3   TTYPE       I    0         100      ! Target Type 
4   B_TARG      I    0         600      ! Target holding field (Tesla x100)
5   PTarg       I    0       10000      ! Target polarization
6   PT_time     I   100  100000000      ! Time measured (according to PC)
7   HE_Level    I    0       10000      ! Liquid Helium Level(% x 100)
8   EIOF        I    0      140000      ! EIO microwave tube frequency (MHz)
9   TTempH      I    0       30000      ! Target temp measured by He Cell (K x100)
10  TTempC      I    0       30000      ! Target temp measured by Cernox (K x100)
11  AnealT      I   100  100000000      ! Date of last target anealing
!
 END TABLE
!

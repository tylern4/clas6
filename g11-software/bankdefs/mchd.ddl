!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   MCHD      ! create write display delete ! MC (generator) HeaD Bank
!
!  comments: REACTYPE allows to specify the reaction 
!                     (e.g. resonance cascade in celeg)
!            PX...PZ_TARG !=0 for fermi motion of target part.
!
!COL ATT-name FMT Min    Max   !Comments
   1 NRUN     I    1      1000 ! Run Number
   2 NEVENT   I    1 100000000 ! Event Number
   3 TIME     I  100 100000000 ! Event Time (UNIX time)
   4 TYPE     I    0     -1000 ! Event Type (MUST be negative)
                               !            < 0 Monte Carlo Events:
                               !                 = -1 - SDA
                               !                 = -2 - GEANT
                               !                 = -3 - Celeg
                               !                 = -4 - ao_gen
                               !                 ....
   5 REACTYPE  I    0  100000   ! optional: user defined reaction type
   6 WEIGHT    F   0.      1.   ! event weight
   7 W         F   0.     15.   ! center_of_mass energy (inv.mass of hadronic states)
   8 Q2        F   0.     30.   ! Q2 (photon virtuallity)
   9 E_PHOT    F   0.     15.   ! energy of (real/virtual) photon
  10 PX_PHOT   F   0.     15.   ! p_x: momentum (in x) of (real/virtual) photon
  11 PY_PHOT   F   0.     15.   ! p_y: momentum (in y) of (real/virtual) photon
  12 PZ_PHOT   F   0.     15.   ! p_z: momentum (in z) of (real/virtual) photon
  13 E_TARG    F   0.     15.   ! energy of target particle
  14 PX_TARG   F   0.     15.   ! p_x: momentum (in x) of target part.
  15 PY_TARG   F   0.     15.   ! p_y: momentum (in y) of target part.
  16 PZ_TARG   F   0.     15.   ! p_z: momentum (in z) of target part.
!
 END TABLE
!
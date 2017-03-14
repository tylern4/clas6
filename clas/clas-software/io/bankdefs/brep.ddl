!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  BREP          ! create write display delete ! Begin run epics values
!
!   ATTributes:
!   -----------
!COL ATT-name                 FMT       Min    Max     ! Comments
!
  1  MBSY2C_energy    F    -999999.0 999999.0 ! beam energy
  2  IGT0I00BIASET    F    -999999.0 999999.0 ! thermionic gun
  3  IGL1I00DAC2      F    -999999.0 999999.0 ! B polarized gun
  4  SMRPOSA          F    -999999.0 999999.0 ! A slit position
  5  SMRPOSB          F    -999999.0 999999.0 ! B slit position
  6  SMRPOSC          F    -999999.0 999999.0 ! C slit position
  7  Harp             F    -999999.0 999999.0 ! harp
  8  hallb_sf_xy560   F    -999999.0 999999.0 ! torus current
  9  MTSETI           F    -999999.0 999999.0 ! Mini set current
 10  MTIRBCK          F    -999999.0 999999.0 ! Mini current readback
 11  MTVRBCK          F    -999999.0 999999.0 ! Mini voltage readback
 12  TMSETI           F    -999999.0 999999.0 ! Tagger set current
 13  TMIRBCK          F    -999999.0 999999.0 ! Tagger current readback
 14  TMVRBCK          F    -999999.0 999999.0 ! Tagger voltage readback
 15  Cryo_pressure    F    -999999.0 999999.0 ! cryotarget pressure
 16  Cryo_temperature F    -999999.0 999999.0 ! cryotarget temperature
 17  Cryo_status      F    -999999.0 999999.0 ! cryotarget status
 18  VCG2C24          F    -999999.0 999999.0 ! upstream beam vacuum
 19  VCG2H01          F    -999999.0 999999.0 ! target vacuum
 20  scalerS2o        F    -999999.0 999999.0 ! Halo UP upstream
 21  scalerS3o        F    -999999.0 999999.0 ! Halo DOWN upstream
 22  scalerS4o        F    -999999.0 999999.0 ! Halo LEFT upstream
 23  scalerS5o        F    -999999.0 999999.0 ! Halo RIGHT upstream
 24  scalerS6o        F    -999999.0 999999.0 ! Halo UP downstream
 25  scalerS7o        F    -999999.0 999999.0 ! Halo DOWN downstream
 26  scalerS8o        F    -999999.0 999999.0 ! Halo LEFT downstream
 27  scalerS9o        F    -999999.0 999999.0 ! Halo RIGHT downstream
 28  IPM2H01_XPOS     F    -999999.0 999999.0 ! bpm 1 x
 29  IPM2H01_YPOS     F    -999999.0 999999.0 ! bpm 1 y
 30  IPM2H01          F    -999999.0 999999.0 ! bpm 1 current
 31  IPM2C24A_XPOS    F    -999999.0 999999.0 ! bpm 2 x
 32  IPM2C24A_YPOS    F    -999999.0 999999.0 ! bpm 2 y
 33  IPM2C24A         F    -999999.0 999999.0 ! bpm 2 current
 34  IPM2C22A_XPOS    F    -999999.0 999999.0 ! bpm 3 x
 35  IPM2C22A_YPOS    F    -999999.0 999999.0 ! bpm 3 y
 36  IPM2C22A         F    -999999.0 999999.0 ! bpm 3 current
!
 END TABLE
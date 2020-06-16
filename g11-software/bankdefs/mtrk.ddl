!       BANKname BANKtype      ! Comments
 TABLE  MTRK  ! create write display delete ! vertex Result bank
!
! THE COORDINATE SYSTEM USED IN THIS BANK IS THE DETECTOR COORDINATE SYSTEM.
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  sect        I       0    1000   ! Sector Number for track
  2  trkl1       I       0      10   ! track segment and track cluster for Superlayer 1 (lnk_t)
  3  trkl2       I       0      10   ! track segment and track cluster for Superlayer 2
  4  trkl3       I       0      10   ! track segment and track cluster for Superlayer 3
  5  trkl4       I       0      10   ! track segment and track cluster for Superlayer 4
  6  trkl5       I       0      10   ! track segment and track cluster for Superlayer 5
  7  trkl6       I       0      10   ! track segment and track cluster for Superlayer 6
!
! typedef struct {
!  short segm;
!  short clust;
!} lnk_t;
!
!typedef struct {
!  unsigned int sect;
!  lnk_t data[6];
!}mtrk_t;
!
!typedef struct {
!  bankHeader_t bank;
!  mtrk_t mtrk[1];
!} clasMTRK_t;
!     
!
 END TABLE

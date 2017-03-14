!
 TABLE  CLST  ! Cluster bank
!
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  Clust     I       0     65536  ! bit packed,  see: include/bosddl.h, clasCLST_t
! Bit packed data structure follows:
! typedef struct {
!  unsigned int layer    : 4;
!  unsigned int clustern : 4;
!  unsigned int csegment : 5;
!  unsigned int cclust   : 4;
!  unsigned int clustp   : 15;
!} clst_t;
!
!typedef struct {
!  bankHeader_t bank;
!  clst_t clst[1];  
!} clasCLST_t;
!
!     
!
 END TABLE

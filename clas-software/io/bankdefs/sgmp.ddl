 TABLE  SGMP  ! SEGMENTs BANK
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max    ! Comments
!
  1  is         I       0      10   ! superlayer number of the segment
  2  sgm1       I       0   65356   ! data structure see: include/bosddl.h, wire_t
  3  sgm2       I       0   65536   ! wire_t
  4  sgm3       I       0   65536   ! wire_t
  5  sgm4       I       0   65536   ! wire_t
  6  sgm5       I       0   65356   ! wire_t
  7  sgm6       I       0   65356   ! wire_t
!
!typedef struct {
!  short wireid;
!  short time;
!} wire_t;
!
!typedef struct {
!  int is;
!  wire_t data[6];
!} sgmp_t;
!
!typedef struct {
!  bankHeader_t bank;
!  sgmp_t sgmp[1];
!} clasSGMP_t;
!
!
 END TABLE

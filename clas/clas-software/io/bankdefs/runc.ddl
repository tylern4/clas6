!       BANKname BANKtype      ! Comments
 TABLE  RUNC  ! create write display ! Run Control Map information
!
! Hit Based Tracking Particle ID
!
!   ATTributes:
!   -----------
!COL ATT-name FMT    Min     Max     ! Comments
!
  1  runno      I       1   999999   ! Run number extracted from map
  2  beam_e     F       1  99999.9   ! beam energy in MeV
  3  b_first    I       1  9999999   ! firsttime
  4  q_live     F       1  99999.9   ! Fcup * live time (1*E-10C)
  5  ql_first   I       1  9999999   ! firsttime	
  6  q_all      F       1  99999.9   ! Fcup (1*E-10C)
  5  qa_first   I       1  9999999   ! firsttime	
  6  tor_curr   F       1  99999.9   ! Torus Current (A)
  7  t_first    I       1  9999999   ! firsttime	
  8  mtor_curr  F       1  99999.9   ! Minitorus Current (A)
  9  mt_first   I       1  9999999   ! firsttime	
  8  tag_curr   F       1  99999.9   ! Tagger Current (A)
 10  tag_first  I       1  9999999   ! firsttime	
!
! see bosddl.h, clasRUNC_t containing 
! typedef struct { float val, int stat} runc_item_t;
!
 END TABLE
!

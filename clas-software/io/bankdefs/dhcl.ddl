!       BANKname BANKtype      ! Comments
 TABLE  DHCL       B08  ! create write display delete ! drift chamber hit cluster
!
! record# = Sector
!  all hit clusters within a superlayer are reported, 
!  those that are used in tracks are marked:  BTRK=2**(track_in_sector-1)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  SLY      I     1       6  ! superlayer
  2  BTRK     I     0     255  ! track_in_sector# (bit set)
  3  TRKS1    I     0     255  ! combinations to track segments
  4  TRKS2    I     0     255  !   "             "       "
  5  WIRE1    I     1     192  ! 1.wire# in this cluster in 1.layer
  6  BWIR1    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
  7  WIRE2    I     1     192  ! 1.wire# in this cluster in 1.layer
  8  BWIR2    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
  9  WIRE3    I     1     192  ! 1.wire# in this cluster in 1.layer
 10  BWIR3    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
 11  WIRE4    I     1     192  ! 1.wire# in this cluster in 1.layer
 12  BWIR4    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
 13  WIRE5    I     1     192  ! 1.wire# in this cluster in 1.layer
 14  BWIR5    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
 15  WIRE6    I     1     192  ! 1.wire# in this cluster in 1.layer
 16  BWIR6    I     0     255  ! hits in this layer (starting from WIRE1) (bit st)
!
 END TABLE
!

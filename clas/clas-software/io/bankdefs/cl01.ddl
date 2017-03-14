!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  CL01    ! create write display delete ! Catch-ALL event bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ac_amp       I     0   65536  ! 60 Hz AC amplitude (pedestal subtracted)
  2  fc_diode_amp I     0   65536  ! Forward Carriage diode amplitude (ped sub.)
  3  fc_diode_t   F     0   65536  ! Forward Carriage diode time
  4  nc_diode_amp I     0   65536  ! North Clamshell diode amplitude (ped sub.)
  5  nc_diode_t   F     0   65536  ! Forward  diode time
  6  sc_diode_amp I     0   65536  ! Forward Carriage diode amplitude (ped sub.)
  7  sc_diode_t   F     0   65536  ! Forward Carriage diode time(ns)
  8  sf_diode_amp I     0   65536  ! Forward Carriage diode amplitude (ped sub.)
  9  sf_diode_t   F     0   65536  ! Forward Carriage diode time(ns)
 10  rf1	  F     0   65536  ! RF time 1 (ns)
 11  rf2	  F     0   65536  ! RF time 2 (ns)
 12  rf	 	  F     0   65536  ! GOOD RF time (ns)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!

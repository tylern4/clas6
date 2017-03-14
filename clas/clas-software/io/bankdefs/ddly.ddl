!-------------------------------------------------------------------------------
 TABLE  DDLY  ! create   delete ! Drift chamber time DeLaY bank
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  ID         I    257 9408   ! ID_wire 
  2  TIDLY      F  -500. 500.   ! TIme DeLaY (ns) 		 
  3  STAT       I      0  900   ! wire STATus = 100*C + 10*A + B
                                ! A: 1 - good wire
                                !    2 - bad wire no data both on pulsing in & out
                                !    3 - bad wire no data on pulsing out only
                                !    4 - bad wire no data on pulsing in only
                                !    5 - good wire warning both on pulsing in & out
                                !    6 - good wire warning on pulsing out only
                                !    7 - good wire warning on pulsing in only
                                ! B: Information on slope_out - slope_in
                                !   1 - between   10% and 100% (negative status)
                                !   0 - between    1% and  10%
                                !   1 - between   .1% and   1%
                                !   2 - between  .01% and  .1%
                                !   3 - between .001% and .01%
                                ! C:Information on bad wires
                                !   0 - no know problem
                                !   1 - dead : isolated
                                !   2 - dead : muxed pair
                                !   3 - dead : HV single layer
                                !   4 - hot  : isolated
                                !   5 - hot  : muxed pair
                                !   6 - dead : signal cable disconnect
                                !   7 - dead : consequative mux channels 
                                !   8 - dead : HV multiple layers
                                !   9 - low gain
!
 END TABLE
!
 ARRAY JW

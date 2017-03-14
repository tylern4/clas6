*     This include file defines some numbers needed in the code.
*     It's in a separate file to make the main code more readable.

*     Could have used data statements, but this is more readable.

*     Define the perpendicular unit vectors to each sector,
*     via perp(sector,xyz)

      perp(1,1)=0.423
      perp(1,2)=0.0
      perp(1,3)=0.906
      
      perp(2,1)=0.212
      perp(2,2)=0.366
      perp(2,3)=0.906
      
      perp(3,1)=-0.212
      perp(3,2)=0.366
      perp(3,3)=0.906
      
      perp(4,1)=-0.423
      perp(4,2)=0.0
      perp(4,3)=0.906
      
      perp(5,1)=-0.212
      perp(5,2)=-0.366
      perp(5,3)=0.906
      
      perp(6,1)=0.212
      perp(6,2)=-0.366
      perp(6,3)=0.906
      
c     The following is the list of varibles being retrieved
c     from the ntuple
      
      char_var(1) = 'npart'
      char_var(2) = 'tr_time'
       char_var(3) = 'rf_time1'
       char_var(4) = 'gpart'
       char_var(5) = 'id'
       char_var(6) = 'dc'
       char_var(7) = 'cc'
       char_var(8) = 'sc'
       char_var(9) = 'ec'
       char_var(10) = 'p'
       char_var(11) = 'q'
       char_var(12) = 'b'
       char_var(13) = 'cx'
       char_var(14) = 'cy'
       char_var(15) = 'cz'
c
      char_var(16) = 'ec_part'
      char_var(17) = 'ec_sect'
      char_var(18) = 'ec_ei'
      char_var(19) = 'ec_eo'
      char_var(20) = 'ec_t'
      char_var(21) = 'ech_x'
      char_var(22) = 'ech_y'
      char_var(23) = 'ech_z'
c
      char_var(24) = 'sc_part'
      char_var(25) = 'sc_sect'
      char_var(26) = 'sc_pd'
      char_var(27) = 'sc_t'
c
      char_var(28) = 'dc_part'
      char_var(29) = 'dc_sect'
      char_var(30) = 'dc_stat'
      char_var(31) = 'dc_xsc'
      char_var(32) = 'dc_ysc'
      char_var(33) = 'dc_zsc'
      char_var(34) = 'dc_cxsc'
      char_var(35) = 'dc_cysc'
      char_var(36) = 'dc_czsc'
      char_var(37) = 'dc_xec'
      char_var(38) = 'dc_yec'
      char_var(39) = 'dc_zec'
c
      char_var(40) = 'ecpc_nhit'
      char_var(41) = 'ecpc_hit'
      char_var(42) = 'ecpc_id'
      char_var(43) = 'ecpc_tdc'
      char_var(44) = 'ecpc_adc'
      






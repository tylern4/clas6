!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   PHTM   B32   ! create write display delete ! Photon_mon bank for mySQL
!
!COL ATT-name FMT Min    Max   !Comments
   1 EVID     I    1   1000000 ! Event ID (number of triggers)
   2 Nsprot   I    1   1000000 ! Number of single proton events
   3 Npip     I    1   1000000 ! Number of single pi+ events
   4 Npim     I    1   1000000 ! Number of single pi- events
   5 Nppippim I    1   1000000 ! Number of single proton pi+ pi- events
   6 Nppip    I    1   1000000 ! Number of single proton pi+ events
   7 Npippim  I    1   1000000 ! Number of single pi+ pi- events
   8 Nppim    I    1   1000000 ! Number of single proton pi- events
   9 Nkp      I    1   1000000 ! Number of single K+ events
  10 Npkp     I    1   1000000 ! Number of single proton K+ events
  11 tag_pi_v F   -9999. 9999. ! Mean of (tagger_time - pion_vertex_time)
!
 END TABLE

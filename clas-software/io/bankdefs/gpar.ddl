!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  GPAR          ! create write display delete !  input parameter bank 
!     for gsim ( record_no.=0 )
!     or event generators ( record_no.>0 )
!
!  save input parameters (float OR int values (char as ascii))
!  (special codes:  fval=-1111.0 -> ival=logical(-1=T,0=F);
!                   fval=1111.0  -> ival=ascii; 
!                   fval=1112.0  -> ival=execution time (name=ascii time))
!                   fval=1114.0  -> ival=creation time  (name=ascii time);
!  (first 10 entries: program_name (incl. path)  [ival='PGM1'...'PGM5'];
!                     cvs tag version            [ival='TAG '];
!                     creation time              [fval=1114.0; ival=unix time];
!                     execution time             [fval=1112.0; ival=unix time];
!                     host name                  [ival='HOST'];
!                     user name                  [ival='USER'] )
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  fval     F  -1000000.    10000000.  ! float value
  2  ival     I  -1000000     10000000   ! int value 
  3  char1    A    0          255       ! char[20] name;
  4  char2    A    0          255       ! 
  5  char3    A    0          255       ! 
  6  char4    A    0          255       ! 
  7  char5    A    0          255       ! 
!
 END TABLE

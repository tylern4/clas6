!********************************************************************
!       BANKname BANKtype      !Comments
 TABLE  TRUE ! create write display delete ! Thrown parameter for particles in Geant4
!
!COL ATT-name FMT Min    Max   !Comments
   1 PID     F     0     100.  ! PARTICLE TYPE, COVERT IT TO INT BEFORE  USING
                               ! -name-      -pid-  -mass-
                               ! Gamma 	  	1   0.0			
                               ! Positron  	2   0.00051099906  	
                               ! Electron  	3   0.00051099906 	
                               ! Neutrino 	4   0.0  	    	
                               ! Muon + 	5   0.105658389   	
                               ! Muon -	  	6   0.105658389   	
                               ! Pion 0	  	7   0.1349764      	
                               ! Pion +	  	8   0.1395700      	
                               ! Pion -	  	9   0.1395700     	
                               ! Kaon 0 long  	10  0.497672   		
                               ! Kaon +	 	11  0.493677  		
                               ! Kaon - 	12  0.493677  		
                               ! Neutron  	13  0.93956563  	
                               ! Proton   	14  0.93827231  	
                               ! Kaon 0 short  	16  0.497672  		
                               ! Omega    	24  1.67245  		
   2 X0       F  -100.  100.   ! initial position x (cm)
   3 Y0       F  -100.  100.   ! initial position y (cm)
   4 Z0       F  -100.  100.   ! initial position z (cm)
   5 E0       F   0.    10.    ! initial total energe E (GeV)	
   6 Px0      F  -10.   10.    ! initial momentum px (GeV/c)
   7 py0      F  -10.   10.    ! initial momentum py (GeV/c)
   8 Pz0      F  -10.   10.    ! initial momentum pz (GeV/c)
   9 q        F  -4.    4.     ! charge  q (e)
  10 X1       F  -20.   20.    ! RTPC boundary position x (cm)
  11 Y1       F  -20.   20     ! RTPC boundary position y (cm)
  12 Z1       F  -20.   20.    ! RTPC boundary position z (cm)
  13 E1       F   0.    10.    ! RTPC boundary Total energy E (GeV)
  14 Px1      F  -10.   10     ! RTPC boundary momentum px (GeV/c)
  15 py1      F  -10.   10.    ! RTPC boundary momentum py (GeV/c)
  16 Pz1      F  -10.   10.    ! RTPC boundary momentum pz (GeV/c)
  17 R        F   0.   100000. ! radius of simulated track R (cm) 
!   
 END TABLE
!
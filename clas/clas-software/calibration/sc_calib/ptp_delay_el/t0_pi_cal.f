C-
C- Make the ntuple for crude tuning
C-
	real function t0_pi_cal()
C-	
	include ?
C-
C-  Array DT_S - Result from the fine tuning
C-
	vector DT_S(48,6)
	vector E_BM(1)		
*
*--   Enter user code here
*
C-
	parameter ( CalConst_RF  =  1. )
	parameter ( CalConst_TOF = 1. )
	parameter ( RF_Period = 2.004 )	
C-
	character*6 VarName(5) 
	data VarName /        'Sect_e' , 'Strp_e',
     &	                      'Sect_h' , 'Strp_h',
     &	                      'D_T' /
     	real XNtuple(5)
C-
	real E_b, M_n, M_pi
	parameter ( M_n = 0.939 )
	parameter ( M_pi = 0.1396 )
C
	logical FirstTime
	logical TBT 
	data FirstTime / .TRUE. /
C
	real E_dep_e, E_dep_h
	real TDC_e, TOF_e, TDC_h, TOF_h, TOF_H_1
	real Mass_h, Beta_h, Path_h, Mass_h2
	real iSect_e, iSect_h
	real iStrip_e, iStrip_h
	real DeltaT, CT
	real DE_DX_CUT_PR, DE_DX_CUT_PI, DE_DX_CUT_EL
C-
	if ( FirstTime ) then
	 E_b = E_BM(1)	
	 write(6,*)'Booking Histograms...'
	 if ( .NOT.HEXIST(30) ) then
	  call HBOOKN(30, 'NTUPLE', 5 , '//LUN44',1000, VarName)
	 endif 	 
     	 write(6,*) 'Booking Done . '
	endif
	FirstTime = .FALSE.
	TBT = .FALSE.
C-
	if ( (gpart .GE. 1) .and. ( dc(1) .EQ. 0. ) ) then
	 tof_cal = 1.
         return	
     	endif	
C-     	
	if ( (gpart .GE. 2) .and. (id(1) .EQ. 11) .and. 
     &	     (dc_stat(dc(1)) .GT. 0.1 ) .and.	 
     &	     (p(1) .GT. 0.01) .and. 
     &	     (p(1) .LT. E_b ) .and. 
     & 	     (sc(1) .GT. 0)  .and. 
     &       (q(1) .LT. 0.) ) then 
C-
C-	 It is an electron that has a hit in SC
C-
	 iSect_e = sc_sect( sc(1) )
	 iStrip_e = sc_pd( sc(1) ) 
	 E_Dep_e = edep( sc(1) )
	 TDC_e = sc_t( sc(1) ) * CalConst_TOF
	 Path_e = sc_r( sc(1) )	 
	 TOF_e = Path_e / 30.
C-
	 do iPrt = 2, gpart
	  if ( dc(iPrt) .EQ. 0. ) then 
	   TBT = .FALSE.
	  else
	   TBT = .TRUE. 
	  endif
	  if ( sc(iPrt) .GT. 0 .and.
     &	      TBT .and. 
     &        p(iPrt) .GT. 0.01 .and. 
     &        p(iPrt) .LT. E_b  
c     &	      .and. q(iPrt) .GT. 0.  
     &	       ) then
C-	  
	   Type = 0  
	   E_Dep_h = edep( sc(iPrt) )	   
	   iSect_h = sc_sect( sc(iPrt) )
	   iStrip_h = sc_pd( sc(iPrt) ) 
	   TDC_h = sc_t( sc(iPrt) ) * CalConst_TOF
	   Path_h = sc_r( sc(iPrt) )  
	   if ( iStrip_e .LE. 48 .and. iStrip_h .LE. 48 ) then
	    if (     E_Dep_h .GT. DE_DX_CUT_PR(p(iPrt)) 		
     &	      .and. E_Dep_h .GT. DE_DX_CUT_EL(p(iPrt))
     &	        ) then    
	     Beta_h = 1. / Sqrt(1. + (M_n/p(iPrt))**2 )   
	     Type = 1
	    elseif ( E_Dep_h .LT. DE_DX_CUT_PI(p(iPrt)) 
     &	       .and. E_Dep_h .GT. DE_DX_CUT_EL(p(iPrt))	  
     &	        ) then    
	     Beta_h = 1. / Sqrt(1. + (M_pi/p(iPrt))**2 )   
	     Type = 2  
	    endif 
	    if ( Type .EQ. 2 ) then 
c	     write(6,*) Beta_h
C-
C-	     Calculate the difference between target time for
C-	     elsectrons and pions as a number multiple of 2.004 ns. 
C-
  	     TOF_h = ( Path_h / Beta_h ) / 30.
	     DeltaT = -( TDC_h - TOF_h - DT_S(iStrip_h,iSect_h) 
     -         - ( Amod( TDC_h - TOF_h - DT_S(iStrip_h,iSect_h) -
     -          rf_time1*CalConst_RF + 100*RF_Period , RF_period ) - 
     -	        RF_Period/2. )
     -	      ) +
     + 	      ( TDC_e - TOF_e - DT_S(iStrip_e,iSect_e) 
     -         - ( Amod( TDC_e - TOF_e - DT_S(iStrip_e,iSect_e) -
     -          rf_time1*CalConst_RF + 100*RF_Period , RF_period  ) - 
     -          RF_Period/2. )
     &        )   
C-
	     XNtuple(1) = iSect_e
	     XNtuple(2) = iStrip_e
	     XNtuple(3) = iSect_h
	     XNtuple(4) = iStrip_h
	     XNtuple(5) = DeltaT
	     call HFN(30, XNtuple)
	    endif 
	   endif	   	   
C-	  
	  endif
	 enddo
C-	 


C-
	endif
	
C-
	t0_pi_cal=1.
	return
	end
C=================================================================
	real function DE_DX_CUT_PR(X)
	 DE_DX_CUT_PR = 19.7547-7.058824*X	
	return
	end  
C==================================================================
	real function DE_DX_CUT_PI(X)
	 DE_DX_CUT_PI = 17.-6.*X	
	return
	end  
C==================================================================
	real function DE_DX_CUT_EL(X)
	 DE_DX_CUT_EL = 6.0 - 0.*X	
	return
	end  
		

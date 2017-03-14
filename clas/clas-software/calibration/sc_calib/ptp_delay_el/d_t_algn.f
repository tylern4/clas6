	real function d_t_algn()
C-	
	include ?
C-
	parameter ( RF_Period = 2.004 )	
C-
	vector DT_EL(48,6)
	vector DT_SS(6)
C-
	d_t_algn = D_T - 
     -	RF_Period*( (DT_SS(Sect_e)+DT_EL(Strp_e,Sect_e)) -
     -              (DT_SS(Sect_h)+DT_EL(Strp_h,Sect_h)) ) 	
C-
	return
	end
	

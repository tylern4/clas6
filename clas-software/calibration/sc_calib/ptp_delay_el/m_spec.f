	real function m_spec()
	include ?
C-
	vector DT(48,6)
	vector Status(48,6)
C-
*
*--   Enter user code here
*

C-
	parameter ( CalConst_RF  =  1. )
	parameter ( CalConst_TOF =  1. )
	parameter ( RF_Period = 2.004 )	

C-
	character*8 VarName(16) 
	data VarName /        'Sect_e' , 'Strp_e',
     &	                      'Sect_h' , 'Strp_h',
     &	                      'P_h', 'Mass', 'W2',
     &			      'DE_DX', 'Stat_e',
     &			      'Stat_h', 'CT_e', 'CT_h',
     &	                      'MM', 'Beta_h', 'Th_h',
     &			       'T0_H' /
     	real XNtuple(16)
C-
	real E_b, M_n, M_pi
	parameter ( E_b = 1.51 )
	parameter ( M_n = 0.938 )
	parameter ( M_pi = 0.1396 )
C
	logical FirstTime
	logical TBT 
	data FirstTime / .TRUE. /
C
	real E_dep_e, E_dep_h
	real TDC_e, TOF_e, TDC_h, TOF_h, TOF_H_1, T_tg_h
	real Mass_h, Beta_h, Path_h, Mass_h2
	real iSect_e, iSect_h
	real iStrip_e, iStrip_h
	real DeltaT, DeltaT_RF, CT
	real DE_DX_CUT_PR, DE_DX_CUT_PI, DE_DX_CUT_EL
C-
	if ( FirstTime ) then
	 write(6,*)'Booking Histograms...'
c	 if ( .NOT.HEXIST(50) ) then
	  call HBOOKN(50, 'NTUPLE', 16 , '//LUN44',1000, VarName)
c	 endif 	 
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
     &	     (p(1) .GT. 0.2) .and. 
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
	 Px_e=p(1)*cx(1)
	 Py_e=p(1)*cy(1)
	 Pz_e=p(1)*cz(1)
c cut for Kaons
	 Q2 = 2.*E_b*p(1)*(1.-cz(1))
	 W2 = M_n**2+2*M_n*(E_b-p(1))-Q2
c	 if( W2 .LT. 1.6**2 ) then
c	  return
c	 endif
c
C-
	 do iPrt = 2, gpart
	  if ( dc(iPrt) .EQ. 0. ) then 
	   TBT = .FALSE.
	  else
	   TBT = .TRUE. 
	  endif
	  if ( sc(iPrt) .GT. 0 .and.
     &	      TBT .and. 
     &        p(iPrt) .GT. 0.001 .and. 
     &        p(iPrt) .LT. E_b  
     &	      .and. q(iPrt) .GT. 0.  
     &	       ) then
C-	  
	   Type = 0  
	   E_Dep_h = edep( sc(iPrt) )	   
	   iSect_h = sc_sect( sc(iPrt) )
	   iStrip_h = sc_pd( sc(iPrt) ) 
	   TDC_h = sc_t( sc(iPrt) ) * CalConst_TOF
	   Path_h = sc_r( sc(iPrt) )  
	   TOF_h = ( TDC_h -  DT(iStrip_h,iSect_h)) -
     - 	       ( TDC_e - TOF_e - DT(iStrip_e,iSect_e) 
     -         - ( Amod( TDC_e - TOF_e - DT(iStrip_e,iSect_e) -
     -      rf_time1*CalConst_RF + 100*RF_Period , RF_period  ) - 
     -       RF_period/2. )
     &	       )   
     	   Beta_h = Path_h / (30. * TOF_h )
     	   CT_e = ( Amod( TDC_e - TOF_e - DT(iStrip_e,iSect_e) -
     -      rf_time1*CalConst_RF + 100*RF_Period , RF_period  ) -
     -     RF_Period/2. )
c  
     	   Mass_h2 = p(iPrt)**2*( ( 30.0 * TOF_h 
     /	     / Path_h )**2-1.) 

     	   if ( Mass_h2 .GT. 0 ) then 
     	     Mass_h=Sqrt(Mass_h2) 
	   else
   	     Mass_h = -Sqrt(-Mass_h2)
	   endif 
	   if ( Mass_H .LT. 0.2 ) then
	    Beta_h_1 = 1. / Sqrt(1. + (M_pi/p(iPrt))**2 ) 
	    TOF_H_1 = ( Path_h / Beta_h_1 ) / 30.
     	    CT_h = ( Amod( TDC_h - TOF_h_1 - DT(iStrip_h,iSect_h) -
     -      rf_time1*CalConst_RF + 100*RF_Period , RF_period  ) - 
     -      RF_Period/2. )	    
	   else
	    CT_h=-3
	   endif     
	   if ( cz(iPrt) .GT. 0. ) then
	    Theta_h = Acos(cz(iPrt))*90./Acos(0.)
	   else
	    return
	   endif     	      
	   Px_h=p(iPrt)*cx(iPrt)
	   Py_h=p(iPrt)*cy(iPrt)
	   Pz_h=p(iPrt)*cz(iPrt)
	   if ( Mass_h .LT. 0.65 .and. Mass_h.GT.0.33 ) then
	    aMassP = 0.493677
	   elseif ( Mass_h .LT. 1.8 .and. Mass_h.GT.0.65 ) then 
	    aMassP = 0.938
           elseif ( Mass_h .LT. 0.33 ) then
	    aMassP = 0.1396
	   endif
	   aMM2  = ( E_b+M_n - p(1) - Sqrt(p(iPrt)**2+aMassP**2) )**2-
     -	    ( ( Px_e+Px_h)**2+(Py_e+Py_h)**2+(E_b-(Pz_e+Pz_h))**2)
	   if ( aMM2 .GT. 0. ) then 
            aMM = Sqrt(aMM2)
	   else
	    aMM = -Sqrt(-aMM2)
	   endif
	   Beta_h_ID = 1. / Sqrt(1. + (aMassP/p(iPrt))**2 )   
	   TOF_H_B = ( Path_h / Beta_h_ID ) / 30.
	   T_tg_h = ( TDC_h - TOF_H_B - DT(iStrip_h,iSect_h)) -
     - 	       ( TDC_e - TOF_e - DT(iStrip_e,iSect_e) 
     -         - ( Amod( TDC_e - TOF_e - DT(iStrip_e,iSect_e) -
     -      rf_time1*CalConst_RF + 100*RF_Period , RF_period  ) - 
     -       RF_period/2. )
     &	       )  
C-
	   XNtuple(1) = iSect_e
	   XNtuple(2) = iStrip_e
	   XNtuple(3) = iSect_h
	   XNtuple(4) = iStrip_h
	   XNtuple(5) = p(iPrt)
	   XNtuple(6) = Mass_h2
c	   write(6,*)Mass_h**2
	   XNtuple(7) = W2
	   XNtuple(8) = E_Dep_h
	   XNtuple(9) =  Status(iStrip_e,iSect_e)
	   XNtuple(10) = Status(iStrip_h,iSect_h)
	   XNtuple(11) = CT_E
	   XNtuple(12) = CT_h	
	   XNtuple(13) = aMM
	   XNtuple(14) = Beta_h
	   XNtuple(15) = Theta_h
	   XNtuple(16) = T_tg_h
	   call HFN(50, XNtuple)
C-	  
	  endif
	 enddo
C-	 


C-
	endif
	
C-
	m_spec=1.
	return
	end
C=================================================================

      SUBROUTINE fill_ntpl(N_good_event)
      
      IMPLICIT NONE  
      
      include "ntpl_com.inc"
      include "EC1nt.inc"
      
      integer N_good_event
      integer mid_st(2,4), sigma_st(2,4)
      integer i, j, sect , ilayer
      real    T(2,4), dT(2,4)
      integer sigma(5)
      REAL *4 mid_E(2,4),dE
      real *4 ntdat(37)
      real *4 E_low,E_high,Ca
      data sigma/0,1,2,4,6/  ! magic numbers

c
c   MIPs selections
c
      
      DO sect=1,2
       good_sect(sect) = .false.
       IF (Nhits(sect,1).le.Npmt .and.
     &     Nhits(sect,2).le.Npmt .and.
     &     Nhits(sect,3).le.Npmt .and.
     &     Nhits(sect,4).le.Npmt .and.
     &     Nhits(sect,1)*Nhits(sect,2)*Nhits(sect,3)*Nhits(sect,4).ne.0
     &                               ) then
         good_sect(sect) = .true.
       end IF
      END DO
      
      IF((.NOT.good_sect(1)).AND.(.NOT.good_sect(2))) RETURN

      tot_ADC_in (1) = 0.
      tot_ADC_in (2) = 0.
      tot_ADC_out(1) = 0.
      tot_ADC_out(2) = 0.
      CALL izero(id,8)
      CALL izero(mid_st,8)
      CALL izero(sigma_st,8)

c initialize ntuple

      DO i=1,37
        ntdat(i) =0.
      END DO

      DO i=1,2
        DO j=1,4
          mid_E(i,j) = 0.
          sumE (i,j) = 0.
	  T (i,j)  = 400.
	  dT(i,j) = 400.
	END DO
      END DO
      
      DO i=1,nEC1
        sect   = sector(i)
        IF(good_sect(sect)) then
          ilayer = layer (i)
          mid_st (sect,layer(i)) = mid_st(sect,layer(i))+strip(i)
        END IF
      END DO

      DO sect=1,2
        IF(good_sect(sect)) then
          DO ilayer=1,4
            mid_st(sect,ilayer) = mid_st(sect,ilayer)/Nhits(sect,ilayer) ! mean value of stack numbers
          END DO
        END IF
      END DO

      DO i=1,nEC1
        sect   = sector(i)
        IF(good_sect(sect)) then
          sigma_st(sect,layer(i)) = 
     &    sigma_st(sect,layer(i)) +abs(mid_st(sect,layer(i))-strip(i))   ! sigma of stack numbers
        END IF
      END DO

      DO i=1,nEC1
        sect = sector(i)
        IF(good_sect(sect)) then
          IF(sigma_st(sect,layer(i)) .eq. sigma(Nhits(sect,layer(i)))) then  ! selected successive stacks
            dE = sqrt(E_l(i)*E_r(i))
            mid_E(sect,layer(i)) = mid_E(sect,layer(i))+strip(i)*dE
            sumE (sect,layer(i)) = sumE (sect,layer(i))+dE
c
c Timing
c
	    if (tdcl(i)*tdcr(i).ne.0) then
	      T (sect,layer(i)) = min( T(sect,layer(i)),(T_L(i)+T_R(i))/2)
              if (abs(dT(sect,layer(i))) .gt. abs(T_L(i)-T_R(i))) then
                dT(sect,layer(i)) = T_L(i)-T_R(i)  
              end if
	    end if
          ELSE
            RETURN  ! not a neibouring stacks fired
          END IF
        END IF
      END DO
      
      DO sect=1,2
        IF(good_sect(sect)) then
          DO ilayer=1,4
            id(sect,ilayer) = int(mid_E(sect,ilayer)/sumE(sect,ilayer)+0.5)
c            ntdat(5+ilayer) = sumE(sect,ilayer)  ! before any corrections
c Timing
c
             ntdat(14+ilayer) =  T(sect,ilayer)
             ntdat(18+ilayer) = dT(sect,ilayer)
          END DO
        END IF
      END DO
      
c
c  Correct ADC values taking into account thickness difference
c  and atten. length

      CALL Att_GeomCorr
      
c      CALL get_lac_plane

c      CALL get_EVNT
C     
c fill ntuple
c       selecting MIPs spot in inner and outer modules separately
c       peak_mips = 26.4 MeV  for layers 1,3,4
c                   26.4 *9/8 for layer 2
c         
c        Ca     = 0.648 ! * 1.073 * 1.019
c        Ca     = 0.7084 ! difference between energy from RECSIS and thisprog
        Ca     = 0.73 ! difference between energy from RECSIS and this prog
	E_low  = 30. ! MeV low cut for dep. energy in inner(outer)
	E_high = 90. ! MeV

      DO sect=1,2
        IF(good_sect(sect)) then
          call hfill(10*sect,tot_ADC_in(sect)/Ca,tot_ADC_out(sect)/Ca,1.)
          call hfill(1000*sect+Ifile*10+5,tot_ADC_in(sect)/Ca,tot_ADC_out(sect)/Ca,1.)
cc          IF (tot_ADC_in (sect).ge.35.and.tot_ADC_in (sect).le.75.and.
cc     &        tot_ADC_out(sect).ge.35.and.tot_ADC_out(sect).le.75.and.
cc     &        abs(y_lac(sect)-(-201.2+9.754*id(sect,2))).le.30   .and.
cc     &        abs(z_lac(sect)-(142+8.585*id(sect,1))).le.30      .and.       
cc     &        abs(id(sect,1)-id(sect,3)).le.ishift               .and.
cc     &        abs(id(sect,2)-id(sect,4)).le.ishift)      THEN
          IF (tot_ADC_in(sect)/Ca.ge.E_low.and.tot_ADC_in(sect)/Ca.le.E_high.and.
     &        tot_ADC_out(sect)/Ca.ge.E_low.and.tot_ADC_out(sect)/Ca.le.E_high.and.
     &        abs(id(sect,1)-id(sect,3)).le.ishift.and.
     &        abs(id(sect,2)-id(sect,4)).le.ishift)      THEN
c
c    Select elastic PROTONS only
c
c           IF (nEVNT.eq.2 .and. idEVNT(2).eq.2212) THEN
c	    IF (.true.) THEN
            N_good_event = N_good_event + 1
            ntdat(1) = nevent
            ntdat(2) = sect
            ntdat(3) = ifile
            ntdat(4) = id(sect,1)
            ntdat(5) = id(sect,2)
            ntdat(6) = id(sect,3)
            ntdat(7) = id(sect,4)
            
            ntdat(8) = tot_ADC_in (sect)/Ca
            ntdat(9) = tot_ADC_out(sect)/Ca
          
            ntdat(10) = sumE(sect,1)/Ca
            ntdat(11) = sumE(sect,2)/Ca
            ntdat(12) = sumE(sect,3)/Ca
            ntdat(13) = sumE(sect,4)/Ca
            ntdat(14) = Nhits(sect,1)*1.+Nhits(sect,2)+Nhits(sect,3)
     &               + Nhits(sect,4)
	    ntdat(34) = Nhits(sect,1)*1.
	    ntdat(35) = Nhits(sect,2)*1.
	    ntdat(36) = Nhits(sect,3)*1.
	    ntdat(37) = Nhits(sect,4)*1.
	    
            ntdat(23) = RF1*1.
            ntdat(24) = RF2*1.
c            if (lac_track(sect).ne.0.and.ntrk_lac.ne.0) then
c              j = lac_track(sect)
c              ntdat(25) = sqrt(px_hbtr(j)**2+py_hbtr(j)**2+pz_hbtr(j)**2)
c              ntdat(26) = q_hbtr (j)
            if ( nEVNT.gt.0) then
              ntdat(25) = pmomEVNT(2)
              ntdat(26) = chargeEVNT(2)
            else if ( nMCTK.gt.0 ) then
              ntdat(25) = pmomMCTK(1)
              ntdat(26) = chargeMCTK(1)
            end if
c              ntdat(27) = x_lac(sect)
c              ntdat(28) = y_lac(sect)
c              ntdat(29) = z_lac(sect)
c              ntdat(30) = trk_len(sect)
c              ntdat(31) = px_hbtr(j)/ntdat(25)
c              ntdat(32) = py_hbtr(j)/ntdat(25)
c              ntdat(33) = pz_hbtr(j)/ntdat(25)
c            end if
          
            call hfill(101+10*sect,0.5+id(sect,1),sumE(sect,1)/Ca,1.)
            call hfill(102+10*sect,0.5+id(sect,2),sumE(sect,2)/Ca,1.)
            call hfill(103+10*sect,0.5+id(sect,3),sumE(sect,3)/Ca,1.)
            call hfill(104+10*sect,0.5+id(sect,4),sumE(sect,4)/Ca,1.)
            call hfn  (200,ntdat)
            
          end if
        END IF
      END DO  
      return
      end
      

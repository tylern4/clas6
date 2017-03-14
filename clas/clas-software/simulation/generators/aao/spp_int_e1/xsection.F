      subroutine xsection

      implicit none

      include 'mpintp.inc'
      include 'spp.inc'
      include 'helamp.inc'

      real vl, vt, vlt, vtt, vltp, vttp
      real phicm_rad, ekin, cph, sph, c2ph, s2ph
      real rnorm1,kin1

      call helicity_amps  ! calc. helicity amplitudes

      sig_t  = (cabs(hh1)**2+cabs(hh2)**2
     1         +cabs(hh3)**2+cabs(hh4)**2)/2.
      sig_l  =  cabs(hh5)**2+cabs(hh6)**2
      sig_tt =  real(-conjg(hh1)*hh4+conjg(hh2)*hh3)
      
      sig_lt  =   real(conjg(hh5)*(hh1-hh4)  + 
     1                 conjg(hh6)*(hh2+hh3))/sqrt(2.0)      
      sig_ltp = -aimag(conjg(hh5)*(hh1-hh4)  + 
     1                 conjg(hh6)*(hh2+hh3))/sqrt(2.0)
     
c Target polarization (see Knochlein et al., nucl-th/9506029)
     
      sigt_t_y   = -aimag(conjg(hh1)*hh2+conjg(hh3)*hh4)
      sigt_l_y   = -2*aimag(conjg(hh5)*hh6)
      
      sigt_lt_x  = aimag(conjg(hh5)*(hh3-hh2) - 
     1                   conjg(hh6)*(hh1+hh4))/sqrt(2.)
      sigt_ltp_x =  real(conjg(hh5)*(hh2-hh3) +
     1                   conjg(hh6)*(hh1+hh4))/sqrt(2.)
     
      sigt_lt_y  = aimag(-conjg(hh5)*(hh2+hh3) + 
     1                    conjg(hh6)*(hh1-hh4))/sqrt(2.)
      sigt_ltp_y =  real(-conjg(hh5)*(hh2+hh3) + 
     1                    conjg(hh6)*(hh1-hh4))/sqrt(2.)
     
      sigt_lt_z  = aimag(-conjg(hh5)*(hh1+hh4) + 
     1                    conjg(hh6)*(hh2-hh3))/sqrt(2.)
      sigt_ltp_z = -real(-conjg(hh5)*(hh1+hh4) + 
     1                    conjg(hh6)*(hh2-hh3))/sqrt(2.)
     
      sigt_tt_x  =  aimag(conjg(hh1)*hh3-conjg(hh2)*hh4)
      sigt_tt_y  = -aimag(conjg(hh1)*hh3+conjg(hh2)*hh4)
      sigt_tt_z  = -aimag(conjg(hh1)*hh4+conjg(hh2)*hh3)
      
      sigt_ttp_x = real(conjg(hh1)*hh2+conjg(hh3)*hh4)
      sigt_ttp_z = (cabs(hh1)**2-cabs(hh2)**2
     1             +cabs(hh3)**2-cabs(hh4)**2)/2.
     
c Recoil polarization

      sigr_lt_y = -aimag(-conjg(hh5)*(hh2+hh3) + 
     1                    conjg(hh6)*(hh1-hh4))/sqrt(2.)
      sigr_ltp_x =  real(conjg(hh5)*(hh3-hh2) +
     1                   conjg(hh6)*(hh1+hh4))/sqrt(2.)
      sigr_ttp_z = (cabs(hh1)**2+cabs(hh2)**2
     1             -cabs(hh3)**2-cabs(hh4)**2)/2. 
     
c      rnorm1 = 0.141383! Convert from microbarns^1/2 to 10**-3/m_pi (MAID tables)
      rnorm1 = 1.0
     
      reph(1)=real(hh1)/rnorm1
      reph(2)=real(hh2)/rnorm1
      reph(3)=real(hh3)/rnorm1
      reph(4)=real(hh4)/rnorm1
      reph(5)=real(hh5)/rnorm1
      reph(6)=real(hh6)/rnorm1
         
      ieph(1)=imag(hh1)/rnorm1
      ieph(2)=imag(hh2)/rnorm1
      ieph(3)=imag(hh3)/rnorm1
      ieph(4)=imag(hh4)/rnorm1
      ieph(5)=imag(hh5)/rnorm1
      ieph(6)=imag(hh6)/rnorm1
            
      phicm_rad = phicm*pi/180.0
      cph  = cos(phicm_rad)
      sph  = sin(phicm_rad)
      c2ph = cos(2*phicm_rad)
      s2ph = sin(2*phicm_rad)
      
c      if (phicm.lt.2.and.abs(csthcm).lt.0.05) then
c      kin1 = nu_cm/qv_mag_cm
c      print *, w,hh1/rnorm1,hh2/rnorm1,hh3/rnorm1
c      print *, hh4/rnorm1,hh5/rnorm1*kin1,hh6*kin1/rnorm1
c      print *, ' '
c      endif
      
      vt   = 1.0
      vl   = epsilon
      vtt  = epsilon
      vlt  = sqrt(2*epsilon*(1+epsilon))
      vltp = sqrt(2*epsilon*(1-epsilon))
      vttp = sqrt(1-epsilon**2)
      
      ekin = sqrt(Q2)/qv_mag_cm
      
c      ekin = sqrt(Q2)/nu_cm

c      if (phicm.eq.7.5.and.csthcm.gt.0.89.and.w.lt.1.6) then
c     print *, nu_cm,ppi_mag_cm
c      print *, w,fkt*sig_t,fkt*sig_l*ekin**2,fkt*sig_tt,
c     1fkt*sig_lt*ekin,fkt*sig_ltp*ekin
c      endif
      
      sigma_t     = fkt         * sig_t
      sigma_l     = fkt  * vl   * sig_l         * ekin**2
      sigma_tt    = fkt  * vtt  * sig_tt  * c2ph   
      sigma_lt    = fkt  * vlt  * sig_lt  * cph * ekin
      sigma_ltp	  = fkt  * vltp * sig_ltp * sph * ekin
      
      sigma_t_y   = fkt  * vt   * pn * sigt_t_y
      sigma_l_y   = fkt  * vl   * pn * sigt_l_y         *ekin**2
      sigma_lt_x  = fkt  * vlt  * pt * sigt_lt_x  * sph *ekin
      sigma_lt_y  = fkt  * vlt  * pn * sigt_lt_y  * cph *ekin
      sigma_lt_z  = fkt  * vlt  * pl * sigt_lt_z  * sph *ekin
      sigma_tt_x  = fkt  * vtt  * pt * sigt_tt_x  * s2ph
      sigma_tt_y  = fkt  * vtt  * pn * sigt_tt_y  * c2ph
      sigma_tt_z  = fkt  * vtt  * pl * sigt_tt_z  * s2ph
      sigma_ltp_x = fkt  * vltp * pt * sigt_ltp_x * cph *ekin
      sigma_ltp_y = fkt  * vltp * pn * sigt_ltp_y * sph *ekin
      sigma_ltp_z = fkt  * vltp * pl * sigt_ltp_z * cph *ekin
      sigma_ttp_x = fkt  * vttp * pt * sigt_ttp_x
      sigma_ttp_z = fkt  * vttp * pl * sigt_ttp_z
      
c      kin1 = nu_cm/qv_mag_cm
c      write(*,100) acos(csthcm)*180/3.14159,sig_t,sigt_t_y,-sigt_tt_y,
c     *sig_tt,sigt_tt_x,sigt_ttp_x,sigt_ttp_z     
     
100   format(8(f7.2))        
   
      end

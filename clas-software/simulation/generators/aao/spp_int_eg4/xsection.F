      subroutine xsection

      implicit none

      include 'mpintp.inc'
      include 'spp.inc'

      real s, qsq, mk, my, qv, kv
      real vl, vt, vlt, vtt, vltp, vttp
      real phicm_rad, ekin

c      call multipole_amps ! calc. multipole amplitudes
      call helicity_amps  ! calc. helicity amplitudes

      phicm_rad = phicm*pi/180.0
c      ekin = Q2/nu_cm**2

C		to eliminate round off error let's eliminate nu_cm in both multipole_amps xsection since they are canceling anyways
c      ekin = sqrt(Q2) / nu_cm
      ekin = sqrt(Q2)


c      print *, q2, nu_cm
      vt   = 1.0
      vl   = epsilon
      vtt  = epsilon
      vlt  = sqrt(2*epsilon*(1+epsilon))
      vltp = sqrt(2*epsilon*(1-epsilon))
      vttp = sqrt(1-epsilon**2)
c
      s = W**2
      qsq = -qsq
      mk  = m_pi0
      my  = m_p
      QV = SQRT(S**2-2.d0*(M_P**2-QSQ)*S + (M_P**2-QSQ)**2 )/2.d0/W    
      KV = SQRT(S**2-2.d0*(MY**2+MK**2)*S+(MY**2-MK**2)**2 )/2.d0/W 
c      fkt = kv / qv
      fkt  = 2*W*ppi_mag_cm/(W**2 - m_p**2)

      sigma_t  = (cabs(hh1)**2+cabs(hh2)**2
     1           +cabs(hh3)**2+cabs(hh4)**2)/2.
      sigma_l  =  cabs(hh5)**2+cabs(hh6)**2
      sigma_tt =  real(-conjg(hh1)*hh4+conjg(hh2)*hh3)
      sigma_lt =  real(conjg(hh5)*(hh1-hh4)  + 
     1                  conjg(hh6)*(hh2+hh3))/sqrt(2.0)
      sigma_ltp = aimag(-conjg(hh5)*(hh1-hh4)  - 
     1                  conjg(hh6)*(hh2+hh3))/sqrt(2.0)
      sigma_t_y   = -aimag(conjg(hh1)*hh2+conjg(hh3)*hh4)
      sigma_l_y   = -2*aimag(conjg(hh5)*hh6)
      sigma_lt_x  = aimag(conjg(hh5)*(hh3-hh2) - 
     1                    conjg(hh6)*(hh1+hh4))/sqrt(2.)
      sigma_lt_y  = aimag(-conjg(hh5)*(hh3+hh2) + 
     1                     conjg(hh6)*(hh1-hh4))/sqrt(2.)
      sigma_lt_z  = aimag(-conjg(hh5)*(hh1+hh4) + 
     1                     conjg(hh6)*(hh2-hh3))/sqrt(2.)
      sigma_tt_x  = aimag(conjg(hh1)*hh3-conjg(hh2)*hh4)
      sigma_tt_y  = -aimag(conjg(hh1)*hh3+conjg(hh2)*hh4)
      sigma_tt_z  = -aimag(conjg(hh1)*hh4+conjg(hh2)*hh3)
      sigma_ltp_x = real(conjg(hh5)*(-hh3+hh2) + 
     1                   conjg(hh6)*(hh1+hh4))/sqrt(2.)
      sigma_ltp_y = real(-conjg(hh5)*(hh3+hh2) + 
     1                    conjg(hh6)*(hh1-hh4))/sqrt(2.)
      sigma_ltp_z = real(conjg(hh5)*(hh1+hh4) + 
     1                   conjg(hh6)*(hh3-hh2))/sqrt(2.)
      sigma_ttp_x = real(conjg(hh1)*hh2+conjg(hh3)*hh4)
      sigma_ttp_z = (cabs(hh1)**2-cabs(hh2)**2
     1              +cabs(hh3)**2-cabs(hh4)**2)/2.




      sigma_l  = fkt*sigma_l*ekin**2
      sigma_t  = fkt*sigma_t
      sigma_tt = fkt*sigma_tt
      sigma_lt = fkt*sigma_lt*ekin
      sigma_ltp= fkt*sigma_ltp*ekin
      sigma_t_y   = fkt  * pn * sigma_t_y
      sigma_l_y   = fkt  * pn * sigma_l_y*ekin**2
      sigma_lt_x  = fkt  * pt * sigma_lt_x*sin(  phicm_rad)*ekin
      sigma_lt_y  = fkt  * pn * sigma_lt_y*cos(  phicm_rad)*ekin
      sigma_lt_z  = fkt  * pl * sigma_lt_z*sin(  phicm_rad)*ekin
      sigma_tt_x  = fkt  * pt * sigma_tt_x*sin(  2*phicm_rad)
      sigma_tt_y  = fkt  * pn * sigma_tt_y*cos(  2*phicm_rad)
      sigma_tt_z  = fkt  * pl * sigma_tt_z*sin(  2*phicm_rad)
      sigma_ltp_x = fkt  * pt * sigma_ltp_x*cos(  phicm_rad)*ekin
      sigma_ltp_y = fkt  * pn * sigma_ltp_y*sin(  phicm_rad)*ekin
      sigma_ltp_z = fkt  * pl * sigma_ltp_z*cos(  phicm_rad)*ekin
      sigma_ttp_x = fkt  * pt * sigma_ttp_x
      sigma_ttp_z = fkt  * pl * sigma_ttp_z

      sigma_0  = vt*sigma_t   + vl*sigma_l + 
     1           vtt*sigma_tt*cos(2*phicm_rad) +
     1           vlt*sigma_lt*cos(  phicm_rad) + 
     1           e_hel*vltp*sigma_ltp*sin(phicm_rad)+
     1           p_hel*(vt*sigma_t_y  + vl*sigma_l_y +
     1             vlt*(sigma_lt_x + sigma_lt_y + sigma_lt_z)+
     1             vtt*(sigma_tt_x + sigma_tt_y + sigma_tt_z))+
     1           p_hel*e_hel*(
     1             vltp*(sigma_ltp_x + sigma_ltp_y + sigma_ltp_z)+
     1             vttp*(sigma_ttp_x + sigma_ttp_z))
C NEW AB
      sigma_00=vt*sigma_t   + vl*sigma_l + 
     1           vtt*sigma_tt*cos(2*phicm_rad) +
     1           vlt*sigma_lt*cos(  phicm_rad)
      sigma_0e=e_hel*vltp*sigma_ltp*sin(phicm_rad)  
      sigma_0t=p_hel*(vt*sigma_t_y  + vl*sigma_l_y +
     1           vlt*(sigma_lt_x + sigma_lt_y + sigma_lt_z)+
     1           vtt*(sigma_tt_x + sigma_tt_y + sigma_tt_z))
      sigma_0et=p_hel*e_hel*(
     1           vltp*(sigma_ltp_x + sigma_ltp_y + sigma_ltp_z)+
     1           vttp*(sigma_ttp_x + sigma_ttp_z))
C	  print *,epsilon,ekin**2,epsilon*(ekin**2),Q2,W,nu_cm

      end

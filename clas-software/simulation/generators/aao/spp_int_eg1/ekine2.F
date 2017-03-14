      program ekine2
c
c     purpose: calculate electron scattering angle and
c              energy for given W, Q2 and ebeam energy. 
      implicit none
c
c
      real ki_mag, kf_mag, theta_e
      real m_p, dummy1, W, Q2, W2
c
      m_p  = 0.93827
c
      write(6,*) 'Q2, W, and ebeam'
      read(5,*) Q2, W, ki_mag
      W2 = W**2
      kf_mag = (2*m_p*ki_mag + m_p**2 - Q2 - W2) / (2*m_p)
c
      dummy1  = sqrt(Q2 / (4*ki_mag*kf_mag))
      theta_e = 2*asin(dummy1)*180./3.141592
      print *, ' kf_mag= ',kf_mag, ' theta_e= ',theta_e
c
      stop
      end

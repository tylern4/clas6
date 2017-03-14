      subroutine getcos(e0,z_e,z_p,mp,costh,pt)
      implicit none
      real  e0,z_e(4),z_p(4),pe,pp,q2,z_q(4),z_w(4),pw, pt
      real gamma,beta,cost,ecm,pcm,pl,mp,costh,pq,w

      pe = z_e(4)
      Pp = sqrt(z_p(1)**2 + z_p(2)**2 +z_p(3)**2)

      Z_Q(1)   = -Z_E(1) 
      Z_Q(2)   = -Z_E(2) 
      Z_Q(3)   =  E0 - Z_E(3) 
      Z_Q(4)   =  E0 - Z_E(4)
      q2 = z_e(1)**2 + z_e(2)**2 + (E0 - z_e(3))**2 - 
     >   (E0 - z_e(4))**2
      w = sqrt(0.9383**2 + 2.*0.9383*(E0-z_e(4))-q2)
      PQ       =  SQRT(Z_Q(4)**2+Q2)

      Z_W(1)   = Z_Q(1) 
      Z_W(2)   = Z_Q(2) 
      Z_W(3)   = Z_Q(3)
      Z_W(4)   = SQRT(PQ**2+W**2)
      PW       = PQ

      GAMMA = Z_W(4)/W
      BETA  = PW/Z_W(4)
      COST  =(Z_W(1)*Z_P(1)+Z_W(2)*Z_P(2)+Z_W(3)*Z_P(3))/(PW*PP)
      ECM   = GAMMA*(Z_P(4)-BETA*PP*COST)
      PCM   = SQRT(ECM**2-MP**2)
      PL    = GAMMA*(PP*COST-BETA*Z_P(4))
      pt = sqrt(pcm**2 - pl**2)
      costh = -1.1
      if(pcm.ne.0.) costh = pl/pcm
      return
      end

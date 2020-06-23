c     Returns mass of particle identified by PDG particle ID
 10   function pdg_pid2mass(pid)
      implicit none

      real pdg_pid2mass
      integer pid

c     Gamma
      if(pid .eq. 22) then
         pdg_pid2mass=0
c     Positron
      elseif(pid .eq. -11) then
         pdg_pid2mass=0.511e-3
c     Electron
      elseif(pid .eq. 11) then
         pdg_pid2mass=0.511e-3
c     Neutrino
      elseif(pid .eq. 4) then
         pdg_pid2mass=0.        ! ambiguous, which neutrino should we
!     use?
c     Muon +
      elseif(pid .eq. -13) then
         pdg_pid2mass=105.65837e-3
c     Muon -
      elseif(pid .eq. 13) then
         pdg_pid2mass=105.65837e-3
c     Pion 0
      elseif(pid .eq. 111) then
         pdg_pid2mass=134.9767e-3
c     Pion +
      elseif(pid .eq. 211) then
         pdg_pid2mass=139.57018e-3
c     Pion -
      elseif(pid .eq. -211) then
         pdg_pid2mass=139.57018e-3
c     Kaon 0 Long
      elseif(pid .eq. 130) then
         pdg_pid2mass=497.614e-3
c     Kaon +
      elseif(pid .eq. 321) then
         pdg_pid2mass=493.677e-3
c     Kaon -
      elseif(pid .eq. -321) then
         pdg_pid2mass=493.677e-3
c     Neutron
      elseif(pid .eq. 2112) then
         pdg_pid2mass=939.565e-3
c     Proton
      elseif(pid .eq. 2212) then
         pdg_pid2mass=938.272e-3
c     Antiproton
      elseif(pid .eq. -2212) then
         pdg_pid2mass=938.272e-3
c     Kaon 0 Short
      elseif(pid .eq. 310) then
         pdg_pid2mass=497.614e-3
c     Eta
      elseif(pid .eq. 221) then
         pdg_pid2mass=547.862e-3
c     Lambda
      elseif(pid .eq. 3122) then
         pdg_pid2mass=1.115683
c     Sigma +
      elseif(pid .eq. 3222) then
         pdg_pid2mass=1.18937
c     Sigma 0
      elseif(pid .eq. 3212) then
         pdg_pid2mass=1.192642
c     Sigma -
      elseif(pid .eq. 3112) then
         pdg_pid2mass=1.197449
c     Xi 0
      elseif(pid .eq. 3322) then
         pdg_pid2mass=1.31486
c     Xi -
      elseif(pid .eq. 3312) then
         pdg_pid2mass=1.32131
c     Omega -
      elseif(pid .eq. 3334) then
         pdg_pid2mass=1.67245
c     Antineutron
      elseif(pid .eq. -2112) then
         pdg_pid2mass=939.565e-3
c     Antilambda
      elseif(pid .eq. -3122) then
         pdg_pid2mass=1.115683
c     Antisigma -
      elseif(pid .eq. -3222) then
         pdg_pid2mass=1.197449
c     Antisigma 0
      elseif(pid .eq. -3212) then
         pdg_pid2mass=1.192642
c     Antisigma +
      elseif(pid .eq. -3222) then
         pdg_pid2mass=1.18937
c     Antixi 0
      elseif(pid .eq. -3322) then
         pdg_pid2mass=1.31486
c     Antixi + (sic, should be Antixi -?) then
      elseif(pid .eq. -3312) then
         pdg_pid2mass=1.32131
c     Antiomega + (sic, like antixi -/+?) then
      elseif(pid .eq. -3334) then
         pdg_pid2mass=1.67245
      endif
      return
      end

c     Returns mass of particle identified by GEANT particle ID
 20   function geant_pid2mass(pid)
      implicit none

      real geant_pid2mass
      integer pid
      integer geant2pdg

      real pdg_pid2mass
      
c     Deuteron
      if(pid .eq. 45) then
         geant_pid2mass=1.8756
c     Triton
      elseif(pid .eq. 46) then
         geant_pid2mass=2.80943   ! this is mass of tritium atom
c     HE3
      elseif(pid .eq. 49) then
         geant_pid2mass=2.80941   ! this is mass of He3 atom
         
      else 
         geant_pid2mass = pdg_pid2mass(geant2pdg(pid))
      endif
      return
      end

c     Returns the PDG particle id from the GEANT particle ID
      function geant2pdg(pid)
      implicit none

      integer pid, geant2pdg

c     Gamma
      if(pid .eq. 1) then
         geant2pdg=22
c     Positron
      elseif(pid .eq. 2) then
         geant2pdg=-11
c     Electron
      elseif(pid .eq. 3) then
         geant2pdg=11
c     Neutrino
      elseif(pid .eq. 4) then
         geant2pdg=12           ! ambiguous, which neutrino should we use?
c     Muon +
      elseif(pid .eq. 5) then
         geant2pdg=-13
c     Muon -
      elseif(pid .eq. 6) then
         geant2pdg=13
c     Pion 0
      elseif(pid .eq. 7) then
         geant2pdg=111
c     Pion +
      elseif(pid .eq. 8) then
         geant2pdg=211
c     Pion -
      elseif(pid .eq. 9) then
         geant2pdg=-211
c     Kaon 0 Long
      elseif(pid .eq. 10) then
         geant2pdg=130
c     Kaon +
      elseif(pid .eq. 11) then
         geant2pdg=321
c     Kaon -
      elseif(pid .eq. 12) then
         geant2pdg=-321
c     Neutron
      elseif(pid .eq. 13) then
         geant2pdg=2112
c     Proton
      elseif(pid .eq. 14) then
         geant2pdg=2212
c     Antiproton
      elseif(pid .eq. 15) then
         geant2pdg=-2212
c     Kaon 0 Short
      elseif(pid .eq. 16) then
         geant2pdg=310
c     Eta
      elseif(pid .eq. 17) then
         geant2pdg=221
c     Lambda
      elseif(pid .eq. 18) then
         geant2pdg=3122
c     Sigma +
      elseif(pid .eq. 19) then
         geant2pdg=3222
c     Sigma 0
      elseif(pid .eq. 20) then
         geant2pdg=3212
c     Sigma -
      elseif(pid .eq. 21) then
         geant2pdg=3112
c     Xi 0
      elseif(pid .eq. 22) then
         geant2pdg=3322
c     Xi -
      elseif(pid .eq. 23) then
         geant2pdg=3312
c     Omega -
      elseif(pid .eq. 24) then
         geant2pdg=3334
c     Antineutron
      elseif(pid .eq. 25) then
         geant2pdg=-2112
c     Antilambda
      elseif(pid .eq. 26) then
         geant2pdg=-3122
c     Antisigma -
      elseif(pid .eq. 27) then
         geant2pdg=-3222
c     Antisigma 0
      elseif(pid .eq. 28) then
         geant2pdg=-3212
c     Antisigma +
      elseif(pid .eq. 29) then
         geant2pdg=-3222
c     Antixi 0
      elseif(pid .eq. 30) then
         geant2pdg=-3322
c     Antixi + (sic, should be Antixi -?) then
      elseif(pid .eq. 31) then
         geant2pdg=-3312
c     Antiomega + (sic, like antixi -/+?) then
      elseif(pid .eq. 32) then
         geant2pdg=-3334
c     Deuteron
      elseif(pid .eq. 45) then
         geant2pdg=0            ! there doesn't appear to be a PDG id for deuteron
c     Triton
      elseif(pid .eq. 46) then
         geant2pdg=0            ! there doesn't appear to be a PDG id for triton
c     Alpha
      elseif(pid .eq. 47) then
         geant2pdg=0            ! there doesn't appear to be a PDG id for alpha
c     Geantino
      elseif(pid .eq. 48) then
         geant2pdg=0            ! there doesn't appear to be a PDG id for geantino
c     HE3
      elseif(pid .eq. 49) then
         geant2pdg=0            ! there doesn't appear to be a PDG id for HE3
c     Cerenkov
      elseif(pid .eq. 50) then
         geant2pdg=0            ! there doesn't appear to be a PDG id for Cerenkov
      endif
 100  return
      end

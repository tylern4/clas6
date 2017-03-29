      subroutine throw_fermi(target_type,offshellness,target,
     +     spectator)
      implicit none

#include "bcs.inc"

      real target(4), spectator(4)
      integer target_type
      integer offshellness

      real targetmag
      real phi, theta
      real fermiv(3)

      real pi
      parameter (pi = 3.1415926)

      integer mlink

c     deuteron mass from NIST, and reduced proton/neutron masses in
c     deuteron
      real md
      real mp
      real mn
      real mp_red
      real mn_red
c     All in GeV/c^2
      parameter (md = 1.8756, mp = 0.9383, mn = 0.9396)
      parameter (mp_red = md + 0.5*(mp-mn), mn_red = md + 0.5*(mn-mp))

      real target_mass, spectator_mass

      real invcdf
      real random
      real eleconshell

c     BOS indices:
      integer iind

c     Misc. variables
      real E_t, E_s, leftover_energy

      random=rand(0)
      targetmag = invcdf(random)

c     debug
      
c     This setting is to test the effect of smearing by reducing the
c     momentum by a factor of 100 to see if the W distribution gets
c     closer to reality.  If it does not, then there is clearly a
c     technical problem in the implementation of the smearing algorithm.

c     targetmag = targetmag*0.01
      
c     end debug
      
      random=rand(0)
      phi = 2.0*pi*random

      random=rand(0)
      theta = acos(2.0*random-1.0)

c     3-momentum is known due to Bohm distribution
      target(2) = targetmag * sin(theta) * cos(phi)
      target(3) = targetmag * sin(theta) * sin(phi)
      target(4) = targetmag * cos(theta)
c     and a copy for eleconshell
      fermiv(1) = target(2)
      fermiv(2) = target(3)
      fermiv(3) = target(4)
c     Minus that for the spectator
      spectator(2) = -target(2)
      spectator(3) = -target(3)
      spectator(4) = -target(4)

c     There are a few options for the energy of the nucleon.
c     
c     0. Target nucleon on-mass shell (naive).  This does not yield an
c     on mass-shell final electron (e''), and also does not match
c     reality since we know that the nucleons are virtual.  I've added
c     it mostly for debugging (since it preserves W in the initial and
c     final state all the way until the final electron is slightly
c     adjusted) and for systematic error estimates so that the variation
c     in the final calculated results can be shown.
      if(offshellness .eq. 0) then
         if(target_type .eq. 1) then
            target_mass=mp
         elseif (target_type .eq. 0) then
            target_mass=mn
         endif
         target(1) = sqrt(targetmag*targetmag+target_mass*target_mass)
         spectator(1) = md - target(1)
c     Use reduced mass of proton and neutron by simply splitting the
c     binding energy among the nucleons equally, with constant energy.
c     This might be the expectation value choice.
      elseif(offshellness .eq. 1) then
         if(target_type .eq. 1) then
            target_mass=mp_red
            spectator_mass=mn_red
         elseif (target_type .eq. 0) then
            target_mass=mn_red
            spectator_mass=mp_red
         endif
         target(1) = target_mass
         spectator(1) = spectator_mass
         
c     2. Dr. Gothe's hunch: Virtuality is constant and shared evenly
c     among the
c     nucleons.  The missing energy of the deuterium nucleus,
c     
c     delta = m_d - (sqrt(m_p^2+f^2)+sqrt(m_n^2+f^2)),
c     
c     is divided in half and added to each nucleon's energy component.
c     Other split ratios are possible and in the future an option for
c     this might be supplied if a 50/50 split does not work well.
      elseif(offshellness .eq. 2) then
         if(target_type .eq. 1) then
            target_mass=mp
            spectator_mass=mn
         elseif (target_type .eq. 0) then
            target_mass=mn
            spectator_mass=mp
         endif
         E_t = target(2)**2+target(3)**2+target(4)**2
     +        + target_mass**2
         E_s = spectator(2)**2+spectator(3)**2+spectator(4)**2
     +        + spectator_mass**2
         leftover_energy = md - E_t - E_s
         target(1) = E_t + 0.5*leftover_energy
         spectator(1) = E_s + 0.5*leftover_energy
      endif

      return
      end

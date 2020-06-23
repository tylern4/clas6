      program maid_xsection

c     maid_xsection generates the MAID model single-photon approximation
c     cross section from the MAID multipole tables.  Reads the
c     (W,Q^2,cos(Theta_pi),Phi_pi) coordinates from input and prints the
c     cross section along with those coordinates to output.  Accepts
c     model parameters and other settings as command line arguments.

      implicit none

c     ARGUMENTS TO DSIGMA
      real theta_e, q2, w, costheta, phi, phi_deg
      integer model_opt, raw_channel_opt, channel_opt, resonance_opt
      integer helicity

c     RETURNS FROM DSIGMA
      real sigma0, sigma_t, sigma_l, sigma_tt, sigma_lt,
     +     sigma_ltp, asym_p

c     ELECTRON_THETA HELPER FUNCTION
      real electron_theta

c     PROGRAM VARIABLES
c     Program arguments: beam_energy, model_opt, channel_opt, helicity,
c     sigma_term
      integer nargs_total
      character * (100) buf
      parameter (nargs_total=5)

      integer nargs
      integer sigma_term
      real beam_energy, target_mass

      real sigma

c     loop variables
      integer i, j
      integer iostat, comment_present

      character * (100) comment_format
      character * (100) xsection_input_format
      character * (100) xsection_output_format

      character * (100) line1, line2

      character * (100) sigma_str

      real x

      real pi
      parameter(pi=4.D0*DATAN(1.D0))

      real deg2rad
      parameter(deg2rad=pi/180.0)

c     INITIALIZATIONS
c     This option is always 0 in aao_rad and aao_norad
      resonance_opt = 0
c     IO status initialization
      iostat = 0

      comment_present = 0
c     format strings
      comment_format = "(A,A14,A15,A15,A15,A15)"
      xsection_input_format = "(ES15.7,ES15.7,ES15.7,ES15.7)"
      xsection_output_format = "(ES15.7,ES15.7,ES15.7,ES15.7,ES15.7)"

c     PARSE ARGUMENTS
      nargs = command_argument_count()
      if(nargs.ne.nargs_total) then
 10      write(*,*) "Usage: maid_xsection <beam_energy>",
     +        " <model_opt> <channel_opt>",
     +        " <helicity> <sigma_term>"
         write(*,*) ""
         write(*,*) "model_opt: "
         write(*,*) "1=AO, 2=daresbury, 3=hlee pi0,"
         write(*,*) "4=MAID98, 5=MAID2000, 6=MAID2003"
         write(*,*) "channel_opt: (1=pi0, 2=pi-, 3=pi+)"
c     write(*,*) "channel_opt (hadron charge):",
c     +        "(0=e+n->e+p+pi-, 1=e+p->e+n+pi+)"
         write(*,*) "helicity: -1, 1, or 0 for no helicity contribution"
         write(*,*) "sigma_term: (0=full, 1=sigma_t, 2=sigma_l, ",
     +        "3=sigma_tt, 4=sigma_lt, 5=sigma_ltp)"
         write(*,*) ""
         write(*,*) "Data should be fed into standard input with"
         write(*,*) "W (GeV), Q^2 (GeV^2), cos(theta) and",
     +        " phi (degrees) columns."
         write(*,*) "Input data can contain an initial comment line",
     +        " starting with a # character."
         goto 100
      else
c     Read arguments
         call getarg(1,buf)
         read(buf,*) beam_energy

c$$$  call getarg(2,buf)
c$$$  read(buf,*) target_mass

         call getarg(2,buf)
         read(buf,*) model_opt

         call getarg(3,buf)
         read(buf,*) channel_opt

         call getarg(4,buf)
         read(buf,*) helicity

         call getarg(5,buf)
         read(buf,*) sigma_term
      endif

c     Set the channel_opt
c     channel_opt = raw_channel_opt + 2

c     Set target mass
      if(channel_opt.eq.3) then
         target_mass = 939.565e-3
      elseif(channel_opt.eq.2) then
         target_mass = 938.272e-3
      else
         write(*,*) "Error, must specify either channel_opt=2",
     +        "(pi-) or 3 (pi+)"
      endif
c     Setup sigma_str
      if(sigma_term.eq.0) then
         sigma_str="sigma"
      elseif(sigma_term.eq.1) then
         sigma_str="sigma_t"
      elseif(sigma_term.eq.2) then
         sigma_str="sigma_l"
      elseif(sigma_term.eq.3) then
         sigma_str="sigma_tt"
      elseif(sigma_term.eq.4) then
         sigma_str="sigma_lt"
      elseif(sigma_term.eq.5) then
         sigma_str="sigma_ltp"
      else
         goto 10
      endif

c     Print comment line
      write(*,comment_format), "#", "W (GeV)", "Q^2 (GeV^2)",
     +     "cos(theta*)", "phi* (deg)",
     +     trim(sigma_str)
c     Loop over input lines
c     Check first line for comment
      read(*,"(A)", IOSTAT=iostat) buf
      if(iostat.ne.0.or.buf(1:1).eq."#") then
         comment_present=1
      endif

c     Main loop
      do while(iostat.eq.0)
         if(comment_present.eq.0) then
            comment_present=1
         else
            read(*,"(A)", IOSTAT=iostat) buf
            if(iostat.ne.0) then
               goto 100
            endif
         endif
         read(buf,*)
     +        w, q2, costheta, phi_deg
c     Convert phi to radians
         phi = phi_deg*deg2rad
c     Compute MAID cross section
         theta_e = electron_theta(beam_energy,target_mass,w,q2)
c     debug
c     write(*,*) "theta_e=", theta_e
c     end debug
c     call dsigma(theta_e, q2, w, costheta, phi,
         call dsigma(theta_e*deg2rad, q2, w, costheta, phi/deg2rad,
     +        model_opt, channel_opt, resonance_opt,
     +        sigma0, sigma_t, sigma_tt, sigma_l, sigma_lt,
     +        sigma_ltp, asym_p,
     +        helicity)
c     debug
c     write(*,*) sigma0, sigma_t, sigma_l, sigma_lt,
c     +        sigma_tt, sigma_ltp
c     end debug
c     Set sigma term:
         if(sigma_term.eq.0) then
            sigma=sigma0
         elseif(sigma_term.eq.1) then
            sigma=sigma_t
         elseif(sigma_term.eq.2) then
            sigma=sigma_l
         elseif(sigma_term.eq.3) then
            sigma=sigma_tt
         elseif(sigma_term.eq.4) then
            sigma=sigma_lt
         elseif(sigma_term.eq.5) then
            sigma=sigma_ltp
         endif
c     Write output line:
         write(*,xsection_output_format) w, q2, costheta,
     +        phi/deg2rad, sigma
      enddo

 100  end program

      function electron_theta(beam_energy,target_mass,w,q2)
      implicit none

      real electron_theta
      real beam_energy, target_mass, w, q2

c     FUNCTION VARIABLES
      real nu, Eprime, p, pprime, costheta
c     CONSTANTS
      real me
      parameter (me=0.511e-3)
      real pi
      parameter(pi=4.D0*DATAN(1.D0))

      nu = (w**2+q2-target_mass**2)/(2.0*target_mass)

      eprime = beam_energy - nu
c     Debug
c     write(*,*) "E' = ", eprime
c     End debug

      p = sqrt(beam_energy**2 - me**2)
      pprime = sqrt(eprime**2 - me**2)

      costheta = (q2/2 - beam_energy*eprime + me**2)
     +     /(p*pprime)

c     debug
c     write(*,*) "costheta=", costheta
c     end debug

c     Ye's
c     theta = 2*TMath::ASin(TMath::Sqrt((Q2)/(4*Ei*Ef)))*(180/TMath::Pi());
c     Ef=Ei-(w*w+Q2-mn*mn)/(2*mn);
      eprime=beam_energy-(w*w+q2-target_mass*target_mass)
     +     /(2*target_mass);
      electron_theta = 2*asin(sqrt((q2)/(4*beam_energy*eprime)))
     +     * (180/pi)
c     End Ye's

c     electron_theta = acos(costheta)

      end

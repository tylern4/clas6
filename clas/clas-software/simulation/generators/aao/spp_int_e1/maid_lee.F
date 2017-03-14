      subroutine maid_lee(a1,a2,a3,a4,a5,
     1           theory_opt,channel_opt,resonance_opt,iopt,
     1           pol_elec,pol_targ,sigu,sigt,
     1           sigl,sigi,sige,siget,sigpt)

      implicit none
      
      real a1,a2,a3,a4,a5

      include 'mpintp.inc'
      include 'spp.inc'

      integer theory_opt, channel_opt, resonance_opt, n_call,iopt
      real pol_targ,pol_elec
      real sigu,sigt,sigl,sigi
      real sige,sigpt,siget
      character*132 fname

      data n_call /0/

      q2          = a1
      w           = a2
      epsilon     = a3
      csthcm      = a4
      phicm       = a5
      
      intopt      = iopt
      
      if (n_call.ne.0) goto 100

      method_helicity = 1

      if (theory_opt.eq.1) then
        if (channel_opt.eq.1) then
          call revinm('CLAS_PARMS',
     1    'spp_tbl/hlee-PPpi.tbl',data_file)
        elseif (channel_opt.eq.2) then
          call revinm('CLAS_PARMS',
     1    'spp_tbl/hlee-NPpi.tbl',data_file)
        elseif (channel_opt.eq.3) then 
          call revinm('CLAS_PARMS',
     1    'spp_tbl/hlee-PNpi.tbl',data_file)
        endif
      endif
      if (theory_opt.eq.5) then
        if (channel_opt.eq.1) then
          call revinm('CLAS_PARMS',
     1    'spp_tbl/dmt-PPpi_all_on.tbl',data_file)
        elseif (channel_opt.eq.3) then 
          call revinm('CLAS_PARMS',
     1    'spp_tbl/dmt-PNpi_all_on.tbl',data_file)
        endif
      endif
      
      if (theory_opt.eq.6) then
        if (channel_opt.eq.4) then
          call revinm('CLAS_PARMS',
     1    'spp_tbl/etamaid-PPeta.tbl',data_file)
        endif
      endif
            
      if (theory_opt.eq.2) then
        if (channel_opt.eq.1) then
          if (resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98-PPpi_S.tbl',data_file)
          elseif (resonance_opt.eq.1) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p33_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.2) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p11_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.3) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d13_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.4) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1535_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.5) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1635_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.6) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_f15_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.7) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d33_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.8) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_born_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.9) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_rho_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.10) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_omega_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.11) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p11_off-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.12) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d13_off-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.13) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1535_off-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.14) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_all_res_off-PPpi.tbl',data_file)
          endif
        endif
        if (channel_opt.eq.2) then
          if (resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.1) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p33_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.2) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p11_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.3) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d13_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.4) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1535_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.5) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1635_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.6) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_f15_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.7) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d33_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.8) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_born_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.9) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_rho_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.10) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_omega_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.11) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p11_off-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.12) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d13_off-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.13) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1535_off-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.14) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_all_res_off-NPpi.tbl',data_file)
          endif
        endif
        if (channel_opt.eq.3) then
          if (resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98-PNpi_S.tbl',data_file)
          elseif (resonance_opt.eq.1) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p33_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.2) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p11_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.3) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d13_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.4) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1535_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.5) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1635_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.6) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_f15_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.7) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d33_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.8) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_born_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.9) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_rho_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.10) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_omega_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.11) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_p11_off-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.12) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_d13_off-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.13) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_s11_1535_off-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.14) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid98_all_res_off-PNpi.tbl',data_file)
          endif
        endif
      endif

      if (theory_opt.eq.3) then
        if (channel_opt.eq.1) then
          if (resonance_opt.eq.-1) then
            print *, 'Enter spp_tbl/table: '
            read(5,'(a)') fname
            call revinm('CLAS_PARMS',
     1      fname,data_file)
          elseif (resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000-PPpi_S.tbl',data_file)
          elseif (resonance_opt.eq.1) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p33_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.2) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p11_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.3) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d13_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.4) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1535_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.5) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1635_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.6) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_f15_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.7) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d33_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.8) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_born_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.9) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_rho_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.10) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_omega_on-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.11) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p11_off-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.12) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d13_off-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.13) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1535_off-PPpi.tbl',data_file)
          elseif (resonance_opt.eq.14) then
            call revinm('CLAS_PARMS',
c     1      'spp_tbl/maid2000_all_res_off-PPpi.tbl',data_file)
     1      'spp_tbl/maid01-0.001Res-PPpi.tbl',data_file)
          endif
        endif
        if (channel_opt.eq.2) then
          if (resonance_opt.eq.-1) then
            print *, 'Enter spp_tbl/table: '
            read(5,'(a)') fname
            call revinm('CLAS_PARMS',
     1      fname,data_file)
          elseif( resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.1) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p33_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.2) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p11_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.3) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d13_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.4) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1535_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.5) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1635_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.6) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_f15_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.7) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d33_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.8) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_born_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.9) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_rho_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.10) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_omega_on-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.11) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p11_off-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.12) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d13_off-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.13) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1535_off-NPpi.tbl',data_file)
          elseif (resonance_opt.eq.14) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_all_res_off-NPpi.tbl',data_file)
          endif
        endif
        if (channel_opt.eq.3) then
          if (resonance_opt.eq.-1) then
            print *, 'Enter spp_tbl/table: '
            read(5,'(a)') fname
            call revinm('CLAS_PARMS',
     1      fname,data_file)
          elseif (resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000-PNpi_S.tbl',data_file)
          elseif (resonance_opt.eq.1) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p33_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.2) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p11_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.3) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d13_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.4) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1535_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.5) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1635_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.6) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_f15_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.7) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d33_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.8) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_born_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.9) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_rho_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.10) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_omega_on-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.11) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_p11_off-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.12) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_d13_off-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.13) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_s11_1535_off-PNpi.tbl',data_file)
          elseif (resonance_opt.eq.14) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2000_all_res_off-PNpi.tbl',data_file)
          endif
        endif
      endif

      if (theory_opt.eq.4) then

        if (channel_opt.eq.1) then
          if (resonance_opt.eq.-1) then
            print *, 'Enter spp_tbl/table: '
            read(5,'(a)') fname
            call revinm('CLAS_PARMS',
     1      fname,data_file)
          elseif (resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2003-PPpi.tbl',data_file)
          endif
        endif
        if (channel_opt.eq.3) then
          if (resonance_opt.eq.-1) then
            print *, 'Enter spp_tbl/table: '
            read(5,'(a)') fname
            call revinm('CLAS_PARMS',
     1      fname,data_file)
          elseif (resonance_opt.eq.0) then
            call revinm('CLAS_PARMS',
     1      'spp_tbl/maid2003-PNpi.tbl',data_file)
          endif
        endif
      endif
     
      if (theory_opt.ge.7) then
        print *, 'Enter spp_tbl/table: '
        read(5,'(a)') fname
        call revinm('CLAS_PARMS','spp_tbl/'//fname,data_file)
      endif      
            
      write(6,*) 'Enter max pi-N angular momentum (0-5): '

      read(5,*) mwave_L   

      write(6,*) 'Reading multipoles from ',data_file

      call read_sf_file(data_file,2)

      n_call = n_call + 1
      
100   call xsection
      
      sigu = sigma_t
      sigl = sigma_l
      sigt = sigma_tt
      sigi = sigma_lt  
     
      sigpt  = sigma_t_y+sigma_l_y
      sigpt  = sigpt+sigma_lt_x+sigma_lt_y+sigma_lt_z
      sigpt  = sigpt+sigma_tt_x+sigma_tt_y+sigma_tt_z      
      
      siget  = sigma_ltp_x+sigma_ltp_y+sigma_ltp_z
      siget  = siget+sigma_ttp_x+sigma_ttp_z
      
      sige   =  pol_elec*sigma_ltp  
      sigpt  =  pol_targ*sigpt
      siget  =  pol_elec*pol_targ*siget
      
      return
      end

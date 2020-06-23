      subroutine int_ctrl_4(ki_mag,theory_opt,channel_opt,resonance_opt)

      implicit none

      include 'spp.inc'

      real sigma0, sigma0_calc
      real ki_mag
      integer iw, ic, ip
      integer theory_opt, channel_opt, resonance_opt
      real sig00,sig0e,sig0t,sig0et
      real sig00_total, sig0e_total,sig0t_total,sig0et_total

      print *, 'Enter q**2 (GeV)^2: '
      read(5,*) q2
c      w = 1.23
c      csthcm = -0.5
c      phicm = 45.0
c      sigma0 =  sigma0_calc(ki_mag,theory_opt,
c     1            channel_opt,resonance_opt)
c      write(6,100) w,q2,epsilon,csthcm,phicm,sigma0,sigma0*0.1
c      write(21,100) w,q2,epsilon,csthcm,phicm,sigma0,sigma0*0.1

      do iw = 1, 30
c        w = 1.115+(iw-1)*0.03+0.0001
        w = 1.1+(iw-1)*0.02
c        do ic = 8,1,-1
c         csthcm = cos((10+(ic-1)*20)*3.14159/180.)
        do ic = 1,10
          csthcm = -0.9+(ic-1)*0.2
          do ip = 1,12
            phicm = 15.+(ip-1)*30
            sigma0 =  sigma0_calc(ki_mag,theory_opt,
     1                channel_opt,resonance_opt,
     1             sig00,sig0e,sig0t,sig0et)
            write(6,100) w,q2,epsilon,csthcm,phicm,sig00,sig0e,sig0t,
     &        sig0et
            write(21,100) w,q2,epsilon,csthcm,phicm,sig00,sig0e,sig0t,
     &        sig0et
          enddo
        enddo
      enddo

 100  format(5F10.3,4E12.3)
      end

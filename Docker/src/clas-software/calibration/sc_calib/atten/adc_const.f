c -------------------------------------------------------------
c     
c      ADCs constants
c 
c -------------------------------------------------------------

       program adc_constants
 
       real*4     gm,par,atten,sig,sig_att,sig_g
       real*4     gmean(1:300),param(1:300)
       real*4     atten_l(1:300),sigma(1:300)
       real*4     sigma_att(1:300),sigma_gm(1:300)
       real*4     c_l(1:300),sigma_l(1:300)
       real*4     c_r(1:300),sigma_r(1:300)       
       integer*2      i, k, pmt, pmt_n1(1:300)
       integer*2      pmt_id, pmt_n2(1:300)        
       character*100  gm_file,par_file,out_file
       
c -----------   INPUT  -----------------------------------------  

       write(6,*)' --------------------------------------------' 
       write(6,*)' type GMean data filename: '
       read (5,*)  gm_file
       write(6,*) gm_file
       write(6,*)' --------------------------------------------'

       open (unit=1, file= gm_file,status='old')

          do i=1,288
            read(1,*) pmt_id, gm, sig_g  
            pmt_n1(i) = i
            gmean(i) = gm
              if(sig_g.eq.0.0)then
              sigma_gm(i) = (gm*0.1)/2.35
              else
              sigma_gm(i) = sig_g
              endif
          end do

       do k=1,288
       write(6,*)pmt_n1(k), gmean(k) 
       end do

       close(unit=1)

       write(6,*)' --------------------------------------------' 
       write(6,*)' type input parameters data filename: '
       read(5,*) par_file
       write(6,*)' --------------------------------------------'

       open (unit=2, file= par_file,status='old')

       do i=1,288
         read(2,*)pmt,par,atten,sig,sig_att
         pmt_n2(i) = i
         param(i) = par
         atten_l(i) = atten
         sigma(i) = sig
         sigma_att(i) = sig_att
       end do
       
       do k=1,288
       write(6,*)pmt_n2(k),param(k),atten_l(k),sigma(k),
     #           sigma_att(k)   
       end do

       close(unit=2)

c  ----------- END OF INPUT ------------------------------
c
c  ----------- CALCULATIONS ------------------------------
c

           do i=1,288 
 
c  -------------- LEFT --------------
                       
         c_l(i) = gmean(i)*sqrt(exp(param(i)))

        sigma_l(i)=sqrt( exp(param(i))*(sigma_gm(i)**2.0)+
     #      (gmean(i)**2)*(sigma(i)**2.0)*exp(param(i)) ) 

c  -------------- RIGHT -------------

       c_r(i) = (gmean(i)**2.0)/c_l(i)      

       sigma_r(i)=sqrt((2.0*gmean(i)*sigma_gm(i)/c_l(i))**2.0+
     #  (((gmean(i)**2.0)*sigma_l(i))/(c_l(i)**2.0))**2.0)

           end do

c  ------ TEST ------

           do k=1,288
           write (6,*) k,c_l(k),c_r(k),sqrt(c_l(k)*c_r(k)),
     #     sigma_l(k),sigma_r(k)
           end do 
c           write (6,*) i, gmean, par 
c          gmean_test = sqrt(const_l(i)*const_r(i))
c          write (6,*) i,  gmean_test  


c  --------------- OUTPUT -----------------------------------

       write(6,*)' --------------------------------------------' 
       write(6,*)' type output filename:'
       read (5,*)  out_file
       write(6,*)' --------------------------------------------'

       open  (unit=3, file = out_file ,status='unknown' )

       do k=1,288
       write (3,*) k,c_l(k),sigma_l(k),c_r(k),sigma_r(k),
     #             atten_l(k),sigma_att(k)
       end do

       close (unit=3)

c -----------------------------------------------------------

       stop
       end







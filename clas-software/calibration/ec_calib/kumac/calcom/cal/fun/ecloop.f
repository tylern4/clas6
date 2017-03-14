      real function ecloop()
      
      real sec(6),adc(6)      
      include ?
            
      do s = 1,6
        sec(s) = 0
        adc(s) = 0
      enddo
      
      do i = 1,nec
        if (adcec(i).gt.0.and.tdcec(i).gt.0) then
        sec(secec(i)) = sec(secec(i)) + 1
        adc(secec(i)) = adc(secec(i)) + adcec(i)
        endif
      enddo
      
      do s = 1,6
        call hf2(1,sec(s),s,1.)
        call hf2(2,adc(s),s,1.)
      enddo
      
      end


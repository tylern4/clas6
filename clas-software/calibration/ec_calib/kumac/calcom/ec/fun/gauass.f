      function gauass(x)
      common/pawpar/par(4)
      if (x.gt.par(2)) then
        gauass = par(1)*exp(-0.5*((x-par(2))/par(4))**2)
      else
        gauass = par(1)*exp(-0.5*((x-par(2))/par(4)*par(3))**2)
      endif
      end
      
      
       
      

      subroutine ecbooktest
      
      character*80 t10000
      
      t10000 = 'EC;TEST NUMBER;SECTOR'
      
      call hmdir('//PAWC/ECTEST','S')
      
      call hbook2(10000,t10000,100,1.,100.,6,1.,7.,0.)
      
      call hcdir('//PAWC',' ')
      
      end
      

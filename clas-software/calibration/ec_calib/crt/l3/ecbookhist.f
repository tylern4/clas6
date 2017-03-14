      subroutine ecbookhist
      
      integer i
      character*80 title
      character*80 t1001,t1002,t1501,t102,t103
      character*8 fstring(6)
      
      t102  = ' EC;TDC;(LAYER-1)*36+PMT'
      t103  = ' EC;ADC;(LAYER-1)*36+PMT'
      t1501 = ' EC;PIXEL NO.;LAYER'
      t1001 = ' EC;INNER PIXEL NO.;'
      t1002 = ' EC;OUTER PIXEL NO.;'
      
      do i = 1,6
        write(fstring(i),20) 'SECTOR ',i
      enddo
      
      call hmdir('//PAWC/EC','S')
      
      call hbook2(2,t1501,36,1.,37.,36,1.,37.,0.)
      call hbook2(3,t1501,36,1.,37.,36,1.,37.,0.)
      
      do i = 1,6
        title = fstring(i)//t102
        call hbook2(i*100+2,title,200,0.,4000.,216,1.,217.,0.)
        title = fstring(i)//t103
        call hbook2(i*100+3,title,100,0.,300.,216,1.,217.,0.)
        title = fstring(i)//t1001
        call hbook1(i*1000+1,title,1296,1.,1297.,0.)
        title = fstring(i)//t1002
        call hbook1(i*1000+2,title,1296,1.,1297.,0.)        
        title = fstring(i)//t1501
        call hbook2(i*1000+501,title,1296,1.,1297.,6,1.,7.,0.)
        call hbook2(i*1000+502,title,1296,1.,1297.,6,1.,7.,0.)
      enddo
        
      call hcdir('//PAWC',' ')
      
20    format(a7,i1)

      end
      

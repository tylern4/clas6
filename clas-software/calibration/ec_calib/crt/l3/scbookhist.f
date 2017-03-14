      subroutine scbookhist
          
      character*80 title
      character*80 t1001,t1002,t1003,t1004
      character*80 t102,t103,t104,t105,t106,t107
      character*8 fstring(6)
      
      do i = 1,6
        write(fstring(i),20) 'SECTOR ',i
      enddo
      
      call hmdir('//PAWC/SC','S')
      
      t102  = ' SC;TDCL;PMT'
      t103  = ' SC;ADCL;PMT'
      t104  = ' SC;TDCR;PMT'
      t105  = ' SC;ADCR;PMT'
      t1001 = ' SC;SQRT(ADCL*ADCR);PMT'
      t1002 = ' SC;150*LOG(ADCL/ADCR);PMT'
      t1003 = ' SC;0.36*(TDCL-TDCR);PMT'
      
      do i = 1,6
        title = fstring(i)//t102
        call hbook2(i*100+2,title,200,10.,3000.,48,1.,49.,0.)
        title = fstring(i)//t103 
        call hbook2(i*100+3,title,100,0.,2500.,48,1.,49.,0.)
        title = fstring(i)//t104
        call hbook2(i*100+4,title,200,10.,3000.,48,1.,49.,0.)
        title = fstring(i)//t105
        call hbook2(i*100+5,title,100,0.,2500.,48,1.,49.,0.)
        title = fstring(i)//t1001
        call hbook2(i*1000+1,title,100,0.,2500.,48,1.,49.,0.)
        title = fstring(i)//t1002
        call hbook2(i*1000+2,title,100,-400.,400.,48,1.,49.,0.)
        title = fstring(i)//t1003
        call hbook2(i*1000+3,title,100,-300.,300.,48,1.,49.,0.)
      enddo
      
      call hcdir('//PAWC',' ')
     
20    format(a7,i1)
       
      end

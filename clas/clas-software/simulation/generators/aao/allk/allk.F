      implicit real*8(a-b,d-h,o-z)
      implicit complex*16(c)
      character tag*30,pfile*30
      real qinput,qout,eout
      integer pinput

      common / cgampi1 /grnn,gonn,grpg,gopg,gdm1,gde2,gdc2,swgm(8)
     &  ,fdelgm,fmrho,fmomg,xkrho,xkomg,phpi,vnnrho,vnnomg
     &  ,mnnrho,mnnomg,lambda,meshgm,lmaxgm
      common / c2001 / wcmmev(1000),qsgev(1000),fact(1000),
     &ebeam,qmin,wmin,delq,delw,meshq,meshw,imaxdt
                      
      read(5,*) qinput,pinput,pfile,tag
       
      call setpara(qinput,pinput,pfile) 
     
      if (qinput.le.-2) then
        call allk_fit_run(qinput,qout,eout)
        qmin = dble(qout)
        ebeam = dble(eout)
      endif
      
      if (qinput.ge.-3) then      
        call tabmesh
      
        print *, 'ALLK: Multipoles tabulated for Q2= ',qmin
      
        do nx=1,imaxdt
          win   = wcmmev(nx) 
          qin   = qsgev(nx) 
          call allkqw(win,qin,nx)   
        enddo
            
        call allk_output
        call mulwrite(lmaxgm,tag)
      endif
      
      if (qinput.lt.-3) then
        call hmpol_init
70      call hmpol_plot
        call hmpol_pick
        go to 70
      endif
           
      stop
      
      end
      
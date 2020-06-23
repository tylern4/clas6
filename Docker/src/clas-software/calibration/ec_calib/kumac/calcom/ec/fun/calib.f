      integer function calibhist()
      
      logical electron
      
      real mom_dc, nstrips
      integer lay,io
      
      include ?
      
      electron = .false.
      adcsum_ec = 0.
      nstrips = 0
            
      do i = 1,gpart
        iec = ec(i)
        idc = dc(i)
        if (q(i).lt.0.and.cc(i).gt.0) then
          electron 	= .true.
          sec_ec 	= ec_sect(iec)
          hit_ec 	= ec_whol(iec)
          etot_ec 	= etot(iec)
          x_ec		= ech_x(iec)
          y_ec		= ech_y(iec)
          mom_dc 	= p(i)
        endif
      enddo
      
      if (.not.electron) return    
        
      bankid1 = sec_ec*100+10+hit_ec
      bankid2 = sec_ec*100+20+hit_ec 
            
      do i = 1,ecpc_nhit
        io =  mod(ecpc_hit(i),100.)/10
        if (ecpc_hit(i).eq.bankid1.or.ecpc_hit(i).eq.bankid2) then
          if (ecpc_tdc(i).ge.0) then
            lay = ecpc_id(i)/100
            adc_ec(lay,io) = ecpc_adc(i) + adc_ec(lay,io)
            adcsum_ec = adcsum_ec + ecpc_adc(i)
            nstrips(lay,io) = nstrips(lay,io) + 1
          endif
        endif
      enddo
            
      e_ec 	= etot_ec/0.273
      e_raw 	= adcsum_ec*0.0001/0.273
      rat 	= e_ec/mom_dc
      rat2	= abs(e_ec/max(0.01,e_raw))
      rat3   	= abs(e_ec/max(0.01,mom_dc))
      rat4	= abs(e_raw/max(0.01,mom_dc))
      
      call hf1(200,rat,1.)
      if (rat.gt.0.8) then      
      call hf2(201,nstrips,sec_ec,1.)
      call hf2(100,mom_dc,e_raw,1.)
      call hf2(101,mom_dc,e_ec,1.)
      call hf2(102,e_raw,e_ec,1.)
      call hf2(103,-x_ec,y_ec,1.)
      call hf2(104,-x_ec,y_ec,rat2)
      call hf2(105,-x_ec,y_ec,rat3)
      call hf2(106,-x_ec,y_ec,rat4)
      endif
      
c      if (rat.lt.0.8) call hf2(106,-x_ec,y_ec,1.)
      
      end
      
      integer function calibgetdata()
      
      include ?
      logical electron,goodev,firstime
      real mom_dc
      integer sec
      
      vector getsec
      vector pcal
      vector gcal
      vector adccal(10000,20)
      vector adcpmt(10000,20)
            
      electron = .false.
      adcsum_ec = 0.
      sec	= getsec(1)
      
      do i = 1,gpart
        iec = ec(i)
        idc = dc(i)
        if (q(i).lt.0.and.cc(i).gt.0) then
          electron 	= .true.
          sec_ec 	= ec_sect(iec)
          hit_ec 	= ec_whol(iec)
          etot_ec 	= etot(iec)
          x_ec		= ech_x(iec)
          y_ec		= ech_y(iec)
          mom_dc 	= p(i)
        endif
      enddo
      
      goodev	= electron.and.sec_ec.eq.sec
      
      if (.not.goodev) return    
      
      bankid1 = sec_ec*100+10+hit_ec
      bankid2 = sec_ec*100+20+hit_ec 
       
      nstr = 0
      firstime = .true.

      do i = 1,ecpc_nhit
        if (ecpc_hit(i).eq.bankid1.or.ecpc_hit(i).eq.bankid2) then
          if (ecpc_tdc(i).gt.0) then
            if (firstime) then
              nevt 		= nevt + 1
              firstime 		= .false.
              pcal(nevt) 	= mom_dc
            endif
            nstr = nstr+1
            if (nstr.le.20) then
              adccal(nevt,nstr) = ecpc_adc(i)
              adcpmt(nevt,nstr) = ecpc_id(i)
            endif
          endif
        endif
      enddo
      
      end
      
      integer function caliblist()
      
      include ?
            
      do i = 1,gpart
        iec = ec(i)
        idc = dc(i)      
        if (iec.gt.0.and.idc.gt.0) then
          print *, 'Sec:',ec_sect(iec),' Hit:',ec_whol(iec),
     +' EC energy:',etot(iec),' EC time: ',ec_t(iec),' DC mom: '
     +,q(i)*p(i),' ID: ',id(i) 
        endif      
      enddo
      
      do i = 1,ecpc_nhit
        print *, 'ecpc ntup row:',i,' bank:',ecpc_hit(i),' id:',
     +ecpc_id(i),' adc:',ecpc_adc(i), ' tdc:',ecpc_tdc(i)
      enddo
      
      print *, ''
      
      end
      
        
      

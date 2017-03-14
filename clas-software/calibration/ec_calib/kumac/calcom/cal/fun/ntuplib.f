      integer function nnsc(sec)
      
      integer sec
      
      include ?
      
      nnsc = 0
      
      do i = 1,nsc
        if (secsc(i).eq.sec) then
          nnsc = nnsc + 1
        endif
      enddo
      
      end
      
      integer function nnecl(io,sec)
            
      integer sec,io
      
      include ?
      
      nnecl = 0
      
      do i = 1,nec
        lay = layerec(i)
        if (secec(i).eq.sec) then
          if(io.eq.0.and.adcec(i).gt.0) then
            nnecl = nnecl + 1
          endif
          if(io.eq.1.and.lay.le.3.and.adcec(i).gt.0) then
            nnecl = nnecl + 1 
          endif
          if(io.eq.2.and.lay.ge.4.and.adcec(i).gt.0) then
            nnecl = nnecl + 1
          endif
        endif
      enddo
      
      end
      
      integer function nnec(sec)
      
      integer sec
      
      include ?
      
      nnec = 0
      
      do i = 1,nec
        if (secec(i).eq.sec) then
          if (adcec(i).gt.0) nnec = nnec + 1
        endif
      enddo
      
      end
      
      integer function nncc(sec)
      
      integer sec
      
      include ?
      
      nncc = 0
      
      do i = 1,ncc
        if (seccc(i).eq.sec) then
          nncc = nncc + 1
        endif
      enddo
      
      end
      
      logical function pix(n1,io,sec)
      
      include ?
      
      integer sec,n1,io,n(6)
      
      pix = .false.
      
      do i = 1,6
        n(i)= 0
      enddo
      
      do i = 1,nec
        lay = layerec(i)
        if (secec(i).eq.sec) then
            n(lay) = n(lay) + 1
        endif
      enddo
      
      if (io.eq.1) then
        if (n(1).eq.n1.and.n(2).eq.n1.and.n(3).eq.n1) pix = .true.
      elseif (io.eq.2) then
        if (n(4).eq.n1.and.n(5).eq.n1.and.n(6).eq.n1) pix = .true.
      endif
      
      END

      integer function ecsum(nopt1,nopt2,io,sec)
      
      integer sec,adcsumm,io,nopt1,nopt2

      include ?
      
      ecsum = 0
      adcsumm = 0
      n = 0
      
      do i = 1,nec
        if (secec(i).eq.sec) then
          if (io.eq.0) then
            n = n+1
            adcsumm = adcsumm + adcec(i)
          elseif (io.eq.1.and.layerec(i).le.3) then
            n = n+1
            adcsumm = adcsumm + adcec(i)
          elseif (io.eq.2.and.layerec(i).ge.4) then
            n = n+1
            adcsumm = adcsumm + adcec(i)
          endif
        endif
      enddo
      
      if (n.ge.nopt1.and.n.le.nopt2) ecsum = adcsumm 
      
      end
      
      integer function scsum(ibar,sec)
      
      integer sec,gmean,ibar
      logical adcl,adcr,tdcl,tdcr
      
      include ?
      
      gmean = 0
      
      do i = 1,nsc
        if (secsc(i).eq.sec.and.idsc(i).eq.ibar) then
          adcl = adclsc(i).gt.0
          adcr = adcrsc(i).gt.0
          tdcl = tdclsc(i).gt.0.and.tdclsc(i).lt.4095
          tdcr = tdcrsc(i).gt.0.and.tdcrsc(i).lt.4095
          if (adcl.and.adcr.and.tdcl.and.tdcr) then
            gmean = gmean + sqrt(float(adclsc(i)*adcrsc(i)))            
          endif
        endif
      enddo
      
      scsum = gmean
      
      end          
      
      integer function ccsum(nopt,sec)
      
      integer sec,adcsumm

      include ?
      
      ccsum = 0
      adcsumm = 0
      n = 0
      
      do i = 1,ncc
        if (seccc(i).eq.sec) then
          n = n+1
          adcsumm = adcsumm + adccc(i)
        endif
      enddo
      
      if (n.gt.nopt) ccsum = adcsumm 
      
      end

      integer function ccid(id,sec)
      integer id,sec
      include ?
      
      ccid = 0

      do i = 1,ncc
        if (seccc(i).eq.sec.and.idcc(i).eq.id) ccid=adccc(i)
      enddo

      end

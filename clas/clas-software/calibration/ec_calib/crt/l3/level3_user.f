c  level3_user.f

c  user entry points to analyze events coming from level 3 fifo

c  ejw, 5-nov-1997


c------------------------------------------------------------------------


      subroutine level3_init
      
      include 'ECtst.inc'

      implicit none
      
      character*80 rznout
      integer istat,ntupid,s
      integer current_run
      
      integer space
      parameter (space=5000000)
      integer hmemor
      common/pawc/hmemor(space)
      
c      data ntupid/2000/
      
c      write(*,'(a)') 'Enter ntuple filename: '
c      read(*,'(a)') rznout
      
c      call hlimit(space)
      call hlimap(space,'COSM')
c      call hropen(1,'CWN',rznout,'N',8191,istat)
c      call hbnt(ntupid,'COSMIC',' ')
      
c      call ecbookcwn(ntupid)
c      call scbookcwn(ntupid)
      call ecbookhist
      call ecbooktest
      call scbookhist
      
      do s = 1,6
        ectest(100,s) = 1
      enddo
            
      call ec_bor
      call sc_bor

      return
      end


c------------------------------------------------------------------------


      subroutine level3_packev(ctl,event,evlen)

      implicit none

      integer ctl(4)
      integer event(*)
      integer evlen

      integer head,find_bank_in_dd,nwrds,i,j,k,nev,nw
      integer sec,ped,pedl,pedr,ntrig
      real    sl
      
      include 'ECnt.inc'
      include 'eccal.inc'
      include 'SCnt.inc'
      include 'sccal.inc'

      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      equivalence (I16(1), I32(1))

      data ntrig/1/

c  executable code:
c  ----------------

      nev=nev+1
      if (nev.eq.ntrig) then
        print *,'Event number: ',nev
        ntrig = ntrig+500000
        call level3_done
      endif
                  
c      head=find_bank_in_dd(event,'HEAD',0,nwrds)
c      if(head.gt.0)then
c         print *,'Head bank length: ',nwrds
c         print *,'Head bank words:  ',(event(i),i=head+1,head+nwrds)
c      else
c         print *,'Couldn''t find HEAD bank!'
c      endif

      nec	= 0
      
      do sec = 1,6      
        head=find_bank_in_dd(event,'EC  ',sec,nwrds)
        do j = 1,nwrds
          I32(j) = event(head+j)
        enddo
        nw = nwrds*2/3
        k = 1
        do i = 1,nw
          nec		= nec + 1
          secec(nec)	= sec
          layec(nec)	= I16(k)/256
          strec(nec)	= mod(I16(k),256)
          ped		= ecped(strec(nec),layec(nec),secec(nec))
          tdcec(nec)	= I16(k+1)
          adcec(nec)	= max(0.,I16(k+2)-ped)
          k		= k + 3
        enddo
      enddo
      
      nsc	= 0      
           
      do sec = 1,6      
        head=find_bank_in_dd(event,'SC  ',sec,nwrds)
        do j = 1,nwrds
          I32(j) = event(head+j)
        enddo
        k = 1
        nw = nwrds*2/5 
        do i = 1,nw
          nsc		= nsc + 1
          secsc(nsc)	= sec
          idsc(nsc)	= I16(k)
          pedl		= scped(idsc(nsc),1,secsc(nsc))
          pedr		= scped(idsc(nsc),2,secsc(nsc))
          tdclsc(nsc)	= I16(k+1)
          adclsc(nsc)	= I16(k+2)-pedl
          tdcrsc(nsc)	= I16(k+3)
          adcrsc(nsc)	= I16(k+4)-pedr
          k		= k + 5
        enddo
      enddo
      
      return
      end
      
c------------------------------------------------------------------------
      
      subroutine level3_analyze()
            
      call ec_store
      call ec_getpix
      call ec_fillhist
      call sc_fillhist
      
      end
      
c------------------------------------------------------------------------

      subroutine ec_store
      
      include 'ECnt.inc'
      include 'ECstore.inc'
      include 'ECtst.inc'
      
      real tdc,adc,chan
      integer s,lay,str,ii,sl,thr(2)
      
      data thr/20,40/      
      
      do s = 1,6
        msec(s) = 0
        do lay = 1,6
          mec(lay,s) = 0
        enddo
      enddo
      
      call hcdir('//PAWC/EC',' ')

      do n = 1,nec
        s   		= secec(n)
        lay 		= layec(n)
        str 		= strec(n)
        chan		= float(str+36*(lay-1))
        adc		= float(adcec(n))
        tdc		= float(tdcec(n))
        io		= 1
        if (lay.gt.3) io = 2        
        if (adc.gt.thr(io)) then
          msec(s)	= msec(s) + 1
          mec(lay,s) 	= mec(lay,s) + 1
          ii 		= mec(lay,s)
          echits(ii,lay,s) = str
          ectdc(ii,lay,s)  = tdc
          ecadc(ii,lay,s)  = adc
          sl		   = (s-1)*6+lay
          call hf2(2,float(str),float(sl),1.)
          call hf2(3,float(str),float(sl),adc)
        endif
      enddo
      
      call hcdir('//PAWC',' ')       
      
      end
      
c------------------------------------------------------------------------

      subroutine sc_store
      
      include 'SCnt.inc'
      include 'SCstore.inc'
      
      logical adcok
      
      do i = 1,6
        msc(i) = 0
      enddo
      
      do n = 1,nsc
        sec = secsc(n)
        adcok = adclsc(n).gt.30.and.adcrsc(n).gt.30
        if (adcok) then
          msc(sec) = msc(sec) + 1
          ii = msc(sec)
          schits(ii,sec)	= idsc(n)
          scadc(ii,1,sec)	= adclsc(n)
          scadc(ii,2,sec)	= adcrsc(n)
          sctdc(ii,1,sec)	= tdclsc(n)
          sctdc(ii,2,sec)	= tdcrsc(n)
        endif
      enddo
      
      end      
      
c------------------------------------------------------------------------

      subroutine ec_getpix
      
      include 'ECstore.inc' 
      include 'ECpix.inc'
      include 'ECtst.inc'
      
      integer puid,pvid,pwid,u,v,w,dalitz,io 
      integer s,off,off1,off2,off3,ii
      
      do s = 1,6
        do io = 1,2
          mpix(io,s) = 0
          esum(io,s) = 0
        enddo
      enddo
      
      do s = 1,6      
c        do i = 1,38
c          ectest(i,s) = 0
c        enddo
c        ectest(98,s) = 0
c        ectest(99,s) = 0
c        if (msec(s).gt.0) ectest(99,s) = 1 
        if (msec(s).gt.5.and.msec(s).le.10) then
c          ectest(98,s) = 1      
          do io=1,2
            off		= 3*(io-1)
            off1	= off+1
            off2	= off+2
            off3	= off+3        
            do puid = 1,mec(off1,s)
              u = echits(puid,off1,s)          
              do pvid = 1,mec(off2,s)
                v = echits(pvid,off2,s)
                do pwid = 1,mec(off3,s)            
                  w = echits(pwid,off3,s)              
                  dalitz = u+v+w              
                  if (dalitz.eq.73.or.dalitz.eq.74) then 
                    mpix(io,s) 		= mpix(io,s) + 1
                    ii 			= mpix(io,s)
                    ecadcpix(ii,off1,s) = ecadc(puid,off1,s)
                    ecadcpix(ii,off2,s) = ecadc(pvid,off2,s)
                    ecadcpix(ii,off3,s) = ecadc(pwid,off3,s)
                    esumpix(ii,io,s)  	= ecadc(puid,off1,s) + 
     &					  ecadc(pvid,off2,s) +
     &					  ecadc(pwid,off3,s)
                    esum(io,s) 		= esum(io,s) + esumpix(ii,io,s)
                    ecpixel(ii,io,s) 	= u*(u-1)+v-w+1                
                  endif                            
                enddo
              enddo
            enddo
          enddo
c         call ec_test(s)
        endif      
      enddo
      
      end
      
c------------------------------------------------------------------------

      subroutine ec_fillhist
      
      include 'ECtst.inc'
      include 'ECstore.inc'
      include 'ECpix.inc'
            
      real radc(3),pixel,adc,tdc,strip,chan
      integer n,ii,s,id3,io,off,off1,off2,off3,lay
      logical pixtest1,pixtest2
      
      call hcdir('//PAWC/EC',' ')
             
      do s = 1,6 
        pixtest1 = mpix(1,s).eq.1.and.mpix(2,s).eq.1
        pixtest2 = ecpixel(1,1,s).eq.ecpixel(1,2,s)
        if(pixtest1.and.pixtest2) then     
c        if (ectest(31,s).eq.1) then
          id2 = 100*s      
          id3 = 1000*s
          do lay = 1,6
            do n = 1,mec(lay,s)
              strip = float(echits(n,lay,s))
              tdc   = float(ectdc(n,lay,s))
              adc   = float(ecadc(n,lay,s))
              chan  = float(strip+36*(lay-1))
              call hf2(id2+2,tdc,chan,1.)
              call hf2(id2+3,adc,chan,1.)
            enddo
          enddo
          do io = 1,2
            off  = 3*(io-1)
            off1 = off+1
            off2 = off+2
            off3 = off+3
            do n = 1,mpix(io,s)
              pixel = float(ecpixel(n,io,s))
              call hf1(id3+io,pixel,1.)
              do ii = 1,3
                radc(ii) = float(ecadcpix(n,off+ii,s))
                call hf2(id3+501,pixel,float(off+ii),radc(ii))
                call hf2(id3+502,pixel,float(off+ii),radc(ii)**2)
              enddo
            enddo
          enddo
        endif
      enddo
      
      call hcdir('//PAWC',' ')       
c      call hcdir('//PAWC/ECTEST',' ')
c      call hpakad(10000,ectest)
c      call hcdir('//PAWC',' ')
      
      end
c-----------------------------------------------------------------------

      subroutine sc_fillhist
      
      include 'SCstore.inc'
      include 'ECpix.inc'
      
      real gmean, lograt, tdiff
      integer oppsec(6),s,goodsector,oppsector
      
      data oppsec/4,5,6,1,2,3/ 
      
      goodsector = -1
      oppsector  = 0
      
      do s = 1,6
        if (mpix(1,s).eq.1.and.mpix(2,s).eq.1) then
          goodsector = s
          oppsector  = oppsec(s)
        endif  
      enddo
      
      if (goodsector.ne.0.) then 
        call sc_store
      else
        return
      endif
            
      call hcdir('//PAWC/SC',' ')      
     
      do s = 1,6
        if (s.gt.goodsector.or.s.eq.oppsector) then 
          id2 = 100*s
          id3 = 1000*s
          do n = 1,msc(s)
            bar  = float(schits(n,s))
            tdcl = float(sctdc(n,1,s))
            adcl = float(scadc(n,1,s))
            tdcr = float(sctdc(n,2,s))
            adcr = float(scadc(n,2,s))
        
            call hf2(id2+2,tdcl,bar,1.)
            call hf2(id2+3,adcl,bar,1.)
            call hf2(id2+4,tdcr,bar,1.)
            call hf2(id2+5,adcr,bar,1.)
          
            gmean  = sqrt(adcl*adcr)
            lograt = 150*alog(adcl/adcr)
            tdiff  = 0.36*(tdcl-tdcr)
          
            call hf2(id3+1,gmean,bar,1.)
            call hf2(id3+2,lograt,bar,1.)
            call hf2(id3+3,tdiff,bar,1.)
          enddo
        endif          
      enddo
      
      call hcdir('//PAWC',' ')
      
      end

c------------------------------------------------------------------------

      subroutine ec_test(s)
      
      logical mi1,mi2,mi3,mo1,mo2,mo3
      logical mip1,mip2,mip3,mip4,mip5,mip6
      logical mipi,mipo,mip,pix
      logical uv,vw,uw,xy,yz,xz,uvw1,xyz1,uvw2,xyz2
      logical pixi0,pixi1,pixi2,pixin
      logical pixo0,pixo1,pixo2,pixon
      logical etest
      logical test(38)
      
      include 'ECstore.inc'
      include 'ECpix.inc'
      include 'ECtst.inc'

      integer s
      
      mi1 = mec(1,s).gt.0
      mi2 = mec(2,s).gt.0
      mi3 = mec(3,s).gt.0
      mo1 = mec(4,s).gt.0
      mo2 = mec(5,s).gt.0
      mo3 = mec(6,s).gt.0
      
      mip1 = mec(1,s).eq.1
      mip2 = mec(2,s).eq.1
      mip3 = mec(3,s).eq.1
      mip4 = mec(4,s).eq.1
      mip5 = mec(5,s).eq.1
      mip6 = mec(6,s).eq.1

      mipi = mip1.and.mip2.and.mip3
      mipo = mip4.and.mip5.and.mip6
      
      mip  = mipi.and.mipo
      
      uvw1 = mi1.or.mi2.or.mi3
      xyz1 = mo1.or.mo2.or.mo3

      uvw2 = mi1.and.mi2.and.mi3
      xyz2 = mo1.and.mo2.and.mo3

      uv   = mi1.or.mi2
      vw   = mi2.or.mi3
      uw   = mi1.or.mi3
      
      xy   = mo1.or.mo2
      yz   = mo2.or.mo3
      xz   = mo1.or.mo3
      
      pixi0 = mpix(1,s).eq.0
      pixi1 = mpix(1,s).eq.1
      pixi2 = mpix(1,s).eq.2
      pixin = mpix(1,s).ge.1
      pixo0 = mpix(2,s).eq.0
      pixo1 = mpix(2,s).eq.1
      pixo2 = mpix(2,s).eq.2
      pixon = mpix(2,s).ge.1
      
      pix   = pixi1.and.pixo1
      
      etest = .true.
      
      if (etest) then

      test(1)   = mi1
      test(2)   = mi2
      test(3)   = mi3
      test(4)   = mo1
      test(5)   = mo2
      test(6)   = mo3      
      test(7)   = uvw1
      test(8)   = xyz1
      test(9)   = uvw1.or.xyz1
      test(10)  = uvw1.and.xyz1
      test(11)  = uvw2
      test(12)  = xyz2
      test(13)  = uvw2.or.xyz2
      test(14)  = uvw2.and.xyz2
      test(15)  = uv.and.vw.and.uw.and.xy.and.yz.and.xz
      test(16)  = mip1
      test(17)  = mip2
      test(18)  = mip3
      test(19)  = mip4
      test(20)  = mip5
      test(21)  = mip6
      test(22)  = mipi
      test(23)  = mipo 
      test(24)  = mipi.or.mipo
      test(25)  = mip
      test(26)  = pixin
      test(27)  = pixon
      test(28)  = pixi1
      test(29)  = pixo1
      test(30)  = pixi1.or.pixo1
      test(31)  = pix
      test(32)  = pixi1.and.pixo0
      test(33)  = pixi0.and.pixo1
      test(34)  = pixi1.and.pixon
      test(35)  = pixin.and.pixon
      test(36)  = mip.and.pix
      test(37)  = mip.and.(.not.pix)
      test(38)  = pix.and.(.not.mip)

      do i = 1,38
        if (test(i)) ectest(i,s) = 1
      enddo
      
      endif
      
      end

c------------------------------------------------------------------------

      subroutine level3_done
      
      call hcdir('//PAWC',' ')
      call hrput(0,'hist/cosmictest.rzh','T')

      return
      end


c------------------------------------------------------------------------



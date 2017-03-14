      subroutine dctrk_book()

      implicit none
      integer isec
      character*80 title
c  Booking DC and TRK related histograms to monitor
c   data processing

c book for the number of words in DC bank.

      call hcdir('//PAWC',' ')
      call hmdir('//PAWC/DC','S')
      do isec=1,6
         write(title,'(A,I1)') 'hits in DC0 - S',isec
         call hbook1(1000+isec,title,101,-0.5,100.5,0.0)
         write(title,'(2(A,I1),A)') 'hits in DC0 - S',isec,
     &              ' (with track found in S',isec,')'
         call hbook1(1010+isec,title,101,-0.5,100.5,0.0)
         write(title,'(A,I1)') 'DC0 layer vs wire - S',isec
         call hbook2(1020+isec,title, 192,0.5,192.5, 36,0.5,36.5,0.)
      enddo

c  book histograms for electron (q=-1)
      call hcdir('//PAWC',' ')
      
      call hmdir('//PAWC/TBTEL','S')
      call hbook_dobook_tbt
      
      call hcdir('//PAWC',' ')

c  book histograms for proton (q=+1) 

      call hmdir('//PAWC/TBTPR','S')
      call hbook_dobook_tbt
c
      call hcdir('//PAWC',' ')
c      
      return
      end


      subroutine hbook_dobook_tbt

c  Book histograms for TBTR and DCLA bank information (Hit Based Tracking) 
c

      implicit none

      logical  gelec, gcc
     
      character*80 hist_title
      integer Sec, Reg, Sup, HistID, ntimebin

      character*2 SL(2)
      real rawtdcLL(6), rawtdcHL(6),fitdocaLL(6),fitdocaHL(6)
c  the limits for raw TDC's may have to change 

      data rawtdcLL /-50., -50., -50., -50., -50., -50./  ! tdc low limit for R1/2/3
      data rawtdcHL /400., 400.,900., 900., 1400., 1400./  ! tdc high limit for R1/2/3
      data fitdocaLL /-1., -1., -1.5, -1.5, -2.5, -2.5 / ! fit DOCA low limit R1/2/3
      data fitdocaHL /1., 1.,1.5,1.5, 2.5, 2.5 / ! fit DOCA high limit R1/2/3

              
       HistID = 7001
       hist_title = 'TBT momentum (GeV)  '
       call hbook1(HistID, hist_title, 40, 0., 4., 0.)

       do Sec=1,6
         write(hist_title,'(A,I2)') 'TBT momentum for sector',Sec
         call hbook1(7001+Sec*100, hist_title, 100, 0., 4., 0.)
       enddo

       HistID = 7002
       hist_title = 'TBT phi  '
       call hbook1(HistID, hist_title, 120, -180., 180., 0.)

       HistID = 7003
       hist_title = 'TBT Z at vertex (cm)  '
       call hbook1(HistID, hist_title, 100, -70., 30., 0.) 

	HistID = 7004
	hist_title='TBT Chi2 for all sectors average'
	call hbook1(HistID,hist_title,50,0.,10.,0.)	

      do Sec = 1,6

c  book the TBT histograms for TBTR bank info

       HistID = 81 + Sec*100 + 7000
       write(hist_title,'(A,I1)') 'TBT Chi2 pdf  Sec',sec
       call hbook1(HistID, hist_title, 50, 0., 10., 0.)
      
c  book 2D for layer vs wire

       HistID = 32 + Sec*100 + 7000 
       write(hist_title,'(A,I1)') 'TBT layer vs wire  Sec',sec
       call hbook2(HistID, hist_title, 192, 0.5, 192.5,
     &      36, 0.5, 36.5, 0.)


c  book histograms for 6 superlayers in the sector

       do sup = 1,6

         if(sec.eq.1) then
            HistID = 45 + sup*1000 
            write(hist_title,'(A,I1,A)') 'TBT residual  SL',sup,
     &                                          ' all sectors'
            call hbook1(HistID, hist_title, 100, -.25, .25, 0.)
            HistID = 46 + sup*1000 
            write(hist_title,'(A,I1,A)') 'TBT residual  SL',sup,
     &           ' all sectors (angle cut)'
            call hbook1(HistID, hist_title, 100, -.25, .25, 0.)
         endif

         HistID = 45 + Sec*100 + sup*1000 
         write(hist_title,'(2(A,I1))') 'TBT residual  SL',sup,'S',sec
         call hbook1(HistID, hist_title, 100, -.25, .25, 0.)
         HistID = 46 + Sec*100 + sup*1000 
         write(hist_title,'(2(A,I1),A)') 'TBT residual  SL',sup,'S',sec,
     &      ' (angle cut)'
         call hbook1(HistID, hist_title, 100, -.25, .25, 0.)
         
         ntimebin = (rawtdcHL(sup) - rawtdcLL(sup))/5
         HistID = 41 + Sec*100 + sup*1000 
         write(hist_title,'(2(A,I1))') 'TBT drift time  SL',sup,'S',sec
         call hbook1(HistID,hist_title,ntimebin,rawtdcLL(sup),rawtdcHL(sup),0.)  

c  book histograms for residuals Vs drifttime 

        HistID = 43 + Sec*100 + sup*1000
        write(hist_title,'(2(A,I1))') 'Residuals Vs time SL',sup,'S',sec
        call hbook2(HistID,hist_title,100,-10.,rawtdcHL(sup),40,-.25,.25, 0.)
       
c  book histograms for fitDOCA Vs resi

       HistID = 44 + Sec*100 + sup*1000
       write(hist_title,'(2(A,I1))')'Residuals Vs fitDOCA SL',sup,'S',sec
        call  hbook2(HistID,hist_title,100,fitdocaLL(sup),fitdocaHL(sup),
     &       40,-.25, .25,0.)    	
         
       enddo
       enddo
      return
      end

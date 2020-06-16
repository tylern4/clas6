      subroutine dctrk_book()

c  Booking DC and TRK related histograms to monitor
c   data processing

c book for the number of words in DC bank.

      call hcdir('//PAWC',' ')
      call hmdir('//PAWC/DC','S') 
      call hbook1(1001,'DC0 - S1',200,0.0,200.0,0.0)
      call hbook1(1002,'DC0 - S2',200,0.0,200.0,0.0)
      call hbook1(1003,'DC0 - S3',200,0.0,200.0,0.0)
      call hbook1(1004,'DC0 - S4',200,0.0,200.0,0.0)
      call hbook1(1005,'DC0 - S5',200,0.0,200.0,0.0)
      call hbook1(1006,'DC0 - S6',200,0.0,200.0,0.0)
      call hbook1(1011,'DC0 - S1el',200,0.0,200.0,0.0)
      call hbook1(1012,'DC0 - S2el',200,0.0,200.0,0.0)
      call hbook1(1013,'DC0 - S3el',200,0.0,200.0,0.0)
      call hbook1(1014,'DC0 - S4el',200,0.0,200.0,0.0)
      call hbook1(1015,'DC0 - S5el',200,0.0,200.0,0.0)
      call hbook1(1016,'DC0 - S6el',200,0.0,200.0,0.0)


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
     
      character*120 hist_title
      character*1 Csec, Creg	
      integer Sec, Reg, Sup, HistID, ntimebin

      character*2 SL(2)
      real rawtdcLL(6), rawtdcHL(6),fitdocaLL(6),fitdocaHL(6)
c  the limits for raw TDC's may have to change 

      data rawtdcLL /-100., -100., -100., -100., -100., -100./  ! tdc low limit for R1/2/3
      data rawtdcHL /400., 400.,900., 900., 1400., 1400./  ! tdc high limit for R1/2/3
      data fitdocaLL /-1., -1., -1.5, -1.5, -2.5, -2.5 / ! fit DOCA low limit R1/2/3
      data fitdocaHL /1., 1.,1.5,1.5, 2.5, 2.5 / ! fit DOCA high limit R1/2/3

              
       HistID = 7001
       hist_title = 'TBT momentum (GeV/cc)  '
       call hbook1(HistID, hist_title, 40, 0., 5., 0.)
      
       HistID = 7002
       hist_title = 'TBT phi  '
       call hbook1(HistID, hist_title, 60, -180., 180., 0.)

       HistID = 7003
       hist_title = 'TBT Z at vertex (cm)  '
       call hbook1(HistID, hist_title, 100, -70., 30., 0.) 
	
	HistID = 7004
	hist_title='TBT Chi2 for all sectors average'
	call hbook1(HistID,hist_title,100,0.,10.,0.)	
      do Reg = 1,6
         write(Creg,'(i1)') Reg
         HistID = 42 + Reg*1000 
         hist_title = 'TBT residual  SL'//Creg
         call hbook1(HistID, hist_title, 40, -.25, .25, 0.)
      enddo          
      do Sec = 1,6

       write(Csec,'(i1)') Sec

c  book the TBT histograms for TBTR bank info

       HistID = 81 + Sec*100 + 7000
       hist_title = 'TBT Chi2 pdf  Sec'//Csec
       call hbook1(HistID, hist_title, 100, 0., 10., 0.)
      
c  book 2D for layer vs wire

       HistID = 32 + Sec*100 + 7000 
       hist_title = 'TBT layer vs wire  Sec'//Csec
       call hbook2(HistID, hist_title, 192, 0.5, 192.5,
     &      36, 0.5, 36.5, 0.)


c  book histograms for 3 region in the sector

       do Reg = 1,6

	write(Creg,'(i1)') Reg

         ntimebin = (rawtdcHL(Reg) - rawtdcLL(Reg))/4
         HistID = 41 + Sec*100 + Reg*1000 
         hist_title = 'TBT TDC  SL'//Creg//'S'//Csec
         call hbook1(HistID, hist_title, ntimebin, rawtdcLL(Reg),
     &        rawtdcHL(Reg), 0.)  

         HistID = 42 + Sec*100 + Reg*1000 
         hist_title = 'TBT residual  SL'//Creg//'S'//Csec
         call hbook1(HistID, hist_title, 40, -.25, .25, 0.)
         
c  book histograms for residuals Vs drifttime 

       HistID = 43 + Sec*100 + Reg*1000
        hist_title = 'Residuals Vs time SL'//Creg//'S'//Csec
        call hbook2(HistID,hist_title,200,-10.,rawtdcHL(Reg),40,-.25,
     &       .25, 0.)
       
c  book histograms for fitDOCA Vs resi

       HistID = 44 + Sec*100 + Reg*1000
        hist_title = 'residuals Vs fitDOCA SL'//Creg//'S'//Csec
        call  hbook2(HistID,hist_title,200,fitdocaLL(Reg),fitdocaHL(Reg),
     &       40,-.25, .25,0.)    	
         
       enddo
       enddo
      return
      end



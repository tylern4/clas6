	PROGRAM Hscan
C
c	Hscan reads an hbook file and dumps the contents to an ASCII
c	file. 
c       The contents of a 1-d histogram is output as x and y and yerr
c       The contents of a 2-d histogram is output as x, y and weight.
c
c	This version is writen with HBOOK version 4.  Elton Smith 10/17/97
C
	parameter (limhst=50000000)
	COMMON /PAWC/ HMEMMOR(limhst)
	character*80 title
	character*80 infile,topfile
	character*1 answer, list
	real wmin
c        integer xdim,xdim2,xdim3
	parameter (wmin=1.)                
	parameter (maxdim=343)
	dimension idvect(maxdim)
        parameter (xdim=135)
        dimension xd1(xdim)
        parameter (xdim2=135)
        dimension xd2(xdim2)
        parameter (xdim3=135)
        dimension xd3(xdim)
        real xd_min, xd_max, d_min, d_max
        data d_min, d_max /100., 2000./

c
	call hlimit (limhst)

c
c	write (6,'(a,$)') ' Enter HBOOK file name: '
c	read (5,'(a)',end=900) infile
c	write (6,'(a,$)') ' Enter output data file name: '
c	read (5,'(a)',end=900) topfile
	infile = 'hscan_input'
	topfile = 'hscan_data'
	write (6,'(/a,a)') ' Hbook symbolic file is  ',infile
	write (6,'(a,a)') ' Data symbolic file is  ',topfile
c
c	open files of interest
c
	lrec = 1024
	call hropen (61,'infile',infile,'X',lrec,istat)
	open (30,file=topfile,status='unknown')
c
c	inquire about Print file
c
c	write (6,'(a,$)') ' Make Print file too (y/n)? '
	list = 'n'
c	read (5,'(a)') list
c
c	Fetch next histogram from file
c
	call hrin (0,9999,0)
c
        write (6,'(a,$)') ' Wish to Scan? '
        read (5,'(a)') answer
        if (answer.eq.'y' .or. answer.eq.'Y') then
c
c       get 2-d histogram list
c
	   call hid2 (idvect,nhist)
c       write (6,*) ' idvect=',idvect(1),' nhist=',nhist
        else
	   nhist = 1
	   write (6,'(a,$)') ' Enter Histogram Number '
	   read (5,*) id
	   idvect(1) = id
        endif
c
	if (nhist .gt. maxdim) then
	   print *,' *** HTOP - Too many histograms - redimension idvect'
	   Stop ' *** HTOP - Too many histograms on file'
	endif
c
	answer = 'n'
	do jj=1,nhist		! Do for each histogram
		id = idvect(jj)
c
c       initialize some arrays used for the max and min of x data: (R)
        xd_min=0.
        xd_max=0.
	do i=1,xdim2
	   xd1(i)=0.
	   xd2(i)=0.
	   xd3(i)=0.
	enddo
c       end of R
		call hgive (id,title,nx,xmi,xma,ny,ymi,yma,nwt,iad)
                write (6,*) ' id=',id,' nx=',nx,' xmi=',xmi,' xma=',xma
	write (6,*) ' ny=',ny,' ymi=',ymi,' yma=',yma
	if (answer .ne. 'g') then
	   write (6,30) ' Send hist=',id,'/ title=',title(1:4*nwt),'
	1	(y/n/q/g)? '
 30	   format (a,i4,3a,$)
	   read (5,'(a)') answer
c       
c               quit if answer = 'q'
c
	   if (answer.eq.'q' .or. answer.eq.'Q') go to 800
c
	endif
	if (answer.ne.'n' .and. answer.ne.'N') then
c       
c       compute bin sizes
c       
	   call hijxy (id,1,1,x1,y1)
	   call hijxy (id,2,2,x2,y2)
	   hx = (x2-x1)/2.
	   hy = (y2-y1)/2.
c       
c       determine number of non-zero entries and average y
c       
	   npts = 0
	   sumy = 0.
	   sumw = 0.
	   
	   do j=1,nx
	      do k=1,ny
		 call hijxy (id,j,k,xlow,ylow)
		 x = xlow + hx
		 y = ylow + hy
		 xd1(j) = xlow + hx         !R
		 w = hij (id,j,k)
		 if (w .gt. wmin) then                             
		    xd2(j)=xd1(j)           !R
		    npts = npts + 1
		    sumy = sumy + y*w
		    sumw = sumw + w
		 endif
	      enddo
	      
	   enddo
	   
c          find the max and min of x data for each histogram: (R)	   
	   l=1
	   xd_max = -100.
	   do j=1,nx
	      if (xd2(j).ge.30.) then
		 xd3(l)=xd2(j)
		 if(xd3(l).gt.xd_max) xd_max=xd3(l)
		 l=l+1
	      endif
	   enddo
	   xd_min = xd3(1)
           print *, 'data range:',xd_min, xd_max
c
           if (xd_min.gt.d_min.or.xd_max.lt.d_max) npts=0
   
c          end of R             

c
c       compute average
c
	      if (sumw .gt. 0) then
	      avey = sumy / sumw
	   else
	      print *,' *** hscan - sumw=',sumw
	      avey = 0.
	   endif
c       if (npts .le. 0) go to 802
c
c       write minuit header information
c
	   write (30,'(a,i4,i4,f6,f6)') '''Histogram Number''',id,npts,xd_min,xd_max ! R (min_main input)
	   write (30,'(a)') 'set title'
	   write (30,'(a)') 'Fitting Time-walk function from laser data'
	   write (30,'(a)') 'parameters'
	   write (30,'(a)') '1   ''p1''      40.  0.1'
c       write (30,'(a)') '2   ''p2''      10.  0.1  0. 200.'
	   write (30,'(a)') '2   ''p2''       1.  0.1  0. 500.'
	   write (30,'(a)') '3   ''p3''       0.02  0.01 0.001 10.'
c       write (30,'(a)') '3   ''p3''       0.02  0.01'
c       write (30,'(a)') '4   ''p4''      50.  1.0 20. 2500.'
	   write (30,'(a)') '4   ''p4''      10.  1.0 20. 2500.'
	   write (30,'(a)') ' '
c
c       write list file headers
c       
	   write (30,'(a)') 'TITLE TOP '''//title(1:4*nwt)//''''
	   write (30,'(i4,a)') npts, ' Number of Entries'
	   write (30,'(g14.9,g14.9,a)') xmi, xma, ' x LIMITS'
	   write (30,'(g14.9,g14.9,a)') ymi, yma, ' y LIMITS'
	   write (30,'(g14.9,a)') avey,' Average y value' 
c       
           write (30,*) xd_min, xd_max	! R (min_walk input)   
           if (xd_min.gt.100.or.xd_max.lt.2000)	then
c             npts=0
             goto 700
           endif              ! end R

	   do j=1,nx
	      do k=1,ny
		 call hijxy (id,j,k,xlow,ylow)
		 x = xlow + hx
		 y = ylow + hy
		 w = hij (id,j,k)
		 if (w .gt. wmin) then
		    write (30,*) x,y,w
		 endif                            
	      enddo
	   enddo
	   
c                  complete minuit input file with fitting commands
c
700	   write (30,'(a)') 'restore'
	   write (30,'(a,g14.9)') 'set param 1 ',3.*avey/4.
c
c           3/5/2001 change initial values
c
c		   write (30,'(a,g)') 'set param 2 ',avey/2
	   write (30,'(a,g14.9)') 'set param 2 ', 4.05*(avey - 3.*avey/4.)
c                  write (30,'(a,g)') 'set param 2 10.'
c		   write (30,'(a)') 'set param 3 0.5'
c		   write (30,'(a)') 'set param 3 0.15'
	   if (avey .gt. 0.) then
c		      write (30,'(a,g14.9)') 'set param 3 ', 1/(4.05*(avey - 3.*avey/4.))
	      write (30,'(a,g14.9)') 'set param 3 0.001' ! R
c		      write (30,'(a,g14.9)') 'set param 3 ', 1/(4.05*(avey - 3.*avey/4.))
	   else
c		      write (30,'(a)') 'set param 3 0.15'    
	      write (30,'(a)') 'set param 3 0.001' ! R
	   endif
	   write (30,'(a)') 'set param 4 50.'
c		   write (30,'(a)') 'fix 4'
	   write (30,'(a)') 'fix 2'
	   write (30,'(a)') 'migrad'
	   write (30,'(a)') 'return'
c
	endif
c
c		go fetch next histogram on file
c
802	continue
	enddo			! End Do for each histogram.
c
c	print files to list file
c
800	continue
	if (list.eq.'y' .or. list.eq.'Y') then
c
c		output histograms to list file
c
		call hidopt (0,'BLAC')
		call hidopt (0,'NPCH')
		call hidopt (0,'INTE')
		call hminim (0,0.)
		CALL HISTDO
	endif
c
	call hrend ('INFILE')
C
	END

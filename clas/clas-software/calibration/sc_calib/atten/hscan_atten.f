	PROGRAM Hscan_atten
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
	parameter (wmin=1.)
	parameter (maxdim=289)
	dimension idvect(maxdim)
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
	call hropen (101,'infile',infile,'X ',lrec,istat)
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
c        write (6,'(a,$)') ' Wish to Scan? '
c        read (5,'(a)') answer

        answer = 'Y'

        if (answer.eq.'y' .or. answer.eq.'Y') then
c
c               get 2-d histogram list
c
          	call hid2 (idvect,nhist)
c		write (6,*) ' idvect=',idvect(1),' nhist=',nhist
        else
                nhist = 1
                write (6,'(a,$)') ' Enter Histogram Number '
                read (5,*) id
                idvect(1) = id
        endif
c
	if (nhist .gt. maxdim) then
	write (6,*) ' *** HTOP - Too many histograms - redimension idvect'
		Stop ' *** HTOP - Too many histograms on file'
	endif
c
	answer = 'n'
	do jj=1,nhist
		id = idvect(jj)
c
		call hgive (id,title,nx,xmi,xma,ny,ymi,yma,nwt,iad)
c        write (6,*) ' id=',id,' nx=',nx,' xmi=',xmi,' xma=',xma
c 	 write (6,*) ' ny=',ny,' ymi=',ymi,' yma=',yma
	if (answer .ne. 'g') then
c      	write (6,30) ' Send hist=',id,'/ title=',title(1:4*nwt),' (y/n/q/g)? '
c30		format (a,i4,3a,$)
c		read (5,'(a)') answer

                answer = 'g'
c
c               quit if answer = 'q'
c
		if (answer.eq.'q' .or. answer.eq.'Q') go to 800
c
	endif
		if (answer.ne.'n' .and. answer.ne.'N') then
c
c                       compute bin sizes
c
			call hijxy (id,1,1,x1,y1)
			call hijxy (id,2,2,x2,y2)
			hx = (x2-x1)/2.
			hy = (y2-y1)/2.
c
c                       determine number of non-zero entries and average y
c
			npts = 0
			sumy = 0.
			sumw = 0.
			do j=1,nx
			   do k=1,ny
				call hijxy (id,j,k,xlow,ylow)
				x = xlow + hx
				y = ylow + hy
				w = hij (id,j,k)
				if (w .gt. wmin) then
				   npts = npts + 1
				   sumy = sumy + y*w
				   sumw = sumw + w
				endif
			   enddo
			enddo
c
c                       compute average
c
			if (sumw .gt. 0) then
			   avey = sumy / sumw
			else
			   write(6,*) ' *** hscan - id=',id,' sumw=',sumw
			   avey = 0.
			endif
c
c                       output all histograms, even with zero entries
c
c			if (npts .le. 0) go to 802
c
c                       write minuit header information
c
		   write (30,'(a,i6,i6)') '''Histogram Number''',id,npts
		   write (30,'(a)') 'set title'   	           
		   write (30,'(a)') 'Fit Attenuation Length of ln ADCL/ADCR'
		   write (30,'(a)') 'parameters'
		   write (30,'(a)') '1   ''p1''      40.  0.1'
		   write (30,'(a)') '2   ''p2''     300.  0.1  0. 600.'
		   write (30,'(a)') ' '
c
c			write list file headers
c
			write (30,'(a)') 'TITLE TOP '''//title(1:4*nwt)//''''
			write (30,'(i5,a)') npts, ' Number of Entries'
			write (30,'(g34.20,g34.20,a)') xmi, xma, ' x LIMITS'
			write (30,'(g34.20,g34.20,a)') ymi, yma, ' y LIMITS'
			write (30,'(g34.20,a)') avey,' Average y value' 
c
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
c
c                  complete minuit input file with fitting commands
c
		   write (30,'(a)') 'migrad'
		   write (30,'(a)') 'return'
c
		endif
c
c		go fetch next histogram on file
c
802	continue
	enddo
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

	PROGRAM Hscan_veff
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
	character argv*128
	character*80 title
	character*80 infile,topfile
	character*1 answer, list
	real wmin
	integer iargc
	parameter (wmin=20.)
	parameter (maxdim=500)
	dimension idvect(maxdim)
	parameter(infile='hscan_input',topfile='min_input')
c
	call hlimit (limhst)
c
	write (6,'(/a,a)') ' Hbook file is  ',infile
	write (6,'(a,a)') ' Data file is  ',topfile
c
c	open files of interest
c
	lrec = 1024
	call hropen (99,'infile',infile,' ',lrec,istat)
	open (98,file=topfile,status='unknown')
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
c       write (6,'(a,$)') ' Wish to Scan? '
c       read (5,'(a)') answer
c       if (answer.eq.'y' .or. answer.eq.'Y') then
c       
c       get 2-d histogram list
c
	call hid2 (idvect,nhist)
c		write (6,*) ' idvect=',idvect(1),' nhist=',nhist
c        else
c                nhist = 1
c                write (6,'(a,$)') ' Enter Histogram Number '
c                read (5,*) id
c                idvect(1) = id
c       endif
c
	if (nhist .gt. maxdim) then
		print *,' *** HTOP - Too many histograms - redimension idvect'
		Stop ' *** HTOP - Too many histograms on file'
	endif
c
	answer = 'g'
	do jj=1,nhist
		id = idvect(jj)
c
		call hgive (id,title,nx,xmi,xma,ny,ymi,yma,nwt,iad)
        write (6,*) ' id=',id,' nx=',nx,' xmi=',xmi,' xma=',xma
	write (6,*) ' ny=',ny,' ymi=',ymi,' yma=',yma
	if (answer .ne. 'g') then
      	write (6,102) ' Send hist=',id,'/ title=',title(1:4*nwt),' (y/n/q/g)? '
102		format (a,i4,3a,$)
		read (5,'(a)') answer
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
			   print *,' *** hscan - sumw=',sumw
			   avey = 0.
			endif
c			if (npts .le. 0) go to 802
c
c                       write minuit header information
c
			write (98,*) '''Histogram Number''',id,npts
			write (98,*) 'set title'
			write (98,*) 'Fit effective velocity'
			write (98,*) 'parameters'
			write (98,*) '1   ''p1''     0.   0.1'
			write (98,*) '2   ''p2''     16.  0.1'
			write (98,*) ' '
c
c			write list file headers
c
			write (98,*) 'TITLE TOP '''//title(1:4*nwt)//''''
			write (98,*) npts, ' Number of Entries'
			write (98,*) xmi, xma, ' x LIMITS'
			write (98,*) ymi, yma, ' y LIMITS'
			write (98,*) avey,' Average y value' 
c
			do j=1,nx
			   do k=1,ny
				call hijxy (id,j,k,xlow,ylow)
				x = xlow + hx
				y = ylow + hy
				w = hij (id,j,k)
				if (w .gt. wmin) then
				   write (98,*) x,y,w
				endif
			   enddo
			enddo
c
c                  complete minuit input file with fitting commands
c
		   write (98,*) 'migrad'
		   write (98,*) 'return'
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



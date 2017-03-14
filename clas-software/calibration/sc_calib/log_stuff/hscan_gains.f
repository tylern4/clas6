	PROGRAM hscan_gains

c       This is modified from hscan_means to process cosmic ray data
c       Joe Santoro 3-26-01

C
c	Hscan reads an hbook file and dumps the contents to an ASCII
c	file. 
c       The contents of a 1-d histogram is output as x and y and yerr
c
c	This version is writen with HBOOK version 4.  Elton Smith 10/17/97
c                                                     Modification for 1-d 7/2/98
C
	parameter (limhst=50000000)
	COMMON /PAWC/ HMEMMOR(limhst)
	character*80 title
	character*80 infile,topfile
	character*1 answer, list, answer2
	real xmax,slope,halfwidth, inc1, inc2
	real wmin,werr,xmin
c
c       output data if more than one entry
c
	parameter (wmin=-1.)
	parameter (maxdim=500)
	dimension idvect(maxdim)
c
	call hlimit (limhst)
c
	infile = 'hscan_input'
	topfile = 'hscan_data'
	write (6,'(/a,a)') ' Hbook symbolic file is  ',infile
	write (6,'(a,a)') ' Data symbolic file is  ',topfile
       
       slope = 1.0
       halfwidth = 1.0
       inc1 = .1
       inc2 = .1
       xmin = -2.0
c
c	open files of interest
c
	lrec = 0
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
c               get 1-d histogram list
c
          	call hid1 (idvect,nhist)
c		write (6,*) ' idvect=',idvect(1),' nhist=',nhist
        else
                nhist = 1
                write (6,'(a,$)') ' Enter Histogram Number '
                read (5,*) id
                idvect(1) = id
        endif
c
	if (nhist .gt. maxdim) then
	write (6,*) ' *** hscan_gains - Too many histograms - redimension idvect'
		Stop ' *** hscan_gains - Too many histograms on file'
	endif
c
	answer = 'n'
	do jj=1,nhist
		id = idvect(jj)
c
		call hgive (id,title,nx,xmi,xma,ny,ymi,yma,nwt,iad)
		 
c        write (6,*) ' id=',id,' nx=',nx,' xmi=',xmi,' xma=',xma
c write (6,*) ' ny=',ny,' ymi=',ymi,' yma=',yma
	if (answer .ne. 'g') then
      	write (6,30) ' Send hist=',id,'/ title=',title(1:4*nwt),' (y/n/q/g)? '
30		format (a,i4,3a,$)
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
			call hix (id,1,x1)
			call hix (id,2,x2)
			hx = (x2-x1)/2.
c
c                       determine number of non-zero entries and average y
c

			npts = 0
			sumx = 0.
			sumw = 0.
			do j=1,nx
			   call hix (id,j,xlow)
			   x = xlow + hx
			   w = hi (id,j)
			   if (w .gt. wmin .and. x.gt.xmin) then
			      npts = npts + 1
			      sumx = sumx + x*w
			      sumw = sumw + w
			   endif
			enddo
c
c                       compute average
c
			if (sumw .gt. 0) then
			   avex = sumx / sumw
			else
			   write (6,*) ' *** hscan - ihst=',id,' sumw=',sumw
			   avex = 0.
			endif
			xmax = hmax(id)
c
c                       output all histogramas, including empty ones
c
c			if (npts .le. 0) go to 802
c
c                       write minuit header information
c
		   write (30,'(a,i4,i4)') '''Histogram Number''',id,npts
		   write (30,'(a)') 'set title'
		   write (30,'(a)') 'Fit LOG(ADCL/ADCR) in Scintillator'
		   write (30,'(a)') 'parameters'
		   write (30,'(a,f14.3,a)') '1   ''p1''      ',avex,' .1'
		   write (30,'(a,f14.3,f12.5)')     '2   ''p2''  ' ,slope, inc1
		   write (30,'(a,f14.3,a)') '3   ''p3''      ',xmax,' 1.'
		   write (30,'(a,f14.3,f14.3)')     '4   ''p4''  ' ,halfwidth, inc2
                   write (30,'(a)') ' '
c
c			write list file headers
c
                        
			write (30,'(a)') 'TITLE '''//title(1:4*nwt)//''''
			write (30,'(i14,g34.20,a)') npts,sumw, ' Number of Entries'
			write (30,'(g34.20,g34.20,a)') xmi, xma, ' x LIMITS'
			write (30,'(g34.20,a)') avex,' Average x value' 
c
			do j=1,nx
			   call hix (id,j,xlow)
			   x = xlow + hx
			   w = hi (id,j)
			   werr = max (sqrt(w),1.)
			   if (w .gt. wmin .and. x.gt.xmin) then
			      write (30,*) x,w,werr
			   endif
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










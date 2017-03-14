	Subroutine min_gains (npar,Grad,chi2,par,iflag,Futil)
c
c	Subroutine to chi2 for energy loss in TOF scintillators.
c	The gmean values are read from unit linput 
c           until EOF is encountered.
c
c	The input file is assumed to have the form:
c
c	TITLE : 'String with information about fit' (used for documentation) 
c	npts	        /number of data points/
c	xmin, xmax	/lower, upper limit of ADC/
c              energy loss  Number of Events	Yerror 	
c		.	        .	          .	
c		.	        .	          .	
c		.	        .	          .	
c
c	This subroutine (referred to as 'FCN' by Minuit documentation) does
c		not return gradients (at present). 
c
	implicit none
        save
	integer npar, iflag, maxnpts, loutput, ldata, j
	integer npts, ndeg
	real xmin, xmax, xave, x, sumw, sumw_min
	common /xlimits/ xmin,xmax,xave
c
c       use same input file as minuit control
c
	Parameter (loutput=8, ldata=9)
	Parameter (maxnpts=100000)
	character*60 title
c
	double precision Grad, chi2, par, Futil
c
c	par		-	array with values of parameters
c	Grad		-	array of derivaties of chi2 with respect to par
c				(not computed here)
c
	Dimension par(*), Grad(*)
c
c	parameters of fit are 	
c		p1	- 
c		p2	- 
c		p3	- 
c		p4	- 

c
	Real delE(maxnpts), counts(maxnpts), yerror(maxnpts)
	Double Precision gains_func, p1, p2, p3, p4, yerr, ratio1, gains
	
c
c	get parameters from argument list
c
	p1 = par(1)
	p2 = par(2)
	p3 = par(3)
	p4 = par(4)
	ndeg = npar
c
c       initialize function
c
c	min_gains = 0.
c
	if (iflag .eq. 1) then
c
c		read data, store for future use
c
		read (ldata,'(a)') title
c		write (6,*) ' title=',title
		read (ldata,*) npts,sumw
c		write (6,*) ' npts=',npts,' sumw=',sumw
c		if (npts .gt. maxnpts) then
c			write (6,*) ' **** min_chi2 - Too many points=',
c	1		npts,' maxpoints=',maxnpts
c			stop ' **** Require dimension change'
c		endif
		read (ldata,*) xmin, xmax
c		write (6,*) ' xmin=',xmin, ' xmax=',xmax
		read (ldata,*) xave
c		write (6,*) ' xave=',xave
c
		do j=1,npts
		   if (j .gt. maxnpts) then
			write (6,*) ' *** min_chi2 - Too many points=',npts
			write (6,*) ' maxpoints=',maxnpts
			stop ' *** Require dimension change'
		   endif
		   read (ldata,*,end=900) delE(j), counts(j), yerror(j)
		enddo
	endif
c
c	gradients not presently computed
c
	if (iflag .eq. 2) then
		write (6,*) ' *** min_chi2 - Gradients not computed, iflag=',iflag
		return
	endif
c
c	compute chi2
c
	sumw_min = 20.
c
	if (sumw .le. sumw_min) then
c
c          if event count is less than sumw_min, increase chi2
c
	   chi2 = 1000.
	else
	   chi2 = 0.
        endif
c
	do j=1,npts
c
	   yerr = yerror(j)
c
c          loop over data points and compute chi2
c
	   x = delE (j)
	   gains = gains_func (x,p1,p2,p3,p4)
	   ratio1 = counts (j)
c
c          fit data only between xmin and xmax
c
	   if (x.ge.xmin .and. x.le.xmax) then
              chi2 = chi2 + (ratio1-gains)**2/yerr**2
	   endif
	enddo
c
	if (iflag .eq. 3) then
c
c
c	     Fitting procedure is complete. Output data
c
c		write (loutput,*) '      ndx       x        y        yerr      Fit'
		do j=1,npts
		   x = delE (j)
                   yerr = yerror(j)
		   gains = gains_func (x,p1,p2,p3,p4)
c		   write (loutput,200) j,x,counts(j),yerr,gains
200			format (i10,4f10.4)
		enddo
c			
		write (loutput,*) ' '	
		write (loutput,*) ' Chi square / D.F =',chi2,' / ', 
     1				   npts-ndeg+1
		write (loutput,*) ' '
		write (loutput,*) ' '
	endif

	return
900	continue
	write (6,*) ' *** min_chi2 - EOF encountered after=',npts
	stop ' *** Unexpedted EOF'
c
	end
        Double Precision function gains_func (x,p1,p2,p3,p4)
c
c      LOG(ADCL/ADCR) data
c      Returns fit to Fermi Function
c
c      fitfun = Fermi (centroid,slope,normalization)
c
c       p1 = centroid
c       p2 = slope
c       p3 = normalization
c       p4 = half width
 
c
       implicit none
       real x
c
       Double precision p1,p2,p3,p4,x_prime
	save
c
c       keep x_prime as single precision, but all calculations in double
c 
      x_prime = x - p1
c
      gains_func = p3/(1+exp((abs(x_prime)-p4)/p2))
c
      end








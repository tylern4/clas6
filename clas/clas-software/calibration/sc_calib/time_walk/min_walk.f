	Subroutine min_walk (npar,Grad,chi2,par,iflag,Futil)
c
c	Subroutine to chi2 for walk correction function.
c	The time vs. ADC data is read from unit linput 
c           until EOF is encountered.
c
c	The input file is assumed to have the form:
c
c	TITLE : 'String with information about fit' (used for documentation) 
c	npts	        /number of data points/
c	xmin, xmax	/lower, upper limit of ADC/
c	ymin, ymax      /lower, upper limits for time/
c		ADC	Time	Weight 	
c		.	.	.	
c		.	.	.	
c		.	.	.	
c
c	This subroutine (referred to as 'FCN' by Minuit documentation) does
c		not return gradients (at present). 
c
	implicit none
	save
	integer npar, iflag, maxnpts, loutput, ldata, j
	integer npts, ndeg
	real xmin, xmax, ymin, ymax, yave, x, ADC, time, weight
        real xd_min, xd_max
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
c		p1	- constant
c		p2	- normalization
c		p3 	- exponent 2 
c       Current parametrization is time = p1 + p2/ADC**p3
c
	Dimension ADC(maxnpts), time(maxnpts), weight(maxnpts)
	Double Precision time_walk, p1, p2, p3, p4, walk, yerr, t1
c
	save
c       inline computation of function
c
c	walk_func(x) = p1 + p2/x**p3
c
c	get parameters from argument list
c
	p1 = par(1)
	p2 = par(2)
	p3 = par(3)
	p4 = par(4)
	ndeg = npar
c
c       initialize funciton
c
c	min_walk = 0.
c
	if (iflag .eq. 1) then
c
c		read data, store for future use
c
		read (ldata,'(a)') title
c		type *,' title=',title
		read (ldata,*) npts
c		type *,' npts=',npts
c		if (npts .gt. maxnpts) then
c			type *,' **** min_chi2 - Too many points=',
c	1		npts,' maxpoints=',maxnpts
c			stop ' **** Require dimension change'
c		endif
		read (ldata,*) xmin, xmax
c		type *,' xmin=',xmin, ' xmax=',xmax
		read (ldata,*) ymin, ymax
c		type *,' ymin=',ymin, ' ymax=',ymax
		read (ldata,*) yave
c		type *,' yave=',yave
c
                read (ldata,*) xd_min,xd_max             ! selecting proper range for fit
               if (xd_min.gt.100.or.xd_max.lt.2000) then
                  print *,'dont fit'
c                  goto 910
               endif                                     ! end selectin proper range for fit
		do j=1,npts
		   if (j .gt. maxnpts) then
			print *,' *** min_chi2 - Too many points=',npts
			print *,' maxpoints=',maxnpts
			stop ' *** Require dimension change'
		   endif
		   read (ldata,*,end=900) ADC(j), time(j), weight(j)
		enddo
	endif
c
c	gradients not presently computed
c
	if (iflag .eq. 2) then
		print *,' *** min_chi2 - Gradients not computed, iflag=',iflag
		return
	endif
c
c	compute chi2
c
	chi2 = 0.
	do j=1,npts
c
c          compute "error" in time from weight factor
c
	   yerr = 1./sqrt(weight(j))
c
c          loop over data points and compute chi2
c
	   x = ADC(j)
	   walk = time_walk(x,p1,p2,p3,p4)
	   t1 = time(j)
c
c          fit data only between xmin and xmax
c
	   if (x.ge.xmin .and. x.le.xmax) then
              chi2 = chi2 + (t1-walk)**2/yerr**2
	   endif
	enddo
c
	if (iflag .eq. 3) then
c
c
c	     Fitting procedure is complete. Output data
c
c		write (loutput,*) '      ndx       x        y        Fit'
		do j=1,npts
		   x = ADC(j)
		   walk = time_walk(x,p1,p2,p3,p4)
c		   write (loutput,200) j,x,time(j),walk
200			format (i10,3f10.4)
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
	print *,' *** min_chi2 - EOF encountered after=',npts
	stop ' *** Unexpected EOF'
c
910	end
        Double Precision function time_walk (x,p1,p2,p3,p4)
c
c      Time-walk correction function
c
       Double precision p1,p2,p3,p4,x1,xnorm,thresh,walk_max,xmax,
     1     adc_max,a2,a3,x0,a
c
c       keep x as single precision, but all calculations in double
c
       real x
c
c      20 mv Threshold converted to channels (20*1.77)
c
       data thresh, adc_max /35., 600./
      data a2, a3, x0 /13.87, 0.074, 19.56/   ! constants for left PMTs
c       data a2, a3, x0 /13.31, 0.066, 4.43./
c
       x1 = x
       xnorm = x1/thresh
c
       if (xnorm .lt. p4) then
       p2 = (a2*a3/p3)*(x0**(p3-a3))
          time_walk = p2/xnorm**p3

       else
       p2 = (a2*a3/p3)*(x0**(p3-a3))
          time_walk = p2*(1+p3)/p4**p3 - p2*p3*xnorm/p4**(p3+1)

       endif
c
c       compute function relative to maximum ADC value adc_max
c
       xmax = adc_max/thresh

       if (xmax .lt. p4) then
       p2 = (a2*a3/p3)*(x0**(p3-a3))
          walk_max = p2/xmax**p3   


       else
       p2 = (a2*a3/p3)*(x0**(p3-a3))
          walk_max = p2*(1+p3)/p4**p3 - p2*p3*xmax/p4**(p3+1)

       endif

       time_walk = p1 + time_walk - walk_max
       return
       end

	

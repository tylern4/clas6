	Real Function min2_veff (npar,Grad,chi2,par,iflag,Futil)
c
c	Subroutine to chi2 for effective velocity function.
c	The time vs position(cm) is read from unit linput 
c           until EOF is encountered.
c
c	The input file is assumed to have the form:
c
c	TITLE : 'String with information about fit' (used for documentation) 
c	npts	        /number of data points/
c	xmin, xmax	/lower, upper limit of ADC/
c	ymin, ymax      /lower, upper limits for time/
c              Time   Position	Weight 	
c		.	        .	  .	
c		.	        .	  .	
c		.	        .	  .	
c
c	This subroutine (referred to as 'FCN' by Minuit documentation) does
c		not return gradients (at present). 
c
	implicit none
	integer npar, iflag, maxnpts, loutput, ldata, j
	integer npts, ndeg
	real xmin, xmax, ymin, ymax, yave, x, pos, time, weight
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
c		p2	- veff 
c
c       Current parametrization is time = p1 + 2* pos/p2
c
	Dimension time(maxnpts), pos(maxnpts), weight(maxnpts)
	common/veff_data/time,pos,weight,npts,xmin,xmax,ymin,ymax,yave

	Double Precision veff_func, p1, p2, yerr, time1, veff
c
c	get parameters from argument list
c
	p1 = par(1)
	p2 = par(2)
c	write(6,*)'p1,p2',p1,p2
	ndeg = npar
c
c       initialize funciton
c
	min2_veff = 0.
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
c          compute "error" in pos from weight factor
c
	   yerr = 1./sqrt(weight(j))
c
c          loop over data points and compute chi2
c
	   x = pos (j)
	   veff = veff_func (x,p1,p2)
	   time1 = time (j)
c
c          fit data only between xmin and xmax
c
	   if (x.ge.xmin .and. x.le.xmax) then
              chi2 = chi2 + (time1-veff)**2/yerr**2
c	      write(6,*)'chi2',chi2
	   endif
	enddo
c
	if (iflag .eq. 3) then
	   write(6,*) 'iflag=3'
c
c
c	     Fitting procedure is complete. Output data
c
c		write (loutput,*) '      ndx       x        y        Fit'
		do j=1,npts
		   x = pos (j)
		   veff = veff_func (x,p1,p2)
c		   write (loutput,200) j,x,time(j),veff
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

	end









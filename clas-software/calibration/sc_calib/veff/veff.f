	subroutine min_veff (npar,Grad,chi2,par,iflag,Futil)
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
	implicit double precision (A-H,O-Z)
	integer npar, iflag, maxnpts, loutput, ldata, j
	integer npts, ndeg
	real xmin, xmax, ymin, ymax, yave, x, pos, time, weight
c
c       use same input file as minuit control
c
	Parameter (loutput=92, ldata=95)
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
c       Current parametrization is time = p1 + pos/p2
c
	Dimension time(maxnpts), pos(maxnpts), weight(maxnpts)
	common/veff_data/time,pos,weight,npts,xmin,xmax,ymin,ymax,yave

	Double Precision p1, p2, yerr, time1, veff
c
c	get parameters from argument list
c
	p1 = par(1)
	p2 = par(2)
	ndeg = npar
c
	if (iflag .eq. 1) then
c
c       read data, store for future use
c       
	   read (ldata,*, END=900) title
c	   print *,' title=',title
	   read (ldata,*, END=900) npts
c	   print *,' npts=',npts
	   read (ldata,*, END=900) xmin, xmax
c	   print *,' xmin=',xmin, ' xmax=',xmax
	   read (ldata,*, END=900) ymin, ymax
c	   print *,' ymin=',ymin, ' ymax=',ymax
	   read (ldata,*, END=900) yave
c	   print *,' yave=',yave
c
	   do j=1,npts
	      if (j .gt. maxnpts) then
		 print *,' *** min_chi2 - Too many points=',npts
		 print *,' maxpoints=',maxnpts
		 stop ' *** Require dimension change'
	      endif
	      read (ldata,*,end=900) pos(j), time(j), weight(j)
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
	   endif
	enddo

c
	if (iflag .eq. 3) then
c
c
c       Fitting procedure is complete. Output data
c
c       write (loutput,*) '      ndx       x        y        Fit'
	   do j=1,npts
	      x = pos (j)
	      veff = veff_func (x,p1,p2)
	   enddo
c       
	   write (loutput,*) ' '	
	   write (loutput,*) ' Chi square / D.F =',chi2,' / ', 
	1	npts-ndeg+1
	   write (loutput,*) ' '
	   write (loutput,*) ' '
	endif
	
	return
 900	continue
	print *,' *** min_chi2 - EOF encountered after=',npts
	stop ' *** Unexpected EOF'
c
	end


        Double Precision function veff_func (x,p1,p2)
c       
c      Veff function
c
	Double precision p1,p2,x1
c
c       keep x as single precision, but all calculations in double
c       
	real x
c
	x1 = x
	veff_func = p1 + 2.0*x1/p2
	return
	end

	

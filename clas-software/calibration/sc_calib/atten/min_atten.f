	Subroutine min_atten (npar,Grad,chi2,par,iflag,Futil)
c
c	Subroutine to chi2 for attenuation length function.
c	The ln(ADCr/ADCl) vs position(cm) is read from unit linput 
c           until EOF is encountered.
c
c	The input file is assumed to have the form:
c
c	TITLE : 'String with information about fit' (used for documentation) 
c	npts	        /number of data points/
c	xmin, xmax	/lower, upper limit of ADC/
c	ymin, ymax      /lower, upper limits for time/
c              ln(ADCr/ADCl)  Position	Weight 	
c		.	        .	  .	
c		.	        .	  .	
c		.	        .	  .	
c
c	This subroutine (referred to as 'FCN' by Minuit documentation) does
c		not return gradients (at present). 
c
	implicit none
c
c                  Save all local variables

	save
	integer npar, iflag, maxnpts, loutput, ldata, j
	integer npts, ndeg
	double precision xmin, xmax, ymin, ymax, yave, x, pos, log_ratio, weight
c
c       use same input file as minuit control
c
	Parameter (loutput=8, ldata=9)
	Parameter (maxnpts=1000000)
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
c		p2	- 2./attenuation 
c
c       Current parametrization is log_ratio = p1 + 2.*pos/p2
c
	Dimension log_ratio(maxnpts), pos(maxnpts), weight(maxnpts)
	Double Precision atten_func, p1, p2, yerr, ratio1, atten
c
c       save varibles for the next time
	save
c
c	get parameters from argument list
c
	p1 = par(1)
	p2 = par(2)
	ndeg = npar
c
c       initialize funciton
c
c	min_atten = 0.
c
	if (iflag .eq. 1) then
c
c		read data, store for future use
c
		read (ldata,'(a)') title
c		write(6,*) ' title=',title
		read (ldata,*) npts
c		write (6,*) ' npts=',npts
c		if (npts .gt. maxnpts) then
c			write (6,*) ' **** min_chi2 - Too many points=',
c	1		npts,' maxpoints=',maxnpts
c			stop ' **** Require dimension change'
c		endif
		read (ldata,*) xmin, xmax
c		write (6,*) ' xmin=',xmin, ' xmax=',xmax
		read (ldata,*) ymin, ymax
c		write (6,*) ' ymin=',ymin, ' ymax=',ymax
		read (ldata,*) yave
c		write (6,*) ' yave=',yave
c
		do j=1,npts
		   if (j .gt. maxnpts) then
			write (6,*) ' *** min_chi2 - Too many points=',npts
			write (6,*) ' maxpoints=',maxnpts
			stop ' *** Require dimension change'
		   endif
		   read (ldata,*,end=900) pos(j), log_ratio(j), weight(j)
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
	   atten = atten_func (x,p1,p2)
	   ratio1 = log_ratio (j)
c
c          fit data only between xmin and xmax
c
	   if (x.ge.xmin .and. x.le.xmax) then
              chi2 = chi2 + (ratio1-atten)**2/yerr**2
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
		   x = pos (j)
		   atten = atten_func (x,p1,p2)
c		   write (loutput,200) j,x,log_ratio(j),atten
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
	write (6,*) ' *** min_chi2 - EOF encountered after=',npts
	stop ' *** Unexpedted EOF'
c
	end
        Double Precision function atten_func (x,p1,p2)
c
c      Attenuation length function
c
       Double precision p1,p2,x1
c
c       keep x as single precision, but all calculations in double
c
       double precision x
c
       x1 = x
       atten_func = p1 - 2.*x1/p2
       return
       end

	

	Subroutine min_means (npar,Grad,chi2,par,iflag,Futil)
c
c	Subroutine to compute chi2 for energy loss in TOF scintillators.
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
c
c       save all local variables
c
	save
	integer npar, iflag, maxnpts, loutput, ldata, j
	integer npts, ndeg
	real xmin, xmax, xave, sumw, sumw_min
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
c		p5	- 
c		p6	-  
c
	Real delE(maxnpts), counts(maxnpts), yerror(maxnpts)
	Double Precision means_func, p1, p2, p3, p4, p5, p6, x, yerr, ratio1, means
c
c	get parameters from argument list
c
	p1 = par(1)
	p2 = par(2)
	p3 = par(3)
	p4 = par(4)
	p5 = par(5)
	p6 = par(6) 
	ndeg = npar
c
	if (iflag .eq. 1) then
c
c		read data, store for future use
c
		read (ldata,'(a)') title
		read (ldata,*) npts,sumw
c		if (npts .gt. maxnpts) then
c			write (6,*) ' **** min_chi2 - Too many points=',
c	1		npts,' maxpoints=',maxnpts
c			stop ' **** Require dimension change'
c		endif
		read (ldata,*) xmin, xmax
		read (ldata,*) xave
c
		do j=1,npts
		   if (j .gt. maxnpts) then
			write (6,*) ' *** min_chi2 - Too many points=',npts
			write (6,*) ' maxpoints=',maxnpts
			stop ' *** Require dimension change'
		   endif
		   read (ldata,*,end=900) delE(j), counts(j), yerror(j)
c		   write (6,*) delE(j), counts(j), yerror(j)
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
	   means = means_func (x,p1,p2,p3,p4,p5,p6)
	   ratio1 = counts (j)
c
c          fit data only between xmin and xmax
c
	   if (x.ge.xmin .and. x.le.xmax) then
              chi2 = chi2 + (ratio1-means)**2/yerr**2
	   endif
c	write (6,*) ' j =',j, x, yerr, chi2,means
	enddo
c
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
		   means = means_func (x,p1,p2,p3,p4,p5,p6)
c		   write (loutput,200) j,x,counts(j),yerr,means
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
        Double Precision function means_func (x,p1,p2,p3,p4,p5,p6)
c
c      Energy Loss function
c      Returns fit to landau + background 
c       Approx to Landau : NIM 174 (1980) 531-533.
c
c      fitfun = Landau (peak,scale,res,normalization,const,linear)
c
c       p1 = Landau peak
c       p2 = scale factor
c       p3 = resolution
c       p4 = normalization
c       p5 = constant background term
c       p6 = linear background term
c
       implicit none
       real const(9), xlam(9), gam(9)
       real lnc1,arg
       double precision A,B,C,s,xlamp
       integer j
c
       Double precision p1,p2,p3,p4,p5,p6,x,fitfun
c
c      save variables for next time
c
      save
c
      data const /0.0368,0.0843,0.0882,0.0647,0.0359,0.0164,0.0064,
     1     0.0021,0.0006/
      data xlam /-1.48,-0.738,0.170,1.33,2.95,5.39,9.40,16.8,30.8/
      data gam /0.737,0.947,1.23,1.68,2.40,3.68,6.18,12.3,39.7/
c 
      B = p2
      C = p3
      lnc1 = log(C**2+1)
      xlamp = -0.2570 + 0.3318*lnc1 + 0.02510*lnc1**2 - 0.001750*lnc1**3
      A = p1 - B*xlamp
      s = (x-A)/B
      fitfun = 0.
c 
      do j=1,9
         arg = (s-xlam(j))**2/(gam(j)**2+C**2)
c         write (6,*) ' x1, j, arg',x1,j,arg
         if (abs(arg) .lt. 100.) then
            fitfun = fitfun + 
     1         const(j)*gam(j)*exp(-arg)/sqrt(gam(j)**2+C**2)
         endif
      enddo
c
      fitfun = p4*fitfun
c
c     include background
c
      fitfun = fitfun + p5+ p6*x
c
      means_func = fitfun
c
      end

	

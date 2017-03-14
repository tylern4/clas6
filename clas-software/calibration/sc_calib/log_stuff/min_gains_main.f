	Program Min_gains_main 
c
c	Main program to drive Minuit
c	External subroutine to compute chi square is min_gains.
c	This routine must be provided.
c
	implicit none
	real min_gains
	external min_gains
	integer linput, loutput, lsave, ldata, flag, lparm, ihst,lkumac
	integer npts
	double precision parm, error, bound1, bound2
	real p1, p2, err1, err2, chi2df, chimin
	real p3,p4
	real err3,err4
	real xmin,xmax,xave
	common /xlimits/ xmin,xmax,xave
	character*10 name
	character*1 answer, select
	character*20 control
	character*20 min_input, min_output, min_parm, min_kumac
	Parameter (min_input='min_input', min_output='min_output',
     1             min_parm='min_parm', min_kumac='min_kumac')
c
	double precision chi2, fedm, errdef
	integer npari, nparx, istat, ndf
c
	Parameter (linput=9, loutput=8, lsave=7, ldata=1, lparm=14,
     1             lkumac=15)
	data chimin /250./
c
c       Write out files in use
c
	write (6,*) 'Min_main - Symbolic Input  File=',min_input
	write (6,*) 'Min_main - Symbolic Output File=',min_output
	write (6,*) 'Min_main - Symbolic Parm   File=',min_parm
	write (6,*) 'Min_main - Symbolic Kumac  File=',min_kumac
c
c       open files
c       
	open (unit=linput,file=min_input,status='old')
	open (unit=loutput,file=min_output,status='unknown',
     1		form='formatted')
	open (unit=lparm,file=min_parm,status='unknown',
     1		form='formatted')
	open (unit=lkumac,file=min_kumac,status='unknown',
     1		form='formatted')
c
	call mintio (linput,loutput,lsave)
c
c       inquire about cuts on kumac file
c
      	write (6,'(a,$)') ' Min_main - Kumac File: Check All/Selected (A/S)? '
c	read (5,'(a)') answer
	answer = 'a'
	if (answer.eq.'s' .or. answer.eq.'S') then
	   write (6,*) 'Min_main - Kumac: Check selected histograms only'
	   answer = 's'
	else
	   write (6,*) 'Min_main - Kumac: Check all histograms'
	endif
c
c       output headers
c
	write (lkumac,*) 'vector/create p(5) R 5*0.'	
c
c       loop over data / one per plot until EOF
c
100	continue
	read (linput,*,end=900) control, ihst, npts
c	write (6,*) control, ihst, npts
	call minuit (min_gains,0)
c
c       output fitted parameters
c
	call mnpout (1,name,parm,error,bound1,bound2,flag)
	p1 = parm
	err1 = error
	call mnpout (2,name,parm,error,bound1,bound2,flag)
	p2 = parm
	err2 = error
	call mnpout (3,name,parm,error,bound1,bound2,flag)
	p3 = parm
	err3 = error
	call mnpout (4,name,parm,error,bound1,bound2,flag)
	p4 = parm
	err4 = error

c
c       Fit quality
c
	call mnstat (chi2,fedm,errdef,npari,nparx,istat)
	ndf = npts - npari + 1
	if (ndf.gt.0 .and. istat.gt.0) then
c
c            note: normalization is given by chi2 / use status as normalizer 4/2/01
c
	     chi2df = chi2/ndf/istat
	else
	     chi2df = 100.
c	     write (6,*) '*** Min_main - chi2df=0., ihst=',ihst
	endif
	select = 'n'
	if (chi2df.gt.chimin .or. istat.eq.0 ) then
	   write (6,*) 'Min_main - ihst=',ihst,' istat=',istat,
     1                 ' chi2df=',chi2df
	   select = 'y'
c
c          if peak is poorly determined, set to 0
c
           p1   = 0.
           err1 = 0.
	endif       

         if (p1 .gt. 50.0) then
          p1 = 10.0
         endif
 
        if (p1 .lt. -50.0) then
          p1 = -10.0
         endif


c
c       output to parameter file
c
	write (lparm,110) ihst,p1,err1,p2,err2,p4,err4,chi2df,istat
 110	format (i6,f14.3,f14.3,f14.3,f14.3,f14.3,f14.3,f14.3,i4)
c
c       output selected files to kumac file
c
	if (select.eq.'y' .or. answer.ne.'s') then
	   write (lkumac,*) 'hi/plot',ihst
	   write (lkumac,112) 'vector/input p ',p1,p2,p3,p4,chi2df
 112	   format (a,5f13.4)
	   write (lkumac,*) 'func/plot fermi.f',xmin,xmax,' s'
	   write (lkumac,*) 'exe window#push'
	   write (lkumac,*) 'text 0.4 0.5 ''Peak=''//$eval(p(1)) 0.2'
	   write (lkumac,*) 'text 0.4 0.3 ''Chi2=''//$eval(p(5)) 0.2'
	   write (lkumac,*) 'exe window#pop'
	   write (lkumac,*) 'wait' 
	endif
c
c       get next plot
c
	go to 100
900	end



	Program Min_atten_main 
c
c	Main program to drive Minuit
c	External subroutine to compute chi square is min_atten.
c	This routine must be provided.
c
	implicit none
	real min_atten
	external min_atten
	integer linput, loutput, lsave, ldata, flag, lparm, ihst,lkumac
	integer npts
	double precision parm, error, bound1, bound2
	real p1, p2, err1, err2, chi2df, chimin
	real p1_old, p2_old, err1_old, err2_old
	character*10 name
	character*1 answer, select
	character*20 control
	character*20 input, output, parms, kumac
	Parameter (input='min_input', output='min_output', parms='min_parm', kumac='min_kumac')
c
	double precision chi2, fedm, errdef
	integer npari, nparx, istat, ndf
c
	Parameter (linput=9, loutput=8, lsave=7, ldata=1, lparm=14,  lkumac=15)
	data chimin /10./
	data p1_old,p2_old,err1_old,err2_old /0.,130.,0.02,20./
c
c       Write out files in use
c
	write (6,*) 'Min_main - Symbolic Input  File=' ,input
	write (6,*) 'Min_main - Symbolic Output File=' ,output
	write (6,*) 'Min_main - Symbolic Parm   File=' ,parms
	write (6,*) 'Min_main - Symbolic Kumac  File=' ,kumac
c
c       open files
c       
	open (unit=linput,file='min_input',status='old')
	open (unit=loutput,file=output,status='unknown',
     1		form='formatted')
	open (unit=lparm,file=parms,status='unknown',
     1		form='formatted')
	open (unit=lkumac,file=kumac,status='unknown',
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
	write (lkumac,*) 'vector/create par(3) R 3*0.'
	write (lkumac,*) 'option nbox'	
c
c       loop over data / one per plot until EOF
c
100	continue
	read (linput,*,end=900) control, ihst, npts
c	write (6,*) control, ihst, npts
	call minuit (min_atten,0)
c
c       output fitted parameters
c
	call mnpout (1,name,parm,error,bound1,bound2,flag)
	p1 = parm
	err1 = error
	call mnpout (2,name,parm,error,bound1,bound2,flag)
	p2 = parm
	err2 = error
c
c       Fit quality
c
	call mnstat (chi2,fedm,errdef,npari,nparx,istat)
	ndf = npts - npari + 1
	if (ndf.gt.0 .and. istat.gt.0) then
c
c            note: normalization is given by chi2 per ndf per status
c                  empirically it seems that this allows comparison of values
c
	     chi2df = chi2/ndf/istat
c
c            note currently max parameter atten fitted is 600 from hscan
c
	     if (p2 .ge. 599.) chi2df = chi2df + 100.
c	     write (6,*) ' ihist=',ihst,' p2=',p2
	else
	     chi2df = 100.
	     write (6,*) '*** Min_main - chi2df=0., ihst=',ihst
	endif
c
	if (chi2df.gt.chimin .or. istat.eq.0) then
	   write (6,*) 'Min_main - ihst=',ihst,' istat=',istat,
     1                 ' chi2dfstat=',chi2df
	   select = 'y'
c
c          for poorly determined attenuation lengths use previous
c
           p1 = 0.
           p2 = p2_old
           err1 = err1_old
           err2 = err2_old
	else
c
c          save valid parameters
c
           p1_old = p1
           p2_old = p2
	   err1_old = err1
           err2_old = err2
	endif
c
c       output to parameter file
c
	write (lparm,14) ihst,p1,p2,err1,err2,chi2df,istat
c	write (6,110) ihst,p1,p2,err1,err2,chi2df,istat
 14	format (i6,4f10.3,f8.2,i4)
c
c       output all histogram parameters
c
	select = 'y'
c
c       output selected files to kumac file
c
	if (select.eq.'y' .or. answer.ne.'s') then
	   write (lkumac,*) 'hi/plot',ihst,'(-200.:200.,-4.:4.) col'
	   write (lkumac,*) 'vector/input par ',p1,p2,chi2df
	   write (lkumac,*) 'func/plot linear.f -200. 200. s' 
	   write (lkumac,*) 'exe window#push'
	   write (lkumac,*) 'text 0.1 0.5 ''Offset=''//$eval(par(1)) 0.2'
	   write (lkumac,*) 'text 0.1 0.3 ''Atten=''//$eval(par(2)) 0.2'
	   write (lkumac,*) 'text 0.1 0.1 ''Chi2=''//$eval(par(3)) 0.2'
	   write (lkumac,*) 'exe window#pop'
           if(ihst.eq.348)then
           write (lkumac,*) 'hi/del 0 '
           endif
c	   write (lkumac,*) 'wait' 
	endif
c
c       get next plot
c
	go to 100
900	end

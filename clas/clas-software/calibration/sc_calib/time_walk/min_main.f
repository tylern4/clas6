	Program Min_main 
c
c	Main program to drive Minuit
c	External subroutine to compute chi square is min_walk.
c	This routine must be provided.
c
	implicit none
	real min_walk
	external min_walk
	integer linput, loutput, lsave, ldata, flag, lparm, ihst,lkumac,w2,w3
	integer npts
	double precision parm, error, bound1, bound2,a
	real p1, p2, p3, p4, err1, err2, err3, err4, chi2df, chimin,af
	real  p1_old,p2_old,p3_old,p4_old,err1_old,err2_old,err3_old,err4_old,chi2df_old
        real xd_min, xd_max
        real a2,a3,a0
        data a2,a3,a0 /13.87, 0.074,19.56/
	integer ihst_old, istat_old
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
	data p1_old, p2_old, p3_old, p4_old, err1_old, err2_old, err3_old, err4_old
     1  /   17.7898, 66.4717, 0.013849, 40.233, 100.,100.,100.,100./

	data chimin /10./
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
	write (lkumac,*) 'vector/create par(4) R 4*0.'	
	write (lkumac,*) 'vector/create chi(1)  R 1*0.'	


c
c       loop over data / one per plot until EOF
c
100	continue
	read (linput,*,end=900) control, ihst, npts, xd_min, xd_max !selecting proper range for fit
c
c	print *, xd_min, xd_max                                     
        if (xd_min.gt.100.or.xd_max.lt.2000)then
	   print *,'bad range'	
c	   goto 100             
	endif                                                       ! end selecting proper fit
c
c
c	write (6,*) control, ihst, npts
	call minuit (min_walk,0)
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
c            note: normalization is given by chi2 per ndf per status
c                  empirically it seems that this allows comparison of values
c
	     chi2df = chi2/ndf/istat
	else
	     chi2df = 100.
	     write (6,*) '*** Min_main - chi2df=0., ihst=',ihst
	endif
	select = 'n'
	if (chi2df.gt.chimin .or. istat.eq.0) then
	   write (6,*) 'Min_main - ihst=',ihst,' istat=',istat,
     1                 ' chi2df=',chi2df
	   select = 'y'
c
c          set parameters to values of previous fit.
c
	   p1 = p1_old
	   p2 = p2_old
	   p3 = p3_old
	   p4 = p4_old
	   err1 = err1_old
	   err2 = err2_old
	   err3 = err3_old
	   err4 = err4_old
	   chi2df = 100.
	   istat = 0.
	endif

c
c       output to parameter file
c
              a = (a2*a3/p3)*(a0**(p3-a3))  ! determine p2 from p3 (constraint w2=g(w3))
c	write (lparm,110) ihst,p1,p2,p3,p4,err1,err2,err3,err4,chi2df,istat	
	write (lparm,110) ihst,p1,a,p3,p4,err1,err2,err3,err4,chi2df,istat,p2 ! output 
c	write (6,110) ihst,p1,p2,p3,err1,err2,err3,chi2df,istat
 110	format (i6,2(2f9.3,f9.3,f9.3),f9.3,i4,f9.3)

c
c       output selected files to kumac file
c
	if (select.eq.'y' .or. answer.ne.'s') then
	   write (lkumac,*) 'hi/plot',ihst,'(0.:8000.,0.:60.) box'
c	   write (lkumac,*) 'vector/input par ',p1,p2,p3,p4
	   write (lkumac,*) 'vector/input par ',p1,a,p3,p4     ! output for kumac file
           write (lkumac,*) 'vector/input chi ',chi2df
           write (lkumac,*) 'func/plot time_walk.f 0. 8000. s'             
c                write (lkumac,*) 'exec window'

c          output parameters to each histogram
c--------------------------------------------------------------------------
           write (lkumac,*) 'exec window call push'
c           write (lkumac,*) 'text 1800 40.25 W1=//$eval(par(2)) 0.2'
           write (lkumac,*) 'text 1800 40.25 W1=//$eval(par(2)) 0.2'
           write (lkumac,*) 'exec window call pop'

           write (lkumac,*) 'exec window call push'
           write (lkumac,*) 'text 1800 35.25 W2=//$eval(par(3)) 0.2'
           write (lkumac,*) 'exec window call pop'

           write (lkumac,*) 'exec window call push'
           write (lkumac,*) 'text 1800 30.25 W0=//$eval(par(4)) 0.2'
           write (lkumac,*) 'exec window call pop'

           write (lkumac,*) 'exec window call push'
           write (lkumac,*) 'text 1800 20.25 chisq=//$eval(chi(1)) 0.2'
           write (lkumac,*) 'exec window call pop'



c--------------------------------------------------------------------------                       
                   write (lkumac,*) 'hi/del 0' 
	           write (lkumac,*) 'wait' 
	endif
c
c       remember current parameters
c
	ihst_old = ihst
	p1_old = p1
	p2_old = p2
	p3_old = p3
	p4_old = p4
	err1_old = err1
	err2_old = err2
	err3_old = err3
	err4_old = err4
        chi2df_old = chi2df
	istat_old = istat	
c
c       get next plot
c
	go to 100
900	end


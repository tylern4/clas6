	Program Min_main 
c
c	Main program to drive Minuit
c	External subroutine to compute chi square is min_veff.
c	This routine must be provided.
c
	implicit none
	external min_veff
	real min2_veff
	external min2_veff
	integer linput, loutput, lsave, ldata, flag, lparm, ihst,lkumac
	integer npts
	double precision parm, error, bound1, bound2
	real p1, p2,err1, err2, chi2df, chimin
	character*10 name
	character*1 answer, select
	character*20 control
	integer ierflag
	character*50 min_input
	character*50 min_output, min_parm, min_kumac
	Parameter (min_output='min_output',min_input='min_input',
     1             min_parm='min_parm', min_kumac='min_kumac')
c
	double precision chi2, fedm, errdef
	integer npari, nparx, istat, ndf
c
	Parameter (linput=95, loutput=92, lsave=7, ldata=91, lparm=98,
     1             lkumac=99)
	data chimin /10./
c
c       Write out files in use
c
c	call getarg(1,argv)
c	if(argv.lt.'1'.or.argv.gt.'6') stop 'Argument should be sector number'
c	min_input='veff_s'//argv//'.dat'
c	min_output='veff_s'//argv//'.out'
c	min_parm='veff_s'//argv//'.parm'
c	min_kumac='veff_s'//argv//'.kumac'
	write (6,*) 'Min_veff - Input  File=',min_input
	write (6,*) 'Min_veff - Output File=',min_output
	write (6,*) 'Min_veff - Parm   File=',min_parm
	write (6,*) 'Min_veff - Kumac  File=',min_kumac
c
c       open files
c       
	open (unit=linput,file=min_input,status='unknown')
	open (unit=loutput,file=min_output,status='unknown',
     1		form='formatted')
	open (unit=lparm,file=min_parm,status='unknown',
     1		form='formatted')
	open (unit=lkumac,file=min_kumac,status='unknown',
     1		form='formatted')
c
	call mintio (linput,loutput,lsave)
c
c       output headers
c
	write (lkumac,*) 'vector/create par(2) R 2*0.'	
c
c       loop over data / one per plot until EOF
c
100	continue
	read (linput,*,end=900) control, ihst, npts
	call minuit (min_veff,0)
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
	else
	     chi2df = 100.
	     write (6,*) '*** Min_veff - chi2df=0., ihst=',ihst
	endif
c	write (6,110) ihst,p1,p2,err1,err2,chi2df,istat

c	call clean_data(p1,p2,1.0)
c	call mnexcm(min2_veff,'restore',0,0,ierflag,0)
c	call mnexcm(min2_veff,'migrad',0,0,ierflag,0)
c	call mnpout (1,name,parm,error,bound1,bound2,flag)
c	p1 = parm
c	err1 = error
c	call mnpout (2,name,parm,error,bound1,bound2,flag)
c	p2 = parm
c	err2 = error
c	call mnstat (chi2,fedm,errdef,npari,nparx,istat)
c	ndf = npts - npari + 1
c	if (ndf.gt.0 .and. istat.gt.0) then
c
c	     chi2df = chi2/ndf/istat
c
c	else
c	     chi2df = 100.
c	     write (6,*) '*** Min_main - chi2df=0., ihst=',ihst
c	endif
c
c       output to parameter file
c
	write (lparm,110) ihst,p1,p2,err1,err2,chi2df,istat
 110	format (i6,4f15.3,f7.2,i4)
	select = 'n'
	if (chi2df.gt.chimin .or. istat.eq.0) then
	   write (6,*) 'Min_veff - ihst=',ihst,' istat=',istat,
     1                 ' chi2dfstat=',chi2df
	   select = 'y'
	endif
c
c       output selected files to kumac file
c
	if (select.eq.'y' .or. answer.ne.'s') then
	   write (lkumac,*) 'hi/plot',ihst,' box'
	   write (lkumac,*) 'vector/input par ',p1,p2
	   write (lkumac,*) 'set dmod 1'
	   write (lkumac,*) 'func/plot linear.f -200 200 s'
	   write (lkumac,*) 'itx -60 40 ''slope=',p2,''''
	   write (lkumac,*) 'itx -60 35 ''intercept=',p1,'''' 
	   write (lkumac,*) 'exe window#pop'
           if(ihst.eq.348)then
           write (lkumac,*) 'hi/del 0 '
           endif
	endif
c
c       get next plot
c
	go to 100
 900	close(linput)
	close(loutput)
	close(lsave)
	close(lkumac)
	close(lparm)

	end








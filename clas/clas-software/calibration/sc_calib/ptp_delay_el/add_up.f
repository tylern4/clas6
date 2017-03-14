C-
C- Adding up contributions from fine and crude tunings
C-
	subroutine add_up()
c
	integer iSec, iStrip
	real Time0
c
	vector dt_s(48,6)
	vector dt_el(48,6)
	vector dt_ss(6)
	vector dt(48,6)
	vector status(48,6)
C-
	RF_Period = 2.004
C-
	open(unit=99, file='dt.dat',status='UNKNOWN')
C-
	do iSec=1, 6
	 do iStrip=1, 48
	  Time0 = DT_SS(iSec)*RF_Period+DT_EL(iStrip,iSec)*RF_Period+
     +	  DT_S(iStrip,iSec)
	  dt(iStrip,iSec) = Time0
	  write(99,1000)iSec, iStrip, dt(iStrip,iSec), 
     &	   status(iStrip,iSec)
	 enddo
	enddo
	write(99,*)' '
C-
	close(99)	 
C-
1000 	format(1X, I4, 2X, I4, 2X, G13.7, 2X, F3.1 )
C
	return
	end

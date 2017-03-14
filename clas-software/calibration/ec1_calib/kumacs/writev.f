	subroutine writev
	integer isect, istack, lp
	vector sec1Txi,sec1Tyi,sec1Txo,sec1Tyo
	vector sec2Txi,sec2Tyi,sec2Txo,sec2Tyo
	vector sec1dTxi,sec1dTyi,sec1dTxo,sec1dTyo
	vector sec2dTxi,sec2dTyi,sec2dTxo,sec2dTyo
	close(unit=4)
	open(unit=4,file='calib_t.dat',status='NEW')
c
c       save sector 1
c
	isect  = 1
	istack = 1
	do lp=1,24
	   tleft  = -(2.*sec1Txi(lp)+sec1dTxi(lp))/2.
	   tright = -(2.*sec1Txi(lp)-sec1dTxi(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
	istack = 2
	do lp=1,40
	   tleft  = -(2.*sec1Tyi(lp)+sec1dTyi(lp))/2.
	   tright = -(2.*sec1Tyi(lp)-sec1dTyi(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
	istack = 3
	do lp=1,24
	   tleft  = -(2.*sec1Txo(lp)+sec1dTxo(lp))/2.
	   tright = -(2.*sec1Txo(lp)-sec1dTxo(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
	istack = 4
	do lp=1,40
	   tleft  = -(2.*sec1Tyo(lp)+sec1dTyo(lp))/2.
	   tright = -(2.*sec1Tyo(lp)-sec1dTyo(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
c
c       save sector 2
c
	isect  = 2
	istack = 1
	do lp=1,24
	   tleft  = -(2.*sec2Txi(lp)+sec2dTxi(lp))/2.
	   tright = -(2.*sec2Txi(lp)-sec2dTxi(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
	istack = 2
	do lp=1,40
	   tleft  = -(2.*sec2Tyi(lp)+sec2dTyi(lp))/2.
	   tright = -(2.*sec2Tyi(lp)-sec2dTyi(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
	istack = 3
	do lp=1,24
	   tleft  = -(2.*sec2Txo(lp)+sec2dTxo(lp))/2.
	   tright = -(2.*sec2Txo(lp)-sec2dTxo(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
	istack = 4
	do lp=1,40
	   tleft  = -(2.*sec2Tyo(lp)+sec2dTyo(lp))/2.
	   tright = -(2.*sec2Tyo(lp)-sec2dTyo(lp))/2.
	   write(4,100) isect, istack, lp, tleft, tright
	end do
	close(unit=4)
 100	format(3(1i3,2x),2(f10.3,2x))
	return
	end

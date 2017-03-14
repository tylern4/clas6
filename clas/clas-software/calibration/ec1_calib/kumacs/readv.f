	subroutine readv
	integer isect, istack, ilp
	real dummy, tleft, tright
	vector sec1Txi,sec1Tyi,sec1Txo,sec1Tyo
	vector sec2Txi,sec2Tyi,sec2Txo,sec2Tyo
	vector sec1dTxi,sec1dTyi,sec1dTxo,sec1dTyo
	vector sec2dTxi,sec2dTyi,sec2dTxo,sec2dTyo
	close(unit=4)
	open(unit=4,file='calib_prev.dat',status='OLD')
	do loop=1,256
	   read(4,*,end=10) isect, istack, ilp, dummy, dummy, tleft, tright

10	   if (isect.eq.1) then
	      if (istack.eq.1) then
		 sec1Txi(ilp)  = -(tright+tleft)/2.
		 sec1dTxi(ilp) = tright-tleft
	      else if (istack.eq.2) then
		 sec1Tyi(ilp)  = -(tright+tleft)/2.
		 sec1dTyi(ilp) = tright-tleft
	      else if (istack.eq.3) then
		 sec1Txo(ilp)  = -(tright+tleft)/2.
		 sec1dTxo(ilp) = tright-tleft
	      else
		 sec1Tyo(ilp)  = -(tright+tleft)/2.
		 sec1dTyo(ilp) = tright-tleft
	      endif
	   else
	      if (istack.eq.1) then
		 sec2Txi(ilp)  = -(tright+tleft)/2.
		 sec2dTxi(ilp) = tright-tleft
	      else if (istack.eq.2) then
		 sec2Tyi(ilp)  = -(tright+tleft)/2.
		 sec2dTyi(ilp) = tright-tleft
	      else if (istack.eq.3) then
		 sec2Txo(ilp)  = -(tright+tleft)/2.
		 sec2dTxo(ilp) = tright-tleft
	      else
		 sec2Tyo(ilp)  = -(tright+tleft)/2.
		 sec2dTyo(ilp) = tright-tleft
	      endif
	   endif
	end do
	close(unit=4)
 100	format(3(1i3,2x),2(f10.3,2x))
	return
	end

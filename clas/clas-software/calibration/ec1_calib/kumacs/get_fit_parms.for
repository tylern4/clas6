	subroutine get_fit_parms(sect,layer,stack)
	integer stack,layer,sect
	real peak,sig
	COMMON/PAWPAR/PAR(3)
	COMMON/mypar/peak(2,4,40),sig(2,4,40)
c	print *,sect,layer,stack
	
	peak(sect,layer,stack)=PAR(2)
	sig (sect,layer,stack)=PAR(3)
	
	return
	end

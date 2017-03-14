	SUBROUTINE BOS_END

	integer ierr

#include "bcs.inc"     

        call fwbos(iw,33,'0',ierr)
        call bosta
        call fparm('CLOSE BOSOUTPUT')
        write(6,*) 'BOSOUTPUT file closed'
        RETURN
        END

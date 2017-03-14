C-----------------------------------------------
C Gives onshell momentum for a given W, m1 and m2
C output is 500 if we are below threshold
C -----------------------------------------------
       subroutine onshell(w,am1,am2,p0)
       implicit real*8(a-h,o-z)
       complex*16 w,p0,w2

       p0=400.
       if(dble(w).gt.(am1+am2))then
       w2=(w**2-am1**2-am2**2)**2-4.*am1**2*am2**2
       p0=sqrt(w2)/2/w
       end if
       return
       end 

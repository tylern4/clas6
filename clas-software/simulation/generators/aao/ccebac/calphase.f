C -----------------------------------------
C Computes phaseshifts and inelasticities
C -----------------------------------------
       subroutine calphase(am1,am2,sk0,ztmx,ztmxo,eta,delta)
       implicit real*8(a-h,o-y)
       implicit complex*16(z)
       e1=sqrt(am1**2+sk0**2)
       e2=sqrt(am2**2+sk0**2)
       phase=3.14159*sk0*e1*e2/(e1+e2)
       ztmxo=-ztmx*phase
       zi=cmplx(0.,1.)
       zs=1-2.*zi*phase*ztmx
       eta=abs(zs)

         
       zs=zs/eta
       zs=sqrt(zs)
       sr=dble(zs)
       si=imag(zs)
       ratio=si/sr
       delta=atan(ratio)*180./3.14159

       eta=1.-eta

       return
       end


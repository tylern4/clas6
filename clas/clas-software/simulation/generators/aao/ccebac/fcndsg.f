       SUBROUTINE FCNdsg (NPAR,GRAD,CHI,PAR,IFLAG)
       IMPLICIT REAL*8 (A-H,O-Y),integer(i-n)
       implicit complex*16(z)
       character*10 parnam
       PARAMETER (MAXH=100)
       DIMENSION GRAD(*),PAR(MAXH)
       common/helpar/hpar(MAXH)
       common/tsato0824/nchx
       common/lcs1/ipar,sca,ifit

       common /pgplotwrite/iwriteplot
       common /noprintout/ isw5
       common /etapi / jetapi

       do i=1,MAXH
         hpar(i)=par(i)
         if (i.eq.ipar) hpar(i)=par(i)*sca
       enddo

       call modcpl	! redefine nonresonant parameters
       
       print *, 'FCN called with iflag=',iflag
       
       if (ifit.ne.1) then
       if (iflag.eq.1) then
         chi=100
         return
       elseif (iflag.eq.4) then
         chi=100
         return
       endif
       endif
       
       call caljme(nchx)

       chi=0.
       
       do ichpi=0,2   !pi0p,pi+n,etap
         call calcrs(ichpi,xdpsgchi,iflag)
         chi=chi+xdpsgchi
       enddo

       iwriteplot=0

       if (mod(icall,200).eq.0) then 
         iwriteplot=1
         icall=0
         write(668,1668) "NEXT",chi
 1668    format(A4,2x,E14.6)
         do ic=1,maxh
         write(668,1669) ic,hpar(ic)
         enddo
 1669    format(I3,7x,E10.4)
       endif

       icall=icall+1
       
       do i=1,82
         call mnpout(i,parnam,val,err,bnd1,bnd2,ivar)
         if (err.gt.0) print *, i,parnam,val
       enddo

       end

c  scgeom.inc

c  $Id: scgeom.inc,v 1.3 1998/01/14 05:09:54 holtrop Exp $

      character*132 ciscgeom
      data ciscgeom/
     1'$Id: scgeom.inc,v 1.3 1998/01/14 05:09:54 holtrop Exp $'
     1/

c  nscin is the number of scintillators of different dimensions
c  ngeoch is the number of geometry channels after ganging scint.
c  nddige is the first scintillator to be ganged
c  ipmax is the maximum ip(plane and/or width different)
c  there are four planar surfaces, but six ip numbers
c  ifirst is the first scintaillator channel of a given ip number
c  ilast is the last scintillator channel of a given ip number
c  phiort is the normal to each surface ip, only four are different
c  alsc is the length of the scintillators
c  xm,ym,zm is the position of the physical center of the scintillator
c  the 48 geometry channels will be transformed to 57 scintillators
c  the information from the 57 scintillators will be combined to 
c       produce 48 electronic channels of information

	integer nscin,ngeoch,nddige,ipmax
	parameter (nscin = 57)
        parameter (ngeoch= 48)
        parameter (nddige= 40)
        parameter (ipmax =  4)
        integer ifirst(ipmax),ilast(ipmax) 
        real phiort(ipmax),alsc(nscin),xm(nscin),ym(nscin),zm(nscin),
     +       wdth(nscin),thck(nscin)
        common/ scgeom/ ifirst,ilast,phiort,alsc,xm,ym,zm,wdth,thck

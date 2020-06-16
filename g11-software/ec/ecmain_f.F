      SUBROUTINE ECMAIN_F(iw,threshold,option)
c
      IMPLICIT NONE
      SAVE
c
      integer iw(*)
      real threshold(3)
      integer option(4)
#if defined(USEGNU) || defined(Linux)
      print *,'FORTRAN do not support structures for this platform,'
      print *,'please use C interface ( ecmain_c.c )'

#else if defined(SunOS) || defined(HPUX)

c
#include "eclib.inc"
c
      integer nsec,j,uvw,io,Npsble
      record /ecstrip/ strip(36,3,2)
c
      integer npeak(3,2)
      record /ecpeak/ peak(18,3,2)
c
      integer nhit
      record /echit/ hit(1000)
c
      external echit,ecstrips,ecpeak,eccorr,ecbos,ecshower
      integer echit,ecstrips,ecpeak,eccorr,ecbos,ecshower
c-----------------------------------------------------------------------
c
c reconstruction
c
      nhit = 1
      DO nsec=1,6 ! sectors loop
        IF(ecstrips(iw,threshold(1), option(1), option(2), nsec, strip).GT.0) THEN
        DO io=1,2 ! inner and outer
          DO uvw=1,3
            npeak(uvw,io)=ecpeak(threshold(2), strip(1,uvw,io), peak(1,uvw,io))
          ENDDO
          Npsble=echit(io,nsec,npeak(1,io),peak(1,1,io),hit(nhit))
          IF(Npsble.GT.0) THEN
            j=eccorr(threshold(2),option(4),io,nsec,npeak(1,io),peak(1,1,io),Npsble,hit(nhit))
            nhit = nhit + j
          ENDIF
        ENDDO
        ENDIF
      ENDDO
      nhit = nhit - 1
ccc      j = ecbos(nhit,hit) ! fill ECHB bank
      j = ecshower(nhit,hit) ! fill ECRB bank
c



      print *,'FORTRAN do not support structures for this platform,'
      print *,'please use C interface ( ecmain_c.c )'

#endif

      RETURN
      END




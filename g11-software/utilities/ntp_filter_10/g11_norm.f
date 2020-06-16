

      subroutine g11_norm
      INCLUDE "ntpl_goa.inc"
      REAL*4   NGammaTtmp(61), NGammaEtmp(767)
      REAL*4   NGammaEnrebin1tmp(225),NGammaEnrebin2tmp(90)
      REAL*4   add1,add2,add3,add4
c+ G11 Normalization analysis
c++ Initialization
c      DO i=1,61
c        NGammaT(i) = 0.
c      ENDDO
      print *,'  INFO: G11 Normalization analysis'
      print *,'  INFO: Reading histo 71 to get the T counts'
      CALL HUNPAK(71,NGammaTtmp,'HIST',0)
      print *,'  INFO: Reading histo 72 to get the E counts'
      CALL HUNPAK(72,NGammaEtmp,'HIST',0)
      print *,'  INFO: Reading histo 73 and 74 to get the rebinned counts'
      CALL HUNPAK(73,NGammaEnrebin1tmp,'HIST',0)
      CALL HUNPAK(74,NGammaEnrebin2tmp,'HIST',0)
      add1 = 0.
      DO i=1,61
        NGammaT(i) = NGammaT(i)+NGammaTtmp(i) 
        add1=add1+NGammaTtmp(i)
      ENDDO
       add2 = 0.
      DO i=1,767
        NGammaE(i) = NGammaE(i)+NGammaEtmp(i)  
        add2=add2+NGammaEtmp(i)
      ENDDO
      add3 = 0.
      DO i=1,225
        NGammaEnRebin1(i) = NGammaEnRebin1(i)+NGammaEnRebin1tmp(i) 
        add3=add3+NGammaEnRebin1tmp(i)
      ENDDO
      add4 = 0.
      DO i=1,90
        NGammaEnRebin2(i) = NGammaEnRebin2(i)+NGammaEnRebin2tmp(i) 
        add4=add4+NGammaEnRebin2tmp(i)
      ENDDO
      NgammaTthisfile = add1 ! add1 or add2
      print *,'  NGammaTthisfile(sumT,sumE) = ',add1
      print *,'  NGammaTthisfile(sumT,sumE) = ',add2
      CALL HPAK(71,NGammaT)
      CALL HPAK(72,NGammaE)
      CALL HPAK(73,NGammaEnrebin1)
      CALL HPAK(74,NGammaEnrebin2)
      RETURN
      END


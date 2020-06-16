      subroutine book_st_nt(ntid)
      IMPLICIT NONE
      integer ntid

#include "STnt.inc"

      call hbname(ntid,'ST',nST,'nST[0,24]:I,'//
     1'secST(nST)[1,6]:I,idST(nST)[1,4]:I,'//
     1'TDCST(nST)[0,4095]:I,ADCST(nST)[0,16383]:I')

      call hbname(ntid,'STR',nSTR,'nSTR[0,24]:I,'//
     1'idSTR(nSTR)[0,699]:I,'//
     1'STR_trk(nSTR)[0,699]:I,'//
     1'STR_t(nSTR):R,'//
     1'STR_l(nSTR):R,'//
     1'STR_z(nSTR):R,'//
     1'STRstat(nSTR)[0,10]:I')

      return
      end

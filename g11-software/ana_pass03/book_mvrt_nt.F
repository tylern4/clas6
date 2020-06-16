      subroutine book_mvrt_nt(ntid)
      IMPLICIT NONE
      integer ntid

#include "MVRTnt.inc"

      call hbname(ntid,'MVRT',vidMVRT,'vidMVRT:I,'//
     1'ntrMVRT:I,xMVRT:R,yMVRT:R,zMVRT:R,ch2MVRT:R,'//
     2'cxxMVRT:R,cxyMVRT:R,cxzMVRT:R,cyyMVRT:R,cyzMVRT:R,'//
     3'staMVRT:I')


      return
      end

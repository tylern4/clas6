      subroutine fill_mvrt_nt
      
#include "bcs.inc"
#include "MVRTnt.inc"
           
      integer min, max, idummy
      integer name_i1,name_i0,idx0,idx1,i,j,k,sector,ifirst,bar,j_pd
      integer ind,nd,str_col,indx
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      data ifirst/0/ 
      equivalence (I16(1), I32(1))
      save name_i
      

*** fill MVRT information
      vidMVRT = 0
      ntrMVRT = 0.
      xMVRT   = 0.
      yMVRT   = 0.
      zMVRT   = 0.
      ch2MVRT = 0.
      cxxMVRT = 0.
      cxyMVRT = 0.
      cxzMVRT = 0.
      cyyMVRT = 0.
      cyzMVRT = 0.
      czzMVRT = 0.
      staMVRT = 0
      IND=mlink(iw,'MVRT ',1)
      IF(IND.NE.0) THEN
         if(abs(RW(ind+3)).lt.100000.) then
            vidMVRT = IW(ind+1)
            ntrMVRT = IW(ind+2)
            xMVRT   = RW(ind+3)
            yMVRT   = RW(ind+4)
            zMVRT   = RW(ind+5)
            ch2MVRT = min(RW(ind+6),100000.)
            cxxMVRT = RW(ind+7)
            cxyMVRT = RW(ind+8)
            cxzMVRT = RW(ind+9)
            cyyMVRT = RW(ind+10)
            cyzMVRT = RW(ind+11)
            czzMVRT = RW(ind+12)
            staMVRT = IW(ind+13)
         endif
      ENDIF
      

 99   return
      end





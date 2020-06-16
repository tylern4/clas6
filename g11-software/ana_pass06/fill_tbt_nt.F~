      subroutine fill_tbt_nt
      
#include "bcs.inc"
#include "TBTnt.inc"
           
      integer name_TBTR,name_TBER,ND,IND,ncol,INDX
      
      name_TBTR = mamind(iw,'TBTR')
      nTBTR=0
      IND=IW(name_TBTR)
      IF(IND.NE.0) THEN
         ND=IW(IND)
         ncol=IW(ind-5)
         DO in = 1, ND, ncol
            INDX=IND+IN-1
               nTBTR=nTBTR+1
               xTBTR(nTBTR) = RW(INDX+1)
               yTBTR(nTBTR) = RW(INDX+2)
               zTBTR(nTBTR) = RW(INDX+3) 
               pxTBTR(nTBTR) = RW(INDX+4)
               pyTBTR(nTBTR) = RW(INDX+5)
               pzTBTR(nTBTR) = RW(INDX+6)
               qTBTR(nTBTR) =  RW(INDX+7)
               c2TBTR(nTBTR) = RW(INDX+8)
               secTBTR(nTBTR) = IW(INDX+9)
               hbtTBTR(nTBTR) = IW(INDX+10) ! HB track number
         ENDDO
      ENDIF
      name_TBER = mamind(iw,'TBER')
      nTBER=0
      IND=IW(name_TBER)
      IF(IND.NE.0) THEN
         ND=IW(IND)
         ncol=IW(ind-5)
         DO in = 1, ND, ncol
            INDX=IND+IN-1
               nTBER=nTBER+1
               qpTBER(nTBER) = RW(INDX+1)
               lbTBER(nTBER) = RW(INDX+2)
               phiTBER(nTBER) = RW(INDX+3)
               d0TBER(nTBER) = RW(INDX+4)
               z0TBER(nTBER) = RW(INDX+5)
               c11TBER(nTBER) = RW(INDX+6)
               c12TBER(nTBER) = RW(INDX+7)
               c13TBER(nTBER) = RW(INDX+8)
               c14TBER(nTBER) = RW(INDX+9)
               c15TBER(nTBER) = RW(INDX+10)
               c22TBER(nTBER) = RW(INDX+11)
               c23TBER(nTBER) = RW(INDX+12)
               c24TBER(nTBER) = RW(INDX+13)
               c25TBER(nTBER) = RW(INDX+14)
               c33TBER(nTBER) = RW(INDX+15)
               c34TBER(nTBER) = RW(INDX+16)
               c35TBER(nTBER) = RW(INDX+17)
               c44TBER(nTBER) = RW(INDX+18)
               c45TBER(nTBER) = RW(INDX+19)
               c55TBER(nTBER) = RW(INDX+20)
               chi2TBER(nTBER) = RW(INDX+21)
               lay1TBER(nTBER) = IW(INDX+22)
               lay2TBER(nTBER) = IW(INDX+23)
         ENDDO
      ENDIF




 99   return
      end





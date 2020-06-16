      subroutine fill_st_nt
      
#include "bcs.inc"
#include "STnt.inc"
           
      integer min, max, idummy
      integer name_i1,name_i0,idx0,idx1,i,j,k,sector,ifirst,bar,j_pd
      integer ind,nd,str_col,indx
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      data ifirst/0/ 
      equivalence (I16(1), I32(1))
      save name_i
      

*** fill ST raw information
      if (ifirst.eq.0) then
         name_i1 = mamind(iw,'STN1')
         name_i0 = mamind(iw,'STN0')
        if (name_i1.eq.0.OR.name_i0.eq.0) then
          print *,' NO ST bank registered'
          return
        endif
        ifirst = 1
      endif
      
      do i=1,24
         idST (i)=0
         secST(i)=0
         adcST(i)=0
         tdcST(i)=0
      enddo
      
      nST = 0
      idx0 = iw(name_i0)
      idx1 = iw(name_i1)
      if(idx0.ne.0.AND.idx1.ne.0) then 
         continue
      else
         goto 99
      endif
      nST  =IW(idx1-4)
      do i = 1, nST 
         j=(i-1)*2
         j_pd = iw16(2*idx1+j+1)
         adcST(j_pd)= iw16(2*idx1+j+2)
      enddo
      nST  =IW(idx0-4)
      do i = 1, nST 
         j=(i-1)*2
         idST(i)=iw16(2*idx0+j+1)
         secST(i) = int((idST(i)-1)/4)+1
         idST(i)=idST(i)-(secST(i)-1)*4
         tdcST(i) = iw16(2*idx0+j+2)
      enddo

*** fill STR information
      nSTR=0
      IND=mlink(iw,'STR ',1)
      IF(IND.NE.0) THEN
         nSTR    = IW(IND-4)
         str_col = IW(IND-5)
         DO i = 1,nSTR
            indx = ind + (i -1)*str_col
            idSTR(i)=iw(indx+1)
            STR_trk(i)=iw(indx+2)
            STR_t(i)=rw(indx+3)
            STR_l(i)=rw(indx+4)
            STR_z(i)=rw(indx+5)
            STRstat(i)=iw(indx+6)
         ENDDO
      ENDIF
      

 99   return
      end





c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Sut Feb 15 14:32:25 EST 2003
c  -------
c
c  Major revisions:
c  ----------------
c     
      Logical Function user_fc(id,p,th,ph)
      integer id
      real p,th,ph
      real pdep,tslop,p_slop,pp
      data p_slop/1.5/
      external tslop
c
      user_fc=.true.
      user_fc=p.gt.0.2
      if(id.eq.-321)then
         user_fc=th.gt.110./(6.4*(p)**0.5)
      elseif(id.eq.321.or.id.eq.2212)then
         user_fc=th.gt.7.
      elseif(id.eq.2112)then
         pdep=getrand(-1.)*tslop(0.15,p_slop)
c-tslop(2.,p_slop))+tslop(2.,p_slop)
         pp=-log(pdep)/abs(p_slop)
         user_fc=p.lt.pp.and.p.gt.0.15
      endif
c
      RETURN
      END


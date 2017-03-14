c =====================================
c
      Function FQ2(x)
c
#include "parm1.inc"
c      Implicit None
      Real Fq2,x
c
      fq2=1./x**nq2
c
      return
      end
c
c =====================================
c
      Function Tslop(x,t_slop)
c
c#include "parm1.inc"
c      Implicit None
      Real Tslop,x,t_slop
c
      ts=abs(t_slop)
      tslop=exp(-ts*x)
c      tslop=1.
c
      return
      end
c     
c     =====================================
c     
      Function Tgen(tmin,tmax,t_slop)
      Implicit None
c#include "parm1.inc"
c     
      save
      Real Tgen,tmin,tmax,trun,Tspace(200)
      Real getrand,tdepm,tdepx,tdep,t_slop
      logical first
      data first/.true./
      real tslop, tm, tx
      external tslop
c
c      if(first)then
c         first=.false.
c         tm=0.
c         tx=7.
c         call funLXP(tslop,tspace,tm,tx)
c      endif
c
      if(tmin.lt.tmax.and.tmin.gt.tm)then
c 111     call FunLUX(tspace,trun,i)
         tdep=getrand(-1.)*(tslop(tmin,t_slop)-tslop(tmax,t_slop))
     &       +tslop(tmax,t_slop)
         trun=-log(tdep)/abs(t_slop)
         call hf1(102,trun,1.)
         Tgen=trun
      else
         Tgen=10.
      endif
c
      return
      end
c
c     =====================================
c     
      Function dual_Tgen(tmin,tmax)
      Implicit None
c     
      save
      Real dual_Tgen,tmin,tmax,trun
      Real getrand,tdep,d_exp
      external d_exp
c
      if(tmin.lt.tmax)then
 101     tdep=getrand(-1.)*(d_exp(tmin)-d_exp(tmax))+d_exp(tmax)
         trun=tmin+getrand(-1.)*(tmax-tmin)
         if(tdep.gt.d_exp(trun))goto 101
         dual_Tgen=trun
      else
         dual_Tgen=10.
      endif
c
      return
      end
c
c =====================================
c
      Function d_exp(x)
c
      Implicit None
      Real d_exp,x,t_slop
      real a, b, c, d
      data a, b, c, d/5E05,41.67,0.36,3.13/
c
      d_exp=a*exp(-b*x)+c*exp(-d*x)
c
      return
      end
c     
c* average b1=41.67; a1=5E05; b2=3.13; a2=0.36; crossing at t=0.49


























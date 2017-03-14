      SUBROUTINE BREIT_AMPL (S,Q2,kgamma, qpion)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine BREIT_AMPL
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Wed Dec 11 17:43:38 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "gpar.inc"
#include "resonances.inc"
#include "fit_par.inc"
#include "multipoles.inc"
#include "delta_dat.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL S, Q2, kgamma, qpion
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile$')
      PARAMETER (CREVIS = '$Revision$')
      PARAMETER (CSTATE = '$State$')
      PARAMETER (CDATE = '$Date$')
      PARAMETER (CAUTHO = '$Author$')
      DATA CRCSID/   
     1'$Id$'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BREIT_AMPL')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
      INTEGER i, ll, j,k
      REAL M22,M2,En1,En2,kr22,qr22,kr,qr,X2,q22,k22
      REAL rvirtual,rpion,WW,Espion,q2pion,back1
      REAL E2pion,qr2pion,r2pion,qxl,qx2l
      REAL Gpion,Ginel,Gtotal,qxgamma,Ggamma,den
      REAL BreitW,Breit,step,corr,corr1
      REAL XIN,YOUT,DR(11),DRR(11)
      REAL Wthr,Eeta,qreta,reta,Gtotal1,REden,IMden
      REAL Eseta,qeta,DeltaW,step1
      REAL sm,mm,Epi,qpi22,qpi,q3,qrr,qrr2,qrm,nom,sind2
      REAL sindelta,cosdelta,del,delta
      REAL bb,aa,wc,Epionc,qc22,qc,fun,fun1,del12
c_end_var
c
c  executable code for routine BREIT_AMPL:
c----6----------------------------------------------------------------72
c-  Calculate Breit-Wigner formula for resonances according to Devenish&Lyth
c-  All contributions correspond to the reaction gamma + p => p + pi0
      q22=qpion**2
      k22=kgamma**2
	WW=sqrt(s)
      Wthr=mn+meta
      DeltaW=WW-Wthr
         Espion=(s-mn22+m2pion**2)/WW/2.  
         q2pion=Espion**2-m2pion**2
         Eseta=(s-mn22+meta**2)/WW/2.
         qeta=Eseta**2-meta**2
      if (DeltaW.ge.0.) go to 866    
        step1=0.
        go to 867
866   step1=1.
867     continue
      if (q2pion.ge.0.) go to 766
        step=0.
        go to 767
766   step=1.
767     continue
	 q2pion=q2pion*step
	 qeta=qeta*step1
c-  First do the loop for the P33(1232) resonance  for A(3/2)/1.5     
      DO i=1,1
         M22=MRES(i)**2	
         M2=2.*MRES(i)
         En1=(M22+mn22+Q2)/M2
         En2=(M22+mn22-mp22)/M2
         kr22=En1**2-mn22
         qr22=En2**2-mn22
         kr=sqrt(kr22)
         qr=sqrt(qr22)
         X2=XRES(i)**2
         rvirtual=kgamma/kr
         rpion=qpion/qr
         qxl=((X2+qr22)/(q22+X2))**Langul(i)
         Gpion=GRES(i)*rpion**(2.*Langul(i)+1.)*qxl
         Gtotal=Gpion/sqrt(s/M22)
         qxgamma=((X2+kr22)/(k22+X2))**Lprime(i)
         Ggamma=GRES(i)*rvirtual**(2.*Lprime(i)+1.)*qxgamma
         den=(M22-s)**2+(MRES(i)*Gtotal)**2
         BreitW=MRES(i)*sqrt(Gpion*Ggamma/(rvirtual*rpion))/den
         Breit=BreitW*MRES(i)*Gtotal
c
         EE3(i)=Breit*AE3(i)
         ES3(i)=Breit*rvirtual*AS3(i)
      ENDDO
c-  First do the loop for resonances with isospin 3/2  for A(3/2)/1.5     
      DO i=2,5
         M22=MRES(i)**2	
         M2=2.*MRES(i)
         En1=(M22+mn22+Q2)/M2
         En2=(M22+mn22-mp22)/M2
         kr22=En1**2-mn22
         qr22=En2**2-mn22
         kr=sqrt(kr22)
         qr=sqrt(qr22)
         X2=XRES(i)**2
         rvirtual=kgamma/kr
         rpion=qpion/qr
         E2pion=(M22-mn22+m2pion**2)/M2
         qr2pion=E2pion**2-m2pion**2
         r2pion=(q2pion/qr2pion)**(Langul(i)+2)
         qxl=((X2+qr22)/(q22+X2))**Langul(i)
         qx2l=((X2+qr2pion)/(q2pion+X2))**(Langul(i)+2)
         Gpion=eta(i)*GRES(i)*rpion**(2.*Langul(i)+1.)*qxl
         Ginel=(1.-eta(i))*GRES(i)*r2pion*qx2l
         Gtotal=Gpion+Ginel
         qxgamma=((X2+kr22)/(k22+X2))**Lprime(i)
         Ggamma=GRES(i)*rvirtual**(2.*Lprime(i)+1.)*qxgamma
         den=(M22-s)**2+(MRES(i)*Gtotal)**2
         BreitW=MRES(i)*sqrt(Gpion*Ggamma/(eta(i)*rvirtual*rpion))/den
         Breit=BreitW*MRES(i)*Gtotal
c
         EE3(i)=Breit*AE3(i)
         EM3(i)=Breit*AM3(i)
         ES3(i)=Breit*rvirtual*AS3(i)
      ENDDO
	DO i=6,13
         EE3(i)=0.
         EM3(i)=0.
         ES3(i)=0.
      ENDDO
c
c-  Now calculate the same for resonances with isospin 1/2.	
      DO ll=1,11
         i=ll+20
c	
         M22=MRES(i)**2	
         M2=2.*MRES(i)
         En1=(M22+mn22+Q2)/M2
         En2=(M22+mn22-mp22)/M2
         kr22=En1**2-mn22
         qr22=En2**2-mn22
         kr=sqrt(kr22)
         qr=sqrt(qr22)
         X2=XRES(i)**2
         rvirtual=kgamma/kr
         rpion=qpion/qr
         E2pion=(M22-mn22+m2pion**2)/M2
         qr2pion=E2pion**2-m2pion**2
         r2pion=(q2pion/qr2pion)**(Langul(i)+2)
         qxl=((X2+qr22)/(q22+X2))**Langul(i)
         qx2l=((X2+qr2pion)/(q2pion+X2))**(Langul(i)+2)
         Gpion=eta(i)*GRES(i)*rpion**(2.*Langul(i)+1.)*qxl 
         Ginel=(1.-eta(i))*GRES(i)*r2pion*qx2l
         Gtotal=Gpion+Ginel
         qxgamma=((X2+kr22)/(k22+X2))**Lprime(i)
         Ggamma=GRES(i)*rvirtual**(2.*Lprime(i)+1.)*qxgamma
         den=(M22-s)**2+(MRES(i)*Gtotal)**2
         BreitW=MRES(i)*sqrt(Gpion*Ggamma/(eta(i)*rvirtual*rpion))/den
         Breit=BreitW*MRES(i)*Gtotal

c-  Do the loop for  A(1/2)/3 (j=1) and A(0)(j=2) photons
         do j=1,2
            EE1(j,ll)=Breit*AE1(j,ll)
            EM1(j,ll)=Breit*AM1(j,ll)
            ES1(j,ll)=Breit*rvirtual*AS1(j,ll)
         enddo
      ENDDO
      DO ll=12,13
         do j=1,2
            EE1(j,ll)=0.
            EM1(j,ll)=0.
            ES1(j,ll)=0.
         enddo
      ENDDO
c     -  Now calculate the same for S11(1535) resonance
      M22=MRES(22)**2
      M2=2.*MRES(22)
      En1=(M22+mn22+Q2)/M2
      En2=(M22+mn22-mp22)/M2
      kr22=En1**2-mn22
      qr22=En2**2-mn22
      kr=sqrt(kr22)
      qr=sqrt(qr22)
      X2=XRES(22)**2
      rvirtual=kgamma/kr
      rpion=qpion/qr
      Eeta=(M22-mn22+meta**2)/M2
      qreta=Eeta**2-meta**2
      reta=sqrt((qeta/qreta)**2)
      reta=sqrt(reta)
      Gtotal1=GRES(22)*rpion
      BreitW=MRES(22)*GRES(22)
      Gpion=0.7*GRES(22)*rpion
      Gtotal=Gpion
      REden=M22-s
      IMden=MRES(22)*Gtotal
      den=REden**2+IMden**2
      Breit=BreitW*IMden/den
         do k=1,2
            EE1(k,2)=Breit*AE1(k,2)
            ES1(k,2)=Breit*rvirtual*AS1(k,2)
         enddo
      if (WW.ge.Wthr) go to 166
      go to 167
166   IMden=MRES(22)*(Gtotal+0.3*GRES(22)*reta)
      den=REden**2+IMden**2
      Breit=BreitW*IMden/den
         do k=1,2
            EE1(k,2)=Breit*AE1(k,2)
            ES1(k,2)=Breit*rvirtual*AS1(k,2)
         enddo
167   continue
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        sm=0.135
        mm=0.938
        Epi=(ww**2+sm**2-mm**2)/ww/2.
        qpi22=Epi**2-sm**2
        qpi=sqrt(qpi22)
        q3=qpi22*qpi
        qrr=0.2265
        qrr2=qrr**2
        qrm=qpi22-qrr2
        nom=(4.27*q3)**2
        sind2=nom/(nom+qrm**2*(1.+40.*qpi22*qrm+21.4*qpi22)**2)
        sindelta=sqrt(sind2)
        cosdelta=sqrt(1.-sind2)
        delta=asin(sindelta)
        If (qpi.ge.qrr) go to 266
        go to 267
266     delta =pi-delta
         cosdelta=-cosdelta
267     continue
        del=sindelta*cosdelta
        If (ww.ge.1.34) go to 366
        go to 367
366     del=sindelta*cosdelta*(1.34/(ww*(1.+10.*(ww-1.34)**2)))**2
367     continue
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	del12=1.3*qpion/(1.+2.5*qpion)/(1.+200.*qpion**4)
        bb=0.04
        aa=6.
        wc=1.3
        Epionc=(Wc**2+mp22-mn22)/Wc/2.
        qc22=Epionc**2-mp22
        qc=sqrt(qc22)
        qc=qpi/qc
        fun1=0.16*0.6*(1.+aa*Q2)/((Q2-1)**2+0.6*(1.+aa*Q2))
        fun=(1.+fun1)/(1.+fun1/(qc+bb)**4)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	XIN=WW
      CALL interp1(WSa,mp33i,144,4,XIN,YOUT)
         EM3(1)=YOUT*AM3(1)
c
	DRR(1)=0.813*(1-4.38*(ww-1.26)/(1.+Q2))
	DRR(1)=DRR(1)/(1.+0.6*Q2*(1.+0.4*Q2))	
	DRR(1)=DRR(1)*del12	
	DRR(3)=0.599/(1+2.*Q2+3.5/((Q2-2.5)**2+1.))
	DRR(3)=DRR(3)*(1.+3.*(ww-1.26))	
	DRR(3)=DRR(3)*del12
 	DRR(2)=(1.5*(Q2-1.))*(1.+0.4/((Q2-1.3)**2+0.5))/(1.+8.*Q2**2)
        DRR(2)=DRR(2)*(1.-(ww-1.26)*2.4)
        DRR(2)=-1.4*qpion*DRR(2)*(1.+(2.2*qpion)**2)/(1+20.*qpion**4)
        DRR(4)=-1.723*(1.+0.06/((Q2-0.6)**2+0.2))/(1.+2.*Q2)*(1+ww-1.26)
        DRR(4)=-0.6*qpion*DRR(4)*(1.+(2.2*qpion)**2)
        DRR(4)=DRR(4)/(15.*(ww-1.25)**2+0.1)/10.
	DRR(10)=-del*0.084/(Q2+0.26/(1.+125.*Q2**3))
	DRR(11)=-del*0.041*fun
	DRR(11)=DRR(11)*7./(1.+Q2/0.58)**1.25
c
	EE1(1,2)=EE1(1,2)+DRR(1)
	EE3(5)=EE3(5)+DRR(2)/1.5
	ES1(1,2)=ES1(1,2)+DRR(3)
	ES3(5)=ES3(5)+DRR(4)/1.5
	EE3(1)=EE3(1)+DRR(10)/1.5
	ES3(1)=ES3(1)+DRR(11)/1.5
c
       RETURN
       END
c----------------------------------------------------------------------------------------



                                                                                                   


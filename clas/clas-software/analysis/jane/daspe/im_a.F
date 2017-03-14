      SUBROUTINE IM_A (S,Q2,Sp)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine IM_A
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
c  Author:         Created:  Mon Dec 16 10:31:34 EST 1996
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
#include "gpar.inc"
#include "ettapar.inc"
#include "imphi.inc"
#include "ima.inc"
#include "fit_par.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL S, Q2, Sp
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
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='IM_A')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      DATA NWRITE/0/
      REAL pq,pk,x
      REAL d1,Wi,W2i,EGi,dt,ok,oq,pk2,XXi,pq2
      REAL on1i,on2i,op1i,om1i,op2i,om2i,Wmi,Wpi
      REAL AKi(6)
      REAL b(6,3)
      REAL dtt(6,3)
      REAL ax
      INTEGER i,j,m,n
      REAL d,dd
      REAL ph1,ph2,ph3,ph4,ph5
c_end_var
c
c  executable code for routine IM_A:
c----6----------------------------------------------------------------72
c
c      IF (NWRITE .LT. 1) THEN
c        NWRITE = NWRITE + 1
c        CRMESS='This is a DUMMY routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
c      ENDIF
        dd=Sp-S
 	d=1.D0/dd
	d1=Sp-u
c	
	Wi=sqrt(Sp)
	W2i=2.*Wi
	EGi=Sp-mn22
	dt=(EGi+qk)/2.
	on1i=(Sp+mn22+Q2)/W2i
	on2i=(Sp+mn22-mp22)/W2i
	ok=Wi-on1i
	oq=Wi-on2i
	pk2=on1i**2-mn22
	pk=sqrt(pk2)
	pq2=on2i**2-mn22
	pq=sqrt(pq2)
	x=(qk+ok*oq)/(pk*pq)
	op1i=on1i+mn
	om1i=on1i-mn
	op2i=on2i+mn
	om2i=on2i-mn
	Wmi=Wi-mn
	Wpi=Wi+mn
	
	XXi=ok*qk-oq*Q2
	AKi(1)=sqrt(op1i*op2i)
	AKi(2)=sqrt(om1i*om2i)
	AKi(3)=AKi(2)*op2i
	AKi(4)=AKi(1)*om2i
	AKi(5)=sqrt(om1i*op2i)/Q2
	AKi(6)=sqrt(op1i*om2i)/Q2
c- Get the Intermediate amplitudes for transition.
	CALL im_phi(sp,x,Q2,pk,pq)
c- 	 
	do i=1,3
	  do j=1,6
	    ph(j,i)=ph(j,i)/AKi(j)
	  end do
	end do
c- Calculate the imaginary parts of Invariant Amplitudes.
c- Invariant Amplitudes are defined according to - Devenish&Lyth, PR D5(1972),47
c- Following notations are made - b(1,i) => B1/4pi (Devenish&Lyth)
c-                                b(2,i) => B2/4pi (Devenish&Lyth)
c-                                b(3,i) => B3/4pi (Devenish&Lyth)
c-                                b(4,i) => B8/4pi (Devenish&Lyth)
c-                                b(5,i) => B5'/4pi(Devenish&Lyth)
c-                                b(6,i) => B6/4pi (Devenish&Lyth)
	do i=1,3
c-loops for isotopic combinations A(+), A(-),A(0)
	  b(1,i)=ph(1,i)-ph(2,i)
	  ph1=ph(3,i)*Wmi-ph(4,i)*Wpi
	  ph2=ph(3,i)+ph(4,i)
	  ph3=-(ph(1,i)*Wpi+ph(2,i)*Wmi)
	  ph4=ph(5,i)*Wmi-ph(6,i)*Wpi+XXi*ph1+
     +         Q2*(om1i*ph(2,i)-op1i*ph(1,i))
	  b(2,i)=ph4/(W2i*pk2)
	  ph5=ph(5,i)+ph(6,i)+
     +	      XXi*ph2+Wmi*op1i*ph(1,i)+Wpi*om1i*ph(2,i)
	  b(6,i)=-ph5/(Wi*pk2)
	  b(3,i)=(b(2,i)+ph1)/2.
	  b(4,i)=b(6,i)/2.-ph2
	  b(5,i)=ph3-dt*b(6,i)
	end do
c- 
	do m=1,6
	  do n=1,3
	    dtt(m,n)=d+ett(m,n)/d1
	  end do
	end do
c- Get the integrands for dispersion relations. 		
	do m=1,6
	  do n=1,3
	    ba(m,n)=b(m,n)*dtt(m,n)
	  end do		
	end do		
c- 	
c      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------





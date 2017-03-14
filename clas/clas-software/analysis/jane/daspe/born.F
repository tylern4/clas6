      SUBROUTINE BORN(S,Q2)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine BORN
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
c  Author:         Created:  Mon Dec 16 10:59:47 EST 1996
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
#include "ettapar.inc"
#include "gpar.inc"
#include "bornterms.inc"
#include "fit_par.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL S,Q2
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
      PARAMETER (CRNAME='BORN')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
      REAL d,d1,tau,f_1(2),f_2(2)
      REAL q1,z,Gmp,Gep,Gmn,Gen,F1p,F2p,F1n,F2n
      REAL EF1(3),EF2(3)
      REAL F_pion, F_dipole
      REAL ae1,be1,be2,be3,am1,bm1,bm2,bm3
      REAL ce1,ce2,de1,de2,de3,de4,cm1,dm1,dm2,dm3
      INTEGER i,j,k
c_end_var
c
c  executable code for routine BORN:
c----6----------------------------------------------------------------72
c
c      IF (NWRITE .LT. 1) THEN
c        NWRITE = NWRITE + 1
c        CRMESS='This is a DUMMY routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
c      ENDIF
c   Calculate Born term contribution (nucleon and pion poles)
c   into invariant amplitudes.
	d=1./(s-mn22)
	d1=u-mn22
        tau=Q2/4.D0/mn22
        q1=sqrt(Q2)
c- Pion Formfactor
        F_pion=1.0/(1.+Q2/0.54)
c- Dipole formula        
        F_dipole=1./(1.+Q2/0.71)**2
c-  Parameterizations from NP B (Proc.Suppl.) 159(2006)127
        ae1=-0.0578
        be1=11.1
        be2=13.6
        be3=33.0
        am1=0.15
        bm1=11.1
        bm2=19.6
        bm3=7.54
        ce1=1.25
        ce2=1.3
        de1=-9.86
        de2=305.
        de3=-758.
        de4=802.
        cm1=1.81
        dm1=14.1
        dm2=20.7
        dm3=68.7
        Gep=(1.+ae1*tau)/(1.+be1*tau+be2*tau**2+be3*tau**3)
        Gmp=2.793*(1.+am1*tau)/(1.+bm1*tau+bm2*tau**2+bm3*tau**3)
        Gen=ce1*tau+ce2*tau**2
        Gen=Gen/(1.+de1*tau+de2*tau**2+de3*tau**3+de4*tau**4)
        Gen=Gen/(1.+Q2)
        Gmn=-1.913*(1.+cm1*tau)/(1.+dm1*tau+dm2*tau**2+dm3*tau**3)
c- Pauli Formfactors        
 	F1p=(Gep+tau*Gmp)/(1.+tau)
        F2p=(Gmp-Gep)/(1.+tau)/mn2
        F1n=(Gen+tau*Gmn)/(1.+tau)
        F2n=(Gmn-Gen)/(1.+tau)/mn2
c- Isovecotor and Isoscalar Pauli Formfactors (1 is V, 2 is S).
        F_1(1)=F1p-F1n
        F_1(2)=F1p+F1n
        F_2(1)=F2p-F2n
        F_2(2)=F2p+F2n
	do k=1,2
c-  This loop counts for isotopic amplitudes (+) and (-),
c-  which are determined by isovector form factors	
	  EF1(k)=F_1(1)
	  EF2(k)=F_2(1)
	end do
c-  The following are isotopic form factors (0) 
c-  which are equal to isoscalar form factors
	EF1(3)=F_1(2)
	EF2(3)=F_2(2)	
	do k=1,3
c-  This loop counts for isotopic states (+),(-),(0)
	  R(1,k)=EF1(k)+mn2*EF2(k)
	  R(2,k)=-EF1(k)
	  R(3,k)=-EF1(k)/2.
	  R(4,k)=EF2(k)
	  R(5,k)=-qk*EF2(k)
	  R(6,k)=2.*EF2(k)
	end do
	do i=1,6
	  do j=1,3
	    R(i,j)=3.212*R(i,j)*(d+ett(i,j)/d1)/1.025
	  end do
	end do
	R(3,2)=R(3,2)-6.424*f_pion/(t-mp22)/1.025
c- The coefficient 3.212 (microbarn)**(1/2) comes from
c- g*e/(8piGeV), where g**2/4pi=14.5, e**2/4pi=1/137.	
c
CCC 		ge=3.9883
CCC In paper:	B3(-)=f_sub - 3.9883*f_pion/(t-mp22)
c		1/GeV=19.7327(microbarn)**(1/2)
CCC In code:	R(3,2)=B3(-)/4piGeV=B3(-)*1.57(microbarn)**(1/2)
c
CCC In code:	R(3,2)=1.57*f_sub - 1.57*3.9883*f_pion/(t-mp22)+...
c
CCC	BB=(-1.62+0.32*Q2)+t*(-0.49+0.11*Q2)
CCC	write(*,*)t,AA,BB
      RETURN
      END
c
c------------------------------------------------------------------------------





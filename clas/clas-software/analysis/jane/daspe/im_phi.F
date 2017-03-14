      SUBROUTINE IM_PHI (S,costh,Q2,kgamma,qpion)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine IM_PHI
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
c  Author:         Created:  Wed Dec 11 16:01:19 EST 1996
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
#include "multipoles.inc"
#include "imphi.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL S, Q2, kgamma, qpion ,costh
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
      PARAMETER (CRNAME='IM_PHI')
      PARAMETER (CRAUTH='I. Aznauryan')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
      REAL E1(2)
      REAL pb(6,3)
      REAL p1,p2,p3,p4,p5
      INTEGER i, j
      REAL m16(2),e16(2),s16(2),m17(2),e17(2),s17(2),
     *     e12(2),s12(2),m11(2),s11(2),m13(2),e13(2),s13(2)
      REAL e3,m31,e31,s31,m32,e32,s32,e35,s35,m36,e36,
     *     s36,m37,s37
c_end_var
c
c  executable code for routine IM_PHI:
c----6----------------------------------------------------------------72
c
c      IF (NWRITE .LT. 1) THEN
c        NWRITE = NWRITE + 1
c        CRMESS='This is a DUMMY routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
c      ENDIF
c
c- 
c
      CALL breit_ampl(S,Q2,kgamma,qpion)
c
c- Calculate derivatives of Legender polinomials
c- p1=P2', p2=P3', p3=P4', p4=P3", p5=P4"
c	      
      p1=3.*costh
      p2=(15.*costh**2-3.)/2.
      p3=2.5*(7.*costh**3-3.*costh)
      p4=15.*costh
      p5=7.5*(7.*costh**2-1.)	
c
c- Calculate intermediate amplitudes for transition from imaginary parts
c- of multipole amplitudes to imaginary parts of Invariant Amplitudes.
c- Multipole ampl. are normalized according to CGLN Phys.Rev. 106 (1957) 1345.
c- They are equal to mult.ampl. of Devenish&Lyth/8piW
c- Here imaginary parts of mult. ampl. are taken as sum over B-W of resonances.
c- In these imaginary parts of mult.ampl. it is nessesary to take into account 
c- background contributions, which can be found empirically.
c- First do the loop for resonances with isospin 1/2.
c- mult.ampl. (1),(10) => (1-)
c- mult.ampl. (2),(5) => (0+)
c- mult.ampl. (3),(4),(8) => (2-)
c- mult.ampl. (6),(13) => (3-)
c- mult.ampl. (7),(11) => (1+)
c- mult.ampl. (9) => (2+)
c- mult.ampl. (12) => (3+)
c
      do i=1,2
c-construct the amplitudes for combinations A(1/2)/3, A(0)
        M16(i)=EM1(i,6)+EM1(i,13)
        E16(i)=EE1(i,6)+EE1(i,13)
        S16(i)=ES1(i,6)+ES1(i,13) 
        M17(i)=EM1(i,7)+EM1(i,11)
        E17(i)=EE1(i,7)+EE1(i,11)
        S17(i)=ES1(i,7)+ES1(i,11) 
        E12(i)=EE1(i,2)+EE1(i,5)
        S12(i)=ES1(i,2)+ES1(i,5)
        M11(i)=EM1(i,1)+EM1(i,10)
        S11(i)=ES1(i,1)+ES1(i,10)
        M13(i)=EM1(i,3)+EM1(i,4)+EM1(i,8)
        E13(i)=EE1(i,3)+EE1(i,4)+EE1(i,8)
        S13(i)=ES1(i,3)+ES1(i,4)+ES1(i,8)
        E1(i)=M16(i)+E16(i)
c23456789012345678901234567890123456789012345678901234567890123456789012
        pb(1,i)=E12(i)+p1*(M17(i)+E17(i))+p3*(3.*EM1(i,12)+EE1(i,12))+
     +       3.*M13(i)+E13(i)+p1*(4.*M16(i)+E16(i))+
     +       p2*(2.*EM1(i,9)+EE1(i,9))
        pb(2,i)=2.*M17(i)+M11(i)+p1*(2.*M13(i)+3.*EM1(i,9))+
     +       3.*p2*M16(i)+4.*p2*EM1(i,12)
        pb(3,i)=3.*(E17(i)-M17(i)+E1(i))+p5*(EE1(i,12)-EM1(i,12))+
     +       p4*(EE1(i,9)-EM1(i,9))
        pb(4,i)=-3.*(M13(i)+E13(i))+3.*(EM1(i,9)-EE1(i,9))-
     -       p4*E1(i)+p4*(EM1(i,12)-EE1(i,12))
        pb(5,i)=S12(i)+2.*p1*S17(i)+4.*p3*ES1(i,12)+3.*p2*ES1(i,9)-
     -       2.*S13(i)-3.*p1*S16(i)
        pb(6,i)=S11(i)+2.*p1*S13(i)+3.*p2*S16(i)-
     -       2.*S17(i)-4.*p2*ES1(i,12)-3.*p1*ES1(i,9)
      end do
c- Now calculate A(3/2)/1.5 for resonances with isospin 3/2.
c- mult.ampl. (1),(4),(10) => (1+)
c- mult.ampl. (2),(12) => (2-)
c- mult.ampl. (7),(8) => (1-)
c- mult.ampl. (6),(13) => (3-)
c- mult.ampl. (5),(9) => (0+)
c- mult.ampl. (3) => (3+)
c- mult.ampl. (11) => (2+)
c
        M31=EM3(1)+EM3(4)+EM3(10)
        E31=EE3(1)+EE3(4)+EE3(10)
        S31=ES3(1)+ES3(4)+ES3(10)
        M32=EM3(2)+EM3(12)
        E32=EE3(2)+EE3(12)
        S32=ES3(2)+ES3(12)
        E35=EE3(5)+EE3(9)
        S35=ES3(5)+ES3(9)
        M36=EM3(6)+EM3(13)
        E36=EE3(6)+EE3(13)
        S36=ES3(6)+ES3(13) 
        M37=EM3(7)+EM3(8)
        S37=ES3(7)+ES3(8)
        E3=M36+E36
c23456789123456789123456789123456789123456789123456789123456789123456789
        pb(1,3)=E35+p1*(M31+E31)+p3*(3.*EM3(3)+EE3(3))+
     +		3.*M32+E32+p1*(4.*M36+E36)+
     + 		p2*(2.*EM3(11)+EE3(11))
        pb(2,3)=M37+2.*M31+p1*(2.*M32+3.*EM3(11))+
     +		4.*p2*EM3(3)+3.*p2*M36
        pb(3,3)=3*E3+3.*(E31-M31)+p5*(EE3(3)-EM3(3))+
     +		p4*(EE3(11)-EM3(11))
        pb(4,3)=-3.*(M32+E32)+3.*(EM3(11)-EE3(11))-
     -		p4*E3+p4*(EM3(3)-EE3(3))
        pb(5,3)=S35+2.*p1*S31+4.*p3*ES3(3)+3.*p2*ES3(11)-
     -		2.*S32-3.*p1*S36
        pb(6,3)=S37+2.*p1*S32+3.*p2*S36-
     -		2.*S31-4.*p2*ES3(3)-3.*p1*ES3(11)
c
c- Now get the Imaginary parts of six intermediate amplitudes for three 
c- isospin combinations A(+), A(-) and A(0)), defined according to 
c- CGLN Phys.Rev. 106 (1957) 1345 
c
      do j=1,6
         ph(j,1)=pb(j,1)+pb(j,3)
         ph(j,2)=pb(j,1)-pb(j,3)/2.
         ph(j,3)=pb(j,2)
c	
c- Number '2' stands for Imaginary parts of amplitudes
c-for  combinations A(+), A(-) and A(0)
         PHisot(j,1,2)=ph(j,1)
         PHisot(j,2,2)=ph(j,2)
         PHisot(j,3,2)=ph(j,3)
	
      end do
c
      RETURN
      END
c
c------------------------------------------------------------------------------





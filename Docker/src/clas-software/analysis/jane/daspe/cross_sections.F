      SUBROUTINE CROSS_SECTIONS (S,azimut,epsilon,sinth,costh,kgamma,qpion)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine CROSS_SECTIONS
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
c  Author:  H. Aznauryan       Created:  Mon Dec 16 19:31:55 EST 1996
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
#include "helicity.inc"
#include "cross_sec.inc"
#include "fit_par.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
        REAL S,azimut,epsilon,sinth,costh
        REAL SLP(3),kgamma,qpion
        REAL sigmaT(2),sigmaL(2)
        REAL sigmaTT(2),sigmaTL(2)
        REAL mpion(2),mnuc,qpi,qkratio
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
      PARAMETER (CRNAME='CROSS_SECTIONS')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER i,j
c_end_var
c
c  executable code for routine CROSS_SECTIONS:
c----6----------------------------------------------------------------72
c-loops for reactions p+pi0, n+pi+
         mnuc=0.93827
         mpion(1)=0.13498
         mpion(2)=0.13957
        SLP(1)=costh
        SLP(2)=(3.*costh**2-1.)/2.
        SLP(3)=(5.*costh**3-3.*costh)/2.
      DO i=1,2
      qpi=sqrt(((s-mnuc**2+mpion(i)**2)/2.)**2/s-mpion(i)**2)
        qkratio=qpi/((s-mnuc**2)/2./sqrt(s))
        sigmaT(i)=0.
        sigmaL(i)=0.
        sigmaTT(i)=0.
        sigmaTL(i)=0.
	sigmaTLP(i)=0.
       Do j=1,2
          sigmat(i) = sigmat(i) + 
     +                H1(i,j)**2.+ H2(i,j)**2.+ H3(i,j)**2.+ H4(i,j)**2.
          sigmaL(i) = sigmaL(i) + H5(i,j)**2. + H6(i,j)**2.
          sigmaTT(i) = sigmaTT(i)-H4(i,j)*H1(i,j)+H3(i,j)*H2(i,j)
          sigmaTL(i) = sigmaTL(i)+H5(i,j)*(H1(i,j)-H4(i,j))+ 
     +                 H6(i,j) * ( H2(i,j) + H3(i,j) )
        EndDo
      sigmaT(i)=sigmaT(i) * qkratio / 2.
      sigmaL(i)=sigmaL(i) * qkratio
      sigmaTT(i)=sigmaTT(i) * qkratio
      sigmaTL(i)=sigmaTL(i) * qkratio / sqrt(2.)
c
      sigma(i) = sigmaT(i) + epsilon * sigmaL(i) + 
     + epsilon * sigmaTT(i) * cos(2. * azimut) + sqrt( 2. * epsilon *
     *(1. + epsilon)) * sigmaTL(i) * cos( azimut )
          sigmaTLP(i) = sigmaTLP(i)+
     1        H5(i,2)*(H1(i,1)-H4(i,1))- H5(i,1)*(H1(i,2)-H4(i,2))+
     1        H6(i,2)*(H2(i,1)+H3(i,1))- H6(i,1)*(H2(i,2)+H3(i,2))
      sigmaTLP(i)  = sigmaTLP(i)  * qkratio / sqrt(2.)
      sigma_leg(1,i) =(sigmaT(i) + epsilon * sigmaL(i))/2.
      sigma_leg(2,i) =1.5*costh*(sigmaT(i) + epsilon * sigmaL(i))
      sigma_leg(3,i) =2.5*SLP(2)*(sigmaT(i) + epsilon * sigmaL(i))
      sigma_leg(4,i) =sigmaTL(i)/sinth/2.
      sigma_leg(5,i) =1.5*costh*sigmaTL(i)/sinth
      sigma_leg(6,i) =sigmaTT(i)/sinth**2/2.
      sigma_leg(7,i) =sigmaTLP(i)/sinth/2.
      sigma_leg(8,i) =1.5*costh*sigmaTLP(i)/sinth
c        sigma_tt(i) = sigmaTT(i)*epsilon
c        sigma_tpl(i) =sigmaT(i) + epsilon * sigmaL(i)
c        sigma_tl(i) =sigmaTL(i)* sqrt( 2. * epsilon *(1. + epsilon))
c        sigma_tlp(i)=sigmaTLP(i)*sqrt(2.*epsilon*(1.-epsilon))
        sigma_tt(i) = sigmaTT(i)
        sigma_tpl(i) =sigmaT(i) + epsilon * sigmaL(i)
        sigma_tl(i) =sigmaTL(i)
        sigma_tlp(i)=sigmaTLP(i)
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------
 



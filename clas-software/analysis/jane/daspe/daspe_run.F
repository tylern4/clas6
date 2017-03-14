      SUBROUTINE DASPE_RUN(Wi,Q2i,epsilon,costhi,phidi,ierr)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine DASPE_RUN
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
c  -------------
c
c  Notes:
c  ------
c
c  Author:  S.Stepanyan     Created:  Wed Jun 23 17:16:39 EST 1996
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
#include "inpar.inc"
#include "gpar.inc"
#include "imphi.inc"
#include "cross_sec.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL Wi, Q2i, epsilon, costhi, phidi
      INTEGER ierr
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
      PARAMETER (CRNAME='DASPE_RUN')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER i,j
      REAL sinth,theta,im_phi_save(6,3),phi
      REAL kgamma,qpion,k22,q22
      REAL W, costh, phid
      REAL Q2, S
c_end_var
c
c  executable code for routine DASPE_RUN:
c----6----------------------------------------------------------------72
c
      w=wi
      q2=q2i
      costh=costhi
      phid=phidi

      CALL daspe_ini_point(Q2)
c      
      theta=acos(costh)
      theta=theta*180./pi
      sinth=sqrt(1.-costh**2)
      phi=phidi*pi/180.
c
      s=w**2
      w2=2.*w
      E=s-mn22
      Egamma=(E-Q2)/w2
      Epion=(E+mp22)/w2
      q22=Epion**2-mp22
      k22=(w-Egamma)**2-mn22
      qpion=sqrt(q22)
      kgamma=sqrt(k22)
      qk=qpion*kgamma*costh-Epion*Egamma
      t=mp22-Q2+2.*qk
      u=2.*mn22+mp22-Q2-s-t
      CALL IM_PHI (S,costh,Q2,kgamma,qpion)
      CALL born(S,Q2)
      do i=1,6
         do j=1,3
            phisot(i,j,2)=ph(i,j)
            im_phi_save(i,j)=phisot(i,j,2)
         enddo
      enddo
      CALL to_integration(S,Q2)
      CALL RE_PHI(S,Q2,COSTH)
      do i=1,6
         do j=1,3
            phisot(i,j,2)=im_phi_save(i,j)
         enddo
      enddo
c
      CALL hel_ampl(S,Q2,theta,kgamma)
c
      CALL cross_sections(s,phi,epsilon,sinth,costh,kgamma,qpion)
      ierr = 0
      RETURN
      END
c
c------------------------------------------------------------------------------











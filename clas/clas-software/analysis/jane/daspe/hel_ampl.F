      SUBROUTINE HEL_AMPL(S,Q2,THETA,kgamma,opt)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine HEL_AMPL
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
c  Author:         Created:  Mon Dec 16 17:23:47 EST 1996
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
#include "inpar.inc"
#include "imphi.inc"
#include "helicity.inc"
#include "add_ampl.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL S,Q2,theta,kgamma
      CHARACTER*10 opt
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
      PARAMETER (CRNAME='HEL_AMPL')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER i,j
      REAL teta,qq,sx,sx2,cx
      REAL PHreac(6,3,2)
c_end_var
c
c  executable code for routine HEL_AMPL:
c----6----------------------------------------------------------------72
      teta=theta*pi/180.
      qq=sqrt(Q2)/kgamma
      sx=sin(teta)
      cx=cos(teta)
      sx2=sx**2
c
c- Calculates Intermediate amplitudes for different reactions. 
c
c- Loop by "i" counts for amplitudes. Loop by "j" stands for 
c- imaginary(j=2) and real(j=1) parts.
      do i=1,6
         do j=1,2
c  - Reaction g + p => p + pi0
            PHreac(i,1,j)=PHisot(i,1,j)+PHisot(i,3,j)
c  - Reaction g + p => n + pi+
            PHreac(i,2,j)=a2*(PHisot(i,2,j)+PHisot(i,3,j)) 
         end do
      end do	
c
c- Now get the helicity amplitudes.
c- Here as before "j" stands for real and imaginary parts.
c- Loop for "i" done for three reactions:
c- i=1 - g + p => p + pi0
c- i=2 - g + p => n + pi+
c- i=3 - g + n => p + pi-
c- h1,h2,...h6 are helicity amplitudes f_m2_m1_l,   
c- where m1 and m2 are helicities of initial and final nucleons, l is 
c- helicity of incomming photon. 
      do i=1,2
               do j=1,2
c - h1 = f++- = f--+
            h1(i,j)=-sx*(PHreac(3,i,j)+cx*PHreac(4,i,j))/a2
c - h2 = f-++ = -f+--
        h2(i,j)=2.*PHreac(1,i,j)-2.*cx*PHreac(2,i,j)+sx2*PHreac(4,i,j)
        h2(i,j)=-h2(i,j)/a2   
c- h3 = f+-+ = f-+-
            h3(i,j)=-sx2*PHreac(4,i,j)/a2
c- h4 = f+++ = f---
        h4(i,j)=2.*PHreac(2,i,j)+PHreac(3,i,j)+cx*PHreac(4,i,j)
        h4(i,j)=sx*h4(i,j)/a2
c- h5 = f--0 = -f++0
            h5(i,j)=qq*(PHreac(5,i,j)+cx*PHreac(6,i,j))
c- h6 = f+-0 = f-+0
            h6(i,j)=qq*sx*PHreac(6,i,j)
         end do
      end do
     
      RETURN
      END
c
c------------------------------------------------------------------------------





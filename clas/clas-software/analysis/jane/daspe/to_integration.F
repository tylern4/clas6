      SUBROUTINE TO_INTEGRATION(S,Q2)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine TO_INTEGRATION
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
c  Author:         Created:  Mon Dec 16 11:22:03 EST 1996
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
#include "identify.inc"
#include "gpar.inc"
#include "bornterms.inc"
#include "integr.inc"
#include "int_func.inc"
#include "daspetab.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL S, Q2
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
      PARAMETER (CRNAME='TO_INTEGRATION')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
      INTEGER i,j

      REAL low, high, eps

c_end_var
c
c  executable code for routine TO_INTEGRATION:
c----6----------------------------------------------------------------72
c
c      IF (NWRITE .LT. 1) THEN
c        NWRITE = NWRITE + 1
c        CRMESS='This is a DUMMY routine, this message written once'
c        CALL RECMES(CRNAME,'I',CRMESS)
c      ENDIF
c
c- Determination of Real parts of Invariant Amplitudes as a sum of 
c- Born term and integrals over Imaginary parts of Ivariant Amplitudes.
c
c      low=1.158D0
      low=(mn+mp)**2+0.0001
      high=5.5D0
      eps=intaccur
c
      Sgen=S
      Q2gen=Q2
      
c-----------   parameters needed in mgauss, mcauch   --------------------
      jm=6
      km=3
c-----------   end of parameters needed in mgauss, mcauch   -------------

      CALL mcauch(low,high,Sgen,eps)
      DO j=1,jm
        do i=1,km
c       	  br(j,i) = h(j,i)/pi
       	  br(j,i) = R(j,i) + h(j,i)/pi
c      	  br(j,i) = R(j,i) 
        enddo
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------



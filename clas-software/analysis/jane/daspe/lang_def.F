      SUBROUTINE LANG_DEF
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine LANG_DEF
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
c  Author:         Created:  Wed Dec 11 17:16:27 EST 1996
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
#include "resonances.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
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
      PARAMETER (CRNAME='LANG_DEF')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
c_end_var
c
c  executable code for routine DASPE_INI_POINT:
c----6----------------------------------------------------------------72
c
c- Langul(i) is the degree of photon momenta in Breit-Wigner formula for 
c- M(l+),M(l-): Langul(i)=l.
      Langul(1)=1
      Langul(2)=2
      Langul(3)=3
      Langul(4)=1  
      Langul(5)=0
      Langul(6)=3
      Langul(7)=1
      Langul(8)=1    
      Langul(9)=0
      Langul(10)=1
      Langul(11)=2
      Langul(12)=2
      Langul(13)=3
C 
      Langul(21)=1
      Langul(22)=0
      Langul(23)=2  
      Langul(24)=2
      Langul(25)=0
      Langul(26)=3    
      Langul(27)=1
      Langul(28)=2
      Langul(29)=2
      Langul(30)=1
      Langul(31)=1
      Langul(32)=3  
      Langul(33)=3
C	
c  Lprime(i) is the degree of photon momenta in Breit-Wigner formula 
c  for E(l+), E(l-):
c  Lprime(i)=l for E(l+), 
c  Lprime(i)=l-2 for E(l-) at l>1,
c  Lprime(i)=l for E(l-) at l=1.    	
      Lprime(1)=1    	
      Lprime(2)=0
      Lprime(3)=3
      Lprime(4)=1
      Lprime(5)=0
      Lprime(6)=1
      Lprime(7)=1
      Lprime(8)=1
      Lprime(9)=0
      Lprime(10)=1
      Lprime(11)=2
      Lprime(12)=0    	
      Lprime(13)=1
C	
      Lprime(21)=1
      Lprime(22)=0
      Lprime(23)=0
      Lprime(24)=0
      Lprime(25)=0
      Lprime(26)=1
      Lprime(27)=1
      Lprime(28)=0
      Lprime(29)=2
      Lprime(30)=1
      Lprime(31)=1
      Lprime(32)=3    	
      Lprime(33)=1
      RETURN
      END
c
c------------------------------------------------------------------------------

      REAL FUNCTION GGAUSS(x_mean,sigma)
c
c_begin_doc
c  RCS ID string
c  $Id: ggauss.F,v 1.2 1997/04/24 15:38:35 holtrop Exp $
c
c  Documentation for function GGAUSS
c
c  Purpose:   Returnes the random number, distributed by GAUSS
c             with x_mean and sigma. Uses only grndm random generator.
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c                      x_mean   -   mean value R*4
c                      sigma    -   dispersion R*4
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c                      random number    R*4
c  -----------------
c
c  Other routines:  grndm
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov      Created:  Tue Oct  8 17:40:48 EDT 1996
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
#include "gconst.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      real*4 x_mean, sigma
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ggauss.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:35 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/   
     1'$Id: ggauss.F,v 1.2 1997/04/24 15:38:35 holtrop Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='GGAUSS')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      real grn(2),z
c_end_var
c
c  executable code for routine GGAUSS:
c----6----------------------------------------------------------------72
c
      call grndm(grn,2)
      z = sin(TWOPI*grn(1))*sqrt(-2.*alog(grn(2)))
      GGAUSS = x_mean + sigma*z
c
      RETURN
      END
c
c---------------------------------------------------------------------





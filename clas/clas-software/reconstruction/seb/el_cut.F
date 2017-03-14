      LOGICAL FUNCTION EL_CUT (p,Ewhole,Einout,M2u,M2v,M2w,M3u,M3v,M3w)
c
c_begin_doc
c  RCS ID string
c  $Id: el_cut.F,v 1.6 2001/02/26 17:03:44 stepanya Exp $
c
c  Documentation for subroutine EL_CUT
c
c  Purpose: Identification of electrons in the EC
c  --------
c
c  Input Parameters:  p      - real - momentum from treking
c  ----------------   Ewhole - real - total energi in the EC
c                     Einout - real - difference Inner and Outer energies
c                     M2u    - real - distribution second momentum on U side
c                     M2v    - real - distribution second momentum on V side
c                     M2w    - real - distribution second momentum on W side
c                     M3u    - real - distribution third momentum on U side
c                     M3v    - real - distribution third momentum on V side
c                     M3w    - real - distribution third momentum on W side
c
c  Output Parameters:  Function returnes .true. if electron was identified.
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S. Stepanyan     Created:  Thu Dec  5 09:58:37 EST 1996
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
#include "evnt_par.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      REAL p,Ewhole,Einout,M2u,M2v,M2w,M3u,M3v,M3w
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: el_cut.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.6 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/02/26 17:03:44 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: el_cut.F,v 1.6 2001/02/26 17:03:44 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EL_CUT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL E_whole
      REAL sigma_whole
      REAL E_in_out
      REAL sigma_in_out
      REAL M3, M2
      REAL sigma_tot, sigma_io
c_end_var
c
c  executable code for routine EL_CUT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine dos electron identification in EC.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
*
      if ( p.lt.1. ) then
        E_whole= p*( 0.23 + 0.071*p - 0.032*p**2 )
      else
        E_whole= p*sampl_frac
      endif
      sigma_whole = sqrt(p)*( 0.08 + 0.0013*p )
      sigma_tot = sqrt ((sigma_etrk*p)**2+sigma_whole**2)
*
      E_in_out= 0.03 + 0.105*p - 0.025*p**2
      sigma_in_out = p*( 0.0634 - 0.00022*p )
      sigma_io = sqrt ((sigma_etrk*p)**2+sigma_in_out**2)
*
      M2 = ( m2u + m2v + m2w )
      M3 = sqrt( m3u**2 + m3v**2 + m3w**2 )
*
      el_cut=ABS(Ewhole-E_whole).LE.etot_ecel_cut*sigma_tot
     &.AND.ABS(Einout-E_in_out).LE.eio_ecel_cut*sigma_io
c      el_cut=M2.LE.m2_ecel_cut.AND.M3.LE.m3_ecel_cut
c     &.AND.ABS(Ewhole-E_whole).LE.etot_ecel_cut*sigma_tot
c     &.AND.ABS(Einout-E_in_out).LE.eio_ecel_cut*sigma_io
*
      RETURN
      END
c
c------------------------------------------------------------------------------
c-----6----1---------1--------31--------41--------51--------61--------71-




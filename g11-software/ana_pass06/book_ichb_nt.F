      SUBROUTINE BOOK_ICHB_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_ichb_nt.F,v 1.2 2003/12/08 22:32:56 stepanya Exp $
c
c  Documentation for subroutine BOOK_ICHB_NT
c
c  Purpose: Booking Ntuple for inner calorimeter reconstructed particles.
c  --------
c
c  Input Parameters:  Non
c  ----------------
c
c  Output Parameters: Non
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S,. Stepanyan    Created:  Fri Mar  7 09:43:38 EST 1997
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
#include "seb_ntn.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_ichb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2003/12/08 22:32:56 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: book_ichb_nt.F,v 1.2 2003/12/08 22:32:56 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_ICHB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER NTID,btype/0/
c_end_var
c
c  executable code for routine BOOK_PART_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for reconstructed particles.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      call hbname(NTID,'IChb',ic_part,
     +'ic_part[0,20]:I,et(ic_part):R,egl(ic_part):R,time(ic_part):R,'//
     +'tgl(ic_part):R,ich_x(ic_part):R,ich_y(ic_part):R,ich_z(ic_part):R,'//
     +'ich_xgl(ic_part):R,ich_ygl(ic_part):R,ich_zgl(ic_part):R,'//
     +'ic_m2(ic_part):R,ic_m3(ic_part):R,ic_m4(ic_part):R,'//
     +'ic_stat(ic_part):I') 
      RETURN
      END
c
c------------------------------------------------------------------------------





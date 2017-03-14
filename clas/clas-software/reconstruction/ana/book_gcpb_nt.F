      SUBROUTINE BOOK_GCPB_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_gcpb_nt.F,v 1.3 2006/07/18 03:57:43 bonus Exp $
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
c  22.11.2005. Changed by V. Tvaskis for adding new variables from BoNuS  (VS2)
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
#include "gcpb_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_gcpb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/07/18 03:57:43 $')
      PARAMETER (CAUTHO = '$Author: bonus $')
      DATA CRCSID/   
     1'$Id: book_gcpb_nt.F,v 1.3 2006/07/18 03:57:43 bonus Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_GCHB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER NTID,btype/0/
c_end_var
c
c  executable code for routine BOOK_GCPB_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for reconstructed particles.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      call hbname(NTID,'GCpb',Gcpart,
     +'gcpart[0,20]:I,pid(gcpart):I,x(gcpart):R,y(gcpart):R,'//
     +'z(gcpart):R,dedx(gcpart):R,px(gcpart):R,py(gcpart):R,pz(gcpart):R,'//
     +'p_tot(gcpart):R,x2(gcpart):R,theta(gcpart):R,charge(gcpart):R,'//
     +'dca(gcpart):R,index(gcpart):I,phi(gcpart):R,vtl(gcpart):R,sdist(gcpart):R,'//
     +'edist(gcpart):R,npts(gcpart):I,r_0(gcpart):R,'//
     +'fiterr(gcpart):I,tothits(gcpart):I,npd_track(gcpart):I,npd_event(gcpart):I,'//
     +'bonus_bits(gcpart):I,q_tot(gcpart):R')
      RETURN
      END
c
c------------------------------------------------------------------------------





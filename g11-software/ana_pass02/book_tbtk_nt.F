      SUBROUTINE BOOK_TBTK_NT(NTID)
c
cc
c  Documentation for subroutine BOOK_TBTK_NT
c
c  ----------------
c
c  Author  :   G. Gavalian      
c  Created :   Mon MAR 9, 1998
c  -------
c
c 
c     
c
      IMPLICIT NONE
      SAVE
c
c
c 
c

 
#include "tbtk_inf.inc"


c
c
c
c 
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: book_tbtk_nt.F,v $')
      PARAMETER (CREVIS = '$ Revision: 1.1 $')
      PARAMETER (CSTATE = '$ State: Exp $')
      PARAMETER (CDATE = '$ Date: 1998/03/09 $')
      PARAMETER (CAUTHO = '$ Author: gavalian $')
      DATA CRCSID/   
     1'$Id: book_tbtk_nt.F,v 1.4 2001/11/01 02:52:11 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_TBTK_NT')
      PARAMETER (CRAUTH='Gagik Gavalian')
c
c  
c 

      INTEGER NWRITE,NTID
c
c
c  
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking TBTK Ntuple...'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

       call hbname(NTID,'tbtk',n_trk,
     +'n_trk[0,20]:I,sect_trk(n_trk):I,Wire(34,n_trk):I,'//
     +'tx(34,n_trk):R,ty(34,n_trk):R,'//
     +'tz(34,n_trk):R,tcx(34,n_trk):R,tcy(34,n_trk):R,'//
     +'tcz(34,n_trk):R,tlen(34,n_trk):R,'//
     +'dtime(34,n_trk):R,alpha(34,n_trk):R,'//
     +'Wlen(34,n_trk):R,fdoca(34,n_trk):R,cdoca(34,n_trk):R')
      RETURN
      END
c
c------------------------------------------------------------------------------





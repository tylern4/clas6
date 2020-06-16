      SUBROUTINE EVTDMP(ISW,ncol)
c
c_begin_doc
c  RCS ID string
c  $Id: evtdmp.F,v 1.2 2000/11/10 21:39:53 boiarino Exp $
c
c  Documentation for subroutine EVTDMP
c
c  Purpose: Dumps out a listing of all the banks within the BOS memory
c  --------
c
c  Input Parameters:  ISW : I : isw=1 IW array isw=2 JW array
c  ----------------  ncol : i : number of columns on the screen
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:  bnktst, bnkfmt.inc
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Aug  1 15:40:28 EDT 1996
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
#include "bnkfmt.inc"
#include "bcs.inc"
#include "wcs.inc"
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
      PARAMETER (CFILE = '$RCSfile: evtdmp.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/11/10 21:39:53 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: evtdmp.F,v 1.2 2000/11/10 21:39:53 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EVTDMP')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MAMIND
      EXTERNAL MAMIND

      CHARACTER*132 CFMT
      CHARACTER*8 CELEMENTS(MAX_ELEMENTS)
      character*4 ctot
      INTEGER NB, NTOT, NAMI, jndex, ncol
      integer nsec_bank, nc_bank, nr_bank, nc_ddl, Isw
      LOGICAL LFIND
c_end_var
c
c  executable code for routine EVTDMP:
c----6----------------------------------------------------------------72
c
      call recmes(crname,'i','************Event dump********')
      NTOT = 0
      DO 100 NB = 1,ICREATE
         CALL BNKTST(CREATE_NAME(NB),LFIND,NC_DDL,CFMT,CELEMENTS)
         IF (.NOT. LFIND) GOTO 100
         if (isw .ne. 2) then
           nami = mamind(iw,create_name(nb))
          jndex = iw(nami)
         else
           nami = mamind(jw,create_name(nb))
          jndex = jw(nami)
         endif
 20      if (jndex .eq. 0) goto 100
           call bnkdmp(create_name(nb), isw, ncol)
 100     CONTINUE
      call recmes(crname,'i','******Finished with Event Dump*****')
      RETURN
      END
c
c------------------------------------------------------------------------------





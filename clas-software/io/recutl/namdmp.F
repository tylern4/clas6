      SUBROUTINE NAMDMP(ISW)
c
c_begin_doc
c  RCS ID string
c  $Id: namdmp.F,v 1.4 2000/11/10 21:43:59 boiarino Exp $
c
c  Documentation for subroutine NAMDMP
c
c  Purpose: Dumps out a listing of all the banks within the BOS memory
c  --------
c
c  Input Parameters:  ISW : I : I=1 IW array, 2 = JW ARRAy
c  ----------------
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
      PARAMETER (CFILE = '$RCSfile: namdmp.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/11/10 21:43:59 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: namdmp.F,v 1.4 2000/11/10 21:43:59 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='NAMDMP')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MAMIND
      EXTERNAL MAMIND

      CHARACTER*132 CFMT
      CHARACTER*8 CELEMENTS(MAX_ELEMENTS)
      character*4 ctot
      INTEGER NB, NTOT, NAMI, jndex
      integer nsec_bank, nc_bank, nr_bank, nc_ddl, ISW
      LOGICAL LFIND
c_end_var
c
c  executable code for routine NAMDMP:
c----6----------------------------------------------------------------72
c
      call recmes(crname,'L',
     1               '*******BOS BANKS currently in memory*****')
      call recmes(crname,'L','BANK  SECTOR  NCOL  NROW NCOL_DDL')
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
           ntot = ntot + 1
           ctot = create_name(nb)
           if (isw .ne. 2) then
            nc_bank = iw(jndex - 5)
            nr_bank = iw(jndex - 4)
            nsec_bank = iw(jndex - 2)
            jndex = iw(jndex -1)
           else
            nc_bank = jw(jndex - 5)
            nr_bank = jw(jndex - 4)
            nsec_bank = jw(jndex - 2)
            jndex = jw(jndex -1)
           endif
           write(crmess,999)ctot, nsec_bank, nc_bank, nr_bank, nc_ddl
 999       format(a4,2x,i2,6x,i2,4x,i4,4x,i2)
           if (nc_ddl .eq. nc_bank) then 
            call recmes(crname,'L',crmess)
           else
            call recmes(crname,'W',crmess)
           endif
           if (jndex .ne. 0) goto 20
 100     CONTINUE
      call recmes(crname,'L','BANK  SECTOR  NCOL  NROW NCOL_DDL')
      RETURN
      END
c
c------------------------------------------------------------------------------





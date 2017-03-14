      SUBROUTINE HBOOK_INIT(histfile,lhst,hmem)
c
c_begin_doc
c  RCS ID string
c  $Id: hbook_init.F,v 1.1 2001/08/30 16:51:45 feuerbac Exp $
c
c  Documentation for subroutine HBOOK_INIT
c
c  Purpose:  initialize HBOOK and shared hmem
c  --------
c
c  Input Parameters:  histfile: c*256 : name of output file blank if none
c  --------------     lhst    : I     : logical unit for output file 0 if none
c                     hmem  : c*4   : name of associated shared hmem
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Sep 12 08:59:43 EDT 1996
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
      PARAMETER (CFILE = '$RCSfile: hbook_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/08/30 16:51:45 $')
      PARAMETER (CAUTHO = '$Author: feuerbac $')
      DATA CRCSID/   
     1'$Id: hbook_init.F,v 1.1 2001/08/30 16:51:45 feuerbac Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*200  CRMESS
      PARAMETER (CRNAME='HBOOK_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer lenocc
      external lenocc

      CHARACTER*(*) histfile
      character*4   hmem
      INTEGER  lhst

c
c  PAW variables
c
      INTEGER HBOOK_MAX
      PARAMETER( HBOOK_MAX = 900000 )
      REAL H
      COMMON /PAWC/H(HBOOK_MAX)
      INTEGER IQUEST, iret
      COMMON/QUEST/IQUEST(100)
c_end_var
c
c  executable code for routine HBOOK_INIT:
c----6----------------------------------------------------------------72
c
c  call HLIMIT to set the limits for the PAWC/HBOOK
c
c

      if (hmem(1:1) .ne. ' ') then
       write(crmess,*)'initializing shared memory =',hmem
       call recmes(crname,'i',crmess)
       CALL HLIMAP(HBOOK_MAX,hmem)
       write(crmess,15)hmem
 15    format('PAW shared memory enable with global section:',a)
       call recmes(crname,'i',crmess)
      else
       CALL HLIMIT(HBOOK_MAX)
      endif
c
c  OK! now open the file.
c
c
c  Now write out a message, giving the name of the RZN file
c
      WRITE(CRMESS,30)histfile
30    FORMAT('Opening histogram file name =',A)
      CALL RECMES(CRNAME,'i',CRMESS)
      iquest(10) = 65000
      call hropen(lhst,'LUN1',histfile(1:lenocc(histfile)),'NQ',1024,IRET)
      RETURN
      END
c
c------------------------------------------------------------------------------





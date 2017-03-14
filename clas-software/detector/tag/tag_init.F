      SUBROUTINE TAG_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: tag_init.F,v 1.11 1999/05/27 18:06:42 anciant Exp $
c
c  Documentation for subroutine TAG_INIT
c
c  Purpose:  TAGGER routine called ONCE at initialization time
c  --------  
c
c  Input Parameters: NONE
c  ----------------
c
c  Output Parameters:
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c
c
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EDT 1997
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
c#include "bcs.inc"
#include "tagcal.CMN"
#include "tag_general.PAR"
#include "tagtcl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
       logical OK
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: tag_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.11 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/05/27 18:06:42 $')
      PARAMETER ( CAUTHO = '$Author: anciant $')
      DATA CRCSID/
     1'$Id: tag_init.F,v 1.11 1999/05/27 18:06:42 anciant Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TAG_INIT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  function type :
c  --------------

        integer tag_bin

c
c  Local Tag defined variables:
c  -----------------------------
        INTEGER NWRITE
        integer dummy_int,i
        character*100 comments
        character*80 fname
        integer Ilun,Iretb

c_end_var
c
c  executable code for routine TAG_INIT:
c  -------------------------------------
c      IF (NWRITE .LT. 1) THEN
c        NWRITE = NWRITE + 1
c        CRMESS='Tagger init routine'
c        CALL RECMES(CRNAME,'I',CRMESS)
c      ENDIF

c  Register my TAG package with job database....
      CALL REVINI('TAG ',CREVIS,CDATE)

c
c   ---------  OK
c
         ok = .true.
         
c
c  ------------------------ raw 2 BOS parameters
c 	

5	format(I2,a100)

30 	if (Tagger_Raw_Format.ne.0) then
c
c  ---- try to open the local file first
c

 	open(Ilun,status='old',err=300,file='tagraw2bos.par')
        goto 302
     
300     CRMESS='No local, raw to BOS parameter file: Looking in CLAS_PARMS...'
        CALL RECMES(CRNAME,'I',CRMESS)
c-------------- If there is no local file go and pick the one in CLAS_PARMS
        call revinm('CLAS_PARMS','tagraw2bos.par',fname)
	open(Ilun,status='old',err=301,file=fname)
        goto 302

301     CRMESS='No Tagger raw to BOS parameter file tagraw2bos.par in CLAS_PARMS: empty parameters '
        CALL RECMES(CRNAME,'W',CRMESS)
        ok = .false.
        return
	
302     CRMESS='Reading Tagger raw to BOS parameter file tagraw2bos.par '
        CALL RECMES(CRNAME,'I',CRMESS)
        read(Ilun,5) DSD_ADC_SLOT,comments	
	read(Ilun,5) DSD_TDC_SLOT,comments	
	read(Ilun,5) TC_LEFT_SLOT,comments	
	read(Ilun,5) TC_RIGHT_SLOT,comments	
 	close(Ilun)
        endif
        
C Free logical unit number:

        Call ResLun(crname,-Ilun,Iretb)
        
        RETURN
      END
c
c------------------------------------------------------------------------------



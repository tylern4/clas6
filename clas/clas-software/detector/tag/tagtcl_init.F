      SUBROUTINE TAGTCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: tagtcl_init.F,v 1.18 2006/03/14 05:34:38 pasyuk Exp $
c
c  Documentation for subroutine TAGTCL_INIT
c
c  Purpose:  TAGGER routine for initializing tcl runtime variables
c  --------
c
c  Input Parameters:  none
c  ----------------
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
c  Author:   Francois Roudot      Created:  Thu Jun 19 12:00:00 EST 1997
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
c
#include "tag_general.PAR"
#include "tagtcl.CMN"
c
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: tagtcl_init.F,v 1.18 2006/03/14 05:34:38 pasyuk Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAGTCL_INIT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      INTEGER NWRITE
      INTEGER ITCL_INT, ITCL_STR, ITCL_INT_READ, ITCL_DOUBLE
      INTEGER lstring
      DATA ITCL_INT, ITCL_STR, ITCL_INT_READ, ITCL_DOUBLE /1,4,129,2/
      DATA lstring/128/
c_end_var
c
c  executable code for routine TAGTCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Tagger  tcl init routine'
        CALL RECMES(CRNAME,'I',CRMESS)
      endif

      call tagtcl_set_def
c
c     defines TCL variable for E& T coinc
c     
      call tcl_linkvar('Tagger_NO_LR',Tagger_NO_LR,itcl_int)
c
c  tcl variable for conversion from raw BOS to regular BOS, default is no conversion
c
      call tcl_linkvar('Tagger_Raw_Format',Tagger_Raw_Format,itcl_int)
c
c  tcl variable for processing without E, default is processing WITH E (0)
c
      call tcl_linkvar('Tagger_NO_E',Tagger_NO_E,itcl_int)
c
c  tcl variable for screen dumping Tagger related event.CMN commons
c defaut is begin = 0, end = -1 => no dumping (there is dumping only is end>=begin
c
      call tcl_linkvar('Tagger_debug_begin',Tagger_debug_begin,itcl_int)
c
      call tcl_linkvar('Tagger_debug_end',Tagger_debug_end,itcl_int)

c
c  tcl variable for dumping DSD related event.CMN commons, default is NO
c
      call tcl_linkvar('Tagger_DSD_debug',Tagger_DSD_debug,itcl_int)

c
c  tcl variable to define the input of parameters and calibration constants.
c
      call tcl_linkvar('Tagger_parms_input',Tagger_parms_input,itcl_int)

c
c  tcl variable to define if a TDC hit is needed in order to analyse DSD events.
c
      call tcl_linkvar('Tagger_PC_TDC',Tagger_PC_TDC,itcl_int)
      call tcl_linkvar('Tagger_PS_TDC',Tagger_PS_TDC,itcl_int)
      call tcl_linkvar('Tagger_TAC_TDC',Tagger_TAC_TDC,itcl_int)
      call tcl_linkvar('Tagger_ETmatch',Tagger_ETmatch,itcl_int)
      call tcl_linkvar('Tagger_convert',Tagger_convert,itcl_int)
      call tcl_linkvar('Tagger_correct',Tagger_correct,itcl_int)
      call tcl_linkvar('Tagger_energy_correct',Tagger_energy_correct,itcl_int)

c
c Filename for corretcdion of T-counters
c
      call tcl_linkchar('tpeak_name',Tagger_tpeaks_file_name,
     +     itcl_str,lstring)

c
c Filename for energy corretcdion
c
      call tcl_linkchar('tagger_ecorr_name',Tagger_ecorr_name,
     +     itcl_str,lstring)

c
c  tcl variable to define how often a given warning message has to be written out
c
      call tcl_linkvar('Tagger_warning',Tagger_warning,itcl_int)
c
c  tcl variables for processing parameters
c
      call tcl_linkvar('Tagger_TTwin',ADJ_T_COINC,itcl_double)
      call tcl_linkvar('Tagger_EEwin',ADJ_E_COINC,itcl_double)
      call tcl_linkvar('Tagger_ETwin',ET_window,itcl_double)
      call tcl_linkvar('Tagger_ETDCmin',E_TDC_MIN,itcl_double)
      call tcl_linkvar('Tagger_ETDCmax',E_TDC_MAX,itcl_double)
      call tcl_linkvar('Tagger_DSDwin',TAG_DSD_window,itcl_double)
      call tcl_linkvar('Tagger_PSOSwin',PS_COINC_WINDOW,itcl_double)
      call tcl_linkvar('Tagger_TLRwin',TLR_WINDOW,itcl_double)

      RETURN
      END
c
c------------------------------------------------------------------------------





      SUBROUTINE TAGTCL_SET_DEF
c
c_begin_doc
c  RCS ID string
c  $Id: tagtcl_set_def.F,v 1.15 2008/09/10 19:27:09 pasyuk Exp $
c
c  Documentation for subroutine TAGTCL_SET_DEF
c
c  Purpose:  TAGGER routine to set default values for tcl runtime variables
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
     1'$Id: tagtcl_set_def.F,v 1.15 2008/09/10 19:27:09 pasyuk Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAGTCL_SET_DEF')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      INTEGER NWRITE

c_end_var
c
c  executable code for routine TAGTCL_SET_DEF:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS=' '
c        CALL RECMES(CRNAME,'I',CRMESS)
      endif
c
c     old tcl variable that was suposed to tell if the E-T coincidence
c     module was on, it is not used in the code. To be cleaned some day.

       Tagger_NO_LR = 0

c
c  tcl variable for conversion from raw BOS to regular BOS, default is no conversion
c
      Tagger_Raw_Format = 0
c
c  tcl variable for processing without E, default is processing WITH E (0)
c
      Tagger_NO_E = 0
c
c  tcl variable for screen dumping event.CMN commons
c defaut is begin = 0, end = -1 => no dumping (there is dumping only is end>=begin
c
      Tagger_debug_begin = 0
c
      Tagger_debug_end = -1
c
c  tcl variable for dumping DSD related event.CMN commons, default is NO
c
      Tagger_DSD_debug = 0

c
c  tcl variable for first input for calibration constants, default is first input should be the Map Manager
c
      Tagger_parms_input = 0

c
c     tcl variable for ET geometry match. 
c     1: Do geometry ET matching. 
c     0: do not do geometry ET matching (all combinations are allowed)
c     Default is 1 (DO)
c

      Tagger_ETmatch = 1

c tcl variable to control T counter conversion scheme
c 0: (default) old scheme time=TDC*slope-peak
c 1: (used in calibration) new scheme time=(TDC-peak)*slope

      Tagger_convert=0

c tcl variable to control time shift correction (g8)
c 0: (default) disable correction
c 1: enable correction. Actual flag fro correction is in the database

      Tagger_correct = 0


c tcl variable to control energy correction (obtained by g10)
c 0: (default) disable correction
c 1: enable correction. 

      Tagger_energy_correct = 1

c default file name for time shifts corrections
c
      Tagger_tpeaks_file_name = 'tag_tshifts.dat'

c default file name for time shifts corrections
c
      Tagger_ecorr_name = 'tag_ecorr.dat'

c
c  tcl variable to define if a TDC hit is needed in order to analyse DSD events.
c  default is yes, we need a hit in the TDC, value is 1
c
      Tagger_PC_TDC = 1
      Tagger_PS_TDC = 1
      Tagger_TAC_TDC = 1

c
c  tcl variable to define how often a given warning message has to be written out
c  default value is 10000, every 10000 warning a message
c
      Tagger_warning = 10000

c
c  tcl variables for processing parameters
c
      ADJ_T_COINC = 10.
      ADJ_E_COINC = 20.
      ET_window = 20.
      E_TDC_MIN = 700
      E_TDC_MAX = 1400
      TAG_DSD_window = 15
      PS_COINC_WINDOW = 30
      TLR_window = 10.


      RETURN
      END
c
c------------------------------------------------------------------------------





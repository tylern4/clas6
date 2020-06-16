	subroutine tagcal_reset(ok)
c
c_begin_doc
c  RCS ID string
c  $Id: tagcal_reset.F,v 1.3 1999/05/27 18:06:43 anciant Exp $
c
c  Documentation for subroutine tagcal_read_file
c
c  Purpose: 
c  --------
c	Resets flags that tells where the calibrations constants have been picked up 
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Eric Anciant, march 1998
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
#include "tagcal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok
c      
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tagcal_reset.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.3 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1999/05/27 18:06:43 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tagcal_read_file')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
c_end_var
c
c  executable code for routine tagcal_read_file:
c  ------------------------------------
c
c      
      crmess = '  '
      
      ok= .true.
     
c
c  ----------------------- T translation table
c

        TagTTranslateMM = 0
        TAGTTranslOK = 0

c
c  the flag TAGTTranslOK as been added so that if for any reason we are not able to read
c  not the translation table in the map manager nor in the local file, we can react.


c
c  ------------------------ E & T TDC constants:
c 	

        TagTDCCalTMM = 0
	
        TagTDCCalEMM = 0	
	
c
c  ------------------------ Ci constants:
c
	
        TagCivalMM = 0

c
c  ------------------------ T peaks positions constants:
c 	

        tagTDCbaseTPeakMM = 0

c
c  ------------------------ E peaks position constants:
c 	
        tagTDCbaseEPeakMM = 0

c
c  ------------------------ DSD calib. constants:
c 	

        TagDSDCalMM = 0
	
 	
c
c ----------------- Read E & T energy boundaries.
c


        tagEboundsMM = 0
	
        tagTboundsMM = 0

c
c------------------ E - T Map
c
        ectcmapsMM = 0

        RETURN
        END
c
c------------------------------------------------------------------------------

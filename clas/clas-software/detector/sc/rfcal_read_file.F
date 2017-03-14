	subroutine RFCAL_READ_FILE(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: rfcal_read_file.F,v 1.1 1998/04/17 21:03:16 anciant Exp $
c
c  Documentation for subroutine RFcal_read_file
c
c  Purpose: 
c  --------
c	Attempts to read calibration data for RF in the local directory.
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
c  Called from: rf_brun
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Eric ANCIANT      Created:  april, 8th 1998
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
#include "bcs.inc"
#include "rfcal.CMN"
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
      PARAMETER (CFILE=  '$RCSfile: rfcal_read_file.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/04/17 21:03:16 $')
      PARAMETER (CAUTHO= '$Author: anciant $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='rfcal_read_file')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local tag defined variables:
c  ----------------------------

	character*100 comments
	character*50 fname
        integer Ilun,Iretb

c_end_var
c
c  executable code for routine rfcal_read_file:
c  ------------------------------------
c
c      
      crmess = '  '
       ok= .true.
     
         
C Reserve a logical unit number:

        Call ResLun(crname,Ilun,Iretb)
      
c ---- first try to open the local file :

        open(Ilun,status='old',err=10,file='rfcal.dat')
  
    
        CRMESS='Reading RF calibration constants from local file rfcal.dat'
        CALL RECMES(CRNAME,'W',CRMESS)

1       format(f7.5,a100)
	read(Ilun,1) rf1channelW,comments
	read(Ilun,1) rf2channelW,comments
	read(Ilun,1) rf2rf1Offset,comments
	close(Ilun)
        
C Free logical unit number:

10      Call ResLun(crname,-Ilun,Iretb)
        

      RETURN
      END
c
c------------------------------------------------------------------------------

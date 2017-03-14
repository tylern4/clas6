      SUBROUTINE TIMELINE_UPDATEDB(run, event, events_processed)
c
c_begin_doc
c  RCS ID string
c  $Id: timeline_updatedb.F,v 1.2 2001/10/16 16:05:07 gilfoyle Exp $
c
c  Documentation for subroutine TIMELINE_UPDATEDB
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Wed Jan  6 11:05:39 EST 1999
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
      PARAMETER (CFILE = '$RCSfile: timeline_updatedb.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/10/16 16:05:07 $')
      PARAMETER (CAUTHO = '$Author: gilfoyle $')
      DATA CRCSID/   
     1'$Id: timeline_updatedb.F,v 1.2 2001/10/16 16:05:07 gilfoyle Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TIMELINE_UPDATEDB')
      PARAMETER (CRAUTH='')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE

      integer run, event, events_processed
c_end_var
c
c  executable code for routine TIMELINE_UPDATEDB:
c----6----------------------------------------------------------------72
c
      print *,'You are calling routines that are normally used only'
      print *,'for monitoring the online DAQ from the CLAS CLON cluster.'
      print *,'Check your tcl input file to see if you are trying to '
      print *,'read from an online ET station (i.e. set read_et $true).'

      RETURN
      END
c
c------------------------------------------------------------------------------





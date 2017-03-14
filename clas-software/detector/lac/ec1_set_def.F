      SUBROUTINE EC1_SET_DEF
c
c_begin_doc
c  Documentation for subroutine EC1TCL_INIT
c
c  Purpose:  initialize runtime control variables for LAC package
c  --------
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c_end_doc
c
      IMPLICIT NONE
      SAVE
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: ec1_set_def.F,v 1.5 2002/02/23 17:16:24 avakian Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC1_TCL_INIT')
      PARAMETER (CRAUTH='Joe Manak')
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "ec1_control.inc"
c_end_inc
c
*- reconstruction mode type for output   1-using LAC_T0
*     account of non overlapping stacks 2-using LAC_TIMING .
      ec1_reco_type=1       
*     maximum number of clusters in LAC   maximum 10
      ec1maxclust=8
*     - calibration data from 0 use a constant for all (GSIM data)
*     ,1-data file,2-MapManager,3-mSQL server
      ec1_calib=2       
*     - ADC to energy conversion
      ec1_caliba1=0.0685        !to MeV
*     - TDC to time conversion 
      ec1_calibt1=5.0           !to ns 
*     - ADC to energy conversion
      ec1_caliba2=0.0      
*     - TDC to time conversion 
      ec1_calibt2=0.0      
*     - minimum threshold for strips to take into account
      ec1_threshold=5.0         !MeV
*     LAC   timing resolution (time in one strip) in nsec.
      ec1_timeresol=20.0
*     - minimum threshold for deposition in cluster center (inner)
      ec1stenmin_in=10.0        !MeV
*     - minimum threshold for deposition in cluster center (outer)
      ec1stenmin_out=10.0       !MeV
      
      RETURN
      END
c

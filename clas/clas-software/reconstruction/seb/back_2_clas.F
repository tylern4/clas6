      SUBROUTINE BACK_2_CLAS (itr,ipl,sec,pos)
c
c_begin_doc
c  RCS ID string
c  $Id: back_2_clas.F,v 1.4 1998/02/25 22:18:29 stepanya Exp $
c
c  Documentation for subroutine BACK_2_CLAS
c
c  Purpose: Performing transformation of x,y,z,cx,cy,cz from 
c  -------- coordinate system defined in trecking packeg (SDA::Bogdan)
c           to the generic CLAS coordinate system. 
c
c  Input Parameters: itr(integer) - treck number
c  ----------------  ipl(integer) - plane number (CC-40, SC-41:44, FEC-45, 1-VERT) 
c                    sec(integer) - sector number
c                    structure pos(cx,cy,cz,x,y,z)
c                  
c  Output Parameters: fills area pos - transformed x,y,z,cx,cy and cz
c  -----------------
c
c  Other routines:  
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan      Created:  Mon Feb 24 10:55:05 EST 1997
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
#include "evnt_general.inc"
#include "hbt_planes.inc"
#include "tbtinf.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER itr,ipl,sec
      REAL pos(6)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: back_2_clas.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/02/25 22:18:29 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: back_2_clas.F,v 1.4 1998/02/25 22:18:29 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BACK_2_CLAS')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      REAL pi
      DATA pi/3.14159265/
c_end_var
c
c  executable code for routine BACK_2_CLAS:
c----6----------------------------------------------------------------72
c
      if(tracking_hb)then
      pos(1) = 
     =        trkp(2,ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     -      - trkp(3,ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(2) = 
     =        trkp(3,ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     +      + trkp(2,ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(3) = trkp(1,ipl,itr)
*
      pos(4) = 
     =         trkp(5,ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     -       - trkp(6,ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(5) = 
     =         trkp(6,ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     +       + trkp(5,ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(6) = trkp(4,ipl,itr)
      elseif(tracking_tb)then
      pos(1) = 
     =        tbt_y(ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     -      - tbt_z(ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(2) = 
     =        tbt_z(ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     +      + tbt_y(ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(3) = tbt_x(ipl,itr)
*
      pos(4) = 
     =         tbt_cy(ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     -       - tbt_cz(ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(5) = 
     =         tbt_cz(ipl,itr)*cos((itr_sect(itr)-1.)*pi/3.)
     +       + tbt_cy(ipl,itr)*sin((itr_sect(itr)-1.)*pi/3.)
      pos(6) = tbt_x(ipl,itr)
      endif
c      
c      if(ipl.eq.1)then
c      endif
      RETURN
      END
c
c------------------------------------------------------------------------------





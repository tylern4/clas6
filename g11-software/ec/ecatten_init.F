      SUBROUTINE ecatten_init(sector)
*----------------------------------------------------------------------
*-
*-   Purpose and Methods : read in geometry bos bank and initialize
*-                         for all sectors, fill part of EcStrip.CMN
*-
*-  Inputs: sector       - CLAS sector (1-6)
*-
*-
*-   Created  9-June-1996   T.-Y.
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ecatten_init.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:50:26 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/ 
     1'$Id: ecatten_init.F,v 1.1.1.1 1997/03/28 20:50:26 marki Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='eccal_init')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
#include "Ec_general.PAR"
#include "EcCal.CMN"
c        
	character*15 filename
	character*50 fname
	INTEGER LIN,LOUT,LLOG,LHST,LTMP,IRET
	integer sector,id,axis,layer,s
	integer secdum, laydum
	real attldum(6,36) 
	write(CRMESS,19)sector	     
        CALL RECLOG(CRNAME,'I',CRMESS)       
19 	format('Reading Attenuation Length DATA, sector',I4) 
	write(filename,20)sector 
20	format('avatt_sec_',i1,'.dbe')
c
c
	call revinm('CLAS_PARMS',filename,fname) ! get filename from 
c                                                    $CLAS_PARAMS
        CALL RECLUN(LIN,LOUT,LLOG,LHST,LTMP)  ! standard recsis utility to 
c                                             reserve logical unit ltmp
	call reopen(fname,ltmp,'OLD',IRET)
c	open(ltmp,file=fname,status='old') 
c
100	read(ltmp,*,end=120)secdum,laydum,id,attldum(laydum,id)
c
	axis=Ec_u+mod(laydum-1,3)
	layer= (laydum-1)/3+inner
	EcCal_atten( id,axis, layer, secdum)
     &    = attldum(laydum,id)
c
	goto 100
c	
120     close(ltmp)
	return
	end

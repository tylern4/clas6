      subroutine sets_ec1

c_begin_doc

c  Documentation for subroutine sets_ec1

c  $Id: sets_ec1.F,v 1.2 1997/04/24 15:38:53 holtrop Exp $

c  Purpose:
c  --------
c  	creates ec1 detector sets


c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 13-apr-95
C    Created:  23-Dec-1993   S.Boiarinov
C    Modified: 22-Mar-1995   H.Avakian
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: sets_ec1.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:53 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'sets_ec1')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: sets_ec1.F,v 1.2 1997/04/24 15:38:53 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "ec1sets.inc"
c_end_inc

c  local variables:
c  ----------------
      integer iali,nbitsv(3),nbitsh(5),nbitsd(4),nbitsa(5),get_idtype
      CHARACTER*4 namesv(3),namesh(5),namesd(4),namesa(5)
      REAL orig(5),fact(5),origa(5),facta(5)
C  Statement functions:        None
C  Local data statements:
      DATA namesv/'SECT','LAYE','STRI'/
      DATA nbitsv/3,6,6/
      DATA namesh/'X   ','Z   ','L   ','ELOS','TOF '/
      DATA nbitsh/16,16,16,32,32/
      DATA namesd/'ADC-','TDC-','ADC+','TDC+'/
      DATA nbitsd/16,16,16,16/
      DATA namesa/'Xin ','Yin ','Px  ','Py  ','Pz  '/
      DATA nbitsa/32,32,32,32,32/
      DATA orig/500.,500.,500.,10.,10./
      DATA fact/10.,10.,10.,1000.,1000./
      DATA origa/500.,500.,4000.,4000.,4000./
      DATA facta/10.,10.,100.,100.,100./
c_end_var


c  executable code:
c  ----------------
      idtype_ec1=get_idtype()
      CALL GSDET('ECSD','ECL-',3,namesv,nbitsv,idtype_ec1,1000,1000
     *,set_ec1,det_ec1)
      CALL GSDETH('ECSD','ECL-',5,namesh,nbitsh,orig,fact)
      CALL GSDETD('ECSD','ECL-',4,namesd,nbitsd)
C
C alias for LAEC
C
      CALL GSDETA('ECSD','ECL-','PART',100,100,IALI)
      CALL GSDETH('ECSD','PART',5,namesa,nbitsa,origa,facta)
C

      return
      end

c-----------------------------------------------------------------------

      Subroutine trk_set_stat()
*
*  Author:         J. J. Manak 
*  -------         TJNAF
*
*
*  Creation Date:  3/27/98
*  --------------    
      Implicit None 
*_begin_inc
*  include files :
*  ---------------------
#include "trktcl_var.inc"   
*_end_inc
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_set_stat.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.4 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/06/28 12:40:44 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_set_stat.F,v 1.4 2004/06/28 12:40:44 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_SET_STAT')
      PARAMETER (CRAUTH='J. J. Manak')
*
      integer i
*
      CALL ana_stati(1,'trk_evnt:  Ntrig (Nevt) >',0.0)
      CALL ana_stati(2,'ana_segm:  Nevt w. segm =',FLOAT(ipar_trk_minlinks))
      if(ipar_trk_startlink.EQ.1) then
        CALL ana_stati(3,'trk_link:  Nevt w. link =',FLOAT(ipar_trk_minlinks))
      else
        CALL ana_stati(3,'ana_link:  Nevt w. link =',FLOAT(ipar_trk_minlinks))
      endif
      CALL ana_stati(4,'ana_prfit: Nevt PR chi2 <',SNGL(dpar_trk_prfit_chi2))
      CALL ana_stati(5,'SEB_eid:   Nevt SEB     =',0.0)
c     CALL ana_stati(6,'trk_dcam:  Nevt LR chi2 <',SNGL(dpar_trk_lrambfit_chi2))
      CALL ana_stati(6,'trk_dcam:  Nevt w. LRseg=',FLOAT(ipar_trk_minlramb-1))
      CALL ana_stati(7,'trk_dcam:  Nevt w. LRseg=',FLOAT(ipar_trk_minlramb))
      CALL ana_stati(8,'ana_fit:   Nevt FF1 chi2 <',SNGL(dpar_trk_tbtfit_chi2))
      CALL ana_stati(9,'ana_fit:   Nevt in TBT >',0.0)

      if(ipar_trk_VXconstr.NE.0) then
        write(CRMESS,'(A,I2,A)')' VertexConstraint #',ipar_trk_VXconstr,'  Parameter:'
        Call RecMes(CRNAME,'I',CRMESS)
        write(CRMESS,'(A,3F5.1,A,F5.1)')' Target: Offset',(dpar_TargetPos(i),i=1,3),
     &';  TargetLength',dpar_TargetLen
        Call RecMes(CRNAME,'I',CRMESS)
        write(CRMESS,'(A,F5.3,2(A,2F6.3))')' Beam: Sigma',dpar_SigmaBeam,'; Offset{x,y}',
     &(dpar_BeamOff(i),i=1,2),'; CosDir{x/z,y/z}',(dpar_BeamAngle(i),i=1,2)
        Call RecMes(CRNAME,'I',CRMESS)
      endif
cccc messages to be added:    dpar_trk_prfit_chi2
c      ,dpar_trk_lrambfit_chi2
c      ,  dpar_trk_tbtfit_chi2
c      ,  dpar_trk_vx2fit_chi2
c      ,   dpar_trk_vxfit_chi2
c      ,     dpar_trk_zstep(1)
c      ,     dpar_trk_zstep(2)
c      ,       dpar_trk_zangle
c      ,     dpar_trk_Chi2diff
c      ,       ipar_trk_magtyp
c      ,     ipar_trk_bgridtyp
c      ,ipar_minitorus_current
c      ,    ipar_torus_current
c      ,ipar_poltarget_current
c      ,   ipar_Bsign_torus
c      ,ipar_trk_statistics
c      , ipar_trk_level
c      ,    ipar_trk_sector
c      , ipar_trk_mintracks
c      ,    ipar_trk_maxsegm
c      ,   ipar_trk_minlinks
c      ,  ipar_trk_startlink
c      ,   ipar_trk_minlramb
c      ,ipar_trk_make_prlink
c      ,   ipar_trk_multscat
c           ipar_trk_maxiter
c           ipar_trk_calcTM
c      ,ipar_trk_histo , ipar_trk_ntuple
cc style for messages:

      write(CRMESS,'(A,6I3)')' min.hits per Segment',(ipar_trk_minhits(i),i=1,6)
        Call RecMes(CRNAME,'I',CRMESS)
      write(CRMESS,'(3(A,I2))')'HBT: start link in Reg.',ipar_trk_startlink,
     & '; min.links',ipar_trk_minlinks,'; min. lramb',ipar_trk_minlramb
        Call RecMes(CRNAME,'I',CRMESS)
	write(CRMESS,*)'tcl_chi2',dpar_trk_prfit_chi2,dpar_trk_lrambfit_chi2,dpar_trk_tbtfit_chi2
        Call RecMes(CRNAME,'I',CRMESS)
      write(CRMESS,'(A,7I3)')'print',(ipar_trk_print(i),i=1,7)
        Call RecMes(CRNAME,'I',CRMESS)
      Return
      End


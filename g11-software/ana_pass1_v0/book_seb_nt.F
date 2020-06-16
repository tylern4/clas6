      SUBROUTINE BOOK_SEB_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_seb_nt.F,v 1.19 2003/12/08 22:35:45 stepanya Exp $
c
c  Documentation for subroutine BOOK_SEB_NT
c
c  Purpose: Booking Ntuple #10 for reconstructed particles.
c  --------
c
c  Input Parameters:  Non
c  ----------------
c
c  Output Parameters: Non
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S,. Stepanyan    Created:  Fri Mar  7 09:43:38 EST 1997
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
#include "seb_nt.inc"
#include "clasmdl.inc"
#include "evnt_par.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_seb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.19 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2003/12/08 22:35:45 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: book_seb_nt.F,v 1.19 2003/12/08 22:35:45 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_SEB_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER NTID,btype/0/
c_end_var
c
c  executable code for routine BOOK_PART_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for reconstructed particles.'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c Ntuple blok for event header.
      call hbname(NTID,'hevt',run_num,'run_num[0,50000]:I')
      call hbname(NTID,'hevt',evntid,'evntid[0,50000000]:I')
      call hbname(NTID,'hevt',evstat,'evstat[-1,1]:I')
      call hbname(NTID,'hevt',evnttrip,'evntrip[-3,3]:I')
      call hbname(NTID,'hevt',l1bit,'l1bit[0,100]:I')
      call hbname(NTID,'hevt',tr_time,'tr_time:R')
      call hbname(NTID,'hevt',rf_time,'rf_time:R')
      call hbname(NTID,'hevt',helicity,'helicity[0,1]:I')
      call hbname(NTID,'hevt',npart,'npart[0,20]:I')

c Trigger bit bank
      call hbname(NTID,'l1trg',inl1b,
     +'inl1b[0,16]:I,L1bit_v(inl1b):I')

c Ntuple blok for particles in the event.
      call hbname(NTID,'evnt',gpart,
     +'gpart[0,20]:I,'//
     +'id(gpart)[-10000,10000]:I,stat(gpart)[-50,50]:I,'//
     +'dc(gpart)[0,20]:I,cc(gpart)[0,20]:I,sc(gpart)[0,20]:I,ec(gpart)[0,20]:I,lec(gpart)[0,20]:I,'//
     +'st(gpart)[0,20]:I,p(gpart):R,m(gpart):R,q(gpart)[-1,1]:I,b(gpart):R,'//
     +'cx(gpart):R,cy(gpart):R,cz(gpart):R,'//
     +'vx(gpart):R,vy(gpart):R,vz(gpart):R') 


c Ntuple blok for DC tracks involved in the event (particles).
      call hbname(NTID,'dcpb',dc_part,
     +'dc_part[0,20]:I,dc_sect(dc_part)[1,6]:I,dc_trk(dc_part)[0,20]:I,dc_stat(dc_part)[-50,50]:I,'//
     +'tb_st(dc_part):I,dc_xsc(dc_part):R,dc_ysc(dc_part):R,dc_zsc(dc_part):R,'//
     +'dc_cxsc(dc_part):R,dc_cysc(dc_part):R,dc_czsc(dc_part):R,'//
     +'dc_vx(dc_part):R,dc_vy(dc_part):R,dc_vz(dc_part):R,dc_vr(dc_part):R,'//
c     +'tl1_cx(dc_part):R,tl1_cy(dc_part):R,tl1_cz(dc_part):R,'//
c     +'tl1_x(dc_part):R,tl1_y(dc_part):R,tl1_z(dc_part):R,tl1_r(dc_part):R,'//
     +'dc_c2(dc_part):R')
c Ntuple blok for EC hits involved in the event (particles).
      call hbname(NTID,'ecpb',ec_part,
     +'ec_part[0,20]:I,ec_stat(ec_part):I,ec_sect(ec_part)[1,6]:I,'//
     +'ec_whol(ec_part):I,ec_inst(ec_part):I,ec_oust(ec_part):I,'//
     +'etot(ec_part):R,ec_ei(ec_part):R,ec_eo(ec_part):R,'//
     +'ec_t(ec_part):R,ec_r(ec_part):R,'//
     +'ech_x(ec_part):R,ech_y(ec_part):R,ech_z(ec_part):R,'// 
     +'ec_m2(ec_part):R,ec_m3(ec_part):R,ec_m4(ec_part):R,ec_c2(ec_part):R') 
c Ntuple blok for SC hits involved in the event (particles).
      call hbname(NTID,'SCpb',sc_part,
     +'sc_part[0,20]:I,sc_sect(sc_part)[1,6]:I,'//
     +'sc_hit(sc_part)[0,20]:I,sc_pd(sc_part)[0,100]:I,sc_stat(sc_part):I,'//
     +'edep(sc_part):R,sc_t(sc_part):R,sc_r(sc_part):R,sc_c2(sc_part):R') 
c Ntuple blok for CC hits involved in the event (particles).
c      call hbname(NTID,'CCpb',cc_part,
c     +'cc_part[0,20]:I,cc_sect(cc_part):I,cc_hit(cc_part):I,cc_segm(cc_part):I,'//
c     +'nphe(cc_part):I,cc_t(cc_part):R,cc_r(cc_part):R,cc_c2(cc_part):R') 
c Ntuple blok for LAC hits involved in the event (particles).
c      call hbname(NTID,'LECpb',lac_part,
c     +'lac_part[0,20]:I,lec_sect(lac_part):I,lec_hit(lac_part):I,lec_stat(lac_part):I,'//
c     +'lec_etot(lac_part):R,lec_ein(lac_part):R,lec_t(lac_part):R,lec_r(lac_part):R,'//
c     +'lec_x(lac_part):R,lec_y(lac_part):R,lec_z(lac_part):R,lec_c2(lac_part):R') 
c For photon runs
      if(photon_trig_type.ne.-1) then
         btype = photon_trig_type       ! for gsim data
      else    
         call make_runc_bank(nccrun)    ! need to make this in order to use get_beam_type
      endif
      call get_beam_type(btype)
      if(btype.ne.0) then
c Ntuple blok for ST hits involved in the event (particles).  
      if(btype.eq.1.OR.btype.eq.3.OR.btype.eq.4) then
         call hbname(NTID,'STpb',st_part,
     +        'st_part[0,20]:I,st_sector(st_part):I,st_ihit(st_part):I,'//
     +        'st_trkno(st_part):I,st_time(st_part):R,'//
     +        'st_rtrk(st_part):R,st_status(st_part):I') 
      endif
c Ntuple blok for TAG hits involved in the event (particles).     
       call hbname(NTID,'TAGR',taghit,
     +'taghit[0,30]:I,E_gamma(taghit):R,T_gamma_noRF(taghit):R,T_gamma(taghit):R,'//
     +'tagstat(taghit):I,Tid(taghit)[0,130]:I,Eid(taghit)[0,800]:I') 
       
       call hbname(NTID,'TGPB',taggoodhit,
     +'taggoodhit[0,20]:I,tag_ptr(taggoodhit):I,vertex_time(taggoodhit):R,'//
     +'tag_energy(taggoodhit):R,dt_st_tag(taggoodhit):R')
      endif       
      RETURN
      END
c
c------------------------------------------------------------------------------





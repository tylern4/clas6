      SUBROUTINE BOOK_SEB_NTN(NTID,BEAM_TYPE)
c
c_begin_doc
c
c  Purpose: Booking Ntuple #10 for reconstructed particle.
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
#include "seb_ntn.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER NTID
      CHARACTER BEAM_TYPE
c
c  Local pre-defined variables:
c
c_end_var
c
c  executable code for routine BOOK_SEBN_NT:
c----6----------------------------------------------------------------72
c
c Ntuple blok for event header.
      call hbname(NTID,'hevt',evntid,
     +'evntid[0,50000000]:I,'//
     +'evntype[-20,20]:I,evntclas[-500000,500000]:I,'//
     +'q_l:R,t_l:R,tr_time:R,rf_time:R')
c,lt1:I,hlsc:I,intt:I') 
c Ntuple blok for particles in the event.
      call hbname(NTID,'evnt',gpart,
     +'gpart[0,20]:I,id(gpart):I,stat(gpart):I,ccst(gpart):I,'//
     +'p(gpart):R,q(gpart)[-1,1]:I,b(gpart):R,'//
     +'cx(gpart):R,cy(gpart):R,cz(gpart):R,'//
     +'vx(gpart):R,vy(gpart):R,vz(gpart):R') 
c Ntuple blok for DC hits involved in the event (particles).
      call hbname(NTID,'dcpb',dc_part,
     +'dc_part[0,20]:I,dc_stat(dc_part)[-1000,1000]:I,'//
     +'dc_xsc(dc_part):R,dc_ysc(dc_part):R,dc_zsc(dc_part):R,'//
     +'dc_cxsc(dc_part):R,dc_cysc(dc_part):R,dc_czsc(dc_part):R,'//
c     +'dc_vx(dc_part):R,dc_vy(dc_part):R,dc_vz(dc_part):R,'//
     +'dc_vr(dc_part):R,'//
     +'dc_c2(dc_part):R')
c Ntuple blok for EC hits involved in the event (particles).
      call hbname(NTID,'ecpb',ec_part,
     +'ec_part[0,20]:I,ec_stat(ec_part):I,'//
     +'ec_inst(ec_part):I,ec_oust(ec_part):I,'//
     +'etot(ec_part):R,ec_ei(ec_part):R,ec_eo(ec_part):R,'//
     +'ec_t(ec_part):R,ec_r(ec_part):R,'//
     +'ech_x(ec_part):R,ech_y(ec_part):R,ech_z(ec_part):R,'// 
     +'ec_m2(ec_part):R,ec_c2(ec_part):R') 
c Ntuple blok for SC hits involved in the event (particles).
      call hbname(NTID,'SCpb',sc_part,
     +'sc_part[0,20]:I,'//
     +'sc_stat(sc_part):I,'//
     +'edep(sc_part):R,sc_t(sc_part):R,sc_r(sc_part):R,sc_c2(sc_part):R')

c Ntuple blok for LAC hits involved in the event (particles).
       call hbname(NTID,'LECpb',lac_part,    
     +'lac_part[0,20]:I,lec_sect(lac_part):I,lec_hit(lac_part):I,'//
     +'lec_stat(lac_part):I,'//
     +'lec_etot(lac_part):R,lec_t(lac_part):R,lec_r(lac_part):R,'//
     +'lec_x(lac_part):R,lec_y(lac_part):R,lec_z(lac_part):R,lec_c2(lac_part):R')  

      if(beam_type.eq.'g') then
c Ntuple blok for ST hits involved in the event (particles).  
      call hbname(NTID,'STpb',st_part,
     +'st_part[0,20]:I,st_status(st_part):I,'//
     +'st_time(st_part):R,'//
     +'st_rtrk(st_part):R') 
c
c       call hbname(NTID,'TGPB',taggoodhit,
c     +'taggoodhit[0,20]:I,tag_ptr(taggoodhit):I,vertex_time(taggoodhit):R,'//
c     +'tag_energy(taggoodhit):R,dt_st_tag(taggoodhit):R')  
      endif       
      RETURN
      END
c
c------------------------------------------------------------------------------





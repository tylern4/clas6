      SUBROUTINE BOOK_SEB_NT(NTID,BEAM_TYPE)
      
      IMPLICIT NONE
      SAVE
      
#include "seb_nt.inc"

      CHARACTER BEAM_TYPE
      INTEGER NTID
      
c Ntuple blok for event header.
      call hbname(NTID,'hevt',npart,
     +'npart[0,20]:I,evstat[0,100]:I,evntid[0,50000000]:I,'//
     +'evtype[-20,20]:I,evntclas[-32,32]:I,evthel[-32,32]:I,'//
     +'evntclas2[-100000,100000]:I,q_l:R,t_l:R,tr_time:R,'//
     +'rf_time1:R,rf_time2:R')
     
c Ntuple blok for particles in the event.
      call hbname(NTID,'evnt',gpart,
     +'gpart[0,40]:I,'//
     +'id(gpart)[-500,2500]:I,stat(gpart)[-5,5]:I,'//
     +'dc(gpart)[0,40]:I,cc(gpart)[0,40]:I,sc(gpart)[0,40]:I,'//
     +'ec(gpart)[0,40]:I,lec(gpart)[0,40]:I,'//
     +'p(gpart):R,m(gpart):R,q(gpart)[-1,1]:I,b(gpart):R,'//
     +'cx(gpart):R,cy(gpart):R,cz(gpart):R,'//
     +'vx(gpart):R,vy(gpart):R,vz(gpart):R')
      
c Ntuple blok for DC tracks involved in the event (particles).
      call hbname(NTID,'dcpb',dc_part,
     +'dc_part[0,40]:I,dc_sect(dc_part)[1,6]:I,dc_trk(dc_part)[0,40]:I,'//
     +'dc_stat(dc_part)[-1,20]:I,'//
     +'dc_xsc(dc_part):R,dc_ysc(dc_part):R,dc_zsc(dc_part):R,'//
     +'dc_cxsc(dc_part):R,dc_cysc(dc_part):R,dc_czsc(dc_part):R,'//
     +'dc_xec(dc_part):R,dc_yec(dc_part):R,dc_zec(dc_part):R,'//
     +'dc_thcc(dc_part):R,dc_c2(dc_part):R')
      
c Ntuple blok for EC hits involved in the event (particles).
      call hbname(NTID,'ecpb',ec_part,
     +'ec_part[0,40]:I,ec_stat(ec_part)[0,65535]:I,'//
     +'ec_sect(ec_part)[1,6]:I,'//
     +'ec_whol(ec_part):I,ec_inst(ec_part):I,ec_oust(ec_part):I,'//
     +'etot(ec_part):R,ec_ei(ec_part):R,ec_eo(ec_part):R,'//
     +'ec_t(ec_part):R,ec_r(ec_part):R,'//
     +'ech_x(ec_part):R,ech_y(ec_part):R,ech_z(ec_part):R,'// 
     +'ec_m2(ec_part):R,ec_m3(ec_part):R,ec_m4(ec_part):R,ec_c2(ec_part):R') 
     
c Ntuple blok for SC hits involved in the event (particles).
      call hbname(NTID,'SCpb',sc_part,
     +'sc_part[0,40]:I,sc_sect(sc_part)[1,6]:I,'//
     +'sc_hit(sc_part)[1,48]:I,sc_pd(sc_part)[1,58]:I,sc_stat(sc_part)[0,48]:I,'//
     +'edep(sc_part):R,sc_t(sc_part):R,sc_r(sc_part):R,sc_c2(sc_part):R')
      
c Ntuple blok for CC hits involved in the event (particles).
      call hbname(NTID,'CCpb',cc_part,
     +'cc_part[0,40]:I,cc_sect(cc_part)[1,6]:I,cc_hit(cc_part)[1,7]:I,'//
     +'cc_segm(cc_part):I,'//
     +'nphe(cc_part)[0,8191]:I,cc_t(cc_part):R,cc_r(cc_part):R,cc_c2(cc_part):R')
     
c Ntuple blok for LAC hits involved in the event (particles).
       call hbname(NTID,'LECpb',lac_part,    
     +'lac_part[0,50]:I,lec_sect(lac_part):I,lec_hit(lac_part):I,'//
     +'lec_stat(lac_part):I,'//
     +'lec_etot(lac_part):R,lec_t(lac_part):R,lec_r(lac_part):R,'//
     +'lec_x(lac_part):R,lec_y(lac_part):R,lec_z(lac_part):R,lec_c2(lac_part):R') 
c Ntuple blok for ST hits involved in the event (particles).  
      if(beam_type.eq.'g') then
      call hbname(NTID,'STpb',st_part,
     +'st_part[0,100]:I,st_status(st_part):I,'//
     +'st_time(st_part):R,'//
     +'st_rtrk(st_part):R') 
      endif       
      
      RETURN
      END



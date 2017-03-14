      subroutine kine_mcin

c_begin_doc

c  Documentation for subroutine kine_mcin

c  $Id: kine_mcin.F,v 1.26 2009/12/22 15:09:16 natasha Exp $

c  Purpose:
c  --------
c     reads HEAD,MCPA,MCTK,MCVX banks from bos file and fills kine banks

c  Notes:
c  ------
c     only stores track if ending vertex = 0
c     ends run when eof reached

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 6-jul-95

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: kine_mcin.F,v $')
      parameter (crevis= '$Revision: 1.26 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/12/22 15:09:16 $')
      parameter (cautho= '$Author: natasha $')
      parameter (crname= 'KINE_MCIN')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: kine_mcin.F,v 1.26 2009/12/22 15:09:16 natasha Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "wcs.inc"
#include "gcbank.inc"
#include "gconst.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "mcin.inc"
#include "gcflag.inc"
#include "ffpar.inc"
#include "ffpar_tg_e2.inc"
#include "ffpar_tg.inc"
c_end_inc

c  local variables:
c  ----------------

      integer j_part,mpart, jtagr,itagr
      integer ivtx,vtx_id(max_vtx), nrow_mctk, nrow_mcvx
      real    buf(5), buf2(1), vx_off(3)
      integer mcvx,mctk,mchd,jmcvx,jmctk,jmchd,mlink,jv
      integer tknum,it,itrk0,id,ierr,iret,iflag,ivx1,ivx2
      integer lenocc,mbank,pdg_to_geant
      character*8 cunit
      logical first_time,noevent
      data first_time /.true./
      character*200 crmess      
      integer skip,primary_vertex_id,i
	real primary_vertex(3),v(3)
      integer spin,jspin
      integer tgbi,jtgbi
c_end_var


c  executable code:
c  ----------------

c  initialization...get filename,daname, and open file----------------
      if(first_time)then
         first_time=.false.
c
c  reserve unit
         call reslun(crname,mcin_unit,iret)
         if(iret.eq.-1)call recmes(crname,'F',
     1        '?unable to reserve fortran unit')
         write(cunit,'(i8)')mcin_unit
         
c  get filename
         call get_param(gsim_mcin_flag,gsim_mcin_env,
     1        mcin_filename,iflag)
         if(iflag.lt.0)then
            mcin_filename=gsim_mcin_name
         elseif(iflag.eq.1)then
            call recmes(crname,'I',
     1          'getting MCIN input filename from command line flag: -'
     1          // gsim_mcin_flag)
         else
            call recmes(crname,'I',
     1           'getting MCIN input filename from env variable: ' //
     1           gsim_mcin_env)
         endif

c  get daname
         mcin_daname='MCIN'
         
c  open file
         write(crmess,'(''opening BOS MCIN file on unit: '',i2,
     1        '', filename: '',a,'', daname: '',a)')
     1        mcin_unit,mcin_filename(1:lenocc(mcin_filename)),
     1        mcin_daname(1:lenocc(mcin_daname))
         call recmes(crname,'I',crmess)
         call fparm(
     1        ' OPEN '//mcin_daname(1:lenocc(mcin_daname))//
     1        ' UNIT='//cunit//
     1        ' FILE="'//mcin_filename(1:lenocc(mcin_filename))//'"'//
     1        ' ACTION=READ STATUS=OLD FORM=BINARY')

c  stop if error
         call fermes(crmess,1)
         if(lenocc(crmess).gt.0)call recmes(crname,'F',
     1        '?FPARM: '//crmess)
         call fermes(crmess,0)
         if(lenocc(crmess).gt.0)call recmes(crname,'I',
     1        'FPARM: '//crmess)
         
c  select only HEAD,MCPA,MCVX,MCTK, and PART banks
c  SELECT not implemented in bosio: use bank list for bos
         call BLIST(iw,'T=','HEADMCHDMCPAMCVXMCTKPARTSPINTAGRTGBI')
c         call fparm(
c     1        ' SELECT '//mcin_daname(1:lenocc(mcin_daname))//
c     1        ' BLOCK=HEAD MCHD MCPA MCVX MCTK PART SPIN TAGR TGBI')
c         call fermes(crmess,1)
c         if(lenocc(crmess).gt.0)call recmes(crname,'F',
c     1        '?FPARM: '//crmess)
c         call fermes(crmess,0)
c         if(lenocc(crmess).gt.0)call recmes(crname,'I',
c     1        'FPARM: '//crmess)
         

c  initialize skip variable

         skip = skip_ff

      endif
      
c  initialization done----------------------------------------
      
c  read INPUT (either PART of MCVX+MCTK) data from BOS file until eof reached
c  close file and end run upon eof
      
      noevent=.true.
      do while(noevent)
         call frbos(jw,mcin_unit,'T',ierr)
c	call examinedata()
	call makehead(1,1,1)
         if(ierr.eq.-1)then
            call recmes(crname,'I',
     1 'eof reached on MCIN file...run stopped...mcin file closed')
            call fparm('CLOSE '//mcin_daname)
            ieorun=1
            ieotri=1
            return
         elseif(ierr.ne.0)then
            call recmes(crname,'F','?error reading ' //
     1           mcin_filename(1:lenocc(mcin_filename)))
         endif
         
c Skip events at the beginning of the file according to the -skip flag

         if(skip.gt.0) then
             skip=skip-1
             call bdrop(jw,'E')
             call bgarb(jw)
         else

c First see if the (newer) PART bank exists. If it does, we give it
c priority over the MCVX and MCTK banks.
         
         
             j_part=mlink(jw,'PART',0)
             jmchd=mlink(jw,'MCHD',0)
             jmcvx=mlink(jw,'MCVX',0)
             jmctk=mlink(jw,'MCTK',0)
             jspin=mlink(jw,'SPIN',0)
             jtagr=mlink(jw,'TAGR',0)
             jtgbi=mlink(jw,'TGBI',0)
c
c Error if no PART and either no MCVX or no MCTK
c
             noevent=(j_part.le.0).and.((jmcvx.le.0).or.(jmctk.lt.0))
         
             if(noevent)then
                call recmes(crname,'I',
     1            '?no PART, MCVX or MCTK bank in input record')
                call bdrop(jw,'E')
                call bgarb(jw)
             endif
         endif
      enddo                     !noevent loop
C
C Indexes into to the PART BOS bank. 
C
C Input banks are in the JW and RJW arrays.
C 
#define PART_NCOL jw(j_part-5)
#define PART_NROW jw(j_part-4)
#define PART_PID(i)    jw(j_part+ PART_NCOL*(i-1)+1)
#define PART_X(i)      rjw(j_part+ PART_NCOL*(i-1)+2)
#define PART_Y(i)      rjw(j_part+ PART_NCOL*(i-1)+3)
#define PART_Z(i)      rjw(j_part+ PART_NCOL*(i-1)+4)
#define PART_E(i)      rjw(j_part+ PART_NCOL*(i-1)+5)
#define PART_PX(i)     rjw(j_part+ PART_NCOL*(i-1)+6)
#define PART_PY(i)     rjw(j_part+ PART_NCOL*(i-1)+7)
#define PART_PZ(i)     rjw(j_part+ PART_NCOL*(i-1)+8)
#define PART_Q(i)      rjw(j_part+ PART_NCOL*(i-1)+9)
#define PART_TRKID(i)  jw(j_part+ PART_NCOL*(i-1)+10)
#define PART_QPID(i)   rjw(j_part+ PART_NCOL*(i-1)+11)
#define PART_QTRK(i)   rjw(j_part+ PART_NCOL*(i-1)+12)
#define PART_FLAGS(i)  jw(j_part+ PART_NCOL*(i-1)+13)

#define MCHD_NCOL   jw(jmchd-5)
#define MCHD_REACTYPE  jw(jmchd+5)
#define MCHD_Q2        rjw(jmchd+8)

#define MCVX_NCOL   jw(jmcvx-5)
#define MCVX_NROW   jw(jmcvx-4)
#define MCVX_X(i)      rjw(jmcvx+ MCVX_NCOL*(i-1)+1)
#define MCVX_Y(i)      rjw(jmcvx+ MCVX_NCOL*(i-1)+2)
#define MCVX_Z(i)      rjw(jmcvx+ MCVX_NCOL*(i-1)+3)
#define MCVX_TOF(i)    rjw(jmcvx+ MCVX_NCOL*(i-1)+4)
#define MCVX_FLAG(i)   jw(jmcvx+ MCVX_NCOL*(i-1)+5)

#define MCTK_NCOL   jw(jmctk-5)
#define MCTK_NROW   jw(jmctk-4)
#define MCTK_CX(i)     rjw(jmctk+ MCTK_NCOL*(i-1)+1)
#define MCTK_CY(i)     rjw(jmctk+ MCTK_NCOL*(i-1)+2)
#define MCTK_CZ(i)     rjw(jmctk+ MCTK_NCOL*(i-1)+3)
#define MCTK_P(i)      rjw(jmctk+ MCTK_NCOL*(i-1)+4)
#define MCTK_M(i)      rjw(jmctk+ MCTK_NCOL*(i-1)+5)
#define MCTK_Q(i)      rjw(jmctk+ MCTK_NCOL*(i-1)+6)
#define MCTK_PID(i)    jw(jmctk + MCTK_NCOL*(i-1)+7)
#define MCTK_FLAG(i)   jw(jmctk + MCTK_NCOL*(i-1)+8)
#define MCTK_VX1(i)    jw(jmctk + MCTK_NCOL*(i-1)+9)
#define MCTK_VX2(i)    jw(jmctk + MCTK_NCOL*(i-1)+10)

C  smear vertex position (different procedures for electron/photon beam)
      call kine_smear_vx(vx_off)

	      do i=1,3
		 primary_vertex(i)=v(i) + vx_off(i)
	      enddo

     
      if(tg_e2)call vert_e2(vx_off)

      if(tg_type_ff.EQ.5)call vert_e5(vx_off)

      if(tg_type_ff.EQ.18)call vert_e2b(vx_off)

      if(tg_type_ff.EQ.19)call vert_e6a(vx_off)
      
      if(tg_type_ff.EQ.26)call vert_eg2(vx_off)

      if(tg_type_ff.EQ.27)call vert_eg6(vx_off)
c
C Process the PART bank: loop over particles: 
C store kine and vertex info for each particle

      if(j_part.gt.0)then
         PART_X(1) = PART_X(1) + vx_off(1)
         PART_Y(1) = PART_Y(1) + vx_off(2)
         PART_Z(1) = PART_Z(1) + vx_off(3)
         if(jmchd.GT.0) then
            call gsvert(PART_X(1),0,0,MCHD_Q2,5,vtx_id(1))
         else
            call gsvert(PART_X(1),0,0,0,0,vtx_id(1))
         endif
         buf2(1)=1
         call gskine(PART_PX(1),PART_PID(1),vtx_id(1),buf2,1,tknum)
      
         do it=2,PART_NROW
            PART_X(it) = PART_X(it) + vx_off(1)
            PART_Y(it) = PART_Y(it) + vx_off(2)
            PART_Z(it) = PART_Z(it) + vx_off(3)
            call gsvert(PART_X(it),0,0,0,0,vtx_id(it))
            buf2(1)=it
            call gskine(PART_PX(it),PART_PID(it),vtx_id(it),buf2,1,tknum)
         enddo  ! particle loop.

      else

C Process the MCVX and MCTK banks.
c  get number of vertices and tracks
         nrow_mctk = MCTK_NROW
         nrow_mcvx = MCVX_NROW

         if(nrow_mctk.le.0 .or. nrow_mctk.le.0) then
            call recmes(crname,'i',
     1           '?no vertices or tracks in event record')
            call bdrop(jw,'E')
            call bgarb(jw)
            return
         endif
         
         
c  error if too many vertices
         if(nrow_mcvx.gt.max_vtx)then
            call recmes(crname,'i','?too many vertices...event skipped')
            call bdrop(jw,'E')
            call bgarb(jw)
            return
         endif

         
c  store vertices, id's, and tof
         itrk0 = 0
         do ivtx=1,nrow_mcvx
            MCVX_X(ivtx) = MCVX_X(ivtx) + vx_off(1)
            MCVX_Y(ivtx) = MCVX_Y(ivtx) + vx_off(2)
            MCVX_Z(ivtx) = MCVX_Z(ivtx) + vx_off(3)
            if(itrk0.EQ.0) then
               if(jmchd.gt.0) then
c  real/virtual photon track (incl. info for Q2,Energy)
                  call gsvert(MCVX_X(ivtx),0,0,MCHD_Q2,5,vtx_id(ivtx))
                  itrk0=-1
               else
                  do it=1,nrow_mctk
                     ivx1=MCTK_VX1(it)
                     ivx2=MCTK_VX2(it)
                     if(MCTK_PID(it).EQ.22 .AND. ivx1.LE.0 .AND. 
     &                      ivx2.GT.0 .AND. ivx2.LE.nrow_mcvx) then
                        buf(1)=MCTK_M(it)
                        buf(2)=MCTK_P(it)
                        buf(3)=MCTK_CX(it) * MCTK_P(it)
                        buf(4)=MCTK_CY(it) * MCTK_P(it)
                        buf(5)=MCTK_CZ(it) * MCTK_P(it)
                        call gsvert(MCVX_X(ivtx),0,0,buf,5,vtx_id(ivtx))
                        itrk0 = it
                     endif
                  enddo
                  if(itrk0.eq.0) then
                     call gsvert(MCVX_X(ivtx),0,0,0,0,vtx_id(ivtx))
                  endif
               endif
            else
               call gsvert(MCVX_X(ivtx),0,0,0,0,vtx_id(ivtx))
            endif
C
C This is a method of accessing the GEANT data structure: "JVERTX" 
C (See GEANT manual KINE100 and KINE199)
C Position 4 is TOF, which is not filled through a subroutine call.
C It's usually zero, for a real event, but may be different if
C background (not related to the promary interaction) is being considered.
C
            jv=LQ(jvertx-vtx_id(ivtx))
            Q(jv+4)=MCVX_TOF(ivtx) + vx_off(3)/clight

         enddo
         
c  store track data in Zebra if track has legal beginning vtx, no ending vtx
         do it=1,nrow_mctk
            ivx1=MCTK_VX1(it)
            ivx2=MCTK_VX2(it)
            if(ivx1.GT.0 .AND. ivx1.LE.nrow_mcvx .AND. ivx2.EQ.0) then
               buf(1)=MCTK_CX(it) * MCTK_P(it)
               buf(2)=MCTK_CY(it) * MCTK_P(it)
               buf(3)=MCTK_CZ(it) * MCTK_P(it)
               id=pdg_to_geant(MCTK_PID(it))
               buf2(1)=it
               call gskine(buf,id,vtx_id(ivx1),buf2,1,tknum)
            endif
         enddo
         
      endif  ! Test for PART bank.

c  delete jw banks and return if no bos output
      if(nobosout(all))then
         call bdrop(jw,'E')
         call bgarb(jw)
         return
      endif
c  create new spin bank in normal BOS common   
      if(jspin.gt.0) then
        spin=mbank(iw,'SPIN',0,jw(jspin-5),jw(jspin-4))
        call ucopy(jw(jspin+1),iw(spin+1),jw(jspin))    
      endif

c  create new TGBI bank in normal BOS common   
      if(jtgbi.gt.0) then
        tgbi=mbank(iw,'TGBI',0,jw(jtgbi-5),jw(jtgbi-4))
        call ucopy(jw(jtgbi+1),iw(tgbi+1),jw(jtgbi))    
      endif

c  create new tagr bank in normal BOS common   
      if(jtagr.gt.0) then
        itagr=mbank(iw,'TAGR',0,jw(jtagr-5),jw(jtagr-4))
        call ucopy(jw(jtagr+1),iw(itagr+1),jw(jtagr))    
      endif

      if(jmchd.GT.0) then
        mchd=MBANK(IW,'MCHD',0,MCHD_NCOL,1)
        call ucopy(JW(jmchd+1),IW(mchd+1),MCHD_NCOL)    
      endif
c
      if(j_part.gt.0)then
         mpart = mbank(iw,'PART',0,PART_NCOL,PART_NROW)
         call ucopy(jw(j_part+1),iw(mpart+1),jw(j_part))
         call ucopy(jw(j_part+1),iw(mpart+1),jw(j_part))
      else
c  create new MCVX,MCTK banks in normal BOS common      
         mcvx=mbank(iw,'MCVX',0,MCVX_NCOL,nrow_mcvx)
         mctk=mbank(iw,'MCTK',0,MCTK_NCOL,nrow_mctk)
c         
c  copy data into new banks
c
         call ucopy(jw(jmcvx+1),iw(mcvx+1),jw(jmcvx))
         call ucopy(jw(jmctk+1),iw(mctk+1),jw(jmctk))
      endif ! Test for PART Bank.
c  delete banks in jw
      call bdrop(jw,'E')
      call bgarb(jw)
c
      return
      end
      
c------------------------------------------------------------------------------



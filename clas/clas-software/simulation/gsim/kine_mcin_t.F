      subroutine kine_mcin_t

c_begin_doc

c  Documentation for subroutine kine_mcin

c  $Id: kine_mcin_t.F,v 1.11 2009/03/14 20:27:51 fklein Exp $

c  Purpose:
c  --------
c     reads HEAD,MCTK,MCVX banks from bos file and fills kine banks

c  Notes:
c  ------
c     only stores track if ending vertex = 0
c     ends run when eof reached

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 6-jul-95
c  Modified:
c     Stepan Stepanyan, TJNAF 7-jul-98

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: kine_mcin_t.F,v $')
      parameter (crevis= '$Revision: 1.11 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:51 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'KINE_MCIN_T')
      parameter (crauth= 'Stepan')
      data crcsid/'
     1$Id: kine_mcin_t.F,v 1.11 2009/03/14 20:27:51 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "wcs.inc"
#include "gcbank.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "mcin.inc"
#include "gcflag.inc"
#include "ffpar.inc"
c_end_inc

c  local variables:
c  ----------------

      integer ivtx,beg_vtx,end_vtx,nvtx,vtx_id(max_vtx)
      real plab(4),pmom, buf(5),buf2(1), jmchd,mchd,itrk0
      integer mcvx,mctk,jmcvx,jmctk,mlink,jv,part_stat,spin,jspin
      integer tgbi,jtgbi
      integer ntrk,tknum,itrk,id,ierr,iret,iflag, jtagr,itagr
      integer lenocc,mbank,pdg_to_geant
      character*8 cunit
      logical first_time,noevent
      data first_time/.true./
      character*200 crmess      
      integer skip
      real  vx_off(3)
c_end_var


c  executable code:
c  ----------------

c  initialization...get filename,daname, and open file----------------
      if(first_time)then
         first_time=.false.
         
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
         
c  select only HEAD,MCHD,MCVX,MCTK,DC0,DOCA,EC,SC,EC1 and CC banks
c  SELECT not implemented in bosio: use bank list for bos
         call BLIST(iw,'T=','HEADMCHDMCPAMCVXMCTKPARTSPINTAGRTGBI')
c         call fparm(
c     1        ' SELECT '//mcin_daname(1:lenocc(mcin_daname))//
c     1        ' BLOCK=HEAD MCHD MCVX MCTK DC0  DOCA EC   SC   CC   EC1 SPIN TAGR TGBI')
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
      
c  read INPUT data from BOS file until eof reached
c  close file and end run upon eof
      
      noevent=.true.
      do while(noevent)

c  delete banks in jw
        call bdrop(jw,'E')
        call bgarb(jw)

        call frbos(jw,mcin_unit,'T',ierr)
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
         else
c- Get indexes for MCTK and MCVX
             jmchd=mlink(jw,'MCHD',0)
             jmcvx=mlink(jw,'MCVX',0)
             jmctk=mlink(jw,'MCTK',0)
             jspin=mlink(jw,'SPIN',0)
             jtagr=mlink(jw,'TAGR',0)
             jtgbi=mlink(jw,'TGBI',0)
c
c Error if no MCVX or no MCTK
c
             noevent=((jmcvx.le.0).or.(jmctk.lt.0))
         
             if(noevent)then
                call recmes(crname,'I',
     1            '?no MCVX or MCTK bank in input record')
                call bdrop(jw,'E')
                call bgarb(jw)
             endif
         endif
      enddo                     !noevent loop
      

C Process the MCVX and MCTK banks.
c  get number of vertices and tracks

#define MCHD_REACTYPE  rjw(jmchd+5)
#define MCHD_Q2        rjw(jmchd+8)

#define MCVX_NCOL   jw(jmcvx-5)
#define MCVX_X(i)      rjw(jmcvx+ MCVX_NCOL*(i-1)+1)
#define MCVX_Y(i)      rjw(jmcvx+ MCVX_NCOL*(i-1)+2)
#define MCVX_Z(i)      rjw(jmcvx+ MCVX_NCOL*(i-1)+3)
#define MCVX_TOF(i)    rjw(jmcvx+ MCVX_NCOL*(i-1)+4)
#define MCVX_FLAG(i)   rjw(jmcvx+ MCVX_NCOL*(i-1)+5)

#define MCTK_NCOL   jw(jmctk-5)
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

         nvtx=jw(jmcvx-4)
         ntrk=jw(jmctk-4)
         if((nvtx.le.0).or.(ntrk.le.0))then
            call recmes(crname,'i',
     1           '?no vertices or tracks in event record')
            call bdrop(jw,'E')
            call bgarb(jw)
            return
         endif
         
         
c  error if too many vertices
         if(nvtx.gt.max_vtx)then
            call recmes(crname,'i','?too many vertices...event skipped')
            call bdrop(jw,'E')
            call bgarb(jw)
            return
         endif

c  smear vertex position if ffread cards 'POSBEAM' or 'SIGBEAM' are set
         call kine_smear_vx(vx_off)

c  store vertices, id's, and tof
         itrk0 = 0
         do ivtx=1,nvtx
            MCVX_X(ivtx) = MCVX_X(ivtx) + vx_off(1)
            MCVX_Y(ivtx) = MCVX_Y(ivtx) + vx_off(2)
            MCVX_Z(ivtx) = MCVX_Z(ivtx) + vx_off(3)
            if(itrk0.EQ.0) then
               if(jmchd.gt.0) then
c  real/virtual photon track (incl. info for Q2,Energy)
                  call gsvert(MCVX_X(ivtx),0,0,MCHD_Q2,5,vtx_id(ivtx))
                  itrk0=-1
               else
                  do itrk=1,ntrk
                     if(MCTK_PID(itrk).EQ.22 .AND. MCTK_VX1(itrk).LE.0 .AND. 
     &                   MCTK_VX2(itrk).GT.0 .AND. MCTK_VX2(itrk).LE.nvtx) then
                        buf(1)=MCTK_M(itrk)
                        buf(2)=MCTK_P(itrk)
                        buf(3)=MCTK_CX(itrk) * MCTK_P(itrk)
                        buf(4)=MCTK_CY(itrk) * MCTK_P(itrk)
                        buf(5)=MCTK_CZ(itrk) * MCTK_P(itrk)
                        call gsvert(MCVX_X(ivtx),0,0,buf,5,vtx_id(ivtx))
                        itrk0 = itrk
                     endif
                  enddo
                  if(itrk0.eq.0)
     &               call gsvert(MCVX_X(ivtx),0,0,0,0,vtx_id(ivtx))
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
            jv=lq(jvertx-vtx_id(ivtx))
            q(jv+4)=rjw(jmcvx+5*(ivtx-1)+4)

         enddo
         
         
c  store track data in Zebra if track has legal beginning vtx, no ending vtx
         do itrk=1,ntrk
            beg_vtx=jw(jmctk+11*(itrk-1)+9)
            end_vtx=jw(jmctk+11*(itrk-1)+10)
            part_stat=jw(jmctk+11*(itrk-1)+8)
            if((itrk.ne.itrk0.and.beg_vtx.le.0).or.(beg_vtx.gt.nvtx))then
               write(crmess,
     1              '(''?ignoring track with illegal beg vtx: '',i10)')
     1              beg_vtx
               call recmes(crname,'I',crmess)
               
            elseif(end_vtx.eq.0.and.part_stat.lt.10)then
               pmom   =rjw(jmctk+11*(itrk-1)+4)
               plab(1)=rjw(jmctk+11*(itrk-1)+1)*pmom
               plab(2)=rjw(jmctk+11*(itrk-1)+2)*pmom
               plab(3)=rjw(jmctk+11*(itrk-1)+3)*pmom
               plab(4)=sqrt(pmom**2+rjw(jmctk+11*(itrk-1)+5)**2)
               id=pdg_to_geant(jw(jmctk+11*(itrk-1)+7))
               buf2(1)=itrk
               call gskine(plab,id,vtx_id(beg_vtx),buf2,1,tknum)
               jw(jmctk+11*(itrk-1)+10)=1
            endif
         enddo
         
c  delete jw banks and return if no bos output
      if(nobosout(all))then
         call bdrop(jw,'E')
         call bgarb(jw)
         return
      endif


c  create new MCVX,MCTK banks in normal BOS common
      
      if(jmchd.GT.0) then
        mchd=MBANK(iw,'MCHD',0,jw(jmchd-5),1)
        call ucopy(JW(jmchd+1),IW(mchd+1),jw(jmchd))    
      endif
      mcvx=mbank(iw,'MCVX',0,jw(jmcvx-5),nvtx)
      mctk=mbank(iw,'MCTK',0,jw(jmctk-5),ntrk)
         
c  copy data into new banks
         if(jmcvx.gt.0.and.mcvx.gt.0)call ucopy(jw(jmcvx+1),iw(mcvx+1),jw(jmcvx))
         if(jmctk.gt.0.and.mctk.gt.0)call ucopy(jw(jmctk+1),iw(mctk+1),jw(jmctk))
         if(jspin.gt.0) then
           spin=mbank(iw,'SPIN',0,jw(jspin-5),jw(jspin-4))
           call ucopy(jw(jspin+1),iw(spin+1),jw(jspin))  
         endif
         if(jtgbi.gt.0) then
           tgbi=mbank(iw,'TGBI',0,jw(jtgbi-5),jw(jtgbi-4))
           call ucopy(jw(jtgbi+1),iw(tgbi+1),jw(jtgbi))  
         endif
         if(jtagr.GT.0) then
            itagr=mbank(iw,'TAGR',0,jw(jtagr-5),jw(jtagr-4))
            call ucopy(jw(jtagr+1),iw(itagr+1),jw(jtagr))    
         endif  
c         call bdrop(jw,'E')
c         call bgarb(jw)
      return
      end
      
c------------------------------------------------------------------------------

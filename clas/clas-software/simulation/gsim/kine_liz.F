	subroutine kine_liz

c_begin_doc

c  Documentation for subroutine kine_liz

c  $Id: kine_liz.F,v 1.8 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c     reads "Liz-format" (Zhujun Li) ascii files and fills kine banks
c     stops run when eof reached

c  Notes:
c  ------
c     primary vertex always at (0,0,0)


c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 6-jul-95

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: kine_liz.F,v $')
      parameter (crevis= '$Revision: 1.8 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'KINE_LIZ')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: kine_liz.F,v 1.8 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcflag.inc"
#include "bcs.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c  local variables:
c  ----------------
	logical first_time
	data first_time/.true./

	real pmom,plab(4),amass,vmod,buf(1)
        integer tknum,primary_vertex_id,lenocc,iostat,id
	real primary_vertex(3)
        integer liz_unit,iret,ntrk,itk,idpdg,mbank,idum
        integer pdg_to_geant,mcvx,mctk,iflag
        real wt,y,w2,q2,ebeam,pchrg
        character*120 filename
c_end_var


c  executable code:
c  ----------------

c  open file
	if(first_time)then
           first_time=.false.
           call reslun(crname,liz_unit,iret)
           if(iret.eq.-1)call recmes(crname,'F',
     1          '?unable to reserve unit')
           
           call get_param(gsim_lizin_flag,gsim_lizin_env,
     1          filename,iflag)
           if(iflag.lt.0)then
              filename=gsim_lizin_name
           elseif(iflag.eq.1)then
              call recmes(crname,'I',
     1           'getting Liz input filename from command line flag: -'
     1           // gsim_lizin_flag)
           else
              call recmes(crname,'I',
     1             'getting Liz input filename from env variable: ' //
     1             gsim_lizin_env)
           endif
           
           call reopen(filename,liz_unit,'old',iret)
           if(iret.eq.-1)call recmes(crname,'F',
     1          '?unable to open lizinput file')
	endif

c  fill bos header bank...15 is for physics events
      call bos_head(15,idrun,idevt)

c  read event header line to get number of tracks
c  stop run on eof
        read(liz_unit,*,iostat=iostat)ntrk,wt,y,w2,q2,ebeam
        if(iostat.lt.0)then
           call recmes(crname,'I',
     1     'eof reached on Liz input file...run stopped...file closed')
           close(unit=liz_unit)
           ieorun=1
           ieotri=1
           return
        elseif(iostat.ne.0)then
           call recmes(crname,'F',
     1          '?error reading ' // filename(1:lenocc(filename)))
        endif
           
c  primary vertex always at (0,0,0)
c  or smeared if ffread cards 'POSBEAM' or 'SIGBEAM' are set
	call kine_smear_vx(primary_vertex)

c  store primary vertex in zebra
	call gsvert(primary_vertex,0,0,0,0,primary_vertex_id)

c  get mcvx,mctk pointers and fill mcvx
        if(.not.nobosout(all))then
           mcvx=mbank(iw,'MCVX',0,5,1)
           mctk=mbank(iw,'MCTK',0,11,ntrk)
           rw(mcvx+1)=primary_vertex(1)
           rw(mcvx+2)=primary_vertex(2)
           rw(mcvx+3)=primary_vertex(3)
           rw(mcvx+4)=0.0
           iw(mcvx+5)=0
        endif


c  read and store tracks in zebra,bos
        do itk=1,ntrk
           read(liz_unit,*,iostat=iostat)idum,idpdg,
     1          plab(1),plab(2),plab(3),plab(4),amass
           if(iostat.ne.0)call recmes(crname,'F',
     1          '?error reading ' // filename(1:lenocc(filename)))

           pmom=vmod(plab,3)
           id=pdg_to_geant(idpdg)
	   buf(1)=itk
           call gskine(plab,id,primary_vertex_id,buf,1,tknum)

           if(.not.nobosout(all))then
              rw(mctk+11*(itk-1)+1) = plab(1)/pmom
              rw(mctk+11*(itk-1)+2) = plab(2)/pmom
              rw(mctk+11*(itk-1)+3) = plab(3)/pmom
              rw(mctk+11*(itk-1)+4) = pmom
              rw(mctk+11*(itk-1)+5) = amass
              rw(mctk+11*(itk-1)+6) = pchrg(idpdg)
              iw(mctk+11*(itk-1)+7) = idpdg
              iw(mctk+11*(itk-1)+8) = 0
              iw(mctk+11*(itk-1)+9) = 1
              iw(mctk+11*(itk-1)+10)= 0
              iw(mctk+11*(itk-1)+11)= 0
           endif

        enddo

	return
	end
	
c------------------------------------------------------------------------------

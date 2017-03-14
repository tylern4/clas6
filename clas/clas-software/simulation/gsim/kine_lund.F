	subroutine kine_lund

c_begin_doc

c  JWP; 4/2/97
c  Documentation for subroutine kine_lund

c  $Id: kine_lund.F,v 1.9 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c  JWP; 4/2/97
c     reads "Lund-format" ascii files and fills GEANT data structures 
c     JVERTX and JKINE with calls to GSVERT and GSKINE, and fills BOS 
c     banks MCVX and MCTK and PART

c  Notes:
c  ------
c
c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 6-jul-95

c  JWP; 4/2/97
c     John W. Price, Rensselaer Polytechnic Institute, 2-apr-97

c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: kine_lund.F,v $')
      parameter (crevis= '$Revision: 1.9 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'KINE_LUND')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: kine_lund.F,v 1.9 2009/03/14 20:27:52 fklein Exp $
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

c  Rustam; 07/22/08 Fixed filling MCVX Bank for all particles 
c  JWP; 4/2/97 -- add variables k(5), v(5); keep plab(4) as p(4) so I 
c                 can use the vmod routine to get the length of the 
c                 vector.
	real pmom,p(4),v(5),amass,vmod,buf(1), vx_off(3)
        integer k(5),tknum,primary_vertex_id,lenocc,iostat,id,i
	real primary_vertex(3)

c  JWP; 4/2/97 -- change ntrk to n to agree with Lund format; delete 
c                 idum; also, idpdg is replaced by k(2).
        integer lund_unit,iret,n,itk,mbank
        integer pdg_to_geant,mcvx,mctk,part,iflag

c  JWP; 4/2/97 -- change wt to x, ebeam to u, to conform to Lund format.
        real x,y,w2,q2,u,pchrg
        character*120 filename
c  H.A. 9/16/2004 Add the PART bank.
c_end_var


c  executable code:
c  ----------------
 
c  open file
	if(first_time)then
           first_time=.false.
           call reslun(crname,lund_unit,iret)
           if(iret.eq.-1)call recmes(crname,'F',
     1          '?unable to reserve unit')
           
           call get_param(gsim_lundin_flag,gsim_lundin_env,
     1          filename,iflag)
           print *,'kine_lund: ',gsim_lundin_flag,gsim_lundin_env,gsim_lundin_name,'*',filename
           if(iflag.lt.0)then
              filename=gsim_lundin_name
           elseif(iflag.eq.1)then
              call recmes(crname,'I',
     1           'getting Lund input filename from command line flag: -'
     1           // gsim_lundin_flag)
           else
              call recmes(crname,'I',
     1             'getting Lund input filename from env variable: ' //
     1             gsim_lundin_env)
           endif
           
           call reopen(filename,lund_unit,'old',iret)
           if(iret.eq.-1)call recmes(crname,'F',
     1          '?unable to open lund input file')
	endif

c  fill bos header bank...15 is for physics events
      call bos_head(15,idrun,idevt)

c  read event header line to get number of tracks
c  stop run on eof
        read(lund_unit,*,iostat=iostat)n,x,y,w2,q2,u
        if(iostat.lt.0)then
           call recmes(crname,'I',
     1     'eof reached on Lund input file...run stopped...file closed')
           close(unit=lund_unit)
           ieorun=1
           ieotri=1
           return
        elseif(iostat.ne.0)then
           call recmes(crname,'F',
     1          '?error reading ' // filename(1:lenocc(filename)))
        endif
           
c  read and store tracks in zebra,bos
        do itk=1,n
           read(lund_unit,*,iostat=iostat) k(1),k(2),k(3),k(4),k(5)
           if(iostat.ne.0)call recmes(crname,'F',
     1          '?error reading ' // filename(1:lenocc(filename)))
           read(lund_unit,*,iostat=iostat) p(1),p(2),p(3),p(4),amass
           if(iostat.ne.0)call recmes(crname,'F',
     1          '?error reading ' // filename(1:lenocc(filename)))
           read(lund_unit,*,iostat=iostat) v(1),v(2),v(3),v(4),v(5)
           if(iostat.ne.0)call recmes(crname,'F',
     1          '?error reading ' // filename(1:lenocc(filename)))

c  JWP; 4/2/97 -- Primary vertex parameters are the vertex parameters 
c                 of the first track.
           if (itk .eq. 1) then

c  smear vertex position if ffread cards 'POSBEAM' or 'SIGBEAM' are set
	      call kine_smear_vx(vx_off)

	      do i=1,3
		 primary_vertex(i)=v(i)
                 v(i) = v(i) + vx_off(i)
	      enddo

c  store primary vertex in zebra
c              call gsvert(primary_vertex,0,0,0,0,primary_vertex_id)
c  get mcvx,mctk pointers and fill mcvx
              if(.not.nobosout(all))then
                 mcvx=mbank(iw,'MCVX',0,5,n)
                 mctk=mbank(iw,'MCTK',0,11,n)
                 part=mbank(iw,'PART',0,13,n)
              endif
           end if

           if(v(1).eq.primary_vertex(1).and.v(2).eq.primary_vertex(2).and.v(3).eq.primary_vertex(3))then
            do i=1,3
             v(i) = v(i) + vx_off(i)
            enddo
           endif

           call gsvert(v,0,0,0,0,primary_vertex_id)
           pmom=vmod(p,3)

c  JWP; 4/2/97 -- Only track the particle if it still exists.
           if (k(1) .le. 10) then
              id=pdg_to_geant(k(2))
	      buf(1)=itk
              call gskine(p,id,primary_vertex_id,buf,1,tknum)
           end if

           if(.not.nobosout(all))then
c HA: 9/16/04 fill the PART bank also
             iw(part+13*(itk-1)+1) = pdg_to_geant(k(2))  ! particle ID (GEANT)
             rw(part+13*(itk-1)+2) = v(1)	! x vertex position
             rw(part+13*(itk-1)+3) = v(2)	! y vertex position
             rw(part+13*(itk-1)+4) = v(3)	! z vertex position
             rw(part+13*(itk-1)+5) = sqrt(amass*amass+pmom*pmom) !energy
             rw(part+13*(itk-1)+6) = p(1)		! px
             rw(part+13*(itk-1)+7) = p(2)		! py
             rw(part+13*(itk-1)+8) = p(3)		! pz
             rw(part+13*(itk-1)+9) = pchrg(k(2))      ! charge
             iw(part+13*(itk-1)+10) = k(3)			! store parent
             rw(part+13*(itk-1)+11) = y                 ! store y
             rw(part+13*(itk-1)+12) = u                 ! store the weight
             iw(part+13*(itk-1)+13) = k(5)              ! store helicities
c
              rw(mctk+11*(itk-1)+1) = p(1)/pmom
              rw(mctk+11*(itk-1)+2) = p(2)/pmom
              rw(mctk+11*(itk-1)+3) = p(3)/pmom
              rw(mctk+11*(itk-1)+4) = pmom
              rw(mctk+11*(itk-1)+5) = amass
              rw(mctk+11*(itk-1)+6) = pchrg(k(2))
              iw(mctk+11*(itk-1)+7) = k(2)
              iw(mctk+11*(itk-1)+8) = k(1)
              iw(mctk+11*(itk-1)+9) = 1
c Rustam;07/22/08 mcvx fix for all particles
	      rw(mcvx+5*(itk-1)+1)=v(1)
	      rw(mcvx+5*(itk-1)+2)=v(2)
	      rw(mcvx+5*(itk-1)+3)=v(3)
	      rw(mcvx+5*(itk-1)+4)=v(4)
	      iw(mcvx+5*(itk-1)+5)=0

c  JWP; 4/2/97 -- e_vtx = 1 if particle has decayed.
              if (k(1) .le. 10) then
                 iw(mctk+11*(itk-1)+10)= 0
              else
                 iw(mctk+11*(itk-1)+10)= 1
              end if

c  JWP; 4/2/97 -- parent track is stored in k(3)
              iw(mctk+11*(itk-1)+11)= k(3)
           endif

        enddo

	return
	end
	
c------------------------------------------------------------------------------

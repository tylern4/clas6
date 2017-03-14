      program fdump

c  dumps fpack file info

c  Elliott Wolin, W&M, 14-aug-95


      implicit none

      integer ierr,narg,iargc,ikey
      logical eof,eor
      character*120 crmess,filename

      character*15 kname,hname,format
      integer numdb,ncol,nrow,nch,numra,numrb,icl,nwd

      integer max_data
      parameter(max_data=20000)
      integer data(max_data)

      integer lenocc,flen,find_arg
      logical pause
      character*1 ans


c  executable code:
c  ----------------

c  check for help request
      narg=iargc()
      if((narg.le.0).or.(find_arg('h').ne.0).or.
     1     (find_arg('help').ne.0))then
         print *
         print *,' Usage:  fdump [-nopause] file.ext'
         print *
         stop
      endif

c  check for nopause option
      pause=find_arg('nopause').eq.0

c  get file name from command line
      if(pause)then
         call getarg(1,filename)
      else
         call getarg(2,filename)
      endif
      flen=lenocc(filename)


c  open file
      print *
      print *,'opening: ',filename(1:flen)
      call fparm(
     1     ' OPEN FDUMP'//
     1     ' UNIT=1'//
     1     ' FILE="' // filename(1:flen) // '"' //
     1     ' RECL=32760'//
     1     ' ACTION=READ STATUS=OLD FORM=BINARY')

      call fermes(crmess,1)
      if(lenocc(crmess).gt.0)then
         print *,'?FPARM: '//crmess
         stop
      endif
      call fermes(crmess,0)
      if(lenocc(crmess).gt.0)print *,'FPARM: '//crmess

      call frname('FDUMP')
      ikey=0



c  loop over all input records
      eof=.false.
      do while(.not.eof)
         call frkey(kname,numra,numrb,icl,ierr)
         eof=ierr.eq.-1
         
c  dump each record key
         if(.not.eof)then
            ikey=ikey+1
            if(ierr.ne.0)print *,'?fdump...error on frkey: ',ierr
            print '('' key ='',i5,''  name = '',a15,''  numra ='',i5,
     1           ''  numrb ='',i5,''  icl ='',i5)',
     1           ikey,kname,numra,numrb,icl
            print *
            
c  dump all headers in this record
            print '(5x,a15,1x,4(a6,1x),4x,a15)',
     1           'hname          ','numdb','ncol','nrow',' nwd',
     1           '         format'
            print '(5x,a15,1x,4(a6,1x),4x,a15)',
     1           '---------------','-----','----','----','----',
     1           '---------------'
            eor=.false.
            do while(.not.eor)
               call frhdr(hname,numdb,ncol,nrow,format,nch,ierr)
               eor=ierr.eq.-1
               if(.not.eor)then
                  if(ierr.ne.0)then
                     print *,'?fdump...error on frhdr: ',ierr
                  else
                     call frdat(nwd,data,max_data)
                     print '(5x,a15,1x,4(i6,1x),4x,a15,1x,i6)',
     1                    hname,numdb,ncol,nrow,nwd,format(1:nch)
                  endif
               endif
            enddo
            print *
            print *
         endif

c  pause between records if requested
         if(pause)then
            print '('' enter return to continue, q to quit: '',$)'
            read(*,'(a1)')ans
            eof=eof.or.(ans.eq.'q').or.(ans.eq.'Q')
         endif

      enddo


c  close file
      print *
      print *,'EOF reached...number of keys read is: ',ikey
      print *
      call fparm('CLOSE FDUMP')
      call fermes(crmess,1)
      if(lenocc(crmess).gt.0)then
         print *,'?FPARM: '//crmess
         stop
      endif
      call fermes(crmess,0)
      if(lenocc(crmess).gt.0)print *,'FPARM: '//crmess
      
      end

c----------------------------------------------------------------

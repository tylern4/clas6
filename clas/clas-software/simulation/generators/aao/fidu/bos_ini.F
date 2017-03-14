      subroutine bos_ini
      implicit none
c     
      character*132 fname       ! input bosfile name
      character*132 dname       ! output bosfile name
      character*132 ddl_file    ! ddl file name
c      
#include "bcs.inc"
c
      CALL bnames(1000)
      call bos(iw,nbcs)
      call revinm ('CLAS_PARMS','clasbanks.ddl',ddl_file)
      CALL txt2bos(ddl_file)
      close (32)
c    
c  
      CALL getenvf('REVB',fname)
      CALL FPARM('OPEN BOSINPUT UNIT=32 FILE="' // fname // '"')
      WRITE(6,22) fname
 22   FORMAT(' Opened Input file: ',A64)

      CALL getenvf('WDST',dname)
      CALL FPARM('OPEN BOSOUTPUT UNIT=33 FILE="' // dname // '" '//          
     1    'RECL=36000 WRITE STATUS=NEW')
      WRITE(6,23) dname
 23   FORMAT(' Opened Output file: ',A64) 
      
c     
      RETURN
      END

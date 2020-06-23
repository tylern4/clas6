C
C
C       data initialization for common blocks is in phas_init
C
	  integer jin,jout,lout,linput,jhist
	  parameter (jhist=34)
	  parameter (linput=5)
	  parameter (lout=6)
C
C     jin,jout     =       discripters for input and output BOS Files
C     lout         =       unit number of list file
C     linput       =       unit number of option specification file
C
C
      logical list, decay, edit,ltagr
      CHARACTER*80 LNAM,INAM,HNAM,txnam
      INTEGER hout,bin,bout,runnumber,txout
      COMMON /parm2/list,decay,edit,ltagr,LNAM,INAM,HNAM,TXNAM,hout,bin,
     +bout,txout,jin,jout,runnumber
C
C       INAM        =   Input data file name (='NO' if no input file)      
C       LNAM        =   Output data file name      
C       HNAM        =   Output histo-Ntuple file name      
C       lstevnt     =   'list' will print all events
C       decayev     =   decay particles before writing out
C       evedit      =   clean JETSET record from decaied particles

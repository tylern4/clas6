

      character*10 MaxEventNum
      integer*4 maxpages
      parameter ( maxpages= 100000)
      parameter ( MaxEventNum='100000000')
     
      integer*4 IQUEST
      integer*4 ipawc
      common /QUEST/IQUEST(100)
      common /pawc/ipawc(maxpages*128)



	integer ntupl_number	
      CHARACTER*100 file_input,file_inputnum
      COMMON /com_file_input/file_input,file_inputnum,ntupl_number

      real NGammaT, NGammaE, NGammaEnRebin1,NGammaEnRebin2
      common NGammaT(61), NGammaE(767), NGammaEnRebin1(225),NGammaEnRebin2(90)





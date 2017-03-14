      integer MAXpoints, ibad(2,100), nbad, datform
      parameter (MAXpoints=100000)
      character*60 fname(4),parofile
      integer point_stat(MAXpoints)
      
      common/janr_data/ibad,nbad,datform,fname,parofile

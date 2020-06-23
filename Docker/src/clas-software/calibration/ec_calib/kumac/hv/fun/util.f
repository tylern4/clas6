      subroutine write(lun,str1,str2,str3)
      
      integer lun,len1,len2,len3
      character*(*) str1,str2,str3
      character*80 string
      
      len1 = lenocc(str1)
      len2 = lenocc(str2)
      len3 = lenocc(str3)
      
      string = str1(1:len1)//' '//str2(1:len2)//' '//str3(1:len3)

      len = lenocc(string)
      write(lun,*) string(1:len)
      
      end
      
      

       subroutine fort_open()

       open(7,file="lastbin.output",status="new")

       return
       end

       subroutine fort_close()

       close(7)

       return
       end

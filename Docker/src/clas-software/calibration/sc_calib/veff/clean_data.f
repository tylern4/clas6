      subroutine clean_data(p1,p2,cut)
      real p1,p2
      real cut,ymin,ymax,yave
      integer maxnpts,npts
      parameter(maxnpts=100000)
      real pos(maxnpts),time(maxnpts),weight(maxnpts)
      common/veff_data/time,pos,weight,npts,ymin,ymax,yave
      integer i,j
      real fit_time
      real temp_time(maxnpts),temp_pos(maxnpts),temp_weight(maxnpts)

      j=0
      do i=1,npts
         fit_time=p1+pos(i)/p2
	 if (abs(fit_time-time(i)).lt.cut) then
            j=j+1
	    temp_pos(j)=pos(i)
	    temp_time(j)=time(i)
	    temp_weight(j)=weight(i)	
c         write(6,*)pos(i),time(i),weight(i)
	 endif
      enddo
      npts=j
      do i=1,npts
	 time(i)=temp_time(i)
	 pos(i)=temp_pos(i)
	 weight(i)=temp_weight(i)
c         write(6,*)pos(i),time(i),weight(i)
      enddo
      return
      end

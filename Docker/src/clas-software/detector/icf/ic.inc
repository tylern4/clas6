       INTEGER length_ic,dbtclflag
       PARAMETER (length_ic = 529)
       real deep,w0,xstep,ystep,zt0
       real ehitmin,egrplmin,egrpmin
       real*8 ehitmind,egrplmind,egrpmind,z0d 
       real encorr(3,16)
       real dcorr(3,16)	    
       common/recon/deep,ehitmin,egrplmin,egrpmin,w0,xstep,ystep,zt0,
     +encorr,dcorr,ehitmind,egrplmind,egrpmind,z0d

      integer maxST
      parameter (maxST=24)
      integer nST
      integer secST
      integer idST
      integer TDCST
      integer ADCST

*
      common /STnt/ nST
     &,secST(maxST)
     &,idST(maxST)
     &,TDCST(maxST)
     &,ADCST(maxST)



	integer nSTR
	integer idSTR
	real    STR_t
	real    STR_L
	real    STR_z
	integer STR_trk
	integer STRstat

	common /STRnt/ nSTR
     &,	idSTR(maxST)
     &, STR_trk(maxST)
     &,	STR_t(maxST)	
     &, STR_L(maxST)
     &, STR_z(maxST)
     &, STRstat(maxST)

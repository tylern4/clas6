c Energy and TIme for each module
      Integer MaxHits,MaxGroup
      PARAMETER (MaxHits=500)
      PARAMETER (MaxGroup=20)
      Integer nhit, ngroup, nmember(MaxGroup),groupmember(MaxGroup,MaxHits),nmix
      Integer ixhit(MaxHits), iyhit(MaxHits), ntime(MaxHits),hitstat(MaxHits)
      Real Eicm(MaxHits), Ticm(MaxHits),xhp(MaxHits),yhp(MaxHits)
      Real Eicg(MaxGroup), Ticg(MaxGroup), m3x(MaxGroup),m3y(MaxGroup)
      Real xhg(MaxGroup), yhg(MaxGroup), zhp(MaxHits),xwg(MaxGroup),ywg(MaxGroup)
      real fre(MaxGroup,MaxHits),NextHit(MaxHits),ClosestHit(MaxHits)
      Common/ichit/nhit,hitstat,ngroup,ixhit,iyhit,eicm,ticm,xhp,yhp,zhp,
     +eicg,ticg,nmember,groupmember,xhg,yhg,xwg,ywg,m3x,m3y,fre,nmix,NextHit,ClosestHit
c

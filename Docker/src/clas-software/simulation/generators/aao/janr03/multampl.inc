       INTEGER Maxmpoints
       PARAMETER (Maxmpoints=101)
       REAL Wtab(Maxmpoints)
       REAL AEm(Maxmpoints,6,3),AEp(Maxmpoints,6,3)   
       REAL ASm(Maxmpoints,6,3),ASp(Maxmpoints,6,3)   
       REAL AMm(Maxmpoints,6,3),AMp(Maxmpoints,6,3)  
       REAL RAEm(Maxmpoints,4,3),RAEp(Maxmpoints,4,3)   
       REAL RASm(Maxmpoints,4,3),RASp(Maxmpoints,4,3)   
       REAL RAMm(Maxmpoints,4,3),RAMp(Maxmpoints,4,3)  
       COMMON/MULTampl/Wtab,AEm,AEp,ASm,ASp,AMm,AMp,
     +     RAEm,RAEp,RASm,RASp,RAMm,RAMp
       save





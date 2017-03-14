       INTEGER MAX1res
       PARAMETER (MAX1res=13)
       INTEGER MAX3res
       PARAMETER (MAX3res=13)
       INTEGER MAX13res
       PARAMETER (MAX13res=40)
       REAL AM3,AE3,AS3  ! 
       REAL AM1,AE1,AS1  ! 
       REAL aa3,aa1,sa1
       REAL BreitE3,BreitM3,BreitS3
       REAL BreitE1,BreitM1,BreitS1,resM
       COMMON/AMPLITUDES/BreitE3(2,MAX3res),BreitE1(2,2,MAX1res),
     +      BreitM3(2,MAX3res),BreitS3(2,MAX3res),
     +      BreitM1(2,2,MAX1res),BreitS1(2,2,MAX1res),resM,
     +      AM3(MAX3res),AE3(MAX3res),AS3(MAX3res),
     +      AM1(2,MAX1res),AE1(2,MAX1res),AS1(2,MAX1res),
     +      aa3(50),aa1(50),sa1(50)





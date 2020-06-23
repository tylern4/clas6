c  lcmat.inc

c  $Id: lcmat.inc,v 1.2 1997/04/24 15:38:48 holtrop Exp $

      character*132 cilcmat
      data cilcmat/
     1'$Id: lcmat.inc,v 1.2 1997/04/24 15:38:48 holtrop Exp $'
     1/


ceb
c    INCLUDE file
c
c    purpose: take the material number out of the code
c    author : E.Burtin 01.24.95 
c
	integer      mat_carbo, mat_h2gas, mat_hegas
        integer      mat_mylar,  mat_areth, mat_plast
        integer      mat_leadg,  mat_freon, mat_stain
        
        common / materials /  mat_carbo, mat_h2gas, mat_hegas,
     +                 mat_mylar,  mat_areth, mat_plast,
     +                 mat_leadg,  mat_freon, mat_stain


############# SUN
f77 = f77

############# HP
# f77 = fort77 +ppu -K

############ FLAG pour saclay
FFLAG = -e 

############ FLAG cebaf sun

#FFLAG = -e 

############ FLAG cebaf HP
#FFLAG = +es +e 

# for debugging add -g -C

#c############  cebaf SUN  -----------
LIBDIRS = -L/$(CLAS_LIB)
LIBS = -lmapmanager 


NAME = tag_get_map

SRC = $(NAME).F
OBJ = $(NAME).o


MAIN = tag_get_map

#UTILS = 
#LISTING= -V

$(OBJ) : $(SRC)
	$(f77) $(FFLAG) $(LISTING) -c $(SRC)

$(NAME) : $(OBJ)
	$(f77) $(FFLAG) -o $(NAME) $(OBJ) \
	 $(LIBDIRS) $(LIBS) $(SPELIBS)







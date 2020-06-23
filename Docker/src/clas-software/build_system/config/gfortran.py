########################################################################
# gfortran.py
# gfortran library configuration file
# Author: Johann Goetz
#
#   default_libs(env)
#       checks what version of fortran compiler is being used.
#       if g77, then we link to g2c. Otherwise its gfortran.
#
########################################################################

def default_libs(env):
    if env['FORTRAN'] == 'g77':
        libs = ['g2c']
    else:
        libs = ['gfortran']
    return libs

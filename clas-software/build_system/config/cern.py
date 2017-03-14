from os import path

def dependency_type():
    return 'binary_only'

#def default_lib_dir():
#    return ''

def default_libs(env):
    ret = [env.File(path.join(env.Dir(env['cernlib']).abspath,env['LIBPREFIX']+x+env['LIBSUFFIX'])) for x in 'pawlib mathlib graflib grafX11 packlib kernlib'.split(' ')]
    ret += 'lapack m X11 nsl crypt dl'.split(' ')
    return ret

def gsim_libs(env):
    ret = [env.File(path.join(env.Dir(env['cernlib']).abspath,env['LIBPREFIX']+x+env['LIBSUFFIX'])) for x in 'geant321 pawlib mathlib graflib grafX11 packlib kernlib'.split(' ')]
    ret += 'Xbae Xm Xaw lapack m Xt X11 nsl crypt dl'.split(' ')
    return ret


#def load_flags(env):
#   return ['-Wl-static']

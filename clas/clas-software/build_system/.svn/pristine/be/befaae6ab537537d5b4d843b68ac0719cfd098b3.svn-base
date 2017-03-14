########################################################################
# clas6.py
# CLAS6 library configuration file
#
# Methods:
#   required_dependencies()
#       returns a list or dictionary of the dependencies
#       required by this library.
#
#   default_libs(env)
#       returns a list of the libraries most commonly used by
#       general clas software.
#
#   clasEvent_libs(env)
#       returns a list of libraries associated with clasEvent.
#       to use this you must also include the defaults and
#       order is important:
#           env.load_dependency(
#               {'clas' : ['clasEvent_libs', 'default_libs']})
#
#   load_flags(env)
#       Configures environment (env) to compile clas software
#       by loading FORTRAN compile-time flags and a C pre-processor
#       define from os.uname()[0]
#
########################################################################

import os

import SCons.Variables

from build_system import variables

def required_dependencies():
    return 'cern mysql tcl gfortran'.split()

def init(env):
    try:
        dir = os.environ['CLAS6_BUILD_SCRIPTS']
    except:
        try:
            dir = os.path.join(os.environ['CLAS6'], 'scripts', 'build')
        except:
            dir = '#scripts/build'
    env['vars'].Add(SCons.Variables.PathVariable(
        'clas6scrdir',
        help = '''\
            build scripts for clas package
            that are associated with the build.
            ''',
        default = dir,
        validator = SCons.Variables.PathVariable.PathAccept))

    ##
    # override the shallowheaders default value to True.
    ##
    variables.modify(env, 'shallowheaders', default=True)
    variables.modify(env, 'shared', default=False)

def load_variables(env):
    env['clas6ScrDir'] = env['clas6scrdir']

    return [('clas6ScrDir', env['clas6ScrDir'])]

def default_libs(env):

    return '''
        c_cern eloss online_dummy pid scaler seb tagM trk vertex scat
        cc dc ec lac sc tag st icf sh
        bankdefs c_bos_io recutl itape
        caldbMap caldbC clasutil
        bosio
        stdc++ pthread m dl c stdc++ z
        '''.split()

def recsis_libs(env):
    return '''
        recsis epics gem user_ana ana
        c_sql
        pthread m dl c z
        '''.split()

def kinfit_libs(env):
    return 'KinFit ClasRuns'.split()

def gsim_libs(env):
    return 'gsim nsl patches blas mathlib X11 Xm'.split()

def clasEvent_libs(env):
    return '''
        clasEvent
        g6pcor g10pcor Pcor
        plib pp pwaUtil
        '''.split()

def ClasTool_libs(env):
    import root

    return '''
        CLASCorrections
        EventSelector
        Filter
        PartSieve
        FillBanks
        ClasTool
        NT10Reader
        DSTReader
        VirtualReader
        ClasBanks
      '''.split() + root.default_libs(env)



def load_flags(env):
    flags = '''
        -fPIC
        -fno-automatic
        -ffixed-line-length-none
        -fno-second-underscore'''.split()
    if env['FORTRAN'] == 'gfortran':
        flags.append('-fno-range-check')
    if env['FORTRAN'] == 'g77':
        flags.append('-finit-local-zero')
    env.AppendUnique(FORTRANFLAGS = flags)

    defines = [os.uname()[0]]
    if defines[0] == 'Darwin':
        defines += ['Linux']
    env.AppendUnique(CPPDEFINES = defines)
    env.AppendUnique(CPPFLAGS = '-fPIC'.split())
    env.AppendUnique(CFLAGS   = '-fPIC'.split())


    return (flags, defines)

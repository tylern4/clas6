########################################################################
# _template.py
# _template library configuration file
# Author: Johann Goetz
#
# This is a template for any compiled or header-only library.
# All methods are optional and are called only by ../dependency.py
# If any method does not exist, dependency.py will catch the exception
# and use the default value(s) indicated below, in the order shown.
# Note that <name-of-library> is a string that is the same as the
# file name in the config directory.
#
# IMPORTANT: it is better to comment out (or delete) the functions
# that are not used. This allows ../dependency.py to use try/except
# blocks to determine default values. If for some reason any default
# value cannot be determined in these methods, they should raise an
# Exception().
#
# Methods:
#
#   required_dependencies()
#       returns a list of strings indicating which libraries
#       should be brought in because they are use by this library.
#
#   dependency_type()
#       returns a string indicating what type of library this is.
#       The string should be one of:
#           'header_and_binary' (default)
#           'header_only'
#           'binary_only'
#
#   default_include_dir()
#       returns a string which is the path to the header files of
#       this library.
#       defaults:
#           os.environ[<name-of-library>.upper() + 'INC']
#           env['incdir']
#
#   default_lib_dir()
#       returns a string which is the path to the precompiled archive
#       files of this library.
#       defaults:
#           os.environ[<name-of-library>.upper() + 'LIB']
#           env['libdir']
#
#   default_libs(env)
#       returns a list of the libraries most commonly used from
#       this library. default is a single library: [<name-of-library>]
#       The SCons.Environment which is passed to function can be
#       used to determine the compiler/version which might affect
#       the default list of libraries.
#
#   <package>_libs(env)
#       does the same as default_libs(env) but can be used to bring
#       in several archives with one specification. change "<package>"
#       to an appropriate name. There can be any number of these
#       methods.
#
#   load_flags(env)
#       Configures the SCons.Environment (env) to compile
#       software by loading compile-time flags and C pre-processor
#       defines. Default is to do nothing. The return value must
#       be False or a tuple of two lists: (flags, defines)
#
########################################################################

def dependency_type():
    return 'header_and_binary'

def required_dependencies():
    return []

def init(env):
    env['vars'].Add(SCons.Variables.PathVariable(
        '_templatescrdir',
        help = 'build scripts for _template package' \
        + ' that are associated with the build.',
        default = '#build_scripts',
        validator = SCons.Variables.PathVariable.PathAccept))
    env['useplatform'] = True
    env['vars'].Update(env)

def load_variables(env):
    env['_templateScrDir'] = env['_templatescrdir']

def default_include_dir():
    return ''

def default_lib_dir():
    return ''

def default_libs(env):
    return []

#def <package>_libs(env):
#    return []

def load_flags(env):
    return False

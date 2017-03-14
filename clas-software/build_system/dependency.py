import os

import SCons.Script
import SCons.Variables

import config

from stylize import infostr, tgtstr, tgt2str, srcstr, src2str, alertstr

def add_variables(env, deps):
    add_variables.added_list = []

    if type(deps) is str:
        deps = deps.split()
    else:
        deps = SCons.Script.Flatten([deps])

    add_variables.added_list = [d for d in deps]

    for dep in deps:
        try:
            req_deps = eval('config.'+dep+'.required_dependencies()')
            for req_dep in req_deps:
                if req_dep in add_variables.added_list:
                    req_deps.remove(req_dep)
            add_variables(env, req_deps)
        except:
            pass

        try:
            eval('config.' + dep + '.init(env)')
        except:
            pass

        try:
            deptype = eval('config.' + dep + '.dependency_type()')
        except:
            deptype = 'header_and_binary'

        if deptype in ['header_and_binary', 'header_only']:
            add_incdir_variable(env, dep)

        if deptype in ['header_and_binary', 'binary_only']:
            add_libdir_variable(env, dep)

    env['vars'].Update(env)

def add_incdir_variable(env, dep):
    try:
        idir = eval('config.' + dep + '.default_include_dir(env)')
    except:
        try:
            idir = eval('config.' + dep + '.default_include_dir()')
        except:
            try:
                idir = os.environ[dep.upper()+'INC']
            except:
                try:
                    idir = os.path.join(
                        os.environ[dep.upper()], 'include')
                except:
                    idir = None #env['incdir']
    env['vars'].Add(SCons.Variables.PathVariable(
        dep+'inc',
        help = 'Include directory for "' \
             + dep + '" dependency.',
        default = idir,
        validator = SCons.Variables.PathVariable.PathAccept))

def add_libdir_variable(env, dep):
    try:
        libdir = eval('config.' + dep + '.default_lib_dir(env)')
    except:
        try:
            libdir = eval('config.' + dep + '.default_lib_dir()')
        except:
            try:
                libdir = os.environ[dep.upper()+'LIB']
            except:
                try:
                    libdir = os.path.join(
                        os.environ[dep.upper()], 'lib')
                except:
                    libdir = None #env['libdir']
    env['vars'].Add(SCons.Variables.PathVariable(
        dep+'lib',
        help = 'Library directory for "' \
             + dep + '" dependency.',
        default        = libdir,
        validator = SCons.Variables.PathVariable.PathAccept))

def load(env, deps, replace=False):
    # split a string into a list
    # or, if deps is a list, flatten it.
    # do nothing if deps is a dict
    libs_to_load = []
    if type(deps) is str:
        libs_to_load = [(x,None) for x in deps.split()]
    elif not type(deps) is dict:
        deps = SCons.Script.Flatten([deps])
        libs_to_load = [(x,None) for x in deps]
    else:
        for dep in deps:
            libs_to_load += [(dep, deps[dep])]

    #print libs_to_load

    # we don't need to load libs that have already
    # been loaded so we create a list of what is
    # loaded
    #
    # however, if replace==True, we start with an
    # empty library list.
    if replace:
        env.Replace(LIBS=[])
    else:
        for dep, liblist in libs_to_load:
            try:
                is_loaded = env[dep+'_loaded_defaults']
                deps.remove(dep)
            except:
                env[dep+'_loaded_defaults'] = True

    # get the required deps for each dependency
    # in the list (or dict) 'deps'
    req_deps = required_dependencies([dep for dep, liblist in libs_to_load])
    #print 'req_deps:', req_deps
    #print 'deps:', [dep for dep, liblist in libs_to_load]
    #print 'req_deps:', req_deps

    # for each required dep, if its not specified in the
    # original deps list, add it using default libs.
    # if deps is a list, prepend the required dep
    # to the dependency list.
    for rd in req_deps:
        if not rd in [dep for dep, liblist in libs_to_load]:
            #print 'adding:', rd
            libs_to_load = [(rd,None)] + libs_to_load

    # setup a list for the loaded dependencies
    loaded = {}
    for dep, liblist in libs_to_load:
        loaded[dep] = {}

    for dep, liblist in libs_to_load:
        try:
            loaded[dep]['type'] = \
                eval('config.' + dep + '.dependency_type()')
        except:
            loaded[dep]['type'] = 'header_and_binary'

        if loaded[dep]['type'] in ['header_and_binary', 'header_only']:
            # actually load the include directories to the environment
            loaded[dep]['incdir'] = load_include_dir(env, dep)

        if loaded[dep]['type'] in ['header_and_binary', 'binary_only']:
            # actually load the libdir and libs into the environment
            loaded[dep]['libdir'] = load_lib_dir(env, dep)
            #print 'loading:', dep
            loaded[dep]['libs']   = load_libs(env, dep, liblist)

        loaded[dep]['flags'] = load_flags(env, dep)

        try:
            loaded[dep]['vars'] = \
                eval('config.' + dep + '.load_variables(env)')
        except:
            loaded[dep]['vars'] = None

    if env['verbose'] > 3:
        print_loaded_dependencies(loaded)

def required_dependencies(deps):
    '''
        gets the list of required libraries in order to use
        the dependency passed to it.
    '''
    req_deps = []
    for dep in deps:
        try:
            req_deps = eval('config.'+dep+'.required_dependencies()')
            req_deps.extend( required_dependencies(req_deps) )
        except:
            pass
    # return a list of unique dependencies
    return list(set(req_deps))

def load_include_dir(env, dep):
    try:
        incdir = env[str(dep)+'inc']
    except:
        incdir = env['buildIncDir']
    incdir = env.Dir(incdir)
    env.AppendUnique(
        PATH = [incdir],
        CPPPATH = [incdir],
        FORTRANPATH = [incdir])
    #for x in 'PATH CPPPATH FORTRANPATH'.split():
    #    env[x] = env[x][:-2]+[env[x][-1]]+[env[x][-2]]
    return incdir

def load_lib_dir(env, dep):
    try:
        libdir = env[str(dep)+'lib']
    except:
        libdir = env['buildLibDir']
    libdir = env.Dir(libdir)
    env.AppendUnique(LIBPATH = [libdir])
    return libdir

def load_libs(env, dep, liblist):
    '''
        dep is the specific dependency to load
        liblist is the list of libs to load
            a value of None means use default
            an empty list [] means use no libs
    '''
    libs = [False]
    try:
        # try to get the list from the deps dict object
        libs = SCons.Script.Flatten([liblist])
        if None in libs:
            raise Exception('no libs given. using defaults...')
        libs_to_remove = []
        libs_to_add = []
        for lib in libs:
            # see if any lib groups are specified (ending in _libs)
            if lib[-5:] == '_libs':
                # no such library as <name>_libs (we want to replace
                # this with the list of actual libs from the function
                # config.<name>.lib(env)
                libs_to_remove += [lib]
                try:
                    libs_to_add += eval('config.'+dep+'.'+lib+'(env)')
                except:
                    # by default we try to link to a library called <name>
                    libs_to_add = [lib[:-5]]
        # remove the libs that need removing
        [libs.remove(x) for x in libs_to_remove]
        # add the libs that need to be added.
        libs.extend(libs_to_add)
    except:
        try:
            # get the default list of libraries for this dependency 
            libs = eval('config.' + dep + '.default_libs(env)')
        except:
            # no list of libs was given, and there is no default known
            # so assume there is a with the name of the dependency
            libs = [dep]

    # add the libraries to the environment
    env.PrependUnique(LIBS = libs)

    # return the list of libraries added to the environment
    return libs

def load_flags(env, dep):
    try:
        flags, defines = eval('config.' + dep + '.load_flags(env)')
        return (flags, defines)
    except:
        return False

def print_loaded_dependencies(loaded):
    for dep in loaded:
        print infostr('  loaded external library:'), tgt2str(dep)
        if loaded[dep]['type'] in ['header_and_binary', 'header_only']:
            print infostr('    include dir:'), \
                srcstr(loaded[dep]['incdir'])
        if loaded[dep]['type'] in ['header_and_binary', 'binary_only']:
            print infostr('    library dir:'), \
                srcstr(loaded[dep]['libdir'])
            print infostr('    libraries:'), \
                srcstr(loaded[dep]['libs'])
        if loaded[dep]['flags']:
            print infostr('    flags:'), \
                srcstr(loaded[dep]['flags'][0])
            print infostr('    defines:'), \
                srcstr(loaded[dep]['flags'][1])
        if loaded[dep]['vars']:
            for var, val in loaded[dep]['vars']:
                print infostr('    '+var+':'), srcstr(val)


def ignore_library_order(env):
    env['LINKCOM'] = env['LINKCOM'].replace("$_LIBDIRFLAGS","-Wl,--start $_LIBDIRFLAGS")
    env['LINKCOM'] = env['LINKCOM'].replace("$_LIBFLAGS","$_LIBFLAGS -Wl,--end")
    env['SHLINKCOM'] = env['SHLINKCOM'].replace("$_LIBDIRFLAGS","-Wl,--start $_LIBDIRFLAGS")
    env['SHLINKCOM'] = env['SHLINKCOM'].replace("$_LIBFLAGS","$_LIBFLAGS -Wl,--end")


########################################################################
# python module: build_system.build
# This module contains a few wrapper functions for the creation
# of libraries and programs, and for calling sconscript files
# consistently.
#
# see documentation for the methods listed below. Private methods
# are meant not to be called directly. The public methods should all
# be folded into the SCons.Environment class and should be called
# as a member function of and environment, i.e. env.program()
#
# public methods:
#   sconstruct()
#   library()
#   jar()
#   program()
########################################################################

import os

import SCons.Script

from stylize import infostr, tgtstr, tgt2str, srcstr, src2str, alertstr

_extensions = {
    'src' : [
        'cpp',
        'cc',
        'c',
        'f',
        'F',
        'f77',
        'f90',
        'f95' ],
    'headers' : [
        'hpp',
        'hh',
        'h',
        'inc',
        'PAR',
        'CMN' ]
}
_ignore_dirs = ['.svn']

#######################################################################
# sconstruct(dirs)
#   dirs is a list of directories
#   This is a wrapper for the SConscript function. It will call
#   the sconstruct file in each of the dirs specified in turn.
#   Usually this will call a directory that contains a
#   program (but no static_libraries).
#
#   example: if there were directories called prog0 and prog1 that
#   each contain any number of programs you could call:
#   env.sconscript(['prog0','prog1'])
#
#   and prog0/sconstruct and prog1/sconstruct files would be
#   read into the scons dependency tree.
#######################################################################
def sconstruct(env, dirs):
    '''sconstruct

    calls SConscript method on sconstruct files in directories
    listed in dirs.

    env
        The SCons environment.

    dirs
        A list of strings respresenting the directories which
        the SConscript method should be called in.
    '''
    dirs = SCons.Script.Flatten([dirs])
    scons_files = [os.path.join(d, 'sconstruct') for d in dirs]
    ret = {}
    for f in scons_files:
        if env['verbose'] > 3:
            print infostr('reading SConstruct file:'), src2str(f)
        env.SConscript(f,
            exports = {
                'env' : env,
                'ret' : ret } )
    return ret

def command(
 env,
 target,
 source,
 action,
 output_type='source',
 executable=None,
 project_name='this'):
    '''command

    runs a Command action. depending on the type of output specified,
    it will place the target file(s) into the scripts, bin, or obj
    directory.

    env
        The SCons environment.

    target
        output files.

    source
        a list of files to call Command on.

    action
        can be an external command, specified as a string, a
        a callable Python object, or script.

    output_type
        a string sppecifying the output type. Acceptible types include:
        'script', 'executable', 'binary', 'source'. This field is not
        case sensitive and keys off of the first three letters only.
        Defaults to 'source'.

    executable
        name of the executable script. Should be used when the
        command is not the actual executable.

    project_name
        name of project the script is associated with, defaults to 'this'.
    '''
    target, src = find_target_and_source(env, target, source)
    source = src['source']
    if not executable:
        executable = action.split()[0]
    executable = find_executable(env, executable, project_name)

    exec_base = os.path.basename(executable)
    if exec_base in action.split():
        action = action.replace(exec_base, executable, 1)

    build_obj_dir = build_obj_directory(env)

    source = [os.path.join(build_obj_dir, str(s)) for s in source]

    target = SCons.Script.Flatten([target])
    if output_type[:3].lower() in ['scr']:
        target = [os.path.join(env['buildScrDir'], t) for t in target]
        env.Install(env['scrDir'], target)
    elif output_type[:3].lower() in ['bin', 'exe']:
        target = [os.path.join(env['buildBinDir'], t) for t in target]
        env.Install(env['binDir'], target)
    else:
        target = [os.path.join(build_obj_dir, t) for t in target]

    ret = env.Command(
        target = target,
        source = source,
        action = action )

    env.Depends(ret, executable)

    return ret


def pre_build(env):
    if env['alignbits'] != 'native':
        align_bits = '-m'+env['alignbits']
        env.AppendUnique(
            FORTRANFLAGS = [align_bits],
            CPPFLAGS     = [align_bits],
            LINKFLAGS    = [align_bits] )
        if env['platformName'][:3] in ['lin', 'dar']:
            #if env['alignbits'] == '32':
            #    env.AppendUnique(LIBPATH = ['/usr/lib'])
            if env['alignbits'] == '64':
                env.AppendUnique(LIBPATH = ['/usr/lib64'])

def library(
 env,
 target   = None,
 source   = None,
 ignore   = None,
 headers  = ['.'],
 lib_type = 'both'):
    '''library

    creates a C/C++/FORTRAN library (static, shared, or both) from
    given source or source files found in the current directory.

    env
        The SCons.Environment
    target
        a string specifying the name of the target
        if type None (default) it will be determined
        from source files in find_target_and_source() method
    source
        list of source files (strings or SCons Nodes)
        if type None (default) SCons will Glob for all
        source-type files in the current directory.
    ignore
        list of source and header files to ignore if and only
        if source or header is not specified respectively.
    headers
        list of headers passed to install_headers method
    lib_type
        'both' (default)
        'shared'
        'static'
    '''
    pre_build(env)

    if not lib_type in ['both', 'static', 'shared']:
        raise Exception(alertstr('''types of libraries may only be one of:
            static, shared, both
            ("'''+lib_type+'''" was given for library "'''+target+'''")'''))

    env.PrependUnique(
        PATH = [env.Dir('.')],
        CPPPATH = [env.Dir('.')],
        FORTRANPATH = [env.Dir('.')] )

    target, src = find_target_and_source(env, target, source, ignore)

    build_obj_dir = build_obj_directory(env)

    if env['verbose'] > 1:
        print infostr('  reading in library:'), tgt2str(target)
        if env['verbose'] > 3:
            print infostr('    source:'),
            for t in ['source', 'static_objs', 'shared_objs']:
                try:
                    for s in src[t]:
                        print srcstr(s),
                except: pass
            print ''
            print infostr('    target directory:'),
            print srcstr(build_obj_dir)

    src['source'] = [os.path.join(build_obj_dir, str(s)) for s in src['source']]
    target = os.path.join(env['buildLibDir'], target)

    if not env['static']:
        if lib_type is 'both':
            lib_type = 'shared'
        else:
            lib_type = 'none'
    if not env['shared']:
        if lib_type is 'both':
            lib_type = 'static'
        else:
            lib_type = 'none'

    static_objs = []
    shared_objs = []

    if lib_type in ['both', 'static']:
        try:
            for s in src['source']:
                obj = env.StaticObject(s)
                if os.path.splitext(str(s))[-1] in ['.F','.Fpp','.fpp']:
                    fpp_incs = fpp_includes(env, env.File(s))
                    env.Depends(obj, fpp_incs)
                static_objs += obj
        except: pass
        try:
            static_objs += src['static_objs']
        except: pass
        if len(static_objs):
            static_lib = env.StaticLibrary(target, static_objs)
            env.Install(env['libDir'], static_lib)

    if lib_type in ['both', 'shared']:
        try:
            for s in src['source']:
                obj = env.SharedObject(s)
                if os.path.splitext(str(s))[-1] in ['.F','.Fpp','.fpp']:
                    fpp_incs = fpp_includes(env, env.File(s))
                    env.Depends(obj, fpp_incs)
                shared_objs += obj
        except: pass
        try:
            shared_objs += src['shared_objs']
        except: pass
        # WORKAROUND
        # shared libs created from shared objects
        # should not be linked to any other libraries
        # hence the LIBPATH=[], LIBS=[], RPATH=[]
        # this is might be an SCons bug
        if len(shared_objs):
            shared_lib = env.SharedLibrary(target, shared_objs,
                LIBPATH=[], LIBS=[], RPATH=[])
            env.Install(env['libDir'], shared_lib)

    install_headers(env, headers, ignore)

    ret = {
        'static_libs' : {},
        'shared_libs' : {} }
    if lib_type in ['both', 'static']:
        try:
            ret['static_libs'][str(static_lib[0])] = static_objs
        except: pass
    if lib_type in ['both', 'shared']:
        try:
            ret['shared_libs'][str(shared_lib[0])] = shared_objs
        except: pass
    return ret

def program(
 env,
 target=None,
 source=None,
 ignore=None,
 libs=None ):
    '''program

    builds a C/C++/FORTRAN program from the given source files
    or those source files in the current directory.

    env
        The SCons environment.

    target
        a string specifying the name of the program to be created.

    source
        a list of source files which SCons will compile into a program.
        The first item of the list must be the source containing main().
        If left unspecified, SCons will Glob for all appropriate source files.

    ignore
        a list of files to ignore.

    libs
        a list of strings specifying libraries the program is dependent on.

    #static
    #    link statically to libraries (default is False which uses the
    #    compiler's default behavior)
    '''
    pre_build(env)

    target, src = find_target_and_source(env, target, source, ignore)

    build_obj_dir = build_obj_directory(env)

    if env['verbose'] > 1:
        print infostr('  reading in program:'), tgtstr(target)
        if env['verbose'] > 3:
            print infostr('    source:'),
            for s in src['source']:
                print srcstr(s),
            print ''
            print infostr('    object directory:'), srcstr(build_obj_dir)

    sources = src['source']
    src['source'] = []
    for s in sources:
        if str(s)[:len(build_obj_dir[1:])] != build_obj_dir[1:]:
            src['source'] += [os.path.join(build_obj_dir, str(s))]
        else:
            src['source'] += [s]
    target = os.path.join(env['buildBinDir'], target)

    env.PrependUnique(
        PATH = [env.Dir('.')],
        CPPPATH = [env.Dir('.')],
        FORTRANPATH = [env.Dir('.')] )

    if libs:
        libs = SCons.Script.Flatten([libs])
        env.AppendUnique(LIBS = libs)

    static_objs = []
    try:
        for s in src['source']:
            static_objs += env.StaticObject(s)
    except: pass
    try:
        static_objs += src['static_objs']
    except: pass

    #if static:
    #    env.AppendUnique(LINKFLAGS = ['-static'])

    prog = env.Program(target, static_objs)
    env.Install(env['binDir'], prog)

    ret = {'prog' : {str(prog) : static_objs}}
    return ret

def install_headers(
 env,
 headers=['.'],
 ignore=None ):
    '''install_headers

    Installs the header files into the appropriate directories.

    env
        The SCons.Environment
    headers
        list of header files or directories containing header
        files. if type None then only header files in the
        current directory are seen and installed.
        directories specified, except for '.', will be
        recursively searched for any files.
        If you want the current working directory to be
        recursively searched, you may add:
            env.Dir('.').srcpath.abspath
    ignore
        list of headers to ignore.

    directories listed in _ignore_dirs will be ignored during
    walk into directories.
    '''
    build_obj_dir = build_obj_directory(env)
    build_inc_dir = build_inc_directory(env)
    install_inc_dir = install_inc_directory(env)

    headers = SCons.Script.Flatten([headers])

    if '.' in headers:
        headers.remove('.')
        headers_to_install = []
        for x in _extensions['headers']:
            headers_to_install += \
                env.Glob(os.path.join(build_obj_dir,'*.'+x))
        headers_to_install = SCons.Script.Flatten(headers_to_install)
        headers_to_install = ignore_files(headers_to_install, ignore)

        if len(headers_to_install):
            if env['verbose'] > 4:
                print '    headers:', [str(x) for x in headers_to_install]
                print '    into directory:', build_inc_dir, install_inc_dir
            env.Install(build_inc_dir, headers_to_install)
            env.Install(install_inc_dir, headers_to_install)

    for f in headers:
        headers_to_install = []
        remove_list = []
        if os.path.isdir(str(f)):
            remove_list += [f]
            for dirpath, dirnames, filenames in os.walk(str(f)):
                for igdir in _ignore_dirs:
                    try: dirnames.remove(igdir)
                    except: pass
                files = [os.path.join(dirpath, x) for x in filenames]
                if env['shallowheaders']:
                    bdir = build_inc_dir
                    idir = install_inc_dir
                else:
                    bdir = os.path.join(build_inc_dir, dirpath)
                    idir = os.path.join(install_inc_dir, dirpath)
                if env['verbose'] > 4:
                    print '    headers:', [str(x) for x in files]
                    print '    into directory:', bdir, idir
                env.Install(bdir, files)
                env.Install(idir, files)
        elif os.path.isfile(str(f)):
            try:
                dynamic_file = \
                    str(env.Glob(
                    os.path.join(build_obj_dir, str(f)) )[0])
                remove_list += [f]
                if env['shallowheaders']:
                    bdir = build_inc_dir
                    idir = install_inc_dir
                else:
                    bdir = os.path.join(build_inc_dir,
                        os.path.split(dynamic_file)[0])
                    idir = os.path.join(install_inc_dir,
                        os.path.split(dynamic_file)[0])
                env.Install(bdir, os.path.basename(dynamic_file))
                env.Install(idir, os.path.basename(dynamic_file))
            except:
                pass
        for r in remove_list:
            headers.remove(r)

    if len(headers):
        if env['verbose'] > 4:
            print '    headers:', [str(x) for x in headers]
            print '    into directory:', build_inc_dir, install_inc_dir
        env.Install(build_inc_dir, headers)
        env.Install(install_inc_dir, headers)

def install_scripts(env, nodes, recursive=True):
    nodes = SCons.Script.Flatten([nodes])
    files = []
    for node in nodes:
        f = str(node)
        if os.path.isdir(f):
            if not recursive:
                env.Install(env['buildScrDir'], f)
                env.Install(env['scrDir'], f)
            else:
                for dirpath, dirnames, filenames in os.walk(f):
                    for igdir in _ignore_dirs:
                        try: dirnames.remove(igdir)
                        except: pass
                    files += [os.path.join(dirpath, x) for x in filenames]
        elif os.path.isfile(f):
            files += [node]
        else:
            raise Exception(alertstr('can not install script: '+str(node)))
    env.Install(env['buildScrDir'], files)
    env.Install(env['scrDir'], files)

def jar(
 env,
 target,
 source=None,
 manifest=None):
    '''jar

    builds jar files from java source code.

    env
        The SCons.Environment
    target
        a string specifying the target jar file to be created.
        This is a required field. The '.jar' extension may be
        omitted (recommended).
    source
        list of source files (strings or SCons Nodes)
        if type None (default) the java compiler will use the
        current directory as the source-base for class files.
    manifest
        a string specifying the manifest file to be used in the jar.
        This manifest file MUST begin with 'Manifest-Version' to ensure
        SCons recognizes it.

    any jar files in the current directory will be automatically
    added to the JAVACLASSPATH SCons variable.
    '''

    if not target:
        raise Exception(alertstr('No target specified'))

    if not source:
        source = ['.']

    build_obj_dir = build_obj_directory(env)
    classdir = os.path.join(build_obj_dir, 'classes')
    target = os.path.join(env['buildJavaDir'], target)

    env.AppendUnique(JAVACLASSPATH=[str(x) for x in env.Glob('*.jar')])

    if env['verbose'] > 1:
        print infostr('  reading in library:'), tgt2str(target)
        if env['verbose'] > 3:
            print infostr('    classes directory:'), srcstr(classdir)

    class_files = env.Java(classdir, source)

    if manifest:
        class_files += [manifest]

    jar_file = env.Jar(target, class_files)

    env.Install(env['javaDir'], jar_file)

    return (jar_file, class_files)

def return_from_sconstruct(env, var):
    try:
        d = SCons.Script.Import('ret')['ret']
    except:
        return
    if type(var) is dict:
        for v in var:
            if v in d:
                d[v].update(var[v])
            else:
                d[v] = var[v]
    else:
        for vdict in var:
            for v in vdict:
                if v in d:
                    d[v].update(vdict[v])
                else:
                    d[v] = vdict[v]

def find_target_and_source(env, target, source, ignore=None):
    '''find_target_and_source

    for C/C++/FORTRAN programs and libraries, returns the name
    of the target and source files to be used based on the
    input parameters and the files in the current working directory.

    source
        can be of many forms. all lists will be flattened.
        1. source = None (default)
            source will be searched for in cwd
        2. a list of source files or nodes.
        3. a dict with specific keys:
            source
                list of source files or nodes
            static_objs
                list of static object files or nodes
            shared_objs
                list of shared object files or nodes
            static_libs
                must be a dict with keys being the library
                and data must be a list of static objects
            shared_libs
                must be a dict with keys being the library
                and data must be a list of shared objects
    target
        will be determined after source is completely determined.
        target can be:
        1. None
            it will be derived from source in this order:
                ### TODO: have first option be the file
                ### (source or object) with main() in it
                src['source'][0]
                src['static_objs'][0]
                src['shared_objs'][0]
        2. given explicitly as a string
        3. given as an SCons File object

    ignore
        a list of files to ignore. These should be basenames
        (without a directory prefix)

    return value
        (target<string>, src<dict>)
    '''
    src = {
        'source' : [],
        'static_objs' : [],
        'shared_objs' : [] }

    build_obj_dir = build_obj_directory(env)

    if not type(source) is dict:
        src['source'] += SCons.Script.Flatten([source])
        if None in src['source']:
            src['source'] = SCons.Script.Flatten([
                env.Glob(os.path.join(build_obj_dir,'*.'+x))
                for x in _extensions['src']])
    else:
        try:
            src['source'] += SCons.Script.Flatten([source['source']])
        except: pass
        for t in ['static', 'shared']:
            try:
                src[t+'_objs'] += SCons.Script.Flatten(
                    [source[t+'_objs']] )
            except: pass
            try:
                for lib in source[t+'_libs']:
                    src[t+'_objs'] += SCons.Script.Flatten(
                        [source[t+'_libs'][lib]] )
            except: pass
    ignore_files(src['source'], ignore)

    n_source = len(src['source'])
    n_obj = len(src['static_objs']) + len(src['shared_objs'])
    n_input = n_source + n_obj
    if n_input == 0:
        raise Exception(alertstr(
            'No source files found or specified.' ))

    if not target:
        for t in ['source', 'static_objs', 'shared_objs']:
            try:
                target = os.path.splitext(
                    os.path.basename(str(
                        src[t][0]
                    )))[0]
                break
            except: pass
    if not target:
        raise Exception(alertstr(
            'No target specified and no sources explicitly given.'))
    if type(target) is SCons.Node.FS.File:
        target = os.path.splitext(os.path.basename(str(target)))[0]

    return (target, src)

def ignore_files(file_list, ignore):
    '''ignore_files

    removes any files in list(file_list) with a basename the same
    as those in list(ignore).
    '''
    if ignore and len(ignore):
        ignore = SCons.Script.Flatten([ignore])
        ignore = [os.path.basename(str(x)) for x in ignore]
        remove_list = []
        for f in file_list:
            if os.path.basename(str(f)) in ignore:
                remove_list += [f]
        for r in remove_list:
            file_list.remove(r)
    return file_list

def find_executable(env, file, project_name=None):
    '''find_executable

    looks for an executable script in these directories:
        1. current directory
        2. build-scripts directory
        3. external-project's scripts directory
        4. the current environment's PATH

    returns a string of the found file-node.
    '''
    search_dir_list = [os.getcwd()]
    if project_name in ['this', env['projectName']]:
        search_dir_list += [env['buildScrDir']]
    else:
        try:
            search_dir_list += [env[project_name+'ScrDir']]
        except:
            pass
    search_dir_list += os.environ['PATH'].split(os.path.pathsep)
    search_dir_list = SCons.Script.Flatten(search_dir_list)

    ret = env.FindFile(file, search_dir_list)

    if not ret:
        print alertstr('could not find executable: '+file)
        print 'env[\'buildScrDir\']:', env['buildScrDir']
        if project_name != 'this':
            print 'env[\''+project_name+'ScrDir\']', env[project_name+'ScrDir']

    return str(env.File(ret).srcnode().abspath)

def build_obj_directory(env):
    '''build_obj_directory

    returns the build object directory path.
    Appends parent directories starting from where scons was called.

    env
        The SCons environment.
    '''

    cwd = env.Dir('.').srcnode().path
    if cwd != '.':
        return os.path.join(env['buildObjDir'], cwd)
    else:
        return env['buildObjDir']

def build_inc_directory(env):
    '''build_inc_directory

    returns the build include directory path.
    If shallowheaders is set to true, it will return the base build
    include directory. If false, it will append parent directories
    starting from where scons was called.

    env
        The SCons environment.
    '''

    cwd = env.Dir('.').srcnode().path
    if cwd == '.' or env['shallowheaders']:
        return env['buildIncDir']
    else:
        return os.path.join(env['buildIncDir'], cwd)

def install_inc_directory(env):
    '''install_inc_directory

    returns the install include directory path.
    If shallowheaders is set to true, it will return the base build
    include directory. If false, it will append parent directories
    starting from where scons was called.

    env
        The SCons environment.
    '''

    cwd = env.Dir('.').srcnode().path
    if cwd == '.' or env['shallowheaders']:
        return env['incDir']
    else:
        return os.path.join(env['incDir'], cwd)

# a fortran preprocessor scanner for #include statements
import SCons.Scanner
import SCons.Script

FppScanner = SCons.Scanner.ClassicCPP(
    "FppScan",
    ".",
    "CPPPATH",
    '^#include[ \t]*(<|")([^>"]+)(>|")')

def fpp_includes(env, node):
    node = env.File(node)
    path_list = env['CPPPATH']
    path = [env.Dir(x) for x in path_list]
    ret = []
    for x in FppScanner.scan(node,path):
        ret.append(env.File(x))
    return ret

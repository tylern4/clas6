import subprocess
import os.path

import SCons.Builder

def default_include_dir(env):
    cmd = env['rootconfig'] + ' --incdir'
    return subprocess.Popen(cmd,
        shell=True, stdout=subprocess.PIPE).communicate()[0][:-1]

def default_lib_dir(env):
    cmd = env['rootconfig'] + ' --libdir'
    return subprocess.Popen(cmd,
        shell=True, stdout=subprocess.PIPE).communicate()[0][:-1]

def default_libs(env):
    cmd = env['rootconfig'] + ' --glibs'
    root_config_glibs = subprocess.Popen(cmd,
        shell=True, stdout=subprocess.PIPE).communicate()[0][:-1].split()
    libs = []
    for l in root_config_glibs:
        if l[:2] == '-l':
            libs += [l[2:]]
    return libs

def init(env):
    env['vars'].Add(SCons.Variables.PathVariable(
        'rootconfig',
        help = 'executable to get the proper directories' \
        + ' and libraries to build root-based programs.',
        default = 'root-config',
        validator = SCons.Variables.PathVariable.PathAccept))
    env['vars'].Update(env)

    SCons.Util.AddMethod(SCons.Environment.Environment,
                         ROOTDictionary,
                         name = 'ROOTDictionary')

    ROOTDictionaryBuilder = SCons.Builder.Builder(
        action = 'rootcint -f $TARGET -c' \
        + ' $_CPPINCFLAGS $_CPPDEFFLAGS' \
        + ' $SOURCES',
        suffix = 'Dict.C' )
    env.Append(BUILDERS = {'ROOTDictionaryBuilder' : ROOTDictionaryBuilder})

def load_flags(env):
    cmd = env['rootconfig'] + ' --glibs --cflags'
    root_config_glibs_cflags = subprocess.Popen(cmd,
        shell=True, stdout=subprocess.PIPE).communicate()[0][:-1].split()

    flags = []
    for l in root_config_glibs_cflags:
        if not l[:2] in ['-L', '-l', '-I']:
            flags += [l]

    for f in ['-m32', '-m64']:
        if f in flags:
            flags.remove(f)

    env.AppendUnique(CPPFLAGS = flags)

    defines = []
    #env.AppendUnique(CPPDEFINES = defines)

    return (flags, defines)

def ROOTDictionary(env, target, sources):
    if env['buildObjDir'][0] == '#':
        buildObjDir = env['buildObjDir'][1:]
    else:
        buildObjDir = env['buildObjDir']

    return env.ROOTDictionaryBuilder(os.path.abspath(os.path.join(buildObjDir, target)), sources)


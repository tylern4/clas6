import sys, os
from subprocess import Popen, PIPE

import SCons.Environment
import SCons.Variables
import SCons.Script
import SCons.Job

from scons_overrides.SCons.Script import SConscript

import build
import dependency
import format
import optimization
import platform_string
import stylize
import variables

from stylize import infostr, tgtstr, srcstr, alertstr

def env(project_name=False, deps=[], build_script_dir=False):
    try:
        imports = SCons.Script.Import('env')
        env = imports['env']
        return env.Clone()
    except:
        return initialize(project_name, deps)

def add_methods(env):
    SCons.Util.AddMethod(SCons.Environment.Environment,
        dependency.load,
        name = 'load_dependency')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        dependency.ignore_library_order,
        name = 'ignore_library_order')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.sconstruct,
        name = 'sconstruct')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.command,
        name = 'command')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.library,
        name = 'library')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.program,
        name = 'program')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.install_headers,
        name = 'install_headers')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.install_scripts,
        name = 'install_scripts')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.jar,
        name = 'jar')
    SCons.Util.AddMethod(SCons.Environment.Environment,
        build.return_from_sconstruct,
        name = 'return_from_sconstruct')

def initialize(project_name, deps):
    try:
        tmp = initialize.run_once
        raise Exception(alertstr(str(initialize)+''' called twice.
            This should never happen.'''))
    except AttributeError:
        initialize.run_once = True

    optimization.init()

    env = SCons.Environment.Environment(ENV = {'PATH' : os.environ['PATH']})
    try:
        env.Append(ENV = {'LD_LIBRARY_PATH' : os.environ['LD_LIBRARY_PATH']})
    except:
        pass
    env.EnsurePythonVersion(2, 2)
    env.EnsureSConsVersion(1, 0)

    add_methods(env)

    if project_name:
        env['projectName'] = project_name
    else:
        env['projectName'] = False

    env['platformName'] = platform_string.platform_string(env)

    variables.set_config_file(env)
    variables.add(env)

    stylize.init(env)

    for tool in env['tools']:
        env.Tool(tool)


    if len(deps):
        dependency.add_variables(env, deps)

    env['verbose'] = int(env['verbose'])

    env['vars'].Save(env['configFile'], env)

    format.generate_help_text(env)
    format.cmd_output(env)

    variables.load(env)

    optimization.set_flags(env)
    set_install_targets(env)

    env.VariantDir(env['buildObjDir'], '.', duplicate=0)
    env['JAVACHDIR']=True

    if env['verbose'] in [1,2]:
        msg = infostr('evaluating targets')
        SCons.Script.Progress([
             '  -  ' + msg + '\r',
            '  \\  ' + msg + '\r',
             '  |  ' + msg + '\r',
             '  /  ' + msg + '\r'],
            interval = 1)

    proc = Popen('which gcc', env=os.environ, shell=True, stdout=PIPE, stderr=PIPE)
    gccpath = proc.communicate()[0].split('\n')[0]
    if os.path.normpath(gccpath[:8]) != os.path.normpath('/usr/lib'):
        gccrpath = os.path.split(gccpath)[0].replace('bin','lib')
        if env['alignbits'] in ['native','64']:
            gccrpath += '64'
        env.AppendUnique(RPATH=[gccrpath])

    return env

########################################################################
# allow for specifying 'install prog1 prog2'
#   to build and install 'prog1' and 'prog2' only,
#   disregarding all other targets.
########################################################################
def set_install_targets(env):
    env.Alias('libs', env['buildLibDir'])
    env.Alias('install-libs', env['libDir'])

    if 'install' in SCons.Script.BUILD_TARGETS:
        if len(SCons.Script.BUILD_TARGETS) > 1:
            SCons.Script.BUILD_TARGETS.remove('install')
            newtargets = []
            for target in SCons.Script.BUILD_TARGETS:
                newtargets.append(os.path.join(env['binDir'], str(target)))
            SCons.Script.BUILD_TARGETS = newtargets
            if env['verbose'] > 3:
                print infostr('will only build and install targets:'),
                print tgtstr(SCons.Script.BUILD_TARGETS)
    else:
        remove_tgts = []
        add_tgts = []
        for tgt in SCons.Script.BUILD_TARGETS:
            if not '/' == tgt[0]:
                if not tgt in 'libs install-libs'.split():
                    remove_tgts += [tgt]
                    add_tgts += [os.path.join(env['buildBinDir'], tgt)]
        for tgt in remove_tgts:
            SCons.Script.BUILD_TARGETS.remove(tgt)
        for tgt in add_tgts:
            SCons.Script.BUILD_TARGETS += [tgt]

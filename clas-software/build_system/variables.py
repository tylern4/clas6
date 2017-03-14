import os

import SCons.Variables

import format
from stylize import infostr, tgtstr, srcstr, alertstr

try:
    prefix = os.environ['PREFIX']
except KeyError:
    prefix = '/usr/local'

try:
    alignbits = os.environ['ALIGNBITS']
except KeyError:
    alignbits = 'native'

_defaults = {
    'build'     : 'standard',
    'builddir'  : 'build',
    'debug'     : False,
    'opt'       : 0,

    'prefix'         : prefix,
    'useplatform'    : False,
    'useprojectname' : False,
    'shallowheaders' : False,

    'static'    : True,
    'shared'    : False,
    'alignbits' : alignbits,

    'verbose'   : 1,

    'cleanconf' : False
}

def add(env):
    evars = env['vars']

    evars.Add(SCons.Variables.EnumVariable(
        'build',
        help           = 'type of build.',
        default        = _defaults['build'],
        allowed_values = ('standard','debug','opt')))
    evars.Add(SCons.Variables.ListVariable(
        'tools',
        help    = '''\
            comma seperated list of tools
            to use for compiling.''',
        default = [],
        names   = ['g77']))
    evars.Add(SCons.Variables.BoolVariable(
        'useplatform',
        help = 'Use platform dependent directory structure.',
        default = _defaults['useplatform']))
    evars.Add(SCons.Variables.BoolVariable(
        'useprojectname',
        help = 'Append project name to installation directories.',
        default = _defaults['useprojectname']))

    evars.Update(env)

    if env['build'] == 'debug':
        _defaults['debug'] = True
        _defaults['opt'] = 0
        _defaults['builddir'] = 'build_debug'
    elif env['build'] == 'opt':
        _defaults['debug'] = False
        _defaults['opt'] = 2
        _defaults['builddir'] = 'build_opt2'

    if env['useplatform']:
        _defaults['builddir'] = os.path.join(_defaults['builddir'], env['platformName'])

    evars.Add(SCons.Variables.BoolVariable(
        'debug',
        help    = 'build debug version.',
        default = _defaults['debug']))
    evars.Add(SCons.Variables.EnumVariable(
        'opt',
        help           = 'build optimization version.',
        default        = _defaults['opt'],
        allowed_values = ('0','1','2','3')))

    evars.Add(SCons.Variables.PathVariable(
        'builddir',
        help      = 'the base build directory.',
        default   = _defaults['builddir'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.PathVariable(
        'prefix',
        help = '''\
            the base directory to install
            all built files (libraries
            scripts, programs, etc).''',
        default = _defaults['prefix'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.BoolVariable(
        'shallowheaders',
        help = '''\
            Place all header files into a single directory.
            Note that this does not apply to directories passed
            to headers argument to library() and install_headers()
            methods.''',
        default = _defaults['shallowheaders']))
    evars.Add(SCons.Variables.BoolVariable(
        'static',
        help = '''\
            build all static library targets.
            ''',
        default = _defaults['static']))
    evars.Add(SCons.Variables.BoolVariable(
        'shared',
        help = '''\
            build all shared library targets.
            ''',
        default = _defaults['shared']))
    evars.Add(SCons.Variables.EnumVariable(
        'alignbits',
        help='''\
            force the alignment of a certain bit-length.
            This is usually the -m32 or -m64 option passed to gcc.
            ''',
        default = _defaults['alignbits'],
        allowed_values = ('native','32','64')))

    evars.Update(env)

    #env.Clean('.', env['builddir'])

    _defaults['execprefix'] = env['prefix']
    if env['useplatform']:
        _defaults['execprefix'] = os.path.join(_defaults['execprefix'], env['platformName'])

    _defaults['bindir'] = os.path.join(_defaults['execprefix'], 'bin')
    _defaults['libdir'] = os.path.join(_defaults['execprefix'], 'lib')
    _defaults['incdir'] = os.path.join(_defaults['execprefix'], 'include')
    _defaults['scrdir'] = os.path.join(_defaults['execprefix'], 'scripts')

    _defaults['javadir']= os.path.join(_defaults['prefix'],'java')

    if env['useprojectname'] and env['projectName']:
        _defaults['bindir'] = os.path.join(_defaults['bindir'], env['projectName'])
        _defaults['libdir'] = os.path.join(_defaults['libdir'], env['projectName'])
        _defaults['incdir'] = os.path.join(_defaults['incdir'], env['projectName'])
        _defaults['scrdir'] = os.path.join(_defaults['scrdir'], env['projectName'])
        _defaults['javadir'] = os.path.join(_defaults['javadir'], env['projectName'])

    evars.Add(SCons.Variables.PathVariable(
        'execprefix',
        help = 'the base directory to install' \
         + ' all platform-specific files (libraries,' \
         + ' programs, etc).',
        default = _defaults['execprefix'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.PathVariable(
        'bindir',
        help = 'directory where executable binaries will be installed',
        default = _defaults['bindir'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.PathVariable(
        'libdir',
        help = 'directory where libraries will be installed',
        default = _defaults['libdir'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.PathVariable(
        'incdir',
        help = 'directory where header files will be installed',
        default = _defaults['incdir'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.PathVariable(
        'scrdir',
        help = 'directory where executable scripts will be installed',
        default = _defaults['scrdir'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.PathVariable(
        'javadir',
        help = 'directory where java jar files will be installed',
        default = _defaults['javadir'],
        validator = SCons.Variables.PathVariable.PathAccept))

    _defaults['etcdir']  = False
    _defaults['datadir'] = False
    _defaults['docdir']  = False

    if env['projectName']:
        _defaults['etcdir']  = _defaults['execprefix']+'/etc/'+env['projectName']
        _defaults['datadir'] = _defaults['execprefix']+'/share/'+env['projectName']
        _defaults['docdir']  = _defaults['execprefix']+'/share/doc/'+env['projectName']

        evars.Add(SCons.Variables.PathVariable(
            'etcdir',
            help = 'directory where configuration files are installed.',
            default = _defaults['etcdir'],
            validator = SCons.Variables.PathVariable.PathAccept))
        evars.Add(SCons.Variables.PathVariable(
            'datadir',
            help = 'shared data installation directory.',
            default = _defaults['datadir'],
            validator = SCons.Variables.PathVariable.PathAccept))
        evars.Add(SCons.Variables.PathVariable(
            'docdir',
            help = 'documentation installation directory.',
            default = _defaults['docdir'],
            validator = SCons.Variables.PathVariable.PathAccept))

        _defaults['testdir'] = False
        _defaults['tutdir']  = False

        if _defaults['docdir']:
            evars.Update(env)

            _defaults['testdir'] = env['docdir']+'/test'
            _defaults['tutdir']  = env['docdir']+'/tutorial'

        evars.Add(SCons.Variables.PathVariable(
            'testdir',
            help = 'testing suite installation directory.',
            default = _defaults['testdir'],
            validator = SCons.Variables.PathVariable.PathAccept))
        evars.Add(SCons.Variables.PathVariable(
            'tutdir',
            help = 'tutorial installation directory.',
            default = _defaults['tutdir'],
            validator = SCons.Variables.PathVariable.PathAccept))

    evars.Add(SCons.Variables.EnumVariable(
        'verbose',
        help = 'set verbosity.',
        default = _defaults['verbose'],
        allowed_values = ('0','1','2','3','4','5')))

    evars.Add(SCons.Variables.PathVariable(
        'config',
        help = '''\
            Specify a different configuration file.
            This option is not saved and must be used
            everytime you want to use a config file different
            from the default.''',
        default = _defaults['config'],
        validator = SCons.Variables.PathVariable.PathAccept))
    evars.Add(SCons.Variables.BoolVariable(
        'cleanconf',
        help = '''\
            Remove saved configuration and exit.
            Afterwards, running scons without options
            will use defaults and environment variables.''',
        default = _defaults['cleanconf']))

    evars.Update(env)

    if env['cleanconf']:
        os.remove(env['configFile'])
        print 'removed configuration file: '+env['configFile']
        print 'Running "scons" after this will reset' \
         + ' all options to defaults and environment variables.'
        sys.exit(0)

def set_config_file(env):
    config_dir = 'build_config'
    _defaults['config'] = config_dir+'/'+env['platformName']+'.config'

    if 'config' in SCons.Script.ARGUMENTS:
        env['configFile'] = SCons.Script.ARGUMENTS['config']
    else:
        try: os.mkdir(config_dir)
        except: pass
        env['configFile'] = _defaults['config']

    env['vars'] = SCons.Variables.Variables(
        env['configFile'],
        args = SCons.Script.ARGUMENTS)

def load(env):
    load_build_dirs(env)
    load_install_dirs(env)

def load_build_dirs(env):
    if env['builddir'][0] != '/':
        env['buildDir'] = "#"+env['builddir']
    else:
        env['buildDir'] = env['builddir']

    env['buildBinDir']  = env['buildDir'] + '/bin'
    env['buildLibDir']  = env['buildDir'] + '/lib'
    env['buildIncDir']  = env['buildDir'] + '/include'
    env['buildScrDir']  = env['buildDir'] + '/scripts'
    env['buildJavaDir'] = env['buildDir'] + '/java'

    env['buildEtcDir']  = env['buildDir'] + '/etc'
    env['buildDataDir'] = env['buildDir'] + '/share'
    env['buildDocDir']  = env['buildDataDir'] + '/doc'

    env['buildTestDir'] = env['buildDocDir'] + '/test'
    env['buildTutDir']  = env['buildDocDir'] + '/tut'

    env['buildObjDir']  = env['buildDir'] + '/obj'

    incdir = env.Dir(env['buildIncDir'])
    libdir = env.Dir(env['buildLibDir'])
    env.PrependUnique(
        PATH = [incdir],
        CPPPATH = [incdir],
        FORTRANPATH = [incdir],
        LIBPATH = [libdir] )

    if env['verbose'] > 3:
        print_build_dirs(env)

def load_install_dirs(env):
    env['binDir'] = env['bindir']
    env['libDir'] = env['libdir']
    env['incDir'] = env['incdir']
    env['scrDir'] = env['scrdir']
    env['javaDir'] = env['javadir']
    env.Alias('install', [
        env.Dir(env['binDir']),
        env.Dir(env['libDir']),
        env.Dir(env['incDir']),
        env.Dir(env['scrDir']),
        env.Dir(env['javaDir']) ])

    env['etcDir']  = False
    env['dataDir'] = False
    env['docDir']  = False

    if env['projectName']:
        env['etcDir']  = env['etcdir']
        env['dataDir'] = env['datadir']
        env['docDir']  = env['docdir']
        env.Alias('install', [
            env.Dir(env['etcDir']),
            env.Dir(env['dataDir']),
            env.Dir(env['docDir']) ])

    env['testDir'] = False
    env['tutDir']  = False

    if env['docDir']:
        env['testDir'] = env['testdir']
        env['tutDir']  = env['tutdir']
        env.Alias('install', [
            env.Dir(env['testDir']),
            env.Dir(env['tutDir']) ])

    incdir = env.Dir(env['incDir'])
    libdir = env.Dir(env['libDir'])
    env.AppendUnique(
        PATH = [incdir],
        CPPPATH = [incdir],
        FORTRANPATH = [incdir],
        LIBPATH = [libdir],
        RPATH = [libdir.srcnode().abspath] )

    if env['verbose'] > 3:
        print_install_dirs(env)

def modify(env, key, help=None, default=None):
    evars = env['vars']
    for opt in evars.options:
        if opt.key == key:
            if not help is None:
                opt.help = help
            if not default is None:
                opt.default = default
    evars.Update(env)
    env['verbose'] = int(env['verbose'])
    env['vars'].Save(env['configFile'], env)
    format.generate_help_text(env)

def print_build_dirs(env):
    print(infostr('==== build directories ====')+'''
  buildDir:     '''+ infostr(env['buildDir']) +'''

  buildBinDir:  '''+ infostr(env['buildBinDir']) +'''
  buildLibDir:  '''+ infostr(env['buildLibDir']) +'''
  buildIncDir:  '''+ infostr(env['buildIncDir']) +'''
  buildScrDir:  '''+ infostr(env['buildScrDir']) +'''

  buildEtcDir:  '''+ infostr(env['buildEtcDir']) +'''
  buildDataDir: '''+ infostr(env['buildDataDir'])+'''
  buildDocDir:  '''+ infostr(env['buildDocDir']) +'''

  buildTestDir: '''+ infostr(env['buildTestDir'])+'''
  buildTutDir:  '''+ infostr(env['buildTutDir']) +'''

  buildObjDir:  '''+ infostr(env['buildObjDir']) +'''
    ''')

def print_install_dirs(env):
    print(infostr('==== install directories ====')+'''
  binDir:  '''+ infostr(env['binDir']) +'''
  libDir:  '''+ infostr(env['libDir']) +'''
  incDir:  '''+ infostr(env['incDir']) +'''
  scrDir:  '''+ infostr(env['scrDir']) +'''

  etcDir:  '''+ infostr(env['etcDir']) +'''
  dataDir: '''+ infostr(env['dataDir'])+'''
  docDir:  '''+ infostr(env['docDir']) +'''

  testDir: '''+ infostr(env['testDir'])+'''
  tutDir:  '''+ infostr(env['tutDir']) +'''
    ''')


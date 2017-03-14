import SCons.Script

def init():
    # If opt or debug is set on the command line
    # this sets the other (debug or opt) to the default
    # 'off' setting. This way, both debug and opt
    # are always set. Checking whether they are compatible
    # settings is done in the load_settings() method.
    if 'opt' in SCons.Script.ARGUMENTS:
        if not 'debug' in SCons.Script.ARGUMENTS:
            SCons.Script.ARGUMENTS['debug'] = False
    elif 'debug' in SCons.Script.ARGUMENTS:
        if not 'opt' in SCons.Script.ARGUMENTS:
            SCons.Script.ARGUMENTS['opt'] = 0

def set_flags(env):
    flags = []
    if env['platformName'][:3] == "win":
        flags += ['/EHsc', '/MD']
    if int(env['opt']) > 0:
        if env['debug'] is True:
            raise Exception("Cannot handle opt (" + env['opt'] + ")" \
             + " and debug (" + str(env['debug']) + ") at same time.")
        flags += ['-O'+env['opt']]
        if env['platformName'][:3] == "win":
            flags += ['/Gs']
    elif env['debug']:
        if env['platformName'][:3] == "win":
            env.AppendUnique(CXXFLAGS = ['/DEBUG'])
        else:
            flags += ['-g']
            # Merge Flags does not seem to work for fortran
            # so we force the gfortran debug flag
            env.AppendUnique(FORTRANFLAGS = ['-g'])
    # this should separate flags according to if they go to the
    # preprocessor or the compiler. Also, *should* convert '-'
    # to '/' for win32 systems.
    env.MergeFlags(flags)

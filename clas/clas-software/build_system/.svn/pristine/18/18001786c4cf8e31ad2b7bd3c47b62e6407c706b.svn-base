
def dependency_type():
    return 'header_and_binary'

#def default_include_dir():
#    return ''

#def default_lib_dir():
#    return ''

def default_libs(env):
    return []

def load_flags(env):
    flags = []
    defines = ['NDEBUG']
    env.AppendUnique(CPPDEFINES = defines)
    return (flags, defines)


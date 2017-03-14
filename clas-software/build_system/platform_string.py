########################################################################
# build_system.platform_string.py
#   puts together a platform identification string
#   that takes in:
#     1. operating system name
#     2. kernel number
#     3. architecture
#     4. C compiler
#     5. C compiler version
#
#   typical output strings look like the following:
#     linux26_i686_gcc43
#     win32_i686_msvc
#     win64_x86_64_msvc
########################################################################
import os, sys, string, subprocess

########################################################################
# getting compiler version is separated into functions
# version is not gotten if this script doesn't know how.
# for example, win/msvc will return something like:
#    msvc
# for gcc the string will be something like:
#    gcc41
########################################################################
def gcc_version():
    try:
        cmd = 'gcc --version'
        compiler_ver = subprocess.Popen(
            cmd,
            shell=True,
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE ).communicate()[0].split()[2]
        ver = compiler_ver.split('.')
        return ver[0] + ver[1]
    except:
        pass
    return ''

def msvc_version():
    return ''

def platform_string(env):
    python_version = string.split(string.split(sys.version)[0], '.')
    if map(int, python_version) < [2, 3, 0]:
        system = os.uname()[0].lower()
        release = str('').join( \
            os.uname()[2] \
             .replace('-','.') \
             .replace('_','.') \
             .split('.')[:2])
        machine = os.uname()[4]
    else:
        import platform
        system = platform.system().lower()
        release = str('').join( \
            platform.release() \
             .replace('-','.') \
             .replace('_','.') \
             .split('.')[:2])
        machine = platform.machine()

    ####################################################################
    # get the compiler
    # TODO: handle the .exe possibility for windows
    ####################################################################
    try:
        ccomp = env['CC']
    except:
        try:
            ccomp = os.environ['CCOMP'].lower()
        except:
            if system[:3] == 'win':
                ccomp = 'msvc'
            else:
                ccomp = 'gcc'
    comp = ccomp.lower()

    ####################################################################
    # At this point, the compiler is specified (or blank)
    # and is always lower-case
    # now we append it with a version (if we know how to get it)
    ####################################################################
    ccomp_ver = ccomp
    if ccomp == 'gcc':
        ccomp_ver += gcc_version()
    elif ccomp == 'msvc':
        ccomp_ver += msvc_version()

    ####################################################################
    # piece together the string and print it out
    ####################################################################
    ret = system
    if release != '':
        ret += release
    if machine != '':
        ret += '_' + machine
    if ccomp_ver != '':
        ret += '_' + ccomp_ver

    return ret

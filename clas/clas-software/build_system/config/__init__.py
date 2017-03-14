import os, glob
#dir = os.path.dirname( os.path.realpath(__file__) )
dir = os.path.dirname( __file__ )
modules = glob.glob( os.path.join(dir, '*.py') )

for m in modules:
    # module name
    module = os.path.splitext(os.path.split(m)[1])[0]
    # current directory name
    dir2 = os.path.split(os.path.split(m)[0])[1]
    # previous directory name
    dir1 = os.path.split(os.path.split(os.path.split(m)[0])[0])[1]

    mod = dir1+'.'+dir2+'.'+module

    if module[0] != '_':
        __import__(mod)

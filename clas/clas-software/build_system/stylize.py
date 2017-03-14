import os, pdb

import SCons.Variables

color = {
    'black'      : '',
    'red'        : '',
    'green'      : '',
    'yellow'     : '',
    'blue'       : '',
    'violet'     : '',
    'cyan'       : '',
    'grey'       : '',
    'lightgrey'  : ''}
style = {
    'bold'      : '',
    'underline' : '',
    'clear'     : ''}

def init(env):
    try:
        a = os.popen("tput setaf 0").read()

        env['vars'].Add(SCons.Variables.BoolVariable(
            'color',
            help = 'Output in color (works only for unix-like OS\'s).',
            default = True))

        env['vars'].Update(env)

        if env['color']:
            color['darkgrey']  = os.popen("tput setaf 0").read()
            color['red']       = os.popen("tput setaf 1").read()
            color['green']     = os.popen("tput setaf 2").read()
            color['yellow']    = os.popen("tput setaf 3").read()
            color['blue']      = os.popen("tput setaf 4").read()
            color['violet']    = os.popen("tput setaf 5").read()
            color['cyan']      = os.popen("tput setaf 6").read()
            color['grey']      = os.popen("tput setaf 7").read()
            color['lightgrey'] = os.popen("tput setaf 8").read()
            color['black']     = os.popen("tput setaf 9").read()

            style['bold']      = os.popen("tput bold").read()
            style['underline'] = os.popen("tput smul").read()

            style['clear']     = os.popen("tput sgr0").read()
    except:
        pass

def infostr(s):
    return color['blue']+str(s)+style['clear']

def tgtstr(s):
    return color['red']+str(s)+style['clear']

def tgt2str(s):
    return color['yellow']+str(s)+style['clear']

def srcstr(s):
    return color['violet']+str(s)+style['clear']

def src2str(s):
    return color['green']+str(s)+style['clear']

def alertstr(s):
    return color['red']+style['bold']+str(s)+style['clear']

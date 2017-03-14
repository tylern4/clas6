import sys, os, textwrap, string

import SCons.Script

from stylize import infostr, tgtstr, tgt2str, srcstr, src2str, alertstr

_ignore_cmd_list = ['ranlib']

def help_text(env, opt, help, default, actual, aliases=[]):
    help = help[::-1].replace(' (','(',1).replace(') ',')',1)[::-1]
    help = textwrap.dedent(help)
    fmt = '\n' + tgtstr(opt) + ': '
    fmt += textwrap.fill(infostr(help),60,subsequent_indent='\t')
    fmt += '\n\t' + infostr('default = ') + srcstr(default)
    if not str(actual) is str(default):
        fmt += '\n\t' + src2str('actual  = ' + str(actual))
    return fmt

def generate_help_text(env):
    SCons.Script.help_text = None
    env['vars'].FormatVariableHelpText = help_text
    SCons.Script.Help('\nUsage: scons [scons-options]' \
     + ' [key=value ...] [targets ...]\n')
    SCons.Script.Help('\n----- keys -----')
    SCons.Script.Help(env['vars'].GenerateHelpText(env))

def cmd_abstract(s, target, source, env):
    '''s is the original command line, target and src
    are lists of target and source nodes respectively,
    and env is the environment.'''
    cmd = s.split()[0]
    if not cmd in _ignore_cmd_list:
        if cmd == 'Install':
            line = infostr('Installing:')
            for x in target:
                line += ' ' + tgt2str(x)
        else:
            line = infostr('Building:')
            for x in source:
                line += ' ' + src2str(str(x).split(os.sep)[-1])
            line += '   ' + infostr('--->') + '  '
            for x in target:
                line += ' ' + tgt2str(str(x).split(os.sep)[-1])
        line += '\n'
        sys.stdout.write(line)

def cmd_blank(s, target, source, env):
    return 0

def cmd_output(env):
    if env['verbose'] == 0:
        env['PRINT_CMD_LINE_FUNC'] = cmd_blank
    elif env['verbose'] in [1, 2]:
        env['PRINT_CMD_LINE_FUNC'] = cmd_abstract
        env['CXXCOM'] = "${TEMPFILE('%s')}" % env['CXXCOM']

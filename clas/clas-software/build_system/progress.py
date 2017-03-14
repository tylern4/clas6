#/usr/bin/env python
# -*- coding: utf-8 -*-

# a fancy progress animation. maybe a bit too fancy...

a = '°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤º°`°º¤ø,¸¸,ø¤º°`'
msg = 'evaluating targets'

msg_list = []
for i in range(0,12):
    msg_list += ['   |' + a[i:] + a[:i] + '|'
    + '   ' + msg + '\r']

screen = open('/dev/tty', 'w')

def set_message(s):
    m = []
    for i in range(0,12):
        m += ['   |' + a[i:] + a[:i] + '|'
        + '   ' + s + '\r']
    globals()['msg_list'] = m

def print_ticker(node):
    try:
        a = print_ticker.init
        print_ticker.i += 1
        if print_ticker.i == len(msg_list):
            print_ticker.i = 0
    except:
        print_ticker.i = 0
        print_ticker.init = True
    screen.write(msg_list[print_ticker.i])
    screen.flush()

#if env['verbose'] is 2:
#    intvl = (12 * 2 + 1)
#    import progress
#    progress.set_message(infostr('evaluating build targets'))
#    SCons.Script.Progress(progress.print_ticker, interval = intvl)

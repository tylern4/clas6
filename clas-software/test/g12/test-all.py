#!/usr/bin/env python

from os import environ
from subprocess import Popen, PIPE

def run_tests():
    results = dict(tests=0, successful=0, failed={})
    tests = '''\
test-g12-genr8.sh
test-g12-gamp2part.sh
test-g12-gsim.sh'''.split('\n')
    for cmd in tests:
        print 'running:',cmd
        proc = Popen(cmd, shell=True, executable='/bin/bash', env=environ, stdout=PIPE, stderr=PIPE)
        output = proc.communicate()[0].split('\n')

        results['tests'] += 1
        if proc.returncode == 0:
            results['successful'] += 1
        else:
            results['failed'][cmd] = output
    return results


def print_results(results):
    print results['successful'],'/',results['tests'],'were successful'
    if len(results['failed']) > 0:
        print 'failed tests:'
        for cmd,output in results['failed']:
            print ' >>> ',cmd,' <<< '
            if len(output) < 15:
                for l in output:
                    print l
            else:
                for l in output[:5]:
                    print l
                print '... [snip] ...'
                for l in output[-10:]:
                    print l


if __name__ == '__main__':
    results = run_tests()
    print_results(results)

#
# script to run a command by setting additional Latex-related env vars.
# Used to build the docs
#
import os, sys, subprocess

val = sys.argv[1]
os.environ['TEXINPUTS'] = '%s%s%s' % (val, os.pathsep,
                                      os.environ.get('TEXINPUTS', ''))
os.environ['BIBINPUTS'] = '%s%s%s' % (val, os.pathsep,
                                      os.environ.get('BIBINPUTS', ''))

exitcode = subprocess.call(sys.argv[2:])
sys.exit(exitcode)

#
# script to generate a single grammar/lexer file from components
#
import os, sys
import optparse

parser = optparse.OptionParser()
parser.add_option('--output')
parser.add_option('--start')

opts, args = parser.parse_args()

d = {}

def val(e):
    try: return int(e[1:])
    except ValueError: return None
    
def process(name):
    vals = []
    while True:
        n, e = os.path.splitext(name)
        v = val(e)
        if v is None:
            break
        vals.append(v)
        name = n
    while vals:
        nn = name + '.' + str(vals[-1])
        d.setdefault(name, set()).add(nn)
        vals.pop()
        name = nn

data = {}
for name in args:
    basename = os.path.basename(name)
    process(basename)
    with open(name) as src:
        data[basename] = src.read()

for key in sorted(d.iterkeys(), reverse=True):
    is_top = val(os.path.splitext(key)[1]) is None
    parts = sorted(d[key])
    out = []
    for i, p in enumerate(parts):
        if not is_top:
            out.append('  /* BEGINS: %s */\n' % p)
        out.append(data[p])
        if not is_top:
            out.append('  /* ENDS: %s */\n' % p)
        if is_top and i != len(parts)-1:
            out.append('%%\n')

    data[key] = ''.join(out)
    if is_top and opts.start:
        data[key] = data[key].replace('@start@', opts.start)

    if is_top:
        if opts.output:
            with open(opts.output, 'w') as f:
                f.write(data[key])
        else:
            sys.stdout.write(data[key])

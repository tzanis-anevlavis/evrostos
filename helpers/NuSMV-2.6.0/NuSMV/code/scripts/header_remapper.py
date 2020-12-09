from converters.includes import IncludesConverter
import sys
import os.path

ROOT_PATHS = [".."]
SEARCH_PATHS = ["./tmp"]
TEST = False

def print_usage():
    """Fixes a set of source and header files containing wrong includes.
Usage:
$> python header_remapper.py [--help] [-n]
              [-s store_session search_path[;...] [root_path[;...]]
"""
    print print_usage.__doc__
    return
    
    
def print_help():
    """Fixes a set of source and header files containing wrong includes.

Fixing is performed w.r.t. NuSMV package inclusion conventions.
E.g. all #include \"grammar.h\" are changed into #include \"parser/grammar.h\"
if the package parser contains a file called \"grammar.h\"

Files to be changed are recursively searched starting from a list of
roots given by the parameter search_path. Files to be choosen as
candidates for substitution are searched into the paths given by the
parameter root_path

Usage:
$> python header_remapper.py [--help] [-d]
              [-s store_session] [root_path[;...] [search_path[;...]]

 Where:
   root_path:   paths containing source files to be changed.
   search_path: paths containing header files to be choosen as candidates
                for the substitution. If not specified, root_path will be
                considered.
   -n:          does not apply any modification to files (only simulates it)
   -s session:  read previously stored session from file 'session'

Warning: multiple paths must be provided by separating them with a
         semi-colon (;) and by do not using any white spaces.

If the user is requested to help the script, a session containing any
provided option is stored. This file can be used later when recalling
the script. 

"""

    print print_help.__doc__
    return

def main(search_path, root_paths, test_mode, session_name=None):
    cands = read_session(session_name)
    
    ic = IncludesConverter(root_paths, search_path, cands)
    ic.convert(test_mode)

    map = ic.get_user_candidates()
    write_session(map, session_name)
    return


def read_session(session_name):
    res = {}
    if session_name:
        print "Reading specified session from '%s'..." % session_name
        sf = open(session_name, 'r')
        lines = sf.read().split('\n')
        for line in lines:
            t = line.split(':')
            if len(t) == 2:
                k = t[0].strip()
                # extracts a list object:
                v = t[1][t[1].index('[')+1:t[1].index(']')].strip()
                v = v.replace("'", "")
                v = v.replace('"', "")
                res[k] = v.split(',')
                pass
            pass
        sf.close()
        pass

    return res;

def write_session(map, _session_name):
    if not map or len(map.keys()) == 0: return

    mode = "w"

    print "\nFilling session data with %d choices" % len(map.keys())
    while (True):
        if _session_name: prompt = "> [default: %s]" % _session_name
        else: prompt = "> "
        print "Speficy a session file name (\"skip\" to skip this): "
        session_name = raw_input(prompt)
        if session_name == "skip":
            session_name = None
            break
        if session_name == "": session_name = _session_name
        
        if os.path.isfile(session_name):
            print "Specified file already esists - Overwrite, Append or Change filename? (O/A/C)"
            inp = raw_input("")
            if inp == "O":
                mode = "w"
                break
            elif inp == "A":
                mode = "a"
                break
            pass
        else: break
        
        pass
    
    if not session_name: return
    sf = open(session_name, mode)
    for k in map: sf.write("%s : %s\n" % (k, map[k]))
    sf.close()
    return    


if __name__ == "__main__":
    test_mode = False
    if len(sys.argv) < 2 or len(sys.argv) > 6:
        print_usage()
        sys.exit(1)
        pass
    
    if "--help" in sys.argv:
        print_help()
        sys.exit(0)
        pass

    args = sys.argv[1:]
    if "-n" in sys.argv:
        test_mode = True
        args = sys.argv[2:]
        pass

    session_name = None
    if args[0] == "-s":
        session_name = args[1]
        args = args[2:]
        pass

    if len(args) == 1: search_paths = root_paths = args[0].split(';')
    elif len(args) == 2:        
        search_paths = args[0].split(';')
        root_paths = args[1].split(';')
    else:
        print_usage()
        sys.exit(1)
        pass

    main(search_paths, root_paths, test_mode, session_name)
    pass




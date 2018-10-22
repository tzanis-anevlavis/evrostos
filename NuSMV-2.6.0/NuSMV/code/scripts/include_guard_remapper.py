from optparse import OptionParser
from extractors.file_searcher import HeaderSearcher
from extractors.include_guard_extr import IncludeGuardExtractor, IncludeGuardSubstituter
import os.path


def main(search_paths, dry_run, debug):
    # Initializations
    include_guard_extractor = IncludeGuardExtractor()
    ig_set = set()

    # for each input file in searched dirs
    h_searcher = HeaderSearcher(search_paths.split(":"))
    h_searcher.search()
    headers = h_searcher.get_matches()

    if debug:
        print "LOG: searched headers:"
        print str(headers)
        print

    # Get the name for the include guard from the pathname
    for single_header in headers:
        if debug:
            print "In header " + single_header

        replace_include_guard = remap(single_header)
        # include guards of nusmv and nuxmv rbc must be the same
        if "_RBC_" in replace_include_guard and "__NUXMV_" in replace_include_guard:
            replace_include_guard = replace_include_guard.replace("__NUXMV_", "__NUSMV_")

        # include guards of list.h and sa_list.h must be the same
        if single_header.endswith("sa_list.h"):
            replace_include_guard = "__NUSMV_CORE_UTILS_LIST_H__"

        if replace_include_guard:
            # handle exceptions to avoid generating identical macros
            if os.path.basename(single_header) == "SymbTable.h":
                replace_include_guard = "__NUSMV_CORE_COMPILE_SYMB_TABLE_SYMB_TABLE_CLASS_H__"
            elif os.path.basename(single_header) == "TraceExecutor.h":
                replace_include_guard = "__NUXMV_CORE_TRACE_EXECUTOR_TRACE_EXECUTOR_CLASS_H__"
            elif os.path.basename(single_header) == "MSatEnc.h":
                replace_include_guard = "__NUXMV_CORE_MATHSAT_ENC_MSAT_ENC_CLASS_H__"

            # Search the macro to remap
            f = open(single_header, 'r')
            header_content = f.read()
            f.close()

            match_include_guard = include_guard_extractor.extract(header_content)

            # Substitute (log it)
            if (replace_include_guard not in ig_set
                or "_RBC_" in replace_include_guard
                or replace_include_guard ==  "__NUSMV_CORE_UTILS_LIST_H__"):
                ig_set.add(replace_include_guard)
                if match_include_guard:
                    if debug and match_include_guard != replace_include_guard:
                        print "LOG: Substituting " + match_include_guard + " with " + replace_include_guard

                    include_guard_substituter = IncludeGuardSubstituter(header_content)
                    modified_header_content = include_guard_substituter.substitute(match_include_guard, replace_include_guard)

                    # Write it
                    if not dry_run and modified_header_content != header_content:
                        f = open(single_header, 'w')
                        f.write(modified_header_content)
                        f.close()
                else:
                    if not debug:
                        print "The above warning(s) refers to " + single_header
            else:
                print "WARNING: Skipping header " + single_header
                print "because include guard " + replace_include_guard + " has already been used"
        else:
            if not debug:
                print "The above warning(s) refers to " + single_header

        if debug:
            print

    return


def remap(single_header):
    # strip prefix until code
    include_guard = single_header[single_header.find("/code/"):]
    if include_guard == single_header[-1:]:
        # try with src
        include_guard = single_header[single_header.find("/src/"):]
        if not include_guard == single_header[-1:]:
            include_guard = include_guard[5:]
    else:
        include_guard = include_guard[6:]

    if not include_guard == single_header[-1:]:
        # Add an underscore before each capitalized word, first excluded
        tmp = ""
        just_added = False
        for c in include_guard:
            if c.isupper() and not just_added:
                tmp = tmp + "_" + c
                just_added = True
            elif c.islower() and just_added:
                tmp = tmp + c
                just_added = False
            else:
                tmp = tmp + c
                include_guard = tmp

        # upcase it
        include_guard = include_guard.upper()

        # change slashes in underscores
        include_guard = include_guard.replace("/", "_")
        include_guard = include_guard.replace(".", "_")
        # Remove double underscores
        include_guard = include_guard.replace("__", "_")
        include_guard = "__" + include_guard + "__"
    else:
        include_guard = ""
        print "WARNING: failed to compute the replace include guard"
    
    return include_guard


if __name__ == "__main__":
    description="""
Rewrites the include guard (the macro that avoid multiple inclusions of the same
header) in all the headers found under the colon separated
'search_paths'. The default action is a dry-run, the user must specify -w to
actually write to disc.

The new include guard will follow this format:
<CAPITALIZED_UNDERSCORE_SEPARATED_FULL_PATHNAME_STARTING_AFTER_CODE_OR_SRC_FOLDER>
Example:
ESTools/NuSMV/code/nusmv/core/compile/symb_table/SymbTable.h
__NUSMV_CORE_COMPILE_SYMB_TABLE_SYMB_TABLE_H__

The script uses a very simple way to detect the include guards and handles
specifically the complicated exceptions:
* The rbc packages in NuSMV and nuXmv must have the same guards
* list.h and sa_list.h must have the same guard
* lsort.h and sugar.h are skipped
* SymbTable.h, TraceExecutor.h, MSatEnc.h guards are hard-coded to avoid clash
with same name module headers.
"""
    parser = OptionParser(usage="usage: %prog [-w] [-d] search_paths",
                          description=description)
    parser.add_option("-w", "--write", action="store_true", default=False,
                      dest="execute",
                      help="Disable the default dry-run execution")
    parser.add_option("-d", "--debug", action="store_true", default=False,
                      dest="debug",
                      help="Enable debug mode")

    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.error("incorrect number of arguments")

    if not options.execute:
        options.debug = True

    main(args[0], not options.execute, options.debug)

    if not options.execute:
        print "##########################################################"
        print "WARNING: dry-run mode, rerun with -w to modify the headers"
        print "##########################################################"
    pass

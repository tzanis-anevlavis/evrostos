import re

class IncludeGuardSubstituter:
    """
    Returns a copy of the string given to the constructors, in which
    old_include_guard has been substituted by new_include_guard.
    """
    def __init__(self, str):
        self.str = str
        return

    def substitute(self, old_include_guard, new_include_guard):
        if old_include_guard == new_include_guard:
            return self.str
        
        res = self.str.replace(old_include_guard, new_include_guard)

        return res

    pass # end of class


class IncludeGuardExtractor:
    """
    Returns the include guard, or the empty string if not found, present in the
    input string. The detection is basic: the include guards is considered to be the
    firs ifndef.
    """
    def __init__(self):
        self.re = re.compile("^\s*#\s*ifndef\s+([\w]+)")
        return

    def extract(self, str):
        res = ""

        for line in str.split("\n"):
            m =  self.re.match(line)
            if m:
                if not res:
                    res = m.group(1)

                    # for now, lazy evaluation
                    if True:
                        return res
                else:
                    print "WARNING: Ignored non first ifndef: "  + m.group(1)
                pass
            pass

        if not res:
            print "WARNING: include guard not found"
            print

        return res
    
    pass # end of class


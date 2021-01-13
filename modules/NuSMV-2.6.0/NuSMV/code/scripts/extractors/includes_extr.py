# Given a string, searches all the #include "name" and returns a list
# of all "names" found

import re

class IncludesSubstituter:
    def __init__(self, str):
        self.str = str
        return

    def substitute(self, old_include, new_include):
        """Returns the number of changed strings"""

        print "DEBUG: Substituting " + old_include + " with " + new_include

        if old_include == new_include: return (self.str, 0)
        r = re.compile("^(\s*#\s*include)\s+[\"\'](%s)[\"\']" % old_include, 
                       re.MULTILINE)
        (self.str, count) = r.subn("\\1 \"%s\"" % new_include, self.str)
        return (self.str, count)

    pass # end of class
        

class IncludesExtractor:

    def __init__(self):
        self.re = re.compile("^\s*#\s*include\s+[\"\']([\w/.]+)[\"\']")
        return

    def extract(self, str):
        res = []
        #pos = 0

        #while (1):
            # start = str.find("#include", pos)
            # if start < 0: break
            
            # end = str.find("\n", start)
            # if end == -1: end = len(str)
            # pos = end
            # line = str[start:end]
        for line in str.split("\n"):
            m =  self.re.match(line)
            if m:
                filename = m.group(1)
                res.append(filename)
                pass
            pass

        return res
    
    pass # end of class

                
                
                

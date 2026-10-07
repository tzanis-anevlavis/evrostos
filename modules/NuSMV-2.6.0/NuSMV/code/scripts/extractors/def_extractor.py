from functions import FunDefPrototype
import re


# ----------------------------------------------------------------------
class FunDefExtractor:

    def __init__(self, filename, comments_stripper):
        f = open(filename, 'r')
        text = f.read()
        f.close()

        self.protos = []

        text = comments_stripper.strip(text)
 
        pattern = "^\s*(?P<ret_type>[*\w\s]+)\s+(?P<fun_name>[\w]+)\s*\((?P<arguments>[\w\s*,]*)\)\s*{"
        r = re.compile(pattern, re.M)
        lineno = 0
        pos = 0

        while (1):
            m = r.search(text, pos)
            if m is None: break
            fun = FunDefPrototype(m.group('ret_type'), m.group('fun_name'),
                                  m.group('arguments'), filename, lineno)
            self.protos.append(fun)
            pos = m.end()
            pass

        return
    
    def get_prototypes(self): return self.protos
    
    pass # end of class

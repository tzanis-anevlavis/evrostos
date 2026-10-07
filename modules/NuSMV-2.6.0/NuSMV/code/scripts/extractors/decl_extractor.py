from functions import FunDeclPrototype
import re

import os.path



class FunDeclExtractor:

    def __init__(self, filename, comments_stripper):
        self.stripper = comments_stripper
        self.filename = filename
        self.protos = []


        f = open(self.filename, 'r')
        header = f.read()

        header = self.stripper.strip(header)

        line_num = 0
        j = -1
        while (1):
            i = header.find("EXTERN", j+1)
            if i == -1: break
            j = header.find(";", i)
            if j == -1: break

            str = header[i:j+1]
            try:
                proto = FunDeclPrototype(str, self.filename, line_num)
                self.protos.append(proto)
            except ValueError:
                pass
            
            pass # end of loop
        
        f.close()
        return

    def get_prototypes(self): return self.protos

    pass  # end of class

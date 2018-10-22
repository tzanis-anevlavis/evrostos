

from comment_stripper import CommentStripper
from decl_extractor import FunDeclExtractor
from def_extractor import FunDefExtractor
import os.path


class HeaderChecker:

    def __init__(self, header_filename,
                 definition_filenames):

        self.cs = CommentStripper()
        self.decl = FunDeclExtractor(header_filename,
                                     self.cs)

        self.defs = []
        for filename in definition_filenames:
            fde = FunDefExtractor(filename, self.cs)
            self.defs.append(fde)
            pass
        
        return

    def get_declarations_count(self):
        return len(self.decl.get_prototypes())

    def check(self):
        """Checks wheather all declared functions are also implemented"""
        decl_names = {}
        def_names  = {}
        for proto in self.decl.get_prototypes():
            decl_names[proto.get_name()] = proto
            pass

        for defs in self.defs:
            for proto in defs.get_prototypes():
                def_names[proto.get_name()] = proto
                pass
            pass

        for proto in self.decl.get_prototypes():
            name = proto.get_name()
            if not def_names.has_key(name):
                print "Warning: function '%s' has no implementation" \
                      % name
                pass
            else:
                proto_def = def_names[name]
                type1 = proto.get_type().strip()
                type2 = proto_def.get_type().strip()
                if type1 != type2:
                    print "Warning: function '%s' is declared to return '%s', but the implementation returns '%s'" \
                      % (name, type1, type2)
                    pass
                proto.get_formal_params().compare(
                    proto_def.get_formal_params())
                
            pass
        return
        

    pass  # end of class



def main():
    if len(sys.argv) < 3:
        print "Interfaces checker by RC 2003 ITC-irst"
        print "Usage: "
        print "header_checker.py  <header filename> <impl filenames>\n"
        sys.exit(1)
        pass

    print "Processing header and implementation files..."
    hc = HeaderChecker(sys.argv[1], sys.argv[2:])
    print "OK. Checking for %d declarations..." % hc.get_declarations_count()
    hc.check()
    print "\nDone"
    return


if __name__ == "__main__":
    import sys
    main()
    pass


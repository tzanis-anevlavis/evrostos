
from extractors.file_searcher import FileSearcher
from converters.pattern_changer import PatternChanger


def main():

    s = FileSearcher("Makefile.am")

    cp = PatternChanger("Copyright \(C\) (.*) by ITC-irst");
    cvs_id = PatternChanger("\$Id: cvs_id_remover.py,v 1.1.2.1 2003-11-27 09:29:12 nusmv Exp $");
    
    s.search(".")

    for makefile in s.get_matches():
        f = file(makefile, 'r')
        str = f.read();
        f.close()
        
        str = cp.change(str, "Copyright (C) 2003 by ITC-irst");
        str = cvs_id.change(str, "\\1")

        f = file(makefile, 'w')
        f.write(str) 
        f.close()
        print "Wrote " + makefile
        pass        
        
    return

if __name__ == "__main__":
    main()
    pass





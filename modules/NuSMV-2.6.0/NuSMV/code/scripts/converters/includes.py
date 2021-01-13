# converts the includes into source code wrt new include rules
# e.g. "#include 'error.h'" is converted into "#include 'utils/error.h'",
# since error.h is located into the package 'utils'

from extractors.includes_extr import IncludesExtractor, IncludesSubstituter
from extractors.file_searcher import HeaderSearcher, ImplementationSearcher, ParserSearcher

import os.path

class IncludesConverter:

    def __init__(self, root_paths, search_paths=None, default_candidates={}):
        """default_candidates is a map"""

        if not search_paths: search_paths = root_paths
        
        self.inc_extractor = IncludesExtractor()

        self.candidates = default_candidates
        self.root_paths = root_paths
        self.search_paths = search_paths

        h_searcher = HeaderSearcher(root_paths)
        c_searcher = ImplementationSearcher(root_paths)
        yl_searcher = ParserSearcher(root_paths)
     
        self.headers = []
        h_searcher.search()
        tmp = h_searcher.get_matches()
        for f in tmp: self.headers.append(os.path.abspath(os.path.normpath(f)))

        self.c_files = []
        c_searcher.search()
        tmp = c_searcher.get_matches()
        for f in tmp: self.c_files.append(os.path.abspath(os.path.normpath(f)))

        yl_searcher.search()
        tmp = yl_searcher.get_matches()
        for f in tmp: self.c_files.append(os.path.abspath(os.path.normpath(f)))

        # searches for candidates:
        h_searcher = HeaderSearcher(search_paths)
        c_searcher = ImplementationSearcher(search_paths)
     
        self.cands_headers = []
        h_searcher.search()
        tmp = h_searcher.get_matches()
        # eliminate duplicates
        tmp = list(set(tmp))
        for f in tmp: 
            self.cands_headers.append(os.path.abspath(os.path.normpath(f)))
#            print "DEBUG" + f
        pass

        self.cands_c_files = []
        c_searcher.search()
        tmp = c_searcher.get_matches()
        for f in tmp: self.cands_c_files.append(os.path.abspath(os.path.normpath(f)))

        self.user_candidates = {}
        return


    def get_user_candidates(self):
        """Returns candidates specified by the user"""
        return self.user_candidates
    

    def get_minimal_path(self, str, path):
        str = os.path.abspath(os.path.normpath(str))
        tmp = []
        for path in self.search_paths:
            path = os.path.abspath(os.path.normpath(path))+"/"
            cp = os.path.commonprefix([str, path])
            if cp:
                s = str[len(cp):]
                tmp.append(s)
                pass
            pass

        res = ""
        l_max = 1000
        for t in tmp:
            l = len(t.split("/"))
            if  l < l_max: res = t; l_max = l
            pass
                
        return res
            
        
    def get_candidates(self, str, path):
        """Given an included file, returns a list of candidates"""
        print "Getting candidates for header " + str
        if not self.candidates.has_key(str): 
            res = []
            
            basename = os.path.basename(str)
            for fn in self.cands_headers:
#                if basename == os.path.basename(fn):
#                print "Considering candidate header" + fn
                if fn.endswith("/" + str):
                    s = self.get_minimal_path(fn, path)
#                    s = self.get_abs_include(path + basename)
                    assert(s)
                    res.append(s)
                    pass
                pass
#                print "Skipped because it does not end with /" + str
            self.candidates[str] = res;
            pass
        
        return self.candidates[str]


    def substitute(self, path, subst, old_include):      
        """Can be interactive. Rets the changed string and the number of changes"""
        cands = self.get_candidates(old_include, path)
        if len(cands) > 1:
            print "  Into %s:" % path
            print "  Candidates for header '%s' are:" % old_include
            print "  ", cands
            print "  Choose one (it will NOT be stored for future):"
            cand = raw_input("> ")
            #self.candidates[old_include] = [cand]
            self.user_candidates[old_include] = [cand]        
        
        elif len(cands) == 0:
            print "  Into %s:" % path
            print "  There are no valid candidates for header '%s'" % old_include
            shorten_include = old_include
            while len(shorten_include.split("/")) > 1 and len(cands) == 0:
                print "Trying removing heading dirname from " + shorten_include
                shorten_include = shorten_include[shorten_include.find("/")+1:]
                cands = self.get_candidates(shorten_include, path)
            if len(cands) == 0:    
                print "  Specify one (it will NOT be stored for future):"
                cand = raw_input("> ")
                #self.candidates[old_include] = [cand]
                self.user_candidates[old_include] = [cand]
            elif len(cands) == 1:
               cand = cands[0]
            else:
                print "  Specify one (it will NOT be stored for future):"
                cand = raw_input("> ")
                #self.candidates[old_include] = [cand]
                self.user_candidates[old_include] = [cand]
            
        else: cand = cands[0]

        return subst.substitute(old_include, cand)
     
    
    def convert(self, dontdoit=False):
        files = self.headers + self.c_files
        for fn in files:
            print "Processing file %s" % fn

            path = os.path.dirname(fn)
            f = open(fn, 'r')
            str = f.read()
            f.close()
            subst = IncludesSubstituter(str)
            incs = self.inc_extractor.extract(str)
            count = 0
            for inc in incs:
                str, c = self.substitute(path, subst, inc)
                count += c
                pass

            if count > 0:
                if dontdoit:
                    print "Did not write \"%s\": %d changed includes" % (fn, count)
                else:
                    f = open(fn, "w")
                    f.write(str)
                    print "Wrote \"%s\": %d changed includes" % (fn, count)
                    f.close()
                    pass
                pass
            
            pass            

        return

    pass # end of class
            
            
            
        

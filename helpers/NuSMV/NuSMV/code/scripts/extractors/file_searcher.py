# Searches into a dir and sub dir all the header files

import os.path
import glob


class FileSearcher:
    """Recursively searches into a dir for files matching a given pattern"""
    
    def __init__(self, pattern):
        """@param pattern is a string in the form given by the glob module"""
        self._matches = []
        self._pattern = pattern
        return

    def reset(self):
        self._matches = []
        return

    def search(self, path):
        """Runs the search from given root"""
        os.path.walk(path, self._visit_dir, self._pattern)
        return

    def _visit_dir(self, pattern, dirname, filenames):
        """Private, used by search_files"""
        pathname = os.path.join(dirname, pattern)
        files = glob.glob(pathname)
        self._matches = self._matches + files
        return

    def get_matches(self): return self._matches

    pass # end of class
# ----------------------------------------------------------------------

class HeaderSearcher (FileSearcher):
    def __init__(self, paths):
        FileSearcher.__init__(self, '*.h')
        self.paths = paths
        return

    def search(self):
        for path in self.paths: FileSearcher.search(self, path)
        return

    pass # end of class


class ImplementationSearcher (FileSearcher):
    def __init__(self, paths):
        FileSearcher.__init__(self, '*.c')
        self.paths = paths
        return

    def search(self):
        for path in self.paths: FileSearcher.search(self, path)
        return

    pass # end of class


class ParserSearcher (FileSearcher):
    def __init__(self, paths):
        FileSearcher.__init__(self, '*.[yl]')
        self.paths = paths
        return

    def search(self):
        for path in self.paths: FileSearcher.search(self, path)
        return

    pass # end of class

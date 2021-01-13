
# Changes the string wrt a given pattern searches in the string for a
# given pattern, and change it wrt the other given pattern

import re

class PatternChanger:

    def __init__(self, pattern_str):
        self.pattern = re.compile(pattern_str, re.MULTILINE)
        return

    def change(self, string, new_pattern, count=0):
        """Use \n to reuse groups inside pattern"""
        return self.pattern.sub(new_pattern, string, count)

    pass # end of class

        

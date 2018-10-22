

class CommentStripper:

    def __init__( self, terminators = [('/*', '*/'), ('//', None)]):
        self.terminators = terminators
        return

    def strip(self, str):
        """Returns a string which is str without comments"""
        res = str

        for term in self.terminators:
            i = 0
            while (1):
                i = res.find(term[0], i)
                if i == -1: break
                j = -1
                if term[1]: j = res.find(term[1], i) + len(term[1])
                else:
                    j = res.find("\n", i)  #Terminator is new line
                    if j == -1: j = len(res) # or the end of string
                    pass

                # removes the comment
                res = res[0:i] + " " + res[j+1:]
                
                pass
            
            pass # end of for loop

        return res;

    pass # end of class

    
    

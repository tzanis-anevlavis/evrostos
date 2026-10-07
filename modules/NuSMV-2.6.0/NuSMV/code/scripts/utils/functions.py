import re
import os.path


# ----------------------------------------------------------------------
class ArgumentList:
    def __init__(self, str, msg_prefix, fun_name):

        """@param str comes in the form 'type1 arg1, type2 arg2, ...,
        typeN argN'"""

        self.params = []
        self.fun_name = fun_name
        str = str.strip()

        if not str: print "%s Warning: Arguments list is empty in function '%s'" \
           % (msg_prefix, fun_name)
        
        for a in str.split(","):
            self.params.append( " ".join(a.split()) )
            pass
        
        return


    def get_params(self):
        return self.params

    def compare(self, other):
        if self.params != other.params:
            print "Warning: declaration of function '%s' is different from the implementation:" \
                  % self.fun_name

            if len(self.params) != len(other.params):
                print "  Number of arguments is different!"
            else:
                for i in range(0, len(self.params)):
                    if self.params[i] != other.params[i]:
                        print "  Parameter %d is declared as '%s' and defined as '%s'" \
                              % (i+1, self.params[i], other.params[i])
                        pass
                    pass
                pass
            pass
        
        return
    
    pass # end of class


# ----------------------------------------------------------------------
class FunDefPrototype:

    def __init__(self, ret_type, name, arguments,
                 filename, line):
        self.ret_type = ret_type
        self.fun_name = name
        msg_prefix =  "%s:%d:1: " % (os.path.abspath(filename), line)
        
        self.args = ArgumentList(arguments, msg_prefix, name)
        return
    
    def get_type(self): return self.ret_type

    def get_name(self): return self.fun_name

    def get_formal_params(self): return self.args

    pass # end of class



# ----------------------------------------------------------------------
class FunDeclPrototype:
    """Keeps info about a function prototype"""

    def __init__(self, str, filename, line):

        """Given a string containing a function definition, extract
        all prototype info and creates a FunPrototype.  Raises a
        ValueError exception if the string does not contain a valid
        function prototype"""
        
        str = str.strip()

        args = "\((?P<arguments>[\w\s*,]*)\)"
        pattern = "EXTERN\s+(?P<ret_type>[\w\s]+)\s+(?P<fun_name>\w+)\s*(?P<args>ARGS\s*\(\s*)?%s\)?\s*;" % args

        r = re.compile(pattern, re.MULTILINE)
        m = r.match(str);

        if not m: raise ValueError("Wrong function prototype: %s" % str)

        msg_prefix =  "%s:%d:1: " % (os.path.abspath(filename), line)
        
        # retrieve fields:
        name = m.group('fun_name')

        if m.group('args') is None:
            print "%s Warning: ARGS is missing in function '%s'" \
                  % (msg_prefix, name)
            pass

        # stores info
        self.ret_type = m.group('ret_type')
        self.fun_name = name
        self.args = ArgumentList(m.group('arguments'),
                                 msg_prefix, name)

        return
    
    def get_type(self): return self.ret_type

    def get_name(self): return self.fun_name

    def get_formal_params(self): return self.args

    pass # end of class


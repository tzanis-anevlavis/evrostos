#! /usr/bin/env python

# This script can be used to create class hierarchy very easily.
# It gets the package name, the class name, and base class name if there is any.
# It generates all scheleton code within the directory where the script is launched.


import templates
import os.path
import time

class Renamer:
    def __init__(self):
        self.keep_word = ("BDD", "RBC")
        return

    def split_words(self, str):
        words = []
        word = ""
        upcase_word = ""

        for s in str:
            if s == "_":
                # commit previous word:
                if word: words.append(word)
                word = ""
                upcase_word = ""
            elif s.isupper():
                if not upcase_word:
                    # no previous upcases, new word:
                    if word: words.append(word)
                    word = s.lower()
                    upcase_word = s
                else:
                    if upcase_word in self.keep_word:
                        words.append(upcase_word)
                        word = s.lower()
                        upcase_word = s
                    else:
                        upcase_word += s
                        word += s.lower()
                        pass
                    pass
                pass
            elif s.islower():
                if len(upcase_word) > 1:
                    words.append(upcase_word[:-1])
                    word = word[-1]
                    pass
                upcase_word = ""
                word += s

                pass
            elif s.isdigit(): word += s

            pass # loop

        if word: words.append(word)
        return words

    def lowcase(self, s):
        words = self.split_words(s)
        return "_".join(map(str.lower, words))

    def upcase(self, s):
        words = self.split_words(s)
        return "_".join(map(str.upper, words))

    pass # end of class


class ClassBuilder:

    def __init__(self, esmc, dpath, bpath, custom_map = {}):
        self._default_map = {
            'author' : "Roberto Cavada",
            'copyright' : "%s by FBK-irst" % time.strftime("%Y")
            }

        # Default values that always overload:
        self._default_map.update(custom_map)

        # this is the translation map:
        self._trans_map = {}
        self.renamer = Renamer()
        self.esmc = esmc
        self.dpath = dpath
        self.bpath = bpath
        return


    def update_trans_map(self, map):
        """Called before building whatever"""
        self._trans_map.update(map)
        self._trans_map.update(self._default_map)
        return


    def build_implementation(self):
        """Returns the code as string, and the filename the code should be stored"""
        local_map = {
            'comment_header' : "CFile",
            'tailing'  : "*" * (79 - len("CFile") - 3),
            'filename' : "%s.c" % self._trans_map['ClassName'],
            'synopsis' : "Implementation of class '%s'" \
                              % self._trans_map['ClassName'],
            'description' : "",
            'seealso' : "%s.h" % self._trans_map['ClassName'],
            }
        self.update_trans_map(local_map)

        derived = self._trans_map['BaseClassName']

        if esmc:
            str  = templates.HEAD_COMMENT_ESMC % self._trans_map
        else:
            str  = templates.HEAD_COMMENT % self._trans_map
        str = str.replace("@Id: ", "$Id: ")
        str += "\n" + templates.INCLUDE % {'inc_file' : "\"%s/%s.h\"" % (self.dpath, self._trans_map['ClassName'])}
        if derived: str += "\n" + templates.INCLUDE % {'inc_file' : "\"%s/%s_private.h\"" % (self.dpath, self._trans_map['ClassName'])}
        str += "\n"

        if derived:
            self._trans_map['class_def'] = "/* See '%(ClassName)s_private.h' for class '%(ClassName)s' definition. */" % self._trans_map
            self._trans_map['static_meth_decl'] = templates.PRIVATE_METHODS_DECL % self._trans_map
            self._trans_map['public_meth_def'] = templates.CLASS_PUBLIC_METHODS % self._trans_map
            self._trans_map['protected_meth_def'] = templates.CLASS_PROTECTED_METHODS % self._trans_map
            self._trans_map['private_meth_def'] = templates.CLASS_PRIVATE_METHODS % self._trans_map
        else:
            self._trans_map['class_def'] = templates.CLASS_DEFINITION_NO_INHERITANCE % self._trans_map
            self._trans_map['static_meth_decl'] = templates.PRIVATE_METHODS_DECL_NO_INHERITANCE % self._trans_map
            self._trans_map['public_meth_def'] = templates.CLASS_PUBLIC_METHODS_NO_INHERITANCE % self._trans_map
            self._trans_map['protected_meth_def'] = ""
            self._trans_map['private_meth_def'] = templates.CLASS_PRIVATE_METHODS_NO_INHERITANCE % self._trans_map
            pass

        str += "\n" + templates.DEF_SECTIONS % self._trans_map
        return (str, self._trans_map['filename'])



    def build_private_header(self):
        """Returns the code as string, and the filename the code should be stored"""
        local_map = {
            'comment_header' : "CHeaderFile",
            'tailing'  : "*" * (79 - len("CHeaderFile") - 3),
            'filename' : "%s_private.h" % self._trans_map['ClassName'],
            'synopsis' : "Private and protected interface of class '%s'" \
                            % self._trans_map['ClassName'],
            'description' : "This file can be included only by derived and friend classes",
            'seealso' : "%s.h" % self._trans_map['ClassName'],
            'upcase_filename' : "%s_%s_PRIVATE_H" % (self._trans_map['PATH_NAME'], self._trans_map['CLASS_NAME'])
            }
        self.update_trans_map(local_map)

        if esmc:
            str  = templates.HEAD_COMMENT_ESMC % self._trans_map
        else:
            str  = templates.HEAD_COMMENT % self._trans_map
        str = str.replace("@Id: ", "$Id: ")
        str += "\n" + templates.HEADER_START % self._trans_map
        str += "\n" + templates.INCLUDE % {'inc_file' : "\"%s/%s.h\"" % (self.dpath, self._trans_map['ClassName'])}
        str += "\n" + templates.INCLUDE % {'inc_file' : "\"%s/%s.h\"" % (self.bpath, self._trans_map['BaseClassName'])}
        str += "\n" + templates.INCLUDE % {'inc_file' : "\"%s/%s_private.h\"" % (self.bpath, self._trans_map['BaseClassName'])}
        str += "\n" + templates.INCLUDE % {'inc_file' : "\"nusmv/core/utils/defs.h\""}
        str += "\n"
        str += "\n" + templates.CLASS_DEFINITION_INHERITANCE % self._trans_map
        str += "\n" + templates.PROTECTED_METHODS_DECL % self._trans_map
        str += "\n" + templates.HEADER_END % self._trans_map
        return (str, self._trans_map['filename'])


    def build_public_header(self):
        """Returns the code as string, and the filename the code should be stored"""
        local_map = {
            'comment_header' : "CHeaderFile",
            'tailing'  : "*" * (79 - len("CHeaderFile") - 3),
            'filename' : "%s.h" % self._trans_map['ClassName'],
            'synopsis' : "Public interface of class '%s'" \
                            % self._trans_map['ClassName'],
            'description' : "",
            'seealso' : "%s.c" % self._trans_map['ClassName'],
            'upcase_filename' : "%s_%s_H" % (self._trans_map['PATH_NAME'], self._trans_map['CLASS_NAME'])
            }

        self.update_trans_map(local_map)

        derived = self._trans_map['BaseClassName']

        if esmc:
            str  = templates.HEAD_COMMENT_ESMC % self._trans_map
        else:
            str  = templates.HEAD_COMMENT % self._trans_map
        str = str.replace("@Id: ", "$Id: ")
        str += "\n" + templates.HEADER_START % self._trans_map
        if (derived): str += "\n" + templates.INCLUDE % {'inc_file' : "\"%s/%s.h\"" % (self.bpath, self._trans_map['BaseClassName'])}
        str += "\n" + templates.INCLUDE % {'inc_file' : "\"nusmv/core/utils/defs.h\""}
        str += "\n"
        str += "\n" + templates.IFC_SECTIONS % self._trans_map
        str += "\n" + templates.HEADER_END % self._trans_map

        return (str, self._trans_map['filename'])

    def dump_code(self, filename, code):
        answ = 'y'
        if os.path.isfile(filename):
            answ = raw_input("File '%s' already exists. Overwrite? [y] " % filename)
            pass
        if answ.lower() in ('', 'y'):
            f = open(filename, "w")
            f.write(code)
        else: print "Skipped generation of %s" % filename
        return

    def build(self, pkg_name, class_name, base_class_name):
        local_map = {
            'pkgname'  : pkg_name,
            'ClassName' : class_name,
            'class_name' : self.renamer.lowcase(class_name),
            'CLASS_NAME' : self.renamer.upcase(class_name),
            'BaseClassName' : base_class_name,
            'base_class_name' : self.renamer.lowcase(base_class_name),
            'BASE_CLASS_NAME' : self.renamer.upcase(base_class_name),
            'PATH_NAME' : rename_pathname(self.dpath),
            'ClassDescription' : ""
            }
        self.update_trans_map(local_map)
        derived = self._trans_map['BaseClassName']

        res = self.build_public_header()
        self.dump_code(res[1], res[0])

        if derived:
            res = self.build_private_header()
            self.dump_code(res[1], res[0])
            pass

        res = self.build_implementation()
        self.dump_code(res[1], res[0])
        return


    pass # end of class



def main(author, esmc, dpath, bpath, pkg_name, class_name, base_class_name=""):
    if author: b = ClassBuilder(esmc, dpath, bpath, {'author' : author})
    else: b = ClassBuilder(esmc, dpath, bpath)

    b.build(pkg_name, class_name, base_class_name)
    return


def print_usage():
    print "python %s [-a 'author name'] [-e] [-p path] [-b path] package_name ClassName [BaseClassName]" % sys.argv[0]
    print "-e      Must be specified if target tool is NOT NuSMV"
    print "-p path Destination pathname [default: working dir]"
    print "-b path Base Class pathname  [default: working dir]"
    print
    return

def get_es_tools_prefix(dpath):
    """
    Given a path, return the relative path to be used as prefix in es
    tools. This is achieved by stripping away the path up to a code or src
    directory.
    """

    res = os.path.abspath(dpath)
    index = res.find("/code/")
    if index == len(res) - 1:
        index = res.find("/src/")
    if index == len(res):
        print "Warning: cannot compute the es tool prefix, using the empty string"
        res = ""
    else:
        res = res[index:]
        if res[1] == 'c':
            res = res[6:]
        elif res[1] == 's':
            res = res[5:]
        else:
            print "Internal error"
            sys.exit(1)

    return res

def rename_pathname(dpath):
    """
    Given a pathname returns a string upcased and with slashes substituted py underscores
    """
    res = dpath.upper()
    res = res.replace("/", "_")
    res = res.replace("\\", "_")
    
    return res
    

if __name__ == "__main__":
    import sys
    import getopt

    try: ptlist, args = getopt.getopt(sys.argv[1:], 'hea:p:b:', ["help"])
    except getopt.GetoptError:
        print_usage()
        sys.exit(1)
        pass

    author = None
    esmc = False
    dpath = None
    bpath = None

    for o,a in ptlist:
        if o in ('-h', '--help'):
            print_usage()
            sys.exit(1)
            pass
        if o == '-a' and not author: author = a
        if o == '-e': esmc = True
        # add a safety check for dpath
        if o == '-p': dpath = a
        if o == '-b': bpath = a
        pass

    if not dpath:
        dpath = os.getcwd()
        dpath = get_es_tools_prefix(dpath)

    if not bpath:
        bpath = os.getcwd()
        bpath = get_es_tools_prefix(bpath)

    if len(args) not in range(2,4):
        print_usage()
        sys.exit(1)
        pass

    if not bpath and len(args) == 3:
        print "Warning: Base Class specified but option -b not provided: defaulting to working dir"

    main(author, esmc, dpath, bpath, *args)
    pass


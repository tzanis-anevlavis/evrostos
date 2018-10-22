import re
import sys
from os import path

def processFile(file, outStream):
    text = file.read()

    #removes all new lines:
    text = text.replace("\n", " ")
    text = removeBlanks(text)

    # removes all comments:
    text = removeComments(text)

    # searches for function definition:
    searchMatch(text, outStream)
    return


#removes all double spaces:
def removeBlanks(text):
    toks = text.split()
    text = ""
    for tok in toks: text = text + tok + " "
    return text


def removeComments(text):
    while(1):
        start = text.find("/*")
        if (start < 0): break
        end = text.find("*/", start)
        assert(end != -1) # comment must be closed!
        text = text[0:start] + text[end+2:]
        
        pass
    return removeBlanks(text)


def searchMatch(text, outStream):
    # matches fun name and previous declarations:
    reg_name = re.compile(r"((\w+\s?[*]\s)|(\w+\s))+(\w+)")

    # matches fun pars:
    reg_pars = reg = re.compile(r"\([\w\s,*]*\)\s?{")
    begin = 0
    while (1):
        man = reg_name.search(text, begin)
        if (man != None):
            begin = man.end()
            map = reg_pars.match(text, begin)
            if (map != None):
                outStream.write(
                    "%s %s; \n\n" \
                    % (man.group(0),
                       map.string[map.start():map.end()-1].strip()))
                begin = map.end()
                pass
        else: break
        pass
    return




######################################################################
# MAIN

if (len(sys.argv) < 2):
    print "Function prototypes extractor - v0.1"
    print "By RC 2002 - cavada@fbk.eu"
    print "Usage: [python] proto_extr.py --generate-headers FILE1, FILE2, ..., FILEN\n"
    sys.exit(1)
    pass

first_arg = 1
bUseStdout = 1
if (sys.argv[first_arg] == "--generate-headers"):
    bUseStdout = 0
    first_arg = 2
    pass

for name in sys.argv[first_arg:]:
    file = open(name, "r")
    if (bUseStdout): processFile(file, sys.stdout)
    else:
        nameWithoutExt = path.splitext(name)[0]
        file_header = open(nameWithoutExt+".h", "w")
        processFile(file, file_header)
        pass
    pass





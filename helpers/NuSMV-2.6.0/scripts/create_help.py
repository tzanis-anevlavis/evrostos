import re
from fnmatch import fnmatch
import os
import os.path
import collections
from html2text import HTML2Text
import optparse


CommandMatch = collections.namedtuple('CommandMatch',
                                      'found_index, command_name, description')


def convert_html2text(s, ignore_tags=(), indent_width=4, page_width=60):
    ignore_tags = [t.lower() for t in ignore_tags]
    parser = HTML2Text(ignore_tags, indent_width, page_width)
    parser.feed(s)
    parser.close()
    parser.generate()
    return parser.result


def find_next_command(text, start_index):
    """
    @returns: CommandMatch, or None if not found
    """

    pattern = (r'\\command{\s*(\w+)\s*}([\*\[\]\(\)\w\s\.\,\"\-\\\']+)'
               r'\\command_args{([^}]*)}(((?!\*\/).)*)')
    aux_text = text[start_index:]
    regex = re.compile(r'\/\*!\s*(%s)' % pattern, re.M | re.DOTALL)
    m = re.search(regex, aux_text)
    if m:
        converted = convert_html2text(m.group(1))
        regex1 = re.compile(pattern, re.M | re.DOTALL)
        m1 = re.search(regex1, converted)
        o = re.search(r'((?!\\sa).)*', m1.group(4), re.M | re.DOTALL)
        p = re.sub(r'\\se', r'Side Effects:', o.group(0))
        st = m1.group(3).replace('\[','[').replace('\]',']')
        new_text = ("COMMAND= %s  %s\nusage: %s %s\n%s" %
                    (m1.group(1), m1.group(2), m1.group(1), st, p))

        return (CommandMatch(m.start() + start_index, m1.group(1), new_text),
                m.end() + start_index)
    else:
        return (None, None)


##start_index=0
##(last_CommandMatch,end_index)=find_next_command(text, start_index)
##print last_CommandMatch


def find_all_commands_in_a_text(text):
    """
    @returns: list of CommandMatch
    """
    command_list = []
    start_index = 0
    end_index = 0
    last_CommandMatch = 1

    while last_CommandMatch is not None:
        last_CommandMatch, end_index = find_next_command(text, start_index)

        if last_CommandMatch is not None:
            command_list.append(last_CommandMatch)
            start_index = end_index + 2

    return command_list


##command_list=find_all_commands_in_a_text(text)
##print command_list
##for com in command_list:
##    print '#######################'
##    print com.description


def find_all_commands(search_paths, commands_list):
    """
     @returns: lista comandi
    """

    for path in search_paths:
        for root, subFolders, files in os.walk(path):
            for fn in files:
                if (fnmatch(fn, "*.[hc]")):
                    try:
                        _file = open(os.path.abspath(os.path.join(root, fn)))
                        text = _file.read()
                        _file.close()

                    except IOError:
                        #  print void because it print on cmdHelp.c
                        #  file so in case of exception it print a
                        #  wrong message on cmdHelp.c
                        print ''

                    else:
                        commands_list_file = find_all_commands_in_a_text(text)
                        commands_list += commands_list_file

    return commands_list


def print_commands_on_txt_files(path_trunk, commands_list):
    dir_name = 'nusmv/help/'
    help_dir = os.path.abspath(os.path.join(path_trunk, dir_name))

    if not os.path.exists(help_dir):
        os.mkdir(help_dir)
    for com in commands_list:
        file_name = "%sCmd.txt" % com.command_name
        try:
            F = open(os.path.abspath(os.path.join(help_dir, file_name)), 'w')
        except IOError:
            ## print 'Error: impossible to open the file' + file_name + '!!'
            print ''
        F.write(com.description)
        F.close()


def print_commands_for_csource(commands_list):
    for com in commands_list:
        rgx = re.sub(r'\\\"', r'"', com.description)
        rgx1 = re.sub(r'"', r'\"', rgx)
        rgx2 = re.sub(r'\n', r'\\n"\n"', rgx1)
        if (com != commands_list[-1]):
            print '{"%s", "%s"},' % (com.command_name, rgx2)
        else:
            print '{"%s", "%s"}' % (com.command_name, rgx2)


def get_opts():
    p = optparse.OptionParser()
    p.add_option('-t', '--txtfiles',
                 help="""create help documentation in files .txt.\n
Need an argument with the absolute path of where is
located the NuSMV build directory into the file system.
""")
    p.add_option('-c', '--csource',
                 help='create help documentation in for a c file',
                 action='store_true', dest='csource')
    p.add_option('-p', '--paths',
                 help='Specify search paths, separated by semicolon ";"',
                 default=os.getcwd())
    p.add_option('-n', '--number-of-commands',
                 help='calculate the number of commmands',
                 action='store_true')

    opts, args = p.parse_args()
    return opts, args


def main():
    #1 takes options from command line
    (opts, args) = get_opts()

    #2 if the param is -t i check if the path inserted of where
    #  put the txt files is correct else exit
    if opts.txtfiles:
        if not os.path.exists(opts.txtfiles):
            # print 'Error: path %s not found' % abs_path
            print ''
            exit(1)

    search_paths = [_dir if os.path.isabs(_dir)
                    else os.path.join(os.getcwd(), _dir)
                    for _dir in opts.paths.split(";")]

    not_found = [_dir
                 for _dir in search_paths
                 if not os.path.isdir(_dir)]

    if not_found:
        for _dir in not_found:
            print 'Error: path %s not found' % _dir
        exit(1)

    commands_list = find_all_commands(search_paths, [])

    #4 If param is -t i call the function for write .txt files
    if opts.txtfiles:
        print_commands_on_txt_files(opts.txtfiles, commands_list)

    #5 if param is -c i call function for create .c
    if opts.csource:
        print_commands_for_csource(commands_list)

    if opts.number_of_commands:
        print len(commands_list)


if __name__ == '__main__':
    main()

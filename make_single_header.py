import os

def add_header(header_name, included_headers, target_file):
    lines = [line for line in open(header_name)]

    for line in lines:
        line = line.rstrip()
        is_line_to_be_skipped = line.startswith('#include "')
        if line.startswith('#include <'):
            if line in included_headers:
                is_line_to_be_skipped = True
            else:
                included_headers.append(line)

        if not is_line_to_be_skipped:
            target_file.write(line + '\n')        


def add_internal_headers(included_headers, target_file): 
    internal_headers = ['iterbase', 'iterator_wrapper', 'iter_tuples', 'iteratoriterator']
    for internal_header in internal_headers:
        add_header('internal/' + internal_header + '.hpp', included_headers, target_file)


def add_headers(included_headers, target_file):
    directory = os.fsencode('./')
    
    for file in os.listdir(directory):
         filename = os.fsdecode(file)
         if filename.endswith(".hpp") and not filename.startswith("single_header"):
             add_header(filename, included_headers, target_file)


def run():
    with open('single_header.hpp', 'w') as target_file:
        included_headers = []
        add_internal_headers(included_headers, target_file)
        add_headers(included_headers, target_file)


if __name__ == '__main__':
    run()

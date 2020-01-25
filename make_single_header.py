import os

def add_header(header_name, index, included_headers, target_file):
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
        add_header('internal/' + internal_header + '.hpp', 0, included_headers, target_file)


def add_headers(included_headers, target_file):
    add_header('./filter.hpp', 8, included_headers, target_file)
    add_header('./starmap.hpp', 8, included_headers, target_file)
    directory = os.fsencode('./')
    for index, file in enumerate(sorted(os.listdir(directory))):
        filename = os.fsdecode(file)
        if filename.endswith('.hpp') and not filename in ['single_header.hpp', 'filter.hpp', 'starmap.hpp', 'zip_longest.hpp']:
            add_header(filename, 10 + index, included_headers, target_file)


def run():
    with open('single_header.hpp', 'w') as target_file:
        included_headers = []
        add_internal_headers(included_headers, target_file)
        add_headers(included_headers, target_file)


if __name__ == '__main__':
    run()

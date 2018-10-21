"""Extracts macro-definitions from a built rustdoc-path"""
import os
import sys

import bs4


def iter_macros_in_docs(path):
    """Prepare the thing"""
    for fname in sorted(os.listdir(path)):
        if not (fname.startswith('macro') and fname.endswith('.html')):
            continue
        with open(os.path.join(path, fname)) as docfile:
            body = docfile.read()
        body = bs4.BeautifulSoup(body, 'html.parser')
        for tag in body.find_all('pre'):
            classes = tag['class']
            if not ('rust' in classes and 'macro' in classes):
                continue
            macro = tag.get_text()
            yield macro.strip()


def build_array_from_docs(path):
    """Do the thing"""
    print(',\n'.join('r#"%s"#' % (m, ) for m in iter_macros_in_docs(path)))


if __name__ == '__main__':
    build_array_from_docs(sys.argv[1])

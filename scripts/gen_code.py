try:
    from yaml import CDumper as Dumper
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader, Dumper

import os
import sys
import yaml
import argparse
import collections

def read_drv_yaml_file(file_path):
    # open yaml file and read it
    with open(file_path) as _file:
        data = yaml.load(_file, Loader=Loader)
        return dict({k.lower().replace("-", "_"): v for k, v in data.items()})

def create_inc_comps(_dict, odir):
    # open file
    with open(os.path.join(odir, 'comps.inc'), 'w') as f:
        # loop through components and create use statements
        od = collections.OrderedDict(sorted(_dict['components'].items()))
        for k1, v1 in od.items():
            comp_name = k1
            comp_module = v1['module']
            f.write('use {}, only: {}SS => SetServices\n'.format(comp_module, comp_name))

def create_inc_macro(_dict, odir):
    # open file
    with open(os.path.join(odir, 'macros.inc'), 'w') as f:
        i = 1
        _str = []
        # loop through components and create statement
        od = collections.OrderedDict(sorted(_dict['components'].items()))
        for k1, v1 in od.items():
            comp_name = k1
            _str.append('compSS({})%s_ptr => {}SS'.format(i, comp_name))
            i = i+1
        # add macro line
        f.write('#define setSS() {}'.format('; '.join(_str)))

def main(argv):
    # default values
    ifile = 'nuopc_drv.yaml'
    odir = '.'

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--ifile', help='Input driver yaml file', required=True)
    parser.add_argument('--odir' , help='Output directory for generated code')
    args = parser.parse_args()

    if args.ifile:
        ifile = args.ifile
    if args.odir:
        odir = args.odir

    # read driver configuration yaml file
    dict_drv = read_drv_yaml_file(ifile)

    # create comps.inc
    create_inc_comps(dict_drv, odir)

    # create macros.inc
    create_inc_macro(dict_drv, odir)

if __name__== "__main__":
	main(sys.argv[1:])

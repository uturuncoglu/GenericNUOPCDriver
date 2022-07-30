import os
import re
import sys
import yaml
import argparse
import collections
try:
    from yaml import CDumper as Dumper
    from yaml import CLoader as Loader
except ImportError:
    from yaml import Loader, Dumper
from CIME.ParamGen.paramgen import ParamGen

def expand_func(varname):
    my_dict = {}

    # loop over list
    for var in glob_list:
        env_var = os.environ.get(var)
        if env_var is not None:
            my_dict[var] = '{}'.format(os.getenv(var))
        else:
            my_dict[var] = ''

    return my_dict[varname]

def read_drv_yaml_file(file_path):
    # open yaml file and read it
    if not os.path.exists(file_path):
        sys.exit('File not found: {}'.format(file_path))
    with open(file_path) as _file:
        data = yaml.load(_file, Loader=Loader)
        return dict({k.lower().replace("-", "_"): v for k, v in data.items()})

def gen_config(_dict):
    # global variable which is used by expand_func
    global glob_list
    glob_list = []

    # flag for driver
    driver = False
    if 'components' in _dict:
        driver = True

    # special care for driver
    if driver:
        # data structure to keep tracking file open mode
        append = {}

        # order list based on components
        od = collections.OrderedDict(sorted(_dict['components'].items()))

        # loop over components read component specific YAML files
        for k1, v1 in od.items():
            if 'drv' == k1:
                # get driver content
                _dict_comp = _dict['components'][k1] 
            else:
                # read component YAML file
                _dict_comp = read_drv_yaml_file(v1)

            # loop over config/s
            if 'config' in _dict_comp:
                for k2, v2 in _dict_comp['config'].items():
                    # set name for output file
                    if 'name' in v2:
                        ofile = v2['name']
                    else:
                        sys.exit("name is not given for '{}:{}' config section!".format(k1, k2))

                    # append to file or not
                    k3 = '{}_{}'.format(k2[0:3], ofile)
                    if k3 in append:
                        append[k3] = True 
                    else:
                        append[k3] = False
                  
                    # process content of config file
                    glob_list.clear()
                    if 'content' in v2:
                        # pass content to ParamGen
                        pg = ParamGen(v2['content'])

                        # loop over data and find dynamic variables like ${VAR}
                        for k4, v4 in pg.data.items():
                            for k5, v5 in v4.items():
                                # convert to string
                                value_str = str(v5['values']).strip()

                                # find start and end indices of each occurance
                                sind = [i for i in range(len(value_str)) if value_str.startswith('${', i)]
                                eind = [i+1 for i in range(len(value_str)) if value_str.startswith('}', i)]

                                # loop over them and create dictionary
                                for i, j in zip(sind, eind):
                                    env_var = value_str[i:j].replace('${', '').replace('}', '')
                                    glob_list.append(env_var)

                        # remove duplicates from list
                        glob_list = list(set(glob_list))

                        # replace dynamic variables with environment ones
                        pg.reduce(expand_func)

                        # write config file in specified format
                        if 'nuopc' in k2:
                            pg.write_nuopc(ofile, append=append[k3])
                        elif 'nml' in k2:
                            pg.write_nml(ofile, append=append[k3])
                        else:
                            sys.exit("{} format for config file is not supported!".format(k2))
                    else:
                        sys.exit("content is not given for '{}:{}' config section!".format(k1, k2))

def mode_type(x):
    if x.lower() == 'config' or x.lower() == 'input' or x.lower() == 'codegen':
        return x
    else:
        raise argparse.ArgumentTypeError('config|input|code are the expected option for mode!')

def main(argv):
    # default values
    ifile = 'nuopc_drv.yaml'
    odir = '.'
    mode = 'config'

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--ifile' , help='Input YAML file')
    parser.add_argument('--odir'  , help='Output directory')
    parser.add_argument('--mode'  , help='Script mode [config|input|codegen]', required=True, type=mode_type, nargs='?')
    args = parser.parse_args()

    if args.ifile:
        ifile = args.ifile
    if args.odir:
        odir = args.odir
    if args.mode:
        mode = args.mode

    # read configuration YAML file
    _dict = read_drv_yaml_file(ifile)

    # generate configuration files
    if mode.lower() == 'config':
        gen_config(_dict)

if __name__== "__main__":
    main(sys.argv[1:])

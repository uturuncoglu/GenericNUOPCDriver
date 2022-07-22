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
import hashlib
import ftplib
import boto3
from botocore import UNSIGNED
from botocore.client import Config

def read_drv_yaml_file(file_path):
    # open yaml file and read it
    if not os.path.exists(file_path):
        sys.exit('File not found: {}'.format(file_path))
    with open(file_path) as _file:
        data = yaml.load(_file, Loader=Loader)
        return dict({k.lower().replace("-", "_"): v for k, v in data.items()})

def recv_files(_dict):
    # loop through available components
    od = collections.OrderedDict(sorted(_dict['components'].items()))
    for k1, v1 in od.items():
        # query protocol, end_point and also list of files
        protocol = v1['input']['protocol']
        end_point = v1['input']['end_point']
        files = v1['input']['files']

        # call data retrieval routine for component
        if protocol == 'ftp':
            print('downloading files using {} protocol ...'.format(protocol))
            ftp_get(end_point, files)
        elif protocol == 's3':
            print('downloading files using {} protocol ...'.format(protocol))
            s3_get(end_point, files)
        else:
            sys.exit("unsupported protocol to download data: {}".format(protocol))

def ftp_get(end_point, files, wget=False):
    if wget:
        # get files
        for f in files:        
            cmd = 'wget -c {}:{}'.format(end_point, f)
            print("cmd is {}\n".format(cmd))
            os.system(cmd)
    else:    
        # open connection to server
        ftp = ftplib.FTP(end_point)
        ftp.login()

        # get files
        for f in files:
            ofile = os.path.basename(f)
            with open(ofile, "wb") as fout:
                print('downloading {}'.format(ofile)) 
                ftp.retrbinary(f"RETR {f}", fout.write)

        # close connection
        ftp.quit()

def s3_get(end_point, files, cli=False):
    # cli uses AWS command line interface
    if cli:
        # get files
        for f in files:
            cmd = 'aws s3 cp --no-sign-request s3://{}/{} .'.format(end_point, f)
            print("cmd is '{}'\n".format(cmd))
            os.system(cmd)
    else:
        # create an S3 access object, config option allows accessing anonymously
        s3 = boto3.client("s3", config=Config(signature_version=UNSIGNED))

        # get files
        for f in files:
            lfile = os.path.basename(f)

            # try to get checksum from s3 bucket
            try:
                md5sum_remote = s3.head_object(Bucket=end_point, Key=f)['ETag'][1:-1]
            except botocore.exceptions.ClientError:
                md5sum_remote = None

            # try to get checksum from local file, if exists
            found = False
            if os.path.exists(lfile):
                found = True
                md5sum_local = hashlib.md5(open(lfile,'rb').read()).hexdigest()
            else:
                md5sum_local = None

            # download file if local file not found or checksums not matched
            download = False
            if not found:
                print('file not found \'{}\''.format(lfile))
                download = True
            if md5sum_remote != md5sum_local:
                print('file \'{}\' is found but checksums are not matched!\ns3   :{}\nlocal:{}'.format(lfile, md5sum_remote, md5sum_local))
                download = True
            if download:    
                print('downloading \'{}\''.format(lfile)) 
                s3.download_file(Bucket=end_point, Key=f, Filename=lfile)
            else:
                print('file \'{}\' is found. skip downloading'.format(lfile))

def main(argv):
    # default values
    ifile = 'nuopc_drv.yaml'

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--ifile' , help='Input driver yaml file', required=True)
    args = parser.parse_args()

    if args.ifile:
        ifile = args.ifile

    # read driver configuration yaml file
    dict_drv = read_drv_yaml_file(ifile)

    # get files
    recv_files(dict_drv)

if __name__== "__main__":
	main(sys.argv[1:])

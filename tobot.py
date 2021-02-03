#!/usr/bin/python
import argparse,sys,math

def main(options):
    if options.input_path is not None:
        with open(options.input_path,'rb') as f:
            data = f.read()
    
    if options.output_path is not None:
        with open(options.output_path,'wb') as f:
            for x in data:
                u = chr(x | 0x100)
                f.write(u.encode('utf-8'))


if __name__=='__main__':
    parser=argparse.ArgumentParser()
    parser.add_argument('-i',dest='input_path',metavar='FILE',help='input from %(metavar)s')
    parser.add_argument('-o',dest='output_path',metavar='FILE',help='output to %(metavar)s')
    main(parser.parse_args())

#!/usr/bin/env python3

def main():
    args = get_program_arguments()
    print(args)

def get_program_arguments():
    import argparse

    # build and run parser
    parser = argparse.ArgumentParser(
            prog="kiss",
            usage="""
    kiss [ OPTIONS ] -t TYPE <INPUT [ >OUTPUT ]
    kiss [ OPTIONS ] [-t TYPE] INPUT [ >OUTPUT ]
            """,
            description="prototype implementation of the kiss-tools suite in a single python module",
            )
    inputTypes = [
            "asm",
            # TODO what do I call linked asm? "vm"?
            "hex",
            "bin",
            ]
    parser.add_argument("-t", "--type",
                        nargs = 1,
                        metavar="TYPE",
                        help="declare the type of the input",
                        choices = inputTypes,
                        dest="input_type",
                        )
    parser.add_argument("input_file",
                        nargs="?",
                        metavar="FILE",
                        help="file to process, or load into vm memory",
                        )
    parser.add_argument("-T", "-output-type",
                        nargs=1,
                        metavar="TYPE",
                        choices = inputTypes + [ "exec" ],
                        dest="output_type",
                        )
    parser.add_argument("-o", "--output",
                        nargs=1,
                        metavar="FILE",
                        help="specify an output file (default is stdout)",
                        )
    args = parser.parse_args()

    # normalize input type
    if args.input_type is None:
        if args.input_file is None:
            raise Exception() # TODO
        elif args.input_file.endswith(".kiss.bin"):
            args.input_type = "bin"
        elif args.input_file.endswith(".kiss.hex"):
            args.input_type = "hex"
        elif args.input_file.endswith(".kiss.asm"):
            args.input_type = "asm"
        else:
            raise Exception() # TODO

    # normalize output type
    if args.output_type is None:
        try:
            defaultOutputs = {
                    "asm": "hex", # TODO which I suppose means we link in-memory, or by using an output file
                    # TODO ig if I want to generate just label addresses, I'll do asm->asm
                    "hex": "bin",
                    "bin": "exec",
                    }
            args.output_type = defaultOutputs[args.input_type]
        except KeyError:
            raise Exception() # TODO

    # done
    return args

# TODO: loading a bin file
# load 32 bytes from the input, ignore and read another 32 if it starts with `#!`.
# the resulting 32 bytes are the kiss vm program header:
#   magic, support level, size of the text, entrypoint, max memory
#   the text is loaded into memory starting at address 16
# whether the program was from stdin or a file, we hook up the vm's input to (the rest of) stdin

if __name__ == "__main__": main()

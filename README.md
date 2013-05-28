# ConfSolve

**v0.7** -- Autonomous configuration management via constraint programming. My PhD research -- very beta.

## Requirements

Before running ConfSolve, you will need to install:

- Make (Windows users: [download](http://gnuwin32.sourceforge.net/packages/make.htm))

- [OCaml](http://caml.inria.fr/download.en.html) binary distribution

- [Gecode]( http://www.gecode.org/download.html) 3.7.3 (or later). Mac users should download the `.dmg`. Windows users should download the installer. Linux users should build from source and run `ldconfig` afterwards. **Windows only:** After installing Gecode, open a command prompt and run `fz -help`. If you get an error message then you need to
    install the appropriate [Visual C++ Runtime](http://www.microsoft.com/download/en/search.aspx?q=Microsoft%20Visual%20C%2b%2b%202010%20Redistributable%20Package).
    
- [MiniZinc](http://www.g12.csse.unimelb.edu.au/minizinc/download.html) 1.5.1 (or later). Windows users should download and run the installer. Mac/Linux users should extract the `.tar.gz` to a permanent location such as `/usr/local/bin` and run `./SETUP`, then add the MiniZinc `bin` directory to the `PATH`. The install works if `mzn2fzn --version` can be run. See [my MiniZinc guide](http://homepages.inf.ed.ac.uk/s0968244/confsolve/minizinc.html) for more information.

## Build from Source

    cd confsolve
    ./build

## Usage

There are five stages to solving a ConfSolve model:

### 1) csm2mzn
Translates a ConfSolve model (csm) into a MiniZinc model (mzn) on `stdout`.

    ocamlrun ./ocaml/bin/csm2mzn <filename.csm>
    
    usage: filename.csm [options]
      -c  Comment generated MiniZinc
      -m  Use the min-changes heuristic
      -p filename.cson  Paramaters
      -q  Quiet mode (no warnings)
      -s filename.cson  Solution to re-configure

In this case we pipe the output to `filename.mzn`:

    ocamlrun ./ocaml/bin/csm2mzn filename.csm > filename.mzn


### 2) mzn2fzn
The MiniZinc is translated FlatZinc using the G12 MiniZinc translator:

    mzn2fzn filename.mzn
    
This will generate a FlatZinc file `filename.fzn` and model description file `filename.ozn`.

### 3) fz

The FlatZinc model is solves using the Gecode constraint solver:

    fz -s filename.fzn
    
This will generate a partial FlatZinc solution file `filename.szn`

### 4) solns2out

The FlatZinc solution is recombined with any constants from the original model:

solns2out filename.ozn filename.szn -o filename-out.szn

This will generate a full FlatZinc solution file `filename-out.fzn`

### 5) szn2cson
The FlatZinc solution file is converted into a ConfSolve solution file (CSON) using the original model:

    ocamlrun ./ocaml/bin/szn2cson
    
    usage: filename.csm filename-out.szn [options]
      -p filename.cson  Paramaters
      --json  Output JSON instead of CSON

In this case we run:

     ocamlrun ./ocaml/bin/szn2cson filename.csm filename.szn
     
This generates `filename.cson`, which is a ConfSolve CSON solution.

## License

Apache License, Version 2.0

## Acknowledgements

This work was funded by Microsoft Research through their European PhD Scholarship Programme.
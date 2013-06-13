# ConfSolve

**v0.7** -- Autonomous configuration management via constraint programming. My PhD research -- very beta.

## Requirements

Before running ConfSolve, you will need to install:

- [Make](http://www.gnu.org/software/make/) (Windows users: [download](http://gnuwin32.sourceforge.net/packages/make.htm))

- [OCaml](http://caml.inria.fr/download.en.html) binary distribution

- [Gecode]( http://www.gecode.org/download.html) 3.7.3 (or later). Mac users should download the `.dmg`. Windows users should download the installer. Linux users should build from source and run `ldconfig` afterwards. **Windows only:** After installing Gecode, open a command prompt and run `fz -help`. If you get an error message then you need to
    install the appropriate [Visual C++ Runtime](http://www.microsoft.com/download/en/search.aspx?q=Microsoft%20Visual%20C%2b%2b%202010%20Redistributable%20Package).
    
- [MiniZinc](http://www.g12.csse.unimelb.edu.au/minizinc/download.html) 1.5.1 (or later). Windows users should download and run the installer. Mac/Linux users should extract the `.tar.gz` to a permanent location such as `/usr/local/bin` and run `./SETUP`, then add the MiniZinc `bin` directory to the `PATH`. The install works if `mzn2fzn --version` can be run. See [my MiniZinc guide](http://homepages.inf.ed.ac.uk/s0968244/confsolve/minizinc.html) for more information.

## Build from Source

    cd confsolve
    ./build

## Usage

Use the `solve` script to run the complete ConfSolve toolchain:

    ./solve filename.csm

Alternatively, run each stage yourself using [these instructions on the wiki](https://github.com/jahewson/confsolve/wiki/Manually-running-the-ConfSolve-toolchain).

## License

Apache License, Version 2.0

## Acknowledgements

This work was funded by Microsoft Research through their European PhD Scholarship Programme.
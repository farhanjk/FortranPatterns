# Facade pattern example

This directory contains a small home-theater facade demo. Build the module and driver together with any Fortran 2008 compiler:

```bash
gfortran -std=f2008 -Wall -Wextra -pedantic -fimplicit-none \
  facade_module.f90 facade_main.f90 -o facade_demo
./facade_demo
```

To verify the output against the expected transcript, run the provided helper script:

```bash
./run_tests.sh
```

The test runner requires `gfortran` to be available on your system.

# Fleet (4 DDR)

Run `sbt "runMain edu.stanford.fleet.StreamingWrapperDriver"` from the top level of this repo. A file called `StreamingWrapper.v` should be produced that contains the entire design.
You can adjust the memory controller parameters and change the processing unit in `StreamingWrapperDriver`. `Tests` provides examples of how to instantiate several different processing units.

To build the design using the standard AWS F1 flow, use the following aws-fpga directory:

https://github.com/jjthomas/aws-fpga (checkout branch `rapidwright_latest_shell`)

Copy `StreamingWrapper.v` to `aws-fpga/hdk/cl/examples/cl_dram_dma/design/sw.sv`. You can then run through the standard F1 flow for the `cl_dram_dma` example (see `aws-fpga/hdk/README.md` for details on the flow).

Example software to drive Fleet designs can be found in `aws-fpga/hdk/cl/examples/cl_dram_dma/software/runtime/test_dram_dma.c`. `aws-fpga/hdk/cl/examples/cl_dram_dma/README.md` has instructions on how to run it. The logic in `main` to load the input data will need to be adapted for your particular application.


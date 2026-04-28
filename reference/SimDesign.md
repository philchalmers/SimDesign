# Structure for Organizing Monte Carlo Simulation Designs

Structure for Organizing Monte Carlo Simulation Designs

## Details

Provides tools to help organize Monte Carlo simulations in R. The
package controls the structure and back-end of Monte Carlo simulations
by utilizing a general generate-analyse-summarise strategy. The
functions provided control common simulation issues such as
re-simulating non-convergent results, support parallel back-end
computations with proper random number generation within each simulation
condition, save and restore temporary files, aggregate results across
independent nodes, and provide native support for debugging. The primary
function for organizing the simulations is
[`runSimulation`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md),
while for array jobs submitting to HPC clusters (e.g., SLURM) see
[`runArraySimulation`](http://philchalmers.github.io/SimDesign/reference/runArraySimulation.md)
and the associated package vignettes.

For an in-depth tutorial of the package please refer to Chalmers and
Adkins (2020;
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)
). For an earlier didactic presentation of the package users can refer
to Sigal and Chalmers (2016;
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)
). Finally, see the associated wiki on Github
(<https://github.com/philchalmers/SimDesign/wiki>) for other tutorial
material, examples, and applications of `SimDesign` to real-world
simulations.

## References

Chalmers, R. P., & Adkins, M. C. (2020). Writing Effective and Reliable
Monte Carlo Simulations with the SimDesign Package.
`The Quantitative Methods for Psychology, 16`(4), 248-280.
[doi:10.20982/tqmp.16.4.p248](https://doi.org/10.20982/tqmp.16.4.p248)

Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching
statistics with Monte Carlo simulation.
`Journal of Statistics Education, 24`(3), 136-156.
[doi:10.1080/10691898.2016.1246953](https://doi.org/10.1080/10691898.2016.1246953)

## Author

Phil Chalmers <rphilip.chalmers@gmail.com>

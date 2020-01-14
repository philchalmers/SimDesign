#' Run a Monte Carlo simulation given a data.frame of conditions and simulation functions
#'
#' This function runs a Monte Carlo simulation study given a set of predefined simulation functions,
#' design conditions, and number of replications. Results can be saved as temporary files in case of interruptions
#' and may be restored by re-running \code{runSimulation}, provided that the respective temp
#' file can be found in the working directory. \code{runSimulation} supports parallel
#' and cluster computing, global and local debugging, error handling (including fail-safe
#' stopping when functions fail too often, even across nodes), provides bootstrap estimates of the
#' sampling variability (optional), and tracking of error and warning messages.
#' For convenience, all functions available in the R workspace are exported across all computational nodes
#' so that they are more easily accessible (however, other R objects are not, and therefore
#' must be passed to the \code{fixed_objects} input to become available across nodes).
#' For a didactic presentation of the package refer to Sigal and Chalmers
#' (2016; \doi{10.1080/10691898.2016.1246953}), and see the associated
#' wiki on Github (\url{https://github.com/philchalmers/SimDesign/wiki})
#' for other tutorial material, examples, and applications of \code{SimDesign} to real-world simulations.
#'
#' The strategy for organizing the Monte Carlo simulation work-flow is to
#'
#' \describe{
#'    \item{1)}{Define a suitable \code{Design} object (a \code{tibble} or \code{data.frame})
#'       containing fixed conditional
#'       information about the Monte Carlo simulations. This is often expedited by using the
#'       \code{\link{createDesign}} function, and if necessary the argument \code{subset}
#'       can be used to remove redundant or non-applicable rows}
#'    \item{2)}{Define the three step functions to generate the data (\code{\link{Generate}}; see also
#'       \url{https://CRAN.R-project.org/view=Distributions} for a list of distributions in R),
#'       analyse the generated data by computing the respective parameter estimates, detection rates,
#'       etc (\code{\link{Analyse}}), and finally summarise the results across the total
#'       number of replications (\code{\link{Summarise}}).
#'    }
#'    \item{3)}{Pass the above objects to the \code{runSimulation} function, and declare the
#'       number of replications to perform with the \code{replications} input. This function will accept
#'       a \code{design} data.frame object and will return a suitable data.frame object with the
#'       simulation results}
#'    \item{4)}{Analyze the output from \code{runSimulation}, possibly using ANOVA techniques
#'      (\code{\link{SimAnova}}) and generating suitable plots and tables}
#' }
#'
#' Expressing the above more succinctly, the functions to be called have the following form,
#' with the exact inputs listed
#'
#' \describe{
#'   \item{\code{Design <- createDesign(...)}}{}
#'   \item{\code{Generate <- function(condition, fixed_objects = NULL) \{...\} }}{}
#'   \item{\code{Analyse <- function(condition, dat, fixed_objects = NULL) \{...\} }}{}
#'   \item{\code{Summarise <- function(condition, results, fixed_objects = NULL) \{...\} }}{}
#'   \item{\code{res <- runSimulation(design=Design, replications, generate=Generate,
#'         analyse=Analyse, summarise=Summarise)}}{}
#' }
#'
#' The \code{condition} object above represents a single row from the \code{design} object, indicating
#' a unique Monte Carlo simulation condition. The \code{condition} object also contains two
#' additional elements to help track the simulation's state: an \code{ID} variable, indicating
#' the respective row number in the \code{design} object, and a \code{REPLICATION} element
#' indicating the replication iteration number. Mainly, these are included to help with debugging,
#' where users can easily locate the \code{r}th replication (e.g., \code{REPLICATION == 500})
#' within the \code{j}th row in the simulation design (e.g., \code{ID == 2}). The
#' \code{REPLICATION} input is also useful when temporarily saving files to the hard-drive
#' when calling external command line utilities (see examples on the wiki).
#'
#' For a skeleton version of the work-flow, which is often useful when initially defining a simulation,
#' use \code{\link{SimFunctions}}. This function will write template simulation code
#' to one/two files so that modifying the required functions and objects can begin immediately
#' with minimal error. This means that you can focus on your Monte Carlo simulation immediately rather
#' than worrying about the administrative code-work required to organize the simulation work-flow.
#'
#' Additional information for each condition are also contained in the object returned by
#' \code{runSimulation}: \code{REPLICATIONS} to indicate the number of Monte Carlo replications,
#' \code{SIM_TIME} to indicate how long (in seconds) it took to complete
#' all the Monte Carlo replications for each respective design condition,
#' \code{COMPLETED} to indicate the date in which the given simulation condition completed,
#' \code{SEED} for the integer values in the \code{seed} argument, and, if applicable,
#' \code{ERRORS} and \code{WARNINGS} which contain counts for the number of error or warning
#' messages that were caught (if no errors/warnings were observed these columns will be omitted).
#' Note that to extract the specific error and warnings messages see
#' \code{\link{SimExtract}}. Finally,
#' if \code{bootSE = TRUE} was included then the final right-most columns will contain the labels
#' \code{BOOT_SE.} followed by the name of the associated meta-statistic defined in \code{summarise()}.
#'
#' Additional examples, presentation files, and tutorials can be found on the package wiki located at
#' \url{https://github.com/philchalmers/SimDesign/wiki}.
#'
#' @section Saving data, results, seeds, and the simulation state:
#'
#' To conserve RAM, temporary objects (such as data generated across conditions and replications)
#' are discarded; however, these can be saved to the hard-disk by passing the appropriate flags.
#' For longer simulations it is recommended to use \code{save = TRUE} to temporarily save the
#' simulation state, and to use the \code{save_results} flag to write the analysis results
#' the to hard-disc.
#'
#' The use of the \code{save_seeds} option can be evoked to save R's \code{.Random.seed} state to allow
#' for complete reproducibility of each replication within each condition. These
#' individual \code{.Random.seed} terms can then be read in with the
#' \code{load_seed} input to reproduce the exact simulation state at any given replication. Most often though,
#' \code{save_seeds} is less useful since problematic seeds are automatically stored in the final
#' simulation object to allow for easier replicability of potentially problematic errors. Finally,
#' providing a vector of \code{seeds} is also possible to ensure
#' that each simulation condition is completely reproducible under the single/multi-core method selected.
#'
#' The \code{load_seed} input will also accept an integer vector corresponding to the exact
#' \code{.Random.seed} state. This is helpful because SimDesign also tracks these seeds for simulation
#' conditions that threw errors, where these values can be extracted via \code{SimExtract(..., what='error_seeds')}
#' function. The column names indicate the respective design row (first number), the order in which
#' the errors were thrown (second number), and finally the error message string (coerced to a proper
#' data.frame column name). After this data.frame object is extracted, individual columns can be passed to \code{load_seed}
#' to replicate the exact error issue that appeared (note that the \code{design} object must be indexed
#' manually to ensure that the correct design conditions is paired with this exact \code{.Random.seed} state).
#'
#' Finally, when the Monte Carlo simulation is complete
#' it is recommended to write the results to a hard-drive for safe keeping, particularly with the
#' \code{save} and \code{filename} arguments provided (for reasons that are more obvious in the parallel computation
#' descriptions below). Using the \code{filename} argument (along with \code{save = TRUE})
#' supplied is much safer than using something
#' like \code{\link{saveRDS}} directly because files will never accidentally be overwritten,
#' and instead a new file name will be created when a conflict arises; this type of safety
#' is prevalent in many aspects of the package and helps to avoid many unrecoverable (yet surprisingly common)
#' mistakes.
#'
#' @section Resuming temporary results:
#'
#' In the event of a computer crash, power outage, etc, if \code{save = TRUE} was used
#' then the original code used to execute \code{runSimulation()} need only be re-run to resume the simulation.
#' The saved temp file will be read into the function automatically, and the simulation will continue
#' one the condition where it left off before the simulation state was terminated.
#'
#' @section A note on parallel computing:
#'
#' When running simulations in parallel (either with \code{parallel = TRUE} or \code{MPI = TRUE})
#' R objects defined in the global environment will generally \emph{not} be visible across nodes.
#' Hence, you may see errors such as \code{Error: object 'something' not found} if you try to use an object
#' that is defined in the workspace but is not passed to \code{runSimulation}.
#' To avoid this type or error, simply pass additional objects to the
#' \code{fixed_objects} input (usually it's convenient to supply a named list of these objects).
#' Fortunately, however, \emph{custom functions defined in the global environment are exported across
#' nodes automatically}. This makes it convenient when writing code because custom functions will
#' always be available across nodes if they are visible in the R workspace. As well, note the
#' \code{packages} input to declare packages which must be loaded via \code{library()} in order to make
#' specific non-standard R functions available across nodes.
#'
#' @param design a \code{tibble} or \code{data.frame} object containing the Monte Carlo simulation
#'   conditions to be studied, where each row represents a unique condition and each column a factor
#'   to be varied. Note that if a \code{tibble} is supplied this object will be coerced to a
#'   \code{data.frame} internally. See \code{\link{createDesign}} for the standard approach
#'   to create this simulation design object
#'
#' @param generate user-defined data and parameter generating function.
#'   See \code{\link{Generate}} for details. Note that this argument may be omitted by the
#'   user if they wish to generate the data with the \code{analyse} step, but for real-world
#'   simulations this is generally not recommended
#'
#' @param analyse user-defined computation function which acts on the data generated from
#'   \code{\link{Generate}} (or, if \code{generate} was omitted, both generates and
#'   analyses the simulated data). See \code{\link{Analyse}} for details
#'
#' @param summarise optional (but highly recommended) user-defined summary function from \code{\link{Summarise}}
#'   to be used after all the replications have completed within each \code{design} condition. Omitting this function
#'   will return a list of \code{data.frame}s (or a single \code{data.frame}, if only one row in
#'   \code{design} is supplied) or, for more general objects (such as \code{list}s), a \code{list}
#'   containing the results returned form \code{\link{Analyse}}.
#'   Alternatively, the value \code{NA} can be passed to let the generate-analyse-summarise process to run as usual,
#'   where the summarise components are instead included only as a placeholder.
#'
#'   Ommiting this input is only recommended for didactic purposes because it leaves out a large amount of
#'   information (e.g., try-errors, warning messages, saving files, etc), can witness memory related issues,
#'   and generally is not as flexible internally. If users do not wish to supply a summarise function then it
#'   is is recommended to pass the values \code{NA} to indicate this function is deliberately omitted, but that
#'   the \code{save_results} option should be used to save the results during the simulation. This provides a
#'   more RAM friendly alternative to storing all the Generate-Analyse results in the working environment, where
#'   the Analysis results can be summarised at a later time
#'
#' @param replications number of replication to perform per condition (i.e., each row in \code{design}).
#'   Must be greater than 0
#'
#' @param fixed_objects (optional) an object (usually a named \code{list})
#'   containing additional user-defined objects
#'   that should remain fixed across conditions. This is useful when including
#'   long fixed vectors/matrices of population parameters, data
#'   that should be used across all conditions and replications (e.g., including a fixed design matrix
#'   for linear regression), or simply control constant global elements such as sample size
#'
#' @param parallel logical; use parallel processing from the \code{parallel} package over each
#'   unique condition?
#'
#' @param cl cluster object defined by \code{\link{makeCluster}} used to run code in parallel.
#'   If \code{NULL} and \code{parallel = TRUE}, a local cluster object will be defined which
#'   selects the maximum number cores available
#'   and will be stop the cluster when the simulation is complete. Note that supplying a \code{cl}
#'   object will automatically set the \code{parallel} argument to \code{TRUE}
#'
#' @param packages a character vector of external packages to be used during the simulation (e.g.,
#'   \code{c('MASS', 'extraDistr', 'simsem')} ). Use this input when \code{parallel = TRUE} or
#'   \code{MPI = TRUE} to use non-standard functions from additional packages,
#'   otherwise the functions must be made available by using explicit
#'   \code{\link{library}} or \code{\link{require}} calls within the provided simulation functions.
#'   Alternatively, functions can be called explicitly without attaching the package with the \code{::} operator
#'   (e.g., \code{extraDistr::rgumbel()})
#'
#' @param warnings_as_errors logical; treat warning messages as errors during the simulation? Default is FALSE,
#'   therefore warnings are only collected and not used to restart the data generation step
#'
#' @param save_results logical; save the results returned from \code{\link{Analyse}} to external
#'   \code{.rds} files located in the defined \code{save_results_dirname} directory/folder?
#'   Use this if you would like to keep track of the individual parameters returned from the analyses.
#'   Each saved object will contain a list of three elements containing the condition (row from \code{design}),
#'   results (as a \code{list} or \code{matrix}), and try-errors. When \code{TRUE}, a temp file will be used to track the simulation
#'   state (in case of power outages, crashes, etc). When \code{TRUE}, temporary files will also be saved
#'   to the working directory (in the same way as when \code{save = TRUE}).
#'   See \code{\link{SimResults}} for an example of how to read these \code{.rds} files back into R
#'   after the simulation is complete. Default is \code{FALSE}.
#'
#'   WARNING: saving results to your hard-drive can fill up space very quickly for larger simulations. Be sure to
#'   test this option using a smaller number of replications before the full Monte Carlo simulation is performed.
#'   See also \code{\link{reSummarise}} for applying summarise functions from saved
#'   simulation results
#'
#' @param save_seeds logical; save the \code{.Random.seed} states prior to performing each replication into
#'   plain text files located in the defined \code{save_seeds_dirname} directory/folder?
#'   Use this if you would like to keep track of the simulation state within each replication and design
#'   condition. Primarily, this is useful for completely replicating any cell in the simulation if need be,
#'   especially when tracking down hard-to-find errors and bugs. As well, see the \code{load_seed} input
#'   to load a given \code{.Random.seed} to exactly replicate the generated data and analysis state (mostly useful
#'   for debugging). When \code{TRUE}, temporary files will also be saved
#'   to the working directory (in the same way as when \code{save = TRUE}). Default is \code{FALSE}
#'
#'   Note, however, that this option is not typically necessary since the \code{.Random.seed} states for simulation
#'   replications that threw errors during the execution are automatically stored within the final simulation
#'   object, and can be extracted and investigated using \code{\link{SimExtract}}. Hence, this option is only of
#'   interest when \emph{all} of the replications must be reproducible, otherwise the defaults to \code{runSimulation}
#'   are likely sufficient for most simulation studies.
#'
#' @param edit this argument has been deprecated. Please use \code{debug} instead
#'
#' @param load_seed a character object indicating which file to load from when the \code{.Random.seed}s have
#'   be saved (after a call with \code{save_seeds = TRUE}), or an integer vector indicating the actual
#'   \code{.Random.seed} values. E.g., \code{load_seed = 'design-row-2/seed-1'}
#'   will load the first seed in the second row of the \code{design} input, or explicitly passing the 626 long
#'   elements from \code{.Random.seed} (see \code{\link{SimExtract}} to extract the seeds associated explicitly
#'   with errors during the simulation, where each column represents a unique seed).
#'   If the input is a character vector then it is important NOT
#'   to modify the \code{design} input object, otherwise the path may not point to the correct saved location, while
#'   if the input is an integer vector then it WILL be important to modify the \code{design} input in order to load this
#'   exact seed for the corresponding design row. Default is \code{NULL}
#'
#' @param filename (optional) the name of the \code{.rds} file to save the final simulation results to
#'   when \code{save = TRUE}. If the same file name already exists in the working
#'   directly at the time of saving then a new
#'   file will be generated instead and a warning will be thrown. This helps to avoid accidentally overwriting
#'   existing files. Default is \code{'SimDesign-results'}
#'
#' @param save_details a list pertaining to information regarding how and where files should be saved
#'   when the \code{save} or \code{save_results} flags are triggered.
#'
#'   \describe{
#'
#'     \item{\code{safe}}{logical; trigger whether safe-saving should be performed. When \code{TRUE} files
#'       will never be overwritten accidentally, and where appropriate the program will either stop or generate
#'       new files with unique names. Default is \code{TRUE}}
#'
#'     \item{\code{compname}}{name of the computer running the simulation. Normally this doesn't need
#'       to be modified, but in the event that a manual node breaks down while running a simulation the
#'       results from the temp files may be resumed on another computer by changing the name of the
#'       node to match the broken computer. Default is the result of evaluating \code{unname(Sys.info()['nodename'])}}
#'
#'     \item{\code{out_rootdir}}{root directory to save all files to. Default uses the
#'        current working directory}
#'
#'     \item{\code{tmpfilename}}{the name of the temporary \code{.rds} file when any of the \code{save} flags are used.
#'        This file will be read-in if it is in the working directory and the simulation will continue
#'        at the last point this file was saved
#'        (useful in case of power outages or broken nodes). Finally, this file will be deleted when the
#'        simulation is complete. Default is the system name (\code{compname}) appended
#'        to \code{'SIMDESIGN-TEMPFILE_'}}
#'
#'     \item{\code{save_results_dirname}}{a string indicating the name of the folder to save
#'       result objects to when \code{save_results = TRUE}. If a directory/folder does not exist
#'       in the current working directory then a unique one will be created automatically. Default is
#'       \code{'SimDesign-results_'} with the associated \code{compname} appended}
#'
#'     \item{\code{save_seeds_dirname}}{a string indicating the name of the folder to save
#'       \code{.Random.seed} objects to when \code{save_seeds = TRUE}. If a directory/folder does not exist
#'       in the current working directory then one will be created automatically. Default is
#'       \code{'SimDesign-seeds_'} with the associated \code{compname} appended}
#'
#'   }
#'
#' @param max_errors the simulation will terminate when more than this number of consecutive errors are thrown in any
#'   given condition. The purpose of this is to indicate that something fatally problematic is likely going
#'   wrong in the generate-analyse phases and should be inspected. Default is 50
#'
#' @param allow_na logical; should \code{NA}s be allowed in the analyse step as a valid result from the simulation
#'   analysis? Default is FALSE
#'
#' @param allow_nan logical; should \code{NaN}s be allowed in the analyse step as a valid result from the simulation
#'   analysis? Default is FALSE
#'
#' @param ncores number of cores to be used in parallel execution. Default uses all available
#'
#' @param MPI logical; use the \code{foreach} package in a form usable by MPI to run simulation
#'   in parallel on a cluster? Default is \code{FALSE}
#'
#' @param save logical; save the simulation state and final results to the hard-drive? This is useful
#'   for simulations which require an extended amount of time. When \code{TRUE}, a temp file
#'   will be created in the working directory which allows the simulation state to be saved
#'   and recovered (in case of power outages, crashes, etc). As well, triggering this flag will
#'   save any fatal \code{.Random.seed} states when conditions unexpectedly crash (where each seed
#'   is stored row-wise in an external .rds file), which provides a much easier mechanism
#'   to debug the issue (see \code{load_seed} for details).
#'
#'   Additionally, to recover your simulation at the last known
#'   location simply re-run the code you used to initially define the simulation and the external file
#'   will automatically be detected and read-in. Upon completion, the final results will
#'   be saved to the working directory, and the temp file will be removed. Default is \code{FALSE}
#'
#' @param debug a string indicating where to initiate a \code{browser()} call for editing and debugging.
#'   General options are \code{'none'} (default; no debugging), \code{'error'}, which starts the debugger
#'   when any error in the code is detected in one of three generate-analyse-summarise functions,
#'   and \code{'all'}, which debugs all the user defined functions regardless of whether an error was thrown
#'   or not. Specific options include: \code{'generate'}
#'   to debug the data simulation function, \code{'analyse'} to debug the computational function, and
#'   \code{'summarise'} to debug the aggregation function.
#'
#'   Alternatively, users may place \code{\link{browser}} calls within the respective functions for
#'   debugging at specific lines, which is useful when debugging based on conditional evaluations (e.g.,
#'   \code{if(this == 'problem') browser()}). Note that parallel computation flags
#'   will automatically be disabled when a \code{browser()} is detected
#'
#' @param seed a vector of integers to be used for reproducibility.
#'   The length of the vector must be equal the number of rows in \code{design}.
#'   This argument calls \code{\link{set.seed}} or
#'   \code{\link{clusterSetRNGStream}} for each condition, respectively,
#'   but will not be run when \code{MPI = TRUE}.
#'   Default randomly generates seeds within the range 1 to 2147483647 for each condition.
#'
#' @param progress logical; display a progress bar for each simulation condition?
#'   This is useful when simulations conditions take a long time to run.
#'   Uses the \code{pbapply} package to display the progress. Default is \code{FALSE}
#'
#' @param bootSE logical; perform a non-parametric bootstrap to compute bootstrap standard error
#'   estimates for the respective meta-statistics computed by the \code{Summarise} function?
#'   When \code{TRUE}, bootstrap samples for each row in \code{Design} will be obtained after
#'   the generate-analyse steps have obtain the simulation results to be summarised so that
#'   standard errors for each statistic can be computed. To compute large-sample confidence
#'   intervals given the bootstrap SE estimates see \code{\link{SimBoot}}.
#'
#'   This option is useful to approximate how accurate the resulting meta-statistic estimates
#'   were, particularly if the number of \code{replications} was relatively low (e.g., less than
#'   5000). If users prefer to obtain alternative bootstrap estimates then consider passing
#'   \code{save_results = TRUE}, reading the generate-analyse data into R via
#'   \code{\link{SimResults}}, and performing the bootstrap manually with function found in the
#'   external \code{boot} package
#'
#' @param boot_draws number of non-parametric bootstrap draws to sample for the \code{summarise}
#'   function after the generate-analyse replications are collected. Default is 1000
#'
#' @param store_results logical; store the complete tables of simulation results
#'   in the returned object? This is \code{FALSE} by default to help avoid RAM
#'   issues (see \code{save_results} as a more suitable alternative). To extract these results
#'   pass the returned object to \code{SimExtract(..., what = 'results')}, which will return a named list
#'   of all the simulation results for each condition
#'
#' @param stop_on_fatal logical; should the simulation be terminated immediately when
#'   the maximum number of consecutive errors (\code{max_errors}) is reached? If \code{FALSE},
#'   the simulation will continue as though errors did not occur, however a column
#'   \code{FATAL_TERMINATION} will be included in the resulting object indicating the final
#'   error message observed, and \code{NA} placeholders will be placed in all other row-elements. Default is
#'   \code{FALSE}
#'
#' @param verbose logical; print messages to the R console? Default is \code{TRUE}
#'
#' @return a \code{tibble} from the \code{dplyr} package (also of class \code{'SimDesign'})
#'   with the original \code{design} conditions in the left-most columns,
#'   simulation results and ERROR/WARNING's (if applicable) in the middle columns,
#'   and additional information (such as REPLICATIONS, SIM_TIME, COMPLETED, and SEED) in the right-most
#'   columns.
#'
#' @aliases runSimulation
#'
#' @seealso \code{\link{SimFunctions}}, \code{\link{createDesign}},
#'   \code{\link{Generate}}, \code{\link{Analyse}}, \code{\link{Summarise}},
#'   \code{\link{SimExtract}},
#'   \code{\link{reSummarise}}, \code{\link{SimClean}}, \code{\link{SimAnova}}, \code{\link{SimResults}},
#'   \code{\link{SimBoot}}, \code{\link{aggregate_simulations}}, \code{\link{Attach}},
#'   \code{\link{SimShiny}}
#'
#' @export runSimulation
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @references
#' Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
#' Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
#' \doi{10.1080/10691898.2016.1246953}
#'
#' @examples
#'
#' #-------------------------------------------------------------------------------
#' # Example 1: Sampling distribution of mean
#'
#' # This example demonstrate some of the simpler uses of SimDesign,
#' # particularly for classroom settings. The only factor varied in this simulation
#' # is sample size.
#'
#' # skeleton functions to be saved and edited
#' SimFunctions()
#'
#' #### Step 1 --- Define your conditions under study and create design data.frame
#'
#' Design <- data.frame(N = c(10, 20, 30))
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define generate, analyse, and summarise functions
#'
#' # help(Generate)
#' Generate <- function(condition, fixed_objects = NULL){
#'     dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
#'     dat
#' }
#'
#' # help(Analyse)
#' Analyse <- function(condition, dat, fixed_objects = NULL){
#'     ret <- mean(dat) # mean of the sample data vector
#'     ret
#' }
#'
#' # help(Summarise)
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     ret <- c(mu=mean(results), SE=sd(results)) # mean and SD summary of the sample means
#'     ret
#' }
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Collect results by looping over the rows in design
#'
#' # run the simulation
#' Final <- runSimulation(design=Design, replications=1000,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' Final
#'
#' # reproduce exact simulation
#' Final_rep <- runSimulation(design=Design, replications=1000, seed=Final$SEED,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' Final_rep
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Extras
#' # compare SEs estimates to the true SEs from the formula sigma/sqrt(N)
#' 5 / sqrt(Design$N)
#'
#' # To store the results from the analyse function either
#' #   a) omit a definition of of summarise(), or
#' #   b) pass save_results = TRUE to runSimulation() and read the results in with SimResults()
#'
#' # e.g., the a) approach
#' res <- runSimulation(design=Design, replications=1000,
#'                      generate=Generate, analyse=Analyse)
#' str(res)
#' head(res[[1]])
#'
#' # or b) approach
#' Final <- runSimulation(design=Design, replications=1000, save_results=TRUE,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' res <- SimResults(Final)
#' str(res)
#' head(res[[1]]$results)
#'
#' # remove the saved results from the hard-drive if you no longer want them
#' SimClean(results = TRUE)
#'
#'
#'
#'
#' #-------------------------------------------------------------------------------
#' # Example 2: t-test and Welch test when varying sample size, group sizes, and SDs
#'
#' # skeleton functions to be saved and edited
#' SimFunctions()
#'
#' \dontrun{
#' # in real-world simulations it's often better/easier to save
#' # these functions directly to your hard-drive with
#' SimFunctions('my-simulation')
#' }
#'
#' #### Step 1 --- Define your conditions under study and create design data.frame
#'
#' Design <- createDesign(sample_size = c(30, 60, 90, 120),
#'                        group_size_ratio = c(1, 4, 8),
#'                        standard_deviation_ratio = c(.5, 1, 2))
#' Design
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define generate, analyse, and summarise functions
#'
#' Generate <- function(condition, fixed_objects = NULL){
#'     N <- condition$sample_size      # alternatively, could use Attach() to make objects available
#'     grs <- condition$group_size_ratio
#'     sd <- condition$standard_deviation_ratio
#'     if(grs < 1){
#'         N2 <- N / (1/grs + 1)
#'         N1 <- N - N2
#'     } else {
#'         N1 <- N / (grs + 1)
#'         N2 <- N - N1
#'     }
#'     group1 <- rnorm(N1)
#'     group2 <- rnorm(N2, sd=sd)
#'     dat <- data.frame(group = c(rep('g1', N1), rep('g2', N2)), DV = c(group1, group2))
#'     dat
#' }
#'
#' Analyse <- function(condition, dat, fixed_objects = NULL){
#'     welch <- t.test(DV ~ group, dat)
#'     ind <- t.test(DV ~ group, dat, var.equal=TRUE)
#'
#'     # In this function the p values for the t-tests are returned,
#'     #  and make sure to name each element, for future reference
#'     ret <- c(welch = welch$p.value, independent = ind$p.value)
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     #find results of interest here (e.g., alpha < .1, .05, .01)
#'     ret <- EDR(results, alpha = .05)
#'     ret
#' }
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Collect results by looping over the rows in design
#'
#' # first, test to see if it works
#' res <- runSimulation(design=Design, replications=5, store_results=TRUE,
#'                      generate=Generate, analyse=Analyse, summarise=Summarise)
#' res
#'
#' \dontrun{
#' # complete run with 1000 replications per condition
#' res <- runSimulation(design=Design, replications=1000, parallel=TRUE,
#'                      generate=Generate, analyse=Analyse, summarise=Summarise)
#' res
#' View(res)
#'
#' ## save final results to a file upon completion (not run)
#' runSimulation(design=Design, replications=1000, parallel=TRUE, save=TRUE, filename = 'mysim',
#'               generate=Generate, analyse=Analyse, summarise=Summarise)
#'
#'
#'
#' ## Debug the generate function. See ?browser for help on debugging
#' ##   Type help to see available commands (e.g., n, c, where, ...),
#' ##   ls() to see what has been defined, and type Q to quit the debugger
#' runSimulation(design=Design, replications=1000,
#'               generate=Generate, analyse=Analyse, summarise=Summarise,
#'               parallel=TRUE, debug='generate')
#'
#' ## Alternatively, place a browser() within the desired function line to
#' ##   jump to a specific location
#' Summarise <- function(condition, results, fixed_objects = NULL){
#'     #find results of interest here (e.g., alpha < .1, .05, .01)
#'     browser()
#'     ret <- EDR(results[,nms], alpha = .05)
#'     ret
#' }
#'
#' runSimulation(design=Design, replications=1000,
#'               generate=Generate, analyse=Analyse, summarise=Summarise,
#'               parallel=TRUE)
#'
#'
#'
#'
#' ## EXTRA: To run the simulation on a MPI cluster, use the following setup on each node (not run)
#' # library(doMPI)
#' # cl <- startMPIcluster()
#' # registerDoMPI(cl)
#' # Final <- runSimulation(design=Design, replications=1000, MPI=TRUE, save=TRUE,
#' #                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' # saveRDS(Final, 'mysim.rds')
#' # closeCluster(cl)
#' # mpi.quit()
#'
#'
#' ## Similarly, run simulation on a network linked via ssh
#' ##  (two way ssh key-paired connection must be possible between master and slave nodes)
#' ##
#' ## define IP addresses, including primary IP
#' # primary <- '192.168.2.20'
#' # IPs <- list(
#' #     list(host=primary, user='phil', ncore=8),
#' #     list(host='192.168.2.17', user='phil', ncore=8)
#' # )
#' # spec <- lapply(IPs, function(IP)
#' #                    rep(list(list(host=IP$host, user=IP$user)), IP$ncore))
#' # spec <- unlist(spec, recursive=FALSE)
#' #
#' # cl <- parallel::makeCluster(type='PSOCK', master=primary, spec=spec)
#' # res <- runSimulation(design=Design, replications=1000, parallel = TRUE, save=TRUE,
#' #                      generate=Generate, analyse=Analyse, summarise=Summarise, cl=cl)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' ###### Post-analysis: Analyze the results via functions like lm() or SimAnova(), and create
#' ###### tables(dplyr) or plots (ggplot2) to help visualize the results.
#' ###### This is where you get to be a data analyst!
#'
#' library(dplyr)
#' res %>% summarise(mean(welch), mean(independent))
#' res %>% group_by(standard_deviation_ratio, group_size_ratio) %>%
#'    summarise(mean(welch), mean(independent))
#'
#' # quick ANOVA analysis method with all two-way interactions
#' SimAnova( ~ (sample_size + group_size_ratio + standard_deviation_ratio)^2, res,
#'   rates = TRUE)
#'
#' # or more specific ANOVAs
#' SimAnova(independent ~ (group_size_ratio + standard_deviation_ratio)^2,
#'     res, rates = TRUE)
#'
#' # make some plots
#' library(ggplot2)
#' library(tidyr)
#' dd <- res %>%
#'    select(group_size_ratio, standard_deviation_ratio, welch, independent) %>%
#'    pivot_longer(cols=c('welch', 'independent'), names_to = 'stats')
#' dd
#'
#' ggplot(dd, aes(factor(group_size_ratio), value)) + geom_boxplot() +
#'     geom_abline(intercept=0.05, slope=0, col = 'red') +
#'     geom_abline(intercept=0.075, slope=0, col = 'red', linetype='dotted') +
#'     geom_abline(intercept=0.025, slope=0, col = 'red', linetype='dotted') +
#'     facet_wrap(~stats)
#'
#' ggplot(dd, aes(factor(group_size_ratio), value, fill = factor(standard_deviation_ratio))) +
#'     geom_boxplot() + geom_abline(intercept=0.05, slope=0, col = 'red') +
#'     geom_abline(intercept=0.075, slope=0, col = 'red', linetype='dotted') +
#'     geom_abline(intercept=0.025, slope=0, col = 'red', linetype='dotted') +
#'     facet_grid(stats~standard_deviation_ratio) +
#'     theme(legend.position = 'none')
#'
#' }
#'
runSimulation <- function(design, replications, generate, analyse, summarise,
                          fixed_objects = NULL, packages = NULL, bootSE = FALSE,
                          boot_draws = 1000L, filename = 'SimDesign-results',
                          seed = rint(nrow(design), min=1L, max = 2147483647L),
                          save = FALSE, save_results = FALSE, store_results = FALSE,
                          warnings_as_errors = FALSE, save_seeds = FALSE, load_seed = NULL,
                          parallel = FALSE, ncores = parallel::detectCores(), cl = NULL, MPI = FALSE,
                          max_errors = 50L, save_details = list(), debug = 'none', progress = TRUE,
                          allow_na = FALSE, allow_nan = FALSE, stop_on_fatal = FALSE,
                          edit = 'none', verbose = TRUE)
{
    if(edit != 'none'){
        warning('The edit argument has been deprecated. Please use \'debug\' instead.', call. = FALSE)
        debug <- edit
    }
    stopifnot(!missing(analyse))
    if(missing(generate) && !missing(analyse))
        generate <- function(condition, dat, fixed_objects = NULL){}
    NA_summarise <- FALSE
    if(!missing(summarise)){
        NA_summarise <- if(!is.function(summarise) && is.na(summarise)) TRUE else FALSE
        if(NA_summarise){
            summarise <- function(condition, results, fixed_objects = NULL){0}
            if(!save_results)
                message('NA value for summarise input supplied; automatically setting save_results to TRUE\n')
            save <- save_results <- TRUE
        }
    }
    if(!all(names(save_results) %in%
            c('compname', 'tmpfilename', 'save_results_dirname')))
        stop('save_details contains elements that are not supported', call.=FALSE)

    compname <- save_details$compname
    safe <- save_details$safe
    out_rootdir <- save_details$out_rootdir
    tmpfilename <- save_details$tmpfilename
    save_results_dirname <- save_details$save_results_dirname
    save_seeds_dirname <- save_details$save_seeds_dirname

    if(!verbose) progress <- FALSE
    if(is.null(compname)) compname <- Sys.info()['nodename']
    if(is.null(safe)) safe <- TRUE
    if(is.null(out_rootdir)) { out_rootdir <- '.' } else { dir.create(out_rootdir, showWarnings=FALSE) }
    if(is.null(tmpfilename)) tmpfilename <- paste0('SIMDESIGN-TEMPFILE_', compname, '.rds')
    if(is.null(save_results_dirname)) save_results_dirname <- paste0('SimDesign-results_', compname)
    if(is.null(save_seeds_dirname)) save_seeds_dirname <- paste0('SimDesign-seeds_', compname)
    if(!is.null(filename)){
        if(grepl('\\.rds', filename))
            filename <- gsub('\\.rds', '', filename)
    }
    if(!is.null(cl)) parallel <- TRUE
    if(!is.null(load_seed)) seed <- NULL
    debug <- tolower(debug)
    summarise_asis <- FALSE
    if(missing(summarise)){
        summarise <- function(condition, results, fixed_objects = NULL) results
        summarise_asis <- TRUE
        stored_time <- 0
        if(save || save_results)
            message("save-based inputs not used when summarise input is missing. Consider passing summarise=NA instead")
        save <- save_results <- FALSE
    }
    Functions <- list(generate=generate, analyse=analyse, summarise=summarise)
    dummy_run <- FALSE
    if(missing(design)){
        design <- data.frame(dummy_run=NA)
        dummy_run <- TRUE
    }
    if(nrow(design) == 1L) verbose <- FALSE
    stopifnot(!missing(replications))
    replications <- as.integer(replications)
    if(!is.null(seed))
        stopifnot(nrow(design) == length(seed))
    debug <- tolower(debug)
    if(!save && any(save_results, save_seeds)) filename <- NULL
    for(i in names(Functions)){
        fms <- names(formals(Functions[[i]]))
        truefms <- switch(i,
                          generate  = c('condition', 'fixed_objects'),
                          analyse = c('dat', 'condition', 'fixed_objects'),
                          summarise = c('results', 'condition', 'fixed_objects'))
        if(!all(truefms %in% fms))
            stop(paste0('Function arguments for ', i, ' are not correct.'), call. = FALSE)
    }
    start <- 1L; end <- nrow(design)
    if(!is.null(load_seed)){
        save <- save_seeds <- parallel <- MPI <- FALSE
        replications <- 1L
        if(is.character(load_seed)){
            load_seed2 <- gsub('design-row-', '', load_seed)
            start <- end <- as.numeric(gsub('/.*', '', load_seed2))
            load_seed <- paste0(save_seeds_dirname, '/', load_seed)
            load_seed <- as.integer(scan(load_seed, sep = ' ', quiet = TRUE))
        }
        stopifnot(is.integer(load_seed))
    }
    if(MPI){
        parallel <- FALSE
        verbose <- FALSE
    }
    packages <- c('SimDesign', packages)
    char_functions <- deparse(substitute(Functions[[i]]))
    if(any(grepl('browser\\(', char_functions))){
        if(verbose && parallel)
            message(paste0('A browser() call was detected. Parallel processing/object ',
                    'saving will be disabled while visible'))
        save <- save_results <- save_seeds <- parallel <- MPI <- FALSE
    }
    if(any(grepl('attach\\(', char_functions)))
        stop('Did you mean to use Attach() instead of attach()?', call.=FALSE)
    was_tibble <- FALSE
    if(is(design, 'tbl') || is(design, 'tbl_df')){
        design <- as.data.frame(design)
        was_tibble <- TRUE
    }
    if(!is(design, 'data.frame'))
        stop('design must be a data.frame or tibble object', call. = FALSE)
    if(replications < 1L)
        stop('number of replications must be greater than or equal to 1', call. = FALSE)
    if(!(debug %in% c('none', 'analyse', 'generate', 'summarise', 'all', 'recover', 'error')))
        stop('debug input is not valid', call. = FALSE)
    if(any(names(design) == 'REPLICATION'))
        stop("REPLICATION is a reserved keyword in the design object. Please use another name", call.=FALSE)
    else design <- data.frame(REPLICATION=integer(nrow(design)), design)
    if(!any(names(design) == 'ID')){
        design <- data.frame(ID=1L:nrow(design), design)
    } else stopifnot(length(unique(design$ID)) == nrow(design))
    use_try  <- !(debug %in% c('error', 'recover'))
    if(debug != 'none'){
        save <- save_results <- save_seeds <- FALSE
        if(!(debug %in% 'summarise')) parallel <- MPI <- FALSE
        if(debug == 'all'){
            debug(Functions[['generate']]); debug(Functions[['analyse']])
            debug(Functions[['summarise']])
            on.exit({undebug(Functions[['generate']]); undebug(Functions[['analyse']])
                undebug(Functions[['summarise']])})
        } else {
            debug(Functions[[debug]])
            on.exit(undebug(Functions[[debug]]))
        }
    }
    export_funs <- parent_env_fun()
    if(parallel){
        if(is.null(cl)){
            cl <- parallel::makeCluster(ncores)
            on.exit(parallel::stopCluster(cl))
        }
        parallel::clusterExport(cl=cl, export_funs, envir = parent.frame(1L))
    }
    Result_list <- stored_Results_list <- vector('list', nrow(design))
    names(Result_list) <- names(stored_Results_list) <- rownames(design)
    time0 <- time1 <- proc.time()[3L]
    files <- dir(out_rootdir)
    if(!MPI && any(files == tmpfilename) && is.null(load_seed)){
        if(verbose)
            message(sprintf('Resuming simulation from %s file with %i replications.',
                            file.path(out_rootdir, tmpfilename), replications))
        Result_list <- readRDS(file.path(out_rootdir, tmpfilename))
        if(!is.null(Result_list[[1L]]$REPLICATIONS))
            replications <- Result_list[[1L]]$REPLICATIONS
        if(nrow(design) != length(Result_list)){
            if(nrow(design) < length(Result_list))
                Result_list <- Result_list[1:nrow(design)]
            else if(nrow(design) > length(Result_list)){
                tmp_new <- vector('list', nrow(design))
                names(tmp_new) <- 1L:nrow(design)
                tmp_new[1:length(Result_list)] <- Result_list
                Result_list <- tmp_new
            }
        }
        start <- min(which(sapply(Result_list, is.null)))
        time0 <- time1 - Result_list[[start-1L]]$SIM_TIME
    }
    if(file.exists(tmpfilename)){
        tmp <- attr(Result_list, 'SimDesign_names')
        save_results_dirname <- tmp['save_results_dirname']
        save_seeds_dirname <- tmp['save_seeds_dirname']
    }
    if(save_results){
        save <- TRUE
        if(!file.exists(file.path(out_rootdir, tmpfilename))) {
            if(safe){
                tmp <- save_results_dirname
                count <- 1L
                while(dir.exists(file.path(out_rootdir, save_results_dirname))) {
                    save_results_dirname <- paste0(tmp, '_', count)
                    count <- count + 1L
                }
                if(tmp != save_results_dirname && verbose)
                    message(sprintf('%s already exists; using %s directory instead',
                                    file.path(out_rootdir, tmp), file.path(out_rootdir, save_results_dirname)))
            }
            dir.create(file.path(out_rootdir, save_results_dirname))
        }
        if(!(length(dir(file.path(out_rootdir, save_results_dirname))) %in% c(start - 1L, start)))
            stop('save_results_dirname not starting from correct location according to tempfile',
                 call.=FALSE)
    }
    if(save_seeds){
        save <- TRUE
        if(!file.exists(file.path(out_rootdir, tmpfilename))) {
            if(safe){
                tmp <- save_seeds_dirname
                count <- 1L
                while(dir.exists(file.path(out_rootdir, save_seeds_dirname))) {
                    save_seeds_dirname <- paste0(tmp, '_', count)
                    count <- count + 1L
                }
                if(tmp != save_seeds_dirname && verbose)
                    message(sprintf('%s already exists; using %s directory instead',
                                    file.path(out_rootdir, tmp), file.path(out_rootdir, save_seeds_dirname)))
            }
            dir.create(file.path(out_rootdir, save_seeds_dirname))
        }
    }
    if(safe && (parallel || MPI)){
        # this is great because it also primes the pipes
        tmp <- packages[packages != 'SimDesign']
        if(!length(tmp)) tmp <- 'stats'
        for(i in 1:length(tmp)){
            packs <- if(parallel){
                try(table(parallel::parSapply(cl, rep(tmp[i], each=length(cl)*2),
                                                   get_packages)))
            } else {
                p <- character()
                try(table(foreach(p=rep(tmp[i], each=length(cl)*2L)) %dopar% get_packages(p)))
            }
            if(tmp[i] == 'stats') next
            if(length(packs) > 1L)
                message(sprintf('Warning message:\nVersions of %s differ across clusters: %s',
                                tmp[i], paste0(names(packs), collapse = ', ')))
        }
    }
    if(is.null(attr(Result_list, 'SimDesign_names')))
        attr(Result_list, 'SimDesign_names') <-
        c(save_results_dirname=file.path(out_rootdir, save_results_dirname),
          save_seeds_dirname=file.path(out_rootdir, save_seeds_dirname))
    if(progress) verbose <- TRUE
    for(i in start:end){
        time0 <- proc.time()[3L]
        if(summarise_asis){
            if(verbose)
                print_progress(i, nrow(design), stored_time=stored_time, progress=progress)
            Result_list[[i]] <- Analysis(Functions=Functions,
                                         condition=if(was_tibble) dplyr::as_tibble(design[i,]) else design[i,],
                                         replications=replications,
                                         fixed_objects=fixed_objects,
                                         cl=cl, MPI=MPI, seed=seed,
                                         bootSE=bootSE, boot_draws=boot_draws,
                                         save=save, allow_na=allow_na, allow_nan=allow_nan,
                                         save_results=save_results,
                                         save_results_out_rootdir=out_rootdir,
                                         save_results_dirname=save_results_dirname,
                                         save_seeds=save_seeds, summarise_asis=summarise_asis,
                                         save_seeds_dirname=save_seeds_dirname,
                                         max_errors=max_errors, packages=packages,
                                         load_seed=load_seed, export_funs=export_funs,
                                         warnings_as_errors=warnings_as_errors,
                                         progress=progress, store_results=FALSE, use_try=use_try,
                                         stop_on_fatal=stop_on_fatal)
            time1 <- proc.time()[3L]
            stored_time <- stored_time + (time1 - time0)
        } else {
            stored_time <- do.call(c, lapply(Result_list, function(x) x$SIM_TIME))
            if(verbose)
                print_progress(i, nrow(design), stored_time=stored_time, progress=progress)
            if(save_seeds)
                dir.create(file.path(out_rootdir,
                                     paste0(save_seeds_dirname, '/design-row-', i)), showWarnings = FALSE)
            tmp <- Analysis(Functions=Functions,
                            condition=if(was_tibble) dplyr::as_tibble(design[i,]) else design[i,],
                            replications=replications,
                            fixed_objects=fixed_objects,
                            cl=cl, MPI=MPI, seed=seed,
                            bootSE=bootSE, boot_draws=boot_draws,
                            save=save, allow_na=allow_na, allow_nan=allow_nan,
                            save_results=save_results,
                            save_results_out_rootdir = out_rootdir,
                            save_results_dirname=save_results_dirname,
                            save_seeds=save_seeds, summarise_asis=summarise_asis,
                            save_seeds_dirname=save_seeds_dirname,
                            max_errors=max_errors, packages=packages,
                            load_seed=load_seed, export_funs=export_funs,
                            warnings_as_errors=warnings_as_errors,
                            progress=progress, store_results=store_results, use_try=use_try,
                            stop_on_fatal=stop_on_fatal)
            if(store_results){
                stored_Results_list[[i]] <- attr(tmp, 'full_results')
                attr(tmp, 'full_results') <- NULL
            }
            Result_list[[i]] <- data.frame(design[i, ], as.list(tmp),
                                           check.names=FALSE)
            attr(Result_list[[i]], 'error_seeds') <- attr(tmp, 'error_seeds')
            Result_list[[i]]$SIM_TIME <- proc.time()[3L] - time0
            Result_list[[i]]$COMPLETED <- date()
            if(save || save_results)
                saveRDS(Result_list, file.path(out_rootdir, tmpfilename))
            time1 <- proc.time()[3L]
            Result_list[[i]]$SIM_TIME <- time1 - time0

        }
    }
    attr(Result_list, 'SimDesign_names') <- NULL
    if(NA_summarise){
        Result_list <- lapply(Result_list, function(x){
            x$value <- NULL
            x
        })
    }
    if(summarise_asis){
        design$ID <- design$REPLICATION <- NULL
        nms <- colnames(design)
        nms2 <- matrix(character(0L), nrow(design), ncol(design))
        for(i in 1L:ncol(design))
            nms2[,i] <- paste0(nms[i], '=', design[,i], if(i < ncol(design)) '; ')
        nms2 <- apply(nms2, 1L, paste0, collapse='')
        names(Result_list) <- nms2
        if(is.list(Result_list[[1L]][[1L]]))
            for(i in seq_len(length(Result_list)))
                attr(Result_list[[i]][[1L]], 'try_errors') <-
            attr(Result_list[[i]][[1L]], 'try_error_seeds') <- NULL
        if(nrow(design) == 1L) Result_list <- Result_list[[1L]]
        return(Result_list)
    }
    stored_time <- do.call(c, lapply(Result_list, function(x) x$SIM_TIME))
    if(verbose)
        message('\nSimulation complete. Total execution time: ', timeFormater(sum(stored_time)), "\n")
    stored_time <- do.call(c, lapply(Result_list, function(x) x$SIM_TIME))
    error_seeds <- data.frame(do.call(cbind, lapply(1L:length(Result_list), function(x){
        ret <- attr(Result_list[[x]], "error_seeds")
        if(length(ret) == 0L || nrow(ret) == 0L) return(NULL)
        rownames(ret) <- paste0("Design_row_", x, '.', 1L:nrow(ret), ": ",
                                rownames(ret))
        t(ret)
    })))
    Final <- plyr::rbind.fill(Result_list)
    if(!stop_on_fatal && any(colnames(Final) == 'FATAL_TERMINATION')){
        warning('One or more design rows were fatally terminated. Please inspect/debug row(s): ',
                paste(which(!is.na(Final$FATAL_TERMINATION)), collapse=','), call.=FALSE)
    }
    SIM_TIME <- Final$SIM_TIME
    COMPLETED <- Final$COMPLETED
    Final$SIM_TIME <- Final$ID <- Final$COMPLETED <-
        Final$REPLICATIONS <- Final$REPLICATION <- NULL
    Final <- data.frame(Final, REPLICATIONS=replications, SIM_TIME, COMPLETED, check.names=FALSE,
                        stringsAsFactors=FALSE)
    if(is.null(Final$SEED)) Final$SEED <- NA
    if(!is.null(seed)) Final$SEED <- seed
    if(!is.null(filename) && safe){ #save file
        files <- dir(out_rootdir)
        filename0 <- filename
        count <- 1L
        # create a new file name if old one exists, and throw warning
        while(TRUE){
            filename <- paste0(filename, '.rds')
            if(filename %in% files){
                filename <- paste0(filename0, '-', count)
                count <- count + 1L
            } else break
        }
        if(count > 1L)
            if(verbose && save)
                message(paste0('\nWARNING:\n', filename0, ' existed in the working directory.
                               Using a unique file name instead.\n'))
    }
    dn <- colnames(design)
    dn <- dn[!(dn %in% c('ID', 'REPLICATION'))]
    ten <- colnames(Final)[grepl('ERROR:', colnames(Final))]
    wen <- colnames(Final)[grepl('WARNING:', colnames(Final))]
    ERROR_msg <- Final[ ,ten, drop=FALSE]
    WARNING_msg <- Final[ ,wen, drop=FALSE]
    ERRORS <- as.integer(rowSums(ERROR_msg, na.rm = TRUE))
    WARNINGS <- as.integer(rowSums(WARNING_msg, na.rm = TRUE))
    en <- c('REPLICATIONS', 'SIM_TIME', 'COMPLETED', 'SEED')
    bsen <- colnames(Final)[grepl('BOOT_SE.', colnames(Final))]
    sn <- colnames(Final)[!(colnames(Final) %in% c(dn, en, ten, wen, bsen))]
    Final <- data.frame(Final[ ,c(dn, sn, bsen, en)], ERRORS, WARNINGS,
                                         check.names = FALSE)
    if(all(ERRORS == 0)) Final$ERRORS <- NULL
    if(all(WARNINGS == 0)) Final$WARNINGS <- NULL
    Final <- dplyr::as_tibble(Final)
    attr(Final, 'design_names') <-
        list(design=dn, sim=sn, bootSE=bsen, extra=en, errors='ERRORS', warnings="WARNINGS")
    if(length(packages) > 1L){
        pack <- packages[packages != 'SimDesign']
        versions <- character(length(pack))
        for(i in 1L:length(pack))
            versions[i] <- as.character(packageVersion(pack[i]))
        pack_vers <- data.frame(packages=pack, versions=versions)
    } else pack_vers <- NULL
    pick <- c(save_results, save_seeds)
    if(!is.null(filename)) pick <- c(save, pick)
    attr(Final, "ERROR_msg") <- dplyr::as_tibble(ERROR_msg)
    attr(Final, "WARNING_msg") <- dplyr::as_tibble(WARNING_msg)
    attr(Final, 'extra_info') <- list(sessionInfo = sessionInfo(), packages=pack_vers,
                                      save_info = c(filename=filename,
                                                    save_results_dirname=save_results_dirname,
                                                    save_seeds_dirname=save_seeds_dirname)[pick],
                                      ncores = if(parallel) length(cl) else if(MPI) NA else 1L,
                                      number_of_conditions = nrow(design),
                                      date_completed = date(), total_elapsed_time = sum(Final$SIM_TIME),
                                      error_seeds=error_seeds,
                                      stored_results = if(store_results) stored_Results_list else NULL)
    if(dummy_run) Final$dummy_run <- NULL
    class(Final) <- c('SimDesign', class(Final))
    if(!is.null(filename) && save){ #save file
        if(verbose)
            message(paste('\nSaving simulation results to file:', filename))
        saveRDS(Final, file.path(out_rootdir, filename))
    }
    if(save || save_results || save_seeds) file.remove(file.path(out_rootdir, tmpfilename))
    return(Final)
}

#' @rdname runSimulation
#' @param object SimDesign object returned from \code{\link{runSimulation}}
#' @param ... additional arguments
#' @export
summary.SimDesign <- function(object, ...){
    ret <- attr(object, 'extra_info')
    ret$total_elapsed_time <- timeFormater(ret$total_elapsed_time, TRUE)
    ret$stored_results <- NULL
    ret$error_seeds <- NULL
    ret
}

#' @param x SimDesign object returned from \code{\link{runSimulation}}
#' @param list2char logical; for \code{tibble} object re-evaluate list elements
#'   as character vectors for better printing of the levels? Note that this
#'   does not change the original classes of the object, just how they are printed.
#'   Default is TRUE
#' @rdname runSimulation
#' @export
print.SimDesign <- function(x, list2char = TRUE, ...){
    class(x) <- c('Design', class(x)[-1L])
    print(x=x, list2char=list2char, ...)
}

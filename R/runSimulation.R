#' Run a Monte Carlo simulation given conditions and simulation functions
#'
#' This function runs a Monte Carlo simulation study given a set of predefined simulation functions,
#' design conditions, and number of replications. Results can be saved as temporary files in case of
#' interruptions and may be restored by re-running \code{runSimulation}, provided that the respective temp
#' file can be found in the working directory. \code{runSimulation} supports parallel
#' and cluster computing (with the \code{\link[parallel]{parallel}} and
#' \code{\link[future]{future}} packages; see also
#' \code{\link{runArraySimulation}} for submitting array jobs to HPC clusters),
#' global and local debugging, error handling (including fail-safe
#' stopping when functions fail too often, even across nodes), provides bootstrap estimates of the
#' sampling variability (optional), and automatic tracking of error and warning messages
#' with their associated \code{.Random.seed} states.
#' For convenience, all functions available in the R work-space are exported across all nodes
#' so that they are more easily accessible (however, other R objects are not, and therefore
#' must be passed to the \code{fixed_objects} input to become available across nodes).
#'
#' For an in-depth tutorial of the package please refer to Chalmers and Adkins (2020;
#' \doi{10.20982/tqmp.16.4.p248}).
#' For an earlier didactic presentation of the package refer to Sigal and Chalmers
#' (2016; \doi{10.1080/10691898.2016.1246953}). Finally, see the associated
#' wiki on Github (\url{https://github.com/philchalmers/SimDesign/wiki})
#' for tutorial material, examples, and applications of \code{SimDesign} to real-world
#' simulation experiments, as well as the various vignette files associated with the package.
#'
#' The strategy for organizing the Monte Carlo simulation work-flow is to
#'
#' \describe{
#'    \item{1)}{Define a suitable \code{Design} object (a \code{tibble} or \code{data.frame})
#'       containing fixed conditional
#'       information about the Monte Carlo simulations. Each row or this \code{design} object pertains
#'       to a unique set of simulation to study, while each column the simulation factor under
#'       investigation (e.g., sample size,
#'       distribution types, etc). This is often expedited by using the
#'       \code{\link{createDesign}} function, and if necessary the argument \code{subset}
#'       can be used to remove redundant or non-applicable rows}
#'    \item{2)}{Define the three step functions to generate the data (\code{\link{Generate}}; see also
#'       \url{https://CRAN.R-project.org/view=Distributions} for a list of distributions in R),
#'       analyse the generated data by computing the respective parameter estimates, detection rates,
#'       etc (\code{\link{Analyse}}), and finally summarise the results across the total
#'       number of replications (\code{\link{Summarise}}).
#'    }
#'    \item{3)}{Pass the \code{design} object and three defined R functions to \code{runSimulation},
#'       and declare the number of replications to perform with the \code{replications} input.
#'       This function will return a suitable
#'       \code{tibble} object with the complete simulation results and execution details}
#'    \item{4)}{Analyze the output from \code{runSimulation}, possibly using ANOVA techniques
#'      (\code{\link{SimAnova}}) and generating suitable plots and tables}
#' }
#'
#' Expressing the above more succinctly, the functions to be called have the following form,
#' with the exact functional arguments listed:
#'
#' \describe{
#'   \item{\code{Design <- createDesign(...)}}{}
#'   \item{\code{Generate <- function(condition, fixed_objects) \{...\} }}{}
#'   \item{\code{Analyse <- function(condition, dat, fixed_objects) \{...\} }}{}
#'   \item{\code{Summarise <- function(condition, results, fixed_objects) \{...\} }}{}
#'   \item{\code{res <- runSimulation(design=Design, replications, generate=Generate,
#'         analyse=Analyse, summarise=Summarise)}}{}
#' }
#'
#' The \code{condition} object above represents a single row from the \code{design} object, indicating
#' a unique Monte Carlo simulation condition. The \code{condition} object also contains two
#' additional elements to help track the simulation's state: an \code{ID} variable, indicating
#' the respective row number in the \code{design} object, and a \code{REPLICATION} element
#' indicating the replication iteration number (an integer value between 1 and \code{replication}).
#' This setup allows users to easily locate the \code{r}th replication (e.g., \code{REPLICATION == 500})
#' within the \code{j}th row in the simulation design (e.g., \code{ID == 2}). The
#' \code{REPLICATION} input is also useful when temporarily saving files to the hard-drive
#' when calling external command line utilities (see examples on the wiki).
#'
#' For a template-based version of the work-flow, which is often useful when initially
#' defining a simulation, use the \code{\link{SimFunctions}} function. This
#' function will write a template simulation
#' to one/two files so that modifying the required functions and objects can begin immediately.
#' This means that users can focus on their Monte Carlo simulation details right away rather
#' than worrying about the repetitive administrative code-work required to organize the simulation's
#' execution flow.
#'
#' Finally, examples, presentation files, and tutorials can be found on the package wiki located at
#' \url{https://github.com/philchalmers/SimDesign/wiki}.
#'
#' @section Saving data, results, seeds, and the simulation state:
#'
#' To conserve RAM, temporary objects (such as data generated across conditions and replications)
#' are discarded; however, these can be saved to the hard-disk by passing the appropriate flags.
#' For longer simulations it is recommended to use the \code{save_results} flag to write the
#' analysis results to the hard-drive.
#'
#' The use of the \code{store_seeds} or the \code{save_seeds} options
#' can be evoked to save R's \code{.Random.seed}
#' state to allow for complete reproducibility of each replication within each condition. These
#' individual \code{.Random.seed} terms can then be read in with the
#' \code{load_seed} input to reproduce the exact simulation state at any given replication.
#' Most often though, saving the complete list of seeds is unnecessary as problematic seeds are
#' automatically stored in the final simulation object to allow for easier replicability
#' of potentially problematic errors (which incidentally can be extracted
#' using \code{SimExtract(res, 'error_seeds')} and passed to the \code{load_seed} argument). Finally,
#' providing a vector of \code{seeds} is also possible to ensure
#' that each simulation condition is macro reproducible under the single/multi-core method selected.
#'
#' Finally, when the Monte Carlo simulation is complete
#' it is recommended to write the results to a hard-drive for safe keeping, particularly with the
#' \code{filename} argument provided (for reasons that are more obvious in the parallel computation
#' descriptions below). Using the \code{filename} argument supplied is safer than using, for instance,
#' \code{\link{saveRDS}} directly because files will never accidentally be overwritten,
#' and instead a new file name will be created when a conflict arises; this type of implementation safety
#' is prevalent in many locations in the package to help avoid unrecoverable (yet surprisingly
#' common) mistakes during the process of designing and executing Monte Carlo simulations.
#'
#' @section Resuming temporary results:
#'
#' In the event of a computer crash, power outage, etc, if \code{save = TRUE} was used (the default)
#' then the original code used to execute \code{runSimulation()} need only be re-run to resume
#' the simulation. The saved temp file will be read into the function automatically, and the
#' simulation will continue one the condition where it left off before the simulation
#' state was terminated. If users wish to remove this temporary
#' simulation state entirely so as to start anew then simply pass \code{SimClean(temp = TRUE)}
#' in the R console to remove any previously saved temporary objects.
#'
#' @section A note on parallel computing:
#'
#' When running simulations in parallel (either with \code{parallel = TRUE}
#' or when using the \code{\link[future]{future}} approach with a \code{plan()} other than sequential)
#' R objects defined in the global environment will generally \emph{not} be visible across nodes.
#' Hence, you may see errors such as \code{Error: object 'something' not found} if you try to use
#' an object that is defined in the work space but is not passed to \code{runSimulation}.
#' To avoid this type or error, simply pass additional objects to the
#' \code{fixed_objects} input (usually it's convenient to supply a named list of these objects).
#' Fortunately, however, \emph{custom functions defined in the global environment are exported across
#' nodes automatically}. This makes it convenient when writing code because custom functions will
#' always be available across nodes if they are visible in the R work space. As well, note the
#' \code{packages} input to declare packages which must be loaded via \code{library()} in order to make
#' specific non-standard R functions available across nodes.
#'
#' @param design a \code{tibble} or \code{data.frame} object containing the Monte Carlo simulation
#'   conditions to be studied, where each row represents a unique condition and each column a factor
#'   to be varied. See \code{\link{createDesign}} for the standard approach
#'   to create this simulation design object
#'
#' @param generate user-defined data and parameter generating function (or named list of functions).
#'   See \code{\link{Generate}} for details. Note that this argument may be omitted by the
#'   user if they wish to generate the data with the \code{analyse} step, but for real-world
#'   simulations this is generally not recommended. If multiple generate functions are provided
#'   as a list then the list of generate functions are executed in order until the first valid
#'   generate function is executed, where the subsequent generation functions are then ignored
#'   (see \code{\link{GenerateIf}} to only apply data generation for specific conditions).
#'
#' @param analyse user-defined analysis function (or named list of functions)
#'   that acts on the data generated from
#'   \code{\link{Generate}} (or, if \code{generate} was omitted, can be used to generate and
#'   analyses the simulated data). See \code{\link{Analyse}} for details
#'
#' @param summarise optional (but strongly recommended) user-defined summary function
#'   from \code{\link{Summarise}} to be used to compute meta-statistical summary
#'   information after all the replications have completed within
#'   each \code{design} condition. Return of this function, in order
#'   of increasing complexity, should be: a named numeric vector or \code{data.frame}
#'   with one row, a \code{matrix} or \code{data.frame} with more than one row, and,
#'   failing these more atomic types, a named \code{list}. For summary objects that
#'   are not easily appended to the original \code{design} object use
#'   \code{\link{SimExtract}} with the option \code{what = 'summarise'}.
#'
#'   Note that unlike the Generate and Analyse
#'   steps, the Summarise portion is not as important to perfectly organize
#'   as the results can be summarised later on by using the built-in
#'   \code{\link{reSummarise}} function (provided either
#'   \code{store_results = TRUE} or \code{save_results = TRUE} were included).
#'
#'   Omitting this function will return a tibble with the \code{Design}
#'   and associated results information for all
#'   \code{nrow(Design) * repliations} evaluations if the results from each
#'   \code{Analyse()} call was a one-dimensional vector.
#'   For more general objects returned by \code{Analyse()}
#'   (such as \code{list}s), a \code{list}
#'   containing the results returned form \code{\link{Analyse}}.
#'   This is generally only recommended for didactic purposes because the results
#'   will leave out a large amount of
#'   information (e.g., try-errors, warning messages, saving files, etc), can
#'   witness memory related issues if the Analyse function returns larger objects,
#'   and generally is not as flexible internally. However, it may be useful
#'   when replications are expensive and ANOVA-based decompositions involving
#'   the within-condition replication information are of interest, though
#'   of course this  can be circumvented by using \code{store_results = TRUE} or
#'   \code{save_results = TRUE} with or without a supplied \code{summarise}
#'   definition.
#'
#' @param replications number of independent replications to perform per
#'   condition (i.e., each row in \code{design}). Can be a single number, which
#'   will be used for each design condition, or an integer vector with length
#'   equal to \code{nrow(design)}. All inputs must be greater than 0, though setting
#'   to less than 3 (for initial testing purpose) will disable the \code{save}
#'   and \code{control$stop_on_fatal} flags
#'
#' @param fixed_objects (optional) an object (usually a named \code{list})
#'   containing additional user-defined objects
#'   that should remain fixed across conditions. This is useful when including
#'   large vectors/matrices of population parameters, fixed data information
#'   that should be used across all conditions and replications (e.g., including a
#'   common design matrix for linear regression models), or simply control
#'   constant global elements (e.g., a constant for sample size)
#'
#' @param parallel logical; use parallel processing from the \code{parallel}
#'   package over each unique condition? This distributes the independent \code{replications}
#'   across the defined nodes, and is repeated for each row condition in the \code{design}
#'   input.
#'
#'   Alternatively, if the \code{\link[future]{future}} package approach is desired then passing
#'   \code{parallel = 'future'} to \code{runSimulation()} will use the defined
#'   \code{\link[future]{plan}} for execution. This allows for greater flexibility when
#'   specifying the general computing plan (e.g., \code{plan(multisession)}) for parallel computing
#'   on the same machine, \code{plan(future.batchtools::batchtools_torque)} or
#'   \code{plan(future.batchtools::batchtools_slurm)} for common MPI/Slurm schedulers, etc).
#'   However, it is the responsibility of the user to use \code{plan(sequential)} to reset the
#'   computing plan when the jobs are completed
#'
#' @param cl cluster object defined by \code{\link{makeCluster}} used to run code in parallel
#'   (ignored if using the \code{\link[future]{future}} package approach).
#'   If \code{NULL} and \code{parallel = TRUE}, a local cluster object will be defined which
#'   selects the maximum number cores available
#'   and will be stopped when the simulation is complete. Note that supplying a \code{cl}
#'   object will automatically set the \code{parallel} argument to \code{TRUE}. Define and supply this
#'   cluster object yourself whenever you have multiple nodes and can link them together manually
#'
#'   If the \code{future} package has
#'   been attached prior to executing \code{runSimulation()} then the associated
#'   \code{plan()} will be followed instead
#'
#' @param packages a character vector of external packages to be used during the simulation (e.g.,
#'   \code{c('MASS', 'extraDistr', 'simsem')} ). Use this input when running code in
#'   parallel to use non-standard functions from additional packages,
#'   otherwise the functions must be made available by using explicit
#'   \code{\link{library}} or \code{\link{require}} calls within the provided simulation functions.
#'   Alternatively, functions can be called explicitly without attaching the package
#'   with the \code{::} operator
#'   (e.g., \code{extraDistr::rgumbel()})
#'
#' @param beep logical; call the \code{beepr} package when the simulation is completed?
#'
#' @param sound \code{sound} argument passed to \code{beepr::beep()}
#'
#' @param notification an optional character vector input that can be used to send
#'   Pushbullet notifications from a configured
#'   computer. This reports information such as the total execution time, the condition
#'   completed, and error/warning
#'   messages recorded. This arguments assumes that users have already A) registered for
#'   a Pushbullet account,
#'   B) installed the application on their mobile device and computer, and C) created an
#'   associated JSON file of the form
#'   \code{~/.rpushbullet.json} using \code{RPushbullet::pbSetup()}).
#'
#'   To utilize the \code{RPushbullet} in \code{SimDesign} first call \code{library(RPushbullet}
#'   before running \code{runSimulation()} to read-in the default JSON file. Next,
#'   pass one of the following supported
#'   options: \code{'none'} (default; send no notification),
#'   \code{'condition'} to send a notification after each condition has completed,
#'   or \code{'complete'} to send
#'   a notification only when the simulation has finished.
#'
#' @param save_results logical; save the results returned from \code{\link{Analyse}} to external
#'   \code{.rds} files located in the defined \code{save_results_dirname} directory/folder?
#'   Use this if you would like to keep track of the individual parameters returned from
#'   the \code{analysis} function.
#'   Each saved object will contain a list of three elements containing the
#'   condition (row from \code{design}),
#'   results (as a \code{list} or \code{matrix}), and try-errors.
#'   See \code{\link{SimResults}} for an example of how to read these \code{.rds} files back into R
#'   after the simulation is complete. Default is \code{FALSE}.
#'
#'   WARNING: saving results to your hard-drive can fill up space very quickly for
#'   larger simulations. Be sure to
#'   test this option using a smaller number of replications before the full Monte
#'   Carlo simulation is performed.
#'   See also \code{\link{reSummarise}} for applying summarise functions from saved
#'   simulation results
#'
#' @param load_seed used to replicate an exact simulation state, which is
#' primarily useful for debugging purposes.
#'   Input can be a character object indicating which file to load from when the
#'   \code{.Random.seed}s have
#'   be saved (after a call with \code{save_seeds = TRUE}), or an integer vector
#'   indicating the actual
#'   \code{.Random.seed} values (e.g., extracted after using \code{store_seeds}).
#'   E.g., \code{load_seed = 'design-row-2/seed-1'}
#'   will load the first seed in the second row of the \code{design} input, or
#'   explicitly passing the
#'   elements from \code{.Random.seed} (see \code{\link{SimExtract}} to extract
#'   the seeds associated explicitly
#'   with errors during the simulation, where each column represents a unique seed).
#'   If the input is a character vector then it is important NOT
#'   to modify the \code{design} input object, otherwise the path may not point
#'   to the correct saved location, while
#'   if the input is an integer vector (or single column \code{tbl} object)
#'   then it WILL be important to modify the \code{design} input in order to load this
#'   exact seed for the corresponding design row. Default is \code{NULL}
#'
#' @param filename (optional) the name of the \code{.rds} file to save the final
#' simulation results to. If the extension
#'   \code{.rds} is not included in the file name (e.g. \code{"mysimulation"}
#'   versus \code{"mysimulation.rds"}) then the
#'   \code{.rds} extension will be automatically added to the file name to ensure
#'    the file extension is correct.
#'
#'   Note that if the same file name already exists in the working
#'   directly at the time of saving then a new
#'   file will be generated instead and a warning will be thrown. This helps to
#'   avoid accidentally overwriting
#'   existing files. Default is \code{NULL}, indicating no file will be saved by default
#'
#' @param control a list for extra information flags for controlling less
#'   commonly used features. These include
#'
#'   \describe{
#'
#'     \item{\code{stop_on_fatal}}{logical (default is \code{FALSE}); should the simulation be
#'       terminated immediately when
#'       the maximum number of consecutive errors (\code{max_errors}) is reached? If \code{FALSE},
#'       the simulation will continue as though errors did not occur, however a column
#'       \code{FATAL_TERMINATION} will be included in the resulting object indicating the final
#'       error message observed, and \code{NA} placeholders will be placed in all other row-elements.
#'       Default is \code{FALSE}, though is automatically set to \code{TRUE} when \code{replications < 3}
#'       for the purpose of debugging}
#'
#'      \item{\code{warnings_as_errors}}{logical (default is \code{FALSE});
#'      treat warning messages as error messages during the simulation? Default is FALSE,
#'      therefore warnings are only collected and not used to restart the data generation step,
#'      and the seeds associated with
#'      the warning message conditions are not stored within the final simulation object.
#'
#'      Note that this argument is generally intended for debugging/early planning
#'      stages when designing a simulation experiment. If specific warnings are known to
#'      be problematic and should be treated as errors then please use
#'      \code{\link{manageWarnings}} instead}
#'
#'      \item{\code{save_seeds}}{
#'      logical; save the \code{.Random.seed} states prior to performing
#'      each replication into
#'      plain text files located in the defined \code{save_seeds_dirname} directory/folder?
#'      Use this if you would like to keep track of every simulation state within each
#'      replication and design
#'      condition. This can be used to completely replicate any cell in the simulation if need be.
#'      As well, see the \code{load_seed} input
#'      to load a given \code{.Random.seed} to exactly replicate the generated data and
#'      analysis state (mostly useful
#'      for debugging). When \code{TRUE}, temporary files will also be saved
#'      to the working directory (in the same way as when \code{save = TRUE}).
#'      Default is \code{FALSE}
#'
#'      Note, however, that this option is not typically necessary or recommended since
#'      the \code{.Random.seed} states for simulation
#'      replications that throw errors during the execution are automatically stored
#'      within the final simulation
#'      object, and can be extracted and investigated using \code{\link{SimExtract}}.
#'      Hence, this option is only of
#'      interest when \emph{all} of the replications must be reproducible (which occurs very rarely),
#'      otherwise the defaults to \code{runSimulation} should be sufficient}
#'
#'      \item{\code{store_Random.seeds}}{logical; store the
#'       complete \code{.Random.seed} states
#'       for each simulation replicate? Default is \code{FALSE} as this can
#'       take up a great deal of unnecessary RAM (see related \code{save_seeds}),
#'       however this may be useful
#'       when used with \code{\link{runArraySimulation}}. To extract use
#'       \code{SimExtract(..., what = 'stored_Random.seeds')}}
#'
#'      \item{\code{store_warning_seeds}}{logical (default is \code{FALSE});
#'       in addition to storing the \code{.Random.seed} states whenever error messages
#'       are raised, also store the \code{.Random.seed} states when warnings are raised? This is
#'       disabled by default
#'       since warnings are generally less problematic than errors, and because many more
#'       warnings messages may be raised
#'       throughout the simulation (potentially causing RAM related issues when constructing
#'       the final simulation object as
#'       any given simulation replicate could generate numerous warnings, and storing the seeds
#'       states could add up quickly).
#'
#'       Set this to \code{TRUE} when replicating warning messages is important, however be aware
#'       that too many warnings messages raised during the simulation implementation could cause
#'       RAM related issues.}
#'
#'      \item{\code{include_replication_index} or
#'        \code{include_reps}}{logical (default is \code{FALSE});
#'        should a REPLICATION element be added to
#'        the \code{condition} object when performing the simulation to track which specific
#'        replication experiment is being evaluated? This is useful when, for instance, attempting
#'        to run external software programs (e.g., Mplus) that require saving temporary data sets
#'        to the hard-drive (see the Wiki for examples)}
#'
#'      \item{\code{try_all_analyse}}{logical; when \code{analyse} is a list, should every generated
#'        data set be analyzed by each function definition in the \code{analyse} list?
#'        Default is \code{TRUE}.
#'
#'        Note that this \code{TRUE} default can be computationally demanding when some analysis
#'        functions require more computational resources than others, and the data should be
#'        discarded early as an invalid candidate (e.g., estimating a model via maximum-likelihood
#'        in on analyze component, while estimating a model using MCMC estimation on another). Hence,
#'        the main benefit of using \code{FALSE} instead is that the data set may be rejected earlier,
#'        where easier/faster to estimate \code{analyse} definitions should be placed earlier in the list
#'        as the functions are evaluated in sequence
#'        (e.g., \code{Analyse = list(MLE=MLE_definition, MCMC=MCMC_definition)}) }
#'
#'      \item{\code{allow_na}}{logical (default is \code{FALSE}); should \code{NA}s be allowed in the
#'       analyse step as a valid result from the simulation analysis?}
#'
#'      \item{\code{allow_nan}}{logical (default is \code{FALSE}); should \code{NaN}s be allowed in the
#'        analyse step as a valid result from the simulation analysis?}
#'
#'      \item{\code{type}}{default type of cluster to create for the \code{cl} object if not supplied.
#'        For Windows OS this defaults to \code{"PSOCK"}, otherwise \code{"SOCK"} is selected
#'        (suitable for Linux and Mac OSX). This is ignored if the user specifies their own \code{cl} object}
#'
#'      \item{\code{print_RAM}}{logical (default is \code{TRUE}); print the amount of RAM
#'        used throughout the simulation? Set to \code{FALSE} if unnecessary or if the call to
#'        \code{\link{gc}} is unnecessarily time consuming}
#'
#'      \item{\code{max_time}}{
#'        Similar to \code{\link{runArraySimulation}}, specifies the (approximate) maximum
#'        time that the simulation is allowed to be executed. However, unlike the implementation
#'        in \code{runArraySimulation} is evaluated on a per condition basis,
#'        where \code{max_time} is only evaluated after every row in the
#'        \code{design} object has been completed (hence, is notably more approximate as it
#'        has the potential to overshoot by a wider margin). Default sets no time limit.
#'        See \code{\link{timeFormater}} for the input specifications.
#'      }
#'
#'      \item{\code{max_RAM}}{
#'        Similar to \code{\link{runArraySimulation}}, specifies the (approximate) maximum
#'        RAM that the simulation is allowed to occupy. However, unlike the implementation
#'        in \code{runArraySimulation} is evaluated on a per condition basis,
#'        where \code{max_RAM} is only evaluated after every row in the
#'        \code{design} object has been completed (hence, is notably more approximate as it
#'        has the potential to overshoot by a wider margin). Default sets no RAM limit.
#'        See \code{\link{runArraySimulation}} for the input specifications.
#'      }
#'
#'    }
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
#'       node to match the broken computer. Default is the result of evaluating
#'       \code{unname(Sys.info()['nodename'])}}
#'
#'     \item{\code{out_rootdir}}{root directory to save all files to. Default uses the
#'        current working directory}
#'
#'     \item{\code{save_results_dirname}}{a string indicating the name of the folder to save
#'       result objects to when \code{save_results = TRUE}. If a directory/folder does not exist
#'       in the current working directory then a unique one will be created automatically. Default is
#'       \code{'SimDesign-results_'} with the associated \code{compname} appended if no
#'       \code{filename} is defined, otherwise the filename is used to replace 'SimDesign'
#'       in the string}
#'
#'     \item{\code{save_results_filename}}{a string indicating the name file to store, where the
#'       \code{Design} row ID will be appended to ensure uniqueness across rows. Specifying
#'       this input will disable any checking for the uniqueness of the file folder, thereby
#'       allowing independent \code{runSimulation} calls to write to the same
#'       \code{save_results_dirname}. Useful when the files should all be stored in the same
#'       working directory, however the rows of \code{Design} are evaluated in isolation (e.g.,
#'       for HPC structures that allow asynchronous file storage).
#'       WARNING: the uniqueness of the file names are not checked using
#'       this approach, therefore please ensure that each generated name will be unique a priori,
#'       such as naming the file based on the supplied row condition information}
#'
#'     \item{\code{save_seeds_dirname}}{a string indicating the name of the folder to save
#'       \code{.Random.seed} objects to when \code{save_seeds = TRUE}. If a directory/folder
#'       does not exist
#'       in the current working directory then one will be created automatically. Default is
#'       \code{'SimDesign-seeds_'} with the associated \code{compname} appended if no
#'       \code{filename} is defined, otherwise the filename is used to replace 'SimDesign'
#'       in the string}
#'
#'       \item{\code{tmpfilename}}{string indicating the temporary file name to save
#'         provisional information to. If not specified the following will be used:
#'         \code{paste0('SIMDESIGN-TEMPFILE_', compname, '.rds')}}
#'
#'   }
#'
#' @param max_errors the simulation will terminate when more than this number of consecutive
#' errors are thrown in any
#'   given condition, causing the simulation to continue to the next unique \code{design} condition.
#'   This is included to avoid getting stuck in infinite re-draws, and to indicate that
#'   something fatally problematic
#'   is going wrong in the generate-analyse phases. Default is 50
#'
#' @param ncores number of cores to be used in parallel execution (ignored if using the
#'   \code{\link[future]{future}} package approach). Default uses all available minus 1
#'
#' @param save logical; save the temporary simulation state to the hard-drive? This is useful
#'   for simulations which require an extended amount of time, though for shorter simulations
#'   can be disabled to slightly improve computational efficiency. When \code{TRUE},
#'   which is the default when evaluating \code{replications > 2}, a temp file
#'   will be created in the working directory which allows the simulation state to be saved
#'   and recovered (in case of power outages, crashes, etc). As well, triggering this flag will
#'   save any fatal \code{.Random.seed} states when conditions unexpectedly crash (where each seed
#'   is stored row-wise in an external .rds file), which provides a much easier mechanism
#'   to debug issues (see \code{load_seed} for details). Upon completion, this temp file will
#'   be removed.
#'
#'   To recover your simulation at the last known location (having patched the issues in the
#'   previous execution code) simply re-run the code you used to
#'   initially define the simulation and the external file will automatically be detected and read-in.
#'   Default is \code{TRUE} when \code{replications > 10} and \code{FALSE} otherwise
#'
#' @param resume logical; if a temporary \code{SimDesign} file is detected should
#'   the simulation resume from this location? Keeping this \code{TRUE} is generally recommended,
#'   however this should be disabled if using \code{runSimulation} within \code{runSimulation} to avoid
#'   reading improper save states. Alternatively, if an integer is supplied then the simulation
#'   will continue at the associated row location in \code{design} (e.g., \code{resume=10}).
#'   This is useful to overwrite a previously evaluate element in the temporary files that was detected
#'   to contain fatal errors that require re-evaluation without discarding the originally valid rows
#'   in the simulation
#'
#' @param debug a string indicating where to initiate a \code{browser()} call for editing
#'   and debugging, and pairs particularly well with the \code{load_seed} argument for precise debugging.
#'   General options are \code{'none'} (default; no debugging), \code{'error'}, which
#'   starts the debugger
#'   when any error in the code is detected in one of three generate-analyse-summarise functions,
#'   and \code{'all'}, which debugs all the user defined functions regardless of
#'    whether an error was thrown
#'   or not. Specific options include: \code{'generate'}
#'   to debug the data simulation function, \code{'analyse'} to debug the computational function, and
#'   \code{'summarise'} to debug the aggregation function.
#'
#'   If the \code{Analyse} argument is supplied as a named list of functions then it is also possible
#'   to debug the specific function of interest by passing the name of the respective function in the list.
#'   For instance, if \code{analyse = list(A1=Analyse.A1, A2=Analyse.A2)} then passing
#'   \code{debug = 'A1'} will debug only the first function in this list, and all remaining analysis
#'   functions will be ignored.
#'
#'   For debugging specific rows in the \code{Design} input (e.g.,
#'   when a number of initial rows successfully complete but the \code{k}th
#'   row fails) the row number can be appended to the standard
#'   \code{debug} input using a \code{'-'} separator.
#'   For instance, debugging whenever an error is raised
#'   in the second row of \code{Design} can be declared via \code{debug = 'error-2'}.
#'
#'   Finally, users may place \code{\link{browser}} calls within the respective functions for
#'   debugging at specific lines, which is useful when debugging based on conditional evaluations (e.g.,
#'   \code{if(this == 'problem') browser()}). Note that parallel computation flags
#'   will automatically be disabled when a \code{browser()} is detected or when a debugging
#'    argument other than
#'   \code{'none'} is supplied
#'
#' @param seed a vector or list of integers to be used for reproducibility.
#'   The length of the vector must be equal the number of rows in \code{design}.
#'   If the input is a vector then \code{\link{set.seed}} or
#'   \code{\link{clusterSetRNGStream}} for each condition will be called, respectively.
#'   If a list is provided then these
#'   numbers must have been generated from \code{\link{gen_seeds}} with the argument
#'   \code{CMRG.seed} used to specify the initial. The list approach ensures random number
#'   generation independence across conditions and replications, while the vector input
#'   ensures independence within the replications per conditions but not necessarily
#'   across conditions. Default randomly generates seeds within the
#'   range 1 to 2147483647 for each condition via \code{link{gen_seeds}}
#'
#' @param progress logical; display a progress bar (using the \code{pbapply} package)
#'   for each simulation condition?
#'   This is useful when simulations conditions take a long time to run (see also the
#'   \code{notifications} argument). Default is \code{TRUE}
#'
#' @param boot_method method for performing non-parametric bootstrap confidence intervals
#'  for the respective meta-statistics computed by the \code{Summarise} function.
#'  Can be \code{'basic'} for the empirical bootstrap CI, \code{'percentile'}
#'  for percentile CIs, \code{'norm'} for normal approximations CIs, or \code{'studentized'}
#'  for Studentized CIs (should only be used for simulations with lower replications due to its
#'  computational intensity). Alternatively, CIs can be constructed using the argument \code{'CLT'},
#'  which computes the intervals according to the large-sample standard error
#'  approximation \eqn{SD(results)/\sqrt{R}}. Default is \code{'none'}, which performs no CI computations
#'
#' @param boot_draws number of non-parametric bootstrap draws to sample for the \code{summarise}
#'   function after the generate-analyse replications are collected. Default is 1000
#'
#' @param CI bootstrap confidence interval level (default is 95\%)
#'
#' @param store_results logical; store the complete tables of simulation results
#'   in the returned object? This is \code{TRUE} default, though if RAM anticipated to
#'   be an issue see \code{save_results} instead. Note that if the \code{Design}
#'   object is omitted from the call to \code{runSimulation()}, or the number of rows in \code{Design}
#'   is exactly 1, then this argument is automatically set to \code{TRUE} as RAM storage is no
#'   longer an issue.
#'
#'   To extract these results
#'   pass the returned object to either \code{\link{SimResults}} or \code{\link{SimExtract}} with
#'   \code{what = 'results'}, which will return a named list
#'   of all the simulation results for each condition if \code{nrow(Design) > 1}; otherwise, if
#'   \code{nrow(Design) == 1} or \code{Design} was missing the \code{results} object will be stored as-is
#'
#' @param verbose logical; print messages to the R console? Default is \code{TRUE}
#'
#' @return a \code{tibble} from the \code{dplyr} package (also of class \code{'SimDesign'})
#'   with the original \code{design} conditions in the left-most columns,
#'   simulation results in the middle columns, and additional information in the right-most columns (see below).
#'
#' The right-most column information for each condition are:
#' \code{REPLICATIONS} to indicate the number of Monte Carlo replications,
#' \code{SIM_TIME} to indicate how long (in seconds) it took to complete
#' all the Monte Carlo replications for each respective design condition,
#' \code{RAM_USED} amount of RAM that was in use at the time of completing
#'   each simulation condition,
#' \code{COMPLETED} to indicate the date in which the given simulation condition completed,
#' \code{SEED} for the integer values in the \code{seed} argument (if applicable; not
#' relevant when L'Ecuyer-CMRG method used), and, if applicable,
#' \code{ERRORS} and \code{WARNINGS} which contain counts for the number of error or warning
#' messages that were caught (if no errors/warnings were observed these columns will be omitted).
#' Note that to extract the specific error and warnings messages see
#' \code{\link{SimExtract}}. Finally,
#' if \code{boot_method} was a valid input other than 'none' then the final right-most
#' columns will contain the labels
#' \code{BOOT_} followed by the name of the associated meta-statistic defined in \code{summarise()} and
#' and bootstrapped confidence interval location for the meta-statistics.
#'
#' @aliases runSimulation
#'
#' @seealso \code{\link{SimFunctions}}, \code{\link{createDesign}},
#'   \code{\link{Generate}}, \code{\link{Analyse}}, \code{\link{Summarise}},
#'   \code{\link{SimExtract}},
#'   \code{\link{reSummarise}}, \code{\link{SimClean}}, \code{\link{SimAnova}}, \code{\link{SimResults}},
#'   \code{\link{SimCollect}}, \code{\link{Attach}}, \code{\link{AnalyseIf}},
#'   \code{\link{SimShiny}}, \code{\link{manageWarnings}}, \code{\link{runArraySimulation}}
#'
#' @export runSimulation
#'
#' @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#'
#' @references
#'
#' Chalmers, R. P., & Adkins, M. C.  (2020). Writing Effective and Reliable Monte Carlo Simulations
#' with the SimDesign Package. \code{The Quantitative Methods for Psychology, 16}(4), 248-280.
#' \doi{10.20982/tqmp.16.4.p248}
#'
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
#' Design <- createDesign(N = c(10, 20, 30))
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 2 --- Define generate, analyse, and summarise functions
#'
#' # help(Generate)
#' Generate <- function(condition, fixed_objects) {
#'     dat <- with(condition, rnorm(N, 10, 5)) # distributed N(10, 5)
#'     dat
#' }
#'
#' # help(Analyse)
#' Analyse <- function(condition, dat, fixed_objects) {
#'     ret <- c(mean=mean(dat)) # mean of the sample data vector
#'     ret
#' }
#'
#' # help(Summarise)
#' Summarise <- function(condition, results, fixed_objects) {
#'     # mean and SD summary of the sample means
#'     ret <- c(mu=mean(results$mean), SE=sd(results$mean))
#'     ret
#' }
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Step 3 --- Collect results by looping over the rows in design
#'
#' # run the simulation in testing mode (replications = 2)
#' Final <- runSimulation(design=Design, replications=2,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' Final
#' SimResults(Final)
#'
#' # reproduce exact simulation
#' Final_rep <- runSimulation(design=Design, replications=2, seed=Final$SEED,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' Final_rep
#' SimResults(Final_rep)
#'
#' \dontrun{
#' # run with more standard number of replications
#' Final <- runSimulation(design=Design, replications=1000,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' Final
#' SimResults(Final)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~
#' #### Extras
#' # compare SEs estimates to the true SEs from the formula sigma/sqrt(N)
#' 5 / sqrt(Design$N)
#'
#' # To store the results from the analyse function either
#' #   a) omit a definition of summarise() to return all results,
#' #   b) use store_results = TRUE (default) to store results internally and later
#' #      extract with SimResults(), or
#' #   c) pass save_results = TRUE to runSimulation() and read the results in with SimResults()
#' #
#' #   Note that method c) should be adopted for larger simulations, particularly
#' #   if RAM storage could be an issue and error/warning message information is important.
#'
#' # a) approach
#' res <- runSimulation(design=Design, replications=100,
#'                      generate=Generate, analyse=Analyse)
#' res
#'
#' # b) approach (store_results = TRUE by default)
#' res <- runSimulation(design=Design, replications=100,
#'                      generate=Generate, analyse=Analyse, summarise=Summarise)
#' res
#' SimResults(res)
#'
#' # c) approach
#' Final <- runSimulation(design=Design, replications=100, save_results=TRUE,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#'
#' # read-in all conditions (can be memory heavy)
#' res <- SimResults(Final)
#' res
#' head(res[[1]]$results)
#'
#' # just first condition
#' res <- SimResults(Final, which=1)
#' head(res$results)
#' dplyr::tibble(res$condition, res$results)
#'
#'
#' # obtain empirical bootstrapped CIs during an initial run
#' # the simulation was completed (necessarily requires save_results = TRUE)
#' res <- runSimulation(design=Design, replications=1000, boot_method = 'basic',
#'                      generate=Generate, analyse=Analyse, summarise=Summarise)
#' res
#'
#' # alternative bootstrapped CIs that uses saved results via reSummarise().
#' # Default directory save to:
#' dirname <- paste0('SimDesign-results_', unname(Sys.info()['nodename']), "/")
#' res <- reSummarise(summarise=Summarise, dir=dirname, boot_method = 'basic')
#' res
#'
#' # remove the saved results from the hard-drive if you no longer want them
#' SimClean(results = TRUE)
#'
#' }
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
#' Generate <- function(condition, fixed_objects) {
#'     N <- condition$sample_size      # could use Attach() to make objects available
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
#' Analyse <- function(condition, dat, fixed_objects) {
#'     welch <- t.test(DV ~ group, dat)$p.value
#'     independent <- t.test(DV ~ group, dat, var.equal=TRUE)$p.value
#'
#'     # In this function the p values for the t-tests are returned,
#'     #  and make sure to name each element, for future reference
#'     ret <- nc(welch, independent)
#'     ret
#' }
#'
#' Summarise <- function(condition, results, fixed_objects) {
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
#' res <- runSimulation(design=Design, replications=2,
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
#' ## save final results to a file upon completion, and play a beep when done
#' runSimulation(design=Design, replications=1000, parallel=TRUE, filename = 'mysim',
#'               generate=Generate, analyse=Analyse, summarise=Summarise, beep=TRUE)
#'
#' ## same as above, but send a notification via Pushbullet upon completion
#' library(RPushbullet) # read-in default JSON file
#' runSimulation(design=Design, replications=1000, parallel=TRUE, filename = 'mysim',
#'               generate=Generate, analyse=Analyse, summarise=Summarise,
#'               notification = 'complete')
#'
#' ## Submit as RStudio job (requires job package and active RStudio session)
#' job::job({
#'   res <- runSimulation(design=Design, replications=100,
#'                        generate=Generate, analyse=Analyse, summarise=Summarise)
#' }, title='t-test simulation')
#' res  # object res returned to console when completed
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
#' Summarise <- function(condition, results, fixed_objects) {
#'     #find results of interest here (e.g., alpha < .1, .05, .01)
#'     browser()
#'     ret <- EDR(results[,nms], alpha = .05)
#'     ret
#' }
#'
#' ## The following debugs the analyse function for the
#' ## second row of the Design input
#' runSimulation(design=Design, replications=1000,
#'               generate=Generate, analyse=Analyse, summarise=Summarise,
#'               parallel=TRUE, debug='analyse-2')
#'
#'
#' ####################################
#' ## EXTRA: To run the simulation on a user-define cluster, use the following setup (not run)
# library(doMPI)
# cl <- startMPIcluster()
# registerDoMPI(cl)
# Final <- runSimulation(design=Design, replications=1000, MPI=TRUE,
#                        generate=Generate, analyse=Analyse, summarise=Summarise)
# saveRDS(Final, 'mysim.rds')
# closeCluster(cl)
# mpi.quit()
#
#
#' ## Network linked via ssh (two way ssh key-paired connection must be
#' ## possible between master and slave nodes)
#' ##
#' ## Define IP addresses, including primary IP
#' primary <- '192.168.2.20'
#' IPs <- list(
#'     list(host=primary, user='phil', ncore=8),
#'     list(host='192.168.2.17', user='phil', ncore=8)
#' )
#' spec <- lapply(IPs, function(IP)
#'                    rep(list(list(host=IP$host, user=IP$user)), IP$ncore))
#' spec <- unlist(spec, recursive=FALSE)
#'
#' cl <- parallel::makeCluster(type='PSOCK', master=primary, spec=spec)
#' res <- runSimulation(design=Design, replications=1000, parallel = TRUE,
#'                      generate=Generate, analyse=Analyse, summarise=Summarise, cl=cl)
#'
#'
#' ## Using parallel='future' to allow the future framework to be used instead
#' library(future) # future structure to be used internally
#' plan(multisession) # specify different plan (default is sequential)
#'
#' res <- runSimulation(design=Design, replications=100, parallel='future',
#'                      generate=Generate, analyse=Analyse, summarise=Summarise)
#' head(res)
#'
#' # The progressr package is used for progress reporting with futures. To redefine
#' #  use progressr::handlers() (see below)
#' library(progressr)
#' with_progress(res <- runSimulation(design=Design, replications=100, parallel='future',
#'                      generate=Generate, analyse=Analyse, summarise=Summarise))
#' head(res)
#'
#' # re-define progressr's bar (below requires cli)
#' handlers(handler_pbcol(
#'    adjust = 1.0,
#'    complete = function(s) cli::bg_red(cli::col_black(s)),
#'    incomplete = function(s) cli::bg_cyan(cli::col_black(s))
#' ))
#'
#' with_progress(res <- runSimulation(design=Design, replications=100, parallel='future',
#'                      generate=Generate, analyse=Analyse, summarise=Summarise))
#'
#' # reset future computing plan when complete (good practice)
#' plan(sequential)
#'
#' ####################################
#'
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
                          fixed_objects = NULL, packages = NULL, filename = NULL,
                          debug = 'none', load_seed = NULL, save = any(replications > 2),
                          store_results = TRUE, save_results = FALSE,
                          parallel = FALSE, ncores = parallelly::availableCores(omit = 1L),
                          cl = NULL, notification = 'none', beep = FALSE, sound = 1,
                          CI = .95, seed = NULL, boot_method='none', boot_draws = 1000L,
                          max_errors = 50L, resume = TRUE, save_details = list(),
                          control = list(), progress = TRUE, verbose = TRUE)
{
    stopifnot(!missing(analyse))
    if(length(control)){
        stopifnot("Argument(s) to control list invalid"=
                      all(names(control) %in% valid_control.list()))
    }
    if(length(save_details)){
        stopifnot("Argument(s) to save_details list invalid"=
                      all(names(save_details) %in% valid_save_details.list()))
    }
    if(replications < 3L){
        if(verbose)
            message('save, stop_on_fatal, and print_RAM flags disabled for testing purposes')
        control$print_RAM <- FALSE
        if(is.null(control$stop_on_fatal))
            control$stop_on_fatal <- TRUE
    }
    resume.row <- NA
    if(is.numeric(resume)){
        resume.row <- resume
        resume <- TRUE
    }
    if(!verbose) control$print_RAM <- FALSE
    ANALYSE_FUNCTIONS <- TRY_ALL_ANALYSE <- NULL
    if(is.character(parallel)){
        useFuture <- tolower(parallel) == 'future'
        parallel <- TRUE
    } else useFuture <- FALSE
    if(is.null(seed))
        seed <- genSeeds(design)
    if(debug != 'none'){
        if(grepl('-', debug)){
            tmp <- strsplit(debug, '-')[[1]]
            debug <- tmp[1L]
            design <- design[as.integer(tmp[2L]), , drop=FALSE]
            seed <- seed[as.integer(tmp[2L])]
        }
    }
    if(missing(generate) && !missing(analyse))
        generate <- function(condition, dat, fixed_objects){}
    if(is.list(generate)){
        if(debug %in% c('all', 'generate'))
            stop('debug input not supported when generate is a list', call.=FALSE)
        if(any(debug == names(generate))){
            generate <- generate[[which(debug == names(generate))]]
            debug <- 'generate'
        } else {
            for(i in 1L:length(generate))
                generate[[i]] <- compiler::cmpfun(generate[[i]])
            .SIMDENV$GENERATE_FUNCTIONS <- generate
            generate <- combined_Generate
            for(i in 1L:length(generate)){
                char_functions <- deparse(substitute(.SIMDENV$GENERATE_FUNCTIONS[[i]]))
                if(any(grepl('browser\\(', char_functions))){
                    if(verbose && parallel)
                        message(paste0('A browser() call was detected. Parallel processing/object ',
                                       'saving will be disabled while visible'))
                    save <- save_results <- save_seeds <- parallel <- MPI <- useFuture <- FALSE
                }
            }
        }
    }
    if(is.list(analyse)){
        # stopifnot(length(names(analyse)) > 0L)
        if(debug %in% c('all', 'analyse'))
            stop('debug input not supported when analyse is a list', call.=FALSE)
        if(any(debug == names(analyse))){
            analyse <- analyse[[which(debug == names(analyse))]]
            debug <- 'analyse'
        } else {
            for(i in 1L:length(analyse))
                analyse[[i]] <- compiler::cmpfun(analyse[[i]])
            .SIMDENV$ANALYSE_FUNCTIONS <- ANALYSE_FUNCTIONS <- analyse
            .SIMDENV$TRY_ALL_ANALYSE <- TRY_ALL_ANALYSE  <-
                ifelse(is.null(control$try_all_analyse),
                       TRUE, control$try_all_analyse)
            analyse <- combined_Analyses
            for(i in 1L:length(ANALYSE_FUNCTIONS)){
                char_functions <- deparse(substitute(ANALYSE_FUNCTIONS[[i]]))
                if(any(grepl('browser\\(', char_functions))){
                    if(verbose && parallel)
                        message(paste0('A browser() call was detected. Parallel processing/object ',
                                       'saving will be disabled while visible'))
                    save <- save_results <- save_seeds <- parallel <- MPI <- useFuture <- FALSE
                }
            }
        }
    }
    stopifnot(notification %in% c('none', 'condition', 'complete'))
    if(notification != 'none')
        if(!("RPushbullet" %in% (.packages())))
            stop('Please use library(RPushbullet) to load the default ~/.rpushbullet.json file',
                 call. = FALSE)
    save_seeds <- ifelse(is.null(control$save_seeds),
                      FALSE, control$save_seeds)
    store_Random.seeds <- ifelse(is.null(control$store_Random.seeds),
                                  FALSE, control$store_Random.seeds)
    store_warning_seeds <- ifelse(is.null(control$store_warning_seeds),
                                  FALSE, control$store_warning_seeds)
    warnings_as_errors <- ifelse(is.null(control$warnings_as_errors),
                                 FALSE, control$warnings_as_errors)
    allow_na <- ifelse(is.null(control$allow_na), FALSE, control$allow_na)
    allow_nan <- ifelse(is.null(control$allow_nan), FALSE, control$allow_nan)
    print_RAM <- ifelse(is.null(control$print_RAM), TRUE, control$print_RAM)
    stop_on_fatal <- ifelse(is.null(control$stop_on_fatal),
                            FALSE, control$stop_on_fatal)
    max_time <- ifelse(is.null(control$max_time), Inf, control$max_time)
    max_RAM <- ifelse(is.null(control$max_RAM), Inf, control$max_RAM)
    max_time <- sbatch_time2sec(max_time)
    max_RAM <- sbatch_RAM2bytes(max_RAM)
    MPI <- ifelse(is.null(control$MPI), FALSE, control$MPI)

    .options.mpi <- ifelse(is.null(control$.options.mpi),
                           list(), control$.options.mpi)
    type <- if(is.null(control$type))
        ifelse(.Platform$OS.type == 'windows', 'PSOCK', 'FORK')
        else control$type
    if(!is.null(control$include_replication_index) &&
       !is.null(control$include_reps))
        stop('Please only use one replication index flag', call.=FALSE)
    if(!is.null(control$include_reps))
        control$include_replication_index <- control$include_reps
    include_replication_index <- ifelse(is.null(control$include_replication_index),
                                        FALSE, control$include_replication_index)
    if(verbose){
        if(any(replications >= 200))
            if(!save_results && !store_results)
                message(c('NOTE: using save_results or store_results is ',
                        'recommended for higher replication simulations'))
    }
    NA_summarise <- FALSE
    if(!missing(summarise)){
        NA_summarise <- if(!is.function(summarise) && is.na(summarise)) TRUE else FALSE
        if(NA_summarise){
            summarise <- function(condition, results, fixed_objects){0}
            if(!save_results)
                message('NA value for summarise input supplied; automatically setting save_results to TRUE\n')
            save <- save_results <- TRUE
        }
    } else save <- FALSE
    if(!all(names(save_results) %in%
            c('compname', 'save_results_dirname')))
        stop('save_details contains elements that are not supported', call.=FALSE)
    generate <- compiler::cmpfun(generate)
    analyse <- compiler::cmpfun(analyse)

    compname <- save_details$compname
    safe <- save_details$safe
    out_rootdir <- save_details$out_rootdir
    tmpfilename <- save_details$tmpfilename
    save_results_dirname <- save_details$save_results_dirname
    save_results_filename <- save_details$save_results_filename
    save_seeds_dirname <- save_details$save_seeds_dirname

    if(!verbose) progress <- FALSE
    if(is.null(compname)) compname <- Sys.info()['nodename']
    if(is.null(safe)) safe <- TRUE
    if(is.null(out_rootdir)) { out_rootdir <- '.' }
       else { dir.create(out_rootdir, showWarnings=FALSE) }
    if(is.null(tmpfilename))
        tmpfilename <- paste0('SIMDESIGN-TEMPFILE_', compname, '.rds')
    if(is.null(save_results_dirname)){
        save_results_dirname <-
            if(!is.null(filename)) paste0(filename, '-results_', compname)
            else paste0('SimDesign-results_', compname)
    }
    if(is.null(save_seeds_dirname)){
        save_seeds_dirname <- if(!is.null(filename))
            paste0(filename, '-seeds_', compname)
            else paste0('SimDesign-seeds_', compname)
    }
    if(!is.null(filename)){
        if(grepl('\\.rds', filename))
            filename <- gsub('\\.rds', '', filename)
    }
    if(!is.null(cl)) parallel <- TRUE
    if(!is.null(load_seed)) seed <- NULL
    debug <- tolower(debug)
    summarise_asis <- FALSE
    if(missing(summarise)){
        summarise <- function(condition, results, fixed_objects) results
        summarise_asis <- TRUE
        stored_time <- 0
        if(save || save_results)
            message("save-based inputs not used when summarise input is missing.
                    Consider passing summarise=NA instead")
        save <- save_results <- FALSE
    }
    Functions <- list(generate=generate, analyse=analyse, summarise=summarise)
    dummy_run <- FALSE
    if(missing(design)){
        design <- createDesign(dummy_run=NA)
        dummy_run <- TRUE
    }
    if(nrow(design) == 1L){
        verbose <- FALSE
        store_results <- TRUE
    }
    if(is.null(attr(design, 'Design.ID')))
        attr(design, 'Design.ID') <- 1L:nrow(design)
    Design.ID <- attr(design, 'Design.ID')
    if(save_results) store_results <- FALSE
    SimSolveRun <- !is.null(attr(design, 'SimSolve'))
    stopifnot(!missing(replications))
    replications <- as.integer(replications)
    if(length(replications) == 1L)
        replications <- rep(replications, nrow(design))
    stopifnot("length of replications not equal to nrow(design)"=
                  nrow(design) == length(replications))
    if(!is.null(seed))
        stopifnot(nrow(design) == length(seed))
    debug <- tolower(debug)
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
        if(length(load_seed) == 7L){
            rngkind <- RNGkind()
            RNGkind("L'Ecuyer-CMRG")
            on.exit(RNGkind(rngkind[1L]), add = TRUE)
        }
        save <- save_seeds <- parallel <- MPI <- useFuture <- FALSE
        replications <- rep(1L, nrow(design))
        if(is.character(load_seed)){
            load_seed2 <- gsub('design-row-', '', load_seed)
            start <- end <- as.numeric(gsub('/.*', '', load_seed2))
            load_seed <- paste0(save_seeds_dirname, '/', load_seed)
            load_seed <- as.integer(scan(load_seed, sep = ' ', quiet = TRUE))
        }
        if(is(load_seed, 'tbl'))
            load_seed <- as.integer(as.data.frame(load_seed)[,1])
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
        save <- save_results <- save_seeds <- parallel <- MPI <- useFuture <- FALSE
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
    if(any(replications < 1L))
        stop('number of replications must be greater than or equal to 1', call. = FALSE)
    if(!(debug %in% c('none', 'analyse', 'generate', 'summarise', 'all', 'error')))
        stop('debug input is not valid', call. = FALSE)
    if(!is.null(ANALYSE_FUNCTIONS) && debug == 'analyse')
        stop(c('debug = \"analyse" not supported when functions are a list. Please place a browser()',
             " in the respective function location that you are trying to debug"), call.=FALSE)
    if(any(names(design) == 'REPLICATION')){
        stop("REPLICATION is a reserved keyword in the design object. Please use another name",
             call.=FALSE)
    } else if(include_replication_index){
        design <- data.frame(REPLICATION=integer(nrow(design)), design)
    }
    if(!any(names(design) == 'ID')){
        design <- data.frame(ID=1L:nrow(design), design)
    } else stopifnot(length(unique(design$ID)) == nrow(design))
    use_try  <- debug != 'error'
    if(debug != 'none' && use_try){
        save <- save_results <- save_seeds <- FALSE
        if(!(debug %in% 'summarise')) parallel <- MPI <- useFuture <- FALSE
        if(debug == 'all'){
            debug(Functions[['generate']]); debug(Functions[['analyse']])
            debug(Functions[['summarise']])
            on.exit({undebug(Functions[['generate']]); undebug(Functions[['analyse']])
                undebug(Functions[['summarise']])}, add = TRUE)
        } else {
            debug(Functions[[debug]])
            on.exit(undebug(Functions[[debug]]), add = TRUE)
        }
    }
    export_funs <- parent_env_fun()
    if(parallel){
        if(!useFuture && is.null(cl)){
            cl <- parallel::makeCluster(ncores, type=type)
            on.exit(parallel::stopCluster(cl), add = TRUE)
        }
        if(!useFuture){
            parallel::clusterExport(cl=cl, export_funs, envir = parent.frame(1L))
            parallel::clusterExport(cl=cl, "ANALYSE_FUNCTIONS", envir = environment())
            parallel::clusterExport(cl=cl, "TRY_ALL_ANALYSE", envir = environment())
            if(verbose)
                message(sprintf("\nNumber of parallel clusters in use: %i", length(cl)))
        }
    }
    Result_list <- vector('list', nrow(design))
    names(Result_list) <- rownames(design)
    time0 <- time1 <- proc.time()[3L]
    files <- dir(out_rootdir)
    if(resume && !MPI && any(files == tmpfilename) && is.null(load_seed) && debug == 'none'){
        if(verbose && is.na(resume.row))
            message(sprintf(c('Resuming simulation from %s file. ',
                              '\nIf not intended, use SimClean() prior to calling runSimulation()'),
                            file.path(out_rootdir, tmpfilename)))

        Result_list <- readRDS(file.path(out_rootdir, tmpfilename))
        if(unname(attr(Result_list, 'SimDesign_names')['design_names']) !=
           paste0(colnames(design), collapse=';'))
            stop('design names are not the same upon resuming simulation.', call.=FALSE)
        if(nrow(design) != length(Result_list)){
            if(nrow(design) < length(Result_list))
                Result_list <- Result_list[1L:nrow(design)]
            else if(nrow(design) > length(Result_list)){
                tmp_new <- vector('list', nrow(design))
                names(tmp_new) <- 1L:nrow(design)
                tmp_new[1L:length(Result_list)] <- Result_list
                Result_list <- tmp_new
            }
        }
        start <- ifelse(is.na(resume.row),
                        min(c(which(sapply(Result_list, is.null)), nrow(design))),
                        resume.row)
        time0 <- time1 - Result_list[[start-1L]]$SIM_TIME
    }
    TIME0 <- proc.time()[3L]
    if(file.exists(tmpfilename)){
        tmp <- attr(Result_list, 'SimDesign_names')
        save_results_dirname <- tmp['save_results_dirname']
        save_seeds_dirname <- tmp['save_seeds_dirname']
        design_names <- tmp['design_names']
    }
    if(save_results){
        save <- TRUE
        if(!file.exists(file.path(out_rootdir, tmpfilename))) {
            if(safe && is.null(save_results_filename)){
                tmp <- save_results_dirname
                count <- 1L
                while(dir.exists(file.path(out_rootdir, save_results_dirname))) {
                    save_results_dirname <- paste0(tmp, '_', count)
                    count <- count + 1L
                }
                if(tmp != save_results_dirname && is.null(save_results_filename) && verbose)
                    message(sprintf('%s already exists; using %s directory instead',
                                    file.path(out_rootdir, tmp),
                                    file.path(out_rootdir, save_results_dirname)))
            }
            dir.create(file.path(out_rootdir, save_results_dirname), showWarnings=FALSE)
        }
        if(!!is.null(save_results_filename) &&
           !(length(dir(file.path(out_rootdir, save_results_dirname))) %in% c(start - 1L, start)))
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
                                    file.path(out_rootdir, tmp),
                                    file.path(out_rootdir, save_seeds_dirname)))
            }
            dir.create(file.path(out_rootdir, save_seeds_dirname))
        }
    }
    load_packages(packages)
    if(safe && (parallel || MPI)){
        tmp <- packages[packages != 'SimDesign']
        if(!length(tmp)) tmp <- 'stats'
        if(!useFuture){
            if(parallel){
                parallel::parSapply(cl, 1L:(length(cl)*2),
                                    function(ind, packages) load_packages(packages),
                                    packages=packages)
            }
        } else {
            future.apply::future_lapply(1L:(future::nbrOfWorkers()*2),
                                        function(ind, packages) load_packages(packages),
                                        packages=packages)
        }
        for(i in 1L:length(tmp)){
            packs <- if(useFuture){
                try(future.apply::future_lapply(rep(tmp[i], each=future::nbrOfWorkers()*2),
                                            get_packages))
                } else if(parallel){
                try(table(parallel::parSapply(cl, rep(tmp[i], each=length(cl)*2),
                                              get_packages)))
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
          save_seeds_dirname=file.path(out_rootdir, save_seeds_dirname),
          design_names=paste0(colnames(design), collapse=';'))
    if(progress) verbose <- TRUE
    memory_used <- character(nrow(design)+1L)
    if(print_RAM)
        memory_used[1L] <- RAM_used()
    if(is.finite(max_RAM)){
        tmp <- RAM_used(format = FALSE)
        max_RAM <- max_RAM - tmp
        if(max_RAM < 0)
            stop(sprintf('max_RAM must be higher than %s. Please increase', RAM_used()), call.=FALSE)
    }
    for(i in start:end){
        time0 <- proc.time()[3L]
        if(summarise_asis){
            if(verbose)
                print_progress(i, nrow(design), stored_time=stored_time,
                               replications=replications[i],
                               RAM=memory_used[i], progress=progress,
                               condition=if(was_tibble) dplyr::as_tibble(design[i,])
                               else design[i,])
            Result_list[[i]] <- Analysis(Functions=Functions,
                                         condition=if(was_tibble) dplyr::as_tibble(design[i,])
                                           else design[i,],
                                         replications=replications[i],
                                         fixed_objects=fixed_objects,
                                         cl=cl, MPI=MPI, .options.mpi=.options.mpi, seed=seed,
                                         boot_draws=boot_draws, boot_method=boot_method, CI=CI,
                                         save=save, allow_na=allow_na, allow_nan=allow_nan,
                                         save_results=save_results, useFuture=useFuture,
                                         store_Random.seeds=store_Random.seeds,
                                         store_warning_seeds=store_warning_seeds,
                                         save_results_out_rootdir=out_rootdir,
                                         save_results_dirname=save_results_dirname,
                                         save_results_filename=save_results_filename,
                                         arrayID=save_details$arrayID,
                                         multirow=nrow(design) > 1L,
                                         save_seeds=save_seeds, summarise_asis=summarise_asis,
                                         save_seeds_dirname=save_seeds_dirname,
                                         max_errors=max_errors, packages=packages,
                                         include_replication_index=include_replication_index,
                                         load_seed=load_seed, export_funs=export_funs,
                                         warnings_as_errors=warnings_as_errors,
                                         progress=progress, store_results=FALSE, use_try=use_try,
                                         stop_on_fatal=stop_on_fatal, max_time=max_time, max_RAM=max_RAM,
                                         allow_gen_errors=!SimSolveRun)
            time1 <- proc.time()[3L]
            stored_time <- stored_time + (time1 - time0)
        } else {
            stored_time <- do.call(c, lapply(Result_list, function(x) x$SIM_TIME))
            if(verbose)
                print_progress(i, nrow(design), stored_time=stored_time,
                               replications=replications[i],
                               RAM=memory_used[i], progress=progress,
                               condition=if(was_tibble) dplyr::as_tibble(design[i,])
                               else design[i,])
            if(save_seeds)
                dir.create(file.path(out_rootdir,
                                     paste0(save_seeds_dirname, '/design-row-', i)),
                           showWarnings = FALSE)
            tmp <- Analysis(Functions=Functions,
                            condition=if(was_tibble) dplyr::as_tibble(design[i,]) else design[i,],
                            replications=replications[i],
                            fixed_objects=fixed_objects,
                            cl=cl, MPI=MPI, .options.mpi=.options.mpi, seed=seed,
                            store_Random.seeds=store_Random.seeds,
                            boot_method=boot_method, boot_draws=boot_draws, CI=CI,
                            save=save, allow_na=allow_na, allow_nan=allow_nan,
                            save_results=save_results, useFuture=useFuture,
                            store_warning_seeds=store_warning_seeds,
                            save_results_out_rootdir = out_rootdir,
                            save_results_dirname=save_results_dirname,
                            save_results_filename=save_results_filename,
                            save_seeds=save_seeds, summarise_asis=summarise_asis,
                            save_seeds_dirname=save_seeds_dirname,
                            arrayID=save_details$arrayID,
                            multirow=nrow(design) > 1L,
                            max_errors=max_errors, packages=packages,
                            include_replication_index=include_replication_index,
                            load_seed=load_seed, export_funs=export_funs,
                            warnings_as_errors=warnings_as_errors,
                            progress=progress, store_results=store_results, use_try=use_try,
                            stop_on_fatal=stop_on_fatal, max_time=max_time, max_RAM=max_RAM,
                            allow_gen_errors=!SimSolveRun)
            if(SimSolveRun){
                full_results <- attr(tmp, 'full_results')
                condition <- if(was_tibble) dplyr::as_tibble(design[i,]) else design[i,]
                summary_results <- sapply(1L:replications[i], function(i){
                    summarise(condition=condition,
                              results=if(!is.data.frame(full_results) &&
                                         is.list(full_results)) full_results[i]
                                      else full_results[i, , drop=FALSE],
                              fixed_objects=fixed_objects)
                })
                return(list(value=tmp[1L], summary_results=summary_results))
            }
            if(store_results)
                stored_Results <- attr(tmp, 'full_results')
            Result_list[[i]] <- data.frame(design[i, ], as.list(tmp),
                                           check.names=FALSE)
            if(store_results)
                attr(Result_list[[i]], 'full_results') <- stored_Results
            attr(Result_list[[i]], 'Random.seeds') <- attr(tmp, 'stored_Random.seeds')
            attr(Result_list[[i]], 'error_seeds') <- attr(tmp, 'error_seeds')
            attr(Result_list[[i]], 'warning_seeds') <- attr(tmp, 'warning_seeds')
            attr(Result_list[[i]], 'summarise_list') <- attr(tmp, 'summarise_list')
            Result_list[[i]]$COMPLETED <- date()
            time1 <- proc.time()[3L]
            Result_list[[i]]$SIM_TIME <- time1 - time0
            if(save || save_results)
                saveRDS(Result_list, file.path(out_rootdir, tmpfilename))
        }
        if(notification == 'condition')
            notification_condition(design[i,], Result_list[[i]], nrow(design))
        if(print_RAM)
            memory_used[i+1L] <- RAM_used()
        if(nrow(design) > 1L && i < nrow(design)){
            if((time1 - TIME0) > max_time){
                stop('max_time exceeded. See the stored temporary files for last evaluated results',
                     call.=FALSE)
            }
            if(is.finite(max_RAM)){
                if(RAM_used(format = FALSE) > max_RAM)
                    stop('max_RAM exceeded. See the stored temporary files for last evaluated results',
                         call.=FALSE)
            }
        }
    }

    memory_used <- memory_used[-1L]
    attr(Result_list, 'SimDesign_names') <- NULL
    if(NA_summarise){
        Result_list <- lapply(Result_list, function(x){
            x$value <- NULL
            x
        })
    }
    if(store_results && !summarise_asis){
        stored_Results_list <- lapply(Result_list, \(x) attr(x, 'full_results'))
        if(is(stored_Results_list[[1L]], 'data.frame') ||
           is(stored_Results_list[[1L]], 'matrix')){
            for(i in seq_len(length(stored_Results_list)))
                stored_Results_list[[i]] <- cbind(design[i,],
                                                 stored_Results_list[[i]], row.names=NULL)
            stored_Results_list <- dplyr::bind_rows(stored_Results_list)
            stored_Results_list$ID <- NULL
            stored_Results_list <- dplyr::as_tibble(stored_Results_list)
        }
    }
    if(summarise_asis){
        design$ID <- design$REPLICATION <- NULL
        if(is(Result_list[[1L]], 'data.frame') || is(Result_list[[1L]], 'matrix')){
            nms <- c(colnames(design), colnames(Result_list[[1L]]))
            for(i in seq_len(length(Result_list)))
                Result_list[[i]] <- as.data.frame(cbind(design[i,], Result_list[[i]],
                                                        row.names = NULL))
            ret <- quiet(dplyr::bind_rows(Result_list))
            colnames(ret) <- nms
            if(any(nms == "dummy_run"))
                ret <- ret[ ,nms != "dummy_run", drop=FALSE]
            ret <- dplyr::as_tibble(ret)
            return(ret)
        } else {
            nms <- colnames(design)
            nms2 <- matrix(character(0L), nrow(design), ncol(design))
            for(i in 1L:ncol(design))
                nms2[,i] <- paste0(nms[i], '=', design[,i], if(i < ncol(design)) '; ')
            nms2 <- apply(nms2, 1L, paste0, collapse='')
            names(Result_list) <- nms2
            if(is.list(Result_list[[1L]][[1L]]))
                for(i in seq_len(length(Result_list)))
                    attr(Result_list[[i]][[1L]], 'stored_Random.seeds') <-
                    attr(Result_list[[i]][[1L]], 'try_errors') <-
                    attr(Result_list[[i]][[1L]], 'try_error_seeds') <-
                    attr(Result_list[[i]][[1L]], 'warning_seeds') <-
                    attr(Result_list[[i]][[1L]], 'summarise_list') <- NULL
            if(nrow(design) == 1L) Result_list <- Result_list[[1L]]
        }
        return(Result_list)
    }
    stored_time <- do.call(c, lapply(Result_list, function(x) x$SIM_TIME))
    if(verbose)
        message('\nSimulation complete. Total execution time: ',
                timeFormater_internal(sum(stored_time)), "\n")
    stored_time <- do.call(c, lapply(Result_list, function(x) x$SIM_TIME))
    if(store_Random.seeds){
        stored_Random.seeds_list <- lapply(1L:length(Result_list),
                        function(x)
            attr(Result_list[[x]], "Random.seeds"))

    } else stored_Random.seeds_list <- NULL
    error_seeds <- data.frame(do.call(cbind, lapply(1L:length(Result_list), function(x){
        ret <- attr(Result_list[[x]], "error_seeds")
        if(length(ret) == 0L || nrow(ret) == 0L) return(NULL)
        rownames(ret) <- paste0("Design_row_", x, '.', 1L:nrow(ret), ": ",
                                rownames(ret))
        t(ret)
    })))
    warning_seeds <- data.frame(do.call(cbind, lapply(1L:length(Result_list), function(x){
        ret <- attr(Result_list[[x]], "warning_seeds")
        if(length(ret) == 0L || nrow(ret) == 0L) return(NULL)
        rownames(ret) <- paste0("Design_row_", x, '.', 1L:nrow(ret), ": ",
                                rownames(ret))
        t(ret)
    })))
    summarise_list <- lapply(1L:length(Result_list), function(x)
        attr(Result_list[[x]], "summarise_list")
    )
    Final <- dplyr::bind_rows(Result_list)
    FATAL_TERMINATION <- NA
    if(!stop_on_fatal && any(colnames(Final) == 'FATAL_TERMINATION')){
        warning('One or more design rows were fatally terminated. Please inspect/debug row(s): ',
                paste(which(!is.na(Final$FATAL_TERMINATION)), collapse=','), call.=FALSE)
        FATAL_TERMINATION <- Final$FATAL_TERMINATION
    }
    SIM_TIME <- Final$SIM_TIME
    COMPLETED <- Final$COMPLETED
    if(!is.null(Final$REPLICATIONS)) replications <- Final$REPLICATIONS
    Final$SIM_TIME <- Final$ID <- Final$COMPLETED <-
        Final$REPLICATIONS <- Final$REPLICATION <- Final$FATAL_TERMINATION <- NULL
    Final <- data.frame(Final, FATAL_TERMINATION,
                        REPLICATIONS=replications, SIM_TIME=SIM_TIME,
                        RAM_USED=memory_used,
                        COMPLETED, check.names=FALSE, stringsAsFactors=FALSE)
    if(all(is.na(Final$FATAL_TERMINATION))) Final$FATAL_TERMINATION <- NULL
    if(is.null(Final$SEED)) Final$SEED <- NA
    if(!is.null(seed)) Final$SEED <- seed
    filename <- unique_filename(filename, safe=safe, verbose=verbose)
    dn <- colnames(design)
    dn <- dn[!(dn %in% c('ID', 'REPLICATION'))]
    ten <- colnames(Final)[grepl('ERROR:', colnames(Final))]
    wen <- colnames(Final)[grepl('WARNING:', colnames(Final))]
    ERROR_msg <- Final[ ,ten, drop=FALSE]
    WARNING_msg <- Final[ ,wen, drop=FALSE]
    colnames(ERROR_msg) <- gsub("ERROR: ." , "ERROR:  ", colnames(ERROR_msg))
    colnames(WARNING_msg) <- gsub("WARNING: ." , "WARNING:  ", colnames(WARNING_msg))
    ERRORS <- as.integer(rowSums(ERROR_msg, na.rm = TRUE))
    WARNINGS <- as.integer(rowSums(WARNING_msg, na.rm = TRUE))
    en <- c('REPLICATIONS', 'SIM_TIME', 'RAM_USED', 'SEED', 'COMPLETED')
    bsen <- colnames(Final)[grepl('BOOT_', colnames(Final))]
    sn <- colnames(Final)[!(colnames(Final) %in% c(dn, en, ten, wen, bsen))]
    Final <- data.frame(Final[ ,c(dn, sn, bsen, en)], ERRORS, WARNINGS,
                                         check.names = FALSE)
    if(is.list(seed)){
        Final$SEED <- NULL
        en <- en[-4L]
    }
    if(all(memory_used == "")) Final$RAM_USED <- NULL
    if(all(ERRORS == 0)) Final$ERRORS <- NULL
    if(all(WARNINGS == 0)) Final$WARNINGS <- NULL
    Final <- dplyr::as_tibble(Final)
    attr(Final, 'design_names') <-
        list(design=dn, sim=sn, bootCI=bsen, extra=en, errors='ERRORS', warnings="WARNINGS")
    if(length(packages) > 1L){
        pack <- packages[packages != 'SimDesign']
        versions <- character(length(pack))
        for(i in 1L:length(pack))
            versions[i] <- as.character(packageVersion(pack[i]))
        pack_vers <- data.frame(packages=pack, versions=versions)
    } else pack_vers <- NULL
    pick <- c(save_results, save_seeds)
    if(!is.null(filename)) pick <- c(save, pick)
    attr(Final, "ERROR_msg") <- dplyr::as_tibble(collect_unique(ERROR_msg))
    attr(Final, "WARNING_msg") <- dplyr::as_tibble(collect_unique(WARNING_msg))
    attr(Final, 'extra_info') <- list(sessionInfo = sessioninfo::session_info(), packages=pack_vers,
                                      save_info = c(filename=filename,
                                                    save_results_dirname=save_results_dirname,
                                                    save_seeds_dirname=save_seeds_dirname)[pick],
                                      seeds=seed, stored_Random.seeds_list=stored_Random.seeds_list,
                                      ncores = if(parallel) length(cl) else if(MPI) NA else 1L,
                                      number_of_conditions = nrow(design),
                                      date_completed = noquote(date()), total_elapsed_time = sum(SIM_TIME),
                                      error_seeds=dplyr::as_tibble(error_seeds),
                                      warning_seeds=dplyr::as_tibble(warning_seeds),
                                      stored_results = if(store_results) stored_Results_list else NULL,
                                      summarise_list=summarise_list, Design.ID=Design.ID)
    if(!is.null(summarise_list[[1L]]) && verbose)
        message('Note: To extract Summarise() results use SimExtract(., what = \'summarise\')')
    if(dummy_run) Final$dummy_run <- NULL
    class(Final) <- c('SimDesign', class(Final))
    if(!is.null(filename)){ #save file
        if(verbose)
            message(paste('\nSaving simulation results to file:', filename))
        saveRDS(Final, file.path(out_rootdir, filename))
    }
    if(save || save_results || save_seeds) file.remove(file.path(out_rootdir, tmpfilename))
    if(notification %in% c('condition', 'complete')) notification_final(Final)
    if(beep)
        beepr::beep(sound=sound)
    return(Final)
}

#' @rdname runSimulation
#' @param object SimDesign object returned from \code{\link{runSimulation}}
#' @param ... additional arguments
#' @export
summary.SimDesign <- function(object, ...){
    ret <- attr(object, 'extra_info')
    ret$total_elapsed_time <- noquote(
        timeFormater_internal(ret$total_elapsed_time, TRUE))
    ret$stored_results <- NULL
    ret$error_seeds <- NULL
    ret$warning_seeds <- NULL
    ret$summarise_list <- NULL
    ret <- ret[!sapply(ret, is.null)]
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
    if(suppressWarnings(!is.null(x$SIM_TIME)))
        x$SIM_TIME <- sapply(x$SIM_TIME, function(x)
            noquote(timeFormater_internal(x)))
    class(x) <- c('Design', class(x)[-1L])
    print(x=x, list2char=list2char, ...)
}

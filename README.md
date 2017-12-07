The package contains utility function for dealing with
eye-tracking data (reading studies only). The package is assuming the output
produced by the eye-tracking scripts from UMD ("EyePy"). In principle, it
should be flexible enough to work with any similarly organized data, but has
not been tested with any other scripts explicitly.

The functions fall into one of the following groups:

1. **Project managing functions** (`make_filename`, `save_named_data_file`, `save_named_table`).
  
    The following project management idea is behind these functions. It is assumed 
    that the project has several well-defined stages of data analysis. For the 
    sake of example, let's say these are preprocessing, cleanup  and analysis itself.
    Let us also assume that each of the stages can be accomplished in several ways. 
    E.g. during cleanup stage you may or may not want to replace NAs with 0, you 
    may or may not want to trim outliers etc.
    
    Then, you could write a separate small subscript for each variant of procedure.
    and call them in sequence from a master file. Each script would save intermediate 
    data and the next script in sequence would read it. I.e. it behaves similar to
    piping, with the difference that intermediate results are saved on disk. Thus,
    analyses pipelines relying on same sub-procedures (e.g. pre-processing the data
    in the same way, but, say, cleaning it up differently) may rely on the same
    intermediate results without the need of recomputing them.
    
    Now, the package provides the functions which name the intermediate files so
    that it is easy to identify which data is contained in them. For example, suppose
    you have a "default" preprocessing procedure and two cleanup procedures, 
    "no-outliers" and "no-NAs". Then you could have two datasets, containing the 
    cleaned data, named something like `prep.default_cl.no-outliers` and
    `prep.default_cl.no-NAs`. 
    
    The names follow a "naming schema", which is defined by you. It works as follows.
    First, you create a list indicating which subparts a name of the file could in
    principle have. The names of the list components correspond to longer descriptions
    of the filename parts, and the values of the list correspond to shorter tags
    which would actually be used in the filename. E.g.
    
        tagging.schema <- list(preprocessing = "prep",
                               cleanup = "cl",
                               analysis = "an")
    
    Then, within each analysis script, another list is created. The names should be
    a subset of the names of tagging.schema; and the values correspond to specific
    parameters used in this script. E.g.
    
        values.schema <- list(preprocessing = "default",
                              cleanup = "no-outliers")
    
    These two lists are necessary for any of the function in this group. The basic
    function is `make_filename`, which combines the tags in a single name, e.g.
    `projectname_prep.default_cl.no-outliers`. The other two functions call
    this function, and then additionally save the .Rdata or a .csv file with 
    the newly created name. See the help pages for the functions for more details
    on their parameters.
    
    The delimeters between the naming components and between the tag and the value
    are customizable. Obviously, the values and tags should not contain characters
    used as delimiters. E.g., if your tags and values are delimited by ".", you
    cannot set a tag like "mk.prep".
    
2. **Eye-tracker data convenience functions**.

    + functions for quick counting of NAs or extreme values (above
    or below a specified threshold), and creating convenient visual summaries
    (`count_NAs`, `count_extremes`)
    + functions for extracting info about your subjects (their number, with indication
    of whether any are missing, and question answering accuracy) (`get_subj_info`,
    `get_quest_acc`)
    + function for assigning convenient names to regions of interest (`name_regions`)
    + function for creating convenient axis labels from long strings (e.g. if
    you have something like "spillover_region_first_pass", the function would
    insert newlines instead of "_", so that the label like this could better 
    fit in a plot. Whether it's a good idea to use such a long label is another
    question...) (`normalize_axis_labels`)
    
3. **General convenience functions**

    + Functions to copy all objects in the current environment into a new
    environment or vice versa (`preserve_env`, `preserve_global_env`, `restore_env`).
    Can be useful if you are  piping several scripts together, as described earlier,
    and want the scripts be executed in essentially empty environment. However, since
    the naming functions would need to know what the naming schema is, you can't
    just purge the environment altogether. The suggested strategy is instead this:
    
        + in the very beginning of each script, capture the current environment to 
        a backup environment with `preserve_global_env`. Normally this would only 
        include whatever parameters are specified in the master file;
        + purge the global environment, removing everything except the backup
        environment. You can do this by executing something like
        `rm(list=setdiff(ls(), "backup_env"))`
        + wrap the rest of the script commands in `tryCatch...finally`, and
        in the `finally` block restore the saved environment with `restore_global_env`.
        This would help to ensure that even if there is an error during the script, 
        the original environment is still restored. 

        Thus, the whole thing would look like this:

          library(ettools)

          backup_env <- preserve_global_env()

          # clean-up the environment, except the backup_env
          rm(list=setdiff(ls(), "backup_env))

          tryCatch({ # even if some script sources with an error...
            ### your code ###
          },
          finally = { # ... we still need to restore the environment we started with
            # clean-up the environment, except the backup_env. We need to do it because
            # otherwise the objects from the backup environment would be added to the 
            global environment, but we may not want to keep anything that the subscript
            # created
            rm(list=setdiff(ls(), "backup_env"))
            # restore global environment
            restore_global_env(backup_env)
          })




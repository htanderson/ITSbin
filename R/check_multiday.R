#' @title check_multiday
#'
#' This function checks each ITS file in a folder to determine if each/any ITS file contains recordings which start on more than one day, based on the specified local `time.zone`. This happens when a family/user starts recording one one day, turns the recorder off, then turns the recorder on again on a later day.
#'
#'  For any ITS file containing more than one recording day, this function will output a CSV containing Recording-level information for the user to review. If any files contain more than one recording day, use the function `remove_recordings` to separate the recordings into multiple ITS files before passing the ITS files to the function `ITS_to_seconds`.
#'
#'   If desired, this function will also output the ITS Recording-level information for every input ITS file.
#'
#' For more information, including example outputs and explanations of all output columns, see \url{https://htanderson.github.io/ITSbin/}
#'
#' @param ITS.dir Directory (string) containing ITS files. Default = working directory.
#' @param CSV.dir Directory (string) to store CSV output files.
#' @param time.zone OS-specific character string for time zone. To use current system timezone, type `Sys.timezone()`. For other options, run `OlsonNames()` for list.
#' @param write.all.recordings Logical. Default = `FALSE`. Output a CSV containing ITS Recording-level information for each input ITS file. Note: The recordings CSV will *always* be written for any ITS files containing recordings which start on more than one day.
#' @return An its/xml file
#' @import xml2
#' @import data.table
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#'
#' check_multiday(
#' ITS.dir = "SERVER:/ITS_Files/",
#' CSV.dir = "SERVER:/CSVOutput/",
#' time.zone = "America/Los_Angeles")
#'
#' }

check_multiday <-
  function(
    ITS.dir,
    CSV.dir,
    time.zone = NULL,
    write.all.recordings = FALSE) {

    # options(digits.secs = 2) ### to view centiseconds in POSIX formats
    if (is.null(time.zone)) {
      stop(
        "time.zone must be specified. If your .its file timezone is the same as your computer’s current system timezone, use time.zone = Sys.timezone(). If your .its file(s) was/were collected using another time zone, run OlsonNames() for options and select the region that matches your .its file(s)."
      )
    }
    #### setup directories to store output ####

    ### add forward slash to directory names if missing

    input.dirs <-
      list("ITS.dir" = ITS.dir,
           "CSV.dir" = CSV.dir)

    endForwardSlash <-
      # to maintain names, must pass
      # names and objects separately
      function(input.dir, nam) {
        # if directory name doesn't
        # end in forward slash
        # add it
        if (!endsWith(x = input.dir, suffix = "/")) {
          assign(x = nam,
                 value = paste0(input.dir, "/"),
                 inherits = TRUE)
        }
      }

    # use purrr::walk as for-loop/lapply substitute
    # with no output
    purrr::walk(.x = names(input.dirs),
                .f = function(n) endForwardSlash(input.dirs[[n]], n))

    rm(endForwardSlash)

    ### create directories for desired outputs  ###

    # list of all possible outputs
    list.to.write <-
      list("write.recordings" = write.all.recordings,
           "write.multiday" = TRUE)

    # limit to only desired outputs
    list.to.write <-
      list.to.write[list.to.write == TRUE]

    # get names of folders by dropping "write."
    folder.names <-
      names(list.to.write) %>%
      {substr(., 7, nchar(.))}

    # function to create sub-folders
    create.folders <-
      function(folder.name) {

        dir.to.create <-
          paste0(CSV.dir, folder.name, "/")

        dir.create(dir.to.create, showWarnings = FALSE)
      }

    # use purrr::walk as for-loop/lapply substitute
    # with no output
    purrr::walk(.x = folder.names,
                .f = create.folders)


    ##### Helper Functions #####

    # Remove characters from columns with time data
    # EG startTime="PT17239.26S" --> 17239.26 &
    # startClockTime="2017-03-28T14:06:55Z" --> 2017-03-2814:06:55
    dropChars <- function(x){
      gsub(x,
           pattern = "[^0-9.:-]",
           replacement = "")
    }

    ### add unique ids to recs, blks, segs ###

    # CODE BLOCK FROM TJ Mahr:
    # https://github.com/HomeBankCode/rlena
    # NESTED FUNCTION (originally for-loop)
    # Add recording id, block id, and segment id
    # to all annotation nodes.
    # These ids can later serve as foreign keys
    # for join operations etc.
    add_id_attrs <- function(its_xml) {
      # start id counts at 0
      r <- 0
      b <- 0
      s <- 0

      # grab all recording nodes
      recording.nodes <-
        xml2::xml_find_all(
          its_xml,
          xpath = "//ProcessingUnit/Recording")

      # function
      extractRecordings <-
        function(rec) {
          # increase r by 1
          assign(x = "r",
                 value = r + 1,
                 inherits = TRUE)
          xml2::xml_set_attr(rec, "num", NULL)
          xml2::xml_set_attr(rec, "recId", r)

          # grab blocks within recording r
          blocks <-
            xml2::xml_children(recording.nodes[r])

          extractBlocks <-
            function(blk) {
              # iterate b
              assign(x = "b",
                     value = b + 1,
                     inherits = TRUE)

              # blkTypeId: running count of Pauses / Conv.
              blkTypeId <-
                xml2::xml_attr(blk, "num")
              blkType <- xml2::xml_name(blk)
              xml2::xml_set_attr(blk, "recId", r)
              xml2::xml_set_attr(blk, "blkId", b)
              xml2::xml_set_attr(blk, "blkTypeId", blkTypeId)
              xml2::xml_set_attr(blk, "num", NULL)
              xml2::xml_set_attr(blk, "blkType", blkType)

              # grab all segments within block
              segments <- xml2::xml_children(blk)

              extractSegments <-
                function(seg) {
                  # iterate s
                  assign(x = "s",
                         value = s + 1,
                         inherits = TRUE)
                  # set attributes to match parent block
                  xml2::xml_set_attr(seg, "recId", r)
                  xml2::xml_set_attr(seg, "blkId", b)
                  xml2::xml_set_attr(seg, "blkTypeId", blkTypeId)
                  xml2::xml_set_attr(seg, "segId", s)
                  xml2::xml_set_attr(seg, "blkType", blkType)
                }
              purrr::walk(
                .x = segments,
                .f = extractSegments)
            }
          purrr::walk(
            .x = blocks,
            .f = extractBlocks)
        }
      purrr::walk(
        .x = recording.nodes,
        .f = extractRecordings)
    }
    # END CODE BLOCK FROM TJ Mahr

    ### End function for file ###

    completeFile <- function() {

      if (any(validation[, !"subjID"] != TRUE)) {
        message(subjID, " failed at least 1 validation check.
                Script may not have run correctly.")
      }

      # assign to global environment in case
      # function ended early
      assign(x = "validation.allfiles",
             value =
               .rbind.data.table(validation.allfiles,
                                 validation,
                                 fill = TRUE),
             envir = .GlobalEnv)


      # write after every file in case crash
      fwrite(x = validation.allfiles,
             file = paste0(CSV.dir,
                           "ITS_script_validation_inprogress.csv"))

      # assign to global environment in case
      # function ended early
      assign(x = "ITS.checks.allfiles",
             value =
               .rbind.data.table(ITS.checks.allfiles,
                                 ITS.checks,
                                 fill = TRUE),
             envir = .GlobalEnv)


      # write after every file in case crash
      fwrite(x = ITS.checks.allfiles,
             file = paste0(CSV.dir,
                           "ITS_file_checks_inprogress.csv"))

      # track completed processing
      # assign to global environment in case
      # function ended early
      assign(x = "processing.completed.allfiles",
             value =
               .rbind.data.table(processing.completed.allfiles,
                                 processing.completed,
                                 fill = TRUE),
             envir = .GlobalEnv)

      # write after every file in case crash
      fwrite(x = processing.completed.allfiles,
             file = paste0(CSV.dir,
                           "processing_completed_inprogress.csv"))


      functionEndTime <- Sys.time()
      message("
              Finished ", subjID, " at ", functionEndTime)
      timeToRun <- round(functionEndTime - functionStartTime, 2)
      message(
        "Time to process ITS: ", timeToRun, attr(timeToRun, "units"), "
        ")
    }

    ### end helper functions

    # create DTs to store all checks
    assign(x = "validation.allfiles",
           value = data.table(),
           envir = .GlobalEnv)
    assign(x = "ITS.checks.allfiles",
           value = data.table(),
           envir = .GlobalEnv)
    assign(x = "processing.completed.allfiles",
           value = data.table(),
           envir = .GlobalEnv)

    # list of ITS files to iterate over
    ITS.files <-
      dir(path = ITS.dir, pattern = ".its")

    ############ begin file processing ############

    for (ITS.file in ITS.files) {

      functionStartTime <- Sys.time()

      ### Create subject ID from ITS.file name ###

      subjID <- ITS.file %>%
        strsplit(split = ".its") %>%
        unlist

      ### Create Validation Tables ###

      validation <-
        data.table(subjID,
                   recCols14 = NA,
                   nrowsMaxRecId = NA)

      ITS.checks <-
        data.table(subjID,
                   recDayDurationHours = NA,
                   continuousRecordingTime = NA,
                   totalRecordingTime = NA,
                   allRecsSameDay = NA)

      # start all false
      # update as completed
      processing.completed <-
        data.table(subjID,
                   recordings.processed = FALSE,
                   recordings.written = FALSE)

      ###### Import ITS File ######

      # create ITS object in R by reading in file
      its_xml <- paste0(ITS.dir, ITS.file, sep = "") %>%
        xml2::read_xml(x = .)

      # run nested function
      add_id_attrs(its_xml)

      ###### Recordings ######

      # set to NA for "started but not finished"
      processing.completed[,
                           recordings.processed := NA]

      # Import ITS recordings as list
      recordings.DF <-
        its_xml %>%
        # Find only Recording paths directly under ProcessingUnit
        # Some exist under//ProcessingUnit/Bar/Recording - not wanted
        xml_find_all(xpath =  ".//ProcessingUnit/Recording") %>%
        # Extract attributes (information) from Recording paths
        xml_attrs() %>%
        # Convert to data frame list
        # NOTE: converting to data.table directly loses colnames
        lapply(FUN = as.data.frame.list,
               stringsAsFactors = FALSE) %>%
        # rowbind dataframes to data.table
        # fill = TRUE for unequal # of columns
        rbindlist(fill = TRUE)

      # Add Subject ID to dataframe as first column
      recordings.DF <- cbind(subjID, recordings.DF)


      timeCols <-
        names(recordings.DF)[grep("Time", names(recordings.DF))]

      recordings.DF[ , (timeCols):=
                       lapply(.SD, FUN = dropChars),
                     .SDcols = timeCols]

      # Convert clocktimes to posix datetime
      recordings.DF[, ':=' (startClockTime =
                              startClockTime %>%
                              as.POSIXct(tz = "UTC"),
                            endClockTime =
                              endClockTime %>%
                              as.POSIXct(tz = "UTC"))]

      ### Adding local & secMidnight times ###

      # convert clocktimes from UTC to local
      recordings.DF[,
                    ':='
                    ( # add time.zone to keep track
                      timezone = time.zone,
                      startclocklocal = format(startClockTime,
                                               tz = time.zone,
                                               usetz = F) %>%
                        as.POSIXct(tz = time.zone) %>%
                        as.character.POSIXt,
                      endclocklocal = format(endClockTime,
                                             tz = time.zone,
                                             usetz = F) %>%
                        as.POSIXct(tz = time.zone) %>%
                        as.character.POSIXt)]

      # remove time; leave only date
      recordings.DF[, dateMidnight_epoch :=
                      recordings.DF[1, substr(startclocklocal, 1, 10)] %>%
                      # add midnight
                      paste("00:00:00") %>%
                      # convert dateMidnight from character to POSIXct
                      # note: it will not print the time when time is midnight
                      # but it's really there!
                      as.POSIXct(tz = time.zone) %>%
                      # covert midnight into epoch time
                      as.numeric]

      # convert UTC values to unix epoch time
      # (time in seconds from jan 1, 1970)
      recordings.DF[, ":="
                    (startclocklocal_secMidnight =
                        startClockTime %>%
                        as.numeric %>%
                        # subtract to get time in seconds from midnight
                        subtract(e2 = dateMidnight_epoch),
                      # Same for endtime
                      endclocklocal_secMidnight =
                        endClockTime %>%
                        as.numeric %>%
                        subtract(e2 = dateMidnight_epoch))]

      # Amount of time passed in full recording
      # Note, if time ends in 0 (eg 02:25:60),
      # Excel will format as numeric. Data is correct in DF and CSV.
      recordings.DF[, recClockEnd :=
                      endTime %>%
                      lubridate::seconds_to_period() %>%
                      {sprintf('%02d:%02d:%05.2f',
                               lubridate::hour(x = .),
                               lubridate::minute(x = .),
                               lubridate::second(x = .)
                      )}]

      # change names of start/end clocktime to UTC for clarity
      setnames(x = recordings.DF,
               old = c("startClockTime", "endClockTime"),
               new = c("startClockTime_UTC", "endClockTime_UTC"))

      # convert remaining character cols to numeric as appropriate
      charCols <-
        names(dplyr::select_if(.tbl = recordings.DF, is.character))

      recordings.DF[,
                    (charCols) := lapply(.SD, type.convert, as.is = TRUE),
                    .SDcols = charCols]

      # calculate duration of each recording
      recordings.DF[, recDur := endTime - startTime]

      ### Script Validation ###

      # Are there 13 columns in this dataframe?
      validation[, "recCols14" := ncol(recordings.DF) == 14]

      # does the number of rows match the number of recordings?
      validation[, "nrowsMaxRecId" :=
                   nrow(recordings.DF) == recordings.DF[, max(recId)]]

      ### Check Recording Days ###

      ITS.checks[,
                 ":=" (
                   # how long is recording day
                   "recDayDurationHours" =
                     recordings.DF[.N,
                                   endclocklocal_secMidnight] %>%
                     lubridate::seconds_to_period() %>%
                     {sprintf('%02d:%02d:%05.2f',
                              lubridate::hour(x = .)+
                                (lubridate::day(x = .) * 24),
                              lubridate::minute(x = .),
                              lubridate::second(x = .)
                     )},

                   # at least 4 hours continuous
                   "continuousRecordingTime" =
                     recordings.DF[, max(recDur) %>%
                                     lubridate::seconds_to_period() %>%
                                     {sprintf('%02d:%02d:%05.2f',
                                              lubridate::hour(x = .)+
                                                (lubridate::day(x = .) * 24),
                                              lubridate::minute(x = .),
                                              lubridate::second(x = .)
                                     )}],

                   # at least 10 hours of recording
                   "totalRecordingTime" =
                     recordings.DF[, sum(recDur) %>%
                                     lubridate::seconds_to_period() %>%
                                     {sprintf('%02d:%02d:%05.2f',
                                              lubridate::hour(x = .)+
                                                (lubridate::day(x = .) * 24),
                                              lubridate::minute(x = .),
                                              lubridate::second(x = .)
                                     )}])]


      # All recordings start on the same day?
      if (nrow(recordings.DF) > 1) {
        # get date recording 1 started
        dayOne <-
          recordings.DF[1, startclocklocal] %>%
          lubridate::date()
        # do all recordings start on recording 1 day?
        ITS.checks[,
                   "allRecsSameDay" :=
                     recordings.DF[,
                                   all(
                                     lubridate::date(startclocklocal) ==
                                       dayOne)]]

        # warning if recordings START on different days
        # does NOT throw warning if a recording is
        # continuously on past midnight

        rm(dayOne)
        if (ITS.checks[, allRecsSameDay != TRUE]) {

          # create directory to store recording info
          dir.create(paste0(CSV.dir, "multiday/"),
                     showWarnings = FALSE)

          fwrite(x = recordings.DF,
                 file =
                   paste0(CSV.dir, "multiday/",
                          subjID, "_ITS_recordings.csv"))

          processing.completed[, ":=" (
            "recordings.processed" =
              TRUE,
            "recordings.written" =
              TRUE)]

          warning(
            sprintf(
              "WARNING in %s:
        Not all recordings start on the same day.
        Review %s_ITS_Recordings.csv for additional
        recording(s).
                    ",
              subjID, subjID))
        }}

      processing.completed[, "recordings.processed" :=
                             TRUE]

      ### Write CSV of Recordings ###

      if (write.all.recordings) {

        fwrite(x = recordings.DF,
               file =
                 paste0(CSV.dir, "recordings/", subjID, "_ITS_recordings.csv"))

        processing.completed[,
                             "recordings.written" :=
                               TRUE]
      }

      completeFile()

    }

    rm(validation,
       ITS.checks)

    ###### Write Validation & Tracking CSVs ######


    fwrite(x = validation.allfiles,
           file = paste0(CSV.dir, "ITS_script_validation_",
                         format(Sys.time(),
                                "%Y%m%d_%H%M%S"),
                         ".csv"))

    fwrite(x = ITS.checks.allfiles,
           file = paste0(CSV.dir, "ITS_file_checks_",
                         format(Sys.time(),
                                "%Y%m%d_%H%M%S"),
                         ".csv"))


    # write completed processing
    fwrite(x = processing.completed.allfiles,
           file = paste0(CSV.dir, "processing_completed_",
                         format(Sys.time(),
                                "%Y%m%d_%H%M%S"),
                         ".csv"))

    ##### Check Validation All Files #####
    # columns with fails
    colsHaveFail <-
      (!validation.allfiles[,
                            lapply(.SD, all, na.rm = TRUE),
                            .SDcols =
                              colnames(
                                validation.allfiles[,!"subjID"])])

    if (any(colsHaveFail)) {

      ### Failed Subject IDs and columns ###

      # subject IDs with fails
      failedIDs <-
        validation.allfiles[,
                            .("anyFalse" =
                                any(.SD == FALSE,
                                    na.rm = TRUE)),
                            .SDcols = colnames(validation.allfiles[,!"subjID"]),
                            by = "subjID"][
                              anyFalse == TRUE,
                              subjID]

      # names of columns with at least 1 fail
      failCols <-
        colnames(colsHaveFail)[colsHaveFail[1,]]

      # table with only failed cols and subjects
      failedChecks <-
        validation.allfiles[subjID %in% failedIDs,
                            c("subjID", failCols),
                            with = FALSE]

      rm(failCols, failedIDs, colsHaveFail)

      # write failedChecks csv
      fwrite(x = failedChecks,
             file = paste0(CSV.dir, "ValidationFails_",
                           format(Sys.time(),
                                  "%Y%m%d_%H%M%S"),
                           ".csv"))

      warning("Some files failed script validation checks.
              See ValidationFails CSV.")

    } else {
      message("No files failed script validation checks.")
    }
  }

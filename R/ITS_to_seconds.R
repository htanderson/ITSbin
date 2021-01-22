#' @title ITS_to_seconds
#'
#' @description This function takes a folder containing LENA ITS files as input, imports Recordings(recorder turned on & off), Blocks (Conversations & Pauses), and Segments (uninterrupted human-created sounds, electronic sounds, noise, or silence) from each ITS file, converts them into dataframes, then adds time from desired time zone in clocktime and seconds from midnight. Files are expanded from 1 row per segment to 1 row per centisecond, then collapsed to 1 row per second (final output). Centiseconds & seconds files start at midnight the first day the recorder was turned on and end at the later of noon the following day or when the recorder was turned off for the last time.
#'
#'  *Any existing output CSV files will be overwritten.* To avoid this, archive old files in separate folder.
#'
#' **IMPORTANT**: On most computers, `ITS_to_seconds` cannot process ITS files with recordings which start on multiple days, due to excessive RAM requirements. It is *strongly recommended* to run the `check_multiday` function prior to running the `ITS_to_seconds` function to identify any ITS files that might be too long. Then, use the `remove_recordings` function to separate multi-day files for smooth processing.
#'
#' @param ITS.dir Directory (string) containing ITS files.
#' @param CSV.dir Directory (string) to store CSV files
#' @param time.zone OS-specific character string for time zone. To use current system timezone, type `Sys.timezone()`. For other options, run `OlsonNames()` for list.
#' @param write.recordings Logical. Default = FALSE. Output a CSV containing ITS Recording-level information for each input ITS file. Note: The Recordings CSV will *always* be written for any ITS files containing recordings which start on more than one day.
#' @param write.blocks Logical. Default = FALSE. Output a CSV containing ITS Block-level information  (Conversations & Pauses) for each input ITS file.
#' @param write.segments Logical. Default = FALSE. Output a CSV containing ITS Segment-level information (uninterrupted human-created sounds, electronic sounds, noise, or silence) for each input ITS file.
#' @param write.centiseconds Logical. Default = FALSE. Output a CSV containing centisecond-level information for each input ITS file. Output file contains 1 row per centisecond in the day, running from midnight the day the recorder was first turned on until the later of noon the following day or when the recorder was turned off for the last time. WARNING: These files are >2GB each.
#' @param write.seconds Logical. Default = TRUE. Output a CSV containing second-level information for each input ITS file. Output file contains 1 row per second in the day, running from midnight the day the recorder was first turned on until the later of noon the following day or when the recorder was turned off for the last time. This file is required for the `bin_seconds` function.
#' @return Per ITS file: 1 CSV each of specified outputs (recordings, blocks, segments, centiseconds, seconds). Per function run: 1 Validation, 1 ITS_checks, and 1 processing_completed file. If any files fail validation, 1 additional tracking CSV output (ValidationFails.csv)
#' @import data.table
#' @import xml2
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#'
#' ITS_to_seconds(
#' ITS.dir = "SERVER:/ITS_Files/",
#' CSV.dir = "SERVER:/CSVOutput/",
#' time.zone = "America/Los_Angeles")
#'
#' }

ITS_to_seconds <-
  function(
    ITS.dir,
    CSV.dir,
    time.zone = NULL,
    write.recordings = FALSE,
    write.blocks = FALSE,
    write.segments = FALSE,
    write.centiseconds = FALSE,
    write.seconds = TRUE) {

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
      list("write.recordings" = write.recordings,
           "write.blocks" = write.blocks,
           "write.segments" = write.segments,
           "write.centiseconds" = write.centiseconds,
           "write.seconds" = write.seconds)

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


    # keep track of number completed
    ITSfileNum <- 0

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


    ### seg to csec functions ###

    ## most frequent value, na's removed ##
    # of charater type

    stat_mode_narm <-
      function(x) {
        x <- na.omit(x)
        ux <- unique(x)

        mode_loc <-
          which.max(
            tabulate(
              match(x, ux)))
        return(ux[mode_loc])
      }

    ## most frequent value, na's retained ##
    # of charater type
    stat_mode_na <-
      function(x) {
        ux <- unique(x)
        freq <- tabulate(
          match(x, ux))

        mode_loc <-
          which.max(freq)

        return(ux[mode_loc])
      }

    ## most frequent speaker ##
    # if tie, paste with "."
    speaker.freq <-
      function(x){
        ux <- unique(x)
        freq <- tabulate(
          match(x, ux))
        if (all(freq == 50)) {
          return(
            paste(ux[1], ux[2],
                  sep = "."))
        } else {
          mode_loc <-
            which.max(freq)
          return(
            ux[mode_loc])
        }
      }

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
              Finished file ", ITSfileNum, "/", length(ITS.files),
              " ", subjID, " at ", functionEndTime)
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

      ITSfileNum <- ITSfileNum + 1

      ### Create subject ID from ITS.file name ###

      subjID <- ITS.file %>%
        strsplit(split = ".its") %>%
        unlist

      message("
Beginning file ", ITSfileNum, "/", length(ITS.files),
" ", subjID, " at ", functionStartTime)

      ### Create Validation Table ###

      validation <-
        data.table(subjID,
                   recCols14 = NA,
                   nrowsMaxRecId = NA)

      ITS.checks <-
        data.table(subjID,
                   recDayDurationHours = NA,
                   continuousRecordingTime = NA,
                   totalRecordingTime = NA,
                   allRecsSameDay = NA,
                   pctSilenceAndNoise = NA,
                   allColumnsPresentInSegments = NA,
                   missingSegmentColumns = NA,
                   allSpeakersPresent = NA,
                   missingSpkrs = NA)

      # start all false
      # update as completed
      processing.completed <-
        data.table(subjID,
                   recordings.processed = FALSE,
                   recordings.written = FALSE,
                   blocks.processed = FALSE,
                   blocks.written = FALSE,
                   segments.processed = FALSE,
                   segments.written = FALSE,
                   centiseconds.processed = FALSE,
                   centiseconds.written = FALSE,
                   seconds.processed = FALSE,
                   seconds.written = FALSE)

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

      # change names of start/end clocktime to UTC for clarity
      setnames(x = recordings.DF,
               old = c("startClockTime", "endClockTime"),
               new = c("startClockTime_UTC", "endClockTime_UTC"))

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

      # convert remaining character cols to numeric as appropriate
      charCols <-
        names(
          dplyr::select_if(
            .tbl = recordings.DF,
            .predicate = is.character))

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

          # stop if centiseconds or seconds desired
          if (write.centiseconds | write.seconds) {
            warning(
              sprintf(
                "WARNING in %s:
        Not all recordings start on the same day.
        On most computers, all recordings in single
        ITS file must start on same day to process
        centiseconds or seconds.
        Review %s_ITS_Recordings.csv in folder 'multiday'.
        Use `remove_recordings` to separate recordings
        before reprocessing with `ITS_to_seconds`.
        If centiseconds and seconds are not desired,
        specify write.centiseconds = FALSE
        & write.seconds = FALSE to process with multiple
        recording days on single ITS file.

        %s centiseconds and/or seconds may not process.
                    ",
                subjID, subjID, subjID))
          }}}

      processing.completed[, "recordings.processed" :=
                             TRUE]

      ### Write CSV of Recordings ###

      if (write.recordings) {

        fwrite(x = recordings.DF,
               file =
                 paste0(CSV.dir, "recordings/", subjID,
                        "_ITS_recordings.csv"))

        processing.completed[,
                             "recordings.written" :=
                               TRUE]
      }

      # finish if no further downstream required
      if (!write.blocks &
          !write.segments &
          !write.centiseconds &
          !write.seconds) {

        completeFile()
        next

      }

      ###### Conversation & Pause Blocks ######
      # only necessary if output desired
      # skip blocks otherwise
      if (write.blocks) {

        # pre-populate block validation
        validation[, ":="
                   (timesMatchEndToEnd.blks = NA,
                    blockCols53 = NA,
                    nrowsMaxBlkId = NA)]

        # set to NA for "started but not finished"
        processing.completed[,
                             blocks.processed := NA]

        # extract block attributes
        blocks.DF <-
          its_xml %>%
          xml2::xml_find_all(xpath = "//ProcessingUnit/Recording") %>%
          # children of each recording
          # names vary, can't do directly
          xml2::xml_children(x = .) %>%
          xml2::xml_attrs(x = .) %>%
          # Convert each item in list to data frame,
          lapply(FUN = as.data.frame.list,
                 stringsAsFactors = FALSE) %>%
          # rowbind dataframes to data.table
          # fill = TRUE for unequal # of columns
          rbindlist(fill = TRUE)

        ### Add Subject ID to dataframe as first column
        blocks.DF <- cbind(subjID, blocks.DF)

        # Remove characters from columns with time data

        # names of columns with time
        timeCols <- c("femaleAdultUttLen", "maleAdultUttLen",
                      "femaleAdultNonSpeechLen", "maleAdultNonSpeechLen",
                      "childUttLen", "childCryVfxLen", "TVF", "FAN",
                      "OLN", "SIL", "NOF", "CXF", "OLF", "CHF", "MAF",
                      "TVN", "NON", "CXN", "CHN", "MAN", "FAF",
                      "startTime", "endTime")

        blocks.DF[, (timeCols) :=
                    lapply(.SD, dropChars),
                  .SDcols = timeCols]

        # convert to numeric or character as appropriate
        blocks.DF[] <-
          lapply(blocks.DF, type.convert, as.is = TRUE)

        ### Adding clock & secMidnight times ###

        # pull datetime info from recordings.DF
        blocks.DF <-
          blocks.DF[
            recordings.DF[, .(dateMidnight_epoch,
                              startclocklocal_secMidnight,
                              recId,
                              recordingStart = startTime)],
            on = "recId"]

        # add secmidnight to start and end times
        # to make seconds from midnight start end times
        blocks.DF[, ":="
                  (# add time.zone to keep track
                    timezone = time.zone,
                    startTimeSecMidnight =
                      startTime %>%
                      add(startclocklocal_secMidnight) %>%
                      subtract(e2 = recordingStart),
                    endTimeSecMidnight =
                      endTime %>%
                      add(startclocklocal_secMidnight) %>%
                      subtract(e2 = recordingStart))]

        # add local clocktimes
        blocks.DF[, ':='
                  (startclocklocal =
                      add(startTimeSecMidnight + dateMidnight_epoch) %>%
                      as.POSIXct(tz = time.zone, origin = "1970-1-1") %>%
                      as.character.POSIXt,
                    endclocklocal =
                      add(endTimeSecMidnight + dateMidnight_epoch) %>%
                      as.POSIXct(tz = time.zone, origin = "1970-1-1") %>%
                      as.character.POSIXt)]


        # Amount of time passed in recording
        # useful for aligning with audio
        # Note, if time ends in 0 (eg 02:25:60),
        # Excel will format as numeric. Data is correct in DF and CSV.
        blocks.DF[, recClockEnd :=
                    endTime %>%
                    lubridate::seconds_to_period() %>%
                    {sprintf('%02d:%02d:%05.2f',
                             lubridate::hour(x = .),
                             lubridate::minute(x = .),
                             lubridate::second(x = .)
                    )}]

        ### Blocks Script Validation ###

        # block times line up end-to-start

        timesMatch.blks <- vector(length = nrow(blocks.DF))

        for (i in 1:(nrow(blocks.DF) - 1)) {
          timesMatch.blks[i] <-
            # Does the end of one segment equal the start of the next?
            blocks.DF$endTimeSecMidnight[i] == blocks.DF$startTimeSecMidnight[i + 1]
        }
        rm(i)

        # This should equal #rows - #recordings
        validation[,
                   "timesMatchEndToEnd.blks" :=
                     (sum(timesMatch.blks) == nrow(blocks.DF) -
                        max(blocks.DF$recId))]

        rm(timesMatch.blks)

        # Are there 53 columns in this dataframe?
        validation[, "blockCols53" := ncol(blocks.DF) == 53]
        # does the number of rows match the number of blocks?
        validation[, "nrowsMaxBlkId" :=
                     nrow(blocks.DF) == blocks.DF[, max(blkId)]]

        processing.completed[, ":="
                             ("blocks.processed" =
                                 TRUE)]

        ### Write CSV of Block Sections ###

        fwrite(x = blocks.DF,
               file = paste0(CSV.dir, "blocks/", subjID, "_ITS_CP_Blocks.csv"))

        processing.completed[, "blocks.written" :=
                               TRUE]

        rm(blocks.DF)

        # end if no further desired
        if (!write.segments &
            !write.centiseconds &
            !write.seconds) {

          completeFile()

          next
        }

      }

      ###### Segments ######

      # pre-populate segments validation
      validation[, ":="
                 (timesMatchEndToEnd.segs = NA,
                   segCols37p3ChildX4 = NA,
                   segNrowMaxSegId = NA)]

      # set to NA for "started but not finished"
      processing.completed[,
                           segments.processed := NA]

      segments.DT <-
        its_xml %>%
        # find all segments in xml ITS.file
        xml2::xml_find_all(xpath = ".//Segment") %>%
        # extract all attributes
        xml2::xml_attrs() %>%
        # Convert each item in list to data frame, then rowbind dataframes
        # (with bind_rows for unequal rows)
        # Takes a little longer - maybe 15 seconds
        # for 18K list on 16GB RAM computer
        lapply(FUN = as.data.frame.list,
               stringsAsFactors = FALSE) %>%
        # rowbind dataframes to data.table
        # fill = TRUE for unequal # of columns
        data.table::rbindlist(fill = TRUE)

      # Add Subject ID to dataframe as first column
      segments.DT <- cbind(subjID, segments.DT)

      # Remove characters from columns with time data & convert to numeric
      # EG startTime="PT17239.26S" --> 17239.26 &
      # startClockTime="2017-03-28T14:06:55Z" --> 2017-03-2814:06:55
      timeCols <-
        names(segments.DT[ , grepl("start", names(segments.DT)) |
                             grepl("end", names(segments.DT)) |
                             grepl("Len", names(segments.DT)), with = FALSE])

      segments.DT[ ,
                   (timeCols) :=
                     lapply(.SD, FUN = dropChars),
                   .SDcols = timeCols]

      # split conversationInfo column into components
      # Notes & colnames from TJ MAHR: https://github.com/HomeBankCode/rlena
      convSplitCols <-
        c("convStatus",        # (BC) begin, (RC) running, (EC) end of block
          "convCount",         # Running Block Count for entire ‘.its’ file
          "convTurnCount",     # Running Turn Count for recording
          "convResponseCount", # Turn count within block
          "convType",          # codes for initiator and participants
          "convTurnType",      # turn type (TIFI/TIMI/TIFR/TIMR/TIFE/TIME/NT)
          "convFloorType")     # (FI) : Floor Initiation, (FH) : Floor Holding

      segments.DT[, (convSplitCols) :=
                    tstrsplit(conversationInfo, "|",
                              fixed=TRUE,
                              type.convert = TRUE,
                              keep = c(2:8))]

      # convert columns in segments.DT to numbers or characters
      segments.DT[] <-
        lapply(segments.DT, type.convert, as.is = TRUE)

      ### Adding clock & secMidnight times ###

      # pull datetime info from recordings.DF
      segments.DT <-
        segments.DT[
          recordings.DF[, .(dateMidnight_epoch,
                            startclocklocal_secMidnight,
                            recId,
                            recordingStart = startTime)],
          on = "recId"]

      # add secmidnight to start and end times
      # to make seconds from midnight start end times
      # subtract recording start time to adjust for pause
      segments.DT[, ":="
                  (# add time.zone to keep track
                    timezone = time.zone,
                    startTimeSecMidnight =
                      startTime %>%
                      add(startclocklocal_secMidnight) %>%
                      subtract(e2 = recordingStart),
                    endTimeSecMidnight =
                      endTime %>%
                      add(startclocklocal_secMidnight) %>%
                      subtract(e2 = recordingStart))]


      # Child Utt, Cry, & Vfx columns to secMidnight
      # must be flexible - different number per ITS
      childAttrs <- c("Utt", "Cry", "Vfx")

      nChildCols <- 0

      for (att in childAttrs) {
        # Index all column names of all utterances
        # Number of columns differ by the file, so requires flexible coding
        childCols <- grep(x = colnames(segments.DT),
                          pattern = paste0(att, "[[:digit:]]"))

        # if child voice is found in file
        if (length(childCols) > 0) {

          # Maximum number of indexed columns
          maxAttrNum <-
            names(segments.DT)[childCols] %>%
            stringi::stri_extract_all_charclass(
              ., "[[:digit:]]") %>%
            as.numeric %>%
            max

          nChildCols <- nChildCols + maxAttrNum

          for (i in 1:maxAttrNum) {
            # extract times, subtract beginning time
            attColStart <- paste0("start", att, i)

            segments.DT[, attColSMstart :=
                          lapply(X = .SD, FUN = add, startTimeSecMidnight) %>%
                          lapply(X = ., FUN = subtract, e2 = startTime),
                        .SDcols = attColStart]

            attColEnd <- paste0("end", att, i)


            segments.DT[, attColSMend :=
                          lapply(X = .SD, FUN = add, startTimeSecMidnight) %>%
                          lapply(X = ., FUN = subtract, e2 = startTime),
                        .SDcols = attColEnd]

            setnames(segments.DT,
                     old = c("attColSMstart", "attColSMend"),
                     new = c(paste0("startSecMidnight", att, i),
                             paste0("endSecMidnight", att, i)))
          }

          rm(i, maxAttrNum, attColStart, attColEnd)
        }
      }

      ## Clocktimes ##

      # add local clocktimes
      segments.DT[, ':='
                  (startclocklocal =
                      add(startTimeSecMidnight + dateMidnight_epoch) %>%
                      as.POSIXct(tz = time.zone, origin = "1970-1-1")%>%
                      as.character.POSIXt,
                    endclocklocal =
                      add(endTimeSecMidnight + dateMidnight_epoch) %>%
                      as.POSIXct(tz = time.zone, origin = "1970-1-1")%>%
                      as.character.POSIXt)]


      # Amount of time passed in recording
      # useful for aligning with audio
      # Note, if time ends in 0 (eg 02:25:60),
      # Excel will format as numeric. Data is correct in DF and CSV.
      segments.DT[, recClockEnd :=
                    endTime %>%
                    lubridate::seconds_to_period() %>%
                    {sprintf('%02d:%02d:%05.2f',
                             lubridate::hour(x = .),
                             lubridate::minute(x = .),
                             lubridate::second(x = .)
                    )}]

      ### Odd recording warnings ###

      # Does the recorder have >75% time silence/white noise?
      ITS.checks[, "pctSilenceAndNoise" :=
                   # time in silence or noise
                   round(segments.DT[spkr %in% c("SIL", "NON", "NOF"),
                               sum(endTime - startTime)] /
                   # total recording time
                   segments.DT[, sum(endTime - startTime)], 2)]

      # column names that can exist
      # any missing = odd file
      allSegColNames <-
        c('subjID', 'spkr', 'average_dB', 'peak_dB',
          'recordingInfo', 'startTime', 'endTime',
          'recId', 'blkId', 'blkTypeId', 'segId',
          'blkType', 'conversationInfo', 'femaleAdultWordCnt',
          'femaleAdultNonSpeechLen', 'femaleAdultUttCnt',
          'femaleAdultUttLen', 'maleAdultWordCnt',
          'maleAdultNonSpeechLen', 'maleAdultUttCnt',
          'maleAdultUttLen', 'childUttCnt', 'childUttLen',
          'childCryVfxLen', 'startUtt1', 'endUtt1', 'startVfx1',
          'endVfx1', 'startCry1', 'endCry1', 'convStatus',
          'convCount', 'convTurnCount', 'convResponseCount',
          'convType', 'convTurnType', 'convFloorType',
          'dateMidnight_epoch', 'startclocklocal_secMidnight',
          'recordingStart', 'startTimeSecMidnight',
          'endTimeSecMidnight', 'startclocklocal',
          'endclocklocal', 'recClockEnd', 'startSecMidnightUtt1',
          'endSecMidnightUtt1', 'startSecMidnightCry1',
          'endSecMidnightCry1', 'startSecMidnightVfx1',
          'endSecMidnightVfx1')

      ## does segments.DT contain all possible columns? ##
      ITS.checks[,
                 "allColumnsPresentInSegments" :=
                   all(allSegColNames %in% colnames(segments.DT))]


      # list of any column names missing
      missingCols <-
        allSegColNames[!allSegColNames %in%
                         colnames(segments.DT)]

      # warning message with next steps
      if (ITS.checks[, allColumnsPresentInSegments != TRUE]) {
        message(
  subjID, " segments does not contain all possible columns.
  Filling missing columns with 0.")

        # create columns with 0
        segments.DT[, c(missingCols) := 0]

        # list missing columns in ITS.checks
        ITS.checks[, "missingSegmentColumns" :=
                     toString(missingCols)]

      }

      ## does file have all possible speakers?  ##

      # Taken from LENA handbook
      speakers <- c("MAN", "MAF", "FAN", "FAF", "CHN",
                    "CHF", "CXN", "CXF", "NON", "NOF",
                    "OLN", "OLF", "TVN", "TVF", "SIL")

      # speakers actually present in file
      speakersInSegs <-
        segments.DT[, unique(spkr)]

      # all present?
      ITS.checks[,
                 "allSpeakersPresent" :=
                   all(speakers %in% speakersInSegs)]

      # if any missing speakers
      # message with list of missing
      if (ITS.checks[, allSpeakersPresent != TRUE]) {

        missingSpkrs <-
          speakers[!speakers %in% speakersInSegs]

        message(
        subjID, " file missing speakers:
                ", #extra line for easy reading in console
        paste(missingSpkrs, collapse = ", "))

        # include list of speakers missing
        ITS.checks[,
                   "missingSpeakers" :=
                     paste(missingSpkrs, collapse = ", ")]
      }


      ### Segments Script Validation ###

      # check that segment times match end-to-start
      timesMatch.segs <- vector(length = nrow(segments.DT))

      for (i in 1:(nrow(segments.DT) - 1)) {
        timesMatch.segs[i] <-
          # Does the end of one segment equal the start of the next?
          segments.DT$endTimeSecMidnight[i] ==
          segments.DT$startTimeSecMidnight[i + 1]
      }
      rm(i)

      # This should equal #rows - #recordings
      validation[, "timesMatchEndToEnd.segs" :=
                   sum(timesMatch.segs) == nrow(segments.DT) -
                   max(segments.DT$recId)]

      # are there 37 + (4*nChildCol + 3) columns?
      validation[, "segCols37p3ChildX4" :=
                   ncol(segments.DT) ==
                   (37 + length(missingCols) +
                      ifelse(test = nChildCols > 0,
                             (4 * nChildCols) + 3,
                             0))]

      # number of rows matches number of segments
      validation[, "segNrowMaxSegId" :=
                   nrow(segments.DT) == segments.DT[, max(segId)]]

      processing.completed[, ":="
                           ("segments.processed" =
                               TRUE)]

      ### Write csv of Segments ###
      if (write.segments) {
        fwrite(x = segments.DT,
               file = paste0(CSV.dir, "segments/", subjID, "_ITS_Segments.csv"))

        # update after write
        processing.completed[, "segments.written" :=
                               TRUE]

      }

      # cleanup
      rm(
        allSegColNames,
        att,
        charCols,
        childAttrs,
        childCols,
        convSplitCols,
        its_xml,
        missingCols,
        timeCols,
        timesMatch.segs
      )

      # end if no downstream desired

      if (!write.centiseconds & !write.seconds) {
        completeFile()
        next
      }

      ##### Centiseconds #####

      # pre-populate centiseconds validation

      validation[, ":="
                 (csecRecOnMatchSegments = NA,
                  csecRecTimeStartAtZero = NA,
                  csecRecTimeLastMatchSegs = NA,
                  femAdultWordsSum = NA,
                  malAdultWordsSum = NA,
                  allAdultWordsSum = NA,
                  fNonSpeechMatch = NA,
                  mNonSpeechMatch = NA,
                  childUttCntSumMatch = NA,
                  childCryVfxSegsMatch = NA,
                  convTurnMatchSegs = NA,
                  csecRows36HrMin = NA,
                  csec60Cols = NA)]

      # set to NA for "started but not finished"
      processing.completed[,
                           centiseconds.processed := NA]

      # ERROR IN WINDOWS/IEEE 754!!!
      # Must round or there is an error
      # with floating decimal point that
      # causes the later match function to
      # miss thousands of rows!
      segments.DT[, ":="
                  (csecMidnightStart =
                      startTimeSecMidnight %>%
                      multiply_by(100) %>%
                      # correct for weird windows rounding error
                      round,
                    csecMidnightEnd =
                      endTimeSecMidnight %>%
                      multiply_by(100) %>%
                      # correct for weird windows rounding error
                      round)]

      segments.DT[, "csecSegDur" :=
                    csecMidnightEnd - csecMidnightStart]

      ### Create base dataframe - centiseconds in a day ###

      centiseconds.DT <-
        data.table(
          # add subject ID as 1st column
          "subjID" = subjID,
          # Csec segFile only 1 day long if total is less than 1 day,
          # or maximum amount of day if longer than 1 day
          csecMidnight =
            # sequence from 0 (same as clock)
            seq(from = 0,
                # to 36 hours or end of DF (whichever larger)
                to =
                  max(segments.DT$csecMidnightEnd, 12959999),
                by = 1))

      # set csecMidnight as they key column for centiseconds.DT
      # speeds computation time of all functions
      setkey(centiseconds.DT, csecMidnight)

      # add day in seconds
      centiseconds.DT[, ("DayInSeconds") :=
                        (csecMidnight / 100) %>%
                        # round to lowest second
                        floor]

      # add local time without date
      centiseconds.DT[, ("clockTime") :=
                        (csecMidnight / 100) %>%
                        lubridate::seconds_to_period() %>%
                        {sprintf('%02d:%02d:%05.2f',
                                 (lubridate::hour(x = .)+
                                    (lubridate::day(x = .) * 24)),
                                 lubridate::minute(x = .),
                                 lubridate::second(x = .)
                        )}]


      ### Recorder On Times ###

      # Output: List of centiseconds of the day
      # during which the recorder was on
      # include segId for index matching later

      # more consistent behavior if 1 is subtracted
      # from end BEFORE pmap_dfr
      segments.DT[, "csecMidnightEnd.1" := csecMidnightEnd - 1]

      recOn.DT <-
        # rowbind each data.table created by the function
        # below to a new, massive data.table
        purrr::pmap_dfr(
          .l =
            list(
              # NOTE: This function ONLY works with $ indexing
              # something about data.table messes up seq in pmap_dfr
              s = segments.DT$csecMidnightStart,
              e = segments.DT$csecMidnightEnd.1,
              segID = segments.DT$segId),
          .f = function(s=s, e=e, segID = segID) {
            # for each row in segments.DT, create a new data.table
            # time column: sequence from start to end-1
            # number of rows in each new data.table = (e-1-s)
            data.table(csecMidnight = seq(from = s, to = e) ,
                       segId = segID)
          }) %>%
        setDT(x = ., key = "csecMidnight")

      # sets Recorder On to 1
      recOn.DT[, ":=" (
        "csecMidnight" = as.integer(csecMidnight),
        "recOn" = 1)]

      # match centiseconds.DT to recOn by csecMidnight
      # this adds a column to csecMidnight for
      # recOn = 1
      centiseconds.DT <-
        merge.data.table(centiseconds.DT,
                         recOn.DT,
                         on = "csecMidnight",
                         all = TRUE)

      # if recOn is NA (was not in recOn datatable)
      # make recOn 0
      centiseconds.DT[is.na(recOn), recOn := 0]

      rm(recOn.DT)

      ### Add recorder time count from 0:End ###

      # If the recorder was on, count from 1:recorder Off, else, NA
      centiseconds.DT[recOn == 1,
                      recTime :=
                        cumsum(recOn) - 1]

      # recording clocktime
      # Amount of time passed in recording
      # (useful for lining up ITS info with audio)
      # Note, if time ends in 0 (eg 02:25:60),
      # Excel will format as numeric. Data is correct in DT and CSV.
      # data.table interferes with lubridate -
      # must specify lubridate::function
      centiseconds.DT[recOn == 1,
                      recClock :=
                        (recTime/100) %>%
                        lubridate::seconds_to_period() %>%
                        {sprintf('%02d:%02d:%05.2f',
                                 lubridate::hour(x = .),
                                 lubridate::minute(x = .),
                                 lubridate::second(x = .)
                        )}]

      ### segsToCsec MOST columns ###

      # columns to set to 0 when recorder on:

      speechCols <- c("maleAdultWordCnt", "maleAdultNonSpeechLen",
                      "femaleAdultWordCnt", "femaleAdultNonSpeechLen",
                      "childUttLen")

      # for each column, set to 0 where na
      for (speechCol in speechCols) {
        setnames(segments.DT, old = speechCol, new = "speechCol")
        segments.DT[is.na(speechCol), speechCol := 0]
        setnames(segments.DT, old = "speechCol", new = speechCol)
      }

      count.DT <- segments.DT[,
                              .(
                                # info about ITS-level chunks
                                recId, recordingInfo, blkId,
                                blkTypeId, blkType, segId,

                                "segAvgdB" = average_dB,
                                "segPeakdB" = peak_dB,

                                # word & utterance counts
                                "adultWordCnt" =
                                  (femaleAdultWordCnt + maleAdultWordCnt) /
                                  csecSegDur,
                                "femaleAdultWordCnt" =
                                  (femaleAdultWordCnt / csecSegDur),
                                "maleAdultWordCnt" =
                                  (maleAdultWordCnt / csecSegDur),

                                # adult speech centiseconds
                                "femaleAdultSpeech" =
                                  ifelse(femaleAdultWordCnt >0, 1, 0),
                                "maleAdultSpeech" =
                                  ifelse(maleAdultWordCnt >0, 1, 0),

                                # adult nonspeech centiseconds
                                "femaleAdultNonSpeech" =
                                  ifelse(femaleAdultNonSpeechLen >0, 1, 0),
                                "maleAdultNonSpeech" =
                                  ifelse(maleAdultNonSpeechLen >0, 1, 0),

                                # conversation info
                                conversationInfo,
                                convStatus, convCount,
                                convType, convTurnType, convFloorType,

                                # datetime info
                                dateMidnight_epoch, timezone,
                                startclocklocal_secMidnight,
                                recordingStart,

                                # speakers
                                spkr,

                                # child utterance length
                                "childUttLen" =
                                  (round(childUttLen * 100) / csecSegDur))]



      # match to centiseconds.DT on segId

      centiseconds.DT <-
        # bind centiseconds.DT and csecPresentWords
        merge.data.table(centiseconds.DT,
                         count.DT,
                         by = "segId",
                         # keep all of both dataframes, whether or not they match (irrelevant here)
                         all = TRUE)



      rm(count.DT, speechCol, speechCols)


      ### Child Utterance Columns ###
      # Most complicated - multiples
      # startUtt# & endUtt# --> childUtt yes/no

      # Child Utt, Cry, & Vfx columns to secMidnight
      # must be flexible - different number per ITS
      childAttrs <- c("Utt", "Cry", "Vfx")

      childCols <- c("segId")

      for (att in childAttrs) {
        # Index all column names of all utterances
        # Number of columns differ by the file, so requires flexible coding

        childColsTmp <- grep(x = colnames(segments.DT),
                             pattern = paste0("SecMidnight", att)) %>%
          colnames(segments.DT)[.]

        childCols <- c(childCols, childColsTmp)

      }

      childAttr.DT <-
        segments.DT[, ..childCols]

      childAttr.DT.long <-
        melt(childAttr.DT,
             measure=patterns("startSecMidnightUtt", "endSecMidnightUtt",
                              "startSecMidnightCry", "endSecMidnightCry",
                              "startSecMidnightVfx", "endSecMidnightVfx"),
             value.name =
               c("startSecMidnight.childUtt", "endSecMidnight.childUtt",
                 "startSecMidnight.childCry", "endSecMidnight.childCry",
                 "startSecMidnight.childVfx", "endSecMidnight.childVfx"),
             variable.name = "attNumInSeg")  %>%

        reshape(.,
                varying = c("startSecMidnight.childUtt",
                            "endSecMidnight.childUtt",
                            "startSecMidnight.childCry",
                            "endSecMidnight.childCry",
                            "startSecMidnight.childVfx",
                            "endSecMidnight.childVfx"),
                # no separator between column name and time
                sep = ".",
                # times available
                times = c("childUtt", "childCry", "childVfx"),
                timevar = "att",
                direction = "long")

      # to account for missing seg cols
      # if END second is 0, set to NA
      childAttr.DT.long[endSecMidnight == 0, endSecMidnight := NA]

      # drop empty rows (MANY)
      childAttr.DT.long <- na.omit(childAttr.DT.long)

      childAttr.DT.long[, ":=" (
        csecMidnightStart = (startSecMidnight * 100) %>%
          round(),
        csecMidnightEnd = (endSecMidnight * 100) %>%
          round() %>%
          # MUCH more consistent to subract NOW instead if
          # in pmap_dfr
          subtract(1))]

      # recalculate child utterances
      # child utterance = start centisecond
      childUtterances <-
        childAttr.DT.long[att == "childUtt",
                          .("csecMidnight" =
                              as.integer(csecMidnightStart),
                            "childUttCnt" = 1)]

      centiseconds.DT <-
        merge.data.table(centiseconds.DT,
                         childUtterances,
                         by = "csecMidnight",
                         all = TRUE)

      centiseconds.DT[recOn == 1 & is.na(childUttCnt),
                      childUttCnt := 0]



      # centiseconds containing utterance, cry, non-speech vocalization
      if (nrow(childAttr.DT.long) > 0) {

        childCsec.DT <-
          # rowbind each data.table created by the function
          # below to a new, massive data.table
          purrr::pmap_dfr(
            .l =
              list(
                seg = childAttr.DT.long$segId,
                # NOTE: This function ONLY works with $ indexing
                # something about data.table messes up seq in pmap_dfr
                s = childAttr.DT.long$csecMidnightStart,
                e = childAttr.DT.long$csecMidnightEnd,
                attrs = childAttr.DT.long$att),
            .f = function(s=s, e=e, seg = seg, attrs = attrs) {
              # for each row in segments.DT, create a new data.table
              # time column: sequence from start to end-1
              # number of rows in each new data.table = (e-1-s)
              data.table(csecMidnight =
                           seq(from = s, to = e,
                               by = 1),
                         segId = seg,
                         att = attrs)
            }) %>%
          setDT(x = ., key = "csecMidnight")

        # must convert csecMidnight to integer for proper binding
        # CANNOT be done in pmap - loses "e-1" and causes overlap???
        childCsec.DT[, ":=" (
          "csecMidnight" = as.integer(csecMidnight),
          # if attribute is in centisecond, place 1
          "attExists" = as.integer(1))]

        # cast wide so 1 col per attribute
        # 1 row per centisecond
        # 1 if present, set 0 if NA
        childUttCryVfx.DT <-
          childCsec.DT %>%
          dcast.data.table(data = .,
                           formula = csecMidnight ~ att,

                           value.var = "attExists",
                           fill = 0)

        # merge to centiseconds DT
        centiseconds.DT <-
          merge.data.table(
            centiseconds.DT,
            childUttCryVfx.DT,
            by = "csecMidnight",
            all = TRUE)

        rm(childCsec.DT, childUttCryVfx.DT)

      }

      # creates columns if they don't exist
      # does nothing if they do
      # must specify integer - default is logical
      centiseconds.DT[recOn == 0,
                      ":=" (childUtt = as.integer(NA),
                            childCry = as.integer(NA),
                            childVfx = as.integer(NA))]

      # where recorder is on and childUtt/Cry/Vfx
      # are NA, set to 0
      centiseconds.DT[recOn == 1 & is.na(childUtt),
                      childUtt := 0]

      centiseconds.DT[recOn == 1 & is.na(childCry),
                      childCry := 0]

      centiseconds.DT[recOn == 1 & is.na(childVfx),
                      childVfx := 0]


      rm(childAttr.DT,
         childUtterances, att, childAttrs,
         childCols, childColsTmp)

      ### Conversational Turns ###

      # conversational turn count is based on
      # when response started

      conversationalTurns.DT <-
        segments.DT[convTurnType == "TIFR" | convTurnType == "TIMR",
                    .(csecMidnight = csecMidnightStart,
                      convTurnCount = 1)]

      centiseconds.DT <-
        merge.data.table(
          centiseconds.DT,
          conversationalTurns.DT,
          by = "csecMidnight",
          all = TRUE)

      rm(conversationalTurns.DT)

      centiseconds.DT[recOn == 1 & is.na(convTurnCount),
                      convTurnCount := 0]

      # conversational turn initiations
      centiseconds.DT[convTurnType == "TIFI" | convTurnType == "TIMI",
                      convTurnInitiate := 1]
      centiseconds.DT[recOn == 1 & is.na(convTurnInitiate),
                      convTurnInitiate := 0]

      # conversational turn responses
      centiseconds.DT[convTurnType == "TIFR" | convTurnType == "TIMR",
                      convTurnRespond := 1]
      centiseconds.DT[recOn == 1 & is.na(convTurnRespond),
                      convTurnRespond := 0]

      # conversational turn extend
      centiseconds.DT[convTurnType == "TIFE" | convTurnType == "TIME",
                      convTurnExtend := 1]
      centiseconds.DT[recOn == 1 & is.na(convTurnExtend),
                      convTurnExtend := 0]

      ### Spread Speakers to Individual Columns ###

      speakers.DT <- centiseconds.DT[recOn == 1]

      # create new columns for present speakers
      # where speaker is spkr, 1, else 0
      csecSpeakersDT <-
        dcast.data.table(data = speakers.DT,
                         formula = csecMidnight ~ spkr,
                         value.var = "recOn",
                         fill = 0)

      csecSpeakersDT[, ":="
                     # ensure csecMidnight is integer for binding
                     ("csecMidnight" = as.integer(csecMidnight))]

      # fill missing speakers columns with 0
      if (ITS.checks[, allSpeakersPresent != TRUE]) {

        csecSpeakersDT[, c(missingSpkrs) := 0]

        rm(missingSpkrs, speakersInSegs)
      }

      # original order is when they appear in
      # dataset - this standardizes to LENA manual
      setcolorder(csecSpeakersDT,
                  neworder = c("csecMidnight", speakers))

      setkey(csecSpeakersDT, csecMidnight)

      centiseconds.DT <-
        # bind centiseconds.DT and csecSpeakersDT
        merge.data.table(centiseconds.DT,
                         csecSpeakersDT,
                         by = "csecMidnight",
                         # keep all of both dataframes,
                         # whether or not they match
                         all = TRUE)

      rm(csecSpeakersDT)

      ### Add OFF Column ###

      # add column for OFF
      centiseconds.DT[, "OFF" := 0]

      # if recorder is off, OFF = 1
      centiseconds.DT[recOn == 0, OFF := 1]

      # adding "OFF" as spkr for recOn == 0
      centiseconds.DT[recOn == 0, spkr := "OFF"]

      # cleanup
      rm(speakers.DT, speakers)

      ### DateTime Additions ###

      # add epoch date (to keep hold of it)
      centiseconds.DT[,
                      "dateMidnight_epoch" :=
                        segments.DT[1, dateMidnight_epoch]]


      # secMidnightDate column: Seconds from epoch start
      centiseconds.DT[,("secMidnightDate") :=
                        DayInSeconds + dateMidnight_epoch]

      # Datetime in local time (time.zone)
      centiseconds.DT[, ("dateTime_UTC") :=
                        as.POSIXct(secMidnightDate,
                                   origin = "1970-1-1",
                                   tz = time.zone)]

      # set centiseconds.DT column order to match
      # seconds.DT
      setcolorder(
        x = centiseconds.DT,
        neworder =
          c("subjID", "csecMidnight", "DayInSeconds",
            "clockTime", "recTime", "recClock", "recOn",
            "recId", "recordingInfo", "blkId", "blkTypeId",
            "blkType", "segId", "segAvgdB", "segPeakdB",
            "adultWordCnt", "femaleAdultWordCnt",
            "maleAdultWordCnt", "femaleAdultSpeech",
            "maleAdultSpeech", "femaleAdultNonSpeech",
            "maleAdultNonSpeech", "childUttCnt",
            "childUttLen", "childUtt", "childCry",
            "childVfx", "convTurnCount", "convTurnInitiate",
            "convTurnRespond", "convTurnExtend",
            "conversationInfo", "convStatus", "convCount",
            "convType", "convTurnType", "convFloorType",
            "spkr",
            "MAN", "MAF", "FAN", "FAF", "CHN", "CHF", "CXN",
            "CXF", "NON", "NOF", "OLN", "OLF", "TVN", "TVF",
            "SIL", "OFF", "startclocklocal_secMidnight",
            "timezone", "recordingStart", "secMidnightDate",
            "dateTime_UTC", "dateMidnight_epoch"))


      ### validation segs to csec script ###

      validation[, ":=" (
        # duration of all segments equal
        # rows recorder on?
        "csecRecOnMatchSegments" =

          all.equal(segments.DT[,sum(csecSegDur)],
                    centiseconds.DT[, sum(recOn)]),

        # first recorder on time should be 0
        "csecRecTimeStartAtZero" =
          centiseconds.DT[recOn == 1, first(recTime)] ==
          0,

        # last row of segs match
        # last recorder on time in csec
        "csecRecTimeLastMatchSegs" =
          # last csec recorder-on time match...
          centiseconds.DT[recOn == 1, last(recTime)]  ==
          # last end time of segments in csec
          segments.DT[,
                      round(last(endTime * 100))-1],

        # total female adult words match
        "femAdultWordsSum" =
          all.equal(
            sum(segments.DT$femaleAdultWordCnt, na.rm = TRUE),
            sum(centiseconds.DT$femaleAdultWordCnt, na.rm = TRUE)),

        # total male adult words match
        "malAdultWordsSum" =
          all.equal(
            sum(segments.DT$maleAdultWordCnt, na.rm = TRUE),
            sum(centiseconds.DT$maleAdultWordCnt, na.rm = TRUE)),

        # total adult words match
        "allAdultWordsSum" =
          all.equal(
            centiseconds.DT[, sum(adultWordCnt,
                                  na.rm = TRUE)],
            segments.DT[, sum(maleAdultWordCnt,
                              femaleAdultWordCnt,
                              na.rm = TRUE)]),

        # female nonspeech centiseconds sum == segments?
        "fNonSpeechMatch" =
          centiseconds.DT[,
                          sum(femaleAdultNonSpeech,
                              na.rm = TRUE)] ==
          segments.DT[, round(femaleAdultNonSpeechLen * 100) %>%
                        sum(na.rm = TRUE)],

        # male nonspeech centiseconds sum == segments?
        "mNonSpeechMatch" =
          centiseconds.DT[,
                          sum(maleAdultNonSpeech,
                              na.rm = TRUE)] ==
          segments.DT[,
                      round(maleAdultNonSpeechLen * 100) %>%
                        sum(na.rm = TRUE)],
        # total child utterances match
        "childUttCntSumMatch" =
          ifelse(nChildCols == 0, TRUE,
                 childAttr.DT.long[,
                                   .(attSegCount = max(as.numeric(attNumInSeg))),
                                   by = c("segId", "att")][
                                     att == "childUtt",
                                     sum(attSegCount)] ==
                   centiseconds.DT[, sum(childUttCnt, na.rm = TRUE)]),

        # child cry & nonspeech match
        "childCryVfxSegsMatch" =
          segments.DT[, sum(round(childCryVfxLen*100), na.rm = TRUE)] ==
          centiseconds.DT[, sum(childCry, childVfx, na.rm = TRUE)],

        # conversational turns match
        "convTurnMatchSegs" =
          segments.DT[, max(convTurnCount, na.rm = TRUE)] ==
          centiseconds.DT[, sum(convTurnCount, na.rm = TRUE)],

        # num rows = 36 hours or max segs
        "csecRows36HrMin" =
          nrow(centiseconds.DT) >= 12960000,

        # 60 columns
        "csec60Cols" =
          ncol(centiseconds.DT) == 60
      )]

      # set to TRUE after complete
      processing.completed[,
                           centiseconds.processed := TRUE]

      ### Write csv of centiseconds ###
      if (write.centiseconds) {

        fwrite(x = centiseconds.DT,
               file = paste0(CSV.dir, "centiseconds/",
                             subjID, "_csec.csv"),
               nThread = (parallel::detectCores() - 1),
               buffMB = 1024)

        # set to TRUE after written
        processing.completed[,
                             centiseconds.written := TRUE]
      }

      rm(segments.DT, childAttr.DT.long, nChildCols)

      # end if no downstream desired

      if (!write.seconds) {
        completeFile()
        next
      }

      #### Seconds ####

      # pre-populate seconds validation
      validation[, ":="
                 (secRows36HrMin = NA,
                  sec60Cols = NA)]


      # set to NA for "started but not finished"
      processing.completed[,
                           seconds.processed := NA]

      # create seconds data.table
      # **must specify data types for all
      # except integers or data.table
      # has grouping error**

      seconds.DT <-
        centiseconds.DT[,
                        .(
                          #time columns
                          "clockTime" = first(clockTime),
                          "recTime" = as.double(first(recTime)/100),
                          "recClock" = as.character(first(recClock)),
                          "recOn" = (sum(recOn)/100),

                          # ID columns
                          "recId" = stat_mode_narm(recId),
                          "recordingInfo" =
                            stat_mode_narm(recordingInfo),
                          "blkId" = stat_mode_narm(blkId),
                          "blkTypeId" = stat_mode_narm(blkTypeId),
                          "blkType" = as.character(stat_mode_narm(blkType)),
                          "segId" = stat_mode_narm(segId),

                          # decibel info
                          "segAvgdB" = as.double(hablar::mean_(segAvgdB, ignore_na = TRUE)),
                          "segPeakdB" = as.double(hablar::max_(segPeakdB, ignore_na = TRUE)),

                          # word counts
                          "adultWordCnt" = as.double(hablar::sum_(adultWordCnt, ignore_na = TRUE)),
                          "femaleAdultWordCnt" = as.double(hablar::sum_(femaleAdultWordCnt, ignore_na = TRUE)),
                          "maleAdultWordCnt" = as.double(hablar::sum_(maleAdultWordCnt, ignore_na = TRUE)),

                          # adult speech & nonspeech
                          "femaleAdultSpeech" =
                            as.double(
                              hablar::sum_(femaleAdultSpeech, ignore_na = TRUE)/100),
                          "maleAdultSpeech" =
                            as.double(
                              hablar::sum_(maleAdultSpeech, ignore_na = TRUE)/100),
                          "femaleAdultNonSpeech" =
                            as.double(
                              hablar::sum_(femaleAdultNonSpeech, ignore_na = TRUE)/100),
                          "maleAdultNonSpeech" =
                            as.double(
                              hablar::sum_(maleAdultNonSpeech, ignore_na = TRUE)/100),

                          # child speech, cry, vfx
                          # raw utt count
                          "childUttCnt" = as.double(
                            hablar::sum_(childUttCnt, ignore_na = TRUE)),

                          # time
                          # equivalent to ADEX
                          "childUttLen" = as.double(
                            hablar::sum_(childUttLen, ignore_na = TRUE)/100),
                          # closer to raw time from segments
                          "childUtt" = as.double(
                            hablar::sum_(childUtt, ignore_na = TRUE)/100),
                          "childCry" = as.double(
                            hablar::sum_(childCry, ignore_na = TRUE)/100),
                          "childVfx" = as.double(
                            hablar::sum_(childVfx, ignore_na = TRUE)/100),

                          # Conversational Turns
                          # sum_
                          "convTurnCount" = as.double(
                            hablar::sum_(convTurnCount, ignore_na = TRUE)),
                          # seconds in initiate, response, extention
                          "convTurnInitiate" = as.double(
                            hablar::sum_(convTurnInitiate, ignore_na = TRUE)/100),
                          "convTurnRespond" = as.double(
                            hablar::sum_(convTurnRespond, ignore_na = TRUE)/100),
                          "convTurnExtend" = as.double(
                            hablar::sum_(convTurnExtend, ignore_na = TRUE)/100),

                          # conversation info
                          # stat_mode_narm
                          "conversationInfo" = as.character(stat_mode_narm(conversationInfo)),
                          "convStatus" = as.character(stat_mode_narm(convStatus)),
                          "convCount" = stat_mode_narm(convCount),
                          "convType" = as.character(stat_mode_narm(convType)),
                          "convTurnType" = as.character(stat_mode_narm(convTurnType)),
                          "convFloorType" = as.character(stat_mode_narm(convFloorType)),

                          # speakers
                          # speaker.freq
                          "spkr" =
                            speaker.freq(spkr),
                          "MAN" =
                            as.double(
                              hablar::sum_(
                                MAN, ignore_na = TRUE)/100),
                          "MAF" =
                            as.double(
                              hablar::sum_(
                                MAF, ignore_na = TRUE)/100),
                          "FAN" =
                            as.double(
                              hablar::sum_(
                                FAN, ignore_na = TRUE)/100),
                          "FAF" =
                            as.double(
                              hablar::sum_(
                                FAF, ignore_na = TRUE)/100),
                          "CHN" =
                            as.double(
                              hablar::sum_(
                                CHN, ignore_na = TRUE)/100),
                          "CHF" =
                            as.double(
                              hablar::sum_(
                                CHF, ignore_na = TRUE)/100),
                          "CXN" =
                            as.double(
                              hablar::sum_(
                                CXN, ignore_na = TRUE)/100),
                          "CXF" =
                            as.double(
                              hablar::sum_(
                                CXF, ignore_na = TRUE)/100),
                          "NON" =
                            as.double(
                              hablar::sum_(
                                NON, ignore_na = TRUE)/100),
                          "NOF" =
                            as.double(
                              hablar::sum_(
                                NOF, ignore_na = TRUE)/100),
                          "OLN" =
                            as.double(
                              hablar::sum_(
                                OLN, ignore_na = TRUE)/100),
                          "OLF" =
                            as.double(
                              hablar::sum_(
                                OLF, ignore_na = TRUE)/100),
                          "TVN" =
                            as.double(
                              hablar::sum_(
                                TVN, ignore_na = TRUE)/100),
                          "TVF" =
                            as.double(
                              hablar::sum_(
                                TVF, ignore_na = TRUE)/100),
                          "SIL" =
                            as.double(
                              hablar::sum_(
                                SIL, ignore_na = TRUE)/100),
                          "OFF" =
                            as.double(
                              hablar::sum_(
                                OFF, ignore_na = TRUE)/100),

                          # dateTime info
                          startclocklocal_secMidnight =
                            first(startclocklocal_secMidnight),
                          timezone = first(timezone),
                          recordingStart = first(recordingStart),
                          secMidnightDate = first(secMidnightDate),
                          dateTime_UTC = first(dateTime_UTC)

                        ),
                        # do each calculation for each day in seconds
                        by = "DayInSeconds"]

      # if more than 1 speaker in second, flag
      seconds.DT[dplyr::contains(".", vars = spkr), doubleSpkr := 1]
      seconds.DT[is.na(doubleSpkr), doubleSpkr := 0]


      seconds.DT <-
        data.table(subjID = subjID,
                   seconds.DT,
                   dateMidnight_epoch =
                     centiseconds.DT[,
                                     unique(
                                       dateMidnight_epoch)])

      ### Validation: Rows & Columns ###

      validation[, ":=" (secRows36HrMin =
                           nrow(seconds.DT) >= 129600,
                         sec60Cols =
                           ncol(seconds.DT) == 60)]

      # set to TRUE after completed
      processing.completed[,
                           seconds.processed := TRUE]

      ### Write csv of seconds ###

      fwrite(x = seconds.DT,
             file = paste0(CSV.dir, "seconds/",
                           subjID, "_sec.csv"),
             nThread = (parallel::detectCores() - 1),
             buffMB = 1024)

      # set to TRUE after written
      processing.completed[,
                           seconds.written := TRUE]

      completeFile()
      next

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
                            .SDcols =
                              colnames(
                                validation.allfiles[,!"subjID"]),
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

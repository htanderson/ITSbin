#' @title bin_seconds
#'
#' @description This function imports standardized secMidnight files and bins them to specified minutes, starting from midnight.
#' @param seconds.dir Directory (string) containing seconds CSV files.
#' @param output.dir Directory (string) to store binned CSV files
#' @param bin.to.mins Character or numeric. Time interval to bin to (num MINS or "Total")
#' @param cross.recordings Logical. Should bins be calculated across or within recordings? (default = TRUE)
#' @param align.rows Character. opts = c("midnight", "recorder-on") Should the first bin start at midnight (00:00:00) the day the recorder was turned on ("midnight") or the first second in which the recorder was on ("recorder-on")
#' @param roll.by.mins Numeric. Default = NULL (sequential bins, eg 0-59, 60-119) How many minutes between bins (or, do bins overlap?). Ex, if bin.to.mins = 60 & roll.by.mins = 1, then 1st bin = minutes 0-59, 2nd bin = mins 1-60, 3rd = mins 2-61, etc.
#' @param subset.by.col Character. Include only rows where this column is NOT 0 (default = NULL). To include only seconds when the recorder was turned on, set subset.by.col = "recOn" & drop.by.subset = TRUE.
#' @param drop.by.subset Logical. If subset column is specified, should rows where subset.by.col == 0 be removed entirely (TRUE) or set to NA (FALSE)? To include only seconds when the recorder was turned on, set subset.by.col = "recOn" & drop.by.subset = TRUE.
#' @param overwrite.existing Logical. If the target file already exists, should the function overwrite it? If FALSE (default), subject will be skipped if file exists. If TRUE, new file will overwrite existing file.
#' @return One CSV file per input seconds file.
#' @examples
#' bin_seconds(
#' seconds.dir = "Server:/LENAData/secMidnight",
#' output.dir = "Server:/LENAData/min1Midnight",
#' bin.to.mins = 5,
#' align.rows = "midnight")
#' @import data.table
#' @import zoo
#' @import magrittr
#' @import hablar
#' @export

bin_seconds <-
  function(
    seconds.dir,
    output.dir,
    bin.to.mins,
    cross.recordings = TRUE,
    align.rows = c("midnight", "recorder-on"),
    roll.by.mins = NULL,
    subset.by.col = NULL,
    drop.by.subset = NULL,
    overwrite.existing = FALSE){

    # check inputs
    if (!is.null(subset.by.col) &
        !(isTRUE(drop.by.subset) |
          isFALSE(drop.by.subset))) {
      stop("If subset.by.col is specified,
      drop.by.subset must be either
      TRUE (remove rows before binning) or
      FALSE (set rows to NA before binning).")
    }

    ### add forward slash to directory names if missing

    input.dirs <-
      list("seconds.dir" = seconds.dir,
           "output.dir" = output.dir)

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

    # create sub-directory for specific bin
    # dir.create will just throw a warning if the
    # folder already exists - doesn't impact folder
    dir.create(output.dir, showWarnings = FALSE)

    user.inputs.filename <-
      paste0(
        # bin minutes
        bin.to.mins, "mins_",
        # alignment
        align.rows, "_",
        # cross recordings
        "CR", cross.recordings, "_",
        # rolling vs sequential
        ifelse(
          test = !is.null(roll.by.mins),
          yes = paste0("RW", roll.by.mins, "min_"),
          no = "seq_"),
        # subset or full data
        ifelse(
          test = !is.null(subset.by.col),
          yes = paste0("subset_", subset.by.col,
                       ifelse(
                         test = drop.by.subset,
                         yes = "drop",
                         no = "NA")),
          no = "alldata")
      )

    bin.CSV.dir <- paste0(output.dir,
                          user.inputs.filename, "/")
    dir.create(bin.CSV.dir, showWarnings = FALSE)

    bin.CSV.files <-
      dir(bin.CSV.dir, pattern = ".csv")

    # set up "by" per user inputs
    by.Cols <- c(
      # subjID as dummy col
      # also ensures subjID always binds
      "subjID",
      # if want only bins within recordings,
      # subset by recId
      if((align.rows == "recorder-on" |
          bin.to.mins == "total" |
          bin.to.mins == "Total") &
         !cross.recordings) {"recRleid"})

    if (bin.to.mins != "total" |
        bin.to.mins != "Total") {
      binMins <-
        bin.to.mins %>%
        as.numeric() %>%
        multiply_by(60)
    }

    # number to roll by if rolling windows
    if (is.null(roll.by.mins)) {
      rollBy <- binMins
    } else {
      rollBy <-
        roll.by.mins %>%
        as.numeric() %>%
        multiply_by(60)
    }

    # rolling function helper
    rollfun <- function(x, f) {
      zoo::rollapply(data = x,
                     FUN = f,
                     width = binMins,
                     by = rollBy,
                     partial = partial.bins,
                     align = "left")
    }

    # sum function helper
    sum_double <- function(sum_col) {
      as.double(hablar::sum_(sum_col, ignore_na = TRUE))
    }

    partial.bins <- TRUE

    ###### read in file ######
    file.names <- dir(path = seconds.dir, pattern = ".csv")

    for (file.name in file.names) {


      functionStartTime <- Sys.time()

      seconds.DT <-
        data.table::fread(input = paste0(seconds.dir, file.name),
                          sep = ",", # CSV
                          verbose = FALSE,
                          # fread does NOT read empty character strings
                          # as NA by default
                          na.strings = c("", "NA"))

      # pull subjid
      subjID <- seconds.DT[, unique(subjID)]

      # create output filename
      output.filename <- paste0(
        subjID, "_",
        user.inputs.filename,
        ".csv")

      # check if file already exists in target directory
      if (output.filename %in% bin.CSV.files) {
        message(output.filename, " already exists.")

        # if user wants overwrite, message & continue
        if (overwrite.existing) {
          message(output.filename, " will be overwritten.
                  If this is not desired, stop function & specify
                  overwrite.existing = FALSE")
        } else {
          # if not, message & skip
          message(subjID, " will be skipped.
                  If this is not desired, stop function & specify
                  overwrite.existing = TRUE")

          next
        }

      }


      # if subsetting data by a column

      if (!is.null(subset.by.col)) {

        if (drop.by.subset) {
          # cannot select with variable column name
          # must be set to specific name
          setnames(seconds.DT,
                   old = subset.by.col,
                   new = "subset.by.col")

          seconds.DT <-
            seconds.DT[subset.by.col != 0]

          # change name back to original
          setnames(seconds.DT,
                   old = "subset.by.col",
                   new = subset.by.col)
        } else {

          # create backup just in case
          # want to retain name for processing

          infoCols <-
            c("recId", "recOn",
              "recTime", "recClock",
              "segAvgdB", "segPeakdB",
              "adultWordCnt", "femaleAdultWordCnt",
              "maleAdultWordCnt", "femaleAdultSpeech",
              "maleAdultSpeech", "femaleAdultNonSpeech",
              "maleAdultNonSpeech", "childUttCnt",
              "childUttLen", "childUtt",
              "childCry", "childVfx",
              "convTurnCount", "convTurnInitiate",
              "convTurnRespond", "convTurnExtend",
              "MAN", "MAF", "FAN", "FAF", "CHN",
              "CHF", "CXN", "CXF", "NON", "NOF",
              "OLN", "OLF", "TVN", "TVF", "SIL", "OFF",
              "spkr")


          # if subset.by.col is in infoCols
          # remove it so updating works
          infoCols <-
            infoCols[!infoCols %in% subset.by.col]

          # cannot select with variable column name
          # must be set to specific name
          setnames(seconds.DT,
                   old = subset.by.col,
                   new = "subset.by.col")

          # where subset.by.col == 0, set to NA
          seconds.DT[subset.by.col == 0 |
                       is.na(subset.by.col),
                     (infoCols) := NA]

          # change name back to original
          setnames(seconds.DT,
                   old = "subset.by.col",
                   new = subset.by.col)
        }}

      # rleid for keeping track of recorder off chunks separately
      seconds.DT[, ":=" (recRleid = rleid(recId))]

      # if aligning to recorder on
      # drop all rows before 1st second of recorder on

      if (align.rows == "recorder-on") {

        first.on <- seconds.DT[recOn >0, first(DayInSeconds)]

        seconds.DT <-
          seconds.DT[DayInSeconds >=first.on]
      }





      ##### Special use case: Summary file #####

      if (bin.to.mins == "total" | bin.to.mins == "Total") {
        minutes.DT <-
          # limit to recorder on only
          seconds.DT[recOn > 0,
                     .(
                       #time of day columns
                       "DayInSecondsStart" =
                         first(DayInSeconds),

                       "DayInSecondsEnd" =
                         last(DayInSeconds),

                       "DayInSecondsDur" =
                         (last(DayInSeconds) -
                            # +1 because count must be inclusive
                            first(DayInSeconds)) + 1,

                       "clockTimeStart" =
                         first(clockTime),

                       "clockTimeEnd" =
                         last(clockTime),

                       # recording time info
                       "recTimeStart" =
                         first(recTime),

                       "recTimeEnd" =
                         last(recTime),

                       "recClockStart" =
                         first(recClock),

                       "recClockEnd" =
                         last(recClock),

                       "recOnSeconds" =
                         sum(recOn),

                       "recOnPeriod" =
                         recOn %>%
                         sum %>%
                         lubridate::seconds_to_period() %>%
                         {sprintf('%02d:%02d:%05.2f',
                                  lubridate::hour(x = .),
                                  lubridate::minute(x = .),
                                  lubridate::second(x = .)
                         )},
                       "recIdStart" =
                         first(recId),
                       "recIdEnd" =
                         last(recId),

                       # segment sound info
                       "segAvgdB" =
                         mean(segAvgdB),
                       "segPeakdB" =
                         max(segPeakdB),

                       # adult word counts
                       "adultWordCnt" =
                         sum(adultWordCnt),
                       "femaleAdultWordCnt" =
                         sum(femaleAdultWordCnt),
                       "maleAdultWordCnt" =
                         sum(maleAdultWordCnt),

                       # adult speech seconds
                       "femaleAdultSpeechScnds" =
                         sum(femaleAdultSpeech),

                       "maleAdultSpeechScnds" =
                         sum(maleAdultSpeech),

                       # adult nonspeech seconds
                       "femaleAdultNonspeechScnds" =
                         sum(femaleAdultNonSpeech),
                       "maleAdultNonspeechScnds" =
                         sum(maleAdultNonSpeech),

                       # child columns
                       "childUttCnt" =
                         sum(childUttCnt),

                       # ADEX-equivalent child utterance seconds
                       "childUttLenScnds" =
                         sum(childUttLen),

                       # child utterance seconds from ITS directly
                       "childUttScnds" =
                         sum(childUtt),

                       "childCryScnds" =
                         sum(childCry),

                       "childVfxScnds" =
                         sum(childVfx),

                       # conversational turns
                       "convTurnCount" =
                         sum(convTurnCount),

                       "convTurnInitiateScnds" =
                         sum(convTurnInitiate),

                       "convTurnRespondScnds" =
                         sum(convTurnRespond),

                       "convTurnExtendScnds" =
                         sum(convTurnExtend),

                       # speakers
                       "MAN" =
                         sum(MAN),

                       "MAF" =
                         sum(MAF),

                       "FAN" =
                         sum(FAN),

                       "FAF" =
                         sum(FAF),

                       "CHN" =
                         sum(CHN),

                       "CHF" =
                         sum(CHF),

                       "CXN" =
                         sum(CXN),

                       "CXF" =
                         sum(CXF),

                       "NON" =
                         sum(NON),

                       "NOF" =
                         sum(NOF),

                       "OLN" =
                         sum(OLN),

                       "OLF" =
                         sum(OLF),

                       "TVN" =
                         sum(TVN),

                       "TVF" =
                         sum(TVF),

                       "SIL" =
                         sum(MAN),

                       "OFF" =
                         sum(OFF)
                     ),
                     by = by.Cols]
      } else {



        ###### Special use case: align rows to midnight, don't cross recordings ######

        if (align.rows == "midnight" & !cross.recordings) {

          minutes.DT <- data.table()

          emptySeconds.DT <- data.table("DayInSeconds" = seq(0, seconds.DT[.N, DayInSeconds]),
                                        "subjID" = seconds.DT[, unique(subjID)])

          for (i in seconds.DT[, unique(recId)]) {

            # bind empty datatable by DayInSeconds
            recIdSeconds.DT <-
              merge.data.table(x = emptySeconds.DT,
                               y = seconds.DT[recId == i],
                               by = c("DayInSeconds", "subjID"),
                               all.x = TRUE)

            # add column to show DayInSeconds only for current selection
            recIdSeconds.DT[!is.na(clockTime),
                            "DayInSeconds.Selection" := DayInSeconds]

            tempMins.DT <-
              recIdSeconds.DT[,
                              .(#time columns
                                "DayInSecondsStart" =
                                  rollfun(x = DayInSeconds.Selection,
                                          f = function(x)
                                            as.numeric(
                                              first_(x,
                                                     ignore_na = TRUE))),

                                "DayInSecondsEnd" =
                                  rollfun(x = DayInSeconds.Selection,
                                          f = function(x)
                                            as.numeric(
                                              last_(x,
                                                    ignore_na = TRUE))),

                                "DayInSecondsDur" =
                                  rollfun(x = DayInSeconds.Selection,
                                          f = function(x)
                                            n_unique_(x,
                                                      ignore_na = TRUE)),

                                "clockTimeStart" =
                                  rollfun(x = clockTime,
                                          f = function(x)
                                            as.character(
                                              first_(x,
                                                     ignore_na = TRUE))),

                                "clockTimeEnd" =
                                  rollfun(x = clockTime,
                                          f = function(x)
                                            as.character(
                                              last_(x,
                                                    ignore_na = TRUE))),

                                "recTimeStart" =
                                  rollfun(x = recTime,
                                          f = function(x)
                                            as.double(
                                              first_(x,
                                                     ignore_na = TRUE))),

                                "recTimeEnd" =
                                  rollfun(x = recTime,
                                          f = function(x)
                                            as.double(
                                              last_(x,
                                                    ignore_na = TRUE))),

                                "recClockStart" =
                                  rollfun(x = recClock,
                                          f = function(x)
                                            as.character(
                                              first_(x,
                                                     ignore_na = TRUE))),

                                "recClockEnd" =
                                  rollfun(x = recClock,
                                          f = function(x)
                                            as.character(
                                              last_(x,
                                                    ignore_na = TRUE))),

                                "recOnSeconds" =
                                  rollfun(x = recOn,
                                          f = function(x)
                                            sum_(x,
                                                 ignore_na = TRUE)),

                                "recOnPeriod" =
                                  rollfun(x = recOn,
                                          f = function(x) {
                                            sum_(x,
                                                 ignore_na = TRUE) %>%
                                              lubridate::seconds_to_period() %>%
                                              {sprintf('%02d:%02d:%05.2f',
                                                       lubridate::hour(x = .),
                                                       lubridate::minute(x = .),
                                                       lubridate::second(x = .)
                                              )}
                                          }),

                                "recIdStart" =
                                  rollfun(x = recId,
                                          f = function(x)
                                            as.character(
                                              first_(x,
                                                     ignore_na = TRUE))),
                                "recIdEnd" =
                                  rollfun(x = recId,
                                          f = function(x)
                                            as.character(
                                              last_(x,
                                                    ignore_na = TRUE))),

                                # segment sound info
                                "segAvgdB" =
                                  rollfun(x = segAvgdB,
                                          f = function(x)
                                            as.double(
                                              mean_(x,
                                                    ignore_na = TRUE))),
                                "segPeakdB" =
                                  rollfun(x = segPeakdB,
                                          f = function(x)
                                            as.double(
                                              max_(x,
                                                   ignore_na = TRUE))),

                                # adult word counts
                                "adultWordCnt" =
                                  rollfun(x = adultWordCnt,
                                          f = sum_double),
                                "femaleAdultWordCnt" =
                                  rollfun(x = femaleAdultWordCnt,
                                          f = sum_double),
                                "maleAdultWordCnt" =
                                  rollfun(x = maleAdultWordCnt,
                                          f = sum_double),

                                # adult speech seconds
                                "femaleAdultSpeechScnds" =
                                  rollfun(x = femaleAdultSpeech,
                                          f = sum_double),

                                "maleAdultSpeechScnds" =
                                  rollfun(x = maleAdultSpeech,
                                          f = sum_double),

                                # adult nonspeech seconds
                                "femaleAdultNonspeechScnds" =
                                  rollfun(x = femaleAdultNonSpeech,
                                          f = sum_double),
                                "maleAdultNonspeechScnds" =
                                  rollfun(x = maleAdultNonSpeech,
                                          f = sum_double),

                                # child columns
                                "childUttCnt" =
                                  rollfun(x = childUttCnt,
                                          f = sum_double),

                                # ADEX-equivalent child utterance seconds
                                "childUttLenScnds" =
                                  rollfun(x = childUttLen,
                                          f = sum_double),

                                # child utterance seconds from ITS directly
                                "childUttScnds" =
                                  rollfun(x = childUtt,
                                          f = sum_double),

                                "childCryScnds" =
                                  rollfun(x = childCry,
                                          f = sum_double),

                                "childVfxScnds" =
                                  rollfun(x = childVfx,
                                          f = sum_double),

                                # conversational turns
                                "convTurnCount" =
                                  rollfun(x = convTurnCount,
                                          f = sum_double),

                                "convTurnInitiateScnds" =
                                  rollfun(x = convTurnInitiate,
                                          f = sum_double),

                                "convTurnRespondScnds" =
                                  rollfun(x = convTurnRespond,
                                          f = sum_double),

                                "convTurnExtendScnds" =
                                  rollfun(x = convTurnExtend,
                                          f = sum_double),

                                # speakers
                                "MAN" =
                                  rollfun(x = MAN,
                                          f = sum_double),

                                "MAF" =
                                  rollfun(x = MAF,
                                          f = sum_double),

                                "FAN" =
                                  rollfun(x = FAN,
                                          f = sum_double),

                                "FAF" =
                                  rollfun(x = FAF,
                                          f = sum_double),

                                "CHN" =
                                  rollfun(x = CHN,
                                          f = sum_double),

                                "CHF" =
                                  rollfun(x = CHF,
                                          f = sum_double),

                                "CXN" =
                                  rollfun(x = CXN,
                                          f = sum_double),

                                "CXF" =
                                  rollfun(x = CXF,
                                          f = sum_double),

                                "NON" =
                                  rollfun(x = NON,
                                          f = sum_double),

                                "NOF" =
                                  rollfun(x = NOF,
                                          f = sum_double),

                                "OLN" =
                                  rollfun(x = OLN,
                                          f = sum_double),

                                "OLF" =
                                  rollfun(x = OLF,
                                          f = sum_double),

                                "TVN" =
                                  rollfun(x = TVN,
                                          f = sum_double),

                                "TVF" =
                                  rollfun(x = TVF,
                                          f = sum_double),

                                "SIL" =
                                  rollfun(x = MAN,
                                          f = sum_double),

                                "OFF" =
                                  rollfun(x = OFF,
                                          f = sum_double)
                              ),
                              by = by.Cols]

            minutes.DT <-
              .rbind.data.table(
                minutes.DT,
                tempMins.DT[!is.na(clockTimeStart) |
                              !is.na(clockTimeEnd)],
                fill = TRUE
              )



          }

          recIdSeconds.DT <-
            merge.data.table(x = emptySeconds.DT,
                             y = seconds.DT[is.na(recId)],
                             by = c("DayInSeconds", "subjID"),
                             all.x = TRUE)

          recIdSeconds.DT[!is.na(clockTime),
                          "DayInSeconds.Selection" := DayInSeconds]

          tempMins.DT <-
            recIdSeconds.DT[,
                            .(#time columns
                              "DayInSecondsStart" =
                                rollfun(x = DayInSeconds.Selection,
                                        f = function(x)
                                          as.numeric(
                                            first_(x,
                                                   ignore_na = TRUE))),

                              "DayInSecondsEnd" =
                                rollfun(x = DayInSeconds.Selection,
                                        f = function(x)
                                          as.numeric(
                                            last_(x,
                                                  ignore_na = TRUE))),

                              "DayInSecondsDur" =
                                rollfun(x = DayInSeconds.Selection,
                                        f = function(x)
                                          n_unique_(x,
                                                    ignore_na = TRUE)),

                              "clockTimeStart" =
                                rollfun(x = clockTime,
                                        f = function(x)
                                          as.character(
                                            first_(x,
                                                   ignore_na = TRUE))),

                              "clockTimeEnd" =
                                rollfun(x = clockTime,
                                        f = function(x)
                                          as.character(
                                            last_(x,
                                                  ignore_na = TRUE))),

                              "recOnSeconds" =
                                rollfun(x = recOn,
                                        f = function(x)
                                          sum_(x,
                                               ignore_na = TRUE)),

                              "recOnPeriod" =
                                rollfun(x = recOn,
                                        f = function(x) {
                                          sum_(x,
                                               ignore_na = TRUE) %>%
                                            lubridate::seconds_to_period() %>%
                                            {sprintf('%02d:%02d:%05.2f',
                                                     lubridate::hour(x = .),
                                                     lubridate::minute(x = .),
                                                     lubridate::second(x = .)
                                            )}
                                        }),

                              "recIdStart" =
                                rollfun(x = recId,
                                        f = function(x)
                                          as.character(
                                            first_(x,
                                                   ignore_na = TRUE)))
                            ),
                            by = by.Cols]

          minutes.DT <-
            .rbind.data.table(
              minutes.DT,
              tempMins.DT[!is.na(clockTimeStart) | !is.na(clockTimeEnd)],
              fill = TRUE
            )
          # order chronologically
          # priority to clockTimeStart,
          # then if equal, order by DayInSecondsStart
          # gets rows in time order, accounting for overlap
          setorder(minutes.DT, DayInSecondsEnd, DayInSecondsStart)


          if (!is.null(roll.by.mins)) {

            minutes.DT[, ":=" (recRleid = rleid(recIdStart))]
            # warning is annoying & meaningless here
            suppressWarnings(
              minutes.DT[, "roll.key" :=
                           rep(1:(bin.to.mins/roll.by.mins), length.out = .N),
                         by = "recRleid"])
          }

        } else {

          ##### all other user options #####

          minutes.DT <-
            seconds.DT[,
                       .(
                         #time columns
                         "DayInSecondsStart" =
                           rollfun(x = DayInSeconds,
                                   f = function(x)
                                     first_(x, ignore_na = TRUE)),

                         "DayInSecondsEnd" =
                           rollfun(x = DayInSeconds,
                                   f = function(x)
                                     last_(x, ignore_na = TRUE)),

                         "DayInSecondsDur" =
                           rollfun(x = DayInSeconds,
                                   f = function(x)
                                     n_unique_(x, ignore_na = TRUE)),

                         "clockTimeStart" =
                           rollfun(x = clockTime,
                                   f = function(x)
                                     as.character(
                                       first_(x, ignore_na = TRUE))),

                         "clockTimeEnd" =
                           rollfun(x = clockTime,
                                   f = function(x)
                                     as.character(
                                       last_(x, ignore_na = TRUE))),

                         "recTimeStart" =
                           rollfun(x = recTime,
                                   f = function(x)
                                     as.double(
                                       first_(x, ignore_na = TRUE))),

                         "recTimeEnd" =
                           rollfun(x = recTime,
                                   f = function(x)
                                     as.double(
                                       last_(x, ignore_na = TRUE))),

                         "recClockStart" =
                           rollfun(x = recClock,
                                   f = function(x)
                                     as.character(
                                       first_(x, ignore_na = TRUE))),

                         "recClockEnd" =
                           rollfun(x = recClock,
                                   f = function(x)
                                     as.character(
                                       last_(x, ignore_na = TRUE))),

                         "recOnSeconds" =
                           rollfun(x = recOn,
                                   f = sum_double),

                         "recOnPeriod" =
                           rollfun(x = recOn,
                                   f = function(x) {
                                     sum_(x, ignore_na = TRUE) %>%
                                       lubridate::seconds_to_period() %>%
                                       {sprintf('%02d:%02d:%05.2f',
                                                lubridate::hour(x = .),
                                                lubridate::minute(x = .),
                                                lubridate::second(x = .)
                                       )}
                                   }),

                         "recIdStart" =
                           rollfun(x = recId,
                                   f = function(x) as.character(first(x))),
                         "recIdEnd" =
                           rollfun(x = recId,
                                   f = function(x) as.character(last(x))),

                         # segment sound info
                         "segAvgdB" =
                           rollfun(x = segAvgdB,
                                   f = function(x) as.double(mean_(x, ignore_na = TRUE))),
                         "segPeakdB" =
                           rollfun(x = segPeakdB,
                                   f = function(x) as.double(max_(x, ignore_na = TRUE))),

                         # adult word counts
                         "adultWordCnt" =
                           rollfun(x = adultWordCnt,
                                   f = sum_double),
                         "femaleAdultWordCnt" =
                           rollfun(x = femaleAdultWordCnt,
                                   f = sum_double),
                         "maleAdultWordCnt" =
                           rollfun(x = maleAdultWordCnt,
                                   f = sum_double),

                         # adult speech seconds
                         "femaleAdultSpeechScnds" =
                           rollfun(x = femaleAdultSpeech,
                                   f = sum_double),

                         "maleAdultSpeechScnds" =
                           rollfun(x = maleAdultSpeech,
                                   f = sum_double),

                         # adult nonspeech seconds
                         "femaleAdultNonspeechScnds" =
                           rollfun(x = femaleAdultNonSpeech,
                                   f = sum_double),
                         "maleAdultNonspeechScnds" =
                           rollfun(x = maleAdultNonSpeech,
                                   f = sum_double),

                         # child columns
                         "childUttCnt" =
                           rollfun(x = childUttCnt,
                                   f = sum_double),

                         # ADEX-equivalent child utterance seconds
                         "childUttLenScnds" =
                           rollfun(x = childUttLen,
                                   f = sum_double),

                         # child utterance seconds from ITS directly
                         "childUttScnds" =
                           rollfun(x = childUtt,
                                   f = sum_double),

                         "childCryScnds" =
                           rollfun(x = childCry,
                                   f = sum_double),

                         "childVfxScnds" =
                           rollfun(x = childVfx,
                                   f = sum_double),

                         # conversational turns
                         "convTurnCount" =
                           rollfun(x = convTurnCount,
                                   f = sum_double),

                         "convTurnInitiateScnds" =
                           rollfun(x = convTurnInitiate,
                                   f = sum_double),

                         "convTurnRespondScnds" =
                           rollfun(x = convTurnRespond,
                                   f = sum_double),

                         "convTurnExtendScnds" =
                           rollfun(x = convTurnExtend,
                                   f = sum_double),

                         # speakers
                         "MAN" =
                           rollfun(x = MAN,
                                   f = sum_double),

                         "MAF" =
                           rollfun(x = MAF,
                                   f = sum_double),

                         "FAN" =
                           rollfun(x = FAN,
                                   f = sum_double),

                         "FAF" =
                           rollfun(x = FAF,
                                   f = sum_double),

                         "CHN" =
                           rollfun(x = CHN,
                                   f = sum_double),

                         "CHF" =
                           rollfun(x = CHF,
                                   f = sum_double),

                         "CXN" =
                           rollfun(x = CXN,
                                   f = sum_double),

                         "CXF" =
                           rollfun(x = CXF,
                                   f = sum_double),

                         "NON" =
                           rollfun(x = NON,
                                   f = sum_double),

                         "NOF" =
                           rollfun(x = NOF,
                                   f = sum_double),

                         "OLN" =
                           rollfun(x = OLN,
                                   f = sum_double),

                         "OLF" =
                           rollfun(x = OLF,
                                   f = sum_double),

                         "TVN" =
                           rollfun(x = TVN,
                                   f = sum_double),

                         "TVF" =
                           rollfun(x = TVF,
                                   f = sum_double),

                         "SIL" =
                           rollfun(x = MAN,
                                   f = sum_double),

                         "OFF" =
                           rollfun(x = OFF,
                                   f = sum_double)
                       ),
                       by = by.Cols]



          # order chronologically
          # priority to clockTimeStart,
          # then if equal, order by DayInSecondsStart
          # gets rows in time order, accounting for overlap
          setorder(minutes.DT, DayInSecondsEnd,  DayInSecondsStart)

          if (!is.null(roll.by.mins)) {
            # warning is annoying & meaningless here
            suppressWarnings(
              minutes.DT[, "roll.key" :=
                           rep(1:(bin.to.mins/roll.by.mins), length.out = .N),
                         by = by.Cols])
          }

        }
      }
      # pull unique from seconds
      minutes.DT[, ":=" (
        # datetime info
        "timezone" =
          seconds.DT[, unique(na.omit(timezone))],

        "dateMidnight_epoch" =
          seconds.DT[, unique(na.omit(dateMidnight_epoch))]
      )]

      # recalculate epochtime
      minutes.DT[, ":=" (
        "epochTimeStart" =
          DayInSecondsStart + dateMidnight_epoch,

        "epochTimeEnd" =
          DayInSecondsEnd + dateMidnight_epoch)]

      # recalculate datetime
      minutes.DT[, ":=" (

        "dateTimeStart" =
          as.POSIXct(epochTimeStart,
                     tz = unique(timezone),
                     origin = "1970-01-01"),

        "dateTimeEnd" =
          as.POSIXct(epochTimeEnd,
                     tz = unique(timezone),
                     origin = "1970-01-01")
      )]


      if ("recRleid" %in% colnames(minutes.DT)) {
        minutes.DT[,
                   # delete runlength id column
                   recRleid := NULL]
      }

      # add binning interval
      minutes.DT[, "Bin.Mins" := bin.to.mins]

      ###### Write csv of binned.DT ######

      message(paste("writing to csv at", Sys.time()))

      # row.names = FALSE or prints extra column of
      # numbers with no column name
      # predict problems in the future if rownames printed

      fwrite(x = minutes.DT,
             file =
               paste0(
                 bin.CSV.dir,
                 output.filename),
             # date
             row.names = FALSE,
             verbose = FALSE,
             nThread = (parallel::detectCores() - 1),
             buffMB = 1024)

      rm(minutes.DT)

      functionEndTime <- Sys.time()
      message("
                  Finished ", subjID, " at ", functionEndTime)
      timeToRun <- round(functionEndTime - functionStartTime, 2)
      message(
        "Time to run bin_seconds: ", timeToRun, " ", attr(timeToRun, "units"), "

            ")

    }
  }

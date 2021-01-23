#' @title remove_recordings
#'
#' @description  This function removes specified recordings from an ITS file, adjusts the recording start and end centisecond counts, then saves a NEW ITS file with the name "originalname_edited.its".
#'
#' For more information, including example outputs and explanations of all output columns, see \url{https://htanderson.github.io/ITSbin/}
#'
#' @param ITS.folder Directory (string) containing ITS files.
#' @param ITS.file Single .its file to be altered (string)
#' @param edited.ITS.folder Directory (string) to store edited ITS file.
#' @param recordings.to.remove Integer: Recording number(s) to be removed?
#' @return An its/xml file
#' @import xml2
#' @import data.table
#' @import magrittr
#' @export
#' @examples
#' \dontrun{
#'
#' remove_recordings(
#' ITS.folder = "SERVER/ITS_Files/ITS",
#' ITS.file = "subj001.its",
#' recordings.to.remove = 1)
#'
#' }
#'


remove_recordings <-
  function(ITS.folder,
           ITS.file,
           edited.ITS.folder,
           recordings.to.remove) {

    ### add forward slash to directory names if missing

    input.dirs <-
      list("ITS.dir" = ITS.folder,
           "edited.ITS.folder" = edited.ITS.folder)

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

    ### Create subject ID from ITS.file name ###

    ITSfilename <- ITS.file %>%
      strsplit(split = ".its") %>%
      unlist

    its.object <-
      ITS.folder %>%
      paste0(ITS.file) %>%
      read_xml()

    # Import ITS recordings as list
    recordings.DF <-
      its.object %>%
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

    # convert to numeric
    recordings.to.remove <-
      as.numeric(recordings.to.remove)
    recordings.DF[, "num" := as.numeric(num)]


    # recording numbers to be kept
     recordings.to.keep <-
       recordings.DF$num[
        !recordings.DF$num %in% recordings.to.remove]

    # are all recordings to be removed
    # before recordings to be kept?
    before.keep <-
      all(recordings.to.remove < recordings.to.keep)

    # ITS passed through function will be altered!
    # Not normal for R - you do NOT create a new object without the
    # sections removed - the old object is changed, new contains only
    # sections removed
    nodeRemoved <-
      its.object %>%
      # find all Recording nodes under ProcessingUnit
      xml_find_all(xpath = "//ProcessingUnit/Recording") %>%
      # select only those with desired recording number
      subset(x = .,
             subset = xml_attr(., attr = "num") %in% recordings.to.remove) %>%
      # remove them from the xml ITS
      xml_remove(free = FALSE)

    if (before.keep == TRUE) {

      message("This file will have many start & end childUtt/Cry/Vfx# = 'NA'.
          This is a side-effect of adjusting the times, but should have
          no impact on future use.")

      firstTime <-
        its.object %>%
        xml_find_first(xpath = "//ProcessingUnit/Recording") %>%
        xml_attr(attr = "startTime") %>%
        stringi::stri_sub(from = 3, to = -2) %>%
        as.numeric()

      # Subtract firstTime and return to LENA PT#.##S format
      # Only for start and end times, not lengths
      subtStEndLENA <- function(x){
        stringi::stri_sub(str = x, from = 3, to = -2) %>%
          as.numeric %>%
          subtract(e2 = firstTime) %>%
          format(digits = 2, nsmall = 2) %>%
          paste0("PT", ., "S") %>%
          gsub(pattern = " ", replacement = "", .)
      }

      paths <- c("//ProcessingUnit/Recording", ".//Conversation",
                 ".//Pause", ".//Segment")

      confirmDF <- data.frame(
        startTime = NA,
        endTime = NA,
        path = NA)


      for (path in paths) {

        numAttr <-
          its.object %>%
          xml_find_first(xpath = path) %>%
          xml_attr(attr = "num")

        if (!is.na(numAttr)) {

          numSubt <-
            its.object %>%
            xml_find_first(xpath = path) %>%
            xml_attr(attr = "num") %>%
            as.numeric %>%
            subtract(1)

          newNums <-
            its.object %>%
            xml_find_all(xpath = path) %>%
            xml_attr(attr = "num") %>%
            as.numeric %>%
            subtract(e2 = numSubt)

          # set numbers
          its.object %>%
            xml_find_all(x = ., xpath = path) %>%
            mapply(FUN = xml_set_attr, x = .,
                   attr = "num", value = newNums)
        }

        # extract times, subtract beginning time
        newStartTimes <-
          its.object %>%
          xml_find_all(xpath = path) %>%
          xml_attr(attr = "startTime") %>%
          subtStEndLENA


        newEndTimes <-
          its.object %>%
          xml_find_all(xpath = path) %>%
          xml_attr(attr = "endTime") %>%
          subtStEndLENA

        # set start times
        its.object %>%
          xml_find_all(x = ., xpath = path) %>%
          mapply(FUN = xml_set_attr, x = .,
                 attr = "startTime", value = newStartTimes)

        # set end times
        its.object %>%
          xml_find_all(x = ., xpath = path) %>%
          mapply(FUN = xml_set_attr, x = .,
                 attr = "endTime", value = newEndTimes)

        # double-check that it worked
        tempDF <-
          its.object %>%
          xml_find_all(xpath = path) %>%
          xml_attr(attr = "startTime") %>%
          cbind.data.frame(
            path = path,
            startTime = .)

        tempCol <-
          its.object %>%
          xml_find_all(xpath = path) %>%
          xml_attr(attr = "endTime") %>%
          cbind(endTime = .)

        tempDF <- cbind(tempDF, tempCol)
        confirmDF <- rbind(confirmDF, tempDF)

        rm(newStartTimes)
        rm(newEndTimes)

      }

      # Correct child Utt, Cry, Vfx attributes
      # startUtt#, endUtt#,
      # startCry#, endCry#
      # startVfx#, endVfx#

      segsDF <-
        its.object %>%
        xml_find_all(x = .,
                     xpath = ".//Segment") %>%
        # extract attributes
        xml_attrs %>%
        # rowbind to dataframe
        lapply(FUN = as.data.frame.list,
               stringsAsFactors = FALSE) %>%
        do.call(what = dplyr::bind_rows)

      childAttrs <- c("Utt", "Cry", "Vfx")

      for (att in childAttrs) {
        # Index all column names of all utterances
        # Number of columns differ by the file, so requires flexible coding
        childCols <- grep(x = colnames(segsDF),
                          pattern = paste0(att, "[[:digit:]]"))

        # Maximum number of indexed columns
        maxAttrNum <-
          colnames(segsDF)[childCols] %>%
          stringi::stri_extract_all_charclass(
            ., "[[:digit:]]") %>%
          as.numeric() %>%
          max()

        for (i in 1:maxAttrNum) {
          # extract times, subtract beginning time
          newStartTimes <-
            its.object %>%
            xml_find_all(xpath = ".//Segment") %>%
            xml_attr(attr = paste0("start", att, i))

          newStartTimes <-
            ifelse(is.na(newStartTimes),
                   yes = NA,
                   no = subtStEndLENA(newStartTimes))

          newEndTimes <-
            its.object %>%
            xml_find_all(xpath = ".//Segment") %>%
            xml_attr(attr = paste0("end", att, i))

          newEndTimes <-
            ifelse(is.na(newEndTimes),
                   yes = NA, no = subtStEndLENA(newEndTimes))

          # set start times
          its.object %>%
            xml_find_all(x = ., xpath = ".//Segment") %>%
            mapply(FUN = xml_set_attr, x = .,
                   attr = paste0("start", att, i),
                   value = newStartTimes)

          # set end times
          its.object %>%
            xml_find_all(x = ., xpath = ".//Segment") %>%
            mapply(FUN = xml_set_attr, x = .,
                   attr = paste0("end", att, i),
                   value = newEndTimes)
        }

      }

      confirmDF <- na.omit(confirmDF)

      confirmDF$ITSfilename <- ITSfilename

      write.csv(confirmDF, file = paste0(ITSfilename, "_newTimes.csv"))

    }

    # write new file
    write_xml(x = its.object,
              file = paste0(edited.ITS.folder,
                            ITSfilename,
                            "_rec",
                            paste0(recordings.to.remove, collapse = ""),
                            "removed.its"))

    message(ITSfilename, " complete. Recording(s) ", paste0(recordings.to.remove, collapse = ", "), " removed. Recording(s) ", paste0(recordings.to.keep, collapse = ", "), " remain in ITS file. See ", paste0(ITSfilename, "_rec", paste0(recordings.to.remove, collapse = ""), "removed.its"))


  }

#' remove_recordings
#'
#' This function removes unwanted recordings from an ITS file and adjusts the recording start and end centisecond counts.
#' @param ITS.folder Directory (string) containing ITS files.
#' @param ITS.file Single .its file to be altered
#' @param remove.num Integer: Recording number(s) to be removed?
#' @param before.keep Logical: Is/are the recording(s) to be removed before the recording(s) to be kept?
#' @return An its/xml file
#' @import xml2
#' @export
#' @examples
#' removeRecds(
#' ITS.folder = "SERVER/ITS_Files/ITS",
#' ITS.file = "subj001.its",
#' remove.num = 1,
#' before.keep = TRUE)


remove_recordings <-
  function(ITS.folder,
           ITS.file,
           remove.num,
           before.keep) {

    ### Create subject ID from ITS.file name ###

    subjID <- ITS.file %>%
      strsplit(split = ".its") %>%
      unlist

    its.object <-
      ITS.folder %>%
      paste0(ITS.file) %>%
      read_xml()

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
             subset = xml_attr(., attr = "num") %in% remove.num) %>%
      # remove them from the xml ITS
      xml_remove(free = FALSE)

    if (before.keep == TRUE) {

      message("This file will have many start & end Utt/Cry/Vfx# = 'NA'.
          This is a side-effect of fixing the file, but should have
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

      confirmDF$subjID <- subjID

      write.csv(confirmDF, file = paste0(subjID, "_newTimes.csv"))

    }

    # write new file
    write_xml(x = its.object,
              file = paste0(subjID,
                            "_rec",
                            paste0(remove.num, collapse = ""),
                            "removed.its"))


  }

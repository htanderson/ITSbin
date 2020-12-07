#' removeRecds
#'
#' This function removes unwanted recordings from an ITS file and adjusts the recording start and end centisecond counts.
#' @param path.in Directory (string) containing ITS files. Default = working directory.
#' @param ITS .its file to be altered
#' @param removeNum Integer: Recording number(s) to be removed?
#' @param beforeRec Logical: Is/are the recording(s) to be removed before the recording(s) to be kept?
#' @return An its/xml file
#' @export
#' @examples
#' removeRecds(path.in = "D:/SkyDrive/Documents/UOregon/16_FYP_LENA/ITS_Files/ITS", ITS = "EH_044_02.its", removeNum = 1, beforeRec = TRUE)


removeRecds <- function(path.in = paste0(getwd(), "/"),
                        ITS, removeNum, beforeRec) {

  # In order to see the centiseconds, you need to set the below options
  # These fix a weird windows rounding error
  options(digits.secs = 2)
  options(digits = 22)
  options(scipen = 999)

  subjID <- substr(ITS, start = 1, stop = 9)

  itsFile <-
    path.in %>%
    paste0(ITS) %>%
    read_xml

  # ITS passed through function will be altered!
  # Not normal for R - you do NOT create a new object without the
  # sections removed - the old object is changed, new contains only
  # sections removed
  nodeRemoved <-
    itsFile %>%
    # find all Recording nodes under ProcessingUnit
    xml_find_all(xpath = "//ProcessingUnit/Recording") %>%
    # select only those with desired recording number
    subset(x = .,
           subset = xml_attr(., attr = "num") %in% removeNum) %>%
    # remove them from the xml ITS
    xml_remove(free = FALSE)

  if (beforeRec == TRUE) {

    message("This file will have many start & end Utt/Cry/Vfx# = 'NA'.
          This is a side-effect of fixing the file, but should have
          no impact on future use.")

    firstTime <-
      itsFile %>%
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
        itsFile %>%
        xml_find_first(xpath = path) %>%
        xml_attr(attr = "num")

      if (!is.na(numAttr)) {

      numSubt <-
        itsFile %>%
        xml_find_first(xpath = path) %>%
        xml_attr(attr = "num") %>%
        as.numeric %>%
        subtract(1)

      newNums <-
        itsFile %>%
        xml_find_all(xpath = path) %>%
        xml_attr(attr = "num") %>%
        as.numeric %>%
        subtract(e2 = numSubt)

      # set numbers
      itsFile %>%
        xml_find_all(x = ., xpath = path) %>%
        mapply(FUN = xml_set_attr, x = .,
               attr = "num", value = newNums)
      }

      # extract times, subtract beginning time
      newStartTimes <-
        itsFile %>%
        xml_find_all(xpath = path) %>%
        xml_attr(attr = "startTime") %>%
        subtStEndLENA


      newEndTimes <-
        itsFile %>%
        xml_find_all(xpath = path) %>%
        xml_attr(attr = "endTime") %>%
        subtStEndLENA

      # set start times
      itsFile %>%
        xml_find_all(x = ., xpath = path) %>%
        mapply(FUN = xml_set_attr, x = .,
               attr = "startTime", value = newStartTimes)

      # set end times
      itsFile %>%
        xml_find_all(x = ., xpath = path) %>%
        mapply(FUN = xml_set_attr, x = .,
               attr = "endTime", value = newEndTimes)

      # double-check that it worked
      tempDF <-
        itsFile %>%
        xml_find_all(xpath = path) %>%
        xml_attr(attr = "startTime") %>%
        cbind.data.frame(
          path = path,
          startTime = .)

      tempCol <-
        itsFile %>%
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
      itsFile %>%
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
        as.numeric %>%
        max

      for (i in 1:maxAttrNum) {
        # extract times, subtract beginning time
        newStartTimes <-
          itsFile %>%
          xml_find_all(xpath = ".//Segment") %>%
          xml_attr(attr = paste0("start", att, i))

        newStartTimes <-
          ifelse(is.na(newStartTimes),
                 yes = NA,
                 no = subtStEndLENA(newStartTimes))

        newEndTimes <-
          itsFile %>%
          xml_find_all(xpath = ".//Segment") %>%
          xml_attr(attr = paste0("end", att, i))

        newEndTimes <-
          ifelse(is.na(newEndTimes),
               yes = NA, no = subtStEndLENA(newEndTimes))

        # set start times
        itsFile %>%
          xml_find_all(x = ., xpath = ".//Segment") %>%
          mapply(FUN = xml_set_attr, x = .,
                 attr = paste0("start", att, i),
                 value = newStartTimes)

        # set end times
        itsFile %>%
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
  write_xml(x = itsFile, file = paste0(subjID, "_edited.its"))


}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ITS_to_seconds.R
\name{ITS_to_seconds}
\alias{ITS_to_seconds}
\title{ITS_to_seconds}
\usage{
ITS_to_seconds(
  ITS.dir,
  CSV.dir,
  time.zone = NULL,
  write.recordings = FALSE,
  write.blocks = FALSE,
  write.segments = FALSE,
  write.centiseconds = FALSE,
  write.seconds = TRUE
)
}
\arguments{
\item{ITS.dir}{Directory (string) containing ITS files.}

\item{CSV.dir}{Directory (string) to store CSV files}

\item{time.zone}{OS-specific character string for time zone. To use current system timezone, type `Sys.timezone()`. For other options, run `OlsonNames()` for list.}

\item{write.recordings}{Logical. Default = FALSE. Output a CSV containing ITS Recording-level information for each input ITS file. Note: The Recordings CSV will *always* be written for any ITS files containing recordings which start on more than one day.}

\item{write.blocks}{Logical. Default = FALSE. Output a CSV containing ITS Block-level information  (Conversations & Pauses) for each input ITS file.}

\item{write.segments}{Logical. Default = FALSE. Output a CSV containing ITS Segment-level information (uninterrupted human-created sounds, electronic sounds, noise, or silence) for each input ITS file.}

\item{write.centiseconds}{Logical. Default = FALSE. Output a CSV containing centisecond-level information for each input ITS file. Output file contains 1 row per centisecond in the day, running from midnight the day the recorder was first turned on until the later of noon the following day or when the recorder was turned off for the last time. WARNING: These files are >2GB each.}

\item{write.seconds}{Logical. Default = TRUE. Output a CSV containing second-level information for each input ITS file. Output file contains 1 row per second in the day, running from midnight the day the recorder was first turned on until the later of noon the following day or when the recorder was turned off for the last time. This file is required for the `bin_seconds` function.}
}
\value{
Per ITS file: 1 CSV each of specified outputs (recordings, blocks, segments, centiseconds, seconds). Per function run: 1 Validation, 1 ITS_checks, and 1 processing_completed file. If any files fail validation, 1 additional tracking CSV output (ValidationFails.csv)
}
\description{
This function takes a folder containing LENA ITS files as input, imports Recordings(recorder turned on & off), Blocks (Conversations & Pauses), and Segments (uninterrupted human-created sounds, electronic sounds, noise, or silence) from each ITS file, converts them into dataframes, then adds time from desired time zone in clocktime and seconds from midnight. Files are expanded from 1 row per segment to 1 row per centisecond, then collapsed to 1 row per second (final output). Centiseconds & seconds files start at midnight the first day the recorder was turned on and end at the later of noon the following day or when the recorder was turned off for the last time.

 *Any existing output CSV files will be overwritten.* To avoid this, archive old files in separate folder.

**IMPORTANT**: On most computers, `ITS_to_seconds` cannot process ITS files with recordings which start on multiple days, due to excessive RAM requirements. It is *strongly recommended* to run the `check_multiday` function prior to running the `ITS_to_seconds` function to identify any ITS files that might be too long. Then, use the `remove_recordings` function to separate multi-day files for smooth processing.

For more information, including example outputs and explanations of all output columns, see \url{https://htanderson.github.io/ITSbin/}
}
\examples{
\dontrun{

ITS_to_seconds(
ITS.dir = "SERVER:/ITS_Files/",
CSV.dir = "SERVER:/CSVOutput/",
time.zone = "America/Los_Angeles")

}
}

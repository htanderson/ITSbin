# ITS_to_seconds

This function takes a folder containing LENA ITS files as input, imports Recordings (recorder turned on & off), Blocks (Conversations & Pauses), and Segments (uninterrupted human-created sounds, electronic sounds, noise, or silence) from each ITS file, converts them into dataframes, then adds time from desired time zone in clocktime and seconds from midnight. Files are expanded from 1 row per segment to 1 row per centisecond, then collapsed to 1 row per second (final output). Centiseconds & seconds files start at midnight the first day the recorder was turned on and end at the later of noon the following day or when the recorder was turned off for the last time.

  *Any existing output CSV files will be overwritten.* To avoid this, archive old files in separate folder.

 **IMPORTANT**: On most computers, `ITS_to_seconds` cannot process ITS files with recordings which start on multiple days, due to excessive RAM requirements. It is *strongly recommended* to run the `check_multiday` function prior to running the `ITS_to_seconds` function to identify any ITS files that might be too long. Then, use the `remove_recordings` function to separate multi-day files for smooth processing.

## Parameters

`ITS.dir` Directory (string) containing ITS files.

`CSV.dir` Directory (string) to store CSV files.

`time.zone` OS-specific character string for time zone. To use current system timezone, type `Sys.timezone()`. For other options, run `OlsonNames()` for list.

`write.recordings` Logical. Default = FALSE. Output a CSV containing ITS Recording-level information for each input ITS file. Note: The Recordings CSV will *always* be written for any ITS files containing recordings which start on more than one day.

`write.blocks` Logical. Default = FALSE. Output a CSV containing ITS Block-level information (Conversations & Pauses) for each input ITS file.

`write.segments` Logical. Default = FALSE. Output a CSV containing ITS Segment-level information (uninterrupted human-created sounds, electronic noise, or silence) for each input ITS file.

`write.centiseconds` Logical. Default = FALSE. Output a CSV containing centisecond-level information for each input ITS file. Output file contains 1 row per centisecond in the day, running from midnight the day the recorder was first turned on until the later of noon the following day or when the recorder was turned off for the last time. *WARNING: These files are >2GB each.*

 `write.seconds` Logical. Default = TRUE. Output a CSV containing second-level information for each input ITS file. Output file contains 1 row per second in the day, running from midnight the day the recorder was first turned on until the later of noon the following day or when the recorder was turned off for the last time. This file is required for the `bin_seconds` function.
 
**Returns** Per ITS file: 1 CSV each of specified outputs (recordings, blocks, segments, centiseconds, seconds). Per function run: 1 Validation, 1 ITS_checks, and 1 processing_completed file. If any files fail validation, 1 additional tracking CSV output (ValidationFails.csv)

### Examples

```r
ITS_to_seconds(
      ITS.dir = "SERVER:/ITS_Files/",
      CSV.dir = "SERVER:/CSVOutput/",
      time.zone = "America/Los_Angeles")
```

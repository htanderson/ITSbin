# ITSbin

[![DOI](https://zenodo.org/badge/319454155.svg)](https://zenodo.org/badge/latestdoi/319454155)

This package takes LENA&trade; `.its` (Iterative Time Segments) files, extracts LENA&trade; automatic annotations, attaches real time, and outputs `.csv` files of recordings, conversation & pause blocks, segments, centiseconds, seconds and/or minutes. 

## Installation

This package is not available on CRAN. To install this package from GitHub, you will also need the package `devtools`. Run the code below to install ITSbin on your computer.

```r   
if(!"devtools" %in% installed.packages()[,"Package"]) {
  install.packages("devtools")
  }
devtools::install_github("htanderson/ITSbin", dependencies = TRUE)
```

If R asks to "install from packages requiring compilation", type `n`.

## Usage

This package has 4 functions intended to be run in order.

1. [`check_multiday`][`check_multiday`] Checks whether LENA&trade; `.its` files have recordings which _start_ on more than one day and may be too long to process.
    
    This package cannot handle ITS files in which recordings *start* on more than one day, due to excessive memory (RAM) requirements (recordings that *span* multiple days are fine). If an ITS file has recordings which *start* on more than one day, the ITS_to_seconds script will not process this file and note which files have issues in `ITS_file_checks.csv`. The file `SUBJID_recordings.csv` will contain the list, dates, and times of recordings present in the ITS file.

2. [`remove_recordings`][`remove_recordings`] If `check_multiday` identified `.its` files with recordings which start on more than one day, use this function to separate those recordings into multiple `.its` files before running `ITS_to_seconds`

3. [`ITS_to_seconds`][`ITS_to_seconds`] Takes in a LENA&trade; `.its` file and outputs user-specified CSV files of ITS recordings, ITS blocks, ITS segments, centiseconds-since-midnight, & seconds-since-midnight.

4. [`bin_seconds`][`bin_seconds`] Takes files in seconds-since-midnight and flexibly bins them into user-defined minutes. Options include: bin to any integer of minutes, sequential or rolling windows, rows align to midnight or rows align to time recorder was first turned on, & subset data based on one column before binning. Detailed explanation & examples of use: [bin_seconds-options][bin_seconds-options]

The functions `check_multiday`, `ITS_to_seconds`, & `bin_seconds` run over all relevant files in an entire folder, while the function `remove_recordings` runs over 1 `.its` file at a time.

## Help Files

Most help files can be accessed within R by typing ?functionname, eg

```r
?ITS_bin::ITS_to_seconds
?bin_seconds
```

For detailed explanations of the column names of each output file, see below links.

**Possible outputs for *each* input ITS file:**

### [`ITS_to_seconds`][`ITS_to_seconds`]

For *each* .its file run through `ITS_to_seconds`, you can choose to output any of the below CSV files. Of these, the Seconds option is necessary to bin to minutes through the `bin_seconds` function. The default is to output *only* the Seconds file.

[Recordings][Recordings] Explanations of column names and possible values from ITS Recordings level .CSV files from `check_multiday` and `ITS_to_seconds`.

[Blocks][Blocks] Explanations of column names and possible values from ITS Blocks level .CSV files from `ITS_to_seconds`.

[Segments][Segments] Explanations of column names and possible values from ITS Segments level .CSV files from `ITS_to_seconds`. NOTE: Order and number of Segments columns varies within each file, based on when sounds occurred during the recording. Additionally, each segment can contain multiple child utterance, cry, and nonspeech vocalization columns.

[Centiseconds][Centiseconds] Explanations of column names and possible values from Centiseconds .CSV files from `ITS_to_seconds` function.

[Seconds][Seconds] Explanations of column names and possible values from Seconds .CSV files from `ITS_to_seconds` function.

### [`bin_seconds`][`bin_seconds`]

[Binned Seconds/Minutes][Binned Seconds/Minutes] Explanations of column names and possible values from binned seconds/minutes .CSV files from `bin_seconds` function. Detailed explanation & examples of use: [bin_seconds-options][bin_seconds-options]

**Outputs from each `ITS_to_seconds` function run**

For the below outputs, one CSV will be output per *folder* run through [`ITS_to_seconds`][`ITS_to_seconds`].

[ITS_checks][ITS_checks] ITS_checks.csv output from `check_multiday` & `ITS_to_seconds` column name explanations

[Validation][Validation] Column name explanations for ITS_script_validation.csv & ValidationFails.csv output from `ITS_to_seconds`. These columns indicate whether the `ITS_to_seconds` function ran as expected. Examples include checking whether each data.table has the expected number of columns and rows, and whether sums of values in the Segments data.table match the same sums of values in the Centiseconds data.table. Possible values for all columns are `TRUE` or `FALSE`. If a cell is `NA`, processing was started but not completed for that file. Levels that have been processed will have validation columns, whether or not they were output as CSV files.


[processing_completed][processing_completed] Column name explanations for processing_completed.csv output from `check_multiday` & `ITS_to_seconds`. The purpose of this CSV is to keep track of what has/has not been completed in a given run of `check_multiday` or `ITS_to_seconds`. `check_multiday` will only have the first 3 fields, while `ITS_to_seconds` will have all fields listed.


[`check_multiday`]: https://htanderson.github.io/ITSbin/reference/check_multiday.html
[`remove_recordings`]: https://htanderson.github.io/ITSbin/reference/remove_recordings.html
[`ITS_to_seconds`]: https://htanderson.github.io/ITSbin/reference/ITS_to_seconds.html
[`bin_seconds`]: https://htanderson.github.io/ITSbin/reference/bin_seconds.html
[bin_seconds-options]: https://htanderson.github.io/ITSbin/articles/bin_seconds-options.html


[Recordings]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/Recordings_ColumnNames.csv
[Blocks]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/Blocks_ColumnNames.csv
[Segments]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/Segments_ColumnNames.csv
[Seconds]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/Seconds_ColumnNames.csv
[Centiseconds]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/Centiseconds_ColumnNames.csv
[Binned Seconds/Minutes]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/Minutes_ColumnNames.csv

[ITS_checks]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/ITS_checks_ColumnNames.csv
[Validation]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/validation_ColumnNames.csv
[processing_completed]: https://github.com/htanderson/ITSbin/blob/master/helpfiles/processing_completed_ColumnNames.csv

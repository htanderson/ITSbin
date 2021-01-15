# ITSbin

This package takes LENA&trade; `.its` (Iterative Time Segments) files, extracts LENA&trade; automatic annotations, attaches real time, and outputs `.csv` files of recordings, conversation & pause blocks, segments, centiseconds, seconds and/or minutes. 

## Installation

This package is not available on CRAN. To install this package from GitHub, you will also need the package `devtools`. Run the code below to install ITSbin on your computer.

```r   
if(!require(devtools) install.packages("devtools")
devtools::install_github("htanderson/ITSbin", dependencies = TRUE)
```

If R asks to "install from packages requiring compilation", type `n`.

## Usage

This package has 4 functions intended to be run in order.

1. `check_multiday` Checks whether LENA&trade; `.its` files have recordings which _start_ on more than one day and may be too long to process.
    
    This package cannot handle ITS files in which recordings *start* on more than one day, due to excessive memory (RAM) requirements (recordings that *span* multiple days are fine). If an ITS file has recordings which *start* on more than one day, the ITS_to_seconds script will not process this file and note which files have issues in `ITS_file_checks.csv`. The file `SUBJID_recordings.csv` will contain the list, dates, and times of recordings present in the ITS file.

2. `remove_recordings` If `check_multiday` identified `.its` files with recordings which start on more than one day, use this function to separate those recordings into multiple `.its` files before running `ITS_to_seconds`

3. `ITS_to_seconds` Takes in a LENA&trade; `.its` file and outputs user-specified CSV files of ITS recordings, ITS blocks, ITS segments, centiseconds-since-midnight, & seconds-since-midnight.

4. `bin_seconds` Takes files in seconds-since-midnight and flexibly bins them into user-defined minutes. Options include: bin to any integer of minutes, sequential or rolling windows, rows align to midnight or rows align to time recorder was first turned on, & subset data based on one column before binning.

The functions `check_multiday`, `ITS_to_seconds`, & `bin_seconds` run over all relevant files in an entire folder, while the function `remove_recordings` runs over 1 `.its` file at a time.

## Help Files

Most help files can be accessed within R by typing ?functionname, eg

```r
?ITS_bin::ITS_to_seconds
?bin_seconds
```

For detailed explanations of the column names of each output file, see below links.

[ITS_checks][ITS_checks] ITS_checks.csv output from check_multiday & ITS_to_seconds column name explanations

[Validation][Validation] ITS_to_seconds Script Validation Column Name explanations

[processing_completed][processing_completed] processing_completed.csv output from check_multiday & ITS_to_seconds column name explanations

[ITS_checks]: /helpfiles/ITS_checks_colnames.md

[Validation]: /helpfiles/Validation_colnames.md

[processing_completed]: /helpfiles/processing_completed_colnames.md

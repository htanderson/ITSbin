# ITSbin

This package takes LENA&trade; `.its` (Iterative Time Segments) files, extracts LENA&trade; automatic annotations, attaches real time, and outputs `.csv` files of recordings, conversation & pause blocks, segments, centiseconds, seconds and/or minutes. 

## Installation

This package is not available on CRAN. To install this package from GitHub, you will also need the package `devtools`. Run the code below to install ITSbin on your computer.

`if(!require(devtools) install.packages("devtools")`
`devtools::install_github("htanderson/ITSbin", dependencies = TRUE)`

If R asks to "install from packages requiring compilation", type `n`.

## Usage

This package has 3 functions:

1. ITS_to_seconds

2. bin_seconds

0. remove_recordings

### ITS_to_seconds




### remove_recordings

The remove_recordings script deletes a specified recording(s) from a single ITS file.

This package cannot handle ITS files in which recordings *start* on more than one day, due to excessive memory (RAM) requirements (recordings that *span* multiple days are fine). If an ITS file has recordings which *start* on more than one day, the ITS_to_seconds script will not process this file and note which files have issues in `ITS_file_checks.csv`. The file `SUBJID_recordings.csv` will contain the list, dates, and times of recordings present in the ITS file.


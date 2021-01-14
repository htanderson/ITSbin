# ITS_to_seconds Script Validation Column Names
Column name explanations for ITS_script_validation.csv & ValidationFails.csv output from ITS_to_seconds. These columns indicate whether the ITS_to_seconds function ran as expected. Examples include checking whether each data.table has the expected number of columns and rows, and whether sums of values in the Segments data.table match the same sums of values in the Centiseconds data.table.

Possible values for all columns are `TRUE` or `FALSE`. If a cell is `NA`, processing was started but not completed for that file.

Levels that have been processed will have validation columns, whether or not they were output as CSV files.

## ITS Recordings

**recCols14** Does the Recordings data.table have 14 columns?

**nrowsMaxRecId** Does the Recordings data.table have the same number of rows as the number of Recordings in the `.its` file?

## ITS Blocks (Conversations & Pauses)

**timesMatchEndToEnd.blks** Within each recording, does the end time (seconds-since-midnight) of each row (Block) match the start time of the next row?

**blockCols53** Does the Blocks data.table have 53 columns?

**nrowsMaxBlkId** Does the Blocks data.table have the same number of rows as the number of Blocks in the `.its` file?

## ITS Segments
**timesMatchEndToEnd.segs** Within each recording, does the end time (seconds-since-midnight) of each row (Segment) match the start time of the next row?

**segCols37p3ChildX4** Does the Segments data.table have 37 columns, plus (3 + (number of child cry/vfx/utt columns) * 4)? If there are *no* sounds from the target child in the file, expect 37 columns.

**segNrowMaxSegId** Does the Segments data.table have the same number of rows as the number of Segments in the `.its` file?

## Centiseconds

**csecRecOnMatchSegments** Does the sum of centiseconds in the Segments data.table match the sum of centiseconds of recorder-on time in the Centiseconds data.table?

**csecRecTimeStartAtZero** For the first row in which the recorder is on, is the value in the column `recTime` 0? (Aka, does the recorder clock start counting from 0?)

**csecRecTimeLastMatchSegs** Does the last row of `recTime` in which the recorder was on in Centiseconds match the last `endTime` of Segments?

**femAdultWordsSum** Does the sum of `femaleAdultWordCnt` in Segments equal the sum of `femaleAdultWordCnt` in Centiseconds?

**malAdultWordsSum** Does the sum of `maleAdultWordCnt` in Segments equal the sum of `maleAdultWordCnt` in Centiseconds?

**allAdultWordsSum** Does the sum of (`femaleAdultWordCnt` + `maleAdultWordCnt`) in Segments equal the sum of `adultWordCnt` in Centiseconds?

**fNonSpeechMatch** Does the sum of `femaleAdultNonSpeechLen` in Segments equal the sum of `femaleAdultNonSpeech` in Centiseconds?

**mNonSpeechMatch** Does the sum of `maleAdultNonSpeechLen` in Segments equal the sum of `maleAdultNonSpeech` in Centiseconds?

**childUttCntSumMatch**  Does the sum of `childUtt` in Segments equal the sum of `childUttCnt` in Centiseconds?

**childCryVfxSegsMatch**  Does the sum of `childCryVfxLen` in Segments equal the sum of `childCry` + `childVfx` in Centiseconds?

**convTurnMatchSegs**  Does the sum of `convTurnCount` in Segments equal the sum of `convTurnCount` in Centiseconds?

**csecRows36HrMin** Does the Centiseconds data.table have *at least* 12,960,000 rows (at least 36 hours).

**csec60Cols** Does the Centiseconds data.table have *exactly* 60 columns?

## Seconds

**rows36HrMin**Does the Seconds data.table have *at least* 129,600 rows (at least 36 hours).

**cols59** Does the Seconds data.table have *exactly* 59 columns?

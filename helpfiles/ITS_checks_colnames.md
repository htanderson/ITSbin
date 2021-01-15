# ITS_checks Column Names

Column name explanations for ITS_checks.csv output from check_multiday & ITS_to_seconds.

**subjID**: ITS file name. (*Future versions will separate ITS file name and ITS child ID.*)

## ITS Recordings
**recDayDurationHours**: HH:MM:SS.CS time duration from midnight the first day the recorder first turned on until the last time the recorder was turned off. If this is longer than 36 hours, R may crash (run out of RAM) when running ITS_to_seconds.

**continuousRecordingTime**: HH:MM:SS.CS time duration of *longest* continuous recording. LENA recommendation is minimum 4 hours for optimal speech processing.

**totalRecordingTime**: HH:MM:SS.CS time duration of total recording. LENA recommendation is minimum 10 hours for optimal speech processing.

**allRecsSameDay**: TRUE/FALSE Do all recordings *start* on the same day? If FALSE, R may crash (run out of RAM) when running ITS_to_seconds.


## ITS segments

**pctSilenceAndNoise**: Percent of total seconds of combined silence & noise out of total recording time.

**allColumnsPresentInSegments**: Are all possible segments columns present in the segments file? If FALSE, remaining columns will be filled with 0 or NA to process from segments to centiseconds.

**missingSegmentColumns**: If any possible columns are missing, they will be listed as string here.

**allSpeakersPresent**: Are all possible speaker types present in the segments file? If FALSE, remaining speaker columns will be filled with 0 or NA to process from segments to centiseconds.

**missingSpkrs**: If any possible speakers are missing, they will be listed as string here.

# Processing_completed Column Names

Column name explanations for processing_completed.csv output from `check_multiday` & `ITS_to_seconds`. The purpose of this CSV is to keep track of what has/has not been completed in a given run of `check_multiday` or `ITS_to_seconds`. `check_multiday` will only have the first 3 columns, while `ITS_to_seconds` will have all columns listed below.


**subjID** ITS file name. (*Future versions will separate ITS file name and ITS child ID.*)

**recordings.processed** Internal: Recordings level of .its file imported and converted into data.table.

**recordings.written** CSV of Recordings level of .its file written to `CSV.dir`.

**blocks.processed** Internal: Conversation & Pause Blocks level of .its file imported and converted into data.table.

**blocks.written** CSV of Conversation & Pause Blocks level of .its file written to `CSV.dir`.

**segments.processed** Internal: Segments level of .its file imported and converted into data.table.

**segments.written** CSV of Segments level of .its file written to `CSV.dir`.

**centiseconds.processed** Internal: Segments data expanded to 1 row per centisecond of the day, starting from midnight the day the recorder was first turned on.
 
**centiseconds.written** CSV of centiseconds data.table written to `CSV.dir`.

**seconds.processed** Internal: Centiseconds collapsed into seconds of day by calculating over 100 rows at a time, starting from midnight the day the recorder was first turned on.

**seconds.written** CSV of seconds data.table written to `CSV.dir`.

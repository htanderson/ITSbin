# Recordings Column Name Explanations

Explanations of column names from ITS Recordings level .CSV files from `check_multiday` and `ITS_to_seconds`.

[Recordings_ColumnNames.csv][Recordings_ColumnNames.csv] CSV of below information.

|Column Name                |Data Type         |Possible Values      |Meaning                                                                                |
|---------------------------|------------------|------------------------------------|---------------------------------------------------------------------------------------|
|subjID                     |character         |                   |ITS file name.                                                                         |
|startClockTime_UTC         |datetime/character |YYYY-MM-DDT HH:MM:SSZ  |start time of recording; UTC                                                           |
|endClockTime_UTC           |datetime/character |YYYY-MM-DDT HH:MM:SSZ  |end time of recording; UTC                                                             |
|startTime                  |numeric           |0+                 |start time of recording; seconds since recorder on                                     |
|endTime                    |numeric           |startTime+         |end time of recording; seconds since recorder on                                       |
|recId                      |integer           |1+                 |recording number, consecutively counts up from 1                                       |
|timezone                   |character         |see OlsonNames()   |timezone entered by user                                                               |
|startclocklocal            |datetime/character|YYYY-MM-DD HH:MM:SS|start time of recording in timezone                                                           |
|endclocklocal              |datetime/character|YYYY-MM-DD HH:MM:SS|start time of recording in timezone                                                           |
|dateMidnight_epoch         |integer           |                   |recording day midnight epoch seconds from 1970-1-1 00:00:00                            |
|startclocklocal_secMidnight|integer           |0+                 |start time of recording in seconds from midnight                                       |
|endclocklocal_secMidnight  |integer           |0+                 |end time of recording in seconds from midnight                                         |
|recClockEnd                |period            |HH:MM:SS           |end time of recording; Time since recorder was turned on (easy to compare to WAV files)|
|recDur                     |numeric           |0+                 |seconds & centiseconds duration of this recording number                               |


[Recordings_ColumnNames.csv]  /CSV_column_names/Recordings_ColumnNames.csv

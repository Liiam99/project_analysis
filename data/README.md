# Data Folders
Each folder in **/runs** contains the exact same structure:

- **boomlokaties_{DAY}{PART OF THE DAY}**: contains the tree locations with a ribbon of the run.
- **lintjes_tijden_{DAY}{PART OF THE DAY}**: contains the timestamps of moments when ribbons were collected.
- **optimaal_{DAY}{PART OF THE DAY}**: contains the distances between ribbons of the optimal run created by the model.
- **track_GPSA_{DAY}{PART OF THE DAY}**: contains the movement data of the run recorded by the A tracker.
- **track_GPSB_{DAY}{PART OF THE DAY}**: contains the movement data of the run recorded by the B tracker.

{DAY} can be 'monday', 'tuesday', 'wednesday', 'thursday', and 'friday'.
{PART OF THE DAY} can be 'morning' or 'afternoon' where 'morning' are the uniform runs and 'afternoon' the clustered runs.

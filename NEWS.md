# TrisomExploreR 0.2.5.9000

## New Functionality 

***
## Major Changes 
Major changes related to an effort to separate data from local application instances. This will allow for data refreshes to be pushed to the applications without having to re-deploy to hosting infrastructure. 
- Removing reliance on local SQLite Databases 
- Adding new class to download application specific files via Azure BLOB storage
- Updating old SQLite queries to read local `.parquet` files 
- Two manager classes are now deprecated due to these changes: 
1. TranscriptomeAppManager
2. ImmuneMapAppManager

***
## Minor Changes
Minor changes to code formatting and linting.
Added labels above all input elements that are rendered server-side for additional context for user. 
Moving call to download files outside of init for `AzureRemoteDataFileManager` class. Must explicity call `download_files` method to invoke local file download. 
Optimized calls to `annotatePointByKey` JS function. Now, trace and point indicies are provided directly rather than having the function foreach through all traces based on key name. Annotation text is driven from R rather than an attempt to parse the `text` property of the specific point in the trace. 
Moved calls to `annotatePointByKey` to `FeatureAnalysis` module. Added logic to figure out trace and key indext to annotate when an analyte is chosen from drop down rather than via click event from Volcano Plot. 

***
## Bug Fixes 
- Resolved issue where UI could become unresponsive when rendering a volcano plot with > 10000 analytes: updated `Analyte` selectize input to render server-side with new '`maxOptions = length(analytes)`' options arg. 
- Resolved issue where Volcano Multi-Select text would persist when only 1 Analyte is chosen: added a call to `r6$updateAnalyteAttributes()` to ensure new text is calculated based on chosen analytes.
-correcting inconsistent references to local file directory. Removed class init param and set as hard coded value "Remote_Data/"


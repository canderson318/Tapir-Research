# Malayan Analysis

Malayan Tapir data (TapirDataMalaysia_2009-2011.csv) was cleaned for independent records (malayanDataCleanForIndependentRecords.R). Independence was determined to be at one hour, so only the first record of several detections occurring at the same site were kept. There were issues running a kernel density plot because bandwidth couldn't be estimated for some reason; this data fixed that issue.

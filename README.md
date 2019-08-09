DataQualityDashboard
=================

DataQualityDashboard is an initiative in the OHDSI community to improve data quality standards in observational data science.

Introduction
============
An R package for characterizing the data quality of a person-level data source that has been converted into the OMOP CDM 5.3.1 format.

Features
========
- Utilizes configurable data checks
- Analyzes data in the common data model format for all data checks
- Produces a set of data check results with supplemental investigation assets.


Technology
==========
DataQualityDashboard is an R package that wraps a Java library for integration of data quality checks in the OHDSI WebAPI as well as stand-alone R processing.

System Requirements
===================
Requires R (version 3.2.2 or higher).  Requires Java.

Getting Started
===============
  ```r

# fill out the connection details -----------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "", user = "", 
                                                                password = "", server = "", 
                                                                port = "", extraSettings = "")

cdmDatabaseSchema <- "yourCdmSchema" # the fully qualified database schema name of the CDM
resultsDatabaseSchema <- "yourResultsSchema" # the fully qualified database schema name of the results schema (that you can write to)
cdmSourceName <- "Your CDM Source" # a human readable name for your CDM source

# determine how many threads (concurrent SQL sessions) to use ----------------------------------------
numThreads <- 1 # on Redshift, 3 seems to work well

# specify if you want to execute the queries or inspect them ------------------------------------------
sqlOnly <- FALSE # set to TRUE if you just want to get the SQL scripts and not actually run the queries

# where should the logs go? -------------------------------------------------------------------------
outputFolder <- "output"

# logging type -------------------------------------------------------------------------------------
verboseMode <- FALSE # set to TRUE if you want to see activity written to the console

# write results to table? -----------------------------------------------------------------------
writeToTable <- TRUE # set to FALSE if you want to skip writing to results table

# if writing to table and using Redshift, bulk loading can be initialized -------------------------------

# Sys.setenv("AWS_ACCESS_KEY_ID" = "",
#            "AWS_SECRET_ACCESS_KEY" = "",
#            "AWS_DEFAULT_REGION" = "",
#            "AWS_BUCKET_NAME" = "",
#            "AWS_OBJECT_KEY" = "",
#            "AWS_SSE_TYPE" = "AES256",
#            "USE_MPP_BULK_LOAD" = TRUE)

# which DQ check levels to run -------------------------------------------------------------------
checkLevels <- c("TABLE", "FIELD", "CONCEPT")

# run the job --------------------------------------------------------------------------------------
DataQualityDashboard::execute(connectionDetails = connectionDetails, 
                              cdmDatabaseSchema = cdmDatabaseSchema, 
                              resultsDatabaseSchema = resultsDatabaseSchema,
                              cdmSourceName = cdmSourceName, 
                              numThreads = numThreads,
                              sqlOnly = sqlOnly, 
                              outputFolder = outputFolder, 
                              verboseMode = verboseMode,
                              writeToTable = writeToTable,
                              checkLevels = checkLevels)

# inspect logs ----------------------------------------------------------------------------
ParallelLogger::launchLogViewer(logFileName = file.path(outputFolder, cdmSourceName, 
                                                        sprintf("log_DqDashboard_%s.txt", cdmSourceName)))

# (OPTIONAL) if you want to write the JSON file to the results table separately -----------------------------
jsonFilePath <- ""
DataQualityDashboard::writeJsonResultsToTable(connectionDetails = connectionDetails, 
                                              resultsDatabaseSchema = resultsDatabaseSchema, 
                                              jsonFilePath = jsonFilePath)

```

User Documentation
==================

Support
=======
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements
 
License
=======
DataQualityDashboard is licensed under Apache License 2.0

### Development status

In early development phase.  Not ready for use.

# Acknowledgements


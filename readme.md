rgoogleanalytics
================

This is a fork of http://code.google.com/p/r-google-analytics/

Because of the deprecation of v2.3 of Google Analytics API I had to adjust the code to work with v2.4

The result is this package.

Installation
------------

Download the tgz-file (currently rgoogleanalytics_1.0.1.tar.gz).

Then install it in your running R using command 
  
	install.packages("~/source/R/rgoogleanalytics_1.0.1.tar.gz", repos = NULL, type="source")
  
  
Simple Query
------------
  
	library("rgoogleanalytics")
	ga <- RGoogleAnalytics()
	ga$SetCredentials("email", "password", "apikey")
	query <- QueryBuilder("apikey")
	query$Init(start.date="2012-07-28", end.date="2012-07-29", dimensions="ga:date", metrics="ga:pageviews", sort="ga:date", table.id="ga:TABLEID")
	ga.data <- ga$GetReportData(query)
	ga.data$data
  
  
ToDo
====
That's my first contact to R. So I have focused on getting the new API-version running.
I've neglected 
* man pages
* Correct package structure

Help is welcome.
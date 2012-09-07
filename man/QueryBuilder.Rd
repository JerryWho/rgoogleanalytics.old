\name{QueryBuilder}
\alias{QueryBuilder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
QueryBuilder(apikey)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{apikey}{
%%     ~~Describe \code{apikey} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (apikey) 
{
    kMaxDimensions <- 7
    kMaxMetrics <- 10
    kMaxTableIds <- 1
    start.date <- NULL
    end.date <- NULL
    dimensions <- NULL
    metrics <- NULL
    segment <- NULL
    sort <- NULL
    filters <- NULL
    max.results <- NULL
    start.index <- NULL
    table.id <- NULL
    StartDate <- function(start.date.param = NA) {
        if (is.null(start.date.param)) {
            start.date <<- NULL
            return(invisible())
        }
        if (is.na(start.date.param)) {
            return(start.date)
        }
        if (is.na(as.Date(start.date.param, "\%Y-\%m-\%d"))) {
            stop("A start date must be specified of the form YYYY-MM-DD")
        }
        start.date <<- start.date.param
        return(invisible())
    }
    EndDate <- function(end.date.param = NA) {
        if (is.null(end.date.param)) {
            end.date <<- NULL
            return(invisible())
        }
        if (is.na(end.date.param)) {
            return(end.date)
        }
        if (is.na(as.Date(end.date.param, "\%Y-\%m-\%d"))) {
            stop("An end date must be specified of the form YYYY-MM-DD")
        }
        end.date <<- end.date.param
        return(invisible())
    }
    Dimensions <- function(dimensions.param = NA) {
        if (is.null(dimensions.param)) {
            dimensions <<- NULL
            return(invisible())
        }
        if (is.na(dimensions.param[1])) {
            return(dimensions)
        }
        if (!is.vector(dimensions.param)) {
            stop(paste("dimensions must be a vector of string variables"))
        }
        if (length(dimensions.param) > kMaxDimensions) {
            stop(paste("Google Analytics can only handle up to", 
                kMaxDimensions, "dimensions parameters"))
        }
        if (!is.character(dimensions.param)) {
            stop(paste("dimensions must be character, please refer to the", 
                "Google Analytics API documentation for more information"))
        }
        dimensions <<- paste(dimensions.param, collapse = ",")
        return(invisible())
    }
    Metrics <- function(metrics.param = NA) {
        if (is.null(metrics.param)) {
            metrics <<- NULL
            return(invisible())
        }
        if (is.na(metrics.param[1])) {
            return(metrics)
        }
        if (!is.vector(metrics.param)) {
            stop("metrics must be a vector of string variables")
        }
        if (length(metrics.param) > kMaxMetrics) {
            stop(paste("Google Analytics can only handle up to", 
                kMaxMetrics, "metrics parameters"))
        }
        if (!is.character(metrics.param)) {
            stop(paste("metrics must be character string, please refer to the", 
                "Google Analytics API documentation for more information"))
        }
        metrics <<- paste(metrics.param, collapse = ",")
        return(invisible())
    }
    Segment <- function(segment.param = NA) {
        if (is.null(segment.param)) {
            segment <<- NULL
            return(invisible())
        }
        if (is.na(segment.param[1])) {
            return(segment)
        }
        segment <<- segment.param
        return(invisible())
    }
    Sort <- function(sort.param = NA) {
        if (is.null(sort.param)) {
            sort <<- NULL
            return(invisible())
        }
        if (is.na(sort.param[1])) {
            return(sort)
        }
        if (!is.vector(sort.param)) {
            stop("sort must be a vector of string variables")
        }
        if (!is.character(sort.param)) {
            stop(paste("sort must be character string, please refer to the", 
                "Google Analytics API documentation for more information"))
        }
        sort <<- paste(sort.param, collapse = ",")
        return(invisible())
    }
    Filters <- function(filters.param = NA) {
        if (is.null(filters.param)) {
            filters <<- NULL
            return(invisible())
        }
        if (is.na(filters.param[1])) {
            return(filters)
        }
        filters <<- filters.param
        return(invisible())
    }
    MaxResults <- function(max.results.param = NA) {
        if (is.null(max.results.param)) {
            max.results <<- NULL
            return(invisible())
        }
        if (is.na(max.results.param[1])) {
            return(max.results)
        }
        if (!is.numeric(max.results.param)) {
            stop("max.results must be a number")
        }
        check.vector.length <- length(max.results.param)
        if (check.vector.length > 1) {
            stop("Max Results must be a single numeric value")
        }
        max.results <<- max.results.param
        return(invisible())
    }
    StartIndex <- function(start.index.param = NA) {
        if (is.null(start.index.param)) {
            start.index <<- NULL
            return(invisible())
        }
        if (is.na(start.index.param[1])) {
            return(start.index)
        }
        if (!is.numeric(start.index.param)) {
            stop("start.index must be a number")
        }
        check.vector.length <- length(start.index.param)
        if (check.vector.length > 1) {
            stop("Start index must be a single numeric value")
        }
        start.index <<- start.index.param
        return(invisible())
    }
    TableID <- function(table.id.param = NA) {
        if (is.null(table.id.param)) {
            table.id <<- NULL
            return(invisible())
        }
        if (is.na(table.id.param[1])) {
            return(table.id)
        }
        if (!is.character(table.id.param)) {
            stop("A table.id  must be of the form 'ga:####'")
        }
        if (!is.vector(table.id.param)) {
            stop(paste("table.id must be a vector (length ", 
                kMaxTableIds, ") string variable"))
        }
        if (length(table.id.param) != kMaxTableIds) {
            stop(paste("Only", kMaxTableIds, "table.id can be used at a time."))
        }
        table.id <<- table.id.param
        return(invisible())
    }
    Validate <- function() {
        missing.params <- c()
        if (is.null(start.date)) {
            missing.params <- append(missing.params, "start.date")
        }
        if (is.null(end.date)) {
            missing.params <- append(missing.params, "end.date")
        }
        if (is.null(metrics)) {
            missing.params <- append(missing.params, "metrics")
        }
        if (is.null(table.id)) {
            missing.params <- append(missing.params, "table.id")
        }
        if (length(missing.params) == 0) {
            return(TRUE)
        }
        missing.string <- paste(missing.params, collapse = ", ")
        stop(paste("All GA queries must have", missing.string, 
            "parameters.", sep = " "))
    }
    ToUri <- function() {
        query <- c(start.date = start.date, end.date = end.date, 
            dimensions = dimensions, metrics = metrics, segment = segment, 
            sort = sort, filters = filters, max.results = max.results, 
            start.index = start.index, table.id = table.id, key = apikey)
        uri <- "https://www.googleapis.com/analytics/v2.4/data?"
        for (name in names(query)) {
            uri.name <- switch(name, start.date = "start-date", 
                end.date = "end-date", dimensions = "dimensions", 
                metrics = "metrics", segment = "segment", sort = "sort", 
                filters = "filters", max.results = "max-results", 
                start.index = "start-index", table.id = "ids", 
                key = "key")
            if (!is.null(uri.name)) {
                uri <- paste(uri, uri.name, "=", query[[name]], 
                  "&", sep = "")
            }
        }
        uri <- sub("&$", "", uri)
        return(uri)
    }
    ClearData <- function() {
        start.date <<- NULL
        end.date <<- NULL
        dimensions <<- NULL
        metrics <<- NULL
        segment <<- NULL
        sort <<- NULL
        filters <<- NULL
        max.results <<- NULL
        start.index <<- NULL
        table.id <<- NULL
        return(invisible())
    }
    Init <- function(start.date = NULL, end.date = NULL, dimensions = NULL, 
        metrics = NULL, segment = NULL, sort = NULL, filters = NULL, 
        max.results = NULL, start.index = NULL, table.id = NULL) {
        StartDate(start.date)
        EndDate(end.date)
        Dimensions(dimensions)
        Metrics(metrics)
        Segment(segment)
        Sort(sort)
        Filters(filters)
        MaxResults(max.results)
        StartIndex(start.index)
        TableID(table.id)
        return(invisible())
    }
    return(list(start.date = StartDate, end.date = EndDate, dimensions = Dimensions, 
        metrics = Metrics, segment = Segment, sort = Sort, filters = Filters, 
        max.results = MaxResults, start.index = StartIndex, table.id = TableID, 
        to.uri = ToUri, clear.data = ClearData, validate = Validate, 
        Init = Init))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

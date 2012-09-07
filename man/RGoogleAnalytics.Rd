\name{RGoogleAnalytics}
\alias{RGoogleAnalytics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RGoogleAnalytics()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    kMaxDefaultRows <- 10000
    kMaxPages <- 100
    auth.token <- NULL
    SetCredentials <- function(username, pass, apikey) {
        if (is.null(username) || is.null(pass)) {
            stop("Please supply a user name and password")
        }
        auth.resp <- postForm("https://www.google.com/accounts/ClientLogin", 
            Email = username, Passwd = pass, accountType = "GOOGLE", 
            source = "r-google-analytics", service = "analytics", 
            key = apikey)
        gtoken <- unlist(strsplit(auth.resp, "\n"))
        parsed.gtoken <- unlist(strsplit(gtoken[3], "Auth="))
        if (length(parsed.gtoken) >= 2) {
            auth.token <<- unlist(strsplit(gtoken[3], "Auth="))[[2]]
        }
        else {
            stop("Authentication failed.")
        }
        return(invisible())
    }
    CheckAuthToken <- function() {
        if (is.null(auth.token)) 
            stop("Please enter the user name and password in SetCredentials")
    }
    GetAnyXMLAttribute <- function(vNode, attr.name) {
        kNumericAttributes = c("value", "confidenceInterval")
        if (xmlName(vNode) == "metric" && attr.name \%in\% kNumericAttributes) 
            return(as.numeric(xmlGetAttr(vNode, attr.name)))
        else return(xmlGetAttr(vNode, attr.name))
    }
    GetProfileData <- function() {
        api.response <- GetAccountFeedXML()
        status <- api.response$status
        if (status$code != "200") {
            error.message = ParseApiErrorMessage(api.response$body)
            stop(paste(status$code, status$message, "\n", error.message$code, 
                error.message$reason))
        }
        return(ParseAccountFeedXML(api.response$body))
    }
    GetAccountFeedXML <- function() {
        CheckAuthToken()
        header = basicTextGatherer()
        google.auth <- paste("GoogleLogin auth=", auth.token, 
            sep = "")
        response <- getURL("https://www.google.com/analytics/feeds/accounts/default", 
            .encoding = "UTF-8", httpheader = c(Authorization = google.auth, 
                `GData-Version` = 2), headerfunction = header$update)
        header.value <- parseHTTPHeader(header$value())
        header$reset()
        return(list(body = response, status = list(code = header.value[["status"]], 
            message = header.value[["statusMessage"]])))
    }
    ParseAccountFeedXML <- function(xml.string) {
        feed.xml <- xmlTreeParse(xml.string, asText = TRUE, useInternalNode = TRUE)
        feed.name.space <- sapply(xmlNamespaceDefinitions(feed.xml), 
            function(ns) ns$uri)
        names(feed.name.space)[1] <- "ns"
        total.results <- data.frame(unlist(xpathApply(feed.xml, 
            path = "//openSearch:totalResults", xmlValue, namespaces = feed.name.space)))
        names(total.results) <- "total.results"
        table.id.list <- data.frame(unlist(xpathApply(feed.xml, 
            path = "//ns:entry/dxp:tableId", xmlValue, namespaces = feed.name.space)))
        ns.path <- "//ns:entry/dxp:property[@name = 'ga:accountName']"
        table.account.list <- data.frame(unlist(xpathApply(feed.xml, 
            path = ns.path, GetAnyXMLAttribute, "value", namespaces = feed.name.space)))
        table.profile.list <- data.frame(unlist(xpathApply(feed.xml, 
            path = "//ns:entry/ns:title", xmlValue, namespaces = feed.name.space)))
        profile <- cbind(table.account.list, table.profile.list, 
            table.id.list)
        names(profile) <- c("AccountName", "ProfileName", "TableId")
        return(list(profile = profile, total.results = total.results))
    }
    GetDataFeed <- function(query.uri) {
        if (!is.character(query.uri)) {
            stop("the query.uri parameter must be a character string")
        }
        api.response <- GetDataFeedXML(query.uri)
        status <- api.response$status
        if (status$code != "200") {
            error.message = ParseApiErrorMessage(api.response$body)
            stop(paste(status$code, status$message, "\n", error.message$code, 
                error.message$reason))
        }
        return(ParseDataFeedXML(api.response$body))
    }
    GetDataFeedXML <- function(query.uri) {
        CheckAuthToken()
        header = basicTextGatherer()
        google.auth <- paste("GoogleLogin auth=", auth.token, 
            sep = "")
        response <- getURL(query.uri, .encoding = "UTF-8", httpheader = c(Authorization = google.auth, 
            `GData-Version` = 2), headerfunction = header$update)
        header.value <- parseHTTPHeader(header$value())
        header$reset()
        return(list(body = response, status = list(code = header.value[["status"]], 
            message = header.value[["statusMessage"]])))
    }
    ParseDataFeedXML <- function(xml.string) {
        if (!is.character(xml.string)) {
            stop("xml.string must be a character string")
        }
        feed.data = xml.string
        feed.xml <- xmlTreeParse(feed.data, asText = TRUE, useInternalNode = TRUE)
        feed.name.space <- sapply(xmlNamespaceDefinitions(feed.xml), 
            function(ns) ns$uri)
        names(feed.name.space)[1] <- "ns"
        titles <- unlist(xpathApply(feed.xml, "//ns:entry[1]/*[@name]", 
            GetAnyXMLAttribute, "name", namespaces = feed.name.space))
        values.list <- lapply(titles, function(name) {
            unlist(xpathApply(feed.xml, paste("//ns:entry/dxp:*[@name='", 
                name, "']", sep = ""), GetAnyXMLAttribute, "value", 
                namespaces = feed.name.space))
        })
        df.value <- data.frame(values.list, stringsAsFactors = FALSE)
        names(df.value) = titles
        ci.check <- unlist(xpathApply(feed.xml, "//ns:entry[1]/*[@confidenceInterval]", 
            GetAnyXMLAttribute, "name", namespaces = feed.name.space))
        df.ci <- data.frame(lapply(ci.check, function(name) {
            unlist(xpathApply(feed.xml, paste("//ns:entry/dxp:*[@name='", 
                name, "']", sep = ""), GetAnyXMLAttribute, "confidenceInterval", 
                namespaces = feed.name.space))
        }), stringsAsFactors = FALSE)
        names(df.ci) <- sub("$", ".C.I", ci.check)
        df.ci <- df.ci[colSums(df.ci) != 0]
        df <- df.value
        if (ncol(df.ci) > 0 && nrow(df.ci) > 0) {
            df <- cbind(df.value, df.ci[colSums(df.ci) != 0])
        }
        aggr.check <- unlist(xpathApply(feed.xml, "//dxp:aggregates/dxp:metric", 
            GetAnyXMLAttribute, "name", namespaces = feed.name.space))
        df.aggr <- data.frame(aggregate.totals = sapply(aggr.check, 
            function(name) {
                unlist(xpathApply(feed.xml, paste("//dxp:aggregates/dxp:*[@name='", 
                  name, "']", sep = ""), GetAnyXMLAttribute, 
                  "value", namespaces = feed.name.space))
            }))
        total.results <- unlist(xpathApply(feed.xml, path = "//openSearch:totalResults", 
            xmlValue, namespaces = feed.name.space))
        names(total.results) <- "total.results"
        total.results <- as.numeric(total.results)
        return(list(data = df, aggr.totals = df.aggr, total.results = total.results))
    }
    ParseApiErrorMessage <- function(xml.string) {
        if (!is.character(xml.string)) {
            stop("xml.string must be a character string")
        }
        code = ""
        reason = ""
        if (length(grep("<domain>GData</domain>", xml.string))) {
            error.xml <- xmlTreeParse(xml.string, asText = TRUE, 
                useInternalNode = TRUE)
            xml.name.space <- sapply(xmlNamespaceDefinitions(error.xml), 
                function(ns) ns$uri)
            names(xml.name.space)[1] <- "ns"
            code <- unlist(xpathApply(error.xml, path = "//ns:error/ns:code", 
                xmlValue, namespaces = xml.name.space))
            reason <- unlist(xpathApply(error.xml, path = "//ns:error/ns:internalReason", 
                xmlValue, namespaces = xml.name.space))
        }
        return(list(code = code, reason = reason))
    }
    GetReportData <- function(query.builder, start.index = 1, 
        max.rows = NULL) {
        query.builder$validate()
        kMaxDefaultRows <- 10000
        if (!is.null(max.rows)) {
            query.builder$max.results(min(kMaxDefaultRows, max.rows))
        }
        df <- GetDataFeed(query.builder$to.uri())
        if (is.null(max.rows) || max.rows <= kMaxDefaultRows) {
            return(df)
        }
        else {
            pagination <- min(kMaxPages, ceiling(df$total.results/kMaxDefaultRows))
            for (i in seq_along(2:pagination)) {
                start.index <- (i * kMaxDefaultRows) + 1
                query.builder$start.index(start.index)
                ga.data <- GetDataFeed(query.builder$to.uri())
                df$data <- rbind(df$data, ga.data$data)
                rm(ga.data)
            }
            return(df)
        }
    }
    return(list(GetProfileData = GetProfileData, GetAccountFeedXML = GetAccountFeedXML, 
        ParseAccountFeedXML = ParseAccountFeedXML, GetReportData = GetReportData, 
        GetDataFeed = GetDataFeed, GetDataFeedXML = GetDataFeedXML, 
        ParseDataFeedXML = ParseDataFeedXML, ParseApiErrorMessage = ParseApiErrorMessage, 
        SetCredentials = SetCredentials))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

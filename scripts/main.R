library(data.table)
library(ggplot2)
library(readxl)


read.Nalsa2010.xlsx <- function(fname) {
    # Ignore first row, second row is the header
    d <- data.table(read_excel(fname, 1, skip = 1))
    d <- d[,1:33] # ignore some columns at the end

    # Columns renamed as Grp* have no data (empty cols), they indicate data groups
    colnames(d) <- c('State', 'District', 
                     'Grp01', 'Appointed.PanelLawyers', 'Appointed.RetainerLawyers', 
                              'Appointed.PanelBreakdown', 'IsSeparatePanels',
                     'Grp02', 'LA.Apps.Rxd.Prison', 'LA.Apps.Rxd.Court', 'LA.Appointed.Prison', 
                              'LA.Appointed.Court', 'LA.HadLawyer.Prison', 'LA.HadLawyer.Court', 
                              'LA.Deficiencies.Prison', 'LA.Deficiencies.Court', 
                     'Grp03', 'Accused.PanelLawyers', 'Accused.RetainerLawyers', 'BailReleases', 
                              'Acquittals', 'CasesDisposed', 'CompletionReports.PanelLawyers', 
                              'Withdrawn.PanelLawyers', 
                     'Grp04', 'IsMonComConstituted', 'MonCom.StaffInfra', 
                     'Grp05', 'Reports.PanelLawyersToMonCom', 'Reports.BiMonthlyByMonCom', 
                     'Grp06', 'Complaint.PanelLawyers', 'Removed.PanelLawyers')

    # Remove Grp* columns
    d[, paste0(rep("Grp0",6), 1:6) := NULL]
    
    # Data type conversions
    cleanForFactor <- function(x) {
        key <- trimws(tolower(x))
        x[key %in% c('to clarify the name')] <- 'Unknown'
        x <- sub("\\s*-\\s*", "-", x, perl=TRUE)
        x <- sub("\\s*\\(\\s*sub division\\s*\\)\\s*", "", x, ignore.case = TRUE, perl=TRUE)
        x <- tools::toTitleCase(tolower(x))
        return(factor(x))
    }
    factorCols <- c('State', 'District')
    d[, (factorCols) := lapply(.SD, cleanForFactor), .SDcols=factorCols]

    cleanForBool <- function(x) {
        # TODO Consider specific processing for each column
        key <- trimws(tolower(x))
        x[key %in% c('yes','constituted')] <- TRUE
        x[key %in% c('no','not constituted')] <- FALSE
        return(as.logical(x))
    }
    boolCols <- c('IsSeparatePanels', 'IsMonComConstituted')
    d[, (boolCols) := lapply(.SD, cleanForBool), .SDcols=boolCols]

    cleanForInt <- function(x) {
        # TODO Consider specific processing for each column
        key <- trimws(tolower(x))
        x[key %in% c('nil','none','no complaints received','no such case','not appointed',
                     '2 cases are pending','pending',
                     'maintained by panel lawyer every day',
                     'constiuted','constituted',
                     'yes , enclosed',
                     'attached',
                     'services are taken as per requirement')] <- 0
        x[key %in% c('na','n.a','not applicable',
                     'no panel','not responded','no response',
                     'not provided','not available','not compiled','not counted',
                     'no record availbale','no record available','information not available',
                     'not known','not submitted by panel lawyer','not reported','report not received',
                     'does not arise','not constituted','no of staff provided',
                     'cumm response','cumm',
                     'infomation pertains to courts','info. pertains to courts',
                     'data not collated',
                     'no such data maintained','no maintained','not maintained','data not maintained',
                     'yes , enclosed','attached',
                     'to check','to check attachment')] <- NA
        x[key %in% c('in all the cases','all')] <- 999999 # placeholder: replace with actual number
        x[key %in% c('monthly report','monthly reports','monthly','regularly','yes',
                     'recvied through sdlscs','maintained by the panel lawyer ecah and every date')] <- 12
        x[key %in% c('bi-monthly meeting held','bi-monthly')] <- 6
        x[key %in% c('quarterly reports','quarterly','quaterly reports','quaterly',
                     'quarterly information','quaterly information')] <- 4
        x[key %in% c('r11 (1) of nalsa 2010-(267)')] <- 267
        x[key %in% c('no exclusive infrastructure','no separate staff',
                     'one long table and three chairs at nyaya seva sadan building')] <- 0
        x[key %in% c('the service of junior administrative assistant who attached to dlsas above is utilized')] <- 1
        
        expr <- paste0(gsub(".*?([0-9]+)[^0-9]*", "\\1+", x, perl=TRUE), "0")
        expr[expr=='NA0'] <- NA
        x <- sapply(sapply(expr, parse, file="", n=NULL), eval)
        names(x) <- NULL

        return(as.integer(x))
    }
    intCols <- colnames(d)[!colnames(d) %in% factorCols]
    d[, (intCols) := lapply(.SD, cleanForInt), .SDcols=intCols]
    
    # Column data to be derived from another column(s)
    d[LA.Appointed.Prison==999999, LA.Appointed.Prison:=LA.Apps.Rxd.Prison]
    d[LA.Appointed.Court==999999, LA.Appointed.Court:=LA.Apps.Rxd.Court]
    
    return(d)
}


read.Nalsa2011.Jail.xlsx <- function(fname) {
    nalsaColNames <- c('State', 'District', 
                       'JailType', 'JailName',
                       'WithClinic', 'ClinicDOB', 
                       'JailVisitingLawyers', 'PrisonerVolunteers', 'CommunityVolunteers')

    d <- data.table(read_excel(fname, 1, skip = 2, col_names = nalsaColNames))

    # Special treatment for date of form %d/%m/%Y (coerced above as integer)
    clinicDOB <- read_excel(fname, 1, skip = 2, col_names = nalsaColNames,
                            col_types = c(rep(c("text"),5), "date", rep(c("text"),3)))$ClinicDOB
    for (i in which(!is.na(clinicDOB))) {
        d$ClinicDOB[i] <- strftime(as.Date(clinicDOB[i], format="%Y-%m-%d"), format="%d-%m-%Y")
    }
    
    # Data type conversions
    cleanForDate <- function(x) {
        key <- trimws(tolower(x))
        x[key %in% c('in the year 2013')] <- "01-01-2013"
        x[key %in% c('june , 2012')] <- "01-06-2012"
        x[key %in% c('7--7-2015')] <- "07-07-2015"
        x[key %in% c('104-2015')] <- "01-01-2015"
        x[key %in% c('29-6-20151')] <- "29-06-2015"
        x[key %in% c('during july 2013')] <- "01-07-2013"
        x[key %in% c('24-10-2013,30-6-2016')] <- "24-10-2013"
        x[key %in% c('na','n.a','not applicable',
                     'not responded','no response',
                     'not provided','data is not available')] <- NA
        x <- gsub("/", "-", x, perl=TRUE)
        return(as.Date(x, "%d-%m-%Y"))
    }
    dateCols <- c('ClinicDOB')
    d[, (dateCols) := lapply(.SD, cleanForDate), .SDcols=dateCols]

    cleanForFactor <- function(x) {
        # TODO Consider specific processing for each column
        key <- trimws(tolower(x))
        x[key %in% c('not clear')] <- "Judicial Lockup" # based on JailName
        x[key %in% c('4 jails','no','na','nil')] <- NA
        x <- gsub(".*JUDICIAL LOCK UP RAMPUR.*", "Rampur and Kalpa", x, perl=TRUE)
        x <- gsub(".*CORRECTIONAL HOME.*", "Correctional Home", x, perl=TRUE)
        x <- gsub("\\.", " ", x, perl=TRUE)
        x <- gsub("\\s+", " ", x, perl=TRUE)
        x <- gsub("\\s*,\\s*", "-", x, perl=TRUE)
        x <- gsub("-$", "", x, perl=TRUE)
        x <- gsub("\\s*\\(.*\\)", "", x, perl=TRUE)
        x <- tools::toTitleCase(tolower(x)) # TODO Doesn't do it for single-letter words!
        x <- sub("c h ", "C H ", x, perl=TRUE)
        x <- sub("l n j p n-", "L N J P N-", x, perl=TRUE)
        x <- sub("j ", "J ", x, perl=TRUE)
        x <- sub("r ", "R ", x, perl=TRUE)
        x <- sub("m ", "M ", x, perl=TRUE)
        return(factor(x))
    }
    factorCols <- c('State', 'District', 'JailType', 'JailName')
    d[, (factorCols) := lapply(.SD, cleanForFactor), .SDcols=factorCols]

    cleanForBool <- function(x) {
        # TODO Consider specific processing for each column
        key <- trimws(tolower(x))
        x[key %in% c('yes','not functioning presently','4.0')] <- TRUE
        x[key %in% c(NA,'-','no','no response',
                     'not responded','not provided')] <- FALSE
        return(as.logical(x))
    }
    boolCols <- c('WithClinic')
    d[, (boolCols) := lapply(.SD, cleanForBool), .SDcols=boolCols]

    cleanForInt <- function(x) {
        # TODO Consider specific processing for each column
        key <- trimws(tolower(x))
        x[key %in% c('pa on rotation')] <- 1
        x[key %in% c('na','n.a','not applicable',
                     'not responded','no response',
                     'not provided')] <- NA
        
        return(as.integer(x))
    }
    intCols <- colnames(d)[!colnames(d) %in% c(dateCols, factorCols, boolCols)]
    d[, (intCols) := lapply(.SD, cleanForInt), .SDcols=intCols]

    return(d)
}


read.Nalsa2011.District.xlsx <- function(fname) {
    nalsaColNames <- c('State', 'District', 
                       'VisitsLawyers', 'TrainingsLawyers',
                       'DaysOfClinicByVolunteers', 
                       'TrainingsForPrisonerVolunteers', 'TrainingsForCommunityVolunteers',
                       'IsOkaySignboard')
    
    d <- data.table(read_excel(fname, 2, skip = 2, col_names = nalsaColNames))
    
    cleanForFactor <- function(x) {
        x <- tools::toTitleCase(tolower(x))
        return(factor(x))
    }
    factorCols <- c('State', 'District')
    d[, (factorCols) := lapply(.SD, cleanForFactor), .SDcols=factorCols]
    
    cleanForBool <- function(x) {
        # TODO Consider specific processing for each column
        key <- trimws(tolower(x))
        x[key %in% c('yes')] <- TRUE
        x[key %in% c(NA,'-','no','no response',
                     'not responded','not provided')] <- FALSE
        return(as.logical(x))
    }
    boolCols <- c('IsOkaySignboard')
    d[, (boolCols) := lapply(.SD, cleanForBool), .SDcols=boolCols]
    
    cleanForInt <- function(x) {
        # TODO Consider specific processing for each column
        key <- trimws(tolower(x))
        x[key %in% c('none','number of traning not organised')] <- 0
        x[key %in% c('90 days in female ward and 75 in male ward i.e. 165 days')] <- 165
        x[key %in% c('na','n.a','not applicable',
                     'to check attachment',
                     'not responded','no response',
                     'not provided')] <- NA

        # Remove numbering: 1. 2. 2). etc.
        x <- trimws(gsub(" [0-9]\\)?\\.", "", paste("", x), perl=TRUE))

        expr <- paste0(gsub(".*?([0-9]+)[^0-9]*", "\\1+", x, perl=TRUE), "0")
        expr[expr=='NA0'] <- NA
        x <- sapply(sapply(expr, parse, file="", n=NULL), eval)
        names(x) <- NULL

        return(as.integer(x))
    }
    intCols <- colnames(d)[!colnames(d) %in% c(factorCols, boolCols)]
    d[, (intCols) := lapply(.SD, cleanForInt), .SDcols=intCols]
}


saveAsCsv <- function(fname, d) {
    write.table(d, file = fname, sep = ",", col.names = NA, qmethod = "double")
}


getOutputCsvName <- function(infile, extra="") {
    base <- tools::file_path_sans_ext(infile)
    outfile <- paste0(base, extra, '.Clean.csv')
    return(outfile)
}


getCompleteCases <- function(d, cols) {
    d <- d[,cols]
    d <- d[complete.cases(d)] # remove NA values
    return(d)
}


showBasicNumbers <- function(d) {
    print(paste0("Number of variables: ", ncol(d)))
    print(paste0("Number of observations: ", nrow(d)))

    d <- d[complete.cases(d)]
    print(paste0("Number of observations with complete data: ", nrow(d)))

    print(paste0("Number of states: ", nlevels(d$State)))
}

nalsa2010 <- function() {
    # Read data, clean it and save cleaned data
    nalsa2010.fname <- "../data/NALSA-2010-Responses.xlsx"
    nalsa2010 <- read.Nalsa2010.xlsx(nalsa2010.fname)
    saveAsCsv(getOutputCsvName(nalsa2010.fname), nalsa2010)
    
    # Print some basic info
    print("[NALSA 2010]:")
    showBasicNumbers(nalsa2010)
}

nalsa2011 <- function() {
    # Read data, clean it and save cleaned data
    nalsa2011.fname <- "../data/NALSA-2011-Responses.xlsx"

    nalsa2011.jail <- read.Nalsa2011.Jail.xlsx(nalsa2011.fname)
    saveAsCsv(getOutputCsvName(nalsa2011.fname, ".Jail"), nalsa2011.jail)
    
    nalsa2011.district <- read.Nalsa2011.District.xlsx(nalsa2011.fname)
    saveAsCsv(getOutputCsvName(nalsa2011.fname, ".District"), nalsa2011.district)
    
    # Print some basic info
    print("[NALSA 2011 Jail-wise]:")
    showBasicNumbers(nalsa2011.jail)
    print("[NALSA 2011 District-wise]:")
    showBasicNumbers(nalsa2011.district)
}

main <- function() {
    nalsa2010()
    nalsa2011()
}


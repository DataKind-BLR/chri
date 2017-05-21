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


saveAsCsv <- function(fname, d) {
    write.table(d, file = fname, sep = ",", col.names = NA, qmethod = "double")
}


getOutputCsvName <- function(infile) {
    base <- tools::file_path_sans_ext("../data/NALSA-2010-Responses.xlsx")
    outfile <- paste0(base, '.Clean.csv')
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


main <- function() {
    # Read data, clean it and save cleaned data
    nalsa2010.fname <- "../data/NALSA-2010-Responses.xlsx"
    nalsa2010 <- read.Nalsa2010.xlsx(nalsa2010.fname)
    saveAsCsv(getOutputCsvName(nalsa2010.fname), nalsa2010)

    # Print some basic info
    showBasicNumbers(nalsa2010)
}


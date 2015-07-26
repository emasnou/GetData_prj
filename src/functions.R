## Arxiu de llibreria d'utilitats.

# Function getpath.
# Function setpath.
# Function ClearEnv.
#

##############
# Function getPath
##############
#' Returns the actual running path.
#' \code{getPath} Returns the actual running path.
#' 
#' This function Returns the actual running path .
#'  
#' 
#' @return Path name
#' 
 getPath = function()
 {
     dirpath = getwd()
     dirpath
 }

##############
# Function setPath
##############
#' Returns the actual running path.
#' \code{setPath} Returns the actual running path.
#' 
#' This function Returns the actual running path .
#'  
#' 
#' @return TRUE, FALSE
#' 
setPath = function(dirpath)
{
    setwd(dirpath)
}



##############
# Function CleanEnv
##############
#' Removes the environment vars.
#' \code{CleanEnv} Removes enivronment vars
#' 
#' This function Removes all the vars in the environment.
#'  
#' 
#' @return NA
#' 

CleanEnv = function()
{
    rm(list=ls())
}

##############
# Function DataExists2
##############
#' Checks for data file and downloads it if missing.
#' \code{DataExists2} Checks for data file and downloads it if missing or 
#' raises an error.
#' 
#' This function checks for data file in data/household_power_consumption.txt
#' If file does not exists and error is raised.
#'  
#' @param file: filename to check for or to name download.
#' @param url: Original url where we can download datafile
#' 
#' @return TRUE
#' 

DataExists2 <- function(file, url = NA ) 
{
    # Datafile and downloadable file are diferent in name.
    # one is compressed the other is a csv.
    file.compressed = paste(file,"bz2",sep=".")
    file.uncompressed = paste(getwd(),"data",file,sep="/")
    file.destination = paste(getwd(),"data",file.compressed,sep="/")
#    print ( file.destination )
    if ( ! file.exists(file.uncompressed) )
    {
        if ( ! is.na(url) )
        {
            download.file(
                url, 
                dest=file.destination,
                method="curl")
            library(R.utils)
            file.name = bunzip2(file.destination,
                                overwrite = TRUE, exdir = "data")
            print (file.name)
        }
        else
        {
            stop(" There is no data file nor url to download it from")
        }
    }
    
    TRUE
}

##############
# Function UnzipFile
##############
#' Checks for file and unzips it into data directory.
#' \code{UnzipFile} Checks for file and unzips it into data directory.
#' 
#' This function checks for data file in data directory.
#' Then unzips it
#'  
#' @param file: filename to check for and unzip.
#' 
#' @return TRUE or die
#' 


UnzipFile = function( file )
{
    file.destination = paste(getwd(),"data",file,sep="/")
    if ( file.exists(file.destination))
    {
        unzip(zipfile = file.destination,overwrite = TRUE,exdir = "data")
        TRUE
    }
    else 
    {
        stop ( "There is no datafile to uncompress")
        FALSE
    }
         
}
##############
# Function DataExists
##############
#' Checks for data file and downloads it if missing.
#' \code{DataExists} Checks for data file and downloads it if missing or 
#' raises an error.
#' 
#' This function checks for data file in data directory.
#' If file does not exists and error is raised.
#'  
#' @param file: filename to check for or to name download.
#' @param url: Original url where we can download datafile
#' 
#' @return TRUE or die
#' 


DataExists <- function(file, url = NA ) 
{
    file.destination = paste(getwd(),"data",file,sep="/")
    #    print ( file.destination )
    if ( ! file.exists(file.destination) )
    {
        if ( ! is.na(url) )
        {
            download.file(
                url, 
                dest=file.destination,
                method="curl")
            TRUE
        }
        else
        {
        # We cannot download the file there is no origin url.
            stop(" There is no data file nor url to download it from")
            FALSE
        }
    }
    else { TRUE }

}

##############
# Function LoadData
##############
#' Loads the data from datafile into a dataframe.
#' \code{LoadData} Loads the data from datafile into a dataframe.
#' 
#' This function loads the data contained in datafile and returns a dataframe with all the information.
#' 
#'  @note: Needed to create a procés to check file typa and uncompress acoordingly
#'  @note: Need to check and manage more than one file into the compresses file.
#' 
#' @return Dataframe with data contained in file.
#' 

LoadData <- function(filename)
{
    file.path = paste(getwd(),"data",filename,sep="/")
    
    DF = read.csv(file.path,header = TRUE, comment.char = "#")
    DF
}

WriteSpecs = function()
{
    file.path = paste(getwd(),"doc","SYSTEM_SPECS",sep="/")
    out<-capture.output(sessionInfo())
    cat(out,file=file.path,sep="\n",append=FALSE)
}



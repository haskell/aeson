#!/usr/bin/env Rscript

args<-commandArgs(TRUE)

library(jsonlite)

jsonData <- fromJSON(args[1])

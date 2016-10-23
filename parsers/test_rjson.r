#!/usr/bin/env Rscript

args<-commandArgs(TRUE)

library(rjson) # > install.packages("rjson")

json_data <- fromJSON(file=args[1])

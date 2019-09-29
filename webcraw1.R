#Write a tipical crawler using the information provided below:

##Start your code with these libraries:
library(RCurl)
library(XML)
library(stringr)
library(httr)

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
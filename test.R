#!/usr/bin/env Rscript


# Algorithm function
process_results = function(args) {
	values = unlist(strsplit(args, ','))
	result = length(values)
}

# Get data values from command line
args = commandArgs(trailingOnly=TRUE)

# Process the data
result = process_results(args)

# Print the result, without newlines or a count
cat(result)



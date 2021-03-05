## Define some miscellaneous functions to help with manipulating and viewing
## dataframes and plots

## Define a function that extracts some columns from a dataframe.
EdR.extract <- function(dataf, timecols, valcols) {
    #Start to form the result
    temp1 <- dataf[timecols]                # Extract the time columns from the input data frame
    result <- temp1                         # Make a copy of the time column data frame
    curr_valcol <- as.character(valcols[1]) # Get the name of the first value column
    result$value <- dataf[,curr_valcol]     # Get the values from the column
    result$value_type <- curr_valcol        # Store the name of the value column

    #Add the rest of the value columns
    if(length(valcols)>1) {
        for(i in 2:length(valcols)) {
            temp2 <- temp1                          # Make a copy of the time column data frame
            curr_valcol <- as.character(valcols[i]) # Get the name of the current value column
            temp2$value <- dataf[,curr_valcol]      # Get the values from the column
            temp2$value_type <- curr_valcol         # Store the name of the value column
            result <- rbind(result, temp2)          # Add this to the result
        }
    }

    #Return the result
    return(result)
}

## Define a function that adds a descriptive column to each dataframe in the
## input list and then merges them
EdR.merge <- function(dfs, vs, newcolname) {
    #Check the input lengths and types as much as possible
    stopifnot(
        is.list(dfs) && length(dfs) > 1,
        is.list(vs) && length(vs) == length(dfs),
        is.character(newcolname) && length(newcolname) == 1
    )

    #Start to form the result
    result <- as.data.frame(dfs[1])
    curr_v <- as.character(vs[1])
    result[,newcolname] = curr_v

    # Add the rest of the data frames with their appropriate values
    for(i in 2:length(dfs)) {
        curr_df <- as.data.frame(dfs[i])
        curr_v <- as.character(vs[i])
        curr_df[,newcolname] = curr_v
        result <- rbind(result, curr_df)
    }

    # Return the result
    return(result)
}

## Define a function that either saves a plot object to a PDF file or prints it
## in a new window. WARNING: save_to_pdf = FALSE will not work on MacOS since
## x11() is only available on Windows.
EdR.plot <- function(
    plot_object,
    save_to_pdf,
    file_name_string,
    width,
    height
)
{
    if (save_to_pdf) {
        pdf(
            file=file_name_string,
            width=width,
            height=height,
            useDingbats=FALSE
        )
        print(plot_object)
        dev.off()
    }
    else {
        x11(width=width, height=height)
        print(plot_object)
    }
}

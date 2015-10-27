#
# Functions for comparing rows
#

"compareDFrows" <- function(df1, df2, label1, label2){

  if (missing(label1)) {
    label1 <- "source"
  }
  if (missing(label2)) {
    label2 <- "target"
  }
  
  df1$Origin <- label1
  df2$Origin <- label2

  df <- rbind(df1, df2)  # Stick them together
  df <- df[, names(df) != "Value"]
  
  # Find the rows which have duplicates in a different group.
  dupRows <- dupsBetweenGroups(df,"Origin")
  
  # Look at duplicated Rows
  df_unique = cbind(df, unique=!dupRows)
  df_unique = df_unique[df_unique["unique"] == TRUE, ]
  
  return(df_unique)
  
}

#' Utility function called by compareDFrows
#' 
dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # =================== Replace NA's with previous non-NA value =====================
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx) + 1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}


#
# Functions for comparing columns 
#

#' Compare Output Entry Structure column wise
#'
"compareOutputEntryCols" <- function(sourceOutputEntry, targetOutputEntry, threshold=0.001, entryPoint='TrialAnalysis') {

    if (entryPoint == 'TrialAnalysis') {
      # Format columns to a common type 
      sourceOutputEntry <- OutputEntryPostProcess(sourceOutputEntry)
      targetOutputEntry <- OutputEntryPostProcess(targetOutputEntry)
    } else if (entryPoint == 'CombinedAnalysis') {
      # Format columns to a common type 
      sourceOutputEntry <- CombinedAnalysisOutputEntryPostProcess(sourceOutputEntry)
      targetOutputEntry <- CombinedAnalysisOutputEntryPostProcess(targetOutputEntry)
    }


    col.names <- names(sourceOutputEntry)

    output.results = vector('list', length(col.names))

    for (i in seq(1, ncol(sourceOutputEntry))) {

       A = sourceOutputEntry[, i]
       B = targetOutputEntry[, i]
       result1 <- all.equal(A, B)
       result2 <- allequalWithRelativeDifference(A, B, threshold)

       if (result1 == TRUE) {
          result = result1
        } else {
          result = result2
        }

      # Edge case for Rank Column. When numbers are very near zero the results of the rank 
      # column differ due to numerical precision in R and Splus. This check reruns the rank
      # function on the desired outputs of R and S and checks they are the same.     
      if (col.names[i] == "RANK") {

        traits = unique(targetOutputEntry[, 1])
        n.traits = length(traits) 
        
        A.subsets = vector('list', n.traits)
        B.subsets = vector('list', n.traits)
        
        for (j in seq(along=traits)) {
          
          A.subset.idx = sourceOutputEntry[, 1] == traits[j]
          B.subset.idx = targetOutputEntry[, 1] == traits[j]
          
          A = sourceOutputEntry[A.subset.idx, 7]
          B = targetOutputEntry[B.subset.idx, 7]
          
          A.subsets[[j]] = zapsmall(A, digits=10)
          B.subsets[[j]] = zapsmall(B, digits=10)
        }
        
        A.ranks = unlist(lapply(A.subsets, rank))
        B.ranks = unlist(lapply(B.subsets, rank))

        result <- all.equal(A.ranks, B.ranks)
      }

      # if (result == FALSE) {
      #   print(paste("Fail in column identity check:", col.names[i]))
      # }
      output.results[[i]] = result
    }
    return(output.results)
}

#' Compare Output Trial structure columnwise 
#'
"compareOutputTrialCols" <- function(sourceOutputTrial, targetOutputTrial, threshold=0.001, entryPoint='TrialAnalysis') {

    if (entryPoint == 'TrialAnalysis') {
      # Format columns to a common type 
      sourceOutputTrial <- OutputTrialPostProcess(sourceOutputTrial)
      targetOutputTrial <- OutputTrialPostProcess(targetOutputTrial)
    } else if (entryPoint == 'CombinedAnalysis') {
      # Format columns to a common type 
      sourceOutputTrial <- CombinedAnalysisOutputTrialPostProcess(sourceOutputTrial)
      targetOutputTrial <- CombinedAnalysisOutputTrialPostProcess(targetOutputTrial)
    }

    col.names <- names(sourceOutputTrial)

    output.results = vector('list', length(col.names))

    for (i in seq(1, ncol(sourceOutputTrial))) {
      
      A = sourceOutputTrial[, i]
      B = targetOutputTrial[, i]
      result1 <- all.equal(A, B)
      result2 <- allequalWithRelativeDifference(A, B, threshold)
      
      if (all(result1 == TRUE)) {
        result = result1
      } else {
        result = result2
      }
      
      # if (result == FALSE) {
      #   #print(paste("Fail in column identity check:", col.names[i]))
      # }
      output.results[[i]] = result
    }
    return(output.results)
}

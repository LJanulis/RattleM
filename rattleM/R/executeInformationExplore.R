executeExploreInformation <- function(dataset){
	
	if(is.null(dataset)){
		errorDialog(Rtxt("No dataset has been loaded"))
		return()
	}
	targ <- theWidget("information_combobox")$getActiveText()
	crs$informations$values <- mutualInformationsExecute(crs$dataset, targ)
}

mutualInformationsExecute <- function(frame, targetColName){
  
  TV <- "information_textview"
  
  analyzedCol <- grep(targetColName, colnames(frame))
  columnIds <- setdiff(1:ncol(frame), analyzedCol)
  columnNames <- setdiff(colnames(frame), names(frame)[analyzedCol])
  
  frequencies <- vector(mode = "list", length = ncol(frame) - 1)
  
  graphColNames <- vector(mode = "character", length = ncol(frame) - 1)
  
  for(i in 1:length(columnIds)){
      frequencies[[i]] <- table(frame[[columnIds[i]]], frame[[analyzedCol]])
      graphColNames[i] <- paste(columnNames[i], "&")
      graphColNames[i] <- paste(graphColNames[i], names(frame)[analyzedCol])
  }

  mutInfos <- vector(mode = "numeric", length = ncol(frame) - 1)
  
  for(i in 1:length(frequencies)){
    mutInfos[i] <- round(mi.empirical(frequencies[[i]], unit = "log2"), 7)
  }
  sortedInfos <- sort(mutInfos, decreasing = TRUE)
  
  output <- character()
  for(i in 1:length(sortedInfos)){
    for(j in 1:length(mutInfos)){
      if(sortedInfos[i] == mutInfos[j]){
        info <- paste(":", mutInfos[j])
        line <- paste("Information of:", paste(graphColNames[match(mutInfos[j], mutInfos)], info))
        output <- paste(paste(output, line), "\n\n")
      }
    }
  }

  minVal <- min(mutInfos)
  maxVal <- max(mutInfos)
  
  minInfo <- paste(graphColNames[match(minVal, mutInfos)], minVal)
  maxInfo <- paste(graphColNames[match(maxVal, mutInfos)], maxVal)
  
  setTextview(TV, Rtxt("Mutual Informations:"), "\n\n",
				  output, "\n\n",
				  paste("Minimum mutual information:", minInfo), "\n\n",
				  paste("Maximum mutual information:", maxInfo), "\n\n")
  
  
  crs$informations$graphColNames <- graphColNames
  
  return(mutInfos)
}

mutualInformationsPlot <- function(){
  targ <- theWidget("information_combobox")$getActiveText()
  graphTitle <- paste("Mutual information of dataset variables with variable:", targ)
  m <- list(l = 60, r = 60, b = 150, t = 50, pad = 4)
  y <- list(title = "Information")
  graph <- (plot_ly(y = crs$informations$values, x = crs$informations$graphColNames, name = "Mutual Informations", type = "bar") %>%
           layout(title = graphTitle, yaxis = y, margin = m) %>%
           add_annotations(text = crs$informations$values, textangle = 320, showarrow = FALSE, yshift = 30))
  print(graph)
}

on_information_plotbutton_clicked <- function(button){
	mutualInformationsPlot()
}

setUpInformationTab2 <- function(new.dataset=TRUE){

  cboxInfo <- theWidget("information_combobox")
  cboxInfo$setSensitive(TRUE)
  if(new.dataset){
	catVars <- getCategoricVariables(type="names", include.target=TRUE)
	numVars <- getNumericVariables(type="names")
	if(length(catVars)){
		cboxInfo$getModel()$clear()
		cboxInfo$appendText(" ")
		lapply(catVars, cboxInfo$appendText)
		lapply(numVars, cboxInfo$appendText)
	}
  }
}
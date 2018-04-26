runJ48 <- function(classVar, dataset, U_val = FALSE, O_val = FALSE, C_val = 0.25, M_val = 2, 
                    R_val = FALSE, N_val = 3, B_val = FALSE, S_val = FALSE, J_val = TRUE)
{
  fmula <- as.formula(paste(as.character(classVar), "~."))
  if(U_val == TRUE){
    mJ48 <- J48(fmula, data = dataset, 
                control = Weka_control(U = U_val, O = O_val, M = M_val, B = B_val, J = J_val))
    return(mJ48)
  }
  else{
    if(R_val == TRUE){
      mJ48 <- J48(fmula, data = dataset, 
                  control = Weka_control(U = U_val, O = O_val, M = M_val, B = B_val, S = S_val, J = J_val, N = N_val, R = R_val))
      return(mJ48)
    }
    else{
      mJ48 <- J48(fmula, data = dataset, 
                  control = Weka_control(U = U_val, O = O_val, C = C_val, M = M_val, B = B_val, S = S_val, J = J_val))
      return(mJ48)
    }
  }
}

executeModelJ48 <- function()
{
	resetTextview("dectrees_textview")
	crs$j48$pruningConfidence <- 0.25
	crs$j48$folds <- 3

	crs$j48$unpruned <- theWidget("use_unpruned_checkbutton")$getActive()
	crs$j48$uncollapsed <- theWidget("dont_collapse_checkbutton")$getActive()
	crs$j48$binSplit <- theWidget("binary_splits_checkbutton")$getActive()
	crs$j48$mdl <- theWidget("mdl_checkbutton")$getActive()
	crs$j48$reducedError <- theWidget("reduced_error_pruning_checkbutton")$getActive()
	crs$j48$subtreeRaising <- theWidget("raise_subtree_checkbutton")$getActive()
	crs$j48$minBucket <- theWidget("min_instances_input")$getValue()

	if(theWidget("pruning_confidence_input")$getSensitive() == TRUE)
		crs$j48$pruningConfidence <- theWidget("pruning_confidence_input")$getValue()
	if(theWidget("folds_input")$getSensitive() == TRUE)
		crs$j48$folds <- theWidget("folds_input")$getValue()
		
	logData();

	trainDataset <- crs$dataset[crs$train, c(crs$input, crs$target)]
	crs$j48$fit <- runJ48(classVar = as.character(crs$target), dataset = trainDataset, U_val = crs$j48$unpruned,
																				  O_val = crs$j48$uncollapsed,
																				  C_val = crs$j48$pruningConfidence,
																				  M_val = crs$j48$minBucket,
																				  R_val = crs$j48$reducedError,
																				  N_val = crs$j48$folds,
																				  B_val = crs$j48$binSplit,
																				  S_val = crs$j48$subtreeRaising,
																				  J_val = crs$j48$mdl)
																	 
	j48SummaryStr <- "summary(crs$j48$fit)"
	j48FitStr <- "crs$j48$fit"	

	TV <- "dectrees_textview"
	
	setTextview(TV, Rtxt("J48 decision tree output:"), "\n\n",
            paste(capture.output(eval(parse(text=j48FitStr))), collapse="\n"),
            "\n\n",
			Rtxt("Summary of the created J48 decision tree:"), "\n\n",
			paste(capture.output(eval(parse(text=j48SummaryStr))), collapse="\n"), 
			"\n\n")
			
	theWidget("rpart_rules_button")$hide()
	theWidget("rpart_plot_button")$show()
	
	theWidget("evaluate_j48_checkbutton")$setActive(TRUE)
	theWidget("evaluate_j48_checkbutton")$setSensitive(TRUE)
	
	
	return(TRUE)
}

evaluateIfJ48Selected <- function(mtypes, testset0, testset, predcmd, respcmd, probcmd){
  if(crv$J48 %in% mtypes){
	testset[[crv$J48]] <- testset0
	predcmd[[crv$J48]] <- sprintf("crs$pr <- predict(crs$j48$fit, newdata=%s)",
                                   testset[[crv$J48]])
	respcmd[[crv$J48]] <- predcmd[[crv$J48]]
	probcmd[[crv$J48]] <- predcmd[[crv$J48]]
  }
  return(list("testset" = testset, "predcmd" = predcmd, "respcmd" = respcmd, "probcmd" = probcmd))
}

logData <- function(){

	 appendLog("Running J48 with parameters:")
	 appendLog(Rtxt("Unpruned:"), crs$j48$unpruned, "\n\n",
			   Rtxt("Uncollapsed:"),crs$j48$uncollapsed, "\n\n",
		       Rtxt("PruningConfidence:"), crs$j48$pruningConfidence,"\n\n",
			   Rtxt("MinBucket:"),crs$j48$minBucket,"\n\n",
			   Rtxt("ReducedError:"), crs$j48$reducedError,"\n\n",
			   Rtxt("Folds:"), crs$j48$folds,"\n\n",
			   Rtxt("BinSplit:"),crs$j48$binSplit,"\n\n",
			   Rtxt("SubtreeRaising:"),crs$j48$subtreeRaising,"\n\n",
			   Rtxt("MDL:"), crs$j48$mdl, "\n\n")

}

calculateDepth <- function(ruleString){
  return(sum(gregexpr("|", ruleString, fixed=TRUE)[[1]] > 0))
}

isNewRule <- function(ruleString){
  return(!grepl(ruleString, "|"))
}
isRuleEnd <- function(ruleString){
  lastchar <- substr(ruleString, nchar(ruleString), nchar(ruleString) + 1)
  return(lastchar == ')')
}


jripToRules <- function(fit){
  treeText <- capture.output(print(fit))
  treeText <- treeText[-1]
  treeText <- treeText[-1]
  treeText <- treeText[-1]
  treeText <- treeText[-length(treeText)]
  treeText <- treeText[-length(treeText)]
  treeText <- treeText[-length(treeText)]
  treeText <- treeText[-length(treeText)]
  treeText <- treeText[-length(treeText)]
  
  rules <- list()
  currRule <- ""
  usedRuleDepths <- c()
  newRuleStartIndex <- 1
  
  for(i in 1:length(treeText)){
    if(isRuleEnd(treeText[[i]])){
      currRule <- treeText[[i]]
      currRule <- gsub(":", " =>", currRule)
      usedRuleDepths[length(usedRuleDepths) + 1] <- calculateDepth(treeText[[i]])
      
      for(j in i:newRuleStartIndex){
        if(!isRuleEnd(treeText[[j]]) && !isNewRule(treeText[[j]])){
          
          currDepth <- calculateDepth(treeText[[j]])
          
          if(!(currDepth %in% usedRuleDepths)){
            usedRuleDepths[length(usedRuleDepths) + 1] <- currDepth
            currRule <- paste(treeText[[j]], currRule)
          }
        }
        else if(isNewRule(treeText[[j]])){
          newRuleStartIndex <- j
          currRule <- paste(treeText[[j]], currRule)
          rules[[length(rules)+1]] <- currRule
          
          currRule <- ""
          usedRuleDepths <- c()
          break
        }
      }
    }
  }
  
  for(i in 1:length(rules)){
    rules[[i]] <- gsub("\\|", " ", rules[[i]])
    rules[[i]] <- gsub("     +", " AND ", rules[[i]])
    ruleSplit <- strsplit(rules[[i]], " AND ")
    for(j in 2:length(ruleSplit[[1]])-1){
      #print(ruleSplit[[1]][j])
    }
    consequentSplit <- strsplit(ruleSplit[[1]][length(ruleSplit[[1]])], "=>")
    #print(consequentSplit[[1]][1])
    #print(paste("             =>", consequentSplit[[1]][2]))
    #print("")
  }
}




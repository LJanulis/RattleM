executeModelC50 <- function(){
	resetTextview("dectrees_textview")
	
	crs$c50$subset <- theWidget("c50_subset_checkbutton")$getActive()
	crs$c50$rules <- theWidget("c50_rules_checkbutton")$getActive()
	crs$c50$bands <- theWidget("c50_bands_input")$getValue()
	crs$c50$winnow <- theWidget("c50_winnow_checkbutton")$getActive()
	crs$c50$noGlobalPruning <- theWidget("c50_noglobalpruning_checkbutton")$getActive()
	crs$c50$fuzzyThreshold <- theWidget("c50_fuzzythreshold_checkbutton")$getActive()
	crs$c50$sample <- theWidget("c50_sample_input")$getValue()
	crs$c50$earlyStopping <- theWidget("c50_earlystopping_checkbutton")$getActive()
	crs$c50$pruningConfidence <- theWidget("pruning_confidence_input")$getValue()
	crs$c50$minBucket <- theWidget("min_instances_input")$getValue()
	crs$c50$boostRuns <- theWidget("c50_boosting_input")$getValue()
	
	
	crs$c50$trainDataset <- crs$dataset[crs$train, c(crs$input, crs$target)]
	crs$c50$classColumnIndex <- which(colnames(crs$c50$trainDataset)==crs$target)
	crs$c50$classColumn <- crs$c50$trainDataset[[crs$c50$classColumnIndex]]	
	
	crs$c50$controlSettingsC50 <- C5.0Control(subset = crs$c50$subset, winnow = crs$c50$winnow, 
									  noGlobalPruning = crs$c50$noGlobalPruning, CF = crs$c50$pruningConfidence,
									  minCases = crs$c50$minBucket, fuzzyThreshold = crs$c50$fuzzyThreshold, 
									  sample = crs$c50$sample, earlyStopping = crs$c50$earlyStopping)								  								  
	crs$c50$fit <- C5.0(x = crs$c50$trainDataset[,-crs$c50$classColumnIndex], y = crs$c50$classColumn, 
						control = crs$c50$controlSettingsC50, trials = crs$c50$boostRuns)
	
	C50SummaryStr <- "summary(crs$c50$fit)"
	C50FitStr <- "crs$c50$fit"
	
	TV <- "dectrees_textview"
	
	setTextview(TV, Rtxt("C5.0 decision tree output:"), "\n\n",
		paste(capture.output(eval(parse(text=C50FitStr))), collapse="\n"),
		"\n\n",
		Rtxt("Summary of the created C5.0 decision tree:"), "\n\n",
		paste(capture.output(eval(parse(text=C50SummaryStr))), collapse="\n"), 
		"\n\n")
		
	logC50Data();
		
	if(crs$c50$rules){
		crs$c50$controlSettingsC50Rules <- c(crs$c50$controlSettingsC50, crs$c50$bands)
		crs$c50$rulesList <- C5.0(x = crs$c50$trainDataset[,-crs$c50$classColumnIndex], y = crs$c50$classColumn, 
		                          rules = TRUE, control = crs$c50$controlSettingsC50Rules, trials = crs$c50$boostRuns)
		rulesListTxt <- "crs$c50$rulesList"
		rulesListSummary <- "summary(crs$c50$rulesList)"
		appendTextview(TV, paste(capture.output(eval(parse(text=rulesListTxt))), collapse="\n"), "\n\n")
		appendTextview(TV, Rtxt("C50 classification rule summary"), "\n\n",
					paste(capture.output(eval(parse(text=rulesListSummary))), collapse="\n"), "\n\n")
	}	
	
	theWidget("evaluate_c50_checkbutton")$setActive(TRUE)
	theWidget("evaluate_c50_checkbutton")$setSensitive(TRUE)
	
	return(TRUE)
}

evaluateIfC50Selected <- function(mtypes, testset0, testset, predcmd, respcmd, probcmd){
  if(crv$C50 %in% mtypes){
	testset[[crv$C50]] <- testset0
	predcmd[[crv$C50]] <- sprintf("crs$pr <- predict.C5.0(crs$c50$fit, newdata=%s)",
                                   testset[[crv$C50]])
	respcmd[[crv$C50]] <- predcmd[[crv$C50]]
	probcmd[[crv$C50]] <- predcmd[[crv$C50]]
  }
  return(list("testset" = testset, "predcmd" = predcmd, "respcmd" = respcmd, "probcmd" = probcmd))
}

logC50Data <- function(){

	 appendLog("Running J48 with parameters:")
	 appendLog(Rtxt("Subset:"), crs$c50$subset, "\n\n",
			   Rtxt("Rules:"),crs$c50$rules, "\n\n",
		       Rtxt("Bands:"), crs$c50$bands,"\n\n",
			   Rtxt("Winnow:"),crs$c50$winnow,"\n\n",
			   Rtxt("NoGlobalPruning:"), crs$c50$noGlobalPruning,"\n\n",
			   Rtxt("FuzzyThreshold:"), crs$c50$fuzzyThreshold,"\n\n",
			   Rtxt("Sample:"),crs$c50$sample,"\n\n",
			   Rtxt("EarlyStopping:"),crs$c50$earlyStopping,"\n\n",
			   Rtxt("PruningConfidence:"), crs$c50$pruningConfidence, "\n\n",
			   Rtxt("MinBucket:"), crs$c50$minBucket, "\n\n",
			   Rtxt("Boosting:"), crs$c50$boostRuns, "\n\n"
			   )

}

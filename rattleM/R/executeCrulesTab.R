runJrip <- function(classVar, dataset, F_val = 3, N_val = 2.0, O_val = 2, E_val = FALSE, P_val = FALSE){
 
   fmula <- as.formula(paste(as.character(classVar), "~."))
   mRip <- JRip(fmula, data = dataset, control = Weka_control(F = F_val, N = N_val, O = O_val,
                                                                 E = E_val, P = P_val))
  return(mRip)
}

executeCrulesTab <- function()
{
	resetTextview("crules_textview")
	crs$crules$Mfolds <- theWidget("crules_folds_input")$getValue()
	crs$crules$Mweight <- theWidget("crules_weights_input")$getValue()
	crs$crules$Mruns <- theWidget("crules_runs_input")$getValue()
	
	crs$crules$MusePruning <- theWidget("crules_usepruning_checkbox")$getActive()
	crs$crules$McheckError <- theWidget("crules_checkerror_check")$getActive()
	
	#trainDataset <- crs$dataset[crs$train,]
	trainDataset <- crs$dataset[crs$train, c(crs$input, crs$target)]
	crs$crules$fit <- runJrip(classVar = as.character(crs$target), dataset = trainDataset, F_val = crs$crules$Mfolds, 
																						   N_val = crs$crules$Mweight,
 																						   O_val = crs$crules$Mruns,
																						   E_val = !crs$crules$MusePruning, 
																						   P_val = !crs$crules$McheckError)
	rSummary <- "summary(crs$crules$fit)"
	mRules <- "crs$crules$fit"
	
	TV <- "crules_textview"
	resetTextview(TV)
    setTextview(TV, Rtxt("JRip Classification Rules:"), "\n\n",
              paste(capture.output(eval(parse(text=mRules))), collapse="\n"),
              "\n\n",
			  Rtxt("Summary of the Classification Rules:"), "\n\n",
			  paste(capture.output(eval(parse(text=rSummary))), collapse="\n"), 
			  "\n\n")
	
	theWidget("evaluate_crules_checkbutton")$setActive(TRUE)
	theWidget("evaluate_crules_checkbutton")$setSensitive(TRUE)
}

runEvalRip <- function(foldsVal, seedVal){
	evalDataset <- crs$dataset[crs$validate, c(crs$input, crs$target)]
	evalMRip <- evaluate_Weka_classifier(crs$crules$fit, newdata = evalDataset, numFolds = foldsVal, seed = seedVal, class = TRUE)
	return(evalMRip)
}

evaluateRMdl <- function(){
	#evaluating model on validation dataset
	crs$crules$Efolds <- theWidget("crules_evalfolds_input")$getValue()
	crs$crules$Eseed <- theWidget("crules_evalseed_input")$getValue()
	
	crs$crules$eFit <- runEvalRip(foldsVal = crs$crules$Efolds, seedVal = crs$crules$Eseed)
	evalTxt <- "crs$crules$eFit"
	
	TV <- "crules_textview"
	appendTextview(TV, paste(capture.output(eval(parse(text=evalTxt))), collapse="\n"), "\n\n")
}

on_crules_evalseed_freqbutton_clicked <-  function(action, window)
{
  evaluateRMdl()
}

evaluateIfCrulesSelected <- function(mtypes, testset0, testset, predcmd, respcmd, probcmd){
  if (crv$CRULES %in%  mtypes){
	testset[[crv$CRULES]] <- testset0
	predcmd[[crv$CRULES]] <- sprintf("crs$pr <- predict(crs$crules$fit, newdata=%s)",
                                   testset[[crv$CRULES]])
	respcmd[[crv$CRULES]] <- predcmd[[crv$CRULES]]
	probcmd[[crv$CRULES]] <- predcmd[[crv$CRULES]]
  }
  return(list("testset" = testset, "predcmd" = predcmd, "respcmd" = respcmd, "probcmd" = probcmd))
}

initCrulesNotebook <- function(){
	crv$NOTEBOOK.CRULES.NAME    <- Rtxt("ClassRules")
	crv$NOTEBOOK.CRULES.WIDGET <- theWidget("crules_tab_widget")
	crv$NOTEBOOK.CRULES.LABEL  <- theWidget("crules_tab_label")
}

setUpCrulesInterface <- function(){
	wid <- theWidget("crules_folds_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 1000, 1, 5, 0)
	wid$setAdjustment(nad)
	wid$setValue(3)

	wid <- theWidget("crules_weights_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 10000, 0.1, 0.5, 0)
	wid$setAdjustment(nad)
	wid$setValue(2.0)

	wid <- theWidget("crules_runs_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 1000000, 1, 10, 0)
	wid$setAdjustment(nad)
	wid$setValue(2)

	wid <- theWidget("crules_evalfolds_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 1000, 1, 5, 0)
	wid$setAdjustment(nad)
	wid$setValue(3)

	wid <- theWidget("crules_evalseed_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 1000000, 1, 10, 0)
	wid$setAdjustment(nad)
	wid$setValue(1)
}

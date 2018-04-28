executeDecTreesTab <- function(){
	if(theWidget("model_tree_j48_radiobutton")$getActive())
	{
		executeModelJ48()
	}
	else
	{
		executeModelC50()
	}
}

on_model_tree_drawbutton_clicked <- function(button){
	if(theWidget("model_tree_j48_radiobutton")$getActive())
	{
		if(!is.null(crs$j48$fit)){
			tmpfile <- base::tempfile()
			RWeka::write_to_dot(crs$j48$fit, tmpfile)
			Rgraphviz::plot(Rgraphviz::agread(tmpfile))
			base::unlink(tmpfile, recursive = TRUE)
		}
		else{
			errorDialog("No C4.5 model has been built!")
		}
	}
	else{
		if(!is.null(crs$c50$fit)){
			plot(crs$c50$fit)
		}
		else{
			errorDialog("No C5.0 model has been built!")
		}
	}
}

initDecTreesNotebook <- function(){
  crv$NOTEBOOK.DECTREES.NAME    <- Rtxt("DecTrees")
  crv$NOTEBOOK.DECTREES.WIDGET <- theWidget("dectrees_tab_widget")
  crv$NOTEBOOK.DECTREES.LABEL  <- theWidget("dectrees_tab_label")
}

setUpJ48Interface <- function(){
	theWidget("use_unpruned_checkbutton")$setSensitive(TRUE)
	theWidget("dont_collapse_checkbutton")$setSensitive(TRUE)
	theWidget("binary_splits_checkbutton")$setSensitive(TRUE)
	theWidget("mdl_checkbutton")$setSensitive(TRUE)
	theWidget("raise_subtree_checkbutton")$setSensitive(TRUE)
	theWidget("pruning_confidence_label")$setSensitive(TRUE)
	theWidget("pruning_confidence_input")$setSensitive(TRUE)
	theWidget("min_instances_label")$setSensitive(TRUE)
	theWidget("min_instances_input")$setSensitive(TRUE)
	theWidget("reduced_error_pruning_checkbutton")$setSensitive(TRUE)
	theWidget("folds_label")$setSensitive(FALSE)
	theWidget("folds_input")$setSensitive(FALSE)


	wid <- theWidget("min_instances_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 1000, 1, 5, 0)
	wid$setAdjustment(nad)
	wid$setValue(2)

	wid <- theWidget("pruning_confidence_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 10000, 0.1, 0.5, 0)
	wid$setAdjustment(nad)
	wid$setValue(0.25)

	wid <- theWidget("folds_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 1000, 1, 5, 0)
	wid$setAdjustment(nad)
	wid$setValue(3)
}

setUpC50Interface <- function(){
	#Disabling J48 buttons, since C50 is open initially
	theWidget("use_unpruned_checkbutton")$setSensitive(FALSE)
	theWidget("dont_collapse_checkbutton")$setSensitive(FALSE)
	theWidget("binary_splits_checkbutton")$setSensitive(FALSE)
	theWidget("mdl_checkbutton")$setSensitive(FALSE)
	theWidget("reduced_error_pruning_checkbutton")$setSensitive(FALSE)
	theWidget("raise_subtree_checkbutton")$setSensitive(FALSE)
	theWidget("folds_input")$setSensitive(FALSE)
	  
	wid <- theWidget("c50_sample_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 10000, 0.1, 0.5, 0)
	wid$setAdjustment(nad)
	wid$setValue(0)

	wid <- theWidget("c50_bands_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 1000, 1, 5, 0)
	wid$setAdjustment(nad)
	wid$setValue(0)

	wid <- theWidget("c50_boosting_input")
	nad <- RGtk2::gtkAdjustmentNew(NULL, 0, 10000, 1, 5, 0)
	wid$setAdjustment(nad)
	wid$setValue(1)
}

activateJ48Buttons <- function(){
  theWidget("use_unpruned_checkbutton")$setSensitive(TRUE)
  theWidget("dont_collapse_checkbutton")$setSensitive(TRUE)
  theWidget("binary_splits_checkbutton")$setSensitive(TRUE)
  theWidget("mdl_checkbutton")$setSensitive(TRUE)
  theWidget("raise_subtree_checkbutton")$setSensitive(TRUE)
  theWidget("pruning_confidence_label")$setSensitive(TRUE)
  theWidget("pruning_confidence_input")$setSensitive(TRUE)
  theWidget("min_instances_label")$setSensitive(TRUE)
  theWidget("min_instances_input")$setSensitive(TRUE)
  theWidget("reduced_error_pruning_checkbutton")$setSensitive(TRUE)
  theWidget("folds_label")$setSensitive(FALSE)
  theWidget("folds_input")$setSensitive(FALSE)
}

on_j48_REP_checkbutton_clicked <- function(button){
#If toggled enable n and disable c
  if (theWidget("reduced_error_pruning_checkbutton")$getActive() == TRUE){
    theWidget("folds_label")$setSensitive(TRUE)
	theWidget("folds_input")$setSensitive(TRUE)
	theWidget("pruning_confidence_label")$setSensitive(FALSE)
	theWidget("pruning_confidence_input")$setSensitive(FALSE)
	theWidget("use_unpruned_checkbutton")$setSensitive(FALSE)
  }
  else{
	theWidget("folds_label")$setSensitive(FALSE)
	theWidget("folds_input")$setSensitive(FALSE)
	theWidget("pruning_confidence_label")$setSensitive(TRUE)
	theWidget("pruning_confidence_input")$setSensitive(TRUE)
	theWidget("use_unpruned_checkbutton")$setSensitive(TRUE)
  }
}

on_use_unpruned_checkbutton_clicked <- function(button){
#If togglet raising, pruning confidence and REP can't be enabled
	if(theWidget("use_unpruned_checkbutton")$getActive() == TRUE){
		theWidget("raise_subtree_checkbutton")$setSensitive(FALSE)
		theWidget("pruning_confidence_input")$setSensitive(FALSE)
		theWidget("reduced_error_pruning_checkbutton")$setSensitive(FALSE)
	}
	else{
		theWidget("raise_subtree_checkbutton")$setSensitive(TRUE)
		theWidget("pruning_confidence_input")$setSensitive(TRUE)
		theWidget("reduced_error_pruning_checkbutton")$setSensitive(TRUE)
	}
}

on_model_tree_j48_radiobutton_toggled <- function(button){
	theWidget("c50_subset_checkbutton")$setSensitive(FALSE)
	#theWidget("c50_rules_checkbutton")$setSensitive(FALSE)
	theWidget("c50_bands_input")$setSensitive(FALSE)
	theWidget("c50_winnow_checkbutton")$setSensitive(FALSE)
	theWidget("c50_noglobalpruning_checkbutton")$setSensitive(FALSE)
	theWidget("c50_fuzzythreshold_checkbutton")$setSensitive(FALSE)
	theWidget("c50_sample_label")$setSensitive(FALSE)
	theWidget("c50_sample_input")$setSensitive(FALSE)
	theWidget("c50_boosting_label")$setSensitive(FALSE)
	theWidget("c50_boosting_input")$setSensitive(FALSE)
	
	theWidget("c50_earlystopping_checkbutton")$setSensitive(FALSE)
	
	activateJ48Buttons();
}

on_model_tree_C50_radiobutton_toggled <- function(button){
	theWidget("c50_subset_checkbutton")$setSensitive(TRUE)
	#theWidget("c50_rules_checkbutton")$setSensitive(TRUE)
	theWidget("c50_bands_input")$setSensitive(TRUE)
	theWidget("c50_winnow_checkbutton")$setSensitive(TRUE)
	theWidget("c50_noglobalpruning_checkbutton")$setSensitive(TRUE)
	theWidget("c50_fuzzythreshold_checkbutton")$setSensitive(TRUE)
	theWidget("c50_sample_label")$setSensitive(TRUE)
	theWidget("c50_sample_input")$setSensitive(TRUE)
	theWidget("c50_earlystopping_checkbutton")$setSensitive(TRUE)
	theWidget("pruning_confidence_label")$setSensitive(TRUE)
	theWidget("pruning_confidence_input")$setSensitive(TRUE)
	theWidget("c50_boosting_label")$setSensitive(TRUE)
	theWidget("c50_boosting_input")$setSensitive(TRUE)
	
	theWidget("use_unpruned_checkbutton")$setSensitive(FALSE)
	theWidget("dont_collapse_checkbutton")$setSensitive(FALSE)
	theWidget("binary_splits_checkbutton")$setSensitive(FALSE)
	theWidget("mdl_checkbutton")$setSensitive(FALSE)
	theWidget("reduced_error_pruning_checkbutton")$setSensitive(FALSE)
	theWidget("raise_subtree_checkbutton")$setSensitive(FALSE)
	theWidget("folds_label")$setSensitive(FALSE)
	theWidget("folds_input")$setSensitive(FALSE)
}


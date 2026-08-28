#############################################################
### function remef (REMove EFfects)
# remove random factors variance and fixed effects from the dependent variable of an LMM analysis

# by Sven Hohenstein, Reinhold Kliegl, 2011, 2012, 2013

# v0.6.9, July 2013

## VERSION HISTORY:
# changes to last version (v0.6.8, June 2013):
# - function now works with glmerMod models created with glmer
# v0.6.7, December 2012:
# - function now uses the new lme4 function for 'lmerMod' objects
#   (lme4Eigen is no longer used)
# - the plot parameter is deprecated
# - minor code changes
# v0.6.6, February 2012:
# - the argument "all" (string) for the parameter 'ran'
#   selects all random effects
# v0.6.5, February 2012:
# - the function now works with objects created by the library
#  	lme4Eigen too
# changes to last version (v0.6.2, August 2011):
# - former parameter 'link' is now called 'family'
# - family parameter is specified as object (not string),
#	like, e.g., in lmer()
# - functions are called from the correct package
# - new parameter 'plot': if TRUE, a plot of the uncorrected
# 	and corrected dependent variable is displayed
# v0.6.2 August 2011
# - a bug occuring when NULL was part of the random effects list
#   was removed
# v0.6, June 2011
# - a bug was removed
# v0.54, June 2011:
# - new parameter 'link' specifying the logit link function; either
#   "identity" or "logit"
# - redundant entries are removed from the vectors in the list 'ran'
# v0.54, June 2011:
# - Fixed effects (in the vector 'fix') can be integers or strings
#	(e.g., c(2:4, "Effect1", 6, "Effect2") )
# v0.52, May 2011
# - Correction: If grouping was TRUE and a specified fixed main effect of a
#     factor was not present in any interaction, the function failed
# v0.51, May 2011:
# - parameter 'grouping' also works if keep = FALSE, if grouping is TRUE,
#	the effect and all adssociated ones of higher order are chosen to be
#	removed
# v0.5, May 2011:
# - new boolean parameter: 'grouping'; if both keep and grouping are TRUE,
#	the effects in fix and all subeffects are chosen to be kept
#	Note: In the present version, if keep is FALSE, grouping will be ignored
# v0.4a, May 2011:
# - if keep is TRUE, the to be kept effects are not removed from the
#	dependent variable but are actually added to the residuals
# - more efficient processing and less operations (if keep is set properly)
# v0.4, April 2011:
# - new parameter 'keep' allows the remove the effects not specified
# - more efficient processing
# v0.32, April 2011:
# - Correction: If random effects of random factors with order numbers < 1
#				were specified, the output was not correct.
# v0.3a, April 2011:
# changes to last version (v0.3):
# - more efficient processing
# v0.3, April 2011:
# changes to last version (v0.2):
# - now it is possible to remove all random effects (even random slopes)
# - the parameter 'ran' must be a list of vectors
# - switched order position of parameters 'fix' and 'ran'
# v0.2, February 2011:
# changes to last version (v0.1):
# - Correction: If just one fixed effect was specified, the function failed.
#				Now it is possible to specify a single number as parameter 'fix'


## INPUT:
# model:	an object of class 'mer' (lme4 packge), 'lmerMod' (lme4Eigen package)
# ran:		list of vectors of natural numbers according to random effects of the model;
#			the maximum length of this list is the number of random factors;
#			each list member includes the numbers of the random effects (of the current random factor)
#			(e.g., list(1:2, NULL, c(1, 3)) )
#			default value: NULL (no random-factor related variance is removed)
# fix:		vector of natural numbers or strings according to fixed effects of the model
#			(e.g., c(2:4, 6, 8:10), c("Effect1", "Effect2"), or c(2:4, "Effect1", 6, "Effect2"))
#			Vectors can consist of both integers and strings (R will transform all intergers to
#			strings if at least one string is entered);
#			redundant values will be ignored (one effect can't be removed more than once)
#			default value: NULL (no fixed effects are removed)
# keep:		logical value; if TRUE, the specified effects are not removed but kept
#			(and all other effects are removed)
# grouping:	logical value; if FALSE the specified effects in fix are used;
#			if TRUE, effects are grouped:
#				- if keep=TRUE, the specified effect (and all effects associated with the
#					variables) as well as all effects of lower order consisting solitary of
#					a subset of the variables of the specified effet are kept
#					(e.g., if the interaction A:B:C is specified, the following effects are
#					chosen:	A:B:C, A:B, A:C, B:C, A, B, and C)
#				- if keep=FALSE, the specified effect (and all effects associated with the
#					variables) as well as all effects of higher order consisting of
#					all variables of the specified effet (and other variables) are removed
#					(e.g., if the interaction A:B:C is specified, the following effects are
#					chosen:	A:B:C and A:B:C:x)
# family: a family object including a link function; 
#         either "identity" (default) or "logit" link function; this must be the
#         link function which was used for generating the model;
#       - gaussian(link = "identity") is the right option for most purposes, e.g. Gaussian
#         distributions; the dependent variable is not transformed
#       - binomial(link = "logit") is used for binomial dependent variables; the output vector
#         will contain probabilities which can be rounded to obtain data in the
#         original metric (zeroes and ones); note that even if keep=FALSE, the
#         base for the calculation are not the input values but the residuals

## OUTPUT:	a numerical vector

## How does it work?
# Random factor variance and fixed effects are removed from the dependent variable of an LMM analysis.
# The order of effects in the fixed-effects input vector is the same as in the model output.
# The returned vector includes the dependent variable corrected by the specified effects.
# Note. The currect version of this function works for the "identity" and "logit" model link functions
# only.

remef <- function(model, fix = NULL, ran = NULL, keep = FALSE, grouping = FALSE, family = gaussian(link = "identity"), plot = FALSE) {
  # name of link function
  mclass <- class(model)
  sup_classes <- c("mer", "lmerMod", "glmerMod")	# supported object classes
  if (!mclass %in% c(sup_classes)) stop("This class is not supported yet.")
  fixef.fnc <- lme4::fixef
  ranef.fnc <- lme4::ranef
  moma <- model.matrix(model)
  	# model matrix
  if (is.function(family)) family <- family()
  link <- family$link
  if (!(link %in% c("identity", "logit"))) stop("Unknown link function specified.")
  if (keep || link == "logit") {
  	#DV <- lme4::residuals(model)			# use residuals as base and add effects
  	DV <- residuals(model)
  } else {
  	# use actual data as base and remove effects (this is not possible with the logit link function)
  	DV <- model@frame[ , 1]
  }	
  fix <- unique(fix)				# remove redundant values
  if (any(is.character(fix))) {		# strings were entered as fixed effects
  	suppressWarnings( fix.num <- as.numeric(fix) )
  		# fix.num, numerical fix vector
  	ef.names.idx <- is.na(fix.num)
  		# logical vector indicating the positions of effect names in the fix vector
  	for (ef in fix[ef.names.idx]) {
  		if(!any(ef == names(fixef.fnc(model)))) stop(paste("The fixed effect", ef, "is not present in the model."))
  			# stops the function if any fixed effect string is not present in the model
  	}	
  	fix.num[ef.names.idx] <- which( names(fixef.fnc(model)) %in% fix[ef.names.idx] )
  	fix <- unique(fix.num)
  }
  if (grouping & length(fix) > 0) {			
  	new.fix <- NULL				# help variable for the construction of a new 'fix' vector
  	for (ef in fix) {			# choose the specified effects and all lower-order/higher-order effects of the relevant variables
  		# var.str <- attr(lme4::model.matrix(model), "assign")
  		var.str <- attr(moma, "assign")
  			# var.str, structure of the variables
  		if (!var.str[ef]) {		# the specified effect is the intercept
  			new.fix <- unique(c(new.fix, ef))
  		} else {				# the specified effect is NOT the intercept
  			var.mat <- attr(terms(model), "factors")
  				# var.mat, variable matrix
  			ef.order <- attr(terms(model), "order")
  				# ef.order, order of effects (0: intercept, 1: main effect, 2: two-factor interaction, etc.)
  			if (keep) {			# choose specified effect an all associated ones of lower order
  				ass.ef <- which( var.str %in% which( colSums( matrix( var.mat[ var.mat[ , var.str[ef] ] == 1, ] , ncol = length(ef.order)) ) == ef.order ) )
  					# ass.ef, effects associated with the input effect (integer(s))
  			} else {			# choose specified effect an all associated ones of higher order
  				ass.ef <- which( var.str %in% which( colSums( matrix( var.mat[ var.mat[ , var.str[ef] ] == 1, ] , ncol = length(ef.order)) ) == ef.order[var.str[ef]] ) )
  			}	
  			new.fix <- unique(c(new.fix, ass.ef))
  		}  		
  	}	  
  fix <- new.fix				  		
  }
  if (!keep && link == "logit") { # keep effects!
  	fix <- setdiff(seq_along(fixef.fnc(model)), fix)
  }  	
  # remove random factor variance
  if (identical(ran, "all")) {
    ran <- lapply(ranef(model), seq_along)    
  }
  if (length(ran) > 0) {
  	rf_before.end.at <- -1
	# for (rf in 1 : length(ran)) {			
    for (rf in seq_along(ranef.fnc(model))) {	# rf, random factor
  		if (rf > length(ran)) {				# non-specified random factors
  			if (!keep && link == "logit") { # keep effects!
  				ran[[rf]] <- seq.int(ncol(ranef.fnc(model)[[rf]]))
  			} else {
  				ran[[rf]] <- NULL
  			}		
  		} else {  			
  			if(!is.null(ran[[rf]])) ran[[rf]] <- unique(ran[[rf]])
  			if (!keep && link == "logit") { # keep effects!
  				ran[[rf]] <- setdiff(seq.int(ncol(ranef.fnc(model)[[rf]])), ran[[rf]])
  			} 
  		}
  		n.rf.levels	<- nrow(ranef.fnc(model)[[rf]])
			# n.rf.levels, number of random-factor levels
      	for (re in ran[[rf]]) {				# re, random effect (e.g., intercept, slope)
        	idx.re <- ( ((re - 1) * n.rf.levels + 1) : (re * n.rf.levels) ) + (rf_before.end.at + 1)
          		#idx.re, numbers of lines of the (transpose) random effect matrix for the currect random effect
        	if (mclass == "lmerMod") re.matrix <- model@pp$Zt else re.matrix <- model@Zt        	  
        	DV <- DV + sign((keep || link == "logit") - 0.5) * ( as.vector( ranef.fnc(model)[[rf]][ , re] %*% re.matrix[idx.re, ] ) )
          		# Note. re.matrix is the transpose sparse random-effect matrix (class dgCMatrix)
				# Note. sign(keep - 0.5) is +1 if keep==TRUE and -1 otherwise
		}
      rf_before.end.at <- rf_before.end.at + prod(dim(ranef.fnc(model)[[rf]]))
        # at which line of the random-effect matrix did the current random effect end?
  	}
  }
  # remove fixed effects  
  DV <- DV + sign((keep || link == "logit") - 0.5) * ( matrix(moma[ , fix], nrow = length(DV)) %*% fixef.fnc(model)[fix] )
  if (link == "logit") DV <- ( 1 / (1 + exp(- DV)) )
  	# if DV is rounded, zeros and ones will result
  if (plot) {
    warning("The 'plot' parameter is deprecated.")
  }	
  return(as.vector(DV))	
}

#############################################################

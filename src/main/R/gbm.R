# Bonsai
# Copyright (C) 2012 eBay Software Foundation
 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

require(gbm)
require(rjson)
require(plyr)


gbm.model.used.variables <- function(object, trees=object$n.trees)
{
  varIndexes <- unique(do.call(c, lapply(object$trees, function(x) unique(x[[1]]))))
                                        # pick only valid indexes and add one since the gbm object uses the
  varIndexes <- varIndexes[varIndexes>=0] + 1
  object$var.names[varIndexes]
}

# from rjson, updated to fine-control precision of output numbers
toJSON.better <- function (x)
{
    if (is.factor(x) == TRUE) {
        tmp_names <- names(x)
        x = as.character(x)
        names(x) <- tmp_names
    }
    if (!is.vector(x) && !is.null(x) && !is.list(x)) {
        x <- as.list(x)
        warning("JSON only supports vectors and lists - But I'll try anyways")
    }
    if (is.null(x))
        return("null")
    if (is.null(names(x)) == FALSE) {
        x <- as.list(x)
    }
    if (is.list(x) && !is.null(names(x))) {
        if (any(duplicated(names(x))))
            stop("A JSON list must have unique names")
        str = "{"
        first_elem = TRUE
        for (n in names(x)) {
            if (first_elem)
                first_elem = FALSE
            else str = paste(str, ",", sep = "")
            str = paste(str, deparse(n), ":", toJSON.better(x[[n]]),
                sep = "")
        }
        str = paste(str, "}", sep = "")
        return(str)
    }
    if (length(x) != 1 || is.list(x)) {
        if (!is.null(names(x)))
            return(toJSON.better(as.list(x)))
        str = "["
        first_elem = TRUE
        for (val in x) {
            if (first_elem)
                first_elem = FALSE
            else str = paste(str, ",", sep = "")
            str = paste(str, toJSON.better(val), sep = "")
        }
        str = paste(str, "]", sep = "")
        return(str)
    }
    if (is.nan(x))
        return("\"NaN\"")
    if (is.na(x))
        return("\"NA\"")
    if (is.infinite(x))
        return(ifelse(x == Inf, "\"Inf\"", "\"-Inf\""))
    if (is.logical(x))
        return(ifelse(x, "true", "false"))
    if (is.character(x))
        return(gsub("\\/", "\\\\/", deparse(x)))
    if (is.numeric(x))
        return(paste(x))
        #return(format(x,digits=15,scientific=0))
    stop("shouldnt make it here - unhandled type not caught")
}

gbm.model.json <- function(object, trees=object$n.trees, name="", ...)
{
  usedVariables <- gbm.model.used.variables(object, trees)
  jsonTree <- vector("list", trees)
  for (tree in 1:trees) {
    jsonTree[tree] <- gbm.tree.json(object, i.tree=tree, trees=trees)
  }

  toJSON.better(list(name=name,
              ...,
              bag.fraction=object$bag.fraction,
              distribution= object$distribution$name,
              interaction.depth=object$interaction.depth,
              n.minobsinnode=object$n.minobsinnode,
              n.trees=trees,
              shrinkage=object$shrinkage,
              classes=object$classes,
              factors = usedVariables,
              trees=jsonTree))
}

gbm.tree.json <- function (model, i.tree = 1, trees=model$trees)
{
  if ((i.tree < 1) || (i.tree > length(model$trees)))
    stop("i.tree is out of range. Must be less than ", length(model$trees))
  else {
    tdata <- model$trees[[i.tree]]
    names(tdata) <- c("SplitVar", "SplitCodePred", "LeftNode",
                     "RightNode", "MissingNode", "ErrorReduction", "Weight",
                     "Prediction")
    tdata$LeftNode <- tdata$LeftNode + 1
    tdata$RightNode <- tdata$RightNode + 1
    tdata$MissingNode <- tdata$MissingNode + 1
    tdata$SplitVar <- tdata$SplitVar + 1

    # transpose the list of lists (we cannot use data.frame because we need SplitCodePred to be list for categorical variables)
    n <- length(tdata[[1]]) # assuming all lists in tdata have the same length
    nodes <- lapply(1:n, function(i) lapply(tdata, "[[", i))

    # extend definition of the node
    extnodes <- lapply(nodes, function(node) {
      if (node$SplitVar >= 1) {
        # split node
        node$SplitVarName <- model$var.names[node$SplitVar]
        if (model$var.type[node$SplitVar] > 0) {
          # categorical variable
          node$Categorical <- TRUE
          c.splits <- model$c.splits[[node$SplitCodePred+1]] # -1 (L) or 1 (R) for each level
          if (!all(c.splits %in% c(-1,1)))
            stop("Unexpected value in c.splits: ", paste(c.splits,collapse=","))
          var.levels <- model$var.levels[[node$SplitVar]] # actual level labels for this variable
          node$SplitCodePred <- as.list(var.levels[which(c.splits==-1)]) # category labels for left tree, enforce list (even when single element)
        }
      } else {
        # terminal node
        if (i.tree == 1) # need to add offset to first tree terminals
          node$SplitCodePred <- node$SplitCodePred + model$initF
      }
      node
    })
    list(tree=node.to.json(extnodes, 1))
  }
}

node.to.json <- function(nodes, nodeidx)
{
  node <- nodes[[nodeidx]]
  if (!is.null(node$SplitVarName)) {
    list(cond=list(var=node$SplitVarName,op=ifelse(is.null(node$Categorical),"<","in"), val=node$SplitCodePred),
         if_true = node.to.json(nodes, node$LeftNode),
         if_false = node.to.json(nodes, node$RightNode),
         if_missing = node.to.json(nodes, node$MissingNode))
  } else { # terminal node
    list(prediction=node$SplitCodePred)
  }
}

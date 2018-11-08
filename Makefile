########################################################################
#
# Makefile for rain pre-built ML model
#
########################################################################

# List the files to be included in the .mlm package.

MODEL_FILES = 				\
	train.R				\
	configure.R			\
	demo.R 				\
	print.R				\
	display.R			\
	score.R				\
	README.txt			\
	DESCRIPTION.yaml		\
	$(MODEL)_rpart_model.RData	\

# Include standard Makefile templates.

include ../git.mk
include ../pandoc.mk
include ../mlhub.mk

$(MODEL)_rpart_model.RData: train.R
	Rscript $<

clean::
	rm -rf README.txt dtree.pdf varimp.pdf

realclean:: clean
	rm -rf $(MODEL)_*.mlm $(MODEL)_rpart_model.RData

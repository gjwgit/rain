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

INC_BASE    = $(HOME)/.local/share/make
INC_PANDOC  = $(INC_BASE)/pandoc.mk
INC_GIT     = $(INC_BASE)/git.mk
INC_MLHUB   = $(INC_BASE)/mlhub.mk
INC_CLEAN   = $(INC_BASE)/clean.mk

ifneq ("$(wildcard $(INC_PANDOC))","")
  include $(INC_PANDOC)
endif
ifneq ("$(wildcard $(INC_GIT))","")
  include $(INC_GIT)
endif
ifneq ("$(wildcard $(INC_MLHUB))","")
  include $(INC_MLHUB)
endif
ifneq ("$(wildcard $(INC_CLEAN))","")
  include $(INC_CLEAN)
endif




$(MODEL)_rpart_model.RData: train.R
	Rscript $<

clean::
	rm -rf README.txt dtree.pdf varimp.pdf

realclean:: clean
	rm -rf $(MODEL)_*.mlm $(MODEL)_rpart_model.RData
	rm -f  	rpart_riskchart.pdf 		\
		rpart_model.pdf			\


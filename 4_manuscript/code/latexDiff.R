# compare the differences bewteen tow tex files to get a marked up pdf using latexdiffr.

#another option is to use online software https://3142.nl/latex-diff/ . It will out put a tex file that you can enter into overleaf. 

#for instructions: https://github.com/hughjonesd/latexdiffr
# need to have latexdiff installed. Instructions are on the above github link

# another o[tion is to ]
library(latexdiffr)

latexdiff(
  path1 = "book/Casey_RETN_manuscript_OGsubmission.tex",
  path2 = "book/Casey_RETN_manuscript.tex",
  clean=FALSE, # to keep the text file
  output = "book/diff"
  #ld_opts = '--add-to-config "VERBATIMENV=Highlighting"'
  # ld_opts = '--add-to-config "LATEX="lualatex\""'
)

# This will result in an error becouse the frontspec package requires either XeTeX. No worries. While a marked up pdf wasn't produced, we can use the
# difference tex file to produce a marked up pdf in overleaf. '


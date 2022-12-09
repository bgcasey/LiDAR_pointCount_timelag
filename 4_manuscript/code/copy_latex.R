file.copy(from="book/Casey_RETN_manuscript.tex", to="../documents/manuscripts/casey_MDPI", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="figures/", to="../documents/manuscripts/casey_MDPI/", 
          overwrite = TRUE, recursive = TRUE, 
          copy.mode = TRUE)

file.copy(from="../../../zotero/library1.bib", to="library.bib", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="library.bib", to="../documents/manuscripts/casey_MDPI/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Definitions/", to="../documents/manuscripts/casey_MDPI/", 
          overwrite = TRUE, recursive = TRUE, 
          copy.mode = TRUE)

file.copy(from="Casey_RETN_manuscript.log", to="../documents/manuscripts/casey_MDPI/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)


file.copy(from="book/diff.tex", to="../documents/manuscripts/casey_MDPI", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

#compress folder for submission
# files2zip <- dir("../documents/manuscripts/casey_MDPI/", full.names = TRUE)
# zip("../documents/manuscripts/casey_MDPI.zip", files=files2zip)


#(cd /Volumes/GoogleDrive/My\ Drive/PhD/thesis/chapter_3/documents/manuscripts/casey_MDPI && luatex Casey_RETN_manuscript.tex)


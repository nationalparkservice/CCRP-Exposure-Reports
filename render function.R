# Rendering rmarkdown Exposure Reports

# Choice of parks
choices <- c("VOYA")


# Function

render_one <- function(name) {
  rmarkdown::render(
    'CCExposure.Rmd',
    output_file = paste0(name, ' Climate Futures Summary.docx'),
    params = list(name = name),
    envir = parent.frame()
  )
}


# Iterate

for (name in choices) {
  render_one(name)
}






# Rendering rmarkdown Exposure Reports

# Choice of parks
choices <- c("FOUN", "TUZI")


# Function

render_one <- function(name) {
  rmarkdown::render(
    'CCExposureReport.Rmd',
    output_file = paste0('Climate Change Exposure - ', name, '.docx'),
    params = list(name = name),
    envir = parent.frame()
  )
}


# Iterate

for (name in choices) {
  render_one(name)
}






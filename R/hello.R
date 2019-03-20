# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("waRRior is up and running")
  message(crayon::red(getURL("https://gist.githubusercontent.com/joelgsponer/ab38b2122fb45e9aa934/raw/290d64c02ace9893771199bcc023738cebdc4dbe/donQuixote",ssl.verifypeer=FALSE)))
}

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
Tinsel::run_app()

#The downloaded source packages are in
#‘C:\Users\ptx4\AppData\Local\Temp\1\RtmpIvq1X5\downloaded_packages’






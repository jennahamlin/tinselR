# Set options here TRUE = production mode, FALSE = development mode
options(golem.app.prod = FALSE)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()


# Document and reload your package
golem::document_and_reload()

# Run the application
Tinsel::run_app()
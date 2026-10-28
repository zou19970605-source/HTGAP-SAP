Sys.setenv(HTGAP_INPUT_DIR=getOption('htgap.input'),HTGAP_OUTPUT_DIR=getOption('htgap.output'))
status<-system2(Sys.getenv('HTGAP_PYTHON','python'),c(shQuote(code_file('machine_learning','compose_figures.py')),'7'))
if(status!=0)stop('Machine-learning figure could not be drawn.')

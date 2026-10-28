if (.Platform$OS.type == 'windows') suppressWarnings(Sys.setlocale('LC_CTYPE','.UTF8'))
args <- commandArgs(trailingOnly=TRUE)
if (!length(args)) stop('Use Rscript run.R analysis, selection, Figure_1, or Figure_S1.')
script <- sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE)[1])
root <- dirname(normalizePath(script, mustWork=TRUE))
input <- normalizePath(Sys.getenv('HTGAP_INPUT_DIR',file.path(root,'private')),mustWork=TRUE)
output <- Sys.getenv('HTGAP_OUTPUT_DIR',file.path(root,'results'))
dir.create(output,recursive=TRUE,showWarnings=FALSE)
output <- normalizePath(output,mustWork=TRUE)
options(htgap.code=root,htgap.input=input,htgap.output=output)
source(file.path(root,'config.R'),encoding='UTF-8')
setwd(output)
for (d in c('analysis','data_private','figures','figures/detail_panels','source_data','models')) dir.create(d,recursive=TRUE,showWarnings=FALSE)
task <- args[1]
if (task == 'impute') {
  source_code('R','impute.R')
} else if (task == 'selection') {
  source_code('R','select_variables.R')
  source_code('R','prepare_selection_plot.R')
} else if (task == 'analysis') {
  source_code('R','fit_nomogram.R')
  source_code('R','derive_score.R')
  source_code('R','spline_tests.R')
} else if (task == 'machine_learning') {
  source_code('machine_learning','train_models.R')
} else {
  if (!grepl('^Figure_(S([1-9]|1[0-3])|[1-4])$',task)) stop('Unknown figure name.')
  folder <- if(grepl('^Figure_S',task)) 'supplementary' else 'figures'
  source_code(folder,paste0(task,'.R'))
}

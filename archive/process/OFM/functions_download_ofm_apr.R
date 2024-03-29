library(here)
library(stringr)

download_ofm_apr1_est <- function(dataset, ...) {

   # This function will download either post-censal or intercensal April 1 data from OFM. 
   # Intercensal data requires a starting and ending census year because of how the filename is constructed.
   # File will download to the current working directory

  root_url <-  'https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1'
  data_dir <-  'data'
  kwargs <- list(...)
  
  if(dataset == 'post') {
    if('posttype' %in% names(kwargs)){
      if(kwargs['posttype'] == 'population') {
        
        filename <- 'ofm_april1_population_final.xlsx'
        dls <-  file.path(root_url, filename)
        dest <-  here(data_dir, filename)
        
      } else if (kwargs['posttype'] == 'housing') {
        
        filename <- 'ofm_april1_housing.xlsx'
        dls <-  file.path(root_url, filename)
        dest <-  here(data_dir, filename)
        
      } else {
        stop("You're missing a posttype keyword argument. Indicate whether posttype is population or housing.\n")
      }
    }
    download.file(dls, dest, mode = 'wb')
    
  } else if(dataset == 'inter') {
    if(('start' %in% names(kwargs)) && ('end' %in% names(kwargs))) {
      
      # variations of intercensal filenames
      filename <- paste0('ofm_april1_intercensal_estimates_', kwargs['start'], '-', kwargs['end'], '.xlsx')
      filenames <- c(filename, str_replace_all(filename, '-', '_'))
      
      for (file in filenames) {
        dls <-  file.path(root_url, 'hseries', file)
        dest <-  here(data_dir, file)
        tryCatch(download.file(dls, dest, mode = "wb", quiet = FALSE),
                 error = function(e) print(paste(file, 'did not download, trying next filename')))
      }
      
    } else {
      stop("You're missing either start or end (or both) keyword arguments.\n")
    }
    
  } else {
    stop("Use either 'post' or 'inter' for dataset argument\n")
  }
}

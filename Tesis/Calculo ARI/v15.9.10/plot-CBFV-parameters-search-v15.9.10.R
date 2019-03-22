

# Change accordingly
VERSION <- "v15.9.10"
WORK.DIR <- file.path("", "research", "Memoristas", VERSION)


CARSIG.SCRIPT.BASENAME <- paste("carSignal", VERSION, sep = "-")
CARSIG.SCRIPT.BASENAME <- paste(CARSIG.SCRIPT.BASENAME, "R", sep = ".")
CARSIG.SCRIPT.NAME <- file.path(WORK.DIR, CARSIG.SCRIPT.BASENAME)
source(CARSIG.SCRIPT.NAME)

METRIC.UTILS.SCRIPT.BASENAME <-
  paste("Metric", "utils", VERSION, sep = "-")
METRIC.UTILS.SCRIPT.BASENAME <-
  paste(METRIC.UTILS.SCRIPT.BASENAME, "R", sep = ".")
METRIC.UTILS.SCRIPT.NAME <-
  file.path(WORK.DIR, METRIC.UTILS.SCRIPT.BASENAME)
source(METRIC.UTILS.SCRIPT.NAME)

MFARI.SCRIPT.BASENAME <- paste("mfARI", VERSION, sep = "-")
MFARI.SCRIPT.BASENAME <- paste(MFARI.SCRIPT.BASENAME, "R", sep = ".")
MFARI.SCRIPT.NAME <- file.path(WORK.DIR, MFARI.SCRIPT.BASENAME)
source(MFARI.SCRIPT.NAME)

ROUND.UTILS.SCRIPT.BASENAME <-
  paste("Rounding", "utils", VERSION, sep = "-")
ROUND.UTILS.SCRIPT.BASENAME <-
  paste(ROUND.UTILS.SCRIPT.BASENAME, "R", sep = ".")
ROUND.UTILS.SCRIPT.NAME <- 
  file.path(WORK.DIR, ROUND.UTILS.SCRIPT.BASENAME)
source(ROUND.UTILS.SCRIPT.NAME)



plot.manoeuvre.signals <- function(
      tgt.plot.filename,
      tgt.plot.id,
      time.instants,
      abp.signal,
      cbfv.signal,
      sampling.time,
      time.release,
      ...
    )
{  
  mfari <- get.mfARI.parameters(
    time.instants = time.instants,
    ABP.signal = abp.signal,
    CBFV.signal = cbfv.signal,
    sampling.time = sampling.time,
    time.release = time.release,
    keep.details = TRUE,
    keep.search.results = TRUE,
    add.search.plots = TRUE,
    ...
  )
  
  search.plots.params <-
    c(mfari[["search.plots"]], list(nrow = 2, ncol = 2, top = tgt.plot.id))
  search.plots <- do.call(marrangeGrob, search.plots.params)
  
  ggsave(
    filename = tgt.plot.filename,
    plot = search.plots,
    width = 11, 
    height = 8,
    units = "in"
  )
}


plot.subject.signals <- function(
      subject,
      src.subject.dir,
      src.ext = "txt",
      header = TRUE,
      tgt.subject.dir,
      tgt.suffix,
      tgt.ext = "txt",
      overwrite = FALSE,
      manoeuvres,
      time.col.name = "Time",
      abp.col.name = "ABP",
      left.cbfv.col.name = "LCBFV",
      right.cbfv.col.name = "RCBFV",
      sampling.time = 0.2,
      time.release = 0,
      left.plot.suffix = "Izq",
      right.plot.suffix = "Der",
      indent = "",
      ...
      )
{
  plot.created <- FALSE
  next.indent <- paste0(indent , "  ")
  
  for(mvre in manoeuvres)
  {
    cat(indent, "-- ", mvre, " --\n", sep = "")
    
    # Sets the subject source file
    src.basename <- paste(subject, mvre, sep = "-")
    src.basename <- paste(src.basename, src.ext, sep = ".")
    src.filename <- file.path(src.subject.dir, src.basename)
    
    if(!file.exists(src.filename))
    {
      cat(
        next.indent,
        "Warning: file ",
        src.filename,
        " does not exists\n",
        sep = ""
      )
      next
    }
    
    # Reads the manoeuvre signals
    mvre.data <- read.table(src.filename, header = header)
    
    # Sets plot titles
    mvre.id <- paste(subject, mvre, sep = ", ")
    
    #
    # Creates the plot for the left hemisphere
    #
    
    cat(next.indent, "-- Left hemisphere --\n", sep = "")
    
    # Sets target plot filename
    tgt.basename <-
      paste(subject, mvre, tgt.suffix, left.plot.suffix, sep = "-")
    tgt.basename <- paste(tgt.basename, tgt.ext, sep = ".")
    tgt.filename <- file.path(tgt.subject.dir, tgt.basename)
    
    # If the target plot does not exists or it should not be overwritten
    if(any(!file.exists(tgt.filename), overwrite))
    {
      # Makes sure the target directory exists
      dir.create(
        path = tgt.subject.dir,
        showWarnings = FALSE,
        recursive = TRUE,
        mode = "0711"
      )
      
      plot.manoeuvre.signals(
        tgt.plot.filename = tgt.filename,
        tgt.plot.id = paste(mvre.id, left.plot.suffix, sep = ", "),
        time.instants = mvre.data[[time.col.name]],
        abp.signal = mvre.data[[abp.col.name]],
        cbfv.signal = mvre.data[[left.cbfv.col.name]],
        sampling.time = sampling.time,
        time.release = time.release,
        ...
      )
      plot.created <- TRUE
    }
    else
      cat(
        next.indent,
        "Warning: target plot already exist and not overwritten\n",
        sep = ""
      )
    
    
    #
    # Creates the plot for the right hemisphere
    #
    
    cat(next.indent, "-- Right hemisphere --\n", sep = "")
    
    # Sets target plot filename
    tgt.basename <-
      paste(subject, mvre, tgt.suffix, right.plot.suffix, sep = "-")
    tgt.basename <- paste(tgt.basename, tgt.ext, sep = ".")
    tgt.filename <- file.path(tgt.subject.dir, tgt.basename)
    
    # If the target plot does not exists or it should not be overwritten
    if(any(!file.exists(tgt.filename), overwrite))
    {
      # Makes sure the target directory exists
      dir.create(
        path = tgt.subject.dir,
        showWarnings = FALSE,
        recursive = TRUE,
        mode = "0711"
      )
      
      plot.manoeuvre.signals(
        tgt.plot.filename = tgt.filename,
        tgt.plot.id = paste(mvre.id, right.plot.suffix, sep = ", "),
        time.instants = mvre.data[[time.col.name]],
        abp.signal = mvre.data[[abp.col.name]],
        cbfv.signal = mvre.data[[right.cbfv.col.name]],
        sampling.time = sampling.time,
        time.release = time.release,
        ...
      )
      plot.created <- TRUE
    }
    else
      cat(
        next.indent,
        "Warning: target plot already exist and not overwritten\n",
        sep = ""
      )
  }
  
  plot.created
}


run <- function(
      src.dir = file.path(WORK.DIR, "Data"),
      src.ext = "txt",
      header = TRUE,
      tgt.suffix = paste("CBFV", "params", "search", "plots", sep = "-"),
      tgt.dir = file.path(WORK.DIR, tgt.suffix),
      tgt.ext = "pdf",
      overwrite = FALSE,
      subjects = c("Sujeto1", "Sujeto2", "Sujeto3"),
      manoeuvres = c("maniobra1", "maniobra2", "maniobra3"),
      time.col.name = "Time",
      abp.col.name = "ABP",
      left.cbfv.col.name = "LCBFV",
      right.cbfv.col.name = "RCBFV",
      left.plot.suffix = "Izq",
      right.plot.suffix = "Der",
      sampling.time = 0.2,
      time.release = 0,
      baseline.initial.time = -5,
      baseline.final.time = time.release,
      min.ABP.max.delta.time = 5 * 0.8,
      min.CBFV.max.delta.time = 8 * 0.8,
      time.tol = sampling.time / 100,
      indent = ""
      )
{
  plot.created <- FALSE
  
  for(subject in subjects)
  {
    cat(indent, "-- ", subject, " --\n", sep = "")
    
    # Sets subject directories
    src.subject.dir <- file.path(src.dir, subject)
    tgt.subject.dir <- file.path(tgt.dir, subject)
    
    subject.plot.created <- plot.subject.signals(
      subject = subject,
      src.subject.dir = src.subject.dir,
      src.ext = src.ext,
      tgt.subject.dir = tgt.subject.dir,
      tgt.suffix = tgt.suffix,
      tgt.ext = tgt.ext,
      overwrite = overwrite,
      manoeuvres = manoeuvres,
      time.col.name = time.col.name,
      abp.col.name = abp.col.name,
      left.cbfv.col.name = left.cbfv.col.name,
      right.cbfv.col.name = right.cbfv.col.name,
      left.plot.suffix = left.plot.suffix,
      right.plot.suffix = right.plot.suffix,
      indent = paste0(indent, "  "),
      header = header,
      sampling.time = sampling.time,
      time.release = time.release,
      baseline.initial.time = baseline.initial.time,
      baseline.final.time = baseline.final.time,
      min.ABP.max.delta.time = min.ABP.max.delta.time,
      min.CBFV.max.delta.time = min.CBFV.max.delta.time,
      time.tol = time.tol
    )
    plot.created <- any(plot.created, subject.plot.created)
  }
  
  plot.created
}

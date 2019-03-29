# Change accordingly
VERSION <- "v15.9.10"
WORK.DIR <- file.path("C:", "Users/Luis/Documents/DataScience/Tesis", "Calculo ARI", VERSION)

CARSIG.SCRIPT.BASENAME <- paste("carSignal", VERSION, sep = "-")
CARSIG.SCRIPT.BASENAME <- paste(CARSIG.SCRIPT.BASENAME, "R", sep = ".")
CARSIG.SCRIPT.NAME <- file.path(WORK.DIR, CARSIG.SCRIPT.BASENAME)
source(CARSIG.SCRIPT.NAME)

DARI.SCRIPT.BASENAME <- paste("atARI", VERSION, sep = "-")
DARI.SCRIPT.BASENAME <- paste(DARI.SCRIPT.BASENAME, "R", sep = ".")
DARI.SCRIPT.NAME <- file.path(WORK.DIR, DARI.SCRIPT.BASENAME)
source(DARI.SCRIPT.NAME)

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


get.normalised.dari.templates <- function(
      time.instants,
      abp.signal,
      sample.release,
      sampling.time,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.instants[sample.release],
      min.abp.max.delta.time = 5 * 0.8,
      min.cbfv.max.delta.time = 8 * 0.8,
      stabilisation.time = 1,
      at.param.rounding.digits = 6,
      normalised.cbfv.rounding.digits = 4,
      time.tol = sampling.time / 100,
      ... # Not used
      )
{
  # Gets normalised ABP
  normalised.abp.signal <- normalise.ABP.signal(
    time.instants = time.instants,
    ABP.signal = abp.signal,
    sample.release = sample.release,
    sampling.time = sampling.time,
    baseline.initial.time = baseline.initial.time,
    baseline.final.time = baseline.final.time,
    min.ABP.max.delta.time = min.abp.max.delta.time,
    time.tol = time.tol
  )
  
  # Gets 91 Aaslid-Tiecks parameters
  at.params <- get.AT.decimal.templates.parameters(
    rounding.digits = at.param.rounding.digits
  )
  
  .tmp.fun <- function(i)
    get.theoretical.CBFV.response(
      T = at.params[i, 1],
      D = at.params[i, 2],
      K = at.params[i, 3],
      time.instants = time.instants,
      ABP.normalised = normalised.abp.signal[["normalised.ABP.signal"]],
      sampling.time = sampling.time,
      stabilisation.time = stabilisation.time
    )
  r <- 1:nrow(at.params)
  templates <- lapply(r, .tmp.fun)
  
  .tmp.fun <- function(t)
    normalise.CBFV.signal(
      time.instants = time.instants,
      CBFV.signal = t[["CBFV.theoretical.response"]],
      sample.release = sample.release,
      sampling.time = sampling.time,
      baseline.initial.time = baseline.initial.time,
      baseline.final.time = baseline.final.time,
      min.CBFV.max.delta.time = min.cbfv.max.delta.time,
      time.tol = time.tol
    )
  normalised.dari.templates <- lapply(templates, .tmp.fun)
  
  .tmp.fun <- function(t)
    round(t[["normalised.CBFV.signal"]], normalised.cbfv.rounding.digits)
  normalised.dari.templates <- lapply(normalised.dari.templates, .tmp.fun)
  
  names(normalised.dari.templates) <- 
    sapply(r, function(i) sprintf("%.1f", at.params[i, 4]))
  
  normalised.dari.templates
}


get.mfari.stats <- function(
      time.instants,
      abp.signal,
      cbfv.signal,
      sampling.time,
      time.release,
      keep.details = FALSE,
      ... # Passed to get.mfARI.parameters
    )
{
  mfari.params <- get.mfARI.parameters(
    time.instants = time.instants,
    ABP.signal = abp.signal,
    CBFV.signal = cbfv.signal,
    sampling.time = sampling.time,
    time.release = time.release,
    keep.details = keep.details,
    ...
  )
  
  mfari <- get.mfARI(
    Ks = mfari.params[["Ks"]],
    Delta.tau = mfari.params[["Delta.tau"]],
    Phi = mfari.params[["Phi"]]
  )
  
  data.frame(
    Ks = mfari.params[["Ks"]],
    Delta.tau = mfari.params[["Delta.tau"]],
    Phi = mfari.params[["Phi"]],
    mfARI = round(mfari, 1)
  )
}


get.dari.stats <- function(
      time.instants,
      cbfv.signal,
      sample.release,
      sampling.time,
      normalised.dari.templates,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.instants[sample.release],
      min.cbfv.max.delta.time = 8 * 0.8,
      fitting.value.name = "GoF",
      fitting.value.rounding.digits = 4,
      time.tol = sampling.time / 100,
      ... # Passed to get.best.templates()
      )
{
  normalised.cbfv.signal <- normalise.CBFV.signal(
    time.instants = time.instants,
    CBFV.signal = cbfv.signal,
    sample.release = sample.release,
    sampling.time = sampling.time,
    baseline.initial.time = baseline.initial.time,
    baseline.final.time = baseline.final.time,
    min.CBFV.max.delta.time = min.cbfv.max.delta.time,
    time.tol = time.tol
  )
  
  dari.search <- get.best.templates(
    time.instants = time.instants,
    signal = normalised.cbfv.signal[["normalised.CBFV.signal"]],
    templates = normalised.dari.templates,
    keep.details = FALSE,
    time.tol = time.tol,
    ...
  )
  
  ibest <- dari.search[["best.template.index"]]
  error <- dari.search[["best.fit.value"]]
  error <- round(error, fitting.value.rounding.digits)
  dari <- as.numeric(names(normalised.dari.templates)[ibest])
  
  d <- data.frame(error, dari)
  colnames(d) <- c(fitting.value.name, "dARI")
  
  d
}


get.signal.stats <- function(
      time.instants,
      abp.signal,
      cbfv.signal,
      sampling.time,
      sample.release,
      normalised.dari.templates,
      time.release = time.instants[sample.release],
      ... # Passed to get.mfari.stats & get.ari.stats
    )
{ 
  mfari.stats <- get.mfari.stats(
    time.instants = time.instants,
    abp.signal = abp.signal,
    cbfv.signal = cbfv.signal,
    sampling.time = sampling.time,
    time.release = time.release,
    ...
  )
  
  dari.stats <- get.dari.stats(
    time.instants = time.instants,
    cbfv.signal = cbfv.signal,
    sample.release = sample.release,
    sampling.time = sampling.time,
    normalised.dari.templates = normalised.dari.templates,
    ...
  )
  
  cbind(mfari.stats, dari.stats)
}


get.mvre.stats <- function(
      mvre.data,
      time.col.name = "Time",
      abp.col.name = "ABP",
      left.cbfv.col.name = "LCBFV",
      right.cbfv.col.name = "RCBFV",
      sampling.time = 0.2,
      time.release = 0,
      time.tol = sampling.time / 100,
      indent = "",
      ... # Passed to get.signal.stats
      )
{
  # Gets sample of release
  sample.release <- which(are.tolerably.equal(
    mvre.data[[time.col.name]],
    time.release,
    time.tol
  ))
  
  # Gets dARI templates
  normalised.dari.templates <- get.normalised.dari.templates(
    time.instants = mvre.data[[time.col.name]],
    abp.signal = mvre.data[[abp.col.name]],
    sample.release = sample.release,
    sampling.time = sampling.time,
    time.tol = time.tol,
    ...
  )
  
  # Left hemisphere
  cat(indent, "-- Left hemisphere --\n", sep = "")
  
  left.signal.stats <- get.signal.stats(
    time.instants = mvre.data[[time.col.name]],
    abp.signal = mvre.data[[abp.col.name]],
    cbfv.signal = mvre.data[[left.cbfv.col.name]],
    sampling.time = sampling.time,
    sample.release = sample.release,
    normalised.dari.templates = normalised.dari.templates,
    time.release = time.release,
    time.tol = time.tol,
    ...
  )
  
  left.signal.stats <- cbind(
    data.frame(Side = "Left"),
    left.signal.stats
  )
  
  # Right hemisphere
  cat(indent, "-- Right hemisphere --\n", sep = "")
  
  right.signal.stats <- get.signal.stats(
    time.instants = mvre.data[[time.col.name]],
    abp.signal = mvre.data[[abp.col.name]],
    cbfv.signal = mvre.data[[right.cbfv.col.name]],
    sampling.time = sampling.time,
    sample.release = sample.release,
    normalised.dari.templates = normalised.dari.templates,
    time.release = time.release,
    time.tol = time.tol,
    ...
  )
  
  right.signal.stats <- cbind(
    data.frame(Side = "Right"),
    right.signal.stats
  )
  
  # Joins stats
  rbind(left.signal.stats, right.signal.stats)
}


get.subject.stats.by.mvre <- function(
      subject,
      manoeuvres,
      src.subject.dir,
      src.ext = "txt",
      header = TRUE,
      indent = "",
      ... # Passed to get.mvre.stats
      )
{
  stats <- NULL
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
        paste(indent, " "),
        "Warning: file ",
        src.filename,
        " does not exists\n",
        sep = ""
      )
      next
    }
    
    # Reads the manoeuvre signals
    mvre.data <- read.table(src.filename, header = header)
    
    # Gets manoeuvre stats
    mvre.stats <- get.mvre.stats(
      mvre.data = mvre.data,
      indent = paste0(indent, "  "),
      ... # Passed to get.signal.stats
    )
    
    mvre.stats <- cbind(
      data.frame(Manoeuvre = mvre),
      mvre.stats
    )
    
    # Joins manoeuvre's stats
    stats <- rbind(stats, mvre.stats)
    
  }
  
  stats
}

get.stats.by.mvre <- function(
      src.dir,
      subjects,
      manoeuvres,
      indent = "",
      ... # Passed to get.subject.stats.by.mvre
      )
{
  stats <- NULL
  for(subject in subjects)
  {
    cat(indent, "-- ", subject, " --\n", sep = "")
    
    # Sets subject source directory
    src.subject.dir <- file.path(src.dir, subject)
    
    # Gets the subject's stats
    subject.stats <- get.subject.stats.by.mvre(
      subject = subject,
      manoeuvres = manoeuvres,
      src.subject.dir = src.subject.dir,
      indent = paste(indent, " "),
      ... # Passed to get.mvre.stats
    )
    
    subject.stats <- cbind(
      data.frame(Subject = subject),
      subject.stats
    )
    
    # Joins subject's stats
    stats <- rbind(stats, subject.stats)
  }
  
  stats
}


get.stats.by.subject <- function(stats.by.mvre, index.name)
{
  col.names <- c("Subject", "Manoeuvre", "Side", index.name)
  table.index <- stats.by.mvre[, col.names]
  table.index <- reshape(
    data = table.index,
    idvar = c("Subject", "Side"),
    timevar = "Manoeuvre",
    direction = "wide"
  )

  col.names <- colnames(table.index)[3:ncol(table.index)]
  col.names <- gsub(paste0(index.name, "."), "", col.names)
  colnames(table.index)[3:ncol(table.index)] <- col.names
  
  means <- apply(table.index[, col.names], 1, mean)
  sds <- apply(table.index[, col.names], 1, sd)
  covs = round(100 * sds / means, 2)
  covs
  table.index[["Mean"]] <- round(means, 1)
  table.index[["SD"]] <- round(sds, 2)
  table.index[["CoV"]] <- round(covs, 2)
  
  table.index
}

run <- function(
      src.dir = file.path(WORK.DIR, "Data"),
      src.ext = "txt",
      header = TRUE,
      tgt.suffix = paste("stats", VERSION, sep = "-"),
      tgt.dir = file.path(WORK.DIR, "stats"),
      tgt.ext = "csv",
      overwrite = FALSE,
      subjects = c("Sujeto1"),
      manoeuvres = c("maniobra1", "maniobra2"),
      time.col.name = "Time",
      abp.col.name = "ABP",
      left.cbfv.col.name = "LCBFV",
      right.cbfv.col.name = "RCBFV",
      left.plot.suffix = "Izq",
      right.plot.suffix = "Der",
      sampling.time = 0.4,
      time.release = 0,
      baseline.initial.time = -68,
      baseline.final.time = time.release,
      min.ABP.max.delta.time = 5 * 0.8,
      min.CBFV.max.delta.time = 8 * 0.8,
      stabilisation.time = -70,
      referential.time.instant = time.release,
      delta.time.before.ref = 0,
      delta.time.after.ref = round(floor(80 * 0.8 / sampling.time) * sampling.time, 1),
      comparison.function = get.MSE,
      fitting.value.name = "MSE",
      fitting.value.rounding.digits = 4,
      at.param.rounding.digits = 6,
      time.tol = sampling.time / 100,
      indent = ""
      )
{
  # Makes sure the target directory exists
  dir.create(
    path = tgt.dir,
    showWarnings = FALSE,
    recursive = TRUE,
    mode = "0711"
  )
  
  next.indent <- paste0(indent, "  ")
  
  # 
  # Results by manoeuvre (English format)
  # 
  
  cat(
    indent,
    "-- CSV file with stats by manoeuvre (English)\n",
    sep = ""
  )
  tgt.basename <- paste("manoeuvres", tgt.suffix, sep = "-")
  tgt.csv.name <- paste(tgt.basename, tgt.ext, sep = ".")
  tgt.csv.filename <- file.path(tgt.dir, tgt.csv.name)
  
  # If the target CSV file exists and it should not be overwritten
  if(all(file.exists(tgt.csv.filename), !overwrite))
  {
    cat(
      next.indent,
      "-- Target CSV file already exist and not overwritten...\n",
      next.indent,
      "   Using these data for any other missing file.\n",
      sep = ""
    )
    stats.by.mvre <- read.csv(tgt.csv.filename)
  }
  else
  {
    stats.by.mvre <- get.stats.by.mvre(
      src.dir = src.dir,
      src.ext = src.ext,
      header = header,
      subjects = subjects,
      manoeuvres = manoeuvres,
      time.col.name = time.col.name,
      abp.col.name = abp.col.name,
      left.cbfv.col.name = left.cbfv.col.name,
      right.cbfv.col.name = right.cbfv.col.name,
      left.plot.suffix = left.plot.suffix,
      right.plot.suffix = right.plot.suffix,
      sampling.time = sampling.time,
      time.release = time.release,
      baseline.initial.time = baseline.initial.time,
      baseline.final.time = baseline.final.time,
      min.ABP.max.delta.time = min.ABP.max.delta.time,
      min.CBFV.max.delta.time = min.CBFV.max.delta.time,
      stabilisation.time = stabilisation.time,
      referential.time.instant = referential.time.instant,
      delta.time.before.ref = delta.time.before.ref,
      delta.time.after.ref = delta.time.after.ref,
      comparison.function = comparison.function,
      fitting.value.name = fitting.value.name,
      fitting.value.rounding.digits = fitting.value.rounding.digits,
      at.param.rounding.digits = at.param.rounding.digits,
      time.tol = time.tol,
      indent = next.indent
    )
    write.csv(stats.by.mvre, file = tgt.csv.filename, row.names = FALSE)
    cat(next.indent, " CSV file created.\n", sep = "")
  }
  
  # 
  # Results by manoeuvre (Spanish format)
  # 
  
  cat(
    indent,
    "-- CSV file with stats by manoeuvre (Spanish)\n",
    sep = ""
  )
  tgt.basename <- paste("maniobras", tgt.suffix, sep = "-")
  tgt.csv.name <- paste(tgt.basename, tgt.ext, sep = ".")
  tgt.csv.filename <- file.path(tgt.dir, tgt.csv.name)
  
  # If the target CSV file exists and it should not be overwritten
  if(all(file.exists(tgt.csv.filename), !overwrite))
    cat(
      next.indent,
      "-- Target CSV file already exist and not overwritten\n",
      sep = ""
    )
  else
  {
    english <- c("Subject", "Manoeuvre", "Side")
    spanish <- c("Sujeto", "Maniobra", "Hemisferio")
    col.names <- mapply(gsub, english, spanish, colnames(stats.by.mvre))
    stats.by.mvre.es <- stats.by.mvre
    colnames(stats.by.mvre.es) <- col.names
    write.csv2(
      x = stats.by.mvre.es,
      file = tgt.csv.filename,
      row.names = FALSE
    )
    cat(next.indent, " CSV file created.\n", sep = "")
  }

  # 
  # Gets stats by subject
  # 
  
  cat(indent, "-- Getting dARI stats by subject\n", sep = "")
  subject.dari.stats <- get.stats.by.subject(stats.by.mvre, "dARI")
  
  cat(indent, "-- Getting mfARI stats by subject\n", sep = "")
  subject.mfari.stats <- get.stats.by.subject(stats.by.mvre, "mfARI")
  
  # 
  # Results by subject: dARI (English format)
  # 
  
  cat(
    indent,
    "-- CSV file with dARI stats by subject (English)\n",
    sep = ""
  )
  tgt.basename <- paste("subjects", "dARI", tgt.suffix, sep = "-")
  tgt.csv.name <- paste(tgt.basename, tgt.ext, sep = ".")
  tgt.csv.filename <- file.path(tgt.dir, tgt.csv.name)
  
  # If the target CSV file exists and it should not be overwritten
  if(all(file.exists(tgt.csv.filename), !overwrite))
    cat(
      next.indent,
      "-- Target CSV file already exist and not overwritten\n",
      sep = ""
    )
  else
  {
    write.csv(
      x = subject.dari.stats,
      file = tgt.csv.filename,
      row.names = FALSE
    )
    cat(next.indent, " CSV file created.\n", sep = "")
  }
  
  # 
  # Results by subject: mfARI (English format)
  # 
  
  cat(
    indent,
    "-- CSV file with mfARI stats by subject (English)\n",
    sep = ""
  )
  tgt.basename <- paste("subjects", "mfARI", tgt.suffix, sep = "-")
  tgt.csv.name <- paste(tgt.basename, tgt.ext, sep = ".")
  tgt.csv.filename <- file.path(tgt.dir, tgt.csv.name)
  
  # If the target CSV file exists and it should not be overwritten
  if(all(file.exists(tgt.csv.filename), !overwrite))
    cat(
      next.indent,
      "-- Target CSV file already exist and not overwritten\n",
      sep = ""
    )
  else
  {
    write.csv(
      x = subject.mfari.stats,
      file = tgt.csv.filename,
      row.names = FALSE
    )
    cat(next.indent, " CSV file created.\n", sep = "")
  }
  
  # 
  # Results by subject: dARI (Spanish format)
  # 
  
  cat(
    indent,
    "-- CSV file with dARI stats by subject (Spanish)\n",
    sep = ""
  )
  tgt.basename <- paste("sujetos", "dARI", tgt.suffix, sep = "-")
  tgt.csv.name <- paste(tgt.basename, tgt.ext, sep = ".")
  tgt.csv.filename <- file.path(tgt.dir, tgt.csv.name)
  
  # If the target CSV file exists and it should not be overwritten
  if(all(file.exists(tgt.csv.filename), !overwrite))
    cat(
      next.indent,
      "-- Target CSV file already exist and not overwritten\n",
      sep = ""
    )
  else
  {
    english <- c("Subject", "Side")
    spanish <- c("Sujeto", "Hemisferio")
    col.names <- mapply(
      FUN = gsub,
      english,
      spanish,
      colnames(subject.dari.stats)
    )
    subject.dari.stats.es <- subject.dari.stats
    colnames(subject.dari.stats.es) <- col.names
    write.csv2(
      x = subject.dari.stats.es,
      file = tgt.csv.filename,
      row.names = FALSE
    )
    cat(next.indent, " CSV file created.\n", sep = "")
  }
  
    
  stats.by.mvre
}

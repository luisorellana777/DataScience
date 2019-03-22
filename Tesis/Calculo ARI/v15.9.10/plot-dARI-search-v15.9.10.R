

# Change accordingly
VERSION <- "v15.9.10"
WORK.DIR <- file.path("", "research", "Memoristas", VERSION)


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
      normalised.abp.signal,
      sampling.time,
      sample.release,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.instants[sample.release],
      min.cbfv.max.delta.time = 8 * 0.8,
      stabilisation.time = 30,
      at.param.rounding.digits = 6,
      cbfv.rounding.digits = 2,
      time.tol = sampling.time / 100,
      ... # Not used
      )
{
  at.params <- get.AT.decimal.templates.parameters(
    rounding.digits = at.param.rounding.digits
  )
  
  .tmp.fun <- function(i)
    get.theoretical.CBFV.response(
      T = at.params[i, 1],
      D = at.params[i, 2],
      K = at.params[i, 3],
      time.instants = time.instants,
      ABP.normalised = normalised.abp.signal,
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
  normalised.templates <- lapply(templates, .tmp.fun)
  
  .tmp.fun <- function(t) t[["normalised.CBFV.signal"]]
  normalised.templates <- lapply(normalised.templates, .tmp.fun)
  
  names(normalised.templates) <- 
    sapply(r, function(i) sprintf("%.1f", at.params[i, 4]))
  
  normalised.templates
}


get.plot.dARI <- function(
      time.instants,
      normalised.abp.signal,
      normalised.cbfv.signal,
      template.search.results,
      abp.palette = brewer.pal(n = 9, name = "Reds"),
      cbfv.palette = brewer.pal(n = 9, name = "Blues"),
      template.palette = brewer.pal(n = 9, name = "Greens"),
      time.tol = min(diff(time.instants)) / 100,
      plot.id = NULL,
      fiting.value.name = "Fitting",
      fiting.value.rounding.digits = 3,
      ... # Not used
      )
{
  # Rebuilds the original signals
  abp.baseline <- attr(normalised.abp.signal, "baseline.value")
  abp.min <- attr(normalised.abp.signal, "min.value")
  abp <- normalised.abp.signal * (abp.baseline - abp.min) + abp.min
  
  cbfv.baseline <- attr(normalised.cbfv.signal, "baseline.value")
  cbfv.min <- attr(normalised.cbfv.signal, "min.value")
  .tmp.fun <- function(x) x * (cbfv.baseline - cbfv.min) + cbfv.min
  cbfv <- .tmp.fun(normalised.cbfv.signal)
  
  # Plots CBFV
  dcbfv <- 
    data.frame(Time = time.instants, Signal = cbfv, Segment = "CBFV")
  p <- ggplot(dcbfv, aes(x = Time, y = Signal, colour = Segment))
  p <- p + geom_line(linetype = "dashed")
  d <- dcbfv[template.search.results[["relevant.samples"]], ]
  p <- p + geom_line(data = d)
  
  # Plots ABP
  dabp <- data.frame(Time = time.instants, Signal = abp, Segment = "ABP")
  p <- p + geom_line(data = dabp, linetype = "dashed")
  
  # Gets relevant time instants
  relevant.time.instants <- 
    time.instants[template.search.results[["relevant.samples"]]]

  # Plots representative templates
  i <- seq(11, 91, 20)
  temps <- template.search.results[["relevant.template.segments"]][i]
  values <- c(sapply(temps, .tmp.fun))
  lengths <- c(sapply(temps, function(t) length(t)))
  aris <- paste("ARI", "=", names(temps))
  aris <- rep(aris, times = lengths)
  times <- rep(relevant.time.instants, times = length(i))
  
  d <- data.frame(Time = times, Signal = values, Segment = aris)
  p <- p + geom_line(data = d, linetype = "dashed")
  
  # Plots best template
  ibest <- template.search.results[["ranking"]][1]
  best <- template.search.results[["relevant.template.segments"]][[ibest]]
  d <- data.frame(
    Time = relevant.time.instants,
    Signal = .tmp.fun(best),
    Segment = "Best template"
  )
  p <- p + geom_line(data = d)
  
  p <- p + xlab("Time") + ylab("Signals")
  cmatch <- c("ABP" = abp.palette[4], "CBFV" = cbfv.palette[4])
  cmatch <- c(cmatch, "Best template" = template.palette[9])
  subpal <- template.palette[3:7]
  aris <- paste("ARI", "=", names(temps))
  names(subpal) <- aris
  cmatch <- c(cmatch, subpal)
  p <- p + scale_colour_manual(values = cmatch, breaks = names(cmatch))
  p <- p + theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha('grey90', 0.0)),
    legend.position = c(0, 0),
    legend.key.size =  unit(0.4, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )
  
  # Adds annotation table
  l1 <- "Time of release"
  v1 <- sprintf(
    "t = %.1f",
    template.search.results[["referential.time.instant"]]
  )
  l2 <- "Compared window"
  v2 <- sprintf(
    "t in [%.1f, %.1f]",
    template.search.results[["initial.time.instant"]],
    template.search.results[["final.time.instant"]]
  )
  l3 <- paste("Best", fiting.value.name)
  v3 <- sprintf(
    "%.*f",
    fiting.value.rounding.digits,
    template.search.results[["fit.values"]][ibest]
  )
  ann.table <- data.frame(label = c(l1, l2, l3), values = c(v1, v2, v3))
  gt <- tableGrob(
    d = ann.table,
    show.rownames = FALSE,
    show.colnames = FALSE,
    gpar.coretext = gpar(col = cbfv.palette[7], cex = 0.6),
    padding.v = unit(2, "mm")
  )
  
  xmin <- time.instants[1]
  xmax <- tail(time.instants, 1)
  rx <- xmax - xmin
  dx <- rx / 100
  ymin <- min(abp, cbfv)
  ymax <- max(abp, cbfv)
  ry <- ymax - ymin
  dy <- ry / 100

  p <- p + annotation_custom(
    grob = gt,
    xmin = xmax - 25 * dx,
    xmax = xmax,
    ymin = ymin,
    ymax = ymin + 10 * dy
  )
  
  # Sets the plot title
  best.ari <- paste(
    "dARI",
    names(template.search.results[["relevant.template.segments"]])[ibest],
    sep = " = "
  )

  if(is.null(plot.id))
    p <- p + ggtitle(best.ari)
  else
    p <- p + ggtitle(paste(plot.id, best.ari, sep = ", "))
  
  p
}


plot.manoeuvre.signals <- function(
      time.instants,
      normalised.abp.signal,
      normalised.cbfv.signal,
      normalised.cbfv.templates,
      tgt.plot.filename,
      tgt.plot.id,
      ...
    )
{
  dari.search <- get.best.templates(
    time.instants = time.instants,
    signal = normalised.cbfv.signal,
    templates = normalised.cbfv.templates,
    keep.details = TRUE,
    ...
  )
    
  p <- get.plot.dARI(
    time.instants = time.instants,
    normalised.abp.signal = normalised.abp.signal,
    normalised.cbfv.signal = normalised.cbfv.signal,
    template.search.results = dari.search,
    time.tol = time.tol,
    plot.id = tgt.plot.id,
    ...
  )
  
  ggsave(
    filename = tgt.plot.filename,
    plot = p,
    width = 11, 
    height = 8,
    units = "in"
  )
}


get.manoeuvre.normalised.abp <- function(
      time.instants,
      abp.signal,
      sampling.time,
      sample.release,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.instants[sample.release],
      min.abp.max.delta.time = 5 * 0.8,
      time.tol = sampling.time / 100,
      ... # Not used
      )
{
  normalised.signal <- normalise.ABP.signal(
    time.instants = time.instants,
    ABP.signal = abp.signal,
    sample.release = sample.release,
    sampling.time = sampling.time,
    baseline.initial.time = baseline.initial.time,
    baseline.final.time = baseline.final.time,
    min.ABP.max.delta.time = min.abp.max.delta.time,
    time.tol = time.tol
  )
  
  normalised.abp.signal <- normalised.signal[["normalised.ABP.signal"]]
  attr(normalised.abp.signal, "baseline.value") <-
    normalised.signal[["ABP.baseline.value"]]
  attr(normalised.abp.signal, "min.value") <-
    normalised.signal[["min.ABP.value"]]
  
  normalised.abp.signal
}


get.manoeuvre.normalised.cbfv <- function(
      time.instants,
      cbfv.signal,
      sampling.time,
      sample.release,
      baseline.initial.time = time.instants[1],
      baseline.final.time = time.instants[sample.release],
      min.cbfv.max.delta.time = 8 * 0.8,
      time.tol = sampling.time / 100,
      ... # Not used
      )
{
  normalised.signal <- normalise.CBFV.signal(
    time.instants = time.instants,
    CBFV.signal = cbfv.signal,
    sample.release = sample.release,
    sampling.time = sampling.time,
    baseline.initial.time = baseline.initial.time,
    baseline.final.time = baseline.final.time,
    min.CBFV.max.delta.time = min.cbfv.max.delta.time,
    time.tol = time.tol
  )
  
  normalised.cbfv.signal <- normalised.signal[["normalised.CBFV.signal"]]
  attr(normalised.cbfv.signal, "baseline.value") <-
    normalised.signal[["CBFV.baseline.value"]]
  attr(normalised.cbfv.signal, "min.value") <-
    normalised.signal[["min.CBFV.value"]]
  
  normalised.cbfv.signal
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
      time.tol = sampling.time / 100,
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
    
    # Sets subject and manoeuvre as initial plot titles
    mvre.id <- paste(subject, mvre, sep = ", ")
    
    # Gets sample of release
    sample.release <- which(are.tolerably.equal(
      mvre.data[[time.col.name]],
      time.release,
      time.tol
    ))
    
    # Gets normalised ABP
    normalised.abp.signal <- get.manoeuvre.normalised.abp(
      time.instants = mvre.data[[time.col.name]],
      abp.signal = mvre.data[[abp.col.name]],
      sampling.time = sampling.time,
      sample.release = sample.release,
      time.tol = time.tol,
      ...
    )
    
    # Gets dARI templates
    normalised.dari.templates <- get.normalised.dari.templates(
      time.instants = mvre.data[[time.col.name]],
      normalised.abp.signal = normalised.abp.signal,
      sampling.time = sampling.time,
      sample.release = sample.release,
      time.tol = time.tol,
      ...
    )
    
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
    
      normalised.cbfv.signal <- get.manoeuvre.normalised.cbfv(
        time.instants = mvre.data[[time.col.name]],
        cbfv.signal = mvre.data[[left.cbfv.col.name]],
        sampling.time = sampling.time,
        sample.release = sample.release,
        time.tol = time.tol,
        ...
      )
      
      plot.manoeuvre.signals(
        time.instants = mvre.data[[time.col.name]],
        normalised.abp.signal = normalised.abp.signal,
        normalised.cbfv.signal = normalised.cbfv.signal,
        normalised.cbfv.templates = normalised.dari.templates,
        tgt.plot.filename = tgt.filename,
        tgt.plot.id = paste(mvre.id, left.plot.suffix, sep = ", "),
        ...
      )
      
      plot.created <- TRUE
    }
    else
      cat(
        paste(next.indent, "  "),
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
      
      normalised.cbfv.signal <- get.manoeuvre.normalised.cbfv(
        time.instants = mvre.data[[time.col.name]],
        cbfv.signal = mvre.data[[right.cbfv.col.name]],
        sampling.time = sampling.time,
        sample.release = sample.release,
        time.tol = time.tol,
        ...
      )
      
      plot.manoeuvre.signals(
        time.instants = mvre.data[[time.col.name]],
        normalised.abp.signal = normalised.abp.signal,
        normalised.cbfv.signal = normalised.cbfv.signal,
        normalised.cbfv.templates = normalised.dari.templates,
        tgt.plot.filename = tgt.filename,
        tgt.plot.id = paste(mvre.id, right.plot.suffix, sep = ", "),
        ...
      )
      
      plot.created <- TRUE
    }
    else
      cat(
        paste(next.indent, "  "),
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
      tgt.suffix = paste("dARI", "search", "plots", sep = "-"),
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
      min.abp.max.delta.time = 5 * 0.8,
      min.cbfv.max.delta.time = 8 * 0.8,
      stabilisation.time = 30,
      referential.time.instant = time.release,
      delta.time.before.ref = 0,
      delta.time.after.ref = round(floor(20 * 0.8 / sampling.time) * sampling.time, 1),
      comparison.function = get.MSE,
      fiting.value.name = "MSE",
      fiting.value.rounding.digits = 4,
      at.param.rounding.digits = 6,
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
      header = header,
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
      sampling.time = sampling.time,
      time.release = time.release,
      baseline.initial.time = baseline.initial.time,
      baseline.final.time = baseline.final.time,
      min.abp.max.delta.time = min.abp.max.delta.time,
      min.cbfv.max.delta.time = min.cbfv.max.delta.time,
      stabilisation.time = stabilisation.time,
      referential.time.instant = referential.time.instant,
      delta.time.before.ref = delta.time.before.ref,
      delta.time.after.ref = delta.time.after.ref,
      comparison.function = comparison.function,
      fiting.value.name = fiting.value.name,
      fiting.value.rounding.digits = fiting.value.rounding.digits,
      at.param.rounding.digits = at.param.rounding.digits,
      time.tol = time.tol,
      indent = paste0(indent, "  ")
    )
    plot.created <- any(plot.created, subject.plot.created)
  }
  
  plot.created
}

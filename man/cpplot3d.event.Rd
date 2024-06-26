% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cplot3d.event.R, R/pplot3d.event.R
\name{cplot3d.event}
\alias{cplot3d.event}
\alias{pplot3d.event}
\title{Interactive scatterplot of the (3 dimensional) points either plotted by color and size or number of points in each voxel corresponding to the value of the points.}
\usage{
cplot3d.event(
  event = 1,
  t = 1,
  turns = 10,
  cruise = 2009116,
  TVG = TRUE,
  TVG.exp = 2,
  dir.data = NULL,
  Paout = TRUE,
  exact = FALSE,
  bgns = TRUE,
  pdns = TRUE,
  nrns = TRUE,
  hins = TRUE,
  kern = NULL,
  segpar = NULL,
  pamkpar = list(),
  nsind = 0.75,
  hins_add = 10,
  pdns_scale = 1e-14,
  TOV = 0,
  cal = 1,
  bmmd = NULL,
  breaks = 40,
  col = "combined",
  colpar = list(start = 0, end = 0.8, flip = TRUE),
  clamp = c(0, 1),
  shrink = TRUE,
  null.value = NA,
  white = 1,
  log = TRUE,
  color.bar = "y--",
  color.bar.lwd = 8,
  color.bar.adj = 0,
  color.bar.tadj = 0.1,
  color.bar.noWhite = TRUE,
  color.bar.nticks = 8,
  color.bar.tickw = 0.005,
  color.bar.tickcol = "black",
  db = TRUE,
  voxels.out = FALSE,
  esnm = "MS70",
  var = c("vbsc", "sgsc", "pr0s", "sgs0", "sgsE", "sgsi", "sgsI", "psis", "tlns"),
  ind = list(-(1:150), NULL),
  range = list(),
  subset = NULL,
  ideal = TRUE,
  seabed = -12000,
  rot = 2,
  compensation = c("pitch", "roll"),
  plot = TRUE,
  cs.xyzlim = "g",
  add = FALSE,
  beamstypes = 1,
  size = cplot3d.size("pow", y = 20, par = 4),
  bottom = FALSE,
  bottom.res = 5,
  bottom.smooth = NULL,
  bottom.col = "topo.col",
  bottom.N = 1e+05,
  bottom.seg = list(sgth = 0.2, misM = 0.001, bwGp = 2, turns = 100),
  adds = NULL,
  xlim = NULL,
  ylim = NULL,
  zlim = NULL,
  view = c("free", "top", "bottom", "south", "west", "north", "east"),
  zoom = 0.7,
  fov = 60,
  school = FALSE,
  schoolcol = "purple",
  schoolsize = 0.3,
  schoolsample = 0.01,
  schoollen = 4,
  schoollwd = 1,
  schooltype = "p",
  schoolcrop = FALSE,
  plot.seg = FALSE,
  seg.col = "green",
  seg.alpha = 0.2,
  subdivide = 3,
  excl.neg = TRUE,
  object = c("ellipsoid", "cuboid"),
  par = double(3),
  center = c(0, 0, 0),
  angle = 0,
  aspect = "iso",
  nticks = 5,
  origin = 1,
  xlab = "x",
  ylab = "y",
  zlab = "z",
  full.box = FALSE,
  edge.vpos = "",
  line.vpos = 0,
  at.vpos = NULL,
  cex.vpos = 1,
  col.vpos = "blue",
  clock = "bbl",
  cex.clock = 1,
  adj.clock = c(0, 0),
  format.clock = "Ping: indt\\nyyyy-mm-dd\\nHH:MM:SS.FFF",
  digits.clock = 2,
  lsp.clock = 0.04,
  col.clock = 4,
  sonar.grid = "frame",
  sonar.grid.col = c("orange", "cornflowerblue"),
  sonar.grid.lwd = 1,
  cs.pos = "g",
  cs.view = "g",
  sides = c("tb", "tb"),
  dens = c(200, 100, 100, 1),
  every = Inf,
  global.grid = FALSE,
  global.grid.lwd = 0.5,
  global.grid.lty = 1,
  ...
)

pplot3d.event(
  event = 1,
  t = 1,
  turns = 10,
  cruise = 2009116,
  TVG = TRUE,
  TVG.exp = 2,
  dir.data = NULL,
  Paout = TRUE,
  exact = FALSE,
  bgns = TRUE,
  pdns = TRUE,
  nrns = TRUE,
  hins = TRUE,
  kern = NULL,
  segpar = NULL,
  pamkpar = list(),
  nsind = 0.75,
  hins_add = 10,
  pdns_scale = 1e-14,
  TOV = 0,
  cal = 1,
  bmmd = NULL,
  N = 1e+05,
  acca = NULL,
  fun = "mod",
  allert = 1e+08,
  nlim = NULL,
  fact = NULL,
  cols = c("black", "navyblue", "magenta4", "red3", "darkorange2", "yellow"),
  stretch = 1,
  scale.voxels = 1,
  rand.gen = c("unif", "beta", "norm"),
  possample = 1,
  esnm = "MS70",
  var = c("vbsc", "sgsc", "pr0s", "sgs0", "sgsE", "sgsi", "sgsI", "psis", "tlns"),
  ind = list(-(1:150), NULL),
  range = list(),
  subset = NULL,
  ideal = TRUE,
  seabed = -12000,
  rot = 2,
  compensation = c("pitch", "roll"),
  plot = TRUE,
  cs.xyzlim = "g",
  add = FALSE,
  beamstypes = 1,
  size = 0.3,
  bottom = FALSE,
  bottom.res = 5,
  bottom.smooth = NULL,
  bottom.col = "topo.col",
  bottom.N = 1e+05,
  bottom.seg = list(sgth = 0.2, misM = 0.001, bwGp = 2, turns = 100),
  adds = NULL,
  xlim = NULL,
  ylim = NULL,
  zlim = NULL,
  view = c("free", "top", "bottom", "south", "west", "north", "east"),
  zoom = 0.7,
  fov = 60,
  school = FALSE,
  schoolcol = "purple",
  schoolsize = 0.3,
  schoolsample = 0.01,
  schoollen = 4,
  schoollwd = 1,
  schooltype = "p",
  schoolcrop = FALSE,
  plot.seg = FALSE,
  seg.col = "green",
  seg.alpha = 0.2,
  subdivide = 3,
  excl.neg = TRUE,
  object = c("ellipsoid", "cuboid"),
  par = double(3),
  center = c(0, 0, 0),
  angle = 0,
  aspect = "iso",
  nticks = 5,
  origin = 1,
  xlab = "x",
  ylab = "y",
  zlab = "z",
  full.box = FALSE,
  edge.vpos = "",
  line.vpos = 0,
  at.vpos = NULL,
  cex.vpos = 1,
  col.vpos = "blue",
  clock = "bbl",
  cex.clock = 1,
  adj.clock = c(0, 0),
  format.clock = "Ping: indt\\nyyyy-mm-dd\\nHH:MM:SS.FFF",
  digits.clock = 2,
  lsp.clock = 0.04,
  col.clock = 4,
  sonar.grid = "frame",
  sonar.grid.col = c("orange", "cornflowerblue"),
  sonar.grid.lwd = 1,
  cs.pos = "g",
  cs.view = "g",
  sides = c("tb", "tb"),
  dens = c(200, 100, 100, 1),
  every = Inf,
  global.grid = FALSE,
  global.grid.lwd = 0.5,
  global.grid.lty = 1,
  ...
)
}
\description{
Interactive scatterplot of the (3 dimensional) points either plotted by color and size or number of points in each voxel corresponding to the value of the points.

Interactive scatterplot of the (3 dimensional) points either plotted by color and size or number of points in each voxel corresponding to the value of the points.
}
\examples{
\dontrun{}

\dontrun{}

}

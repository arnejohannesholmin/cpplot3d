#*********************************************
#*********************************************
#' Interactive scatterplot of the (3 dimensional) points either plotted by color and size or number of points in each voxel corresponding to the value of the points.
#'
#' @param cpplot3d_type  is "c" for cplot3d.event() and "p" for pplot3d.event().
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param t  is either a vector of the numbers of the pings to be returned, as listed from 1 to the number of pings in the event, or a vector of time points given as strings "yyyymmddHHMMSS.FFF" or "HHMMSS.FFF" from which the range of the time points to be read is extracted. If t=="all", all files are read and if t=="none" an empty list is returned.
#' @param turns  is the number of pings read and plotted in each turn, useful if many time points are to be plotted and memory is an issue (such as >100 time steps for MS70).
#' @param cruise  is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
#' @param TVG  is FALSE if no Time Varied Gain compensation is to be performed on the acoustic data.
#' @param dir.data  is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
#' @param Paout  is TRUE if pressure data are to be returned in Pascal.
#' @param exact  is TRUE if the volumes should be calculated by the slower procedure incorporating the edges of the voxels. If FALSE (default) use the fast functions which only account for changes in the thickness of the voxels.
#' @param bgns  indicates wheter the estimated background noise should be subtracted, resulting in some negative sv-values. If FALSE the background noise estimates are attemptedly located in the event directory or the noise estimate directory of the cruise. If string, it should be the path to the file holding the background noise estimates. If the length of 'bgns' is longer than 1, the background noise estmate taken as the simple mean for all beams, regardless of the periodic noise is used.
#' @param pdns  indicates wheter the estimated periodic noise should be subtracted. If FALSE the periodic noise estimates are attemptedly located in the event directory or the noise estimate directory of the cruise. If string, it should be the path to the file holding the periodic noise estimates.
#' @param nrns  indicates wheter the estimated near range (close range) noise should be subtracted. If FALSE the near range noise estimates are attemptedly located in the event directory or the noise estimate directory of the cruise.
#' @param hins  indicates wheter the estimated high intensity noise should be subtracted. If FALSE the high intensity noise estimates are attemptedly located in the event directory or the noise estimate directory of the cruise.
#' @param kern  is the standard deviation in units of the number of voxels used in a Gaussian kernel smoother along the beams (no smoothing if kern==0 or length(kern)==0, which is the default).
#' @param segpar  is a list of elements named "bwGp", "lsth"/"rlst", "usth"/"rust", or "sgth"/"sgt0" specifying the parameters of the segmentation data to read.
#' @param pamkpar  is a list of parameters ('krange', 'criterion', 'alpha' and 'mindist') used pamk() when clustering the segmented voxels 'sgsc'. The voxels 'sgsc' are also ordered so that the voxels belonging to the largest cluster lead and the second to largest cluster follows. A suggested set of parameters are pamkpar=list(krange=1:4,criterion="asw",alpha=0.05,N=1e2,mindist=100).
#' @param nsind  is a vector of indexes along the beams, as input to ind.expand(), used to select the subset over which the estimation of the phase of the periodic noise is done. If given as a single numeric, the outermost 'nsind' voxels are used in each beam.
#' @param hins_add  is the number of voxels that should be discarded on both sides of high intensity noise voxels voxels along beams, used for accounting for possible high values that are related to the high intensity noise but not classified as such voxels.
#' @param pdns_scale  is used in get.pdns_phase.event() to scale the noise in order to allow the optimization to work.
#' @param TOV  is the time offset of the vessel information, discovered by Holmin and Korneliussen in 2013. The default is found in "/Applications/echoIBM/Documentation/Error in yaw MS70/Error in yaw MS70.R".
#' @param breaks  has two possible inputs: (1) the number of breaks of the scale on which the data 'z' are arranged, equally spaced between min(z) and max(z) (or logarithmically equaly spaced when log=TRUE). (2) a vector of values for the breaks given in dB values (volume backscattering strength Sv), typically -82:-30 for EK60.
#' @param col  is either the color vector if length(col)>1, or the color function to generate the colors from. Currently the color function name must be one of "rainbow", "grey", "heat.colors", "terrain.colors", "topo.colors" and "cm.colors", but other color functions may be inplemented in the future. Set color.bar=NULL to supress plotting the color bar.
#' @param colpar  is a list of parameters used in colscale().
#' @param size  is the size vector of the points (see plot3d()), or a function of the number of breaks, such as the functions returned by cplot3d.size(), or a list of inputs to cplot3d.size().
#' @param clamp  is a vector of two elements defining the start and end value along the range of the data, outside which the colors plotted are the colors representing the lowest bin and the highest bin, (depending on the value of 'flip' and/or 'start' and 'end' specified in 'colpar'). If given in [0,1] 'clamp' refers to the fractions along the range of the data, and to dB values if not.
#' @param shrink  is TRUE if discarded points due to the value of 'white' are to be excluded from the plott, so that the bounding box is shrinked to the range of visible points.
#' @param null.value  is is the value to set non-postitive values to in the case of logarithmic plotting. The default is the only non-numeric value allowed, and uses the smallest positive value of the data.
#' @param white  is an integer value representing the number of the lower breaks for which points should not be plotted (corresponding to white points). Can be given as a dB value below which breaks are plotted as white.
#' @param log  is TRUE if log transformed data are to be plotted. If so, all values must be positive.
#' @param color.bar  is a string of three characters, where the first is one of "x", "y", "z", and denotes along which dimension the color bar should be parallel, the second and third characters are either "-" or "+", and denotes along which of the four edges parallel to the axis given by the first charater, the color bar should be put (where the axes are ranged "x", "y", "z"). color.bar="x-+" puts the color bar on the top south edge, if the x-axis is defined east, the y-axis north and the z-axis vertical. If 'color.bar' is FALSE, NULL or "", plotting of the color bar is suppressed.
#' @param color.bar.lwd  is the line width of the color bar (thickness).
#' @param color.bar.adj  is a vector of adjustment values for the color bar (in the x, y, and z direction), where the values are given as fractions of the size of the corresponding dimensions. If given as a single numeric, 'color.bar.adj' is interpreted as the adjustment in the lowest available dimension (for example in the y direciton if color.bar="x--").
#' @param color.bar.tadj  is similar to 'color.bar.adj', but for the labels of the tickmarks on the color bar.
#' @param color.bar.noWhite  is FALSE to include all values which are not plotted due to 'white' in the color bar.
#' @param color.bar.nticks  is the desired number of ticks on the color bar, used in pretty().
#' @param color.bar.tickw  is the thickness of the tick marks on the color bar, as a fraction of the plotting frame.
#' @param color.bar.tickcol  is the color of the tich marks.
#' @param db  is FALSE if the color bar should display linear (not desibel) values.
#' @param voxels.out  is TRUE if the position of all voxels plotted should be included in the output.
#' @param N  is the approximate number of points plotted.
#' @param acca  is the acoustic scattering cross sectional area (assuming omnidirectional targets). Overriding 'N' if given.
#' @param fun  is the name of the funciton used for obtaining the number of points in each voxel by "rounding off" to integers. The string given in 'fun' must be the name of a function taking 'x' as the input, such as "floor", "round", "ceiling" and "mod", where the last function is defined in sv2pos() and draws between floor(x) and ceiling(x) with probability P(ceiling(x))=x\%\%1. Also, there is the option of adding a numeric to "mod", such as "mod3" or "mod4.2", to indicate that all voxels with number of points below the given numeric, are set to zero.
#' @param allert  is the limit of the number of points inducing a warning asking the user to continue. Default is NULL, indicating no check for the number of points to be generated.
#' @param nlim  is a vector of limits for the number of points, dividing the voxels into classes in which the number of points are reduced by the values specified in 'fact'. If nlim==NULL, the default values are set in pplot3d.set.default.nlim() depending on whether the acoustical instrument is a sonar (nlim = 30^(1:5)) or an echosounder (nlim = 50^(1:4)) (for echosounders the bottom echo may require larger values of 'nlim'). Note that 0 is added to the head of 'nlim', representing the unchnaged class of points which should not be changed in number. Thus, if nlim=100, the voxels containing less than 100 points are assigned to the first level, and voxels with 100 points or more are assigned to the second level. The values nlim=Inf and nlim=0 imply only one class, which is also achieved by setting 'cols' to only one color.
#' @param fact  is either NULL, indicating that fact=1/sqrt(nlim), or the factor by which to scale the number of points in voxels exceeding the value of 'nlim'. If 'fact' has length shorter than 'nlim', the first value is used as a base in fact = fact[1]^(1:length(nlim)). As for 'nlim', 'fact' is added 1 to the head, indicating no scaling of the number of points in the voxels in the first level. 'fact' can also be given as a function, in which case the function recreating the default factors is function(x) x^(-1/2), whereas function(x) x^(-2/3) would reduce the number of points plotted.
#' @param cols  is a vector specifying the colors of the points in the different levels. Differing from 'nlim' and 'fact', the color vector must include also the basic class of voxels starting from one point in each voxel. If the length of 'cols' is shorter than 'nlim' or 'fact', these are cropped to the length of 'cols' after 0 is added to 'nlim' and 1 is added to 'fact'. Thus, specifying cols=2 sets all points to red, and applies no classes for the voxels.
#' @param stretch  is used to stretch the voxels of the ME70 multibeam echosounder in the direction of motion, so that space in between voxels is smoothed out. If given as a two element vector, the first element sets the stretching factor applied at the depth given by the second element. This results in attemptedly constant stretching for all depths.
#' @param possample  is used to select a sample of the regenerated points to plot, used when all points are at the same level og TS, and the number of points should be reduced in the plot but not in the data.
#' @param esnm  is the name of the acoustical instrument, given as a four character string. See sonR_implemented() for the implemented systems. May be given in 'data', and in lower case.
#' @param var  is a string specifying the variable to plot. Currently supported are "vbsc", for volume backscattering data, "sgsc"/"sgs0"/"sgsE"/"sgsi"/"sgsI", for (thresholded) segmentation data, "pr0s" or "psis" for unhtresholded not-school probability data, and "tlns" for pure estimated noise. Segmentation data can be subtracted or added to the data by by adding "+" or "-" to a second element of var, where this is a segmentation variable name such as "-sgbt", which implies subtracting bottom segmentation mask. See cpplot3d.extract_var(). 
#' @param ind  is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' @param range  is a list of elements with names matching names of 'data', specifying the range of the corresponding elements.
#' @param subset  is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' @param ideal  is TRUE to represent the simple case where the speed of sound 'data$asps' is invariant of depth.
#' @param seabed  is the depth of the seabed, at which the beams are reflected when calculating the midpoints of voxels.
#' @param rot  see soundbeam.TSD().
#' @param compensation  is a vector of string giving which rotation values that are compensated for in the sonar. Only c("pitch","roll") is available for the current version. Used in soundbeam.TSD.
#' @param plot  is FALSE to prevent plotting.
#' @param cs.xyzlim  defines the coordinate system to use when specifying the plotting frame. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel.
#' @param add  is TRUE if points are to be added to an existing plot.
#' @param beamstypes  is a vector of strings "rect" and "circ", or a vector integers 1 and 2 specifying whether to plot rectangular og circular beams. If beamstypes==1, the rectangular beams are plotted if both present, and the once present are plotted if only rectangular or only circular beams are present.
#' @param bottom  has five possible values: (a) If bottom=="new", a new set of segmentation data is generated, where the bottom is detected due to the default specifications used in echoIBM.segment.event() (sgth=0.2, misM=1e-3, bwGp=2, turns=100). Note that no plot is generated when bottom=="new". (b) If 'bottom' is FALSE or 0, no bottom is plotted or removed (default). (c) If 'bottom' is TRUE or 1, the segmentation mask holding the bottom (sgbt) is removed from the data, and a surface generated by cpplot3d.bottom.TSD() based on points generated by pplot3d.TSD() is plotted. (d) If bottom==2, the segmentation mask holding the bottom, and all voxels below these voxels, are removed from the data, and the surface plotted as in (c). (e) If 'bottom' is negative, the same action is taken as for positive values, but the bottom surface is not plotted (only removing the bottom segmentation mask, and everything below in the case that bottom=-2).
#' @param bottom.res  is the resolution in meters of the grid to which the bottom is estimated. Higher values causes smoother bottom, but more spiky edges. Lower values causes more noise and a risk of missing fields in the bottom surface.
#' @param bottom.col  is the function to use for coloring the bottom surface, generated by colscale(). Possibel values are "rainbow", "grey", "heat.colors", "terrain.colors", "topo.colors", "cm.colors", and "combined". Use ... to set parameters of the color function, such as 'start' and 'end'. See colscale() for explaination of parameters of the color functions.
#' @param bottom.N  is the number of points generated by pplot3d.TSD() upon which the generated bottom surface is calculated by interpolation. High values may lead to more accurate bottom surface, but will also be time demanding.
#' @param bottom.seg  is a list of segmentation parameters used when segmenting out the bottom or selecting previously generated segmentation masks. Do not change these settings unless the preformance of the segmentation method is unsatisfactory for the specific data set. Specifically, 'sgth' denotes that the default segmentation threshold is raised to the power 'sgth' in order to allow for large segmentation mask covering all points of the bottom; 'misM' is the input mean volume backscatterinf coefficient specifying the levels of the bottom; 'bwGp' is the bandwidth in meters of the Gaussian kernel used in echoIBM.segment.event(); and 'turns' is the number of pings used in each turn of the segmentation method, where higher values leads to fewer edge effects in the bottom surface.
#' @param adds  is an optional list of variables overriding the variables in 'data'.
#' @param xlim  the x limits (x1, x2) of the plot. Can only be used to increase the plotting frame. Note that x1 > x2 is allowed and leads to a ‘reversed axis’. If one of xlim, ylim or zlim is NA, all the data and the entire sonar frame are included in the plot.
#' @param ylim  the y limits (y1, y2) of the plot. Can only be used to increase the plotting frame. Note that x1 > x2 is allowed and leads to a ‘reversed axis’. If one of xlim, ylim or zlim is NA, all the data and the entire sonar frame are included in the plot.
#' @param zlim  the z limits (z1, z2) of the plot. Can only be used to increase the plotting frame. Note that x1 > x2 is allowed and leads to a ‘reversed axis’. If one of xlim, ylim or zlim is NA, all the data and the entire sonar frame are included in the plot.
#' @param view  defines the view point of the user and has 4 possible values:
#' @param zoom  is the zoom value of the plot, utilized in rgl.viewpoint().
#' @param fov  is the field of view of the plot, utilized in rgl.viewpoint().
#' @param light  sets the angle of the rgl light source, given as c(azimuth angel, elevation angle) (default is no light source, light = NULL).
#' @param school  is a logical indicating whether fish positions stored in .school files present in the event specified by 'event' (and 'cruise' and 'dir.data') are to be plotted with colors 'col.school' and sizes 'size.school'.
#' @param schoolcol  is the color of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsize  is the size of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsample  is the proportion of the fish positions given by the .school specified by 'school', that are to be sampled for plotting. Useful when the number of fish is large.
#' @param schoollen  is the length of the fish when plotted as lines.
#' @param schoollwd  is the width of the fish when plotted as lines.
#' @param schooltype  is either "p" (default) for plotting points a the fish positions, "l" for plotting the fish as lines in the direcitons of the fish, or "o", for plotting lines with a dot at the head of the fish. Several values can be given.
#' @param schoolnumt  is the number of time steps of the school, specified if the school data has been recycled and thus should be recycled when plotting as well.
#' @param schoolcrop  is TRUE to discard school positions to be plotted outside of the specified plotting frame.
#' @param plot.seg  is TRUE if the segmentation volume used for segmenting out the school is to be plotted, and the calculated total echo returned. If plot.seg==FALSE plotting of the volume will not be done but the total echo inside the volume returned. If plot.seg==NULL the total echo will not be calculated or plotted. Specify the segmentation volume with the following parameters: 'object', 'par', 'center', 'angle', 'seg.col' and 'seg.alpha'.
#' @param seg.col  is the color to use for the segmentation volume.
#' @param seg.alpha  is the transparacy parameter to use for the segmentation volume.
#' @param subdivide  is the density of points on the plotted segmentation ellipsoid.
#' @param excl.neg  is FALSE if negative mass values are to be included in the calculation of the centers of mass of the school.
#' @param object  is string representing the type of object to use. Currently implemented are "ellipsoid" and "cuboid" (may be abbreviated).
#' @param par  is a vector of three elements representing the semi axis lengths for ellipsoid and the x-width, y-depth and z-height of cuboid.
#' @param center  is a vector of three elements representing the centre position of the object.
#' @param angle  is the angle of the major axis of the ellipsoid in the x-y-plane, in the case that object=="ellipsoid".
#' @param aspect  is used to set the dimension of the plotting frame (see aspect3d()).
#' @param nticks  is either a single integer specifying the suggested number of ticks used on the axes, or a vector of three elements giving the number of ticks on each axis, in which case the axes will NOT move so that they may obscure the data.
#' @param origin  is (1) a vector of two elements representing the origin of the global coordinate system (G), (2) the numbering index of the ping in the total sequence of pings of the event, which is to be regarded as the origin of (G) (ignoring heave so that the x-y-plane of (G) is on the surface of the sea), or (3) NULL, implying that the origin be put to the mid point of the vessel posistions.
#' @param xlab  is the label for the x axis.
#' @param ylab  is the label for the y axis.
#' @param zlab  is the label for the z axis.
#' @param edge.vpos  defines the placement of the vessel position, where edge.vpos="x-+" places the vessel position information on the upper south edge of the plot.
#' @param line.vpos  adjusts the position of the text by the given number of lines
#' @param at.vpos  adjusts the position left/right (see mtext3d()).
#' @param cex.vpos  is the character expansion of the vessel information.
#' @param col.vpos   is the color for the vessel position information.
#' @param clock  is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
#' @param cex.clock  is the numeric character expansion value for the clock.
#' @param adj.clock  is used in text3d() when plotting the date and time.
#' @param format.clock  is the format of the date and time, specified as the input to utim2ftim(). Default is "yyyy-mm-dd\\nHH:MM:SS.FFF" resulting in date and time separated by a line break of width according to the value of 'lsp.clock'. All "\\n" apparing in 'format.clock' cause a line break.
#' @param digits.clock  is a numeric specifying the number of digits of the seconds part of the time.
#' @param lsp.clock  is the spacing factor for the line break, given as a multiple of the corresponding x-, y- or zlim. Default is 0.04.
#' @param col.clock  is the color of the plotted time and date.
#' @param sonar.grid  is a string representing whether to plot the sonar grid as a projection onto the surface ("proj"), as a grid around the sonar volume ("frame") or no grid ("").
#' @param sonar.grid.col  is a vector of two strings specifying the color to use for plotting the sonar grid edges, where the first is the main color, and the second is the color to use for lines that are above the plotting frame, but that are plotted onto the top of the frame.
#' @param sonar.grid.lwd  is the line width of the sonar grid.
#' @param cs.pos  defines the coordinate system to use when generating the points. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel. If given as a three element vector, 'cs.pos' is interpreted as c(cs.pos,cs.view,cs.xyzlim).
#' @param cs.view  defines the coordinate system to use when specifying the view point. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel.
#' @param sides  is a vector of two strings specifying on which sides of the sonar volume to plot edges of the beams and bows, respectively, where "t" is "top", "r" is "right", "b" is "bottom" and "l" is "left".
#' @param dens  is a vector of 4 elements specifying 
#' @param every  (used in plot_volume.ME70()) is a single integer representing that the frame should be plotted every 'every' time step, where the first and last are included, or a vector of integers givint the time steps for which the frame should be plotted (not including the first and last time step).
#' @param global.grid  is the spacing between the gridlines of the global grid, plotted if 'global.grid' is not FALSE or empty. If global.grid==TRUE, the grid width is set to 100.
#' @param global.grid.lwd  is the line width of the global grid lines.
#' @param global.grid.lty  is the line type of the global grid lines.
#' @param ...  are inputs passed on to  add.sonar.grid(), pplot3d.TSD, and decorate3d(). Also the direction of the light source in the plot is given here by the variables 'theta' and 'phi' (see rgl.light, default is to clear the light source). To set color and character size of labels and axis annotation, use cex.lab, cex.axis, col.lab and col.axis.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom echoIBM sonR_implemented_ind echoIBM.generate_dynschool
#' @importFrom rgl clear3d light3d
#' @importFrom sonR read.event
#' @importFrom TSD labl.TSD ones strff
#'
#' @export
#' @rdname cpplot3d.event
#'
cpplot3d.event<-function(
	# (-15) Specifying whether to use cplot3d or pplot3d:
	cpplot3d_type, 
	# (-14) Used in read.event() and elsewhere in cpplot3d.event():
	event=1, t=1, turns=10, cruise=2009116, TVG=TRUE, TVG.exp=2, dir.data=NULL, Paout=TRUE, exact=FALSE, bgns=TRUE, pdns=TRUE, nrns=TRUE, hins=TRUE, kern=NULL, segpar=NULL, pamkpar=list(), nsind=0.75, hins_add=10, pdns_scale=1e-14, TOV=0, cal=1, bmmd=NULL, 
	# (-13) Used in cplot3d.TSD():
	breaks=40, col="combined", colpar=list(start=0,end=0.8,flip=TRUE), clamp=c(0,1), shrink=TRUE, null.value=NA, 
	# (-12) Used in cplot3d.plot.color.bar():
	white=1, log=TRUE, color.bar="y--", color.bar.lwd=8, color.bar.adj=0, color.bar.tadj=0.1, color.bar.noWhite=TRUE, color.bar.nticks=8, color.bar.tickw=0.005, color.bar.tickcol="black", db=TRUE, voxels.out=FALSE, endcol=c("", ""), 
	# (-11) Used in pplot3d_sv2pos.TSD() and elsewhere in pplot3d.event():
	N=1e5, acca=NULL, fun="mod", allert=1e8, nlim=NULL, fact=NULL, cols=c("black","navyblue","magenta4","red3","darkorange2","yellow"), stretch=1, scale.voxels=1, rand.gen=c("unif","beta","norm"), possample=1, 
	# (-10) Used in pplot3d.TSD() and cplot3d.TSD():
	esnm="MS70", var=c("vbsc","sgsc","pr0s","sgs0","sgsE","sgsi","sgsI","psis","tlns"), ind=list(-(1:150),NULL), range=list(), subset=NULL, ideal=TRUE, seabed=-12000, rot=2, compensation=c("pitch","roll"), plot=TRUE, cs.xyzlim="g", add=FALSE, beamstypes=1, size=cplot3d.size("pow",y=20,par=4), 
	# (-9) Used in cpplot3d.bottom.TSD():
	bottom=FALSE, bottom.res=5, bottom.smooth=NULL, bottom.col="topo.col", bottom.N=1e5, bottom.seg=list(sgth=0.2, misM=1e-3, bwGp=2, turns=100, ind=list(-(1:100), NULL)), 
	# (-8) Used for plotting with plot3d():
	adds=NULL, xlim=NULL, ylim=NULL, zlim=NULL, view=c("free","top","bottom","south","west","north","east"), zoom=0.7, fov=60, 
	# (-7) Used for plotting the school:
	school=FALSE, schoolcol="purple", schoolsize=0.3, schoolsample=0.01, schoollen=4, schoollwd=1, schooltype="p", schoolnumt=NULL, schoolcrop=FALSE, 
	# (-6) Used for plotting the school (when schoolsample is a character = "obj") and the segmentation object:
	plot.seg=FALSE, seg.col="green", seg.alpha=0.2, subdivide=3, excl.neg=TRUE, object=c("ellipsoid","cuboid"), par=double(3), center=c(0,0,0), angle=0, 
	# (-5) Used when plotting frame bounding box, aspect, titles, and axes, and else throughout cpplot3d.decorate():
	aspect="iso", nticks=5, origin=1, xlab="x", ylab="y", zlab="z", full.box=FALSE, 
	# (-4) Used when plotting vessel position:
	edge.vpos="", line.vpos=0, at.vpos=NULL, cex.vpos=1, col.vpos="blue", 
	# (-3) Used when plotting date and time:
	clock="bbl", cex.clock=1, adj.clock=c(0,0), format.clock="Ping: indt\nyyyy-mm-dd\nHH:MM:SS.FFF", digits.clock=2, lsp.clock=0.04, col.clock=4, 
	# (-2) Used when plotting the sonar grid:
	sonar.grid="frame", sonar.grid.col=c("orange","cornflowerblue"), sonar.grid.lwd=1, cs.pos="g", cs.view="g", sides=c("tb","tb"), dens=c(200,100,100,1), every=Inf, 
	# (-1) Used when plotting the global grid:
	global.grid=FALSE, global.grid.lwd=0.5, global.grid.lty=1, 
	# (0) Passed on to add.sonar.grid(), pplot3d.TSD, and decorate3d():
	...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-02 - Clean version.
	# Update: 2010-02-19 - Changed to supporting the subset list 'subset', used in extract.
	# Update: 2010-03-02 - Added support for specifying the ranges of "x", "y" and "z" ('range').
	# Update: 2010-04-17 - Altered to letting the first and the last break move to infinity.
	# Update: 2011-01-06 - Added the option 'null.value'.
	# Update: 2011-01-17 - Added the option 'shrink'.
	# Update: 2011-01-17 - Simplified using the new cplot3d.TSD(), that was expanded to support multiple time steps in the input 'data'.
	# Update: 2011-10-02 - Changed to add axes and bounding box at the end of the function, and added the option 'beams' for use when plotting sonar grid og the ME70 echosounder. Also added plotting of sonar grid at the end of the funciton, using add.sonar.grid().
	# Update: 2012-03-15 - Added the options 'nticks' and 'edge' for customizing axes individually.
	# Update: 2012-06-28 - Added support for plotting 'pr0s', 'sgsc' and 'sgs0', specified by 'type'.
	# Update: 2012-11-15 - Added 'color.bar.noWhite'.
	# Update: 2013-01-05 - Added the option type="tlns".
	# Update: 2013-08-19 - Restructured function and categorized inputs.
	# Update: 2013-10-07 - Added the option of var[1]="-vbsc" indicating disabeling plotting of the acoustic data.
	# Update: 2013-10-07 - Merged cplot3d.event() and pplot3d.event().
	# Update: 2014-04-03 - Added 'schoolnumt'.
	# Update: 2014-05-27 - Updated to include 'indt'.
	# Last: 2015-03-10 - Added 'light' for setting the angle of the rgl light source (default is no light source).
	
	
	##################################################
	##################################################
	########## Preparation ##########
	ind <- sonR_implemented_ind(ind)
	# Allow for ftim input in 't':
	#if(nchar(t[1])>=13){
		t <- read.event(event, t=t, var="indt")$indt
	#	}
	if(strff("h", bmmd)){
		t <- locateNonVerticalBeamMode(t, event)
	}
	if(length(t)==0){
		warning("None of the requested time steps given in 't' present in the data (with the corre)")
		return(list())
	}
	
	# Get the time list used when reading the data:
	tlist <- split(t, rep(seq_len(ceiling(length(t)/turns)), each=turns)[1:length(t)])
	# Define the vessel variables and the beams variables to read:
	vesselvar <- labl.TSD("v")
	beamsvar <- labl.TSD("rb")
	
	# If 'cs.pos' has length 3, it specifies all of 'cs.pos', 'cs.view', 'cs.xyzlim', in that order:
	if(length(cs.pos)==3){
		cs.view <- cs.pos[2]
		cs.xyzlim <- cs.pos[3]
		cs.pos <- cs.pos[1]
		}
	
	########## Execution ##########
	# Read the data used for plotting the sonar grid:
	### data <- read.event(var=c("time",vesselvar,beamsvar), cruise=cruise, event=event, esnm=esnm, t=t, adds=adds, TVG=TVG, TVG.exp=TVG.exp, ideal=ideal, seabed=seabed, rot=rot, compensation=compensation, origin=origin, dir.data=dir.data, drop.out=TRUE, strip.out=TRUE, dir.out=FALSE, Paout=Paout, merge=TRUE, cs=cs.pos, exact=exact, TOV=TOV)
	data <- read.event(var=c("time", "vessel", "beams"), cruise=cruise, event=event, esnm=esnm, t=t, adds=adds, TVG=TVG, TVG.exp=TVG.exp, ideal=ideal, seabed=seabed, rot=rot, compensation=compensation, origin=origin, dir.data=dir.data, drop.out=TRUE, strip.out=TRUE, dir.out=FALSE, Paout=Paout, merge=TRUE, cs=cs.pos, exact=exact, TOV=TOV)
	if(sonar.grid[1] %in% c("frame","proj")){
		xyzlim <- NULL
		if(suppressWarnings(any(is.na(xlim),is.na(ylim),is.na(zlim)))){
			# Apply sides=c("tb","tb") to include the entire sonar volume:
			xyzlim <- add.sonar.grid(data, xyzlim=NULL, xyzlim.out=TRUE, sonar.grid=sonar.grid, cs.pos=cs.pos, cs.view=cs.view, sides=c("tb","tb"), dens=dens, every=every, ...)
			xlim <- xyzlim[,1]
			ylim <- xyzlim[,2]
			zlim <- xyzlim[,3]
			}
		}
	
	# Define the variables to read from the event, depending on the value of 'schoolsample' and 'school'. If 'schoolsample' is a string, this indicates that the school object should be plotted, if available. Else if 'school' is TRUE, the fish positions are attempted read from the event. See also the code below reading the variables 'thisvar' with read.event():
	thisvar <- c("utim", "indt", "voxels", vesselvar, beamsvar)
	compactSchoolvar <- labl.TSD("cs")
	if(is.character(schoolsample)){
		schoolvar <- c("Sctr", "Spar", "Sobj", "Sang")
		}
	else{
		if(isTRUE(school)){
			#schoolvar <- c("psxf","psyf","pszf")
			schoolvar <- c("psxf", "psyf", "pszf")
			if(any(strff(c("l", "o"), schooltype))){
				schoolvar <- c(schoolvar, "rtxf", "rtzf")
				}
			}
		else{
			schoolvar <- NULL
			}
		}
	
	# Generate new bottom segmentation data if required:
	if(strff("n",bottom)){
		# Defaults:
		if(length(bottom.seg$sgth)==0){
			bottom.seg$sgth <- 0.2
			}
		if(length(bottom.seg$misM)==0){
			bottom.seg$misM <- 1e-3
			}
		if(length(bottom.seg$bwGp)==0){
			bottom.seg$bwGp <- 2
			}
		if(length(bottom.seg$turns)==0){
			bottom.seg$turns <- 100
			}
		if(length(bottom.seg$ind)==0){
			bottom.seg$ind <- list(-(1:100), NULL)
			}
		# Segmentation of the bottom:
		echoIBM.segment.event(event=event, t=t, misM=rep(bottom.seg$misM,length(t)), ind=bottom.seg$ind, range=bottom.seg$range, subset=bottom.seg$subset,  h=bottom.seg$bwGp, var="sgbt", pow=bottom.seg$sgth, turns=bottom.seg$turns)
		
		cat("Bottom segmentation created. No plot generated. To plot, set 'bottom' different from \"new\"")
		return()
	}
	
	# The following implements the value of 'bottom'. For negative values, do not plot the bottom surface. For absolute value equal to 1, remove the segmentation mask of the bottom ('sgbt'). For absolute value equal to 2, remove the bottom and all points below the bottom:
	if(abs(bottom)>0){
		var[2] <- "-sgbt"
		}
	# Also, if bottom==2, add a minus at the end of var[2] to discard plotting data below the bottom:
	if(abs(bottom)==2){
		var[2] <- paste0(var[2], "-")
		}
	# If bottom==-1, discard the bottom but do not plot the bottom surface:
	if(bottom<0){
		bottom <- FALSE
		}
	
	# If the first element of 'var' has a minus, discard plotting the data. This works independently of the value of 'bottom':
	if(substr(var[1], 1, 1) == "-"){
		var[1] <- substr(var[1], 2, 5)
		plot <- FALSE
		}
	
	# If the second element of 'var' starts with a minus or a plus, read the data given in var[2] (the minus or plus is removed in read.event()):
	if(var[1] %in% labl.TSD(var="as")){
		thisvar <- c(thisvar, var[1])
		if(substr(var[2], 1, 1) %in% c("+", "-")){
			thisvar <- c(thisvar, var[2])
			}
		}
	else{
		if(!var[1] %in% c("vbsC", "vbsA")){
		 	warning("'var' has the wrong value. Acoustic data 'vbsc' used")
		 	}
		thisvar <- c(thisvar, "vbsc")
		}
	
	# Add unthresholded interpolated segmentation mask if the interpolated probabilities of school 'psis' are requested for plotting:
	if(var[1]=="psis"){
		thisvar <- c(thisvar, var[1], "sgsI")
		}
	
	# !!! The following warning concerns plotting clusters of segmentation data, which is not supported at in the current version. However, the code is kept for future use:
	# Add clustering of segmented voxels to the plotting, with different color for the different clusters:
	if(length(pamkpar)>0 && is.list(pamkpar)){
		thisvar <- c(thisvar, "clsz")
		}
	
	### Read and plot the pings in turns: ###
	# TIME <- TRUE speeds up the plotting, since the time information store in the UNIX_time-files are only read once:
	TIME <- TRUE
	# Define the variables to return from the plotting function, mostly time information, segmentation variables, and the parameters from pplot3d.TSD() or cplot3d.TSD:
	out <- list()
	outnames <- c("utim", # Unix time
	"view", "xyzlim", # Common for cplot3d.TSD() and pplot3d.TSD()
	"tvbs", "tvol", "aniS", "psxS", "psyS", "pszS", "nseg", "anio", "psxo", "psyo", "pszo", "szxo", "szyo", "szzo", "ango", "typo", # Returned from echoSegment.TSD()
	"misM", # Derived from the output of echoSegment.TSD()
	#"finalN", "finalacca", "idx", # Parameters specific for pplot3d.TSD()
	"finalN", "finalacca", # Parameters specific for pplot3d.TSD()
	"plotted", "br",
	"psxr", "psyr", "pszr", # Parameters specific for cplot3d.TSD() 
	"psxg", "psyg", "pszg", "nbrp", "vbsC", "vbsA", # Parameters specific for cpplot3d.TSD()
	labl.TSD("v")) # Vessel names
	
	# Add variables to read if the bottom is to be plotted. Also define the list of regenerated bottom points used in the plotting of the bottom surface after the plotting of the data:
	if(abs(bottom)>0){
		thisvar <- c(c("sgbt", "psxx", "psyx"), thisvar)
		segpar <- bottom.seg
		p <- list(psxr=vector("list", length(tlist)), psyr=vector("list", length(tlist)), pszr=vector("list", length(tlist)))
		}
	
	### # Plot a time bar showing the progress of the reading and plotting:
	### infostring <- paste("Reading ",if(plot) "and plotting pings:",sep="")
	### cat(infostring,"\n",sep="")
	### totalsteps <- length(tlist)
	### stepfact <- nchar(infostring)/totalsteps
	### oldvalue <- 0
	
	# Clear the scene:
	if(!add){
		clear3d()
		}
	
	# Loop through the 'turns' and color plot the data:
	for(i in seq_along(tlist)){
		### # Print a dot if the floor of the new value exceeds the old value in:
		### thisvalue <- floor(i*stepfact)
		### if(thisvalue > oldvalue){
		### 	cat(rep(".",thisvalue-oldvalue),if(i==totalsteps) "\n", sep="")
		### 	oldvalue <- thisvalue
		### 	}
		
		# Read the data:
		read.event_parlist <- list(cruise=cruise, adds=adds, esnm=esnm, TVG=TVG, TVG.exp=TVG.exp, ideal=ideal, seabed=seabed, rot=rot, compensation=compensation, origin=origin, dir.data=dir.data, drop.out=FALSE, strip.out=TRUE, dir.out=FALSE, Paout=Paout, merge=TRUE, ind=NULL, cs=cs.pos, exact=exact, bgns=bgns, pdns=pdns, nrns=nrns, hins=hins, kern=kern, segpar=segpar, pamkpar=pamkpar, nsind=nsind, hins_add=hins_add, pdns_scale=pdns_scale, TIME=TIME, TOV=TOV)
		
		s <- do.call("read.event", c(read.event_parlist, list(event=event, var=thisvar, t=tlist[[i]], pad=TRUE, split=TRUE, cal=cal, msg=FALSE)))
		if("TIME" %in% names(s)){
			TIME <- s$TIME
			}
		
		# Add school data from the directory given in 'school':
		ss <- list()
		if(isTRUE(school)){
			ss <- do.call("read.event", c(read.event_parlist, list(event=event, var=c(schoolvar,compactSchoolvar), t=if(length(schoolnumt)>0) (tlist[[i]]-1) %% schoolnumt + 1 else tlist[[i]])))
			}
		else if(is.character(school) && isTRUE(file.info(school)$isdir)){
			ss <- do.call("read.event", c(read.event_parlist, list(event=school, var=c(schoolvar,compactSchoolvar), t=if(length(schoolnumt)>0) (tlist[[i]]-1) %% schoolnumt + 1 else tlist[[i]])))
			school <- TRUE
			}
		if(length(ss$psxS)>0){
			vesselutim <- read.event(cruise=cruise, event=event, esnm=esnm, var="utim",t="all")$utim
			ss <- echoIBM.generate_dynschool(c(ss,s), t=tlist[[i]], vesselutim=vesselutim, adds=adds)
			}
		schoolVarToFillIn <- intersect(names(ss), schoolvar)
		s[schoolVarToFillIn] <- ss[schoolVarToFillIn]
		rm(ss)
			
		# Plot the points:
		ll <- list(...)
		
		if(strff("c", cpplot3d_type)){
			size <- cplot3d.getsize(size)
			# Define the list of variables used as input to pplot3d.TSD(), given in the order used in that function:
			thisl <- list(
				# Main variable:
				data=s, 
				# Used in cplot3d.TSD():
				breaks=if(i==1) breaks else thisout$br, col=col, colpar=colpar, clamp=if(i==1) clamp else 0:1, shrink=shrink, null.value=null.value, 
				# Used in cplot3d.plot.color.bar():
				white=white, log=log, color.bar=if(i==length(tlist) && !add) color.bar else FALSE, color.bar.lwd=color.bar.lwd, color.bar.adj=color.bar.adj, color.bar.tadj=color.bar.tadj, color.bar.noWhite=color.bar.noWhite, color.bar.nticks=color.bar.nticks, color.bar.tickw=color.bar.tickw, color.bar.tickcol=color.bar.tickcol, db=db, voxels.out=voxels.out, endcol=endcol, 
				# Used in pplot3d.TSD() and cplot3d.TSD():
				esnm=esnm, var=var, ind=ind, range=range, subset=subset, ideal=ideal, seabed=seabed, rot=rot, compensation=compensation, plot=plot, cs.xyzlim=cs.xyzlim, add=if(i==1) add else TRUE, beamstypes=beamstypes, size=size, 
				# Used for plotting with plot3d():
				xlim=xlim, ylim=ylim, zlim=zlim, view=view, zoom=zoom, fov=fov, 
				# Used for plotting the school:
				school=school, schoolcol=schoolcol, schoolsize=schoolsize, schoolsample=schoolsample, schoollen=schoollen, schoollwd=schoollwd, schooltype=schooltype, 
				# Used for plotting the school (when schoolsample is a character = "obj") and the segmentation object:
				plot.seg=plot.seg, seg.col=seg.col, seg.alpha=seg.alpha, subdivide=subdivide, excl.neg=excl.neg, object=object, par=par, center=center, angle=angle, 
				# Used when plotting frame bounding box, aspect, titles, and axes, and else throughout cpplot3d.decorate() (note that bounding box and axes are plotted below, and not at this point):
				box=FALSE, axes=FALSE, origin=origin, xlab="", ylab="", zlab="", 
				# Used when plotting vessel position (note that vessel positions are plotted below, and not at this point):
				edge.vpos=FALSE, 
				# Used when plotting date and time (note that date and time are plotted below, and not at this point):
				clock=FALSE, 
				# Used when plotting the sonar grid (note that the sonar grid is plotted below, and not at this point):
				sonar.grid=FALSE, cs.pos=cs.pos, cs.view=cs.view, 
				# Used when plotting the global grid (note that the global grid is plotted below, and not at this point):
				global.grid=FALSE)
			
			# Define the variables present in '...' but not in the list 'thisl':
			otherl <- ll[setdiff(names(ll),names(thisl))]
			thisout <- do.call("cplot3d.TSD",c(thisl,otherl))
			}
		else if(strff("p", cpplot3d_type)){
			if(!is.numeric(size)){
				size <- 0.3
				}
			# !!! The following warning concerns plotting clusters of segmentation data, which is not supported at in the current version. However, the code is kept for future use:
			if(length(s$clsz)>0){
				warning("Color settings overrided by the clusters of segmentation data when 'pamkpar' is a list")
				cols[[1]]
				}
			# Warning if 'acca' is not given when plotting multiple pings:
			if(length(acca)==0 && length(t)>1){
				warning("When multiple time steps are plotted, 'acca' should be given to avoid fitting 'acca' to each time step separately, causing varying target strength of the points depending on the total backscattered energy.")
				}
			# Define the list of variables used as input to pplot3d.TSD(), given in the order used in that function:
			thisl <- list(
				# Main variable:
				data=s, 
				# Used in pplot3d_sv2pos.TSD() and elsewhere in pplot3d.event():
				N=N * length(tlist[[i]])/length(t), acca=acca, fun=fun, allert=allert, nlim=nlim, fact=fact, cols=cols, stretch=stretch, scale.voxels=scale.voxels, rand.gen=rand.gen, possample=possample, 
				# Used in pplot3d.TSD() and cplot3d.TSD():
				esnm=esnm, var=var, ind=ind, range=range, subset=subset, ideal=ideal, seabed=seabed, rot=rot, compensation=compensation, plot=plot, cs.xyzlim=cs.xyzlim, add=if(i==1) add else TRUE, beamstypes=beamstypes, size=size, 
				# Used for plotting with plot3d():
				xlim=xlim, ylim=ylim, zlim=zlim, view=view, zoom=zoom, fov=fov, 
				# Used for plotting the school:
				school=school, schoolcol=schoolcol, schoolsize=schoolsize, schoolsample=schoolsample, schoollen=schoollen, schoollwd=schoollwd, schooltype=schooltype, 
				# Used for plotting the school (when schoolsample is a character = "obj") and the segmentation object:
				plot.seg=plot.seg, seg.col=seg.col, seg.alpha=seg.alpha, subdivide=subdivide, excl.neg=excl.neg, object=object, par=par, center=center, angle=angle, 
				# Used when plotting frame bounding box, aspect, titles, and axes, and else throughout cpplot3d.decorate() (note that bounding box and axes are plotted below, and not at this point):
				box=FALSE, axes=FALSE, origin=origin, xlab="", ylab="", zlab="", 
				# Used when plotting vessel position (note that vessel positions are plotted below, and not at this point):
				edge.vpos=FALSE, 
				# Used when plotting date and time (note that date and time are plotted below, and not at this point):
				clock=FALSE, 
				# Used when plotting the sonar grid (note that the sonar grid is plotted below, and not at this point):
				sonar.grid=FALSE, cs.pos=cs.pos, cs.view=cs.view, 
				# Used when plotting the global grid (note that the global grid is plotted below, and not at this point):
				global.grid=FALSE)
			
			# Define the variables present in '...' but not in the list 'thisl':
			otherl <- ll[setdiff(names(ll),names(thisl))]
			thisout <- do.call("pplot3d.TSD",c(thisl,otherl))
			}
		else{
			stop("Invalid values of 'cpplot3d_type'. Should be one of \"c\" and \"p\"")
			}
		# Set the light of the plot:
		if(length(ll$theta)>0 && length(ll$phi)>0){
			clear3d(type="lights")
			light3d(theta=ll$theta, phi=ll$phi)
		 	}
		
		
		# Add to the output:
		for(thisname in outnames){
			# Merge in the list case:
			if(is.list(thisout[[thisname]])){
				# If the current element is a list of length 1 containing a list, then pick out that first and only element:
				if(length(thisout[[thisname]])==1 && is.list(thisout[[thisname]][[1]])){
					out[[thisname]] <- c(out[[thisname]], thisout[[thisname]][[1]])
					}
				else{
					out[[thisname]] <- c(out[[thisname]], thisout[[thisname]])
				}
			}
			else if(length(thisout[[thisname]]) %in% c(1, length(tlist[[i]]))){
				out[[thisname]]	<- c(out[[thisname]], c(thisout[[thisname]]))
			}
			else if(length(thisout[[thisname]])){
				out[[thisname]]	<- c(out[[thisname]], list(thisout[[thisname]]))
			}
		}
		
		# Save the regenerated bottom points, used for plotting the bottom surface below this for loop:
		if(bottom>0){
			bottom.stretch <- 5
			bottom.nlim <- 5^(1:10)
			bottom.fact <- 1/sqrt(bottom.nlim)
			bottom.cols <- ones(length(bottom.fact)+1)
			bottom.kern <- NULL
			# Add the bottom points:
			suppressWarnings(thisp <- pplot3d.TSD(s, N=bottom.N*length(tlist[[i]])/length(t), fun="mod", nlim=bottom.nlim, fact=bottom.fact, cols=bottom.cols, stretch=bottom.stretch, plot=FALSE, add=TRUE, esnm=esnm, var=c("vbsc","+sgbt"), kern=bottom.kern, segpar=segpar, rand.gen="beta", scale.voxels=2))
			p$psxr[[i]] <- unlist(thisp$psxr)
			p$psyr[[i]] <- unlist(thisp$psyr)
			p$pszr[[i]] <- unlist(thisp$pszr)
			}
		}
	if(strff("p", cpplot3d_type)){
		cat("Points drawn: ", 
			sum(out$finalN), 
			if(length(acca)==0) paste0(" (", N, " specified)"), 
			paste0(". (Range of 'acca' :", paste(range(out$finalacca), collapse=" - "), ")"), 
			"\n", sep="")
		}
			
	# Plot the bottom surface:
	if(bottom>0){
		# Check for missing segmentation data:
		atsgbt <- which(sapply(TIME$labl, function(x) length(grep("sgbt", x))>0))
		present_sgbt <- unique(unlist(TIME$indt[atsgbt]))
		missing_sgbt <- setdiff(t, present_sgbt)
		if(length(missing_sgbt)>0){
			warning(paste0("The following time steps are missing in the bottom segmentation data: ", paste(missing_sgbt, collapse=", "), "\nTo generate new bottom segmentation data, run use bottom=\"new\" and specify 'ind' and 'range' and other specifics for the segmentation used in echoIBM.segment.event() in the list 'bottom.seg'"))
			}
		# Plot the bottom surface:
		out$bottom <- cpplot3d.bottom.TSD(c(p, s[c("numb","lenb","psxx","psyx")]), bottom.res=bottom.res, bottom.smooth=bottom.smooth, col=bottom.col, ...)
		}
		
	##### Add the following features to the plot: (1) bounding box and titles, (2) axes, (3) vessel positions, (4) date and time, (5) sonar grid, (6) global grid. If pplot3d.TSD() is called from pplot3d.event(), these features are plotted at the level of pplot3d.event() instead, indicated by the following parameters specified when calling pplot3d.TSD() in pplot3d.event(): box=FALSE, axes=FALSE, title=list(xlab="", ylab="", zlab=""), edge.vpos=FALSE, clock=FALSE, sonar.grid=FALSE: #####
	#if(plot && !add && length(out$xyzlim)>0){
	if(plot && length(out$xyzlim)>0){
		# Extract the limits of the plot:
		if(is.list(out$xyzlim)){
			out$xyzlim <- matrix(unlist(out$xyzlim), nrow=6)
			out$xyzlim <- cbind(c(out$xyzlim[1:2,]), c(out$xyzlim[3:4,]), c(out$xyzlim[5:6,]))
			}
		xyzlim <- apply(out$xyzlim, 2, range)
		# Update the 'xyzlim' with bottom points:
		if(bottom>0){
			xyzlim[1] <- min(xyzlim[1], out$bottom$x)
			xyzlim[2] <- max(xyzlim[2], out$bottom$x)
			xyzlim[3] <- min(xyzlim[3], out$bottom$y)
			xyzlim[4] <- max(xyzlim[4], out$bottom$y)
			}
		
		# Decorate:
		suppressWarnings(cpplot3d.decorate(
			# (1) Used in decorate_pplot_cplot():
			data=data[c("utim", "indt", vesselvar, beamsvar)], plot=TRUE, 
			# (2) Used when plotting frame bounding box, aspect, titles, and axes, and else throughout decorate_pplot_cplot():
			aspect=aspect, xyzlim=xyzlim, nticks=nticks, origin=origin, xlab=xlab, ylab=ylab, zlab=zlab, full.box=full.box, 
			# (3) Used when plotting vessel position:
			edge.vpos=edge.vpos, line.vpos=line.vpos, at.vpos=at.vpos, cex.vpos=cex.vpos, col.vpos=col.vpos, 
			# (4) Used when plotting date and time:
			clock=clock, cex.clock=cex.clock, adj.clock=adj.clock, format.clock=format.clock, digits.clock=digits.clock, lsp.clock=lsp.clock, col.clock=col.clock, 
			# (5) Used when plotting the sonar grid:
			sonar.grid=sonar.grid, sonar.grid.col=sonar.grid.col, sonar.grid.lwd=sonar.grid.lwd, cs.pos=cs.pos, cs.view=cs.view, sides=sides, dens=dens, every=every, 
			# (6) Used when plotting the global grid:
			global.grid=global.grid, global.grid.lwd=global.grid.lwd, global.grid.lty=global.grid.lty, 
			# Passed on to add.sonar.grid():
			...))
		}
	##### End of add features to the plot #####
		
		
	########## Output ##########
	out <- lapply(out, drop)
	if(length(out$plotted)){
		out$plotted <- sum(out$plotted)
	}
	
	invisible(out)
	##################################################
	##################################################
	}

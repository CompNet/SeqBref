# Contient la fonction permettant de produire une représentation visuelle
# de la matrice de transitions issue de l'analyse de séquences.
# 
# 02/2023 Vincent Labatut
# source("src/common/transmat.R")
###############################################################################




###############################################################################
# Computes transition rates and plots the related matrix.
#
# sd: object containing the collection of sequences.
# file.name: base name of the produced files.
###############################################################################
compute.transition.rates <- function(sd, file.name)
{	# with/without considering missing states
	with.missing <- c(TRUE, FALSE)
	for(k in 1:length(with.missing))
	{	# without/with empty rows
		sfx <- c("nonempty","all")
		for(i in 1:length(sfx))
		{	# compute transition matrix
			trate.mat <- seqtrate(sd,				# data
					with.missing=with.missing[k]	# whether to take missing states into account
			)		
			idxr <- which(apply(trate.mat, 1, sum)>0)
			idxc <- which(apply(trate.mat, 2, sum)>0)
			trate.mat <- trate.mat * 100
			#trate.mat[which(trate.mat==0)] <- NA
						
			# plot transition matrix
			plot.file <- paste0(file.name, paste0("transition_rates__miss=",with.missing[k],"_rows=",sfx[i]))
			for(format in c("pdf","png"))
			{	if(format=="pdf")
					pdf(paste0(plot.file,".pdf"), width=15, height=15)
				else if(format=="png")
					png(paste0(plot.file,".png"), width=1024, height=1024)
				par(mar=c(5.4, 5.4, 2.6, 4.1))	# margins B L T R 
				plot(if(sfx[i]=="all") trate.mat else trate.mat[idxr,idxc],		# matrix
					col=viridis,												# colors
					#col=colorRampPalette(c("yellow",'red')),					# colors
					#breaks=50,
					breaks=log_breaks(n=10,base=10)(trate.mat[trate.mat>0]),
					digits=2,													# display values, with 2 decimal digits
					las=2,														# labels orientation
					xlab=NA,	#"Etat posterieur",								# x axis title
					ylab=NA,	#"Etat anterieur",								# y axis title
					main="Taux de transition"									# main title
				)
				dev.off()
			}
		}
		
		# record transition rates
		tab.file <- paste0(file.name, paste0("transition_rates__miss=",with.missing[k],".txt"))
		write.table(trate.mat, tab.file, quote=FALSE, sep="\t")
		#print(round(trate.mat),2)
	}
}




#############################################################
# Builds the transition graph based on the list of sequences,
# and records it as a plot and a graphml file.
#
# sd: object containing the collection of sequences.
# file.name: base name of the produced files.
#############################################################
plot.transition.graph <- function(sd, file.name)
{	seq.folder <- file.path(plots.folder, "sequences")
	
	# init adjacency matrix
	alpha <- alphabet(sd)
	adj <- matrix(0, nrow=length(alpha)+1, ncol=length(alpha)+1, dimnames=list(c(alpha,"*"), c(alpha,"*")))
	
	# init size vector
	state.count <- rep(0, length(alpha)+1)
	names(state.count) <- c(alpha,"*")
	
	# compute the ajacency matrix
	for(s in 1:nrow(sd))
	{	si <- sd[s,]
		prev <- NA
		for(c in 1:ncol(si))
		{	cur <- as.character(si[1,c])
			if(!is.na(prev))
				adj[prev,cur] <- adj[prev,cur] + 1
			state.count[cur] <- state.count[cur] + 1
			prev <- cur
		}
	}
	
	# record adjacency matrix
	adj.file <- paste0(file.name, paste0("transition_graph_adjmat.csv"))
	write.csv(x=adj, file=adj.file, row.names=TRUE, fileEncoding="UTF-8")#, col.names=TRUE)
	
	# remove "*" state
	sc <- which(colnames(adj)=="*")
	adj <- adj[-sc,-sc]
	state.count <- state.count[-sc]
	
	# remove self-edges (loops)
	diag(adj) <- 0
	
	# build the graph based on the adjacency matrix
	g <- graph_from_adjacency_matrix(
			adjmatrix=adj,
			mode="directed",
			weighted=TRUE
	)
	V(g)$occurrences <- state.count
	
	# possibly load the layout
	lay.file <- paste0(file.name, paste0("transition_graph_layout.txt"))
	if(file.exists(lay.file))
	{	lay <- as.matrix(read.table(file=lay.file, row.names=1, header=TRUE))
		lay <- lay[alpha,]
	}
	# otherwise, init and export
	else
	{	lay <- layout_with_fr(g)
		rownames(lay) <- alpha
		colnames(lay) <- c("x", "y")
		write.table(x=lay, file=lay.file)
	}
	
	# graphical parameters
	v.sizes <- 2000 + 2*V(g)$occurrences
	e.widths <- 1 + 0.3*E(g)$weight
	# colors
	v.cols <- cpal(sd)
	e.cols <- c()
	# edge curvature and arrows
	curvature <- c()
	arrows <- c()
	el <- as_edgelist(g, names=FALSE)
	for(r in 1:nrow(el))
	{	curvature <- c(curvature, if(are_adjacent(g, el[r,2], el[r,1])) 0.25 else 0)
		arrows <- c(arrows, if(el[r,1]==el[r,2]) "--" else "->")
		e.cols <- c(e.cols, combine.colors(col1=v.cols[el[r,1]], col2=v.cols[el[r,2]], transparency=66))
	}
	
	# record as a graphml file
	V(g)$color <- v.cols
	E(g)$color <- e.cols
	graph.file <- paste0(file.name, paste0("transition_graph.graphml"))
	write.graph(graph=g, file=graph.file, format="graphml")
	
	# plot bounds
	xmargin <- (max(lay[,1])-min(lay[,1]))*0.1
	xlim <- c(min(lay[,1])-xmargin, max(lay[,1])+xmargin)
	ymargin <- (max(lay[,2])-min(lay[,2]))*0.1
	ylim <- c(min(lay[,2])-ymargin, max(lay[,2])+ymargin)
	
	# plot the graph
	plot.file <- paste0(file.name, paste0("transition_graph"))
	for(format in c("png","pdf"))
	{	if(format=="png")
			png(paste0(plot.file,".png"), width=1024, height=1024)
		else if(format=="pdf")
			pdf(paste0(plot.file,".pdf"))
		
		plot(g,											# graph to plot
			layout=lay,									# layout
			xlim=xlim, ylim=ylim,						# plot y bounds
#			ylim=c(min(lay[,2]),max(lay[,2]*0.1)),		# plot bounds
			vertex.size=v.sizes,						# node size
			vertex.color=v.cols,						# node color
			vertex.label.cex=1.2,						# label size
			vertex.label.family="sans",					# font type
			vertex.label.font=2,						# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
			vertex.label.label.dist=0,					# label distance to node center (0=center)
			vertex.label.color="BLACK",					# label color
			edge.color=e.cols,							# edge color
#			edge.arrow.size=E(g)$weight,				# size of the arrows
			edge.arrow.mode=arrows,						# whether or not to draw the arrow heads
			edge.width=e.widths,						# link thickness
			rescale=FALSE,								# keep the actual coordinates
#			asp=1,										# keep the x/y ratio
			edge.curved=curvature						# specify edge curvature
		)
		dev.off()
	}
}

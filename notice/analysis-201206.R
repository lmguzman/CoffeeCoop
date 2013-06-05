library(Cairo)
library(png)
library(gridBase)

CairoFonts(
           regular="Hoefler Text",
           bold="Hoefler Text:style=Bold",
           italic="Hoefler Text:style=Italic",
           bolditalic="Hoefler Text:style=Bold Italic"
)

oldwd <- setwd("..")
source("R/load.R")
source("R/process-html.R")
source("R/accounts.R")
source("R/process-latex.R")
dat <- load.data()
setwd(oldwd)

d <- read.csv("../data/2012-06-12.csv", as.is=TRUE)

d.coffee <- d[d$Stage != "",]

stage <- d.coffee$Stage
stage[stage %in% c("t", "u")] <- "o"
lvl <- c(m="MSc", c="PhD\n[pre-comps]", d="PhD\n[post-comps]",
         p="Postdoc", f="Faculty", o="Other")
stage <- factor(stage, levels=names(lvl), labels=lvl)
n.mean <- tapply(d.coffee$Coffee, stage, mean, na.rm=TRUE)
n.err <- tapply(d.coffee$Coffee, stage, function(x)
                sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1)))
ylim <- c(0, 60)

CairoPDF("201206-coffee-by-stage.pdf", width=6, height=2, bg="white")
par(mar=c(4.4, 3.1, 1, 0), mgp=c(1.75, .5, 0), tcl=-.2)
x <- barplot(n.mean, col="black", las=2, ylim=ylim,
             space=2/3, cex.names=.75, cex.axis=.75, yaxt="n", )
axis(2, c(0, 20, 40, 60), cex.axis=.75, las=1)
mtext("Coffee count", 2, cex=.75, line=1.75)
arrows(x, n.mean, x, n.mean+n.err, code=2, len=0.05, angle=90)
dev.off()

d.tea <- subset(d, !is.na(Tea), c(Name, Tea))
d.tea$country <- country[match(d.tea$Name, names(country))]
n.tea <- rev(sort(tapply(d.tea$Tea, d.tea$country, mean)))

flags <- lapply(sprintf("../signup/flags/%s.png", names(n.tea)), readPNG)
names(flags) <- names(n.tea)

CairoPDF("201206-tea-by-country.pdf", width=6, height=2, bg="white")
par(mar=c(.5, 3.1, 1, 0), mgp=c(1.75, .5, 0), tcl=-.2)
x <- barplot(n.tea, col="black", las=2, #ylim=ylim,
             space=2/3, cex.names=.75, cex.axis=.75, yaxt="n",
             xaxt="n")
axis(2, cex.axis=.75, las=1)
mtext("Tea count (per person)", 2, cex=.75, line=1.75)
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
for ( i in seq_along(x) ) {
  obj <- rasterGrob(image=flags[[i]], 
                    x=unit(x[i], "native"),
                    y=unit(0, "native"),
                    width=unit(1, "native"),
                    just=c("center", "bottom"))
  grid.draw(obj)
}
popViewport(3)
dev.off()

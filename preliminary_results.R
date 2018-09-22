options(scipen = 999)

library(data.table)
library(jsonlite)
library(ggplot2)
library(gridExtra)

# Dataset -----------------------------------------------------------------


data <- fread('./data/data.csv')
PreliminaryAnalysisMetadata <- jsonlite::fromJSON('results_metdata.json')
Headers <- PreliminaryAnalysisMetadata$Headers
names(data) <- Headers

data <- data[GrossAnualWage > 0 & GrossAnualWage <= 165000,]

# Global Results ----------------------------------------------------------
par(mfrow=c(1,2))
quants <- data.table(Percentiles=data[,quantile(GrossAnualWage, probs = c(0.25,0.5,0.75)),])
rownames(quants) <- c('Percentil 25','Mediana','Percentil 75')
# quants <- rbind(quants,data.table(quantiles=mean(data$GrossAnualWage),Quantiles="Media"))

summary(data$GrossAnualWage)

ggplot(data = data , aes(x = GrossAnualWage)) +
  geom_histogram(
    aes(y = ..density..) ,
    bins = 20,
    fill = "#CBD2D3",
    color = "white",
    alpha = 0.5
  ) +
  scale_x_continuous(breaks = seq(0, 300000, 15000)) +
  geom_density(alpha = 0.5 ,
               size = 0.75,
               color="#ECDC98",
               fill = "#EAB726") +
  geom_hline(yintercept=0, colour="white", size=1) +
  geom_vline(
    data = data,
    aes(xintercept = mean(GrossAnualWage),color = "Media"),
    linetype = "solid",
    size = 0.75
  ) +
  scale_color_manual(values=c("#6E5AFF")) +
  xlab('Salario Bruto Anual (€)') +
  ylab('Densidad') +
  ggtitle('Distribución Global del Salario Bruto Anual Forocochero') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )


dens <- density(data$GrossAnualWage)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.25, 0.5, 0.75)
quantiles <- quantile(data$GrossAnualWage, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) +
  geom_line(color="white") +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
  scale_x_continuous(breaks=quantiles) +
  scale_fill_brewer(guide="none",palette="YlOrBr") +
  xlab('Salario Bruto Anual (€)') +
  ylab('Densidad') +
  ggtitle('Distribución Global del Salario Bruto Anual Forocochero') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  annotation_custom(tableGrob(quants), xmin=105000, xmax=135000, ymin=0, ymax=0.00002)


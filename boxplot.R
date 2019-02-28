boxInspect <- function(i){
    unikeKjoepere_list <- split(dataset,dataset$vBud)

    antRunder <- sapply(unikeKjoepere_list, NROW) 
    antRunder <- sort(antRunder, decreasing = TRUE)
    
    inspeksjon <- dataset[grep(names(antRunder[i]), dataset$vBud),]
    inspeksjon <- inspeksjon[order(inspeksjon$tilslag_flaske), c(5,11,15,16,21, 22, 23,4)]
    inspeksjon$month <- month(inspeksjon$Dato)
    
    theme_box <- function(base_family = "serif"){
      theme_bw(base_family = base_family) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(size = 8),
        axis.ticks.length = unit(-0.05, "in"),
        axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")), 
        axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm"), 
                                   angle=90),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        aspect.ratio = 1,
        legend.background = element_rect(color = "black", fill = "white")
      )
    }
    update_geom_defaults("text", list(size = 3, family = "serif"))
    
    n_fun <- function(x){
      return(data.frame(y = (max(inspeksjon$tilslag_flaske)*1.1),
                        label = length(x)))
    }
    
    plotting <- ggplot(data = inspeksjon, 
           aes(x = as.factor(Dato), y = tilslag_flaske, group=as.factor(Dato))) +
      stat_boxplot(geom ='errorbar', width = 0.6) +
      geom_boxplot(fill = "lightgrey") +
      stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5) +
      expand_limits(y = 0) +
      theme_box() +
      xlab("Auksjoner") +
      ylab("Betalt per flaske")
      scale_y_continuous(sec.axis = dup_axis(label = NULL, name = NULL), #add tick marks
                         expand = expand_scale(mult = c(0, 0)), # remove padding
                         breaks = pretty(c(0,max(inspeksjon$tilslag_flaske)), n = 6), # set ticks
                         labels = comma,
                         limits = c(0,(max(inspeksjon$tilslag_flaske)*1.15))) # set range
    return(plotting)
    }

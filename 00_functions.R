name_the_tiers <- function(df_codescores){
  
  ##############################
  #### Some data processing ####
  ##############################
  # create tier 1 names
  df_codescores <- df_codescores %>% 
    mutate(tier1 = sub("\\d.*", "", dimension),
           tier1 = case_when(
             tier1 == "s"   ~ "Social, Econ., and Polit. Settings (S)",
             tier1 == "rs"  ~ "Resource Systems (RS)",
             tier1 == "gs"  ~ "Governance Systems (GS)",
             tier1 == "a"   ~ "Actors (A)",
             tier1 == "o"   ~ "Outcomes (O)",
             tier1 == "eco" ~ "Related Ecosystems (ECO)",
             tier1 == "i"   ~ "Interactions (I)"
           )) %>% 
    mutate(tier1 = factor(tier1, ordered = TRUE, levels = c(
      
      "Social, Econ., and Polit. Settings (S)",
      "Resource Systems (RS)",
      "Governance Systems (GS)",
      "Actors (A)",
      "Interactions (I)",
      "Related Ecosystems (ECO)",
      "Outcomes (O)"
      
    )))
  
  # create tier 2 names
  df_codescores <- df_codescores %>% 
    mutate(tier3 = case_when(
      dimension == "a1.1.1.actor.group.size.(#.of.cattle)"       ~ "A1.1: Number of relevant actors (# Cattle)",
      dimension == "a1.1.1.actor.group.size.(#.of.sheep/goats)"  ~ "A1.1: Number of relevant actors (# Sheep/goats)",
      dimension == "a2.1.economic.heterogeneity"                 ~ "A2.1: Economic heterogeneity",
      dimension == "a2.2.interest.heterogeneity"                 ~ "A2.2: Interest heterogeneity",
      dimension == "a4.1.leadership.accountability"              ~ "A4.1: Leadership accountability",
      dimension == "a5.1.actor.group.trust"                      ~ "A5.1: Actor group trust",
      dimension == "a5.2.inter-group.trust"                      ~ "A5.2: Inter-group trust",
      dimension == "a7.1.economic.dependence"                    ~ "A7.1: Economic dependence",
      dimension == "a7.2.commons.alternatives"                   ~ "A7.2: Commons alternatives",
      
      dimension == "eco1.01.rainfall.patterns"                   ~ "ECO1.1: Rainfall patterns",
      
      dimension == "gs2.1.external.support"                      ~ "GS2.1: External support",
      dimension == "gs3.2.property.security"                     ~ "GS3.2: Property security",
      dimension == "gs4.1.rules-in-use"                          ~ "GS4.1: Rules-in-use",
      dimension == "gs4.2.governance.strictness.trend"           ~ "GS4.2: Governance strictness trend",
      dimension == "gs5.1.external.recognition"                               ~ "GS5.1: External recognition",
      dimension == "gs5.4.participation.in.zoning"                            ~ "GS5.4: Participation in zoning",
      dimension == "gs5.3.participation.in.rule.making"                       ~ "GS5.3: Participation in rule making",
      dimension == "gs5.5.commons.political.power"                            ~ "GS5.5: Commons political power",
      dimension == "gs6.2.outsider.exclusion"                                 ~ "GS6.2: Outsider exclusion",
      dimension == "gs7.1.environmental.monitoring"                           ~ "GS7.1: Environmental monitoring",
      dimension == "gs7.2.self.sanctions"                                     ~ "GS7.2: Self sanctions",
      dimension == "gs7.3.external.sanctions"                                 ~ "GS7.3: External sanctions",
      
      dimension == "i1.1.conflict.resolution"                                 ~ "I1.1: Conflict resolution",
      dimension == "i2.1.participation.in.social.monitoring.(enforcement)"    ~ "I2.1: Participation in social monitoring",
      
      dimension == "o1.1.compliance"                                          ~ "O1.1: Compliance",
      dimension == "o2.1.commons.condition.trend"                             ~ "O2.1: Commons condition trend",
      dimension == "o2.3.invasives"                                           ~ "O2.3: Invasives",
      
      dimension == "rs2.1.commons.boundaries"                                 ~ "RS2.1: Commons boundaries",
      dimension == "rs2.2.commons.boundary.negotiability"                     ~ "RS2.2: Commons boundaries negotiability",
      dimension == "rs3.1.commons.spatial.extent.(ha)"                        ~ "RS3.1: Commons spatial extent",
      dimension == "rs5.1.productivity"                                       ~ "RS5.1: Productivity",
      
      dimension == "s1.1.human.population.size.change.(annual.increase)"      ~ "S1.1: Change in human population size",
      dimension == "s1.2.changes.in.ethnic.composition.(village.leader.data)" ~ "S1.2: Changes in ethnic composition",
      dimension == "s1.3.changes.in.livelihood.activities"                    ~ "S1.3: Changes in livelihood activities"
    ))
  
  # check data
  stopifnot(sum(is.na(df_codescores$tier1))==0)
  stopifnot(sum(is.na(df_codescores$tier3))==0)
  
  return(  df_codescores)
  
}


coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}

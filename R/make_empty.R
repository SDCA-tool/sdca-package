make_empty_demand <- function(){
  res <- list(0, 0, 0,
              0, 0, 0,
              0, 0, 0,
              data.frame(mode = c("walk","cycle","lgv","drive","passenger","rail","bus","hgv"),	
                         before	 = 0,
                         after_low = 0,
                         after_average = 0,
                         after_high = 0,
                         changeemissions_low = 0,
                         changeemissions_average = 0,
                         changeemissions_high = 0))
  names(res) <- c("emissions_increase", "emissions_decrease", "emissions_net",
                  "emissions_increase_low", "emissions_decrease_low", "emissions_net_low",
                  "emissions_increase_high", "emissions_decrease_high", "emissions_net_high",
                  "emissions_total")
  return(res)
}

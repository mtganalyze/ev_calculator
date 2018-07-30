
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

load("roundspecs")
source('globals.R')

shinyServer(function(input, output){
    tourn.specs <- reactive({
      if(input$tourn == 1){
        tspecs <- list(n_rounds = 15, cut_rounds = 8, cut_wins = 6, mean_skill = 1554, relbyes = c(0.424, 0.263, 0.019))
        if(input$sdens == 1){tspecs$sd_skill <- 66}
        if(input$sdens == 2){tspecs$sd_skill <- 100}
        if(input$sdens == 3){tspecs$sd_skill <- 200}
      } else {
        tspecs <- list(n_rounds = 16, cut_rounds = 8, cut_wins = 4, mean_skill = 1778, relbyes = c(0,0,0))
        if(input$sdens == 1){tspecs$sd_skill <- 150}
        if(input$sdens == 2){tspecs$sd_skill <- 200}
        if(input$sdens == 3){tspecs$sd_skill <- 300}
      }
      tspecs
    })
    
    rating_sample <- reactive({
      set.seed(1)
      rnorm(n = 20000, mean = tourn.specs()$mean_skill, sd = tourn.specs()$sd_skill)
    })
    
    virtual_rating <- reactive({wp.rating(input$fieldwp/100, rating_sample(), elo_coeff = elo_coeff)})
    
    round.specs <- reactive({
      if(input$tourn == 1){
        rspecs <- r.sp$GP
      } else {
        rspecs <- r.sp$PT
      }
      if(input$sdens == 1){return(rspecs$dense)}
      if(input$sdens == 2){return(rspecs$base)}
      if(input$sdens == 3){return(rspecs$spread)}
    })
    
    virtual.score <- reactive({c(input$wins + (input$draws%/%3), 
                                 input$losses + 2 * (input$draws%/%3), 
                                 input$draws%%3)})
    
    wpt <- reactive({wp.byround(virtual_rating(), round.specs())})
    
    finalscore <- reactive({score_distribution(wins = virtual.score()[1], 
                                     losses = virtual.score()[2], 
                                     draws = virtual.score()[3], 
                                     wpbr = wpt(), 
                                     tourn.specs = tourn.specs())})
    
    virtual.standings <- reactive({standings(n_players = input$n_players, 
                                             rel.byes = tourn.specs()$relbyes, 
                                             abs.byes = NULL, 
                                             tourn.specs = tourn.specs())})
    
    ### top8stuff
    t8mon <- reactive({top8money(tourn = ifelse(input$tourn == 1, "GP", "PT"))})
    t8pp <- reactive({top8propoints(tourn = ifelse(input$tourn == 1, "GP", "PT"))})
    
    t8skill <- reactive({top8skill(stands = virtual.standings(), round.specs = round.specs())})
    t8mwp <- reactive({top8_matchwp(rating = virtual_rating(),stands = virtual.standings(), round.specs = round.specs())})
    expt8pm <- reactive({exptop8prize(winprob = t8mwp(), prizemoney = t8mon())})
    expt8pp <- reactive({exptop8prize(winprob = t8mwp(), prizemoney = t8pp())})
    
    ### prizes
    
    pmoney <- reactive({prizemoney(et8p = expt8pm(), 
                                   tourn =  ifelse(input$tourn == 1, "GP", "PT"),
                                   n_players = input$n_players)})
    
    money.bw <- reactive({meanpayout.bywins(stands = virtual.standings(), pmoney = pmoney())})
    t8chance.bw <- reactive({meanpayout.bywins(stands = virtual.standings(), pmoney = rep(1,8))})
    propoints.bw <- reactive({meanpropoints.bywins(tourn = ifelse(input$tourn == 1, "GP", "PT"), 
                                                   t8pp = expt8pp(), t8chance.bywins = t8chance.bw()$nodraw$p)})
    ### expected winnings
    
    expmoney <- reactive({
      if(virtual.score()[3] > 0){
        exp.payout(scoredistri = finalscore(), payoutbyscore =  money.bw()$plusdraw$p)
      } else {
        exp.payout(scoredistri = finalscore(), payoutbyscore =  money.bw()$nodraw$p)
      }})
    t8chance <- reactive({
      if(virtual.score()[3] > 0){
        exp.payout(scoredistri = finalscore(), payoutbyscore =  t8chance.bw()$plusdraw$p)
      } else {
        exp.payout(scoredistri = finalscore(), payoutbyscore =  t8chance.bw()$nodraw$p)
      }})
    exppropoints <- reactive({
      if(virtual.score()[3] > 0){
        exp.payout(scoredistri = finalscore(), payoutbyscore =  propoints.bw()$plusdraw)
      } else {
        exp.payout(scoredistri = finalscore(), payoutbyscore =  propoints.bw()$nodraw)
      }})
    
    ### Compute Deltas
    finalscore_win <- reactive({score_distribution(wins = virtual.score()[1] + 1, 
                                               losses = virtual.score()[2], 
                                               draws = virtual.score()[3], 
                                               wpbr = wpt(), 
                                               tourn.specs = tourn.specs())})
    
    finalscore_loss <- reactive({score_distribution(wins = virtual.score()[1], 
                                               losses = virtual.score()[2] + 1, 
                                               draws = virtual.score()[3], 
                                               wpbr = wpt(), 
                                               tourn.specs = tourn.specs())})
    
    delta_expmoney <- reactive({
      if(virtual.score()[3] > 0){
        exp.payout(scoredistri = finalscore_win(), payoutbyscore =  money.bw()$plusdraw$p) - exp.payout(scoredistri = finalscore_loss(), payoutbyscore =  money.bw()$plusdraw$p)
      } else {
        exp.payout(scoredistri = finalscore_win(), payoutbyscore =  money.bw()$nodraw$p) - exp.payout(scoredistri = finalscore_loss(), payoutbyscore =  money.bw()$nodraw$p)
      }})
    delta_t8chance <- reactive({
      if(virtual.score()[3] > 0){
        exp.payout(scoredistri = finalscore_win(), payoutbyscore =  t8chance.bw()$plusdraw$p) - exp.payout(scoredistri = finalscore_loss(), payoutbyscore =  t8chance.bw()$plusdraw$p)
      } else {
        exp.payout(scoredistri = finalscore_win(), payoutbyscore =  t8chance.bw()$nodraw$p) - exp.payout(scoredistri = finalscore_loss(), payoutbyscore =  t8chance.bw()$nodraw$p)
      }})
    delta_exppropoints <- reactive({
      if(virtual.score()[3] > 0){
        exp.payout(scoredistri = finalscore_win(), payoutbyscore =  propoints.bw()$plusdraw) - exp.payout(scoredistri = finalscore_loss(), payoutbyscore =  propoints.bw()$plusdraw)
      } else {
        exp.payout(scoredistri = finalscore_win(), payoutbyscore =  propoints.bw()$nodraw) - exp.payout(scoredistri = finalscore_loss(), payoutbyscore =  propoints.bw()$nodraw)
      }})
    
    ### Outputs ####
    
    output$standings <- renderText({paste0(propoints.bw()$nodraw, collapse = " ")})
    output$scoredistribution <- renderPlot({plot(finalscore())})
    output$virtrating <- renderText({round(virtual_rating(), 0)})
    output$wpt <- renderPlotly({wpt.plot(wpt(), tourn.specs())})
    output$ev <- renderTable({data.frame("PrizeMoneyEV" = sprintf("%.2f $", expmoney()),
                                         "PropointsEV" = sprintf("%.2f", exppropoints()),
                                         "Top8Chance" = sprintf("%.2f%%", 100 * t8chance()))},
                             align = "c",
                             rownames = F)
    output$delta_ev <- renderTable({data.frame("PrizeMoneyEV" = sprintf("%.2f $", delta_expmoney()),
                                         "PropointsEV" = sprintf("%.2f", delta_exppropoints()),
                                         "Top8Chance" = sprintf("%.2f%%", 100 * delta_t8chance()))},
                                   align = "c",
                                   rownames = F)
})

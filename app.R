library(shiny)
library(tidyverse)

ui <- fluidPage(
    tabsetPanel(
        tabPanel("Wave composition explorer", fluid = TRUE, 
                 sidebarLayout(
                     sidebarPanel(width = 2,
                                  sliderInput("amp_1.1",
                                              "Amplitude of the first wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("w_freq_1.1",
                                              "Frequency of the first wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("phase_1.1",
                                              "Phase of the first wave:",
                                              min = 0,
                                              max = 360,
                                              value = 0, 
                                              step = 20),
                                  sliderInput("amp_1.2",
                                              "Amplitude of the second wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("w_freq_1.2",
                                              "Frequency of the second wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("phase_1.2",
                                              "Phase of the second wave:",
                                              min = 0,
                                              max = 360,
                                              value = 0, 
                                              step = 20)
                     ),
                     mainPanel(
                         plotOutput("final_plot_1"),
                         h3(withMathJax("Wave formula: $$s(t) = A \\times \\cos \\left({2\\pi \\times f \\times t + \\phi}\\right)$$"))
                     )
                 )
        ),
        tabPanel("Polar coordinate system explorer", fluid = TRUE, 
                 sidebarLayout(
                     sidebarPanel(width = 2,
                                  sliderInput("amp_2.1",
                                              "Amplitude of the first wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("w_freq_2.1",
                                              "Frequency of the first wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("phase_2.1",
                                              "Phase of the first wave:",
                                              min = 0,
                                              max = 360,
                                              value = 0, 
                                              step = 20),
                                  sliderInput("w_freq_2.2",
                                              "Frequency of the second wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("wind_freq_2.1",
                                              "Circle cycle frequency:",
                                              min = 0.2,
                                              max = 4,
                                              value = 1, 
                                              step = 0.1)),
                     mainPanel(plotOutput("final_plot_2"))
                 )
        ),
        tabPanel("Furier transform", fluid = TRUE, 
                 sidebarLayout(
                     sidebarPanel(width = 2,
                                  sliderInput("w_freq_3.1",
                                              "Frequency of the first wave:",
                                              min = 1,
                                              max = 3,
                                              value = 1, 
                                              step = 1),
                                  sliderInput("w_freq_3.2",
                                              "Frequency of the second wave:",
                                              min = 1,
                                              max = 4,
                                              value = 3, 
                                              step = 1),
                                  sliderInput("wind_freq_3.1",
                                              "Circle cycle frequency:",
                                              min = 0.2,
                                              max = 4,
                                              value = 1, 
                                              step = 0.1),
                                  sliderInput("arrow_point",
                                              "Where to place arrow?",
                                              min = 0,
                                              max = 4,
                                              value = 0, 
                                              step = 0.05),
                                  checkboxInput(inputId = "area", 
                                                label = "add area", 
                                                value = FALSE),
                                  checkboxInput(inputId = "points", 
                                                label = "add points", 
                                                value = FALSE)
                     ),
                     mainPanel(plotOutput("final_plot_3"),
                               h3(withMathJax("Arrow length: $$arrow = \\sqrt{\\sin^2(\\alpha)+\\cos^2(\\alpha)}$$")),
                               h3(withMathJax("Сontinuous Furie Transform: $$\\hat{g}_a(f) = \\int_{-\\infty}^{+\\infty}g(t)\\times\\cos({2\\pi ft})dt; \\hat{g}_b(f) = \\int_{-\\infty}^{+\\infty}g(t)\\times\\sin({2\\pi ft})dt; $$")),
                               h3(withMathJax("Discrete Fourier Transform: $$\\hat{g}_k = \\sum_{n = 0}^{N-1}g_n\\times \\cos({\\frac{2\\pi kn}{N}}); \\hat{g}_k = \\sum_{n = 0}^{N-1}g_n\\times \\sin({\\frac{2\\pi kn}{N}}); \\frac{k}{N} \\simeq f, n \\simeq t$$")))
                 )
        )
    )
)
server <- function(input, output) {
    output$final_plot_1 <- renderPlot({
        tibble(t = seq(0, 4, length.out = 1000),
               `wave 1` = input$amp_1.1 * cos(2 * pi * input$w_freq_1.1 * t+2*pi*input$phase_1.1/360),
               `wave 2` = input$amp_1.2 * cos(2 * pi * input$w_freq_1.2 * t+2*pi*input$phase_1.2/360),
               `wave 1 + wave 2` = `wave 1`+`wave 2`,
               wave_4 = `wave 1`,
               wave_5 = `wave 2`) %>% 
            pivot_longer(names_to = "wave", values_to = "values", `wave 1`:`wave 1 + wave 2`) %>% 
            mutate(wave = factor(wave, levels = c("wave 1", "wave 2", "wave 1 + wave 2"))) %>% 
            ggplot(aes(t, values, color = wave))+
            geom_line()+
            geom_line(aes(y = wave_4), alpha = 0.6, linetype = 3, color = "black")+
            geom_line(aes(y = wave_5), alpha = 0.4, linetype = 5, color = "black")+
            facet_wrap(~wave, nrow = 3)+
            theme_minimal()+
            theme(legend.position="bottom", 
                  legend.title = element_blank(),
                  text = element_text(size = 22))+
            labs(x = "t (seconds)", y = "y") ->
            p1
        freq <- ifelse(input$w_freq_1.1 == input$w_freq_1.2, input$w_freq_1.1, 1)
        tibble(id = seq(1, 360, length.out = 1000),
               t_1 = seq(0, 1/input$w_freq_1.1, length.out = 1000),
               t_2 = seq(0, 1/input$w_freq_1.2, length.out = 1000),
               t_3 = seq(0, 1/freq, length.out = 1000),
               wave_1 = input$amp_1.1 * cos(2 * pi * input$w_freq_1.1 * t_1+2*pi*input$phase_1.1/360),
               wave_2 = input$amp_1.2 * cos(2 * pi * input$w_freq_1.2 * t_2+2*pi*input$phase_1.2/360),
               wave_3 = input$amp_1.1 * cos(2 * pi * input$w_freq_1.1 * t_3 + 2*pi*input$phase_1.1/360)+
                   input$amp_1.2 * cos(2 * pi * input$w_freq_1.2 * t_3+2*pi*input$phase_1.2/360)) %>% 
            pivot_longer(names_to = "wave", values_to = "values", wave_1:wave_3) %>% 
            mutate(values = round(values, 4)) %>% 
            ggplot(aes(id, values, color = wave))+
            geom_line(show.legend = FALSE)+
            coord_polar(start = -2*pi/4, direction = -1)+
            scale_x_continuous(breaks = 0:12*30)+
            labs(x = "phase", y = "")+
            theme_minimal() +
            theme(text = element_text(size = 22)) ->
            p2
        gridExtra::grid.arrange(p1, p2, nrow = 1)
    })
    output$final_plot_2 <- renderPlot({
        tibble(line = seq(0, 4, by = 1/input$wind_freq_2.1),
               wave = factor("wave 1 + wave 2", levels = c("wave 1", "wave 2", "wave 1 + wave 2"))) ->
            lines
        tibble(t = seq(0, 4, length.out = 1000),
               `wave 1` = input$amp_2.1 * cos(2 * pi * input$w_freq_2.1 * t+2*pi*input$phase_2.1/360),
               `wave 2` = cos(2 * pi * input$w_freq_2.2 * t),
               `wave 1 + wave 2` = `wave 1`+`wave 2`,
               wave_4 = `wave 1`,
               wave_5 = `wave 2`) %>% 
            pivot_longer(names_to = "wave", values_to = "values", `wave 1`:`wave 1 + wave 2`) %>% 
            mutate(wave = factor(wave, levels = c("wave 1", "wave 2", "wave 1 + wave 2"))) %>% 
            ggplot(aes(t, values, color = wave))+
            geom_vline(data = lines, aes(xintercept = line), linetype = 2, color = "orange", size = 1.2)+
            geom_line()+
            geom_line(aes(y = wave_4), alpha = 0.6, linetype = 3, color = "black")+
            geom_line(aes(y = wave_5), alpha = 0.4, linetype = 5, color = "black")+
            facet_wrap(~wave, nrow = 3)+
            theme_minimal()+
            theme(legend.position="bottom", 
                  legend.title = element_blank(),
                  text = element_text(size = 22))+
            labs(x = "t (seconds)", y = "y") ->
            p1
        tibble(t_3 = seq(0, 4, length.out = 1000),
               wave_3 = input$amp_2.1 * cos(2 * pi * input$w_freq_2.1 * t_3 + 2*pi*input$phase_2.1/360)+
                   cos(2 * pi * input$w_freq_2.2 * t_3)) %>% 
            mutate(cycle = floor(t_3/(1/input$wind_freq_2.1)),
                   t = t_3 - cycle*(1/input$wind_freq_2.1)) %>% 
            ggplot(aes(t, wave_3, group = cycle))+
            geom_line(show.legend = FALSE)+
            coord_polar(start = -2*pi/4, direction = -1)+
            scale_x_continuous(breaks = round((1/input$wind_freq_3.1) * seq(0, 1, by = 0.25), 3))+
            labs(x = "", y = "")+
            theme_minimal()+
            theme(text = element_text(size = 22), axis.text.y = element_blank()) ->
            p2
        gridExtra::grid.arrange(p1, p2, nrow = 1)
    })
    output$final_plot_3 <- renderPlot({
        tibble(line = seq(0, 4, by = 1/input$wind_freq_3.1),
               wave = factor("wave 1 + wave 2", levels = c("wave 1", "wave 2", "wave 1 + wave 2"))) ->
            lines
        tibble(t = seq(0, 4, length.out = 1000),
               wave_1 = cos(2 * pi * input$w_freq_3.1 * t),
               wave_2 = cos(2 * pi * input$w_freq_3.2 * t),
               wave_3 = wave_1+wave_2) %>% 
            ggplot(aes(t, wave_3))+
            geom_vline(data = lines, aes(xintercept = line), linetype = 2, color = "orange", size = 1.2)+
            geom_line()+
            theme_minimal()+
            theme(legend.position="bottom", 
                  legend.title = element_blank(),
                  text = element_text(size = 22))+
            labs(x = "t (seconds)", y = "y") ->
            p1
        
        if(input$arrow_point > 0){
            tibble(t = input$arrow_point,
                   value = cos(2 * pi * input$w_freq_3.1 * t) + cos(2 * pi * input$w_freq_3.2 * t)) ->
                df
            p1 <- p1 +  geom_segment(data = df, aes(x = t, xend  = t ,y = -2, yend = value),
                                     color = "red",
                                     arrow = arrow(length=unit(0.60,"cm"), type = "closed"))
        }
        
        tibble(t_3 = seq(0, 4, length.out = 1000),
               wave_1 = cos(2 * pi * input$w_freq_3.1 * t_3),
               wave_2 = cos(2 * pi * input$w_freq_3.2 * t_3),
               wave_3 = wave_1+wave_2) %>% 
            mutate(cycle = floor(t_3/(1/input$wind_freq_3.1)),
                   t = t_3 - cycle*(1/input$wind_freq_3.1)) %>% 
            ggplot(aes(t, wave_3, group = cycle))+
            geom_line(show.legend = FALSE)+
            coord_polar(start = -2*pi/4, direction = -1)+
            scale_x_continuous(breaks = round((1/input$wind_freq_3.1) * seq(0, 1, by = 0.25), 3))+
            labs(x = "", y = "")+
            theme_minimal()+
            theme(text = element_text(size = 22), axis.text.y = element_blank()) ->
            p2
        
        if(input$arrow_point > 0){
            tibble(t_3 = input$arrow_point,
                   value = cos(2 * pi * input$w_freq_3.1 * t_3) + cos(2 * pi * input$w_freq_3.2 * t_3)) %>% 
                mutate(cycle = floor(t_3/(1/input$wind_freq_3.1)),
                       t = t_3 - cycle*(1/input$wind_freq_3.1)) ->
                df
            
            p2 <- p2 +  geom_segment(data = df, aes(x = t, xend  = t ,y = -2, yend = value),
                                     color = "red",
                                     arrow = arrow(length=unit(0.60,"cm"), type = "closed"))
        }
        if(isTRUE(input$points)){
            tibble(t_3 = seq(0, 4, length.out = 7*4),
                   wave_1 = cos(2 * pi * input$w_freq_3.1 * t_3),
                   wave_2 = cos(2 * pi * input$w_freq_3.2 * t_3),
                   wave_3 = wave_1+wave_2) %>% 
                mutate(cycle = floor(t_3/(1/input$wind_freq_3.1)),
                       t = t_3 - cycle*(1/input$wind_freq_3.1)) ->
                dots
                
            
            p2 <- p2 +  geom_point(data = dots, aes(x = t, ,y = wave_3),
                                     color = "darkgreen")
        }
        
        
        if(isTRUE(input$points)){
            tibble(t_3 = seq(0, 4, length.out = 7*4),
                   wave_1 = cos(2 * pi * input$w_freq_3.1 * t_3),
                   wave_2 = cos(2 * pi * input$w_freq_3.2 * t_3),
                   wave_3 = wave_1+wave_2) %>% 
                mutate(cycle = floor(t_3/(1/input$wind_freq_3.1)),
                       t = t_3 - cycle*(1/input$wind_freq_3.1)) ->
                dots
            
            
            p2 <- p2 +  geom_point(data = dots, aes(x = t, ,y = wave_3),
                                   color = "darkgreen")
        }
        
        if(isTRUE(input$area)){
            p2 <- p2 +  geom_ribbon(ymin = -2, aes(ymax = wave_3), alpha = 0.1, color = "grey90")
        }
        
        
        
        gridExtra::grid.arrange(p1, p2, nrow = 1)        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

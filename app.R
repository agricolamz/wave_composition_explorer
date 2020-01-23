library(shiny)
library(tidyverse)

ui <- fluidPage(
    titlePanel("Wave composition explorer"),
    sidebarLayout(
        sidebarPanel(width = 2,
            sliderInput("amp_1",
                        "Amplitude of the first wave:",
                        min = 1,
                        max = 3,
                        value = 1, 
                        step = 1),
            sliderInput("w_freq_1",
                        "Frequency of the first wave:",
                        min = 1,
                        max = 3,
                        value = 1, 
                        step = 1),
            sliderInput("phase_1",
                        "Phase of the first wave:",
                        min = 0,
                        max = 360,
                        value = 0, 
                        step = 20),
            sliderInput("amp_2",
                        "Amplitude of the second wave:",
                        min = 1,
                        max = 3,
                        value = 1, 
                        step = 1),
            sliderInput("w_freq_2",
                        "Frequency of the second wave:",
                        min = 1,
                        max = 3,
                        value = 1, 
                        step = 1),
            sliderInput("phase_2",
                        "Phase of the second wave:",
                        min = 0,
                        max = 360,
                        value = 0, 
                        step = 20)
        ),
        mainPanel(
            plotOutput("final_plot"),
            h2(withMathJax("Wave formula: $$s(t) = A \\times \\cos \\left({2\\pi \\times f \\times t + \\phi}\\right)$$"))
        )
    )
)
server <- function(input, output) {
    output$final_plot <- renderPlot({
        tibble(t = seq(0, 4, length.out = 1000),
               `wave 1` = input$amp_1 * cos(2 * pi * input$w_freq_1 * t+2*pi*input$phase_1/360),
               `wave 2` = input$amp_2 * cos(2 * pi * input$w_freq_2 * t+2*pi*input$phase_2/360),
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
        freq <- ifelse(input$w_freq_1 == input$w_freq_2, input$w_freq_1, 1)
        tibble(id = seq(1, 360, length.out = 1000),
               t_1 = seq(0, 1/input$w_freq_1, length.out = 1000),
               t_2 = seq(0, 1/input$w_freq_2, length.out = 1000),
               t_3 = seq(0, 1/freq, length.out = 1000),
               wave_1 = input$amp_1 * cos(2 * pi * input$w_freq_1 * t_1+2*pi*input$phase_1/360),
               wave_2 = input$amp_2 * cos(2 * pi * input$w_freq_2 * t_2+2*pi*input$phase_2/360),
               wave_3 = input$amp_1 * cos(2 * pi * input$w_freq_1 * t_3 + 2*pi*input$phase_1/360)+
                       input$amp_2 * cos(2 * pi * input$w_freq_2 * t_3+2*pi*input$phase_2/360)) %>% 
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
}

# Run the application 
shinyApp(ui = ui, server = server)

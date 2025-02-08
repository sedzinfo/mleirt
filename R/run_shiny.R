##########################################################################################
# MLE IRT
##########################################################################################
#' @title MLE IRT
#' @import shiny shinydashboard
#' @keywords mle irt
#' @export
#' @examples
#' mle_irt()
mle_irt<-function(){
  ui<-tagList(navbarPage("MLE EAP MAP using IRTDemo",# titlePanel(h4("by Metin Bulus")),
                         ##########################################################################################
                         # MLE PERSON LOCATION GIVEN ITEM DIFFICULTY RACH
                         ##########################################################################################
                         tabPanel("MLE Rasch", titlePanel(h4("by unknown contributor")),
                                  sidebarPanel(width=4,
                                               sliderInput("r_plid",label="Raw Score (r)",min=0,max=5,value=2,step=1),
                                               sliderInput("t_0_plid",label="Starting Value",min=-2,max=2,value=0,step=0.5),
                                               sliderInput("iter_plid",label="Iteration",min=1,max=5,value=1,step=1),
                                               strong("Note:"),
                                               p("Respondent's raw score are assumed to come from following patterns:
                                                r=0,p=00000;r=1,p=10000;r=2,p=11000;
                                                r=3,p=11100;r=4,p=11110;r=5,p=11111."),
                                               p("Item difficulties for five item instrument are assumed to be known (-1.90,-0.60,-0.25,0.30,0.45) and are provided in de Ayala (2009,p.22)."),
                                               p("This app replicates person locations in Table 2.2 in de Ayala (2009,p.28)")),
                                  mainPanel(plotOutput(outputId="MLEIRT_plot",height="800px"),verbatimTextOutput("iterinfo"))),
                         ##########################################################################################
                         # MLE ITEM DISCRIMINATION DIFFICULTY 2PL MODEL
                         ##########################################################################################
                         tabPanel("MLE 2PL", titlePanel(h4("by unknown contributor")),
                                  sidebarPanel(width=3,
                                               sliderInput("alpha_idd2pl",label="Discrimination",min=-3,max=3,value=1,step=0.1),
                                               sliderInput("delta_idd2pl",label="Difficulty",min=-3,max=3,value=-0.5,step=0.1),
                                               sliderInput("angle1_idd2pl",label="Rotate Right-Left",min=0,max=90,value=30,step=1),
                                               sliderInput("angle2_idd2pl",label="Rotate Up-Down",min=0,max=90,value=30,step=1)
                                  ),
                                  helpText("Values on the control panel are true values and projection of red dot corresponds to the intersection of discrimination and difficulty parameters that maximize the joint log-likelihood."),
                                  mainPanel(plotOutput(outputId="est2PL_plot",width="100%",height="800px"))),
                         ##########################################################################################
                         # MLE ITEM DISCRIMINATION DIFFICULTY 2PL MODEL
                         ##########################################################################################
                         tabPanel("MLE 3PL", titlePanel(h4("by Metin Bulus")),
                                  sidebarPanel(width=3,
                                               sliderInput("guess_igdd3pl",label="Guessing",min=0.05,max=0.6,value=0.4,step=0.05),
                                               sliderInput("alpha_igdd3pl",label="Discrimination",min=-3,max=3,value=1,step=0.15),
                                               sliderInput("delta_igdd3pl",label="Difficulty",min=-3,max=3,value=-0.5,step=0.15),
                                               sliderInput("angle1_igdd3pl",label="Rotate Left-Right",min=0,max=90,value=30,step=1),
                                               sliderInput("angle2_igdd3pl",label="Rotate Up-Down",min=0,max=90,value=30,step=1)),
                                  helpText("Note: The graph may take a minute to appear!"),
                                  mainPanel(plotOutput(outputId="est3PL_plot_all",width="100%",height="800px"))),
                         ##########################################################################################
                         # EAP MAP PERSON ABILITY ESTIMATION IN RACH MODEL
                         ##########################################################################################
                         tabPanel("EAP-MAP Rasch", titlePanel(h4("by Metin Bulus & Wes Bonifay")),
                                  sidebarPanel(width=3,
                                               sliderInput("r_eapmappae",label="Raw Score (r)",min=0,max=5,value=2,step=1),
                                               sliderInput("quad_eapmappae",label="Quadrature Points",min=10,max=100,value=10,step=10),
                                               sliderInput("mean_eapmappae",label="Prior Mean",min=-2,max=2,value=0,step=0.1),
                                               sliderInput("sd_eapmappae",label="Prior SD",min=0.1,max=2,value=1,step=0.1),
                                               sliderInput("skew_eapmappae",label="Prior Skewness",min=.5,max=2,value=1,step=.1)),
                                  helpText("Respondent's raw score are assumed to come from following patterns: r=0,patt=00000; r=1,patt=10000; r=2,patt=11000; r=3,patt=11100; r=4,patt=11110; and r=5,patt=11111."),
                                  helpText("Note: Default values replicate the analysis by de Ayala (2009,p. 79) where difficulty parameters for Item 1-5 are fixed at -2.155,-.245,.206,.984,and 1.211."),
                                  mainPanel(tabsetPanel(tabPanel("Seperate",dashboardBody(fluidRow(
                                    column(width=6,plotOutput("Prior_plot")),
                                    column(width=6,plotOutput("TraceLines_plot")),
                                    column(width=6,plotOutput("Likelihood_plot")),
                                    column(width=6,plotOutput("Posterior_plot"))))),
                                    tabPanel("Combined",plotOutput(outputId="Combined_plot",width="100%",height="800px")))))
              ))
  ##########################################################################################
  # MLE
  ##########################################################################################
  server<-function(input,output) {
    ##########################################################################################
    # MLE ITEM DISCRIMINATION DIFFICULTY 2PL MODEL
    ##########################################################################################
    pcorrmtx_idd2pl<-reactive({
      pcorrmtx_idd2pl<-matrix(NA,nrow=N_idd2pl,ncol=1)
      for(j in 1:N_idd2pl){ pcorrmtx_idd2pl[j]<-Pfun_idd2pl(theta=thetas_idd2pl[j],delta=input$delta_idd2pl,alpha=input$alpha_idd2pl) }
      pcorrmtx_idd2pl
    })
    datamtx_idd2pl<-reactive({
      pcorrmtx_idd2pl<-pcorrmtx_idd2pl()
      datamtx_idd2pl<-matrix(NA,nrow=N_idd2pl,ncol=1)
      for(j in 1:N_idd2pl){ datamtx_idd2pl[j]<-rbinom(1,1,pcorrmtx_idd2pl[j]) }
      datamtx_idd2pl
    })
    logLike_idd2pl<-reactive({
      datamtx_idd2pl<-datamtx_idd2pl()
      pik<-array(NA,dim=c(k_idd2pl,k_idd2pl,N_idd2pl))
      logLike_idd2pl<-matrix(NA,k_idd2pl,k_idd2pl)
      for (m in 1:k_idd2pl){
        for(n in 1:k_idd2pl){
          for (j in 1:N_idd2pl){
            pik[m,n,j]<-Pfun_idd2pl(theta=thetas_idd2pl[j],alpha=alphas_idd2pl[m],delta=deltas_idd2pl[n],u=datamtx_idd2pl[j])
          }
          logLike_idd2pl[m,n]<-sum(log(pik[m,n,]))
        }
      }
      logLike_idd2pl
    })
    xy_idd2pl<-reactive({
      logLike_idd2pl<-logLike_idd2pl()
      which(logLike_idd2pl==max(logLike_idd2pl),TRUE)
    })
    output$est2PL_plot<-renderPlot({
      xy_idd2pl<-xy_idd2pl()
      logLike_idd2pl<-logLike_idd2pl()
      logLike3D<- graphics::persp(alphas_idd2pl,
                                  deltas_idd2pl,
                                  logLike_idd2pl,
                                  theta=input$angle1_idd2pl,
                                  phi=input$angle2_idd2pl,
                                  col="#347AB6",
                                  xlab="Discrimination",
                                  ylab="Difficulty",
                                  zlab="Log-Likelihood")
      points(trans3d(alphas_idd2pl[xy_idd2pl[1,1]],
                     deltas_idd2pl[xy_idd2pl[1,2]],
                     logLike_idd2pl[xy_idd2pl[1,1],
                                    xy_idd2pl[1,2]],
                     logLike3D),
             col='black',cex=1.5,pch=21,bg="red")
    })
    # output$est2PL_plot <- renderPlotly({
    #   xy_idd2pl <- xy_idd2pl()
    #   logLike_idd2pl <- logLike_idd2pl()
    #   plot_ly(
    #     x=alphas_idd2pl,
    #     y=deltas_idd2pl,
    #     z=logLike_idd2pl,
    #     type="surface",
    #     colors=colorRamp(c("#347AB6")),
    #     opacity=0.8) %>%
    #     layout(
    #       scene=list(
    #         xaxis=list(title="Discrimination"),
    #         yaxis=list(title="Difficulty"),
    #         zaxis=list(title="Log-Likelihood"),
    #         camera=list(eye=list(x=input$angle1_idd2pl,
    #                              y=input$angle2_idd2pl,
    #                              z=1))))%>%
    #     add_trace(
    #       x=alphas_idd2pl[xy_idd2pl[1,1]],
    #       y=deltas_idd2pl[xy_idd2pl[1,2]],
    #       z=logLike_idd2pl[xy_idd2pl[1,1],xy_idd2pl[1,2]],
    #       type="scatter3d",
    #       mode="markers",
    #       marker=list(color="red",size=20,symbol="circle")
    #     )
    # })
    ##########################################################################################
    # MLE ITEM GUESSING DISCRIMINATION DIFFICULTY 3PL MODEL
    ##########################################################################################
    pcorrmtx_igdd3pl<-reactive({
      pcorrmtx_igdd3pl<-matrix(NA,nrow=N_igdd3pl,ncol=1)
      for(j in 1:N_igdd3pl){
        pcorrmtx_igdd3pl[j]<-Pfun_igdd3pl(guess=input$guess_igdd3pl,theta=thetas_igdd3pl[j],delta=input$delta_igdd3pl,alpha=input$alpha_igdd3pl)
      }
      pcorrmtx_igdd3pl})
    datamtx_igdd3pl<-reactive({
      pcorrmtx_igdd3pl<-pcorrmtx_igdd3pl()
      datamtx_igdd3pl<-matrix(NA,nrow=N_igdd3pl,ncol=1)
      for(j in 1:N_igdd3pl){
        datamtx_igdd3pl[j]<-rbinom(1,1,pcorrmtx_igdd3pl[j])
      }
      datamtx_igdd3pl})
    logLike1<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[1],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike2<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[2],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike3<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[3],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike4<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[4],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike5<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[5],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike6<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[6],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike7<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[7],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike8<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[8],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike9<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[9],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike10<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[10],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike11<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[11],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    logLike12<-reactive({
      datamtx_igdd3pl<-datamtx_igdd3pl()
      for (m in 1:k_igdd3pl){
        for(n in 1:k_igdd3pl){
          for (j in 1:N_igdd3pl){
            pik_igdd3pl[m,n,j]<-Pfun_igdd3pl(guess=guesses_igdd3pl[12],theta=thetas_igdd3pl[j],alpha=alphas_igdd3pl[m],delta=deltas_igdd3pl[n],u=datamtx_igdd3pl[j])
          }
          logLike_igdd3pl[m,n]<-sum(log(pik_igdd3pl[m,n,]))
        }
      }
      logLike_igdd3pl
    })
    xy1<-reactive({
      logLike1<-logLike1()
      which(logLike1==max(logLike1),TRUE)
    })
    xy2<-reactive({
      logLike2<-logLike2()
      which(logLike2==max(logLike2),TRUE)
    })
    xy3<-reactive({
      logLike3<-logLike3()
      which(logLike3==max(logLike3),TRUE)
    })
    xy4<-reactive({
      logLike4<-logLike4()
      which(logLike4==max(logLike4),TRUE)
    })
    xy5<-reactive({
      logLike5<-logLike5()
      which(logLike5==max(logLike5),TRUE)
    })
    xy6<-reactive({
      logLike6<-logLike6()
      which(logLike6==max(logLike6),TRUE)
    })
    xy7<-reactive({
      logLike7<-logLike7()
      which(logLike7==max(logLike7),TRUE)
    })
    xy8<-reactive({
      logLike8<-logLike8()
      which(logLike8==max(logLike8),TRUE)
    })
    xy9<-reactive({
      logLike9<-logLike9()
      which(logLike9==max(logLike9),TRUE)
    })
    xy10<-reactive({
      logLike10<-logLike10()
      which(logLike10==max(logLike10),TRUE)
    })
    xy11<-reactive({
      logLike11<-logLike11()
      which(logLike11==max(logLike11),TRUE)
    })
    xy12<-reactive({
      logLike12<-logLike12()
      which(logLike12==max(logLike12),TRUE)
    })
    output$est3PL_plot_all<-renderPlot({
      xy1<-xy1()
      logLike1<-logLike1()
      xy2<-xy2()
      logLike2<-logLike2()
      xy3<-xy3()
      logLike3<-logLike3()
      xy4<-xy4()
      logLike4<-logLike4()
      xy5<-xy5()
      logLike5<-logLike5()
      xy6<-xy6()
      logLike6<-logLike6()
      xy7<-xy7()
      logLike7<-logLike7()
      xy8<-xy8()
      logLike8<-logLike8()
      xy9<-xy9()
      logLike9<-logLike9()
      xy10<-xy10()
      logLike10<-logLike10()
      xy11<-xy11()
      logLike11<-logLike11()
      xy12<-xy12()
      logLike12<-logLike12()
      logLike3D1<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike1,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,ltheta=120,shade=0.75,col=2,xlab="Discrimination",ylab="Difficulty",zlab="Log-Likelihood");par(new=TRUE)
      logLike3D2<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike2,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=3,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D3<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike3,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=4,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D4<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike4,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=5,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D5<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike5,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=6,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D6<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike6,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=7,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D7<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike7,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=8,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D8<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike8,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=9,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D9<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike9,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=10,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D10<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike10,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=11,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D11<- graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike11,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=12,xlab="",ylab="",zlab="");par(new=TRUE)
      logLike3D12<-graphics::persp(alphas_igdd3pl,deltas_igdd3pl,logLike12,ltheta=120,shade=0.75,theta=input$angle1_igdd3pl,phi=input$angle2_igdd3pl,col=13,xlab="",ylab="",zlab="")
      maxLL<-max(logLike1[xy1[1,1],xy1[1,2]],logLike2[xy2[1,1],xy2[1,2]],logLike3[xy3[1,1],xy3[1,2]],logLike4[xy4[1,1],xy4[1,2]],logLike5[xy5[1,1],xy3[1,2]],logLike6[xy6[1,1],xy1[1,2]],logLike7[xy7[1,1],xy2[1,2]],logLike8[xy8[1,1],xy3[1,2]],logLike9[xy9[1,1],xy4[1,2]],logLike10[xy7[1,1],xy2[1,2]],logLike11[xy8[1,1],xy3[1,2]],logLike12[xy9[1,1],xy4[1,2]])
      points(trans3d(alphas_igdd3pl[xy1[1,1]],deltas_igdd3pl[xy1[1,2]],logLike1[xy1[1,1],xy1[1,2]],logLike3D1),col='black',pch=21,bg=2,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy2[1,1]],deltas_igdd3pl[xy2[1,2]],logLike2[xy2[1,1],xy2[1,2]],logLike3D2),col='black',pch=21,bg=3,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy3[1,1]],deltas_igdd3pl[xy3[1,2]],logLike3[xy3[1,1],xy3[1,2]],logLike3D3),col='black',pch=21,bg=4,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy4[1,1]],deltas_igdd3pl[xy4[1,2]],logLike4[xy4[1,1],xy4[1,2]],logLike3D4),col='black',pch=21,bg=5,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy5[1,1]],deltas_igdd3pl[xy5[1,2]],logLike5[xy5[1,1],xy3[1,2]],logLike3D5),col='black',pch=21,bg=6,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy6[1,1]],deltas_igdd3pl[xy6[1,2]],logLike6[xy6[1,1],xy1[1,2]],logLike3D6),col='black',pch=21,bg=7,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy7[1,1]],deltas_igdd3pl[xy7[1,2]],logLike7[xy7[1,1],xy2[1,2]],logLike3D7),col='black',pch=21,bg=8,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy8[1,1]],deltas_igdd3pl[xy8[1,2]],logLike8[xy8[1,1],xy3[1,2]],logLike3D8),col='black',pch=21,bg=9,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy9[1,1]],deltas_igdd3pl[xy9[1,2]],logLike9[xy9[1,1],xy4[1,2]],logLike3D9),col='black',pch=21,bg=10,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy10[1,1]],deltas_igdd3pl[xy10[1,2]],logLike10[xy10[1,1],xy3[1,2]],logLike3D10),col='black',pch=21,bg=11,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy11[1,1]],deltas_igdd3pl[xy11[1,2]],logLike11[xy11[1,1],xy1[1,2]],logLike3D11),col='black',pch=21,bg=12,cex=1.5)
      points(trans3d(alphas_igdd3pl[xy12[1,1]],deltas_igdd3pl[xy12[1,2]],logLike12[xy12[1,1],xy2[1,2]],logLike3D12),col='black',pch=21,bg=13,cex=1.5)
      legend(legend=paste("c=",seq(0.05,.60,.05),sep=""),col=2:13,pch=19,title="Guessing","right")
    })
    ##########################################################################################
    # MLE PERSON LOCATION GIVEN ITEM DIFFICULTY RACH
    ##########################################################################################
    output$MLEIRT_plot<-renderPlot({
      patt<-c(rep(1,input$r_plid),rep(0,5-input$r_plid))
      logLike_plid[,1]<-thetas_plid
      for (j in 1:N_plid) {
        for (i in 1:n_plid) {
          u<-patt[i]
          pji_plid[j,i]<-Pfun_plid(theta=thetas_plid[j],delta=deltas_plid[i],u=patt[i])
        }
        logLike_plid[j,2]<-sum(log(pji_plid[j,]))
        logLike_plid[j,3]<-prod(pji_plid[j,])
      }
      thetahat<-logLike_plid[logLike_plid[,2]==max(logLike_plid[,2]),1]
      maxLL<-logLike_plid[logLike_plid[,2]==max(logLike_plid[,2]),2]
      t<-logLike_plid[,1]
      ll<-logLike_plid[,2]
      t_0<-input$t_0_plid
      for(i in 1:input$iter_plid){
        graphics::plot(t,ll,type='l',xlab=expression(theta),ylab='LogLikelihood',xlim=c(-6,6),ylim=c(-15,1))
        abline(h=0,col=8)
        spline<-smooth.spline(ll ~ t)
        lines(spline,col='black')
        fderiv0<-predict(spline,x=t_0,deriv=0)
        fderiv1<-predict(spline,x=t_0,deriv=1)
        fderiv2<-predict(spline,x=t_0,deriv=2)
        points(fderiv0,col='black',pch=21,bg="red",cex=1.5) # point to predict tangent
        text(fderiv0$x,fderiv0$y-1,expression(hat(theta)^0))
        t_1<-t_0-(fderiv1$y/fderiv2$y)
        theta0<-t_0
        output$iterinfo<-renderText({
          paste(sep="","Newton-Raphson step:","\n","theta1=theta0-(LL' / LL'')","\n",round(t_1,4),"=",round(theta0,4),"-","(",round(fderiv1$y,4)," / ",round(fderiv2$y,4),")","\n","\n","Acute angle with abscissa=",round(deg,digits=0)," degrees")
        })
        yint<-fderiv0$y-(fderiv1$y*t_0)
        xint<--yint/fderiv1$y
        lines(t,yint + fderiv1$y*t,col='blue') # tangent (1st deriv. of spline at t_0)
        rad<-atan(fderiv0$y/(xint-t_0))
        deg<-rad*180/pi
        if(round(abs(deg),digits=0)>0){polygon(x=c(xint,t_0,t_0),y=c(0,0,fderiv0$y),density=c(30))}
        y_1<-predict(spline,x=t_1,deriv=0)
        points(t_1,y_1$y,col='black',pch=21,bg="green",cex=1.5)
        text(t_1,y_1$y-1,expression(hat(theta)^1))
        abline(v=t_0,col=8,lty='dotted')
        abline(v=t_1,col=8,lty='dotted')
        t_0<-t_1
      }
    })
    ##########################################################################################
    # EAP MAP PERSON ABILITY ESTIMATION IN RACH MODEL
    ##########################################################################################
    Xr<-reactive({ seq(-4,4,length=input$quad_eapmappae) })
    pattInput<-reactive({ c(rep(1,input$r_eapmappae),rep(0,5-input$r_eapmappae)) })
    AXr<-reactive({ Xr<-Xr()
    fGarch::dsnorm(Xr,mean=input$mean_eapmappae,sd=input$sd_eapmappae,xi=input$skew_eapmappae,log=FALSE)/sum(fGarch::dsnorm(Xr,mean=input$mean_eapmappae,sd=input$sd_eapmappae,xi=input$skew_eapmappae,log=FALSE))})
    pji<-reactive({ Xr<-Xr()
    N<-length(Xr)
    pji<-matrix(NA,N,n_eapmappae)
    for(j in 1:N){
      for(i in 1:n_eapmappae){
        patt<-pattInput()
        Xr<-Xr()
        pji[j,i]<-PLfun_eapmappae(theta=Xr[j],alpha=alphas_eapmappae[i],delta=deltas_eapmappae[i],u=patt[i])
      }
    }
    pji})
    LXr<-reactive({pji<-pji()
    Xr<-Xr()
    N<-length(Xr)
    LXr<-matrix(NA,N,1)
    for(j in 1:N){ LXr[j]<-prod(pji[j,]) }
    LXr})
    output$Prior_plot<-renderPlot({ Xr<-Xr()
    AXr<-AXr()
    graphics::plot(Xr,AXr,type='l',xlab=expression(X[r]),ylab=expression(A(X[r])),xlim=c(-6,6),main=expression(paste("Prior Distribution of ",theta)))
    inc<-(abs(Xr[2]-Xr[1])/2)
    for(i in 1:length(Xr)){ polygon(x=c(Xr[i]-inc,Xr[i]+inc,Xr[i]+inc,Xr[i]-inc) ,y=c(0,0,AXr[i],AXr[i]),density=c(30)) }})
    output$TraceLines_plot<-renderPlot({Xr<-Xr()
    pji<-pji()
    patt<-pattInput()
    graphics::plot(NULL,xlim=c(-3,3),ylim=c(0,1),xlab=expression(X[r]),ylab=expression(P(X[r])),main=expression(paste("Item Tracelines")))
    for(i in 1:n_eapmappae){
      lines(Xr,pji[,i],type='l',col=2+patt[i]*2)
    }
    legend(legend=c(1,0),col=c(4,2),lty=1,title="Response","right")
    })
    output$Likelihood_plot<-renderPlot({
      Xr<-Xr()
      LXr<-LXr()
      graphics::plot(Xr,LXr,type='l',xlab=expression(X[r]),ylab=expression(L(X[r])),xlim=c(-3,3),main=expression(paste("Likelihood")))
    })
    output$Posterior_plot<-renderPlot({
      Xr<-Xr()
      AXr<-AXr()
      LXr<-LXr()
      graphics::plot(Xr,AXr*LXr,type='l',xlab=expression(X[r]),ylab=expression(paste(A(X[r])," * ",L(X[r]))),xlim=c(-2.5,2.5),main=expression(paste("Posterior Distribution of ",theta)))
      EAP<-sum(Xr*AXr*LXr)/sum(AXr*LXr)
      PSD<-sqrt(sum((Xr-EAP)^2*LXr*AXr)/sum(LXr*AXr))
      MAP<-Xr[which.max(AXr*LXr)] #now it makes sense why it is iterative
      abline(v=EAP,col=4)
      abline(v=MAP,col=2)
      text(-1.8,max(AXr*LXr)*.9,paste('EAP =',round(EAP,4)),col=4)
      text(1.8,max(AXr*LXr)*.9,paste('MAP =',round(MAP,4)),col=2)
    })
    output$Combined_plot<-renderPlot({
      Xr<-Xr()
      AXr<-AXr()
      LXr<-LXr()
      graphics::plot(Xr,5*AXr*LXr,type='l',xlab=expression(X[r]),ylab=expression(f(X[r])),ylim=c(0,0.20),xlim=c(-4,4),main=expression(paste("Prior,Likelihood,and Posterior Distributions")),sub=expression(paste("Note: Posterior distribution is rescaled for comperability!")))
      lines(Xr,LXr,type='l',col="blue")
      lines(Xr,AXr,type='l',col="red")
      EAP<-sum(Xr*AXr*LXr)/sum(AXr*LXr)
      text(-3.5,0.2,paste('Posterior Mean =',round(EAP,2)),col="black")
      text(3.5,0.2,paste('Prior Mean =',input$mean_eapmappae),col="red")
      abline(v=input$mean_eapmappae,col="red",lty=2)
      abline(v=EAP,col="black",lty=2)
      legend(legend=c("Prior","Likelihood","Posterior"),col=c("red","blue","black"),lty=1,title="Distribution","right")})
  }
  shiny::shinyApp(ui=source("ui.R"),server=server)
}

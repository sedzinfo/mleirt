##########################################################################################
# MLE IRT
##########################################################################################
#' @title Explore maximum likelihood, EAP and MAP estimation in item response theory
#' @description Launches a Shiny app that shows, with interactive plots, how the estimates
#' of item response theory (IRT) are found: a person's ability by maximum likelihood with the
#' Newton-Raphson method (Rasch model), an item's discrimination and difficulty by maximum
#' likelihood (two- and three-parameter logistic models), and a person's ability by Bayesian
#' expected a posteriori (EAP) and maximum a posteriori (MAP) estimation. The examples follow
#' de Ayala (2009), and the default settings reproduce his published results.
#' @details All tabs use the logistic item response function
#' \deqn{P(u = 1 \mid \theta) = c + (1 - c) \frac{\exp(\alpha(\theta - \delta))}{1 + \exp(\alpha(\theta - \delta))}}{P(u = 1 | theta) = c + (1 - c) * exp(alpha * (theta - delta)) / (1 + exp(alpha * (theta - delta)))}
#' the probability that a person of ability \eqn{\theta}{theta} answers an item correctly,
#' where \eqn{\delta}{delta} is the item's difficulty, \eqn{\alpha}{alpha} its discrimination
#' and \eqn{c} its guessing parameter. The Rasch model fixes \eqn{\alpha = 1}{alpha = 1} and
#' \eqn{c = 0}, the two-parameter logistic model (2PL) fixes \eqn{c = 0}, and the
#' three-parameter logistic model (3PL) estimates all three. No scaling constant is used
#' (\eqn{D = 1}). The likelihood of a set of responses is the product of the probabilities of
#' the responses given, and the log-likelihood is its logarithm.
#'
#' The app has four tabs, described in the sections below.
#' @section MLE Rasch:
#' How the Newton-Raphson method finds the maximum likelihood estimate of a person's ability
#' when the item difficulties are known. The test has five items with difficulties -1.90,
#' -0.60, -0.25, 0.30 and 0.45 (de Ayala, 2009, p. 22), and a raw score \eqn{r} is taken to
#' come from answering the first \eqn{r} items correctly: \eqn{r = 0} is the response pattern
#' 00000, \eqn{r = 2} is 11000 and \eqn{r = 5} is 11111.
#' \itemize{
#'   \item \strong{Controls:} the raw score, the starting value of the estimate (-2 to 2) and
#'     the iteration to show (1 to 5).
#'   \item \strong{Plot:} the log-likelihood of the response pattern for abilities from -6 to
#'     6. For the chosen iteration it shows the estimate the step starts from (red dot), the
#'     tangent to the log-likelihood there (blue line) and the estimate the step ends at (green
#'     dot), given by
#'     \eqn{\theta_1 = \theta_0 - LL'(\theta_0) / LL''(\theta_0)}{theta1 = theta0 - LL'(theta0) / LL''(theta0)},
#'     with the derivatives taken from a smoothing spline of the log-likelihood. The shaded
#'     triangle shows the angle between the tangent and the horizontal axis, which is zero at
#'     the maximum.
#'   \item \strong{Text:} the numbers of the Newton-Raphson step and the angle.
#' }
#' Raising the iteration shows the estimates converging on the maximum of the curve, which
#' reproduces the person locations of de Ayala (2009, Table 2.2, p. 28). For raw scores of 0
#' and 5 the log-likelihood keeps rising towards minus or plus infinity, so it has no maximum
#' and the steps move away: the maximum likelihood estimate does not exist for zero or perfect
#' scores.
#' @section MLE 2PL:
#' The log-likelihood surface of an item's discrimination and difficulty under the 2PL, and
#' where its maximum lies. The app simulates the responses of 50 people, whose abilities are
#' known and spread evenly from -3 to 3, to one item with the true discrimination and
#' difficulty set on the sliders, and draws new responses whenever a true value changes.
#' \itemize{
#'   \item \strong{Controls:} the true discrimination and difficulty (-3 to 3), and two
#'     sliders that set the viewing angle. The plot can also be rotated and zoomed with the
#'     mouse, and hovering shows the values under the pointer.
#'   \item \strong{Plot:} the log-likelihood of the simulated responses for every combination
#'     of discrimination and difficulty from -3 to 3 in steps of 0.1, as a 3-D surface. The red
#'     dot marks its highest point: the maximum likelihood estimates of the two parameters.
#'     With only 50 responses they differ from the true values by sampling error, and they
#'     change with every new draw.
#' }
#' @section MLE 3PL:
#' The same for the 3PL, to show how the guessing parameter affects the estimation. The app
#' simulates the responses of the same 50 people from the true guessing, discrimination and
#' difficulty set on the sliders, then computes the log-likelihood surface of discrimination
#' and difficulty (-3 to 3 in steps of 0.15) for each of twelve assumed guessing values, 0.05,
#' 0.10, ..., 0.60.
#' \itemize{
#'   \item \strong{Controls:} the true guessing (0.05 to 0.60), discrimination and difficulty,
#'     and two sliders that set the viewing angle.
#'   \item \strong{Plot:} the twelve surfaces drawn over each other in different colours,
#'     each with a dot at its highest point; the legend gives the guessing value of each
#'     colour, and clicking an entry hides or shows that surface. All surfaces share one
#'     vertical scale. Computing twelve surfaces takes time, and the plot can take a minute to
#'     appear.
#' }
#' @section EAP-MAP Rasch:
#' Bayesian estimation of a person's ability under the Rasch model, by quadrature. The test
#' has five items with difficulties fixed at -2.155, -0.245, 0.206, 0.984 and 1.211 (de Ayala,
#' 2009, Table 4.4), and raw scores map to response patterns as in the MLE Rasch tab. With
#' prior weights \eqn{A(X_r)} and likelihood \eqn{L(X_r)} at the quadrature points
#' \eqn{X_r},
#' \deqn{EAP = \frac{\sum_r X_r L(X_r) A(X_r)}{\sum_r L(X_r) A(X_r)}}{EAP = sum(X_r * L(X_r) * A(X_r)) / sum(L(X_r) * A(X_r))}
#' is the mean of the posterior distribution, and MAP is the quadrature point where the
#' posterior \eqn{L(X_r) A(X_r)} is highest.
#' \itemize{
#'   \item \strong{Controls:} the raw score; the number of quadrature points (10 to 100,
#'     spread evenly from -4 to 4); and the mean, standard deviation and skewness of the prior,
#'     a skew-normal distribution (\code{fGarch::dsnorm()}) that is normal when the skewness
#'     is 1.
#'   \item \strong{First sub-tab, four plots:} the prior weights at the quadrature points; the
#'     item trace lines, the probability of each observed response across ability (blue for
#'     items answered correctly, red for items answered incorrectly); the likelihood, the
#'     product of those probabilities; and the posterior, with lines and values for the EAP
#'     (blue) and the MAP (red).
#'   \item \strong{Combined sub-tab:} the prior (red), likelihood (blue) and posterior (black,
#'     rescaled to be comparable) in one plot, with dashed lines at the prior mean and the
#'     posterior mean.
#' }
#' The default settings replicate de Ayala (2009, p. 79). A narrower prior pulls the EAP
#' towards the prior mean, more quadrature points make both estimates more precise, and,
#' unlike the maximum likelihood estimate, EAP and MAP stay finite for raw scores of 0 and 5.
#' @section Acknowledgments:
#' The plots in this app were originally developed by Metin Bulus and Wes Bonifay for the
#' irtDemo package (Bulus and Bonifay, 2022), available on CRAN at
#' \url{https://cran.r-project.org/package=irtDemo}, and the app keeps their credits on the
#' 3PL tab (Metin Bulus) and the EAP-MAP tab (Metin Bulus and Wes Bonifay). Dimitrios
#' Zacharatos adapted them into this package.
#' @return A Shiny app object (\code{shiny::shinyApp()}). Printing it, which is what happens
#'   when you call \code{mleirt()} at the console, starts the app in the browser or the
#'   RStudio viewer; you can also pass it to \code{shiny::runApp()}.
#' @references Bulus, M., and Bonifay, W. (2022). irtDemo R Package: Pedagogical Interactive
#'   Web Applications for Estimation, Scoring, and Multi Dimensionality in Item Response
#'   Theory. \emph{Anadolu University Journal of Education Faculty}, 6(1), 92-108.
#'   \doi{10.34056/aujef.913781}
#'
#'   de Ayala, R. J. (2009). \emph{The Theory and Practice of Item Response Theory}. New York:
#'   Guilford Press.
#' @import shiny shinydashboard
#' @keywords mle irt
#' @export
#' @examples
#' if (interactive()) mleirt()
mleirt<-function(){
  ##########################################################################################
  # MLE ITEM DISCRIMINATION DIFFICULTY 2PL MODEL
  ##########################################################################################
  N_idd2pl<-50
  thetas_idd2pl<-seq(from=-3,to=3,length.out=N_idd2pl)
  deltas_idd2pl<-seq(from=-3,to=3,by=0.1)
  alphas_idd2pl<-seq(from=-3,to=3,by=0.1)
  k_idd2pl<-length(deltas_idd2pl)
  Pfun_idd2pl<-function(theta,alpha,delta,D=1,u=1){
    z=D*alpha*(theta-delta)
    P=exp(u*z)/(1+exp(z))
    return(P)
  }
  ##########################################################################################
  # MLE ITEM GUESSING DISCRIMINATION DIFFICULTY 3PL MODEL
  ##########################################################################################
  N_igdd3pl<-50
  guesses_igdd3pl<-seq(from=0.05,to=0.6,by=0.05)
  thetas_igdd3pl<-seq(from=-3,to=3,length.out=N_igdd3pl)
  deltas_igdd3pl<-seq(from=-3,to=3,by=0.15)
  alphas_igdd3pl<-seq(from=-3,to=3,by=0.15)
  k_igdd3pl<-length(deltas_igdd3pl)
  Pfun_igdd3pl<-function(guess,theta,alpha,delta,D=1,u=1){
    z=D*alpha*(theta-delta)
    P=guess+((1-guess)* exp(z)/(1+exp(z)))
    if(u==1){return(P)}
    if(u==0){return(1-P)}
  }
  pik_igdd3pl<-array(NA,dim=c(k_igdd3pl,k_igdd3pl,N_igdd3pl))
  logLike_igdd3pl<-matrix(NA,k_igdd3pl,k_igdd3pl)
  ##########################################################################################
  # MLE PERSON LOCATION GIVEN ITEM DIFFICULTY RACH
  ##########################################################################################
  Pfun_plid<-function(theta,delta,D=1,u=1){
    z=D*(theta-delta)
    P=exp(u*z)/(1 + exp(z))
    return(P)
  }
  deltas_plid<-c(-1.90,-0.60,-0.25,0.30,0.45)
  thetas_plid<-seq(from=-6,to=6,by=0.001)
  N_plid<-length(thetas_plid)
  n_plid<-length(deltas_plid)
  pji_plid<-matrix(NA,N_plid,n_plid)
  logLike_plid<-matrix(NA,N_plid,3)
  colnames(logLike_plid)<-c("thetas","LogLike","Like")
  ##########################################################################################
  # EAP MAP PERSON ABILITY ESTIMATION IN RACH MODEL
  ##########################################################################################
  # define probability or likelihood function for 2PL model
  PLfun_eapmappae<-function(theta,alpha,delta,D=1,u=1){
    z=D*alpha*(theta-delta)
    P=exp(u*z)/(1+exp(z))
    return(P)
  }
  alphas_eapmappae<-c(1,1,1,1,1) # discriminations
  deltas_eapmappae<-c(-2.155,-.245,.206,.984,1.211) # de Ayala difficulty estimates (Table 4.4)
  n_eapmappae<-length(deltas_eapmappae)
  ##########################################################################################
  # PLOTLY HELPERS
  ##########################################################################################
  # vertical line across the whole plot, for layout(shapes=)
  vline<-function(x,color,dash="solid"){
    list(type="line",x0=x,x1=x,y0=0,y1=1,yref="paper",line=list(color=color,dash=dash,width=1))
  }
  # camera from persp() style angles: theta azimuth, phi elevation (degrees)
  camera<-function(theta,phi,r=2.2){
    theta<-theta*pi/180
    phi<-phi*pi/180
    list(eye=list(x=r*cos(phi)*sin(theta),y=-r*cos(phi)*cos(theta),z=r*sin(phi)))
  }
  
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
                                  mainPanel(plotly::plotlyOutput(outputId="MLEIRT_plot",height="800px"),verbatimTextOutput("iterinfo"))),
                         ##########################################################################################
                         # MLE ITEM DISCRIMINATION DIFFICULTY 2PL MODEL
                         ##########################################################################################
                         tabPanel("MLE 2PL", titlePanel(h4("by unknown contributor")),
                                  sidebarPanel(width=3,
                                               sliderInput("alpha_idd2pl",label="Discrimination",min=-3,max=3,value=1,step=0.1),
                                               sliderInput("delta_idd2pl",label="Location",min=-3,max=3,value=-0.5,step=0.1)),
                                  helpText("Values on the control panel are true values and projection of red dot corresponds to the intersection of discrimination and difficulty parameters that maximize the joint log-likelihood."),
                                  mainPanel(plotly::plotlyOutput(outputId="est2PL_plot",width="100%",height="800px"))),
                         ##########################################################################################
                         # MLE ITEM DISCRIMINATION DIFFICULTY 2PL MODEL
                         ##########################################################################################
                         tabPanel("MLE 3PL", titlePanel(h4("by Metin Bulus")),
                                  sidebarPanel(width=3,
                                               sliderInput("guess_igdd3pl",label="Lower Asymptote",min=0.05,max=0.6,value=0.4,step=0.05),
                                               sliderInput("alpha_igdd3pl",label="Discrimination",min=-3,max=3,value=1,step=0.15),
                                               sliderInput("delta_igdd3pl",label="Location",min=-3,max=3,value=-0.5,step=0.15)),
                                  helpText("Note: The graph may take a minute to appear!"),
                                  mainPanel(plotly::plotlyOutput(outputId="est3PL_plot_all",width="100%",height="800px"))),
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
                                    column(width=6,plotly::plotlyOutput("Prior_plot")),
                                    column(width=6,plotly::plotlyOutput("TraceLines_plot")),
                                    column(width=6,plotly::plotlyOutput("Likelihood_plot")),
                                    column(width=6,plotly::plotlyOutput("Posterior_plot"))))),
                                    tabPanel("Combined",plotly::plotlyOutput(outputId="Combined_plot",width="100%",height="800px")))))
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
      for(j in 1:N_idd2pl){ datamtx_idd2pl[j]<-stats::rbinom(1,1,pcorrmtx_idd2pl[j]) }
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
    output$est2PL_plot<-plotly::renderPlotly({
      xy_idd2pl<-xy_idd2pl()
      logLike_idd2pl<-logLike_idd2pl()
      # surface z is indexed [y,x], so transpose the [alpha,delta] matrix
      plotly::plot_ly() |>
        plotly::add_surface(x=alphas_idd2pl,
                            y=deltas_idd2pl,
                            z=t(logLike_idd2pl),
                            colorscale=list(c(0,1),c("#347AB6","#347AB6")),
                            showscale=FALSE,
                            hovertemplate="Discrimination: %{x:.2f}<br>Difficulty: %{y:.2f}<br>Log-Likelihood: %{z:.3f}<extra></extra>") |>
        plotly::add_trace(type="scatter3d",mode="markers",
                          x=alphas_idd2pl[xy_idd2pl[1,1]],
                          y=deltas_idd2pl[xy_idd2pl[1,2]],
                          z=logLike_idd2pl[xy_idd2pl[1,1],xy_idd2pl[1,2]],
                          marker=list(color="red",size=7,line=list(color="black",width=1)),
                          hovertemplate="Maximum<br>Discrimination: %{x:.2f}<br>Difficulty: %{y:.2f}<br>Log-Likelihood: %{z:.3f}<extra></extra>") |>
        plotly::layout(showlegend=FALSE,
                       scene=list(xaxis=list(title="Discrimination"),
                                  yaxis=list(title="Difficulty"),
                                  zaxis=list(title="Log-Likelihood")))
    })
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
        datamtx_igdd3pl[j]<-stats::rbinom(1,1,pcorrmtx_igdd3pl[j])
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
    output$est3PL_plot_all<-plotly::renderPlotly({
      logLikes<-list(logLike1(),logLike2(),logLike3(),logLike4(),logLike5(),logLike6(),
                     logLike7(),logLike8(),logLike9(),logLike10(),logLike11(),logLike12())
      xys<-list(xy1(),xy2(),xy3(),xy4(),xy5(),xy6(),xy7(),xy8(),xy9(),xy10(),xy11(),xy12())
      colours<-grDevices::hcl.colors(length(guesses_igdd3pl),palette="Dark 3")
      p<-plotly::plot_ly()
      for(g in seq_along(guesses_igdd3pl)){
        group<-paste("c=",guesses_igdd3pl[g],sep="")
        xy<-xys[[g]]
        # surface and its maximum share a legend group, so the legend toggles both
        p<-p |>
          plotly::add_surface(x=alphas_igdd3pl,y=deltas_igdd3pl,z=t(logLikes[[g]]),
                              colorscale=list(c(0,1),c(colours[g],colours[g])),
                              showscale=FALSE,opacity=0.7,
                              name=group,legendgroup=group,showlegend=FALSE,
                              hovertemplate=paste(group,"<br>Discrimination: %{x:.2f}<br>Difficulty: %{y:.2f}<br>Log-Likelihood: %{z:.3f}<extra></extra>",sep="")) |>
          plotly::add_trace(type="scatter3d",mode="markers",
                            x=alphas_igdd3pl[xy[1,1]],
                            y=deltas_igdd3pl[xy[1,2]],
                            z=logLikes[[g]][xy[1,1],xy[1,2]],
                            marker=list(color=colours[g],size=6,line=list(color="black",width=1)),
                            name=group,legendgroup=group,
                            hovertemplate=paste(group," maximum<br>Discrimination: %{x:.2f}<br>Difficulty: %{y:.2f}<br>Log-Likelihood: %{z:.3f}<extra></extra>",sep=""))
      }
      p |>
        plotly::layout(legend=list(title=list(text="Guessing")),
                       scene=list(xaxis=list(title="Discrimination"),
                                  yaxis=list(title="Difficulty"),
                                  zaxis=list(title="Log-Likelihood")))
    })
    ##########################################################################################
    # MLE PERSON LOCATION GIVEN ITEM DIFFICULTY RACH
    ##########################################################################################
    nr_plid<-reactive({
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
      t<-logLike_plid[,1]
      ll<-logLike_plid[,2]
      spline<-stats::smooth.spline(ll ~ t)
      t_0<-input$t_0_plid
      # only the last iteration is shown, as in the original plot
      for(i in 1:input$iter_plid){
        fderiv0<-stats::predict(spline,x=t_0,deriv=0)
        fderiv1<-stats::predict(spline,x=t_0,deriv=1)
        fderiv2<-stats::predict(spline,x=t_0,deriv=2)
        t_1<-t_0-(fderiv1$y/fderiv2$y)
        yint<-fderiv0$y-(fderiv1$y*t_0)
        xint<--yint/fderiv1$y
        rad<-atan(fderiv0$y/(xint-t_0))
        deg<-rad*180/pi
        y_1<-stats::predict(spline,x=t_1,deriv=0)
        step<-list(t_0=t_0,t_1=t_1,y_0=fderiv0$y,y_1=y_1$y,d1=fderiv1$y,d2=fderiv2$y,yint=yint,xint=xint,deg=deg)
        t_0<-t_1
      }
      list(t=t,ll=ll,step=step)
    })
    output$MLEIRT_plot<-plotly::renderPlotly({
      nr<-nr_plid()
      s<-nr$step
      hat0<-"\u03b8\u0302<sup>0</sup>"
      hat1<-"\u03b8\u0302<sup>1</sup>"
      p<-plotly::plot_ly() |>
        plotly::add_lines(x=nr$t,y=nr$ll,line=list(color="black"),name="Log-likelihood",
                          hovertemplate="\u03b8: %{x:.3f}<br>LogLikelihood: %{y:.4f}<extra></extra>") |>
        plotly::add_lines(x=c(-6,6),y=s$yint+s$d1*c(-6,6),line=list(color="blue"),name="Tangent",hoverinfo="skip")
      if(round(abs(s$deg),digits=0)>0){
        p<-p |> plotly::add_trace(type="scatter",mode="lines",x=c(s$xint,s$t_0,s$t_0,s$xint),y=c(0,0,s$y_0,0),
                                  fill="toself",fillcolor="rgba(0,0,0,0.15)",line=list(color="black",width=1),
                                  name="Angle",hoverinfo="skip")
      }
      p |>
        plotly::add_markers(x=s$t_0,y=s$y_0,marker=list(color="red",size=12,line=list(color="black",width=1)),
                            name="Start",hovertemplate=paste(hat0,": %{x:.4f}<br>LogLikelihood: %{y:.4f}<extra></extra>",sep="")) |>
        plotly::add_markers(x=s$t_1,y=s$y_1,marker=list(color="green",size=12,line=list(color="black",width=1)),
                            name="Next",hovertemplate=paste(hat1,": %{x:.4f}<br>LogLikelihood: %{y:.4f}<extra></extra>",sep="")) |>
        plotly::layout(showlegend=FALSE,
                       xaxis=list(title="\u03b8",range=c(-6,6),zeroline=FALSE),
                       yaxis=list(title="LogLikelihood",range=c(-15,1),zeroline=FALSE),
                       shapes=list(list(type="line",x0=0,x1=1,xref="paper",y0=0,y1=0,line=list(color="gray",width=1)),
                                   vline(s$t_0,"gray","dot"),
                                   vline(s$t_1,"gray","dot")),
                       annotations=list(list(x=s$t_0,y=s$y_0-1,text=hat0,showarrow=FALSE),
                                        list(x=s$t_1,y=s$y_1-1,text=hat1,showarrow=FALSE)))
    })
    output$iterinfo<-renderText({
      s<-nr_plid()$step
      paste(sep="","Newton-Raphson step:","\n","theta1=theta0-(LL' / LL'')","\n",round(s$t_1,4),"=",round(s$t_0,4),"-","(",round(s$d1,4)," / ",round(s$d2,4),")","\n","\n","Acute angle with abscissa=",round(s$deg,digits=0)," degrees")
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
    output$Prior_plot<-plotly::renderPlotly({
      Xr<-Xr()
      AXr<-AXr()
      plotly::plot_ly() |>
        plotly::add_bars(x=Xr,y=AXr,width=abs(Xr[2]-Xr[1]),
                         marker=list(color="rgba(0,0,0,0.12)",line=list(color="black",width=1)),
                         hovertemplate="X<sub>r</sub>: %{x:.3f}<br>A(X<sub>r</sub>): %{y:.4f}<extra></extra>") |>
        plotly::add_lines(x=Xr,y=AXr,line=list(color="black"),hoverinfo="skip") |>
        plotly::layout(title="Prior Distribution of \u03b8",showlegend=FALSE,bargap=0,
                       xaxis=list(title="X<sub>r</sub>",range=c(-6,6)),
                       yaxis=list(title="A(X<sub>r</sub>)"))
    })
    output$TraceLines_plot<-plotly::renderPlotly({
      Xr<-Xr()
      pji<-pji()
      patt<-pattInput()
      p<-plotly::plot_ly()
      for(i in 1:n_eapmappae){
        response<-as.character(patt[i])
        p<-p |> plotly::add_lines(x=Xr,y=pji[,i],line=list(color=c("#DF536B","#2297E6")[patt[i]+1]),
                                  name=response,legendgroup=response,showlegend=!(patt[i] %in% patt[seq_len(i-1)]),
                                  hovertemplate=paste("Item ",i,"<br>X<sub>r</sub>: %{x:.3f}<br>P(X<sub>r</sub>): %{y:.4f}<extra></extra>",sep=""))
      }
      p |> plotly::layout(title="Item Tracelines",legend=list(title=list(text="Response")),
                          xaxis=list(title="X<sub>r</sub>",range=c(-3,3)),
                          yaxis=list(title="P(X<sub>r</sub>)",range=c(0,1)))
    })
    output$Likelihood_plot<-plotly::renderPlotly({
      Xr<-Xr()
      LXr<-LXr()
      plotly::plot_ly() |>
        plotly::add_lines(x=Xr,y=as.vector(LXr),line=list(color="black"),
                          hovertemplate="X<sub>r</sub>: %{x:.3f}<br>L(X<sub>r</sub>): %{y:.5f}<extra></extra>") |>
        plotly::layout(title="Likelihood",showlegend=FALSE,
                       xaxis=list(title="X<sub>r</sub>",range=c(-3,3)),
                       yaxis=list(title="L(X<sub>r</sub>)"))
    })
    output$Posterior_plot<-plotly::renderPlotly({
      Xr<-Xr()
      AXr<-AXr()
      LXr<-as.vector(LXr())
      EAP<-sum(Xr*AXr*LXr)/sum(AXr*LXr)
      PSD<-sqrt(sum((Xr-EAP)^2*LXr*AXr)/sum(LXr*AXr))
      MAP<-Xr[which.max(AXr*LXr)] #now it makes sense why it is iterative
      plotly::plot_ly() |>
        plotly::add_lines(x=Xr,y=AXr*LXr,line=list(color="black"),
                          hovertemplate="X<sub>r</sub>: %{x:.3f}<br>A(X<sub>r</sub>) * L(X<sub>r</sub>): %{y:.6f}<extra></extra>") |>
        plotly::layout(title="Posterior Distribution of \u03b8",showlegend=FALSE,
                       xaxis=list(title="X<sub>r</sub>",range=c(-2.5,2.5)),
                       yaxis=list(title="A(X<sub>r</sub>) * L(X<sub>r</sub>)"),
                       shapes=list(vline(EAP,"#2297E6"),vline(MAP,"#DF536B")),
                       annotations=list(list(x=-1.8,y=max(AXr*LXr)*.9,text=paste('EAP =',round(EAP,4)),showarrow=FALSE,font=list(color="#2297E6")),
                                        list(x=1.8,y=max(AXr*LXr)*.9,text=paste('MAP =',round(MAP,4)),showarrow=FALSE,font=list(color="#DF536B"))))
    })
    output$Combined_plot<-plotly::renderPlotly({
      Xr<-Xr()
      AXr<-AXr()
      LXr<-as.vector(LXr())
      EAP<-sum(Xr*AXr*LXr)/sum(AXr*LXr)
      plotly::plot_ly() |>
        plotly::add_lines(x=Xr,y=AXr,line=list(color="red"),name="Prior") |>
        plotly::add_lines(x=Xr,y=LXr,line=list(color="blue"),name="Likelihood") |>
        plotly::add_lines(x=Xr,y=5*AXr*LXr,line=list(color="black"),name="Posterior") |>
        plotly::layout(title="Prior, Likelihood, and Posterior Distributions<br><sup>Note: Posterior distribution is rescaled for comparability!</sup>",
                       legend=list(title=list(text="Distribution")),
                       xaxis=list(title="X<sub>r</sub>",range=c(-4,4)),
                       yaxis=list(title="f(X<sub>r</sub>)",range=c(0,0.20)),
                       shapes=list(vline(input$mean_eapmappae,"red","dash"),vline(EAP,"black","dash")),
                       annotations=list(list(x=-3.5,y=0.2,yanchor="top",text=paste('Posterior Mean =',round(EAP,2)),showarrow=FALSE,font=list(color="black")),
                                        list(x=3.5,y=0.2,yanchor="top",text=paste('Prior Mean =',input$mean_eapmappae),showarrow=FALSE,font=list(color="red"))))
    })
  }
  shiny::shinyApp(ui=ui,server=server)
}

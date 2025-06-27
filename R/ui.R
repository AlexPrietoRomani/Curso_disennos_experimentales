# R/ui.R
library(shiny)
library(shinythemes)

ui <- fluidPage(
  withMathJax(),
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      /* Títulos de sesión */
      .session-title {
        margin: 20px 0 15px;
        color: #2c3e50;
        border-bottom: 2px solid #e0e0e0;
        padding-bottom: 5px;
      }
      /* Encabezados de sección */
      .section-header {
        margin: 15px 0 5px;
        color: #34495e;
        font-weight: 600;
      }
      /* Tablas de actividades */
      .activity-table {
        width: 100%;
        margin: 1em 0;
        border-collapse: separate;
        border-spacing: 0;
      }
      .activity-table th,
      .activity-table td {
        padding: 8px;
        vertical-align: top;
        border: 1px solid #ddd;
      }
      .activity-table th {
        background: #f8f9fa;
      }
      /* Cajas de gráficos/imagenes */
      .plot-box, .image-box {
        border: 1px solid #ddd;
        padding: 10px;
        border-radius: 5px;
        margin: 1em 0;
        background: #fcfcfc;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      /* Mejoras para código R */
      .r-code {
          background: #2d2d2d;
          color: #e6e6e6;
          border-radius: 8px;
          padding: 15px;
          font-size: 0.9em;
          user-select: text;
          -webkit-user-select: text;
          cursor: pointer;
          position: relative;
      }
      .r-code:hover::after {
          content: 'Copiar';
          position: absolute;
          top: 5px;
          right: 10px;
          background: #424242;
          padding: 4px 8px;
          border-radius: 4px;
          font-size: 0.8em;
          color: #e0e0e0;
      }
      .decision-box {
          border: 1px solid #7f8c8d;
          background-color: #ecf0f1;
          padding: 10px;
          border-radius: 5px;
          display: inline-block;
      }
      .content-row {
          display: grid;
          grid-template-columns: 3fr 1fr;
          gap: 2em;
          margin-bottom: 2em;
          overflow: hidden;
      }
      .note-cloud {
          background: #e0f7fa;
          border-radius: 1.5em;
          padding: 1.2em;
          margin: 1em auto;
          max-width: 300px;
          position: relative;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          transition: transform 0.2s;
      }
      .note-cloud:hover {
          transform: translateY(-5px);
      }
      .note-cloud:after {
          content: '';
          position: absolute;
          bottom: -15px;
          left: 2em;
          border-width: 15px;
          border-style: solid;
          border-color: #e0f7fa transparent transparent transparent;
      }
      .note-cloud strong {
          color: #00796b;
          display: block;
          margin-bottom: 0.5em;
      }
      .small-diagram {
          text-align: center;
          margin: 0.5em 0 1em 0;
      }
      .venn-diagram {
          background: white;
          padding: 1em;
          border-radius: 1em;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
          margin: 1em auto;
          max-width: 200px;
      }
      .venn-diagram svg {
          width: 100%;
          height: auto;
          max-width: 120px;
      }
      .table-bordered {
          border-collapse: collapse !important;
          width: 100%;
          margin: 1em 0;
          font-size: 0.9em;
      }
      .table-bordered th,
      .table-bordered td {
          border: 1px solid #dee2e6 !important;
          padding: 0.75em;
          text-align: left;
      }
      .table-bordered th {
          background-color: #f8f9fa;
      }
      /* Responsive design */
      @media (max-width: 768px) {
          .content-row {
              grid-template-columns: 1fr;
          }
          .note-cloud {
              margin: 1em 0;
          }
      }

      /* Estilos para la línea de tiempo */
      .timeline-container {
        position: relative;
        padding: 20px 0;
      }

      .timeline {
        list-style: none;
        padding: 20px 0 20px;
        position: relative;
      }

      .timeline:before {
        top: 0;
        bottom: 0;
        position: absolute;
        content: ' ';
        width: 3px;
        background-color: #eeeeee;
        left: 50%;
        margin-left: -1.5px;
      }

      .timeline > li {
        margin-bottom: 20px;
        position: relative;
      }

      .timeline > li:before,
      .timeline > li:after {
        content: ' ';
        display: table;
      }

      .timeline > li:after {
        clear: both;
      }

      .timeline > li > .timeline-panel {
        width: 46%;
        float: left;
        border: 1px solid #d4d4d4;
        border-radius: 5px;
        padding: 20px;
        position: relative;
        box-shadow: 0 1px 6px rgba(0, 0, 0, 0.175);
      }

      .timeline > li > .timeline-badge {
        color: #fff;
        width: 50px;
        height: 50px;
        line-height: 50px;
        font-size: 1.4em;
        text-align: center;
        position: absolute;
        top: 16px;
        left: 50%;
        margin-left: -25px;
        background-color: #999999;
        z-index: 100;
        border-radius: 50%;
      }

      .timeline > li.timeline-inverted > .timeline-panel {
        float: right;
      }

      .timeline-panel:before {
        position: absolute;
        top: 26px;
        right: -15px;
        display: inline-block;
        border-top: 15px solid transparent;
        border-left: 15px solid #ccc;
        border-right: 0 solid #ccc;
        border-bottom: 15px solid transparent;
        content: ' ';
      }

      .timeline-inverted > .timeline-panel:before {
        border-left-width: 0;
        border-right-width: 15px;
        left: -15px;
        right: auto;
      }

      .timeline-panel:after {
          position: absolute;
          top: 27px;
          right: -14px;
          display: inline-block;
          border-top: 14px solid transparent;
          border-left: 14px solid #fff;
          border-right: 0 solid #fff;
          border-bottom: 14px solid transparent;
          content: ' ';
      }

      .timeline-inverted > .timeline-panel:after {
        border-left-width: 0;
        border-right-width: 14px;
        left: -14px;
        right: auto;
      }

      .timeline-badge.primary { background-color: #2e6da4 !important; }
      .timeline-badge.success { background-color: #3f903f !important; }
      .timeline-badge.info { background-color: #31b0d5 !important; }
      .timeline-badge.warning { background-color: #f0ad4e !important; }

      .timeline-title { margin-top: 0; color: inherit; }
      .timeline-body > p, .timeline-body > ul { margin-bottom: 0; }
    ")),
    tags$script(HTML("
      $(document).on('click', '.r-code', function() {  // Usa jQuery para elementos dinámicos
        const code = $(this).text().trim();
        navigator.clipboard.writeText(code).then(() => {
          const tooltip = $('<div>').text('¡Copiado!')
            .css({
              position: 'fixed',
              top: '20px',
              right: '20px',
              background: '#4CAF50',
              color: 'white',
              padding: '8px 12px',
              borderRadius: '4px'
            });
          $('body').append(tooltip);
          setTimeout(() => tooltip.remove(), 1500);
        });
      });
    "))
  ),
  
  titlePanel("📊 Temario de R para Estadística Agrícola"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      h4("Navegación"),
      radioButtons("parte", "Parte:", choices = names(sesiones)),
      uiOutput("sesion_ui")
    ),
    mainPanel(
      width = 10,
      uiOutput("contenido_ui")
    )
  )
)
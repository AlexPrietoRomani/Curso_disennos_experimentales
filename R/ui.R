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
        margin: 20px 0 15px; /* Simplifica usando shorthand [[4]] */
        color: #2c3e50;
        border-bottom: 2px solid #e0e0e0; /* Agrega separador visual */
        padding-bottom: 5px;
      }
      /* Encabezados de sección */
      .section-header {
        margin: 15px 0 5px; /* Mantén el espaciado original */
        color: #34495e;
        font-weight: 600;
      }
      /* Tablas de actividades */
      .activity-table {
        width: 100%;
        margin: 1em 0;
        border-collapse: separate; /* Evita colapso de bordes [[1]] */
        border-spacing: 0;
      }
      .activity-table th,
      .activity-table td {
        padding: 8px;
        vertical-align: top;
        border: 1px solid #ddd; /* Añade bordes consistentes */
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
        box-shadow: 0 2px 4px rgba(0,0,0,0.05); /* Profundidad sutil */
      }
      /* Mejoras para código R */
      .r-code {
          background: #2d2d2d;
          color: #e6e6e6;
          border-radius: 8px;
          padding: 15px;
          font-size: 0.9em;
          user-select: text; /* Permite seleccionar texto */
          -webkit-user-select: text; /* Compatibilidad con navegadores */
          cursor: pointer; /* Cambia el cursor al pasar el mouse */
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
      .content-row {
          display: grid;
          grid-template-columns: 3fr 1fr; /* Mantiene proporción 3:1 */
          gap: 2em;
          margin-bottom: 2em;
          overflow: hidden; /* Evita solapamiento */
      }
      .note-cloud {
          background: #e0f7fa;
          border-radius: 1.5em;
          padding: 1.2em;
          margin: 1em auto; /* Centrado horizontal */
          max-width: 300px; /* Ancho máximo razonable */
          position: relative;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          transition: transform 0.2s;
      }
      .note-cloud:hover {
          transform: translateY(-5px); /* Movimiento flotante */
      }
      .note-cloud:after {
          content: '';
          position: absolute;
          bottom: -15px; /* Ajusta posición de la punta */
          left: 2em;
          border-width: 15px; /* Tamaño de la punta */
          border-style: solid;
          border-color: #e0f7fa transparent transparent transparent;
      }
      .note-cloud strong {
          color: #00796b; /* Verde oscuro para títulos */
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
          margin: 1em auto; /* Centrado */
          max-width: 200px; /* Tamaño reducido */
      }
      .venn-diagram svg {
          width: 100%;
          height: auto;
          max-width: 120px; /* Más pequeño para mejor proporción */
      }
      .table-bordered {
          border-collapse: collapse !important; /* Fusionar bordes */
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
              grid-template-columns: 1fr; /* Una columna en móviles */
          }
          .note-cloud {
              margin: 1em 0;
          }
      }
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
      width = 3,
      h4("Navegación"),
      radioButtons("parte", "Parte:", choices = names(sesiones)),
      uiOutput("sesion_ui")
    ),
    mainPanel(
      width = 9,
      uiOutput("contenido_ui")
    )
  )
)
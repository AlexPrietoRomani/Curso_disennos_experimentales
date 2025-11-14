function applySessionLabelTitles(context) {
  const $context = context ? $(context) : $(document);
  $context.find('.session-list-item').each(function() {
    const $item = $(this);
    const labelText = $item.find('.session-list-label').text().trim();
    if (labelText && $item.attr('title') !== labelText) {
      $item.attr('title', labelText);
    }
  });
}

/**
 * Inicializa las pestañas de una sesión:
 * - Guarda el texto completo en data-fullLabel
 * - Genera "Pestaña N" como data-shortLabel (si hay más de una pestaña)
 * - Ajusta el texto visible según si está activa o no
 */
function initSessionTabs(context) {
  const $context = context ? $(context) : $(document);
  const selector =
    '.session-detail-body .nav-tabs .nav-link, ' +
    '.session-detail-body .nav-pills .nav-link';

  const $links = $context.find(selector);
  if (!$links.length) return;

  // Si sólo hay una pestaña, no compactamos; sólo ponemos el title
  if ($links.length === 1) {
    $links.each(function() {
      const $link = $(this);
      const fullLabel = $.trim($link.text());
      if (!$link.data('fullLabel')) {
        $link.data('fullLabel', fullLabel);
        $link.attr('title', fullLabel);
      }
    });
    return;
  }

  // Hay varias pestañas: compactamos a "Pestaña N"
  $links.each(function(index) {
    const $link = $(this);

    let fullLabel = $link.data('fullLabel');
    if (!fullLabel) {
      fullLabel = $.trim($link.text());
      $link.data('fullLabel', fullLabel);
      $link.attr('title', fullLabel);
    }

    let shortLabel = $link.data('shortLabel');
    if (!shortLabel) {
      shortLabel = 'Pestaña ' + (index + 1);
      $link.data('shortLabel', shortLabel);
    }
  });

  updateSessionTabsView($links);
}

/**
 * Actualiza el texto visible de cada pestaña según su estado:
 * - activa  -> texto completo
 * - inactiva -> texto corto "Pestaña N"
 */
function updateSessionTabsView($links) {
  const selector =
    '.session-detail-body .nav-tabs .nav-link, ' +
    '.session-detail-body .nav-pills .nav-link';

  const $allLinks = $links && $links.length ? $links : $(selector);

  $allLinks.each(function() {
    const $link = $(this);
    const fullLabel = $link.data('fullLabel');
    const shortLabel = $link.data('shortLabel') || fullLabel;

    if ($link.hasClass('active')) {
      if (fullLabel) {
        $link.text(fullLabel);
        $link.addClass('is-expanded');
      }
    } else {
      if (shortLabel) {
        $link.text(shortLabel);
        $link.removeClass('is-expanded');
      }
    }
  });
}

// ---------------------------------------------------------------------
// Eventos Shiny: cuando se renderiza sidebar o contenido de sesión
// ---------------------------------------------------------------------
$(document).on('shiny:value', function(event) {
  if (event.target && event.target.id === 'course_sidebar') {
    applySessionLabelTitles(event.target);
  }
  if (event.target && event.target.id === 'contenido_ui') {
    // Cada vez que se vuelve a renderizar el contenido de la sesión,
    // re-inicializamos las pestañas.
    initSessionTabs(event.target);
  }
});

// Sidebar: tooltips de sesiones
$(document).on('mouseenter focus', '.session-list-item', function() {
  applySessionLabelTitles(this);
});

// ---------------------------------------------------------------------
// Pestañas: expandir/plegar en hover, foco y cambio de pestaña activa
// ---------------------------------------------------------------------
const tabSelector =
  '.session-detail-body .nav-tabs .nav-link, ' +
  '.session-detail-body .nav-pills .nav-link';

// Cuando cambia la pestaña activa (evento propio de Bootstrap 5)
$(document).on('shown.bs.tab', tabSelector, function() {
  const container = $(this).closest('.session-detail-body');
  initSessionTabs(container.length ? container[0] : document);
});

// Hover / foco: mostrar título completo
$(document).on('mouseenter focus', tabSelector, function() {
  const $link = $(this);
  const fullLabel = $link.data('fullLabel');

  if (fullLabel) {
    $link.text(fullLabel);
    $link.addClass('is-expanded');
  }
});

// Salir de hover / perder foco: volver a corto si no es la activa
$(document).on('mouseleave blur', tabSelector, function() {
  const $link = $(this);

  if ($link.hasClass('active')) {
    // La activa siempre muestra el título completo
    const fullLabel = $link.data('fullLabel');
    if (fullLabel) {
      $link.text(fullLabel);
      $link.addClass('is-expanded');
    }
    return;
  }

  const shortLabel = $link.data('shortLabel') || $link.data('fullLabel');
  if (shortLabel) {
    $link.text(shortLabel);
    $link.removeClass('is-expanded');
  }
});

// Inicialización general al cargar la página
$(document).ready(function() {
  applySessionLabelTitles(document);
  initSessionTabs(document);
});

// ---------------------------------------------------------------------
// Copiar bloques de código R al portapapeles
// ---------------------------------------------------------------------
$(document).on('click', '.r-code', function() {
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

// ---------------------------------------------------------------------
// Menú responsive de navegación principal
// ---------------------------------------------------------------------
$(document).on('click', '.nav-toggle', function() {
  const $button = $(this);
  const isExpanded = $button.attr('aria-expanded') === 'true';
  const $navLinks = $button.closest('.primary-nav').find('.nav-links');
  $navLinks.toggleClass('is-open', !isExpanded);
  $button.attr('aria-expanded', String(!isExpanded));
});

$(document).on('click', '.nav-links .nav-link', function() {
  const $menu = $(this).closest('.nav-links');
  const $toggle = $menu.siblings('.nav-toggle');
  if ($menu.hasClass('is-open')) {
    $menu.removeClass('is-open');
    $toggle.attr('aria-expanded', 'false');
  }
});

$(window).on('resize', function() {
  if (window.innerWidth > 992) {
    $('.primary-nav .nav-links').removeClass('is-open');
    $('.primary-nav .nav-toggle').attr('aria-expanded', 'false');
  }
});

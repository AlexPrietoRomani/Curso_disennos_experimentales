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

$(document).on('shiny:value', function(event) {
  if (event.target && event.target.id === 'course_sidebar') {
    applySessionLabelTitles(event.target);
  }
});

$(document).on('mouseenter focus', '.session-list-item', function() {
  applySessionLabelTitles(this);
});

$(document).ready(function() {
  applySessionLabelTitles(document);
});

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


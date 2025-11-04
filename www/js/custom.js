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


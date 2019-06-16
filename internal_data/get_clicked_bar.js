$(document).on('plotly_click', '#%s', function() {
  selector = "#%s > div > div > svg:nth-child(5) > .hoverlayer > g.hovertext > text > tspan:nth-child(1)";
  out = document.querySelector(selector).innerHTML;
  Shiny.onInputChange('%s', out);
});

var width = (window.innerWidth * .8) - 20;

if(width < 1200){
var width = (window.innerWidth) - 20;
}


// var width = document.getElementById("full_bodied").offsetWidth;
var height = width * .6;

var stage_circles = [{'line_1':'Pre-birth', 'line_2': 'to early', 'line_3': 'years', "x":.05, "y":.1},{'line_1':'School', 'line_2': 'years', "x":.6, "y":.1},{"line_1":'Early', 'line_2': 'working', 'line_3':'life', "x":.52, "y":.35},{"line_1":'Mid working', 'line_2':'life to', 'line_3':'retirement', "x":.47, "y":.6},{"line_1":'Retirement', 'line_2':'to older age', "x":.6, "y":.85}]

var stage_arrows = [{"x":.11, "y":.1, "img":'right_blue.png'},{"x":.37, "y":.1, "img":'right_blue.png'},{"x":.67, "y":.1, "img":'right_blue.png'},{"x":.95, "y":.1, "img":'right_blue.png'},{"x":.95, "y":.35, "img":'right_blue.png'},{"x":.665, "y":.35, "img":'right_blue.png'},{"x":.3, "y":.35, "img":'right_blue.png'},{"x":.05, "y":.35, "img":'right_blue.png'},{"x":.05, "y":.6, "img":'right_brown.png'},{"x":.315, "y":.6, "img":'right_blue.png'},{"x":.675, "y":.6, "img":'right_blue.png'},{"x":.95, "y":.6, "img":'right_blue.png'},{"x":.95, "y":.85, "img":'right_blue.png'},{"x":.3, "y":.85, "img":'right_blue.png'}]

var significance_key = d3.scaleOrdinal()
.domain(['Not applicable', 'Significantly lower','Similar','Significantly higher'])
.range(['It is not appropriate to compare this value with England statistically', 'This is significantly lower compared to England', 'This is not statistically different compared to England', 'This is significantly higher compared to England'])

var polarity_key = d3.scaleOrdinal()
.domain(['Not applicable', 'Lower is better','Similar','Higher is better'])
.range(['', 'For this indicator, a lower value is better.', 'For this indicator, a higher value is better.'])

// Add some scale functions to place circles on the svg
var x_pos = d3.scaleLinear()
.range([0, width]);

var y_pos = d3.scaleLinear()
.range([0, height]);

var stage_size = d3.scaleLinear()
.domain([0,2000])
.range([10,70]);

var arrow_size = d3.scaleLinear()
.domain([0,2000])
.range([0,20]);

var circle_size = d3.scaleLinear()
.domain([0,2000])
.range([5,40]);

// possibly a font size scale?

// Set up tooltip for circles
var tooltip_lt = d3.select("#wsx_lower_tier_walkthrough")
    .append("div")
    .style("opacity", 0)
    .attr("class", "tooltip")
    .style("position", "absolute")
    .style("z-index", "10")
    .style("background-color", "white")
    .style("border", "solid")
    .style("border-width", "1px")
    .style("border-radius", "5px")
    .style("padding", "10px")

var showTooltip_lt = function(d, i) {

tooltip_lt
  .html("<h4>" + d.Name + '</h4><p class = "tt_text">'+ d.Description + '</p><p class = "tt_text">In ' + d.Area_name + ' in ' + d.Timeperiod + ' the ' + d.Unit + ' was <font color = "#1e4b7a" size = "3"><b>' + d.Label + '</b></font>.</p><p class = "tt_text">' + significance_key(d.Significance) + '. ' + polarity_key(d.Polarity))
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible")
  }

var mouseleave = function(d) {
  tooltip_lt.style("visibility", "hidden")
  }

// Components of change
var request = new XMLHttpRequest();
  request.open("GET", "./lt_data_extract_compare_england.json", false);
  request.send(null);

var json = JSON.parse(request.responseText); // parse the fetched json data into a variable

// List of areas in the dataset
var areas_lower_tier = d3.map(json, function (d) {
    return (d.Area_name)
    })
    .keys()

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_area_lt_button")
  .selectAll('myOptions')
  .data(areas_lower_tier)
  .enter()
  .append('option')
  .text(function (d) {
        return d; }) // text to appear in the menu - this does not have to be as it is in the data (you can concatenate other values).
  .attr("value", function (d) {
        return d; }) // corresponding value returned by the button

var selected_area_option = d3.select('#select_area_lt_button').property("value")

// append the svg object to the body of the page
var svg_lt_walkthrough = d3.select("#wsx_lower_tier_walkthrough")
.append("svg")
.attr("width", width)
.attr("height", height)
.append("g");

// Walkthrough dotted lines
svg_lt_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.1)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.1)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.95)})
  .attr('y1', function(d){ return y_pos(0.1)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.35)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.35)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.35)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.35)})
  .attr('x2', function(d){ return x_pos(0.05)})
  .attr('y2', function(d){ return y_pos(0.6)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.6)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.6)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.95)})
  .attr('y1', function(d){ return y_pos(0.6)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.85)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.85)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.85)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

// Big circles for each stage of life
svg_lt_walkthrough
  .selectAll()
  .data(stage_circles)
  .enter()
  .append('circle')
  .attr("cx", function(d){ return x_pos(d.x) })
  .attr("cy", function(d){ return y_pos(d.y) })
  .attr("r",  function(d){ return stage_size(width)})
  .style("fill", "black")
  .attr('id', 'stage_labels');

svg_lt_walkthrough
  .selectAll()
  .data(stage_circles)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y) - 5 })
  .text(function(d){ return d.line_1})
  .style("fill", "white")
  .style('stroke','white')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'stage_line_1');

svg_lt_walkthrough
  .selectAll()
  .data(stage_circles)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y) + 10 })
  .text(function(d){ return d.line_2})
  .style("fill", "white")
  .style('stroke','white')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'stage_line_2');

svg_lt_walkthrough
  .selectAll()
  .data(stage_circles)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y) + 25 })
  .text(function(d){ return d.line_3})
  .style("fill", "white")
  .style('stroke','white')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'stage_line_3');

// Small circles for arrows
svg_lt_walkthrough
  .selectAll()
  .data(stage_arrows)
  .enter()
  .append("circle")
  .attr('id', "stage_arrow_circles")
  .attr("cx", function(d){ return x_pos(d.x) } )
  .attr("cy", function(d){ return y_pos(d.y) } )
  .attr("r",  function(d){ return arrow_size(width)});

var selected_area_df = json.filter(function(d){
    return d.Area_name === selected_area_option})

svg_lt_walkthrough
.selectAll('.outcomes')
.data(selected_area_df)
.enter()
.append("circle")
.attr('class','outcomes')
.attr("cx", function(d){ return x_pos(d.x) } )
.attr("cy", function(d){ return y_pos(d.y) } )
.attr("r",  function(d){ return circle_size(width)})
.attr('fill',  function(d){ return d.Colour})
.attr('stroke', 'none')
.on("mousemove", showTooltip_lt)
.on('mouseout', mouseleave);

svg_lt_walkthrough
  .selectAll()
  .data(selected_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .075) })
  .text(function(d){ return d.Label_screen})
  .style("fill", "black")
  .style('font-weight', 'bold')
  .style('stroke','none')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'label_1');

svg_lt_walkthrough
  .selectAll()
  .data(selected_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .1) })
  .text(function(d){ return d.line_1})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'line_1');

svg_lt_walkthrough
  .selectAll()
  .data(selected_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .12) })
  .text(function(d){ return d.line_2})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'line_2');

svg_lt_walkthrough
  .selectAll()
  .data(selected_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .14) })
  .text(function(d){ return d.line_3})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'line_3');

svg_lt_walkthrough
  .selectAll()
  .data(selected_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .16) })
  .text(function(d){ return d.line_4})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'line_4');

svg_lt_walkthrough
  .selectAll()
  .data(selected_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .18) })
  .text(function(d){ return d.line_5})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'line_5');

function update_lt_walkthrough(selected_area_option) {

// Set up tooltip for circles
var tooltip_lt = d3.select("#wsx_lower_tier_walkthrough")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("background-color", "white")
  .style("border", "solid")
  .style("border-width", "1px")
  .style("border-radius", "5px")
  .style("padding", "10px")

var showTooltip_lt = function(d, i) {

tooltip_lt
  .html("<h4>" + d.Name + '</h4><p class = "tt_text">'+ d.Description + '</p><p class = "tt_text">In ' + d.Area_name + ' in ' + d.Timeperiod + ' the ' + d.Unit + ' was <font color = "#1e4b7a" size = "3"><b>' + d.Label + '</b></font>.</p><p class = "tt_text">' + significance_key(d.Significance) + '. ' + polarity_key(d.Polarity))
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible")
  }

var mouseleave = function(d) {
  tooltip_lt.style("visibility", "hidden")
  }

var selected_area_option = d3.select('#select_area_lt_button').property("value")

var selected_area_df = json.filter(function(d){
    return d.Area_name === selected_area_option})

svg_lt_walkthrough
  .selectAll("#label_1")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_lt_walkthrough
  .selectAll("#line_1")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_lt_walkthrough
  .selectAll("#line_2")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_lt_walkthrough
  .selectAll("#line_3")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_lt_walkthrough
  .selectAll("#line_4")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_lt_walkthrough
  .selectAll("#line_5")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_lt_walkthrough
  .selectAll()
  .data(selected_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .075) })
  .text(function(d){ return d.Label_screen})
  .style("fill", "black")
  .style('font-weight', 'bold')
  .style('stroke','none')
  .attr('text-anchor','middle')
  .style('font-size', '.8rem')
  .attr('id', 'label_1')
  .style('opacity', 0)
  .transition()
  .duration(750)
  .style('opacity', 1);

  svg_lt_walkthrough
    .selectAll()
    .data(selected_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .1) })
    .text(function(d){ return d.line_1})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .style('font-size', '.8rem')
    .attr('id', 'line_1')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_lt_walkthrough
    .selectAll()
    .data(selected_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .12) })
    .text(function(d){ return d.line_2})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .style('font-size', '.8rem')
    .attr('id', 'line_2')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_lt_walkthrough
    .selectAll()
    .data(selected_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .14) })
    .text(function(d){ return d.line_3})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .style('font-size', '.8rem')
    .attr('id', 'line_3')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_lt_walkthrough
    .selectAll()
    .data(selected_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .16) })
    .text(function(d){ return d.line_4})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .style('font-size', '.8rem')
    .attr('id', 'line_4')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_lt_walkthrough
    .selectAll()
    .data(selected_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .18) })
    .text(function(d){ return d.line_5})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .style('font-size', '.8rem')
    .attr('id', 'line_5')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

svg_lt_walkthrough
  .selectAll('.outcomes')
  .data(selected_area_df)
  .transition()
  .duration(1750)
  .attr('fill',  function(d){ return d.Colour})
  .on("mousemove", showTooltip_lt)
  .on('mouseout', mouseleave);
  }

  d3.select("#select_area_lt_button").on("change", function(d) {
    var selected_area_option = d3.select('#select_area_lt_button').property("value")
  update_lt_walkthrough(selected_area_option)
  })
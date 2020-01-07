var width = (window.innerWidth * .8) - 20;
// var width = document.getElementById("full_bodied").offsetWidth;
var height = width * .6;

var stage_circles = [{"Stage":"Pre-birth to early years", "x":.05, "y":.1},{"Stage":"School years", "x":.6, "y":.1},{"Stage":"Early working life", "x":.52, "y":.35},{"Stage":"Mid working life to retirement", "x":.47, "y":.6},{"Stage":"Retirement to older age", "x":.6, "y":.85}]

var stage_arrows = [{"x":.11, "y":.1, "img":'right_blue.png'},{"x":.37, "y":.1, "img":'right_blue.png'},{"x":.67, "y":.1, "img":'right_blue.png'},{"x":.95, "y":.1, "img":'right_blue.png'},{"x":.95, "y":.35, "img":'right_blue.png'},{"x":.665, "y":.35, "img":'right_blue.png'},{"x":.3, "y":.35, "img":'right_blue.png'},{"x":.05, "y":.35, "img":'right_blue.png'},{"x":.05, "y":.6, "img":'right_brown.png'},{"x":.315, "y":.6, "img":'right_blue.png'},{"x":.675, "y":.6, "img":'right_blue.png'},{"x":.95, "y":.6, "img":'right_blue.png'},{"x":.95, "y":.85, "img":'right_blue.png'},{"x":.3, "y":.85, "img":'right_blue.png'}]

// add a scale to place circles on the svg
// Add X axis
var x_pos = d3.scaleLinear()
.range([0, width]);

var y_pos = d3.scaleLinear()
.range([0, height]);

var stage_size = d3.scaleLinear()
.domain([0,2000])
.range([10,70])

var arrow_size = d3.scaleLinear()
.domain([0,2000])
.range([0,20])

var circle_size = d3.scaleLinear()
.domain([0,2000])
.range([5,40])

// Components of change
var request = new XMLHttpRequest();
    request.open("GET", "./lt_data_extract_compare_england.json", false);
    request.send(null);

var json = JSON.parse(request.responseText); // parse the fetched json data into a variable

// List of years in the dataset
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

svg_lt_walkthrough
  .append("line")
  .attr('id','dotty')
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.1)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.1)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('id','dotty')
  .attr('x1', function(d){ return x_pos(0.95)})
  .attr('y1', function(d){ return y_pos(0.1)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.35)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('id','dotty')
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.35)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.35)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('id','dotty')
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.35)})
  .attr('x2', function(d){ return x_pos(0.05)})
  .attr('y2', function(d){ return y_pos(0.6)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('id','dotty')
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.6)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.6)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('id','dotty')
  .attr('x1', function(d){ return x_pos(0.95)})
  .attr('y1', function(d){ return y_pos(0.6)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.85)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .append("line")
  .attr('id','dotty')
  .attr('x1', function(d){ return x_pos(0.05)})
  .attr('y1', function(d){ return y_pos(0.85)})
  .attr('x2', function(d){ return x_pos(0.95)})
  .attr('y2', function(d){ return y_pos(0.85)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_lt_walkthrough
  .selectAll("stage_labels")
  .data(stage_circles)
  .enter()
  .append("circle")
  .attr("cx", function(d){ return x_pos(d.x) } )
  .attr("cy", function(d){ return y_pos(d.y) } )
  .attr("r",  function(d){ return stage_size(width)})
  .style("fill", "black");

svg_lt_walkthrough
  .selectAll("stage_labels")
  .data(stage_arrows)
  .enter()
  .append("circle")
  .attr("cx", function(d){ return x_pos(d.x) } )
  .attr("cy", function(d){ return y_pos(d.y) } )
  .attr("r",  function(d){ return arrow_size(width)})
  .style("fill", "right_blue.png");


// var selected_area_df = json.filter(function(d){
        // return d.Area_name === selected_area_option})

// svg_lt_walkthrough
//   .selectAll("selected_indicators")
//   .data(selected_area_df)
//   .enter()
//   .append("circle")
//   .attr("cx", function(d){ return x_pos(d.x) } )
//   .attr("cy", function(d){ return y_pos(d.y) } )
//   .attr("r",  function(d){ return circle_size(width)})
//   .attr('fill',  function(d){ return d.Colour})
//   .attr('stroke', function(d){ return d.Colour});


  function update_lt_walkthrough(selected_area_option) {

    var selected_area_option = d3.select('#select_area_lt_button').property("value")

    var selected_area_df = json.filter(function(d){
            return d.Area_name === selected_area_option})

    svg_lt_walkthrough
      .selectAll("selected_indicators")
      .data(selected_area_df)
      .enter()
      .append("circle")
      .attr("cx", function(d){ return x_pos(d.x) })
      .attr("cy", function(d){ return y_pos(d.y) })
      .transition()
      .duration(1750)
      .attr("r",  function(d){ return circle_size(width)})
      .attr('fill',  function(d){ return d.Colour})
      .attr('stroke', function(d){ return d.Colour});

  }

  d3.select("#select_area_lt_button").on("change", function(d) {
    var selected_area_option = d3.select('#select_area_lt_button').property("value")
  update_lt_walkthrough(selected_area_option)
  })

update_lt_walkthrough(selected_area_option)
// Create values
// var nat_change = d3.max(wsx_coc, function (d) {
  // return +d.natchange;});

// console.table(adur_lc)

// wsx_lower_tier_walkthrough

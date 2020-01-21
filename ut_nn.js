
var width_ut_nn = document.getElementById("ut_nearest_neighbours").offsetWidth;
var height_ut_nn = 400;

// margins
var margin = {
    top: 30,
    right: 250,
    bottom: 80,
    left: 30
};

// data frame
var request = new XMLHttpRequest();
  request.open("GET", "./ut_data_neighbours.json", false);
  request.send(null);

var json_ut_nn = JSON.parse(request.responseText); // parse the fetched json data into a variable

// List of areas in the dataset
var areas_upper_tier_explore = d3.map(json_ut_nn, function (d) {
    return (d.Area_x)
    })
    .keys()

// List of areas in the dataset
var indicators_upper_tier_explore = d3.map(json_ut_nn, function (d) {
  return (d.Name)
  })
  .keys()

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_area_ut_explore_button")
  .selectAll('myOptions')
  .data(areas_upper_tier_explore)
  .enter()
  .append('option')
  .text(function (d) {
        return d; }) // text to appear in the menu - this does not have to be as it is in the data (you can concatenate other values).
  .attr("value", function (d) {
        return d; }) // corresponding value returned by the button

var selected_ut_area_option = d3.select('#select_area_ut_explore_button').property("value")

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_indicator_ut_explore_button")
  .selectAll('myOptions')
  .data(indicators_upper_tier_explore)
  .enter()
  .append('option')
  .text(function (d) {
        return d; }) // text to appear in the menu - this does not have to be as it is in the data (you can concatenate other values).
  .attr("value", function (d) {
        return d; }) // corresponding value returned by the button

var selected_ut_indicator_option = d3.select('#select_indicator_ut_explore_button').property("value")

selected_area_ind_ut = json_ut_nn.filter(function (d) { // gets a subset of the json data
    return d.Name === selected_ut_indicator_option &
        d.Area_x === selected_ut_area_option })
    .sort(function (a, b) { // sorts it according to the number of deaths (descending order)
        return d3.ascending(a.Rank, b.Rank);
    })

var x_scale_areas_ut = d3.scaleBand()
.range([0, width_ut_nn - margin.left - margin.right])
.padding(0.2);

var y_scale_explore_ind_ut = d3.scaleLinear() // our y axis is continuous (linear)
.range([height_ut_nn - margin.bottom - margin.top, 0]);

// append the svg object to the body of the page
var svg_ut_walkthrough = d3.select("#ut_nearest_neighbours")
.append("svg")
.attr("width", width_ut_nn)
.attr("height", height_ut_nn)
.append("g");

var xAxis_ind_explore_ut = svg_ut_walkthrough
.append("g")
.attr("transform", "translate(" + margin.left + "," + (height_ut_nn - margin.bottom) + ")");

x_scale_areas_ut
.domain(selected_area_ind_ut.map(function (d) {
  return d.Area_name; }))

xAxis_ind_explore_ut
.call(d3.axisBottom(x_scale_areas_ut))

xAxis_ind_explore_ut
.selectAll("text")
.attr("transform", "rotate(-45)")
.style("text-anchor", "end")
.style('stroke-opacity',0);

var yAxis_ind_explore_ut = svg_ut_walkthrough
.append("g")
.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

y_scale_explore_ind_ut
.domain([0, d3.max(selected_area_ind_ut, function (d) {
return d.Value })]); // update the yaxis based on 'data'

yAxis_ind_explore_ut
.call(d3.axisLeft(y_scale_explore_ind_ut));

yAxis_ind_explore_ut
.selectAll('text')
.style("stroke-opacity", 0)

var bars_ut_nn = svg_ut_walkthrough
.selectAll("rect")
.data(selected_area_ind_ut)

bars_ut_nn
.enter()
.append("rect") // Add a new rect for each new element
.merge(bars_ut_nn) // get the already existing elements as well
.attr("x", function (d) {
  return x_scale_areas_ut(d.Area_name) + margin.left;
  })
.attr("y", function (d) {
  return y_scale_explore_ind_ut(d.Value) ;
  })
.attr("width", x_scale_areas_ut.bandwidth())
.attr("height", function (d) {
  return height_ut_nn - y_scale_explore_ind_ut(d.Value) - margin.bottom;
  })
.style("fill", function (d) {
  return d.Colour })
.style("stroke-opacity", function(d) {
         if (d.Area_name === selected_ut_area_option) {
         return 1
         } else {
         return 0
         }
     });

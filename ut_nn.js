// margins
var margin = {
    top: 40,
    right: 250,
    bottom: 200,
    left: 50
};

var possible_units = d3.scaleOrdinal()
    .domain(["proportion", "rate", "average score", "life expectancy in years"])
    .range(['Proportion', 'Rate', 'Average score', 'Years']);


var width_ut_nn = document.getElementById("ut_nearest_neighbours").offsetWidth;
var height_ut_nn = 425;

// data
var request = new XMLHttpRequest();
    request.open("GET", "./ut_data_meta_extract.json", false);
    request.send(null);
var json_ut_meta = JSON.parse(request.responseText); // parse the fetched json data into a variable

// data
var request = new XMLHttpRequest();
    request.open("GET", "./ut_data_neighbours.json", false);
    request.send(null);
var json_ut_nn = JSON.parse(request.responseText); // parse the fetched json data into a variable

// data
var request = new XMLHttpRequest();
    request.open("GET", "./Comp_data_ut.json", false);
    request.send(null);
var json_comp_ut = JSON.parse(request.responseText); // parse the fetched json data into a variable

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

selected_ut_nn_max = d3.map(selected_area_ind_ut, function (d) {
  return (d.Max_value)
  })
  .keys()

selected_area_ut_nn_explore = selected_area_ind_ut.filter(function (d) { // gets a subset of the json data
    return d.Area_name === selected_ut_area_option })

selected_ut_meta = json_ut_meta.filter(function (d) { // gets a subset of the json data
    return d.Name === selected_ut_indicator_option })

selected_ut_ind_comp = json_comp_ut.filter(function (d) { // gets a subset of the json data
        return d.Name === selected_ut_indicator_option })

// append the svg object to the body of the page
var svg_ut_nn = d3.select("#ut_nearest_neighbours")
.append("svg")
.attr("width", width_ut_nn)
.attr("height", height_ut_nn)
.append("g")
.attr("transform", "translate(" + margin.left + ","  + margin.top +")");

var xAxis_ind_explore_ut = svg_ut_nn
.append("g")
.attr("transform", "translate(0," + (height_ut_nn - margin.bottom) + ")");

var x_scale_areas_ut = d3.scaleBand()
.range([0, width_ut_nn - margin.left - margin.right])
.padding(0.2);

x_scale_areas_ut
.domain(selected_area_ind_ut.map(function (d) {
  return d.Area_name; }))

xAxis_ind_explore_ut
.call(d3.axisBottom(x_scale_areas_ut))

xAxis_ind_explore_ut
.selectAll("text")
.attr("transform", "translate(0)rotate(-45)")
.style("text-anchor", "end")
.style('stroke-opacity', 0);

var y_scale_explore_ind_ut = d3.scaleLinear()
.range([height_ut_nn - margin.bottom, 0]);

y_scale_explore_ind_ut
.domain([0, selected_ut_nn_max]); // update the yaxis based on 'data'

var yAxis_ind_explore_ut = svg_ut_nn
.append("g")
.call(d3.axisLeft(y_scale_explore_ind_ut));

yAxis_ind_explore_ut
.selectAll('text')
.style("stroke-opacity", 0)

// tooltip
var tooltip_ut_nn = d3.select("#ut_nearest_neighbours")
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

// The tooltip function
var showTooltip_ut_nn = function(d, i) {
tooltip_ut_nn
  .html("<h4>" + d.Area_name + '</h4><p>' + 'in ' + selected_ut_meta[0]['Timeperiod'] + ' the ' + selected_ut_meta[0]['Unit'] + ' was ' + d.Label + '</p><p>' +d.Area_name + ' ranks ' + d.Rank_label.toLowerCase() + ' out of the 16 nearest neighbours.</p>')
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible")
  }

var mouseleave_ut_nn = function(d) {
  tooltip_ut_nn.style("visibility", "hidden")
  }

var bars_ut_nn = svg_ut_nn
.selectAll("rect")
.data(selected_area_ind_ut);

bars_ut_nn
.enter()
.append("rect") // Add a new rect for each new element
.merge(bars_ut_nn) // get the already existing elements as well
.attr("x", function (d) {
  return x_scale_areas_ut(d.Area_name);
  })
.attr("y", function (d) {
  return y_scale_explore_ind_ut(d.Value) ;
  })
.attr("width", x_scale_areas_ut.bandwidth())
.attr("height", function (d) {
  return  (height_ut_nn - margin.bottom) - y_scale_explore_ind_ut(d.Value);
  })
.style("fill", function (d) {
  return d.Colour })
.attr("stroke-width", function(d) {
  if (d.Area_name === selected_ut_area_option) {
  return 3
  } else {
  return 0
  }})
.attr("stroke", function(d) {
  if (d.Area_name === selected_ut_area_option) {
  return '#0f4c81'
  } else {
  return '#fffff'
  }})
.on("mousemove", showTooltip_ut_nn)
.on('mouseout', mouseleave_ut_nn);

var lines = svg_ut_nn.selectAll('line.error')
.data(selected_area_ind_ut);

lines
.enter()
.append('line')
.attr('class', 'error')
.merge(lines)
.attr('x1', function(d) { return x_scale_areas_ut(d.Area_name) + x_scale_areas_ut.bandwidth()/2; })
.attr('x2', function(d) { return x_scale_areas_ut(d.Area_name) + x_scale_areas_ut.bandwidth()/2; })
.attr('y1', function(d) { return y_scale_explore_ind_ut(d.Upper_CI); })
.attr('y2', function(d) { return y_scale_explore_ind_ut(d.Lower_CI); })
.attr('id', 'conf_ut_vertical_line');

lines
.enter()
.append('line')
.attr('class', 'error')
.merge(lines)
.attr('x1', function(d) { return x_scale_areas_ut(d.Area_name) + x_scale_areas_ut.bandwidth()*.25; })
.attr('x2', function(d) { return x_scale_areas_ut(d.Area_name) + x_scale_areas_ut.bandwidth()*.75; })
.attr('y1', function(d) { return y_scale_explore_ind_ut(d.Upper_CI); })
.attr('y2', function(d) { return y_scale_explore_ind_ut(d.Upper_CI); })
.attr('id', 'conf_ut_upper_line');

lines
.enter()
.append('line')
.attr('class', 'error')
.merge(lines)
.attr('x1', function(d) { return x_scale_areas_ut(d.Area_name) + x_scale_areas_ut.bandwidth()*.25; })
.attr('x2', function(d) { return x_scale_areas_ut(d.Area_name) + x_scale_areas_ut.bandwidth()*.75; })
.attr('y1', function(d) { return y_scale_explore_ind_ut(d.Lower_CI); })
.attr('y2', function(d) { return y_scale_explore_ind_ut(d.Lower_CI); })
.attr('id', 'conf_ut_lower_line');

lines_eng = svg_ut_nn.selectAll('line.eng')
.data(selected_ut_ind_comp);

lines_eng
.enter()
.append('line')
.attr('class', 'comparator')
.merge(lines_eng)
.attr('x1', function(d) { return 0; })
.attr('x2', function(d) { return width_ut_nn - 300; })
.attr('y1', function(d) { return y_scale_explore_ind_ut(d.Comp_Value); })
.attr('y2', function(d) { return y_scale_explore_ind_ut(d.Comp_Value); })
// .attr('stroke', '#616570')
.attr('stroke-width', 2)
.attr('id', 'comparator_ut_line');

svg_ut_nn
.append("text")
.attr('id', 'nn_ut_y_axis_title')
.attr("text-anchor", "end")
.attr("transform", "rotate(-90)")
.attr("y", - margin.left + 20)
.attr("x", - margin.top - 60)
.data(selected_ut_meta)
.text(function(d) {
  return possible_units(d.Unit)
})
.style('stroke', 'none')
.attr('id', 'nn_ut_y_axis_title');

svg_ut_nn
.append("text")
.attr("text-anchor", "left")
.attr("x", width_ut_nn * .2)
.attr("y", height_ut_nn - 120)
.text(function(d) {
  return 'Areas similar to ' + selected_ut_area_option
})
.style('stroke','none')
.style('font-size', '.9rem')
.attr('id', 'nn_ut_x_axis_title');

svg_ut_nn
.append("text")
.data(selected_ut_meta)
.attr("x", 0)
.attr("y", height_ut_nn - 120) // 100 is where the first dot appears. 20 is the distance between dots
// .attr("transform", "translate(" + margin.left + ",)")
.text(function(d) {
  if (d.Polarity === 'Not applicable') {
    return 'Lowest'
    } else {
    return 'Best'
    }})
.attr('stroke-opacity', 0)
.style('font-weight', 'bold')
.attr('font-size', '.8rem')
.attr('id', 'nn_ut_x_rank_min');

svg_ut_nn
.append("text")
.data(selected_ut_meta)
.attr("x", width_ut_nn - 350)
.attr("y", height_ut_nn - 120)
.text(function(d) {
  if (d.Polarity === 'Not applicable') {
    return 'Highest'
    } else {
    return 'Worst'
    }})
.attr('stroke-opacity', 0)
.style('font-weight', 'bold')
.attr('font-size', '.8rem')
.attr('id', 'nn_ut_x_rank_max');

svg_ut_nn
.append("text")
.data(selected_ut_meta)
.attr("x", 0)
.attr("y", height_ut_nn - 90)
.text('The dashed line represents the value for England.')
.attr('stroke-opacity', 0)
.attr('font-size', '.8rem')
.attr('id', 'comparator_explain_text');

d3.select("#Area_ut_nn_1")
    .data(selected_ut_meta)
    .text(function(d) {
        return d.Description});

svg_ut_nn
.data(selected_area_ut_nn_explore)
.append("text")
.attr("x", width_ut_nn - 270)
.attr("y", 20)
.text(function(d){
  return d.Area_name
})
.attr('stroke-opacity', 0)
.attr('font-size', '1.4rem')
.style('font-weight', 'bold')
.attr('fill', '#0f4c81')
.attr('id', 'selected_area_text');

svg_ut_nn
.data(selected_area_ut_nn_explore)
.append("text")
.attr("x", width_ut_nn - 250)
.attr("y", 60)
.text(function(d){
  return d.Label + ' in ' + selected_ut_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

svg_ut_nn
.data(selected_area_ut_nn_explore)
.append("text")
.attr("x", width_ut_nn - 250)
.attr("y", 100)
.text(function(d){
  return d.Rank_label + ' compared to '
})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_rank_text_1');

svg_ut_nn
.append("text")
.attr("x", width_ut_nn - 250)
.attr("y", 120)
.text('similar areas')
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_rank_text_2');

svg_ut_nn
.data(selected_area_ut_nn_explore)
.append("text")
.attr("x", width_ut_nn - 250)
.attr("y", 160)
.text(function(d) {
  if (selected_ut_meta[0]['Polarity'] === 'Not applicable') {
    return
    } else {
    return d.Significance
    }})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_significance_text_1');

svg_ut_nn
.data(selected_area_ut_nn_explore)
.append("text")
.attr("x", width_ut_nn - 250)
.attr("y", 180)
.text(function(d) {
  if (selected_ut_meta[0]['Polarity'] === 'Not applicable') {
    return
    } else {
    return 'compared to England'
    }})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selecred_significance_text_2');

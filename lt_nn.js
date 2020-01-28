// margins
var margin = {
    top: 40,
    right: 250,
    bottom: 200,
    left: 50
};

var possible_units = d3.scaleOrdinal()
    .domain(["proportion", "rate", "average score", "life expectancy in years", 'median'])
    .range(['Proportion', 'Rate', 'Average score', 'Years', 'Median hourly wage (£s)']);

var width_lt_nn = document.getElementById("lt_nearest_neighbours").offsetWidth;
var height_lt_nn = 425;

// data
var request = new XMLHttpRequest();
    request.open("GET", "./lt_data_meta_extract.json", false);
    request.send(null);
var json_lt_meta = JSON.parse(request.responseText); // parse the fetched json data into a variable

// data
var request = new XMLHttpRequest();
    request.open("GET", "./lt_data_neighbours.json", false);
    request.send(null);
var json_lt_nn = JSON.parse(request.responseText); // parse the fetched json data into a variable

// data
var request = new XMLHttpRequest();
    request.open("GET", "./Comp_data_lt.json", false);
    request.send(null);
var json_comp_lt = JSON.parse(request.responseText); // parse the fetched json data into a variable

// List of areas in the dataset
var areas_lower_tier_explore = d3.map(json_lt_nn, function (d) {
    return (d.Area_x)
    })
    .keys()

// List of indicators in the dataset
var indicators_lower_tier_explore = d3.map(json_lt_nn, function (d) {
  return (d.Name)
  })
  .keys()

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_area_lt_explore_button")
  .selectAll('myOptions')
  .data(areas_lower_tier_explore)
  .enter()
  .append('option')
  .text(function (d) {
        return d; }) // text to appear in the menu - this does not have to be as it is in the data (you can concatenate other values).
  .attr("value", function (d) {
        return d; }) // corresponding value returned by the button

var selected_lt_nn_area_option = d3.select('#select_area_lt_explore_button').property("value")

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_indicator_lt_explore_button")
  .selectAll('myOptions')
  .data(indicators_lower_tier_explore)
  .enter()
  .append('option')
  .text(function (d) {
        return d; }) // text to appear in the menu - this does not have to be as it is in the data (you can concatenate other values).
  .attr("value", function (d) {
        return d; }) // corresponding value returned by the button

var selected_lt_indicator_option = d3.select('#select_indicator_lt_explore_button').property("value")

selected_area_ind_lt = json_lt_nn.filter(function (d) { // gets a subset of the json data
    return d.Name === selected_lt_indicator_option &
        d.Area_x === selected_lt_nn_area_option })
    .sort(function (a, b) { // sorts it according to the number of deaths (descending order)
        return d3.ascending(a.Rank, b.Rank);
    })

selected_lt_nn_max = d3.map(selected_area_ind_lt, function (d) {
  return (d.Max_value)
  })
  .keys()

selected_area_lt_nn_explore = selected_area_ind_lt.filter(function (d) { // gets a subset of the json data
    return d.Area_name === selected_lt_nn_area_option })

selected_lt_meta = json_lt_meta.filter(function (d) { // gets a subset of the json data
    return d.Name === selected_lt_indicator_option })

selected_lt_ind_comp = json_comp_lt.filter(function (d) { // gets a subset of the json data
        return d.Name === selected_lt_indicator_option })

// append the svg object to the body of the page
var svg_lt_nn = d3.select("#lt_nearest_neighbours")
.append("svg")
.attr("width", width_lt_nn)
.attr("height", height_lt_nn)
.append("g")
.attr("transform", "translate(" + margin.left + ","  + margin.top +")");

var xAxis_ind_explore_lt = svg_lt_nn
.append("g")
.attr("transform", "translate(0," + (height_lt_nn - margin.bottom) + ")");

var x_scale_areas_lt = d3.scaleBand()
.range([0, width_lt_nn - margin.left - margin.right])
.padding(0.2);

x_scale_areas_lt
.domain(selected_area_ind_lt.map(function (d) {
  return d.Area_name; }))

xAxis_ind_explore_lt
.call(d3.axisBottom(x_scale_areas_lt))

xAxis_ind_explore_lt
.selectAll("text")
.attr("transform", "translate(0)rotate(-45)")
.style("text-anchor", "end")
.style('stroke-opacity', 0);

var y_scale_explore_ind_lt = d3.scaleLinear()
.range([height_lt_nn - margin.bottom, 0]);

y_scale_explore_ind_lt
.domain([0, selected_lt_nn_max]); // update the yaxis based on 'data'

var yAxis_ind_explore_lt = svg_lt_nn
.append("g")
.call(d3.axisLeft(y_scale_explore_ind_lt));

yAxis_ind_explore_lt
.selectAll('text')
.style("stroke-opacity", 0)

// tooltip
var tooltip_lt_nn = d3.select("#lt_nearest_neighbours")
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
var showTooltip_lt_nn = function(d, i) {
tooltip_lt_nn
  .html("<h4>" + d.Area_name + '</h4><p>' + 'in ' + selected_lt_meta[0]['Timeperiod'] + ' the ' + selected_lt_meta[0]['Unit'] + ' was ' + d.Label + '</p><p>' +d.Area_name + ' ranks ' + d.Rank_label.toLowerCase() + ' out of the 16 nearest neighbours.</p>')
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible")
  }

var mouseleave_lt_nn = function(d) {
  tooltip_lt_nn.style("visibility", "hidden")
  }

var bars_lt_nn = svg_lt_nn
.selectAll("rect")
.data(selected_area_ind_lt);

bars_lt_nn
.enter()
.append("rect") // Add a new rect for each new element
// .merge(bars_lt_nn) // get the already existing elements as well
.attr("x", function (d) {
  return x_scale_areas_lt(d.Area_name);
  })
.attr("y", function (d) {
  return y_scale_explore_ind_lt(d.Value) ;
  })
.attr("width", x_scale_areas_lt.bandwidth())
.attr("height", function (d) {
  return  (height_lt_nn - margin.bottom) - y_scale_explore_ind_lt(d.Value);
  })
.style("fill", function (d) {
  return d.Colour })
.attr("stroke-width", function(d) {
  if (d.Area_name === selected_lt_nn_area_option) {
  return 3
  } else {
  return 0
  }})
.attr("stroke", function(d) {
  if (d.Area_name === selected_lt_nn_area_option) {
  return '#0f4c81'
  } else {
  return '#fffff'
  }})
.on("mousemove", showTooltip_lt_nn)
.on('mouseout', mouseleave_lt_nn);

// var lines_vt = svg_lt_nn
// .selectAll('line.error_vt')
// .data(selected_area_ind_lt);
//
// lines_vt
// .enter()
// .append('line')
// .attr('class', 'error_vt')
// .merge(lines_vt)
// .attr('x1', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()/2; })
// .attr('x2', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()/2; })
// .attr('y1', function(d) { return y_scale_explore_ind_lt(d.Upper_CI); })
// .attr('y2', function(d) { return y_scale_explore_ind_lt(d.Lower_CI); })
// .attr('id', 'conf_lt_vertical_line');
//
// var lines_uci = svg_lt_nn
// .selectAll('line.error_uci')
// .data(selected_area_ind_lt);
//
// lines_uci
// .enter()
// .append('line')
// .attr('class', 'error_uci')
// .merge(lines_uci)
// .attr('x1', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.25; })
// .attr('x2', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.75; })
// .attr('y1', function(d) { return y_scale_explore_ind_lt(d.Upper_CI); })
// .attr('y2', function(d) { return y_scale_explore_ind_lt(d.Upper_CI); })
// .attr('id', 'conf_lt__line');
//
// var lines_lci = svg_lt_nn
// .selectAll('line.error_lci')
// .data(selected_area_ind_lt);
//
// lines_lci
// .enter()
// .append('line')
// .attr('class', 'error_lci')
// .merge(lines_lci)
// .attr('x1', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.25; })
// .attr('x2', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.75; })
// .attr('y1', function(d) { return y_scale_explore_ind_lt(d.Lower_CI); })
// .attr('y2', function(d) { return y_scale_explore_ind_lt(d.Lower_CI); })
// .attr('id', 'conf_lt_lower_line');

lines_eng = svg_lt_nn.selectAll('line.eng')
.data(selected_lt_ind_comp);

lines_eng
.enter()
.append('line')
.attr('class', 'comparator')
.merge(lines_eng)
.attr('x1', function(d) { return 0; })
.attr('x2', function(d) { return width_lt_nn - 300; })
.attr('y1', function(d) { return y_scale_explore_ind_lt(d.Comp_Value); })
.attr('y2', function(d) { return y_scale_explore_ind_lt(d.Comp_Value); })
// .attr('stroke', '#616570')
.attr('stroke-width', 2)
.attr('id', 'comparator_lt_line');

svg_lt_nn
.append("text")
.attr('id', 'nn_lt_y_axis_title')
.attr("text-anchor", "end")
.attr("transform", "rotate(-90)")
.attr("y", - margin.left + 20)
.attr("x", - margin.top - 60)
.data(selected_lt_meta)
.text(function(d) {
  return possible_units(d.Unit)
})
.style('stroke', 'none')
.attr('id', 'nn_lt_y_axis_title');

svg_lt_nn
.append("text")
.attr("text-anchor", "left")
.attr("x", width_lt_nn * .2)
.attr("y", height_lt_nn - 120)
.text(function(d) {
  return 'Areas similar to ' + selected_lt_nn_area_option
})
.style('stroke','none')
.style('font-size', '.9rem')
.attr('id', 'nn_lt_x_axis_title');

svg_lt_nn
.append("text")
.data(selected_lt_meta)
.attr("x", 0)
.attr("y", height_lt_nn - 120) // 100 is where the first dot appears. 20 is the distance between dots
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
.attr('id', 'nn_lt_x_rank_min');

svg_lt_nn
.append("text")
.data(selected_lt_meta)
.attr("x", width_lt_nn - 350)
.attr("y", height_lt_nn - 120)
.text(function(d) {
  if (d.Polarity === 'Not applicable') {
    return 'Highest'
    } else {
    return 'Worst'
    }})
.attr('stroke-opacity', 0)
.style('font-weight', 'bold')
.attr('font-size', '.8rem')
.attr('id', 'nn_lt_x_rank_max');

svg_lt_nn
.append("text")
.data(selected_lt_meta)
.attr("x", 0)
.attr("y", height_lt_nn - 90)
.text('The dashed line represents the value for England.')
.attr('stroke-opacity', 0)
.attr('font-size', '.8rem')
.attr('id', 'comparator_explain_text');

d3.select("#Area_lt_nn_1")
    .data(selected_lt_meta)
    .text(function(d) {
        return d.Description});

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 25)
.text(function(d){
  return d.Area_name
})
.attr('stroke-opacity', 0)
.attr('font-size', '1.4rem')
.style('font-weight', 'bold')
.attr('fill', '#0f4c81')
.attr('id', 'selected_area_text');

// This is the if not
if(selected_lt_meta[0]['ID'] === '92196'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 1,000 live births'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};

// This is the if not
if(selected_lt_meta[0]['Unit'] !== 'rate'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d.Label + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');
};


// if(selected_lt_meta[0]['Unit'] === 'rate' ){
// console.log('Whatcha gonna do when they come for you')
// };
//
// svg_lt_nn
// .data(selected_area_lt_nn_explore)
// .append("text")
// .attr("x", width_lt_nn - 270)
// .attr("y", 60)
// .text(function(d){
//   return d.Label + ' in ' + selected_lt_meta[0]['Timeperiod']
// })
// .attr('stroke-opacity', 0)
// .attr('font-size', '.9rem')
// .style('font-weight', 'bold')
// .attr('id', 'selected_label_text');

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 100)
.text(function(d){
  return d.Rank_label + ' compared to '
})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_rank_text_1');

svg_lt_nn
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 120)
.text('similar areas')
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_rank_text_2');

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 160)
.text(function(d) {
  if (selected_lt_meta[0]['Polarity'] === 'Not applicable') {
    return
    } else {
    return d.Significance
    }})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_significance_text_1');

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 180)
.text(function(d) {
  if (selected_lt_meta[0]['Polarity'] === 'Not applicable') {
    return
    } else {
    return 'compared to England'
    }})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_significance_text_2');

function update_lt_nn(selected_area_lt_nn_explore) {

var selected_lt_nn_area_option = d3.select('#select_area_lt_explore_button').property("value")
var selected_lt_indicator_option = d3.select('#select_indicator_lt_explore_button').property("value")

selected_area_ind_lt = json_lt_nn.filter(function (d) { // gets a subset of the json data
    return d.Name === selected_lt_indicator_option &
          d.Area_x === selected_lt_nn_area_option })
      .sort(function (a, b) { // sorts it according to the number of deaths (descending order)
          return d3.ascending(a.Rank, b.Rank);
      })

selected_lt_nn_max = d3.map(selected_area_ind_lt, function (d) {
    return (d.Max_value)
    })
    .keys()

selected_area_lt_nn_explore = selected_area_ind_lt.filter(function (d) { // gets a subset of the json data
      return d.Area_name === selected_lt_nn_area_option })

selected_lt_meta = json_lt_meta.filter(function (d) { // gets a subset of the json data
      return d.Name === selected_lt_indicator_option })

selected_lt_ind_comp = json_comp_lt.filter(function (d) { // gets a subset of the json data
          return d.Name === selected_lt_indicator_option })

svg_lt_nn
.selectAll("#Area_lt_nn_1")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

d3.select("#Area_lt_nn_1")
.data(selected_lt_meta)
.text(function(d) {
      return d.Description});

svg_lt_nn
.selectAll("#selected_area_text")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

svg_lt_nn
.selectAll("#selected_label_text")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

svg_lt_nn
.selectAll("#selected_label_text_rate")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

svg_lt_nn
.selectAll("#selected_rank_text_1")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

svg_lt_nn
.selectAll("#selected_rank_text_2")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

svg_lt_nn
.selectAll("#selected_significance_text_1")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

svg_lt_nn
.selectAll("#selected_significance_text_2")
.transition()
.duration(750)
.style('opacity' ,0 )
.remove();

svg_lt_nn
.selectAll("#nn_lt_x_rank_max")
.transition()
.duration(750)
.style('opacity', 0)
.remove();

svg_lt_nn
.selectAll("#nn_lt_x_rank_min")
.transition()
.duration(750)
.style('opacity', 0)
.remove();

svg_lt_nn
.selectAll("#nn_lt_x_axis_title")
.transition()
.duration(750)
.style('opacity', 0)
.remove();

svg_lt_nn
.selectAll("#nn_lt_y_axis_title")
.transition()
.duration(750)
.style('opacity', 0)
.remove();

// svg_lt_nn
// .selectAll(".error")
// .transition()
// .duration(750)
// .style('opacity', 0)
// .remove();

svg_lt_nn
.selectAll("#no_data_warning")
.transition()
.duration(750)
.style('opacity', 0)
.remove();

// svg_lt_nn
// .selectAll("#conf_lt_vertical_line")
// .transition()
// .duration(750)
// .style('opacity', 0)
// .remove();
//
// svg_lt_nn
// .selectAll("#conf_lt__line")
// .transition()
// .duration(750)
// .style('opacity', 0)
// .remove();
//
// svg_lt_nn
// .selectAll("#conf_lt_lower_line")
// .transition()
// .duration(750)
// .style('opacity', 0)
// .remove();

svg_lt_nn
.selectAll("#comparator_lt_line")
.transition()
.duration(750)
.style('opacity', 0)
.remove();

var x_scale_areas_lt = d3.scaleBand()
.range([0, width_lt_nn - margin.left - margin.right])
.padding(0.2);

x_scale_areas_lt
.domain(selected_area_ind_lt.map(function (d) {
  return d.Area_name; }))

xAxis_ind_explore_lt
.transition()
.duration(1000)
.call(d3.axisBottom(x_scale_areas_lt))

xAxis_ind_explore_lt
.selectAll("text")
.attr("transform", "translate(0)rotate(-45)")
.style("text-anchor", "end")
.style('stroke-opacity', 0);

y_scale_explore_ind_lt
.domain([0, selected_lt_nn_max]); // update the yaxis based on 'data'

yAxis_ind_explore_lt
.transition()
.duration(1000)
.call(d3.axisLeft(y_scale_explore_ind_lt));

yAxis_ind_explore_lt
.selectAll('text')
.style("stroke-opacity", 0)

var bars_lt_nn = svg_lt_nn
.selectAll("rect")
.data(selected_area_ind_lt)

bars_lt_nn
.enter()
.append("rect")
.merge(bars_lt_nn)
.transition()
.duration(1000)
.attr("x", function (d) {
  return x_scale_areas_lt(d.Area_name);
  })
.attr("y", function (d) {
  return y_scale_explore_ind_lt(d.Value) ;
  })
.attr("width", x_scale_areas_lt.bandwidth())
.attr("height", function (d) {
  return  (height_lt_nn - margin.bottom) - y_scale_explore_ind_lt(d.Value);
  })
.style("fill", function (d) {
  return d.Colour })
.attr("stroke-width", function(d) {
  if (d.Area_name === selected_lt_nn_area_option) {
  return 3
  } else {
  return 0
  }})
.attr("stroke", function(d) {
  if (d.Area_name === selected_lt_nn_area_option) {
  return '#0f4c81'
  } else {
  return '#fffff'
}})

bars_lt_nn
.exit()
.remove()

// recreate tooltip
var tooltip_lt_nn = d3.select("#lt_nearest_neighbours")
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
var showTooltip_lt_nn = function(d, i) {
tooltip_lt_nn
.html("<h4>" + d.Area_name + '</h4><p>' + 'in ' + selected_lt_meta[0]['Timeperiod'] + ' the ' + selected_lt_meta[0]['Unit'] + ' was ' + d.Label + '</p><p>' +d.Area_name + ' ranks ' + d.Rank_label.toLowerCase() + ' out of the 16 nearest neighbours.</p>')
.style("opacity", 1)
.style("top", (event.pageY - 10) + "px")
.style("left", (event.pageX + 10) + "px")
.style("visibility", "visible")
    }

var mouseleave_lt_nn = function(d) {
    tooltip_lt_nn.style("visibility", "hidden")
    }

bars_lt_nn
.on("mousemove", showTooltip_lt_nn)
.on('mouseout', mouseleave_lt_nn);

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 25)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d.Area_name
})
.attr('stroke-opacity', 0)
.attr('font-size', '1.4rem')
.style('font-weight', 'bold')
.attr('fill', '#0f4c81')
.attr('id', 'selected_area_text');

// This is the if infant mortality is true
if(selected_lt_meta[0]['ID'] === '92196'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 1,000 live births'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};


// This is the if admissions for self-harm true
if(selected_lt_meta[0]['ID'] === '21001'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 100,000 population'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};

// This is the if teenage pregnancies true
if(selected_lt_meta[0]['ID'] === '20401'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 1,000 pregnancies'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};

// This is the if admissions for self-harm true
if(selected_lt_meta[0]['ID'] === '11001'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 100,000 population'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};


// This is the if violent crime true
if(selected_lt_meta[0]['ID'] === '11202'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 1,000 population'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};

// This is the if Admission episodes for alcohol-related conditions true
if(selected_lt_meta[0]['ID'] === '91414'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 100,000 population'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};

// This is the if cancer mortality true
if(selected_lt_meta[0]['ID'] === '40501'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 100,000 population'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};

// This is the if cvd mortality true
if(selected_lt_meta[0]['ID'] === '40401'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 100,000 population'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};

// This is the if hip fractures true
if(selected_lt_meta[0]['ID'] === '41401'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 45)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d3.format('.0f')(d.Value) + ' per 100,000 population'
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');

//
svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return '95% CI ' + d3.format('.0f')(d.Lower_CI) + '-' + d3.format('.0f')(d.Upper_CI) + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text_rate');
};


// This is the if not
if(selected_lt_meta[0]['Unit'] !== 'rate'){

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 60)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d.Label + ' in ' + selected_lt_meta[0]['Timeperiod']
})
.attr('stroke-opacity', 0)
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_label_text');
};

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 100)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d){
  return d.Rank_label + ' compared to '
})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_rank_text_1');

svg_lt_nn
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 120)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text('similar areas')
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_rank_text_2');

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 160)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d) {
  if (selected_lt_meta[0]['Polarity'] === 'Not applicable') {
    return
    } else {
    return d.Significance
    }})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_significance_text_1');

if(selected_area_ind_lt.filter(function (d) { // gets a subset of the json data
    return d.data_available === 'No data' }).length >0) {
svg_lt_nn
.append("text")
.attr("text-anchor", "left")
.attr('class', 'remember_overlap_text')
.style('stroke','none')
.attr('id', 'no_data_warning')
.attr('font-size', '.8rem')
.style('fill', 'red')
.attr("x", 0)
.attr("y", height_lt_nn - 80)
.text(function(d) { return 'There are no data for some areas.'});
}

svg_lt_nn
.data(selected_area_lt_nn_explore)
.append("text")
.attr("x", width_lt_nn - 270)
.attr("y", 180)
.attr('opacity',0)
.transition()
.duration(1500)
.attr('opacity',1)
.text(function(d) {
  if (selected_lt_meta[0]['Polarity'] === 'Not applicable') {
    return
    } else {
    return 'compared to England'
    }})
.style('stroke', 'none')
.attr('font-size', '.9rem')
.style('font-weight', 'bold')
.attr('id', 'selected_significance_text_2');

svg_lt_nn
.append("text")
.data(selected_lt_meta)
.attr("x", 0)
.attr("y", height_lt_nn - 120)
.text(function(d) {
  if (d.Polarity === 'Not applicable') {
    return 'Lowest'
    } else {
    return 'Best'
    }})
.attr('stroke-opacity', 0)
.style('font-weight', 'bold')
.attr('font-size', '.8rem')
.attr('id', 'nn_lt_x_rank_min');

svg_lt_nn
.append("text")
.data(selected_lt_meta)
.attr("x", width_lt_nn - 350)
.attr("y", height_lt_nn - 120)
.text(function(d) {
  if (d.Polarity === 'Not applicable') {
    return 'Highest'
    } else {
    return 'Worst'
    }})
.attr('stroke-opacity', 0)
.style('font-weight', 'bold')
.attr('font-size', '.8rem')
.attr('id', 'nn_lt_x_rank_max');
//
// var lines_vt = svg_lt_nn
// .selectAll('line.error_vt')
// .data(selected_area_ind_lt);
//
// lines_vt
// .enter()
// .append('line')
// .attr('class', 'error_vt')
// .merge(lines_vt)
// .attr('x1', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()/2; })
// .attr('x2', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()/2; })
// .attr('y1', function(d) { return y_scale_explore_ind_lt(d.Upper_CI); })
// .attr('y2', function(d) { return y_scale_explore_ind_lt(d.Lower_CI); })
// .attr('id', 'conf_lt_vertical_line');
//
// var lines_uci = svg_lt_nn
// .selectAll('line.error')
// .data(selected_area_ind_lt);
//
// lines_uci
// .enter()
// .append('line')
// .attr('class', 'error_uci')
// .merge(lines_uci)
// .attr('x1', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.25; })
// .attr('x2', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.75; })
// .attr('y1', function(d) { return y_scale_explore_ind_lt(d.Upper_CI); })
// .attr('y2', function(d) { return y_scale_explore_ind_lt(d.Upper_CI); })
// .attr('id', 'conf_lt__line');
//
// var lines_lci = svg_lt_nn
// .selectAll('line.error_uci')
// .data(selected_area_ind_lt);
//
// lines_lci
// .enter()
// .append('line')
// .attr('class', 'error')
// .merge(lines_lci)
// .attr('x1', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.25; })
// .attr('x2', function(d) { return x_scale_areas_lt(d.Area_name) + x_scale_areas_lt.bandwidth()*.75; })
// .attr('y1', function(d) { return y_scale_explore_ind_lt(d.Lower_CI); })
// .attr('y2', function(d) { return y_scale_explore_ind_lt(d.Lower_CI); })
// .attr('id', 'conf_lt_lower_line');

lines_eng = svg_lt_nn
.selectAll('line.eng')
.data(selected_lt_ind_comp);

lines_eng
.enter()
.append('line')
.attr('class', 'comparator')
.merge(lines_eng)
.attr('x1', function(d) { return 0; })
.attr('x2', function(d) { return width_lt_nn - 300; })
.attr('y1', function(d) { return y_scale_explore_ind_lt(d.Comp_Value); })
.attr('y2', function(d) { return y_scale_explore_ind_lt(d.Comp_Value); })
.attr('stroke-width', 2)
.attr('id', 'comparator_lt_line')
.style('opacity',0)
.transition()
.duration(750)
.style('opacity',1);

svg_lt_nn
.append("text")
.attr('id', 'nn_lt_y_axis_title')
.attr("text-anchor", "end")
.attr("transform", "rotate(-90)")
.attr("y", - margin.left + 20)
.attr("x", - margin.top - 60)
.data(selected_lt_meta)
.text(function(d) {
  return possible_units(d.Unit)
})
.style('stroke', 'none')
.attr('id', 'nn_lt_y_axis_title')
.attr('opacity',0)
.transition()
.duration(750)
.attr('opacity',1);

svg_lt_nn
.append("text")
.attr("text-anchor", "left")
.attr("x", width_lt_nn * .2)
.attr("y", height_lt_nn - 120)
.text(function(d) {
  return 'Areas similar to ' + selected_lt_nn_area_option
})
.style('stroke','none')
.style('font-size', '.9rem')
.attr('id', 'nn_lt_x_axis_title')
.attr('opacity',0)
.transition()
.duration(750)
.attr('opacity',1);
}

d3.select("#select_area_lt_explore_button").on("change", function(d) {
  var selected_lt_nn_area_option = d3.select('#select_area_lt_explore_button').property("value")
  var selected_lt_indicator_option = d3.select('#select_indicator_lt_explore_button').property("value")
  update_lt_nn(selected_area_lt_nn_explore)
  })

d3.select("#select_indicator_lt_explore_button").on("change", function(d) {
  var selected_lt_nn_area_option = d3.select('#select_area_lt_explore_button').property("value")
  var selected_lt_indicator_option = d3.select('#select_indicator_lt_explore_button').property("value")
  update_lt_nn(selected_lt_indicator_option)
    })

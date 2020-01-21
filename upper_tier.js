var width = (window.innerWidth * .9) - 20;
var height = width * .6;

// Get the modal
var modal = document.getElementById("myModal");

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// When the user clicks on <span> (x), close the modal
span.onclick = function() {
 modal.style.display = "none";
}

// When the user clicks anywhere outside of the modal, close it
window.onclick = function(event) {
  if (event.target == modal) {
    modal.style.display = "none";
  }
}

var ut_stage_circles = [{'line_1':'Pre-birth', 'line_2': 'to early', 'line_3': 'years', "x":.05, "y":.1},{'line_1':'School', 'line_2': 'years', "x":.67, "y":.1},{"line_1":'Early', 'line_2': 'working', 'line_3':'life', "x":.32, "y":.35},{"line_1":'Mid working', 'line_2':'life to', 'line_3':'retirement', "x":.61, "y":.6},{"line_1":'Retirement', 'line_2':'to older age', "x":.635, "y":.85}]

var ut_stage_arrows = [{"x":.11, "y":.1, "img":'./images/right_white.svg'},{"x":.37, "y":.1, "img":'./images/right_white.svg'},{"x":.78, "y":.1, "img":'./images/right_white.svg'},{"x":.96, "y":.1, "img":'./images/down_white.svg'},{"x":.96, "y":.35, "img":'./images/left_white.svg'},{"x":.62, "y":.35, "img":'./images/left_white.svg'},{"x":.04, "y":.35, "img":'./images/down_white.svg'},{"x":.04, "y":.6, "img":'./images/right_white.svg'},{"x":.42, "y":.6, "img":'./images/right_white.svg'},{"x":.8, "y":.6, "img":'./images/right_white.svg'},{"x":.96, "y":.6, "img":'./images/down_white.svg'},{"x":.96, "y":.85, "img":'./images/left_white.svg'},{"x":.275, "y":.85, "img":'./images/left_white.svg'}]

var significance_key = d3.scaleOrdinal()
.domain(['Not applicable', 'Significantly upper','Similar','Significantly higher'])
.range(['It is not appropriate to compare this value with England statistically', 'This is significantly upper compared to England', 'This is not statistically different compared to England', 'This is significantly higher compared to England'])

var polarity_key = d3.scaleOrdinal()
.domain(['Not applicable', 'upper is better','Similar','Higher is better'])
.range(['', 'For this indicator, a upper value is better.', 'For this indicator, a higher value is better.'])

// We want the infographic to redraw at the scale of the users screen. We have identified (through trial and error) the best position within the width
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

if(width < 1300){
var scaled_icon_size = 30
  }

if(width > 1300){
  var scaled_icon_size = 50
  }

var circles_for_key = [{'line_1':'Significantly', 'line_2': 'better', 'colour': '#3ECC26', 'x': 50, 'y':31},{'line_1':'Similar', 'colour': '#E7AF27', "x": 125, "y": 31},{"line_1":'Significantly', 'line_2': 'worse','colour': '#CC2629',  "x": 200, "y":31},{"line_1":'Not', 'line_2':'applicable', 'colour': '#8E8E8E',  "x": 275, "y":31}]

var svg_key = d3.select('#circle_key')
.append('svg')
.attr('width', 325)
.attr('height', 62)
.style('background-color', '#fcfaf7')
.append('g');

svg_key
.selectAll()
.data(circles_for_key)
.enter()
.append('circle')
.attr("cx", function(d){ return d.x })
.attr("cy", function(d){ return d.y })
// .attr("r",  function(d){ return circle_size(width)})
.attr('r', 30)
.style("fill", function(d){ return d.colour })
.style('stroke',function(d){ return d.colour });

svg_key
.selectAll()
.data(circles_for_key)
.enter()
.append('text')
.attr("dx", function(d){ return d.x })
.attr("dy", function(d){ return d.y - 5 })
.text(function(d){ return d.line_1})
.style("fill", "white")
.style('stroke', 'white')
.style('stroke-opacity',0)
.attr('text-anchor','middle')
.attr("font-size", '.6rem');

svg_key
.selectAll()
.data(circles_for_key)
.enter()
.append('text')
.attr("dx", function(d){ return d.x })
.attr("dy", function(d){ return d.y + 10 })
.text(function(d){ return d.line_2})
.style("fill", "white")
.style('stroke', 'white')
.style('stroke-opacity',0)
.attr('text-anchor','middle')
.attr("font-size", '.6rem');

// Set up tooltip for circles
var tooltip_ut = d3.select("#wsx_upper_tier_walkthrough")
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

var showTooltip_ut = function(d, i) {

tooltip_ut
  .html("<h4>" + d.Name + '</h4><p class = "tt_text">In ' + d.Area_name + ' in ' + d.Timeperiod + ' the ' + d.Unit + ' was <font color = "#1e4b7a" size = "3"><b>' + d.Label + '</b></font>. </p><p class = "tt_text">' + significance_key(d.Significance) + '. ' + polarity_key(d.Polarity))
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible")
  }

var mouseleave_ut = function(d) {
  tooltip_ut.style("visibility", "hidden")
  }

// Modal
var choose_an_indicator = function(d, i) {
    selected_indicator = d['ID']

d3.select("#indicator_chosen_title")
.selectAll('text')
.remove()

d3.select("#indicator_chosen_title")
  .append('text')
  .data(selected_ut_area_df)
  .text(d.Name)
  .attr('id', 'ind_text_1');

d3.select("#indicator_description")
 .selectAll('text')
 .remove()

d3.select("#indicator_description")
  .append('text')
  .text(d.Description + ' ' + polarity_key(d.Polarity))
  .attr('id', 'ind_text_2');

d3.select("#indicator_label")
  .selectAll('text')
  .remove()

d3.select("#indicator_label")
  .append('text')
  .html('In ' + d.Area_name + ' in ' + d.Timeperiod + ' the ' + d.Unit + ' was <font color = "#1e4b7a" size = "3"><b>' + d.Label + '</b></font>. ' + significance_key(d.Significance) + '.</p>')
  .attr('id', 'ind_text_3');

d3.select("#indicator_further_info")
  .selectAll('text')
  .remove()

d3.select("#indicator_further_info")
  .append('text')
  .text('Information about the source of data, and quick caveats or "You should know..." points')
  .attr('id', 'ind_text_5');

d3.select("#related_indicator")
  .selectAll('text')
  .remove()

d3.select("#related_indicator")
  .append('text')
  .text('Here could be a list of relevant indicators to look at, perhaps links to profiles on fingertips or to other sections of our JSNA site.')
  .attr('id', 'ind_text_6');

modal.style.display = "block";
}

// data frame
var request = new XMLHttpRequest();
  request.open("GET", "./ut_data_extract_compare_england.json", false);
  request.send(null);

var json_ut = JSON.parse(request.responseText); // parse the fetched json data into a variable

// List of areas in the dataset
var areas_upper_tier = d3.map(json_ut, function (d) {
    return (d.Area_name)
    })
    .keys()

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_area_ut_button")
  .selectAll('myOptions')
  .data(areas_upper_tier)
  .enter()
  .append('option')
  .text(function (d) {
        return d; }) // text to appear in the menu - this does not have to be as it is in the data (you can concatenate other values).
  .attr("value", function (d) {
        return d; }) // corresponding value returned by the button

var selected_ut_area_option = d3.select('#select_area_ut_button').property("value")

// append the svg object to the body of the page
var svg_ut_walkthrough = d3.select("#wsx_upper_tier_walkthrough")
.append("svg")
.attr("width", width)
.attr("height", height)
.attr('class','upper_tier_svg')
.append("g");

// Walkthrough dotted lines
svg_ut_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.04)})
  .attr('y1', function(d){ return y_pos(0.1)})
  .attr('x2', function(d){ return x_pos(0.96)})
  .attr('y2', function(d){ return y_pos(0.1)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.96)})
  .attr('y1', function(d){ return y_pos(0.1)})
  .attr('x2', function(d){ return x_pos(0.96)})
  .attr('y2', function(d){ return y_pos(0.35)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.04)})
  .attr('y1', function(d){ return y_pos(0.35)})
  .attr('x2', function(d){ return x_pos(0.96)})
  .attr('y2', function(d){ return y_pos(0.35)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.04)})
  .attr('y1', function(d){ return y_pos(0.35)})
  .attr('x2', function(d){ return x_pos(0.04)})
  .attr('y2', function(d){ return y_pos(0.6)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.04)})
  .attr('y1', function(d){ return y_pos(0.6)})
  .attr('x2', function(d){ return x_pos(0.96)})
  .attr('y2', function(d){ return y_pos(0.6)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.96)})
  .attr('y1', function(d){ return y_pos(0.6)})
  .attr('x2', function(d){ return x_pos(0.96)})
  .attr('y2', function(d){ return y_pos(0.85)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
  .append("line")
  .attr('x1', function(d){ return x_pos(0.04)})
  .attr('y1', function(d){ return y_pos(0.85)})
  .attr('x2', function(d){ return x_pos(0.96)})
  .attr('y2', function(d){ return y_pos(0.85)})
  .attr('stroke', 'black')
  .style('stroke-dasharray', ('2,4'));

// Big circles for each stage of life
svg_ut_walkthrough
  .selectAll()
  .data(ut_stage_circles)
  .enter()
  .append('circle')
  .attr("cx", function(d){ return x_pos(d.x) })
  .attr("cy", function(d){ return y_pos(d.y) })
  .attr("r",  function(d){ return stage_size(width)})
  .style("fill", "black")
  .attr('id', 'ut_stage_labels');

svg_ut_walkthrough
  .selectAll()
  .data(ut_stage_circles)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y) - 5 })
  .text(function(d){ return d.line_1})
  .style("fill", "white")
  .style('stroke','white')
  .attr('text-anchor','middle')
  // .style('font-size', '.8rem')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".8rem"; }
    else {
    return '.9rem';}
  })
  .attr('id', 'ut_stage_line_1');

svg_ut_walkthrough
  .selectAll()
  .data(ut_stage_circles)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y) + 10 })
  .text(function(d){ return d.line_2})
  .style("fill", "white")
  .style('stroke','white')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".8rem"; }
    else {
    return '.9rem';}
  })
  .attr('id', 'ut_stage_line_2');

svg_ut_walkthrough
  .selectAll()
  .data(ut_stage_circles)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y) + 25 })
  .text(function(d){ return d.line_3})
  .style("fill", "white")
  .style('stroke','white')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".8rem"; }
    else {
    return '.9rem';}
  })
  .attr('id', 'ut_stage_line_3');

// Small circles for arrows
svg_ut_walkthrough
  .selectAll()
  .data(ut_stage_arrows)
  .enter()
  .append("circle")
  .attr('id', "ut_stage_arrow_circles")
  .attr("cx", function(d){ return x_pos(d.x)})
  .attr("cy", function(d){ return y_pos(d.y)})
  .attr("r",  function(d){ return arrow_size(width)});

var ut_images_arrows = svg_ut_walkthrough.selectAll("bar")
  .data(ut_stage_arrows)
  .enter()
  .append("svg:image")
  .attr("x", function(d) { return x_pos(d.x) - 10; })
  .attr('y', function(d) { return y_pos(d.y) - 10; })
  .attr('height', 20)
  .attr("xlink:href", function(d) {return d.img; })
  .attr('id', '#ut_arrow_images');

var selected_ut_area_df = json_ut.filter(function(d){
    return d.Area_name === selected_ut_area_option
  })

svg_ut_walkthrough
  .selectAll('.outcomes')
  .data(selected_ut_area_df)
  .enter()
  .append("circle")
  .attr('class','outcomes')
  .attr("cx", function(d){ return x_pos(d.x) } )
  .attr("cy", function(d){ return y_pos(d.y) } )
  .attr("r",  function(d){ return circle_size(width)})
  .attr('fill',  function(d){ return d.Colour})
  .attr('stroke', 'none')
  .on("mousemove", showTooltip_ut)
  .on('click', choose_an_indicator)
  .on('mouseout', mouseleave_ut);

svg_ut_walkthrough
  .selectAll()
  .data(selected_ut_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .075) })
  .text(function(d){ return d.Label_screen})
  .style("fill", "black")
  .style('font-weight', 'bold')
  .style('stroke','none')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".9rem"; }
    else {
    return '1rem';}
  })
  .attr('id', 'ut_label_1');

svg_ut_walkthrough
  .selectAll()
  .data(selected_ut_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .1) })
  .text(function(d){ return d.line_1})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".6rem"; }
    else {
    return '.8rem';}
  })
  .attr('id', 'ut_line_1');

svg_ut_walkthrough
  .selectAll()
  .data(selected_ut_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .12) })
  .text(function(d){ return d.line_2})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".6rem"; }
    else {
    return '.8rem';}
  })
  .attr('id', 'ut_line_2');

svg_ut_walkthrough
  .selectAll()
  .data(selected_ut_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .14) })
  .text(function(d){ return d.line_3})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".6rem"; }
    else {
    return '.8rem';}
  })
  .attr('id', 'ut_line_3');

svg_ut_walkthrough
  .selectAll()
  .data(selected_ut_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .16) })
  .text(function(d){ return d.line_4})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".6rem"; }
    else {
    return '.8rem';}
  })
  .attr('id', 'ut_line_4');

svg_ut_walkthrough
  .selectAll()
  .data(selected_ut_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .18) })
  .text(function(d){ return d.line_5})
  .style("fill", "black")
  .style('stroke','none')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".6rem"; }
    else {
    return '.8rem';}
  })
  .attr('id', 'ut_line_5');

  svg_ut_walkthrough
    .append('text')
    .attr("dx", function(d){ return x_pos(selected_ut_area_df[3].x) })
    .attr("dy", function(d){ return y_pos(selected_ut_area_df[3].y - 0.01) })
    .text('Infant')
    .style("fill", "#FFFFFF")
    .style('font-weight', 'bold')
    .style('stroke','none')
    .attr('text-anchor','middle')
    .attr("font-size", function (d) {
      if (width < 1300) {
      return ".7rem"; }
      else {
      return '.8rem';}
      })
    .attr('id', 'indicator_4_line_1');

    svg_ut_walkthrough
      .append('text')
      .attr("dx", function(d){ return x_pos(selected_ut_area_df[3].x) })
      .attr("dy", function(d){ return y_pos(selected_ut_area_df[3].y + 0.01) })
      .text('mortality')
      .style("fill", "#FFFFFF")
      .style('font-weight', 'bold')
      .style('stroke','none')
      .attr('text-anchor','middle')
      .attr("font-size", function (d) {
        if (width < 1300) {
        return ".7rem"; }
        else {
        return '.8rem';}
        })
      .attr('id', 'indicator_3_line_2');

// Icons
svg_ut_walkthrough.selectAll('icons_yo')
  .data(selected_ut_area_df)
  .enter()
  .append("svg:image")
  .attr("x", function(d) { return x_pos(d.x) - scaled_icon_size/2; })
  .attr('y', function(d) { return y_pos(d.y) - scaled_icon_size/2; })
  .attr('height', scaled_icon_size)
  .on("mousemove", showTooltip_ut)
  .on('mouseout', mouseleave_ut)
  .on('click', choose_an_indicator)
  .attr('class', 'icons_yo')
  .attr("xlink:href", function(d) {return d.img_path; })
  .attr('id', 'ut_indicator_icon_images');

function update_ut_walkthrough(selected_ut_area_option) {

// Set up tooltip for circles
var tooltip_ut = d3.select("#wsx_upper_tier_walkthrough")
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

var showTooltip_ut = function(d, i) {

tooltip_ut
.html("<h4>" + d.Name + '</h4><p class = "tt_text">In ' + d.Area_name + ' in ' + d.Timeperiod + ' the ' + d.Unit + ' was <font color = "#1e4b7a" size = "3"><b>' + d.Label + '</b></font>. </p><p class = "tt_text">' + significance_key(d.Significance) + '. ' + polarity_key(d.Polarity))
  .style("opacity", 1)
  .style("top", (event.pageY - 10) + "px")
  .style("left", (event.pageX + 10) + "px")
  .style("visibility", "visible")
  }

var mouseleave_ut = function(d) {
  tooltip_ut.style("visibility", "hidden")
  }

var selected_ut_area_option = d3.select('#select_area_ut_button').property("value")

var selected_ut_area_df = json_ut.filter(function(d){
    return d.Area_name === selected_ut_area_option})

svg_ut_walkthrough
  .selectAll("#ut_label_1")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_ut_walkthrough
  .selectAll("#ut_line_1")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_ut_walkthrough
  .selectAll("#ut_line_2")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_ut_walkthrough
  .selectAll("#ut_line_3")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_ut_walkthrough
  .selectAll("#ut_line_4")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_ut_walkthrough
  .selectAll("#ut_line_5")
  .transition()
  .duration(750)
  .style('opacity' ,0 )
  .remove();

svg_ut_walkthrough
  .selectAll()
  .data(selected_ut_area_df)
  .enter()
  .append('text')
  .attr("dx", function(d){ return x_pos(d.x) })
  .attr("dy", function(d){ return y_pos(d.y + .075) })
  .text(function(d){ return d.Label_screen})
  .style("fill", "black")
  .style('font-weight', 'bold')
  .style('stroke','none')
  .attr('text-anchor','middle')
  .attr("font-size", function (d) {
    if (width < 1300) {
    return ".9rem"; }
    else {
    return '1rem';}
  })
  .attr('id', 'ut_label_1')
  .style('opacity', 0)
  .transition()
  .duration(750)
  .style('opacity', 1);

  svg_ut_walkthrough
    .selectAll()
    .data(selected_ut_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .1) })
    .text(function(d){ return d.line_1})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    // .style('font-size', '.7rem')
    .attr("font-size", function (d) {
      if (width < 1300) {
      return ".6rem"; }
      else {
      return '.8rem';}
    })
    .attr('id', 'ut_line_1')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_ut_walkthrough
    .selectAll()
    .data(selected_ut_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .12) })
    .text(function(d){ return d.line_2})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .attr("font-size", function (d) {
      if (width < 1300) {
      return ".6rem"; }
      else {
      return '.8rem';}
    })
    .attr('id', 'ut_line_2')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_ut_walkthrough
    .selectAll()
    .data(selected_ut_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .14) })
    .text(function(d){ return d.line_3})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .attr("font-size", function (d) {
      if (width < 1300) {
      return ".6rem"; }
      else {
      return '.8rem';}
    })
    .attr('id', 'ut_line_3')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_ut_walkthrough
    .selectAll()
    .data(selected_ut_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .16) })
    .text(function(d){ return d.line_4})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .attr("font-size", function (d) {
      if (width < 1300) {
      return ".6rem"; }
      else {
      return '.8rem';}
    })
    .attr('id', 'ut_line_4')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

  svg_ut_walkthrough
    .selectAll()
    .data(selected_ut_area_df)
    .enter()
    .append('text')
    .attr("dx", function(d){ return x_pos(d.x) })
    .attr("dy", function(d){ return y_pos(d.y + .18) })
    .text(function(d){ return d.line_5})
    .style("fill", "black")
    .style('stroke','none')
    .attr('text-anchor','middle')
    .attr("font-size", function (d) {
      if (width < 1300) {
      return ".6rem"; }
      else {
      return '.8rem';}
    })
    .attr('id', 'ut_line_5')
    .style('opacity', 0)
    .transition()
    .duration(750)
    .style('opacity', 1);

svg_ut_walkthrough
  .selectAll('.outcomes')
  .data(selected_ut_area_df)
  .on("mousemove", showTooltip_ut)
  .on('mouseout', mouseleave_ut)
  .transition()
  .duration(1750)
  .attr('fill',  function(d){ return d.Colour});

svg_ut_walkthrough
  .selectAll('.icons_yo')
  .data(selected_ut_area_df)
  .on("mousemove", showTooltip_ut)
  .on('mouseout', mouseleave_ut)
  .on('click', choose_an_indicator);

  }

  d3.select("#select_area_ut_button").on("change", function(d) {
  var selected_ut_area_option = d3.select('#select_area_ut_button').property("value")
    update_ut_walkthrough(selected_ut_area_option)
    })

// This function listens to if there is a window size change
var globalResizeTimer_ut = null;

$(window).resize(function() {

if(globalResizeTimer_ut != null) window.clearTimeout(globalResizeTimer_ut);
  globalResizeTimer_ut = window.setTimeout(function() {

var width = (window.innerWidth * .9) - 20;
var height = width * .6;

// We want the infographic to redraw at the scale of the users screen. We have identified (through trial and error) the best position within the width
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

if(width < 1300){
var scaled_icon_size = 30
  }

if(width > 1300){
var scaled_icon_size = 50
  }

d3.select('.upper_tier_svg')
  .remove();

// append the svg object to the body of the page
var svg_ut_walkthrough = d3.select("#wsx_upper_tier_walkthrough")
.append("svg")
.attr("width", width)
.attr("height", height)
.attr('class', 'upper_tier_svg')
.append("g");

// Walkthrough dotted lines
svg_ut_walkthrough
.append("line")
.attr('x1', function(d){ return x_pos(0.04)})
.attr('y1', function(d){ return y_pos(0.1)})
.attr('x2', function(d){ return x_pos(0.96)})
.attr('y2', function(d){ return y_pos(0.1)})
.attr('stroke', 'black')
.style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
.append("line")
.attr('x1', function(d){ return x_pos(0.96)})
.attr('y1', function(d){ return y_pos(0.1)})
.attr('x2', function(d){ return x_pos(0.96)})
.attr('y2', function(d){ return y_pos(0.35)})
.attr('stroke', 'black')
.style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
.append("line")
.attr('x1', function(d){ return x_pos(0.04)})
.attr('y1', function(d){ return y_pos(0.35)})
.attr('x2', function(d){ return x_pos(0.96)})
.attr('y2', function(d){ return y_pos(0.35)})
.attr('stroke', 'black')
.style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
.append("line")
.attr('x1', function(d){ return x_pos(0.04)})
.attr('y1', function(d){ return y_pos(0.35)})
.attr('x2', function(d){ return x_pos(0.04)})
.attr('y2', function(d){ return y_pos(0.6)})
.attr('stroke', 'black')
.style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
.append("line")
.attr('x1', function(d){ return x_pos(0.04)})
.attr('y1', function(d){ return y_pos(0.6)})
.attr('x2', function(d){ return x_pos(0.96)})
.attr('y2', function(d){ return y_pos(0.6)})
.attr('stroke', 'black')
.style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
.append("line")
.attr('x1', function(d){ return x_pos(0.96)})
.attr('y1', function(d){ return y_pos(0.6)})
.attr('x2', function(d){ return x_pos(0.96)})
.attr('y2', function(d){ return y_pos(0.85)})
.attr('stroke', 'black')
.style('stroke-dasharray', ('2,4'));

svg_ut_walkthrough
.append("line")
.attr('x1', function(d){ return x_pos(0.04)})
.attr('y1', function(d){ return y_pos(0.85)})
.attr('x2', function(d){ return x_pos(0.96)})
.attr('y2', function(d){ return y_pos(0.85)})
.attr('stroke', 'black')
.style('stroke-dasharray', ('2,4'));

// Big circles for each stage of life
svg_ut_walkthrough
.selectAll()
.data(ut_stage_circles)
.enter()
.append('circle')
.attr("cx", function(d){ return x_pos(d.x) })
.attr("cy", function(d){ return y_pos(d.y) })
.attr("r",  function(d){ return stage_size(width)})
.style("fill", "black")
.attr('id', 'ut_stage_labels');

svg_ut_walkthrough
.selectAll()
.data(ut_stage_circles)
.enter()
.append('text')
.attr("dx", function(d){ return x_pos(d.x) })
.attr("dy", function(d){ return y_pos(d.y) - 5 })
.text(function(d){ return d.line_1})
.style("fill", "white")
.style('stroke','white')
.attr('text-anchor','middle')
.attr("font-size", function (d) {
    if (width < 1300) {
    return ".8rem"; }
    else {
    return '.9rem';}
      })
.attr('id', 'ut_stage_line_1');

svg_ut_walkthrough
.selectAll()
.data(ut_stage_circles)
.enter()
.append('text')
.attr("dx", function(d){ return x_pos(d.x) })
.attr("dy", function(d){ return y_pos(d.y) + 10 })
.text(function(d){ return d.line_2})
.style("fill", "white")
.style('stroke','white')
.attr('text-anchor','middle')
.attr("font-size", function (d) {
    if (width < 1300) {
    return ".8rem"; }
    else {
    return '.9rem';}
      })
.attr('id', 'ut_stage_line_2');

svg_ut_walkthrough
.selectAll()
.data(ut_stage_circles)
.enter()
.append('text')
.attr("dx", function(d){ return x_pos(d.x) })
.attr("dy", function(d){ return y_pos(d.y) + 25 })
.text(function(d){ return d.line_3})
.style("fill", "white")
.style('stroke','white')
.attr('text-anchor','middle')
.attr("font-size", function (d) {
    if (width < 1300) {
    return ".8rem"; }
    else {
    return '.9rem';}
    })
.attr('id', 'ut_stage_line_3');

// Small circles for arrows
svg_ut_walkthrough
.selectAll()
.data(ut_stage_arrows)
.enter()
.append("circle")
.attr('id', "ut_stage_arrow_circles")
.attr("cx", function(d){ return x_pos(d.x)})
.attr("cy", function(d){ return y_pos(d.y)})
.attr("r",  function(d){ return arrow_size(width)});

var ut_images_arrows = svg_ut_walkthrough.selectAll("bar")
.data(ut_stage_arrows)
.enter()
.append("svg:image")
.attr("x", function(d) { return x_pos(d.x) - 12; })
.attr('y', function(d) { return y_pos(d.y) - 12; })
.attr('height', 24)
.attr("xlink:href", function(d) {return d.img; })
.attr('id', '#ut_arrow_images');

var selected_ut_area_option = d3.select('#select_area_ut_button').property("value")

var selected_ut_area_df = json_ut.filter(function(d){
            return d.Area_name === selected_ut_area_option
          })

        svg_ut_walkthrough
          .selectAll('.outcomes')
          .data(selected_ut_area_df)
          .enter()
          .append("circle")
          .attr('class','outcomes')
          .attr("cx", function(d){ return x_pos(d.x) } )
          .attr("cy", function(d){ return y_pos(d.y) } )
          .attr("r",  function(d){ return circle_size(width)})
          .attr('fill',  function(d){ return d.Colour})
          .attr('stroke', 'none')
          .on("mousemove", showTooltip_ut)
          .on('click', choose_an_indicator)
          .on('mouseout', mouseleave_ut);

        svg_ut_walkthrough
          .selectAll()
          .data(selected_ut_area_df)
          .enter()
          .append('text')
          .attr("dx", function(d){ return x_pos(d.x) })
          .attr("dy", function(d){ return y_pos(d.y + .075) })
          .text(function(d){ return d.Label_screen})
          .style("fill", "black")
          .style('font-weight', 'bold')
          .style('stroke','none')
          .attr('text-anchor','middle')
          .attr("font-size", function (d) {
            if (width < 1300) {
            return ".9rem"; }
            else {
            return '1rem';}
          })
          .attr('id', 'ut_label_1');

        svg_ut_walkthrough
          .selectAll()
          .data(selected_ut_area_df)
          .enter()
          .append('text')
          .attr("dx", function(d){ return x_pos(d.x) })
          .attr("dy", function(d){ return y_pos(d.y + .1) })
          .text(function(d){ return d.line_1})
          .style("fill", "black")
          .style('stroke','none')
          .attr('text-anchor','middle')
          .attr("font-size", function (d) {
            if (width < 1300) {
            return ".6rem"; }
            else {
            return '.8rem';}
          })
          .attr('id', 'ut_line_1');

        svg_ut_walkthrough
          .selectAll()
          .data(selected_ut_area_df)
          .enter()
          .append('text')
          .attr("dx", function(d){ return x_pos(d.x) })
          .attr("dy", function(d){ return y_pos(d.y + .12) })
          .text(function(d){ return d.line_2})
          .style("fill", "black")
          .style('stroke','none')
          .attr('text-anchor','middle')
          .attr("font-size", function (d) {
            if (width < 1300) {
            return ".6rem"; }
            else {
            return '.8rem';}
          })
          .attr('id', 'ut_line_2');

        svg_ut_walkthrough
          .selectAll()
          .data(selected_ut_area_df)
          .enter()
          .append('text')
          .attr("dx", function(d){ return x_pos(d.x) })
          .attr("dy", function(d){ return y_pos(d.y + .14) })
          .text(function(d){ return d.line_3})
          .style("fill", "black")
          .style('stroke','none')
          .attr('text-anchor','middle')
          .attr("font-size", function (d) {
            if (width < 1300) {
            return ".6rem"; }
            else {
            return '.8rem';}
          })
          .attr('id', 'ut_line_3');

        svg_ut_walkthrough
          .selectAll()
          .data(selected_ut_area_df)
          .enter()
          .append('text')
          .attr("dx", function(d){ return x_pos(d.x) })
          .attr("dy", function(d){ return y_pos(d.y + .16) })
          .text(function(d){ return d.line_4})
          .style("fill", "black")
          .style('stroke','none')
          .attr('text-anchor','middle')
          .attr("font-size", function (d) {
            if (width < 1300) {
            return ".6rem"; }
            else {
            return '.8rem';}
          })
          .attr('id', 'ut_line_4');

        svg_ut_walkthrough
          .selectAll()
          .data(selected_ut_area_df)
          .enter()
          .append('text')
          .attr("dx", function(d){ return x_pos(d.x) })
          .attr("dy", function(d){ return y_pos(d.y + .18) })
          .text(function(d){ return d.line_5})
          .style("fill", "black")
          .style('stroke','none')
          .attr('text-anchor','middle')
          .attr("font-size", function (d) {
            if (width < 1300) {
            return ".6rem"; }
            else {
            return '.8rem';}
          })
          .attr('id', 'ut_line_5');

          svg_ut_walkthrough
            .append('text')
            .attr("dx", function(d){ return x_pos(selected_ut_area_df[3].x) })
            .attr("dy", function(d){ return y_pos(selected_ut_area_df[3].y - 0.01) })
            .text('Infant')
            .style("fill", "#FFFFFF")
            .style('font-weight', 'bold')
            .style('stroke','none')
            .attr('text-anchor','middle')
            .attr("font-size", function (d) {
              if (width < 1300) {
              return ".7rem"; }
              else {
              return '.8rem';}
              })
            .attr('id', 'indicator_4_line_1');

            svg_ut_walkthrough
              .append('text')
              .attr("dx", function(d){ return x_pos(selected_ut_area_df[3].x) })
              .attr("dy", function(d){ return y_pos(selected_ut_area_df[3].y + 0.01) })
              .text('mortality')
              .style("fill", "#FFFFFF")
              .style('font-weight', 'bold')
              .style('stroke','none')
              .attr('text-anchor','middle')
              .attr("font-size", function (d) {
                if (width < 1300) {
                return ".7rem"; }
                else {
                return '.8rem';}
                })
              .attr('id', 'indicator_4_line_2');

        // Icons
        svg_ut_walkthrough.selectAll('icons_yo')
          .data(selected_ut_area_df)
          .enter()
          .append("svg:image")
          .attr("x", function(d) { return x_pos(d.x) - scaled_icon_size/2; })
          .attr('y', function(d) { return y_pos(d.y) - scaled_icon_size/2; })
          .attr('height', scaled_icon_size)
          .on("mousemove", showTooltip_ut)
          .on('mouseout', mouseleave_ut)
          .on('click', choose_an_indicator)
          .attr('class', 'icons_yo')
          .attr("xlink:href", function(d) {return d.img_path; })
          .attr('id', 'ut_indicator_icon_images');

          function update_ut_walkthrough(selected_ut_area_option) {

          // Set up tooltip for circles
          var tooltip_ut = d3.select("#wsx_upper_tier_walkthrough")
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

          var showTooltip_ut = function(d, i) {

          tooltip_ut
          .html("<h4>" + d.Name + '</h4><p class = "tt_text">In ' + d.Area_name + ' in ' + d.Timeperiod + ' the ' + d.Unit + ' was <font color = "#1e4b7a" size = "3"><b>' + d.Label + '</b></font>. </p><p class = "tt_text">' + significance_key(d.Significance) + '. ' + polarity_key(d.Polarity))
            .style("opacity", 1)
            .style("top", (event.pageY - 10) + "px")
            .style("left", (event.pageX + 10) + "px")
            .style("visibility", "visible")
            }

          var mouseleave_ut = function(d) {
            tooltip_ut.style("visibility", "hidden")
            }

          var selected_ut_area_option = d3.select('#select_area_ut_button').property("value")

          var selected_ut_area_df = json_ut.filter(function(d){
              return d.Area_name === selected_ut_area_option})

          svg_ut_walkthrough
            .selectAll("#ut_label_1")
            .transition()
            .duration(750)
            .style('opacity' ,0 )
            .remove();

          svg_ut_walkthrough
            .selectAll("#ut_line_1")
            .transition()
            .duration(750)
            .style('opacity' ,0 )
            .remove();

          svg_ut_walkthrough
            .selectAll("#ut_line_2")
            .transition()
            .duration(750)
            .style('opacity' ,0 )
            .remove();

          svg_ut_walkthrough
            .selectAll("#ut_line_3")
            .transition()
            .duration(750)
            .style('opacity' ,0 )
            .remove();

          svg_ut_walkthrough
            .selectAll("#ut_line_4")
            .transition()
            .duration(750)
            .style('opacity' ,0 )
            .remove();

          svg_ut_walkthrough
            .selectAll("#ut_line_5")
            .transition()
            .duration(750)
            .style('opacity' ,0 )
            .remove();

          svg_ut_walkthrough
            .selectAll()
            .data(selected_ut_area_df)
            .enter()
            .append('text')
            .attr("dx", function(d){ return x_pos(d.x) })
            .attr("dy", function(d){ return y_pos(d.y + .075) })
            .text(function(d){ return d.Label_screen})
            .style("fill", "black")
            .style('font-weight', 'bold')
            .style('stroke','none')
            .attr('text-anchor','middle')
            .attr("font-size", function (d) {
              if (width < 1300) {
              return ".9rem"; }
              else {
              return '1rem';}
            })
            .attr('id', 'ut_label_1')
            .style('opacity', 0)
            .transition()
            .duration(750)
            .style('opacity', 1);

            svg_ut_walkthrough
              .selectAll()
              .data(selected_ut_area_df)
              .enter()
              .append('text')
              .attr("dx", function(d){ return x_pos(d.x) })
              .attr("dy", function(d){ return y_pos(d.y + .1) })
              .text(function(d){ return d.line_1})
              .style("fill", "black")
              .style('stroke','none')
              .attr('text-anchor','middle')
              // .style('font-size', '.7rem')
              .attr("font-size", function (d) {
                if (width < 1300) {
                return ".6rem"; }
                else {
                return '.8rem';}
              })
              .attr('id', 'ut_line_1')
              .style('opacity', 0)
              .transition()
              .duration(750)
              .style('opacity', 1);

            svg_ut_walkthrough
              .selectAll()
              .data(selected_ut_area_df)
              .enter()
              .append('text')
              .attr("dx", function(d){ return x_pos(d.x) })
              .attr("dy", function(d){ return y_pos(d.y + .12) })
              .text(function(d){ return d.line_2})
              .style("fill", "black")
              .style('stroke','none')
              .attr('text-anchor','middle')
              .attr("font-size", function (d) {
                if (width < 1300) {
                return ".6rem"; }
                else {
                return '.8rem';}
              })
              .attr('id', 'ut_line_2')
              .style('opacity', 0)
              .transition()
              .duration(750)
              .style('opacity', 1);

            svg_ut_walkthrough
              .selectAll()
              .data(selected_ut_area_df)
              .enter()
              .append('text')
              .attr("dx", function(d){ return x_pos(d.x) })
              .attr("dy", function(d){ return y_pos(d.y + .14) })
              .text(function(d){ return d.line_3})
              .style("fill", "black")
              .style('stroke','none')
              .attr('text-anchor','middle')
              .attr("font-size", function (d) {
                if (width < 1300) {
                return ".6rem"; }
                else {
                return '.8rem';}
              })
              .attr('id', 'ut_line_3')
              .style('opacity', 0)
              .transition()
              .duration(750)
              .style('opacity', 1);

            svg_ut_walkthrough
              .selectAll()
              .data(selected_ut_area_df)
              .enter()
              .append('text')
              .attr("dx", function(d){ return x_pos(d.x) })
              .attr("dy", function(d){ return y_pos(d.y + .16) })
              .text(function(d){ return d.line_4})
              .style("fill", "black")
              .style('stroke','none')
              .attr('text-anchor','middle')
              .attr("font-size", function (d) {
                if (width < 1300) {
                return ".6rem"; }
                else {
                return '.8rem';}
              })
              .attr('id', 'ut_line_4')
              .style('opacity', 0)
              .transition()
              .duration(750)
              .style('opacity', 1);

            svg_ut_walkthrough
              .selectAll()
              .data(selected_ut_area_df)
              .enter()
              .append('text')
              .attr("dx", function(d){ return x_pos(d.x) })
              .attr("dy", function(d){ return y_pos(d.y + .18) })
              .text(function(d){ return d.line_5})
              .style("fill", "black")
              .style('stroke','none')
              .attr('text-anchor','middle')
              .attr("font-size", function (d) {
                if (width < 1300) {
                return ".6rem"; }
                else {
                return '.8rem';}
              })
              .attr('id', 'ut_line_5')
              .style('opacity', 0)
              .transition()
              .duration(750)
              .style('opacity', 1);

          svg_ut_walkthrough
            .selectAll('.outcomes')
            .data(selected_ut_area_df)
            .on("mousemove", showTooltip_ut)
            .on('mouseout', mouseleave_ut)
            .transition()
            .duration(1750)
            .attr('fill',  function(d){ return d.Colour});

          svg_ut_walkthrough
            .selectAll('.icons_yo')
            .data(selected_ut_area_df)
            .on("mousemove", showTooltip_ut)
            .on('mouseout', mouseleave_ut)
            .on('click', choose_an_indicator);

            }

            d3.select("#select_area_ut_button").on("change", function(d) {
            var selected_ut_area_option = d3.select('#select_area_ut_button').property("value")
              update_ut_walkthrough(selected_ut_area_option)
              })

console.log(window.innerWidth)

      }, 400);
  });

  d3.select("#select_area_ut_button").on("change", function(d) {
  var selected_ut_area_option = d3.select('#select_area_ut_button').property("value")
    update_ut_walkthrough(selected_ut_area_option)
    })

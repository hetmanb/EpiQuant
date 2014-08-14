d3.chordDiagram = function module() {
  var width = 720,
      height = 720,
      id = null,
      outerRadius = Math.min(width, height) / 2 - 10,
      innerRadius = outerRadius - 24,
      formatPercent = d3.format(".1%");


  function chord_diagram(_selection) {

    var arc = d3.svg.arc()
        .innerRadius(innerRadius)
        .outerRadius(outerRadius);

    var layout = d3.layout.chord()
        .padding(.04)
        .sortSubgroups(d3.descending)
        .sortChords(d3.ascending);

    var path = d3.svg.chord()
        .radius(innerRadius);
    var svg = d3.select("#" + id).append("svg")
        .attr("width", width)
        .attr("height", height)
      .append("g")
        .attr("id", "circle")
        .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

    svg.append("circle")
        .attr("r", outerRadius);
    _selection.each(function(data) { 
      _selection.each(function(data) {
        matrix = data.matrix
        data = data.data
        // Compute the chord layout.
        layout.matrix(matrix);

        // Add a group per neighborhood.
        var group = svg.selectAll(".group")
            .data(layout.groups)
          .enter().append("g")
            .attr("class", "group")
            .on("mouseover", mouseover);

        // Add a mouseover title.
        // not meaningful for wikipedia votes
        // group.append("title").text(function(d, i) {
        //   return data[i].name + ": " + formatPercent(d.value) + " of origins";
        // });

        // Add the group arc.
        var groupPath = group.append("path")
            .attr("id", function(d, i) { return "group" + i; })
            .attr("d", arc)
            .style("fill", function(d, i) { return data[i].color; });

        // Add a text label.
        var groupText = group.append("text")
            .attr("x", 6)
            .attr("dy", 15);

        groupText.append("textPath")
            .attr("xlink:href", function(d, i) { return "#group" + i; })
            .text(function(d, i) { return data[i].name; });

        // Remove the labels that don't fit. :(
        groupText.filter(function(d, i) { return groupPath[0][i].getTotalLength() / 2 - 25 < this.getComputedTextLength(); })
            .remove();

        // Add the chords.
        var chord = svg.selectAll(".chord")
            .data(layout.chords)
          .enter().append("path")
            .attr("class", "chord")
            .style("fill", function(d) { return data[d.source.index].color; })
            .attr("d", path);

        // Add an elaborate mouseover title for each chord.
        // must be customized on individual basis
        chord.append("title").text(function(d){
          return formatPercent(d.source.value) +
              " of voters who voted for " + 
              data[d.source.index].name + "\nalso voted for " +
              data[d.target.index].name + 
              "\n" + formatPercent(d.target.value) +
              " of voters who voted for " +
              data[d.target.index].name + "\nalso voted for " +
              data[d.source.index].name;
        })
        // chord.append("title").text(function(d) {
        //   return data[d.source.index].name
        //       + " → " + data[d.target.index].name
        //       + ": " + formatPercent(d.source.value)
        //       + "\n" + data[d.target.index].name
        //       + " → " + data[d.source.index].name
        //       + ": " + formatPercent(d.target.value);
        // });

        function mouseover(d, i) {
          chord.classed("fade", function(p) {
            return p.source.index != i
                && p.target.index != i;
          });
        }
      });
    });
  };
  chord_diagram.id = function(_x) {
    if(!arguments.length) return id;
    id = _x
    return chord_diagram;
  }
  chord_diagram.width = function(_x) {
    if(!arguments.length) return width;
    width = _x;
    calc_radii();
    return chord_diagram;
  };
  chord_diagram.height = function(_x) {
    if(!arguments.length) return height;
    height = _x;
    calc_radii();
    return chord_diagram;
  };
  calc_radii = function() {
    outerRadius = Math.min(width, height) / 2 - 10
    innerRadius = outerRadius - 24
    return chord_diagram
  }
  d3.rebind(chord_diagram);
  return chord_diagram;
};
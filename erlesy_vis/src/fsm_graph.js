// TODO: Index multiple selv-loops to make them different size OR collate them together in erl part and display as event | another_event
// TODO: Degree centraliy - node size (out degree is the interesting one)


var width = 1920,
    height = 1024;

var color = d3.scale.category20();

var force = d3.layout.force()
    .charge(-5000)
    .linkDistance(150)
    .size([width, height]);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

d3.json("example_fsm.json", function(error, graph) {
    if (error) throw error;
    var edges = [];
    find_vertex = function(name, vertices) {
        for (i = 0; i < vertices.length; i++)
            if (vertices[i].name == name)
                return i;
        return -1;
    };
    graph.edges.forEach(function(e) {

        edges.push({
            source: find_vertex(e.source,
                graph.vertices),
            target: find_vertex(e.target,
                graph.vertices),
            label: e.label
        });
    });

    force
        .nodes(graph.vertices)
        .links(edges)
        .start();

    var linkGroups = svg.selectAll(".linkGroup")
        .data(edges)
        .enter().append("g")
        .attr("class", "linkGroup");

    linkGroups.append("path")
        .attr("class", "link")
        .style("stroke-width", 2);

    linkGroups.append("text")
        .text(function(d) {
            return d.label;
        });

    var groups = svg.selectAll(".nodeGroup")
        .data(graph.vertices)
        .enter().append("g")
        .attr("class", "nodeGroup")
        .call(force.drag);

    groups.append("circle")
        .attr("class", "node")
        .attr("r", 30)
        .attr("cx", 0)
        .attr("cy", 0)
        .style("fill", color(1));

    groups.append("text")
        .text(function(d) {
            return d.name;
        })
        .attr("x", 0)
        .attr("y", 0);


    force.on("tick", function() {

        linkGroups.selectAll("path").attr("d",
            function(d) {
                var x1 = d.source.x,
                    y1 = d.source.y,
                    x2 = d.target.x,
                    y2 = d.target.y,
                    dx = x2 - x1,
                    dy = y2 - y1,
                    dr = Math.sqrt(dx * dx + dy *
                        dy),

                    // Defaults for normal edge.
                    drx = dr,
                    dry = dr,
                    xRotation = 0, // degrees
                    largeArc = 0, // 1 or 0
                    sweep = 1; // 1 or 0

                // Self edge.
                if (x1 === x2 && y1 === y2) {
                    // Fiddle with this angle to get loop oriented.
                    xRotation = -45;

                    // Needs to be 1.
                    largeArc = 1;

                    // Change sweep to change orientation of loop. 
                    //sweep = 0;

                    // Make drx and dry different to get an ellipse
                    // instead of a circle.
                    drx = 30;
                    dry = 20;

                    // For whatever reason the arc collapses to a point if the beginning
                    // and ending points of the arc are the same, so kludge it.
                    x2 = x2 + 1;
                    y2 = y2 + 1;
                }

                return "M" + x1 + "," + y1 + "A" +
                    drx + "," + dry + " " +
                    xRotation + "," + largeArc +
                    "," + sweep + " " + x2 + "," +
                    y2;
            });


        //    linkGroups.selectAll("line").attr("x1", function(d) { return d.source.x; })
        //        .attr("y1", function(d) { return d.source.y; })
        //        .attr("x2", function(d) { return d.target.x; })
        //        .attr("y2", function(d) { return d.target.y; });

        linkGroups.selectAll("text")
            .attr("x", function(d) {
                if (d.source.x == d.target.x)
                    return d.source.x + 25;
                else
                    return (d.source.x + d.target
                        .x) / 2;
            })
            .attr("y", function(d) {
                if (d.source.y == d.target.y)
                    return d.source.y - 45;
                else
                    return (d.source.y + d.target
                        .y) / 2;
            });


        groups.attr("transform", function(d) {
            return "translate(" + d.x + "," +
                d.y + ")";
        });
    });
}); 
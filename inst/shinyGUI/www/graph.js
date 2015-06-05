/*
<style>


.node {
    stroke: #000;
    stroke-width: 0.5px;
}


.node-cluster{
    stroke: #000;
    fill: rgb(79,147,222);
    stroke-width: 0.5px;
}

.node-landmark{
    stroke: #000;
    fill: rgb(255,117,128);
    stroke-width: 0.5px;
}

.link {
    stroke: #999;
    stroke-opacity: .6;
}




</style>
*/
//<script src="http://d3js.org/d3.v2.js"></script>


//<script type="text/javascript">

function display_edge(option_val, edge_type)
{
    if(option_val == "All")
        return true;
    else if(option_val == "Inter cluster")
        return(edge_type == "inter_cluster");
    else if(option_val == "To landmark")
        return(edge_type == "cluster_to_landmark" || edge_type == "highest_scoring");
    else if(option_val == "Highest scoring")
        return(edge_type == "highest_scoring");
    else
        return(false);
}

function parse_trans_string (a)
{
    //This requires the different arguments (transl, scale etc.) to be space-delimited
    var b = {};
    console.log(a);
    for (var i in a = a.match(/(\w+\((\-?\d+\.?\d*e?\-?\d*,?)+\))+/g))
    {
        var c = a[i].match(/[\w\.\-]+/g);
        b[c.shift()] = c;
    }
    return b;
}


var networkOutputBinding = new Shiny.OutputBinding();
$.extend(networkOutputBinding, {
         find: function(scope) {
         return $(scope).find('.shiny-network-output');
         },
         renderValue: function(el, data)
         {
            if(data == null) return;
            var nodes = new Array();
            for (var i = 0; i < data.names.length; i++)
            {
                nodes.push({"name": data.names[i], "X": data.X[i], "Y": data.Y[i], "color": data.color[i], "type": data.type[i], "size": data.size[i], "highest_scoring_edge" : data.highest_scoring_edge[i]});
            }
            
            var lin = new Array();
            var edges = data.edges;
            for(var i = 0; i < edges.id.length; i++)
            {
                lin.push({"x1" : edges.x1[i], "x2" : edges.x2[i], "y1" : edges.y1[i], "y2" : edges.y2[i], "source" : edges.source[i], "target" : edges.target[i], "edge_type" : edges.edge_type[i], "id" : edges.id[i], "is_highest_scoring" : edges.is_highest_scoring[i]});
            }
            
                
            
            function rescale()
            {
                vis.attr("transform", "translate(" + d3.event.translate + ") scale(" + d3.event.scale + ")");
            }
         
            var width = 1200;
            var height = 800;
            
            
            var zoom = d3.behavior.zoom()
                .scaleExtent([0.01, 10])
                .on("zoom", rescale);
         
            //remove the old graph
            var svg = d3.select(el).select("svg");
            svg.remove();
            Shiny.onInputChange("graphui_selected_landmark", "");
            Shiny.onInputChange("graphui_selected_cluster", "");
         
            $(el).html("");
         
            //append a new one
            svg = d3.select(el).append("svg");
            svg = make_draggable(svg, zoom);
            svg.attr("width", width)
                .attr("height", height)
                .attr("id", "main_graph")
                .attr("viewBox", "0 0 " + width + " " + height)
                .attr("perserveAspectRatio", "xMinYMid")
                .attr("pointer-events", "all")
                .call(zoom)
                ;
         
            var aspect = width / height,
            chart = $("#main_graph");
            $(window).on("resize", function() {
                var targetWidth = chart.parent().width();
                chart.attr("width", targetWidth);
                chart.attr("height", targetWidth / aspect);
            });
         

         
            var vis = svg.append('svg:g');
         
            if(data.trans_to_apply)
            {
                var trans = parse_trans_string(data.trans_to_apply);
                var trasl = trans.translate.map(parseFloat);
                zoom.translate(trasl);
                zoom.scale(trans.scale.map(parseFloat));
                zoom.event(svg);
            }
         
            var link = vis.selectAll("line.link")
                .data(lin)
                .enter().append("line")
                    .attr("class", "link")
                    .attr("x1", function(d) { return d.x1; })
                    .attr("y1", function(d) { return d.y1; })
                    .attr("x2", function(d) { return d.x2; })
                    .attr("y2", function(d) { return d.y2; })
                    .style("stroke-width", function(d) { return Math.sqrt(d.value); })
                    .style({"stroke" : "#999", "stroke-opacity" : ".6"})
                    .style("display", function(d) {
                          var displ_edges = $('[id=graphui_display_edges]').val();
                          return(display_edge(displ_edges, d.edge_type) ? "" : "none");
                          });
         
            var node = vis.selectAll("circle.node")
                .data(nodes)
                .enter().append("circle")
                    .attr("class", function(d) {return (d.type == "1" ? "node node-landmark" : "node node-cluster"); })
                    .attr("r", function(d) {
                          var node_size = $('[id=graphui_node_size]').val();
                          if(node_size == "Proportional")
                            return(d.size);
                          else
                            return(d.type == "1" ? "8" : "5");
                          })
                    .attr("cx", function(d) { return d.X; })
                    .attr("cy", function(d) { return d.Y; })
                    .style("fill", function(d) { return d.color; }) //attr or style??
                   
                    //.on("click", function(d) {d.type == "1" ? Shiny.onInputChange("graphui_selected_landmark", d.name) : Shiny.onInputChange("graphui_selected_cluster", d.name)})
                    .on("click", function(d)
                        {
                            if(!d3.event.shiftKey) 
                            {
                                d3.selectAll( '.selected').classed( "selected", false);
                            }
                            d3.select(this).classed("selected", true);
                            var res = d3.selectAll(".selected").data().map(function(d) {return(d.name)});
                            Shiny.onInputChange("graphui_selected_nodes", res);
                        }
                    )
                    .on("mouseenter", function(d)
                        {
                            if(d.type != "1")
                            {
                                var target_edge = d.highest_scoring_edge;
                                links = d3.selectAll("line.link");
                                links.style("opacity", "0.06");
                                links[0][target_edge - 1].style.opacity = "1";
                            }
                        })
                    .on("mouseleave", function() {d3.selectAll("line.link").style("opacity", "1");})
                    ;
           
            var labels = vis.selectAll("text.label")
                .data(nodes)
                .enter().append("text")
                    .attr("class", function(d) {return(d.type == "1" ? "label-landmark" : "label-cluster"); })
                    .attr("x", function(d) { return d.X; })
                    .attr("y", function(d) { return d.Y; })
                    .text(function(d) {return d.name.replace(".fcs", "");})
                    .style("fill", function(d) {return(d.type == "1" ? "rgb(0,0,0)" : "rgb(200,200,200)");})
                    .style("font-size", function(d) {return(d.type == "1" ? "16" : "8");})
                    .style("opacity", function(d) {return(d.type == "1" ? "1" : "0.8");})
                    .style("display", function(d) {return(d.type == "1" ? "" : "none"); });               
         }
    });
Shiny.outputBindings.register(networkOutputBinding, 'networkbinding');

Shiny.addCustomMessageHandler("color_nodes",
    function(color)
    {
        //This is necessary to restore the data that is overwritten by
        //the color command
        var old_data = d3.selectAll(".node").data();
        d3.selectAll(".node")
            .data(color)
            .style("fill", function(d) {return d; });
        d3.selectAll(".node").data(old_data);

    }
);

Shiny.addCustomMessageHandler("reset_colors",
    function(value)
    {
        d3.selectAll(".node").style("fill", "");
    }
);

Shiny.addCustomMessageHandler("reset_graph_position",
    function(value)
    {
        d3.select("g").attr("transform", "");
    }
);

Shiny.addCustomMessageHandler("toggle_label",
    function(value)
    {
        var target = value.target == "cluster" ? ".label-cluster" : ".label-landmark";
        d3.selectAll(target).style("display", value.display);
    }
);


Shiny.addCustomMessageHandler("toggle_display_edges",
    function(value)
    {
        d3.selectAll(".link").style("display", function(d) {return(display_edge(value, d.edge_type) ? "" : "none")});
        /*if(value == "All")
            d3.selectAll(".link").style("display", "");
        else if(value == "Inter cluster")
            d3.selectAll(".link").style("display", function(d) {return(d.type == "inter_cluster" ? "" : "none")});
        else if(value == "To landmark")
            d3.selectAll(".link").style("display", function(d) {return((d.type == "cluster_to_landmark" || d.type == "highest_scoring") ? "" : "none")});
        else if(value == "Highest scoring")
            d3.selectAll(".link").style("display", function(d) {return(d.type == "highest_scoring" ? "" : "none")});*/
    }
);

Shiny.addCustomMessageHandler("get_selected_nodes",
    function(value)
    {
        var res = d3.selectAll(".selected").data().map(function(d) {return(d.name)});
        Shiny.onInputChange("graphui_selected_nodes", res);
    }
);


Shiny.addCustomMessageHandler("toggle_node_size",
    function(value)
    {
        if(value.display == "proportional")
            d3.selectAll("circle.node").attr("r", function(d) {return d.size;});
        else if(value.display == "default")
            d3.selectAll("circle.node").attr("r", function(d) {return d.type == "1" ? "8" : "5"});
    }
);


//</script>

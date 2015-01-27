function getScreenCoords(x, y, ctm) {
  var xn = ctm.e + x*ctm.a;
  var yn = ctm.f + y*ctm.d;
  return { x: xn, y: yn };
}


var make_draggable = function(svg, zoom)
{
    svg
.on( "mousedown", function() {
    if( !d3.event.shiftKey) {
        d3.selectAll( '.selected').classed( "selected", false);
        var res = d3.selectAll(".selected").data().map(function(d) {return(d.name)});
        Shiny.onInputChange("graphui_selected_nodes", res);
    }
    if(d3.event.altKey)
    {
        svg.call(zoom)
            .on("mousedown.zoom", null)
            .on("touchstart.zoom", null)
            .on("touchmove.zoom", null)
            .on("touchend.zoom", null);
        
        var p = d3.mouse( this);

        svg.append("rect")
            .attr({
                rx      : 6,
                ry      : 6,
                class   : "selection",
                x       : p[0],
                y       : p[1],
                width   : 0,
                height  : 0
            })
    }
})
.on( "mousemove", function() {
    var s = svg.select( "rect.selection");

    if( !s.empty()) {
        var p = d3.mouse( this),
            d = {
                x       : parseInt( s.attr( "x"), 10),
                y       : parseInt( s.attr( "y"), 10),
                width   : parseInt( s.attr( "width"), 10),
                height  : parseInt( s.attr( "height"), 10)
            },
            move = {
                x : p[0] - d.x,
                y : p[1] - d.y
            }
        ;

        if( move.x < 1 || (move.x*2<d.width)) {
            d.x = p[0];
            d.width -= move.x;
        } else {
            d.width = move.x;       
        }

        if( move.y < 1 || (move.y*2<d.height)) {
            d.y = p[1];
            d.height -= move.y;
        } else {
            d.height = move.y;       
        }
       
        s.attr( d);
        d3.selectAll( '.selected').classed( "selected", false);

        d3.selectAll('.node-cluster').each( function(node_data, i) {
            var cx = this.getAttribute('cx');
            var cy = this.getAttribute('cy');
            var radius = this.getAttribute('r');
            var ctm = this.getCTM();
            var coords = getScreenCoords(cx, cy, ctm);
            
            if( 
                //!d3.select( this).classed( "selected") && 
                    // inner circle inside selection frame
                //coords.x - radius >= d.x && coords.x + radius <= d.x + d.width && 
                //coords.y - radius >= d.y && coords.y + radius <= d.y + d.height
                coords.x >= d.x && coords.x <= d.x + d.width && 
                coords.y >= d.y && coords.y <= d.y + d.height
                
            ) {
                d3.select(this)
                .classed( "selected", true);
            }
        });
        var res = d3.selectAll(".selected").data().map(function(d) {return(d.name)});
        Shiny.onInputChange("graphui_selected_nodes", res);
    }
})
.on( "mouseup", function() {
       // remove selection frame
    svg.selectAll( "rect.selection").remove();

        // remove temporary selection marker class
    d3.selectAll( 'g.state.selection').classed( "selection", false);
    svg.call(zoom);
})
.on( "mouseout", function() {
    if( d3.event.relatedTarget.tagName=='HTML') {
            // remove selection frame
        svg.selectAll( "rect.selection").remove();

            // remove temporary selection marker class
        d3.selectAll( 'g.state.selection').classed( "selection", false);
    }
});
    return(svg);
};



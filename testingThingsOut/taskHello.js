// app
var lc = Elm.fullscreen(Elm.TaskHello);

lc.ports.runHello.subscribe(function(s) {
    console.log(s);
});


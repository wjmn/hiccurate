import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import juicebox from 'juicebox.js';
import '../node_modules/juicebox.js/dist/css/juicebox.css';


/************************************************************************/
/* JUICEBOX CONTAINER */ 
/************************************************************************/

var juiceboxContainer = document.getElementById('juicebox-container');

/************************************************************************/
/* ELM ENTRY POINT */ 
/************************************************************************/

var app = Elm.Main.init({
  node: document.getElementById('root')
});

/************************************************************************/
/* ELM PORTS */
/************************************************************************/

var hicBrowser = null;
var hicBrowserId = null;
var hicViewport = null;
var hicUpperNavbar = null;

// When Hi-C file is selected, load it into Hi-C browser
app.ports.sendHicLoadedFile.subscribe(file => {

  // Load the file into Hi-C browser
  juicebox.init(juiceboxContainer, {
    "url": file,
  }).then((browser) => {
    hicBrowser = browser;
    hicBrowserId = hicBrowser.id;
    hicViewport = document.getElementById(hicBrowserId+"-viewport");
    hicUpperNavbar = document.getElementById(hicBrowserId+"-upper-hic-nav-bar-widget-container");

    const sendMouseMoveData = function(e) {
      var rect = e.target.getBoundingClientRect();
      var relX = e.clientX - rect.left;
      var relY = e.clientY - rect.top;
      var state = hicBrowser.getSyncState();
      app.ports.receiveMouseMoveData.send({
        "relX": relX,
        "relY": relY,
        "rect": rect,
        "syncState": state,
      })
    };


    // Update mouse move data when mouse moves
    hicViewport.addEventListener("mousemove", sendMouseMoveData);
    hicUpperNavbar.addEventListener("click", function(e) {
      // pause slightly
      setTimeout(function() {
        app.ports.receiveMouseMoveData.send({
          "relX": 0,
          "relY": 0,
          "rect": hicViewport.getBoundingClientRect(),
          "syncState": hicBrowser.getSyncState(),
        })
      }, 10)
    });

    hicViewport.addEventListener("click", function(e) {
      // pause slightly
      setTimeout(function() {
        sendMouseMoveData(e)
      }, 10)});

    //   document.getElementsByClassName("hic-resolution-selector-container")[0].children[1].addEventListener("change", function(e) {
    //     var state = hicBrowser.getSyncState();
    //     setTimeout(function() {
    //       app.ports.receiveMouseMoveData.send({
    //       "relX": 0,
    //       "relY": 0,
    //       "rect": hicViewport.getBoundingClientRect(),
    //       "syncState": state,
    //     })}, 20);
    //   });

    // Send right click event to Elm
    hicViewport.addEventListener("contextmenu", function(e) {
      e.preventDefault();
      app.ports.rightClickedHicPosition.send({});
    });


  });
})


app.ports.sendNewHicBrowserSyncState.subscribe(newSyncState => {
  console.log(newSyncState);
  if (hicBrowser != null) {
    hicBrowser.syncState(newSyncState);
  };

  var rect = hicViewport.getBoundingClientRect();

  app.ports.receiveMouseMoveData.send({
    "relX": rect.width / 2,
    "relY": rect.height / 2,
    "rect": rect,
    "syncState": newSyncState,
  });
})


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

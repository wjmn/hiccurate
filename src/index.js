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

// When Hi-C file is selected, load it into Hi-C browser
app.ports.sendHicLoadedFile.subscribe(file => {

  // Load the file into Hi-C browser
  juicebox.init(juiceboxContainer, {
    "url": file,
  }).then((browser) => {
    hicBrowser = browser;
    hicBrowserId = hicBrowser.id;
    hicViewport = document.getElementById(hicBrowserId+"-viewport");

    // Update 
    hicViewport.addEventListener("mousemove", function(e) {
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
    });


  });
})


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

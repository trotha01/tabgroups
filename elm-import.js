document.addEventListener('DOMContentLoaded', function() {
  var app = Elm.Main.fullscreen();

  if (chrome.tabs) {
    // Subscribe to tab selection change
    chrome.tabs.onActivated.addListener(function(activeInfo) {
      chrome.tabs.captureVisibleTab(activeInfo.windowId, {}, function(img) {
        // undefined doesn't work with ports
        if (!img) {
          img = null;
        }
        // Send tab screenshot back to Elm
        app.ports.tabScreenshot.send({id: activeInfo.tabId, img: img});
      });
    });

    // Subscribe to tab remove
    chrome.tabs.onRemoved.addListener(function(activeInfo) {
      // Since this api doesn't give us the id of the removed tab,
      // we have to query for all of them again
      getAllTabs();
    });
  }

  // Subscribe to Elm getTabs port
  app.ports.getTabs.subscribe(function() {
    getAllTabs();
  });

  getAllTabs = function() {
    // If we're not running in a chrome extension
    // then return some sample results
    if (!chrome.tabs) {
      console.log("not in extenstion, sending example tabs");
      app.ports.tabs.send([
	{ title: "tab 1", url: "tab1.com", screenshot: "", id: "0"},
	{ title: "tab 2", url: "tab2.com", screenshot: "", id: "1"}
      ]);
      return
    }

    queryChromeForTabs();
  };


  // queryChromeForTabs is called from getAllTabs
  // and sends results back to Elm
  // TODO: return values instead?
  // TODO: get screenshot for each tab
  queryChromeForTabs = function() {
    chrome.tabs.query({}, function(tabs) {
      console.log(tabs);

      // append empty screenshot to each tab
      var newTabs = tabs.map(function(tab) {
        tab.screenshot = null;
        tab.position = {x: 10, y: 10};
        tab.drag = null;
        return tab;
      });

      app.ports.tabs.send(newTabs);
    });
  };

  ////// LOCAL STORAGE ////////////
  app.ports.saveModel.subscribe(function(model) {
    localStorage.setItem('model', JSON.stringify(model));
  });

  app.ports.getModel.subscribe(function() {
    model = localStorage.getItem('model');
    if (!model) {
      app.ports.savedModel.send(null);
      return;
    }
    model = JSON.parse(model);
    for (var id in model.tabGroups) {
      console.log("sending tabGroup", model.tabGroups[id]);
      app.ports.tabGroup.send(model.tabGroups[id]);
    }
  });

});

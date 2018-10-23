chrome.browserAction.onClicked.addListener(function(tab) {
  chrome.tabs.create({'url': chrome.runtime.getURL('index.html')}, function(tab) {
    // Tab opened.
  });
});

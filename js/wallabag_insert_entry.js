javascript:(function(){
    let server = "http://localhost:5001";
    fetch(`${server}/source`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ source: document.documentElement.outerHTML })
    })
    .then(response => response.json())
    .then(() => {
       location.href = 'org-protocol://wallabag?url=' +
         encodeURIComponent(location.href) +
         '&title=' + encodeURIComponent(document.title || "[untitled page]") +
         '&body=' + encodeURIComponent(function () {
           var html = "";
           if (typeof window.getSelection != "undefined") {
             var sel = window.getSelection();
             if (sel.rangeCount) {
               var container = document.createElement("div");
               for (var i = 0, len = sel.rangeCount; i < len; ++i) {
                 container.appendChild(sel.getRangeAt(i).cloneContents());
               }
               html = container.innerHTML;
             }
           } else if (typeof document.selection != "undefined") {
             if (document.selection.type == "Text") {
               html = document.selection.createRange().htmlText;
             }
           }
           var relToAbs = function (href) {
             var a = document.createElement("a");
             a.href = href;
             var abs = a.protocol + "//" + a.host + a.pathname + a.search + a.hash;
             a.remove();
             return abs;
           };
           var elementTypes = [['a', 'href'], ['img', 'src']];
           var div = document.createElement('div');
           div.innerHTML = html;
           elementTypes.map(function(elementType) {
             var elements = div.getElementsByTagName(elementType[0]);
             for (var i = 0; i < elements.length; i++) {
               elements[i].setAttribute(elementType[1], relToAbs(elements[i].getAttribute(elementType[1])));
             }
           });
           return div.innerHTML;
         }());
       })
    .catch(error => {
        console.error('Error sending source:', error);
    });
})();

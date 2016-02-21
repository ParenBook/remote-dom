window.RemoteDOM = {};
RemoteDOM.nodes = {};

RemoteDOM.root = document.createElement('div');
document.body.appendChild(RemoteDOM.root);

/* Functions */

RemoteDOM.registerNode = function(id, name) {
  RemoteDOM.nodes[id] = document.createElement(name);
};

RemoteDOM.registerTextNode = function(id, text) {
  RemoteDOM.nodes[id] = document.createTextNode(text);
};

RemoteDOM.prependChild = function(parent_id, child_id) {
  var parent = RemoteDOM.nodes[parent_id];
  var child = RemoteDOM.nodes[child_id];
  parent.insertBefore(child, parent.childNodes[0]);
};

RemoteDOM.appendChild = function(parent_id, child_id) {
  var parent = RemoteDOM.nodes[parent_id];
  var child = RemoteDOM.nodes[child_id];
  parent.appendChild(child);
};

window.RemoteDOM = {};
RemoteDOM.nodes = {};

RemoteDOM.root = document.createElement('div');
document.body.appendChild(RemoteDOM.root);
RemoteDOM.nodes[1] = RemoteDOM.root;

/* Functions */

RemoteDOM.registerNode = function(id, parent_id, name) {
  RemoteDOM.nodes[id] = document.createElement(name);
  RemoteDOM.appendChild(parent_id, id);
};

RemoteDOM.registerTextNode = function(id, parent_id, text) {
  RemoteDOM.nodes[id] = document.createTextNode(text);
  RemoteDOM.appendChild(parent_id, id);
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

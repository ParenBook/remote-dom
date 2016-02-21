(in-package :cl-user)
(defpackage remote-dom
  (:use :cl)
  ;; Document
  (:export :document
           :document-root
           :start
           :stop)
  ;; DOM API
  (:documentation "The remote-dom package."))
(in-package :remote-dom)

;;; Documents

(defclass document ()
  ((context :accessor document-context
            :initarg :context
            :type remote-js:buffered-context
            :documentation "A remote-js context.")
   (root :accessor document-root
         :initarg :root
         :initform (plump-dom:make-root)
         :type plump-dom:root
         :documentation "The root Plump node.")
   (id :initform 0
       :documentation "The ID counter.")
   (ids :initform (make-hash-table)
        :documentation "A map from Plump nodes to IDs."))
  (:documentation "The remote document."))

(defmethod initialize-instance :after ((document document) &key)
  ;; Create the context object
  (with-slots (context) document
    (setf context (remote-js:make-buffered-context
                   :callback #'(lambda (message)
                                 (declare (ignore message))
                                 nil)))))

;;; Document utilities

(defun js-eval (document js)
  "Evaluate JS remotely."
  (declare (type document document)
           (type string js))
  (remote-js:eval (document-context document) js))

;;; Lifecycle methods

(defparameter +startup-code+
  (list
   "var RemoteDOM = {}"
   "RemoteDOM.root = document.createElement('div')"
   "document.body.appendChild(RemoteDOM.root)"
   "RemoteDOM.nodes = {}"
   "RemoteDOM.registerNode = function(id, name) {
  RemoteDOM.nodes[id] = document.createElement(name)
}"
   "RemoteDOM.registerTextNode = function(id, text) {
  RemoteDOM.nodes[id] = document.CreateTextNode(text)
}"
   "RemoteDOM.prependChild = function(parent_id, child_id) {
  var parent = RemoteDOM.nodes[parent_id];
  var child = RemoteDOM.nodes[child_id];
  parent.insertBefore(child, parent.childNodes[0]);
}"
   "RemoteDOM.appendChild = function(parent_id, child_id) {
  var parent = RemoteDOM.nodes[parent_id];
  var child = RemoteDOM.nodes[child_id];
  parent.appendChild(child);
}"))

(defgeneric start (document)
  (:documentation "Start the remote document.")

  (:method ((document document))
    (with-slots (context) document
      ;; Start the server
      (remote-js:start context)
      ;; Create some client-side stuff
      (dolist (str +startup-code+)
        (remote-js:eval context str)))))

(defgeneric stop (document)
  (:documentation "Stop the remote document.")

  (:method ((document document))
    (with-slots (context) document
      (remote-js:stop context))))

;;; Node IDs

(defun next-id (document)
  "Increase the document's ID counter."
  (with-slots (id) document
    (incf id)))

(defun register-node (document node)
  "Register a node with a unique ID within the document, return the ID."
  (with-slots (ids) document
    (let ((id (next-id document)))
      (setf (gethash node ids) id)
      id)))

(defun deregister-node (document node)
  "Remove a node from the node-to-ID map."
  (with-slots (ids) document
    (remhash node ids)))

(defun node-id (document node)
  "Find the ID of a node in a document object. Raise an error if it's not
found."
  (with-slots (ids) document
    (or (gethash node ids) (error "Node not found in the document."))))

;;; Document API

(defun doc-make-node (document parent name &key attributes)
  "Create a node in a document and the client."
  (declare (type string name))
  (let ((node (plump-dom:make-element parent name :attributes attributes)))
    ;; Client-side
    (js-eval document
             (format nil "RemoteDOM.registerNode(~D, ~A)"
                     (register-node document node) name))
    node))

(defun doc-make-text-node (document parent text)
  "Create a text node in a document and the client."
  (declare (type string text))
  (let ((node (plump-dom:make-text-node parent text)))
    ;; Client-side
    (js-eval document
             (format nil "RemoteDOM.registerTextNode(~D, ~A)"
                     (register-node document node) text))
    node))

(defun doc-prepend-child (document parent child)
  "Prepend a child node to a parent node."
  ;; Server
  (plump-dom:prepend-child parent child)
  ;; Client side
  (js-eval document
           (format nil "RemoteDOM.prependChild(~D, ~D)"
                   (node-id document parent)
                   (node-id document child))))

(defun doc-append-child (document parent child)
  "Append a child node to a parent node."
  ;; Server
  (plump-dom:append-child parent child)
  ;; Client side
  (js-eval document
           (format nil "RemoteDOM.appendChild(~D, ~D)"
                   (node-id document parent)
                   (node-id document child))))

;;; DOM API

(defvar *document*)

(defun make-node (parent name &key attributes)
  "Create a DOM node."
  (declare (type string name))
  (doc-make-node *document* parent name :attributes attributes))

(defun make-text-node (parent text)
  "Create a text node."
  (declare (type string text))
  (doc-make-text-node *document* parent text))

(defun prepend-child (parent child)
  "Add a node to the beginning of the parent's children."
  (doc-prepend-child *document* parent child))

(defun append-child (parent child)
  "Add a node to the end of the paretn's children."
  (doc-append-child *document* parent child))

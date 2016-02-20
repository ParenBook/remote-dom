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

;;; Classes

(defclass document ()
  ((context :accessor document-context
            :initarg :context
            :type remote-js:buffered-context
            :documentation "A remote-js context.")
   (root :accessor document-root
         :initarg :root
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

(defgeneric start (document)
  (:documentation "Start the remote document.")

  (:method ((document document))
    (with-slots (context) document
      ;; Start the server
      (remote-js:start context)
      ;; Create some client-side stuff
      (dolist (str (list "var RemoteDOM = {}"
                         "RemoteDOM.ids = {}"))
        (remote-js:eval context str)))))

(defgeneric stop (document)
  (:documentation "Stop the remote document.")

  (:method ((document document))
    (with-slots (context) document
      (remote-js:stop context))))

(defvar *document*)

;;; Internal

(defun next-id (document)
  "Increase the document's ID counter."
  (with-slots (id) document
    (incf id)))

(defun register-node (document node)
  "Register a node with a unique ID within the document."
  (with-slots (ids) document
    (setf (gethash node ids) (next-id document))))

(defun deregister-node (document node)
  "Remove a node from the node-to-ID map."
  (with-slots (ids) document
    (remhash node ids)))

;;; DOM API

(defun append-child (parent child)
  (plump-dom:append-child parent child))

(defun preprend-child (parent child)
  (plump-dom:prepend-child parent child))

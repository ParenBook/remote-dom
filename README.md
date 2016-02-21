# remote-dom

Control a browser DOM from Common Lisp.

# Overview

# Usage

## Example

First things first: Quickload

```lisp
(ql:quickload :remote-dom)
```

First, create a document object:

```lisp
(defvar doc (remote-dom:make-document))
```

Then, start the WebSockets server:

```lisp
(remote-dom:start doc)
```

Now, run this code. This will create an HTML file (`~/test.html`) to act as the
client to our DOM:

```lisp
(with-open-file (stream (merge-pathnames #p"test.html" (user-homedir-pathname))
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (write-string (remote-js:html (remote-dom::document-context doc)) stream))
```

Now, open `~/test.html~`. Then run this:

```lisp
(let* ((remote-dom:*document* doc)
       (root (remote-dom:document-root doc))
       (text (remote-dom:make-text-node root "Hello, world!"))))
```

And you should see the string "Hello, world!" appear in an otherwise empty page.

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.

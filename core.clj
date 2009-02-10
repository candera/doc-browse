;; Copyright (c) 2009 Wangdera Corporation candera@wangdera.com

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; TODO
;; 
;; * Figure out why the source doesn't show up for most things
;; * Add collapse/expand functionality
;; * Remove the whojure dependency
;;
;; DONE
;;
;; * Add collapsible source
;; * Add links at the top to jump to each namespace
;; * Add object type (var, function, whatever)
;; * Add argument lists for functions
;; * Add links at the top of each namespace to jump to members
;; * Add license statement

(ns com.wangdera.doc-browse.core
  (:require [clojure.contrib.duck-streams :as duck-streams])
  (:use [clojure.contrib seq-utils str-utils repl-utils def])
  (:import [java.lang Exception]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML generation stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *xml-event-factory* nil) 

(defmacro- write-to-xml-writer [xml-writer & args]
  `(. ~xml-writer (add (. *xml-event-factory* ~@args))))

(defn- write-start-element [xml-writer element-name]
  (write-to-xml-writer xml-writer (createStartElement "" "" element-name)))

(defn- write-end-element [xml-writer element-name]
  (write-to-xml-writer xml-writer (createEndElement "" "" element-name)))

(defn- write-attribute [xml-writer name value]
  (write-to-xml-writer xml-writer (createAttribute name value)))

(defn- write-text [xml-writer text]
  (write-to-xml-writer xml-writer (createCharacters text)))

(defn- strcat 
  ([] (str))
  ([x y]
     (. x (concat y)))
  ([x y & zs]
     (strcat (strcat x y) (reduce strcat zs))))

(defn- item-type 
  ([item] 
     (item-type nil item))
  
  ([x item]
     (cond (string? item) :string
	   (keyword? item) :keyword
	   (map? item) :map
	   (coll? item) :collection
	   :else :unknown)))

(defmulti html 
  item-type) 

(defmethod html :string 
  ([xml-writer item]
     (write-text xml-writer item)))

(defmethod html :collection 
  ([xml-writer item]
     (let [element-name (name (first item))]
       (write-start-element xml-writer element-name)
       (dorun (map #(html xml-writer %) (rest item)))
       (write-end-element xml-writer element-name))))

(defmethod html :map 
  ([xml-writer item]
     (dorun (map #(write-attribute xml-writer (name (first %)) (second %)) item))))

(defmethod html :keyword 
  ([xml-writer item]
     (write-start-element xml-writer (name item))))

(defmethod html :unknown 
  ([xml-writer item]))

(defn- create-xml-writer [stream]
  (let [output-factory (. javax.xml.stream.XMLOutputFactory (newInstance))]
    (. output-factory (createXMLEventWriter stream))))
    
(defn- create-xml-event-factory []
  (. javax.xml.stream.XMLEventFactory (newInstance)))

(defn- html-output-to-string 
  "Creates a new XML Writer over a StringWriter and calls html
on it, then calls toString on the StringWriter, returning the 
resulting string." 
  [expr]
  (let [stream (new java.io.StringWriter) 
	xml-writer (create-xml-writer stream)]
     (binding [*xml-event-factory* (create-xml-event-factory)]
       (html xml-writer expr)
       (. xml-writer (flush))
       (. xml-writer (close))
       (. stream (toString)))))

(defn- with-html-output [xml-writer expr]
  (binding [*xml-event-factory* (create-xml-event-factory)]
    (html xml-writer expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doc generation constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *script* " // <![CDATA[

        function toggleSource( id )
        {
          var elem
          var link

          if( document.getElementById )
          {
            elem = document.getElementById( id )
            link = document.getElementById( \"linkto-\" + id )
          }
          else if ( document.all )
          {
            elem = eval( \"document.all.\" + id )
            link = eval( \"document.all.linkto-\" + id )
          }
          else
            return false;

          if( elem.style.display == \"block\" )
          {
            elem.style.display = \"none\"
            link.innerHTML = \"Show Source\"
          }
          else
          {
            elem.style.display = \"block\"
            link.innerHTML = \"Hide Source\"
          }
        }

      // ]]>
")

(def *style* ".library-member-source
{
  display: none
}
.library-member-docs
{
  font-family:monospace
}
.library-member-arglists
{
  font-family: monospace
}
.library-member-type
{
  font-weight: bold; 
  font-size: small;
  font-style: italic;
  color: darkred
}
.lib-links
{
  margin: 0 0 1em 0
}

.lib-link-header
{
  color: white;
  background: darkgreen;
  width: 100%
}

.library-name 
{ 
  color: white;
  background: darkblue;
  width: 100%
}

.missing-library
{
  color: darkred; 
  margin: 0 0 1em 0 
}

.library-members
{
  list-style: none
}

.library-member-name
{
  font-weight: bold
}")

(defn- extract-documentation [v]
  (if-let [docs (:doc (meta v))]
    (map (fn [l] [:div {:class "library-member-doc-line"} l]) (re-split #"\n" docs)) 
    ""))

(defn- member-type [x]
  (try 
   (let [dx (deref x)] 
     (cond 
      (:macro (meta x)) :macro 
      (fn? dx) :fn 
      (= clojure.lang.MultiFn (:tag (meta x))) :multi 
      true :var))
   (catch Exception e
     :unknown)))

(defn- anchor-for-member [libid memberid]
  (str "member-" libid "-" memberid))


(defn- id-for-member-source [libid memberid]
  (str "membersource-" libid "-" memberid))

(defn- id-for-member-source-link [libid memberid]
  (str "linkto-membersource-" libid "-" memberid))

(defn- format-source [libid memberid]
  (try
   (get-source memberid)
   (catch Exception ex
     nil)))

(defn- generate-lib-member [libid [n v]]
  [:li {:class "library-member"}
   [:a {:name (anchor-for-member libid n)}]
   [:dl {:class "library-member-table"} 
    [:dt {:class "library-member-name"}
     (str n)]
    [:dd 
     [:div {:class "library-member-info"}
      [:span {:class "library-member-type"} (name (member-type v))]
      " "
      [:span {:class "library-member-arglists"} (str (:arglists (meta v)))]]
     (into [:div {:class "library-member-docs"}] (extract-documentation v))
     (let [member-source-id (id-for-member-source libid n)
	      member-source-link-id (id-for-member-source-link libid n)]
       (if-let [member-source (format-source libid n)] 
	 [:div {:class "library-member-source-section"}
	  [:div {:class "library-member-source-toggle"}
	   [:a {:href (format "javascript:toggleSource('%s')" member-source-id)
		:id member-source-link-id} "Show Source"]]
	  [:div {:class "library-member-source" :id member-source-id}
	   [:pre member-source]]]))]]])

(defn- anchor-for-library [id]
  (str "library-" id))

(defn- generate-lib-member-link [libid [n v]]
  [:a {:class "lib-member-link" :href (str "#" (anchor-for-member libid n))} (name n)])

(defn- generate-lib-doc [lib]
  (let [ns (find-ns lib)]
    (if ns 
      (let [lib-members (sort (ns-publics ns))]
	[:div {:class "library"}
	 [:a {:name (anchor-for-library lib)}]
	 [:div {:class "library-name"} (str (ns-name ns))]
	 (into [:div {:class "library-member-links"}]
	       (interpose " " (map #(generate-lib-member-link lib %) lib-members)))
	 (into [:ol {:class "library-members"}]
	       (map #(generate-lib-member lib %) lib-members))])
      [:div {:class "missing-library"}
        [:div {:class "library-name"} (name lib)] "Could not load library"])))

(defn- load-lib [lib]
  (try 
   (require lib)
   (catch java.lang.Exception x
       nil)))

(defn- generate-lib-link [lib]
  (let [ns (find-ns lib)]
    (if ns
      [:a {:class "lib-link" :href (str "#" (anchor-for-library lib))} (str (ns-name ns))])))

(defn- generate-lib-links [lib-vec]
  (into [:div {:class "lib-links"} 
	 [:div {:class "lib-link-header"} "Namespaces"]] 
	(interpose " " (map generate-lib-link lib-vec))))

(defn generate-documentation [libs]
  "Returns a string which is the HTML documentation for the libraries
named by libs. Libs is a vector of symbols identifying Clojure
libraries."
  (dorun (map load-lib libs))
  (html-output-to-string 
   [:html {:xmlns "http://www.w3.org/1999/xhtml"}
    [:head 
     [:title "Clojure documentation browser"]
     [:style *style*]
     [:script {:language "JavaScript" :type "text/javascript"} *script*]]
    (let [lib-vec (sort libs)] 
      (into [:body (generate-lib-links lib-vec)]
	    (map generate-lib-doc lib-vec)))]))

(defn generate-documentation-to-file [path libs]
  "Calls generate-documentation on the libraries named by libs and
emits the generated HTML to the path named by path."
  (duck-streams/spit path (generate-documentation libs)))

(comment 
  (generate-documentation-to-file 
   "C:/TEMP/CLJ-DOCS.HTML"
   ['clojure.contrib.accumulators])

  (generate-documentation-to-file 
   "C:/temp/clj-docs.html"
   [
    'clojure.set
    'clojure.main 
    'clojure.core  
    'clojure.zip   
    'clojure.xml
    'clojure.contrib.accumulators
    'clojure.contrib.apply-macro
    'clojure.contrib.auto-agent
    'clojure.contrib.combinatorics
    'clojure.contrib.command-line
    'clojure.contrib.cond
    'clojure.contrib.condt
    'clojure.contrib.def
    'clojure.contrib.duck-streams
    'clojure.contrib.enum
    'clojure.contrib.error-kit
    'clojure.contrib.except
    'clojure.contrib.fcase
    'clojure.contrib.import-static
    'clojure.contrib.javadoc
    'clojure.contrib.javalog
    'clojure.contrib.lazy-seqs
    'clojure.contrib.lazy-xml
    'clojure.contrib.macros
    'clojure.contrib.math
    'clojure.contrib.miglayout
    'clojure.contrib.mmap
    'clojure.contrib.monads
    'clojure.contrib.ns-utils
    'clojure.contrib.prxml
    'clojure.contrib.repl-ln
    'clojure.contrib.repl-utils
    'clojure.contrib.seq-utils
    'clojure.contrib.server-socket
    'clojure.contrib.shell-out
    'clojure.contrib.sql
    'clojure.contrib.stacktrace
    'clojure.contrib.stream-utils
    'clojure.contrib.str-utils
    'clojure.contrib.template
    'clojure.contrib.test-clojure
    'clojure.contrib.test-contrib
    'clojure.contrib.test-is
    'clojure.contrib.trace
    'clojure.contrib.walk
    'clojure.contrib.zip-filter
    'clojure.contrib.javadoc.browse
    'clojure.contrib.json.read
    'clojure.contrib.json.write
    'clojure.contrib.lazy-xml.with-pull
    'clojure.contrib.miglayout.internal
    'clojure.contrib.probabilities.dist
    'clojure.contrib.probabilities.dist.examples
    'clojure.contrib.sql.internal
    'clojure.contrib.test-clojure.evaluation
    'clojure.contrib.test-clojure.for
    'clojure.contrib.test-clojure.numbers
    'clojure.contrib.test-clojure.printer
    'clojure.contrib.test-clojure.reader
    'clojure.contrib.test-clojure.sequences
    'clojure.contrib.test-contrib.shell-out
    'clojure.contrib.test-contrib.str-utils
    'clojure.contrib.zip-filter.xml
    ])
  )
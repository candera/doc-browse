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
;; * Remove doc strings from source code
;; * Add collapse/expand functionality for each namespace
;; * Add collapse/expand functionality for all namespaces
;; * Move to clojure.contrib
;;   * Change namespace
;;   * Change license as appropriate
;;   * Double-check doc strings
;;
;; DONE
;;
;; * See if converting to use clojure.contrib.prxml is possible
;; * Figure out why the source doesn't show up for most things
;; * Add collapsible source
;; * Add links at the top to jump to each namespace
;; * Add object type (var, function, whatever)
;; * Add argument lists for functions
;; * Add links at the top of each namespace to jump to members
;; * Add license statement
;; * Remove the whojure dependency

(ns com.wangdera.doc-browse.core
  (:require [clojure.contrib.duck-streams :as duck-streams])
  (:use [clojure.contrib seq-utils str-utils repl-utils def prxml])
  (:import [java.lang Exception]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doc generation constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *script* " // <![CDATA[

function toggleSource( id )
{
  toggle(id, 'linkto-' + id, 'Show Source', 'Hide Source')
}

function toggle(targetid, linkid, textWhenOpen, textWhenClosed)
{
  var elem
  var link

  if( document.getElementById )
  {
    elem = document.getElementById( targetid )
    link = document.getElementById( linkid )
  }
  else if ( document.all )
  {
    elem = eval( \"document.all.\" + targetid )
    link = eval( \"document.all.\" + linkid )
  }
  else
    return false;

  if( elem.style.display == \"none\" )
  {
    elem.style.display = \"block\"
    link.innerHTML = textWhenClosed
  }
  else
  {
    elem.style.display = \"none\"
    link.innerHTML = textWhenOpen
  }
}

      // ]]>
")

(def *style* ".library
{
  padding: 0.5em 0 0 0 
}
.library-contents-toggle
{
 font-size: small;
}
.library-contents-toggle a
{
 color: white
}
.library-member-doc-whitespace
{
 white-space: pre
}
.library-member-source-toggle
{
  font-size: small;
  margin-top: 0.5em
}
.library-member-source
{
  display: none;
  border-left: solid lightblue 
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
  font-weight: bold;
  font-size: 105%
}")

(defn- extract-documentation [v]
  (if-let [docs (:doc (meta v))]
    (map 
     (fn [l] 
       [:div {:class "library-member-doc-line"} 
	(if (= 0 (count l)) 
	  [:span {:class "library-member-doc-whitespace"} " "] ; We need something here to make the blank line show up
	  l)]) 
     (re-split #"\n" docs)) 
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
   (if-let [ns (find-ns libid)]
     (get-source (symbol (name (ns-name ns)) (name memberid))))
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
	   "[ "
	   [:a {:href (format "javascript:toggleSource('%s')" member-source-id)
		:id member-source-link-id} "Show Source"]
	   " ]"]	  
	  [:div {:class "library-member-source" :id member-source-id}
	   [:pre member-source]]]))]]])

(defn- anchor-for-library [id]
  "Given a symbol id identifying a namespace, returns an identifier
suitable for use as the name attribute of an HTML anchor tag."
  (str "library-" id))

(defn- generate-lib-member-link [libid [n v]]
  "Emits a hyperlink to a member of a namespace given libid (a symbol
identifying the namespace) and the vector [n v], where n is the symbol
naming the member in question and v is the var pointing to the
member." 
  [:a {:class "lib-member-link" 
       :href (str "#" (anchor-for-member libid n))} (name n)])

(defn- anchor-for-library-contents [lib]
  "Returns an HTML ID that identifies the element that holds the
  documentation contents for the specified library."
  (str "library-contents-" lib))

(defn- anchor-for-library-contents-toggle [lib]
  "Returns an HTML ID that identifies the element that toggles the
visibility of the library contents."
  (str "library-contents-toggle-" lib))

(defn- generate-lib-doc [lib]
  "Emits the HTML that documents the namespace identified by the
symbol lib."
  [:div {:class "library"} 
   [:div {:class "library-name"} 
    [:span {:class "library-contents-toggle"} 
     "[ "
     [:a {:id (anchor-for-library-contents-toggle lib) 
	  :href (format "javascript:toggle('%s', '%s', '+', '-')" 
			(anchor-for-library-contents lib)
			(anchor-for-library-contents-toggle lib))} 
      "-"]
     " ] "]
    (name lib)]
   (let [ns (find-ns lib)]
     (if ns 
       (let [lib-members (sort (ns-publics ns))]
	 [:a {:name (anchor-for-library lib)}]
	 [:div {:class "library-contents" :id (anchor-for-library-contents lib)}
	  (into [:div {:class "library-member-links"}]
		(interpose " " (map #(generate-lib-member-link lib %) lib-members)))
	  (into [:ol {:class "library-members"}]
		(map #(generate-lib-member lib %) lib-members))])
       [:div {:class "missing-library library-contents" :id (anchor-for-library-contents lib)} "Could not load library"]))])

(defn- load-lib [lib]
  "Calls require on the library identified by lib, eating any
exceptions."
  (try 
   (require lib)
   (catch java.lang.Exception x
       nil)))

(defn- generate-lib-link [lib]
  "Generates a hyperlink to the documentation for a namespace given
lib, a symbol identifying that namespace."
  (let [ns (find-ns lib)]
    (if ns
      [:a {:class "lib-link" :href (str "#" (anchor-for-library lib))} (str (ns-name ns))])))

(defn- generate-lib-links [libs]
  "Generates the list of hyperlinks to each namespace, given libs, a
vector of symbols naming namespaces."
  (into [:div {:class "lib-links"} 
	 [:div {:class "lib-link-header"} "Namespaces"]] 
	(interpose " " (map generate-lib-link libs))))

(defn generate-documentation [libs]
  "Returns a string which is the HTML documentation for the libraries
named by libs. Libs is a vector of symbols identifying Clojure
libraries."
  (dorun (map load-lib libs))
  (let [writer (new java.io.StringWriter)]
   (binding [*out* writer] 
     (prxml 
      [:html {:xmlns "http://www.w3.org/1999/xhtml"}
       [:head 
	[:title "Clojure documentation browser"]
	[:style *style*]
	[:script {:language "JavaScript" :type "text/javascript"} [:raw! *script*]]]
       (let [lib-vec (sort libs)] 
	 (into [:body (generate-lib-links lib-vec)]
	       (map generate-lib-doc lib-vec)))]))
   (.toString writer)))


(defn generate-documentation-to-file [path libs]
  "Calls generate-documentation on the libraries named by libs and
emits the generated HTML to the path named by path."
  (duck-streams/spit path (generate-documentation libs)))

(comment 
  (generate-documentation-to-file 
   "C:/TEMP/CLJ-DOCS.HTML"
   ['clojure.contrib.accumulators])

  (defn gen-all-docs [] 
    (generate-documentation-to-file 
     "C:/temp/clj-libs.html"
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
     ]))
  )
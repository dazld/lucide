(ns lucide.core
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clojure.string :as str]
            [clojure.pprint :as pp]))


;; Helper function to convert kebab-case to camelCase for attribute names
(defn- kebab->camel [s]
  (str/replace s #"-(.)" (fn [[_ c]] (str/upper-case c))))

;; Process attributes map to convert from XML format to Hiccup format
(defn- process-attrs [attrs]
  (reduce-kv
   (fn [m k v]
     (let [key-name (name k)
           ;; Handle special cases for SVG attributes (namespace issues)
           clean-key (cond
                       (str/starts-with? key-name "xmlns:") key-name
                       (= key-name "xmlns") key-name
                       :else (keyword (kebab->camel key-name)))]
       (cond
         (= key-name "width") (assoc m clean-key `(~'or ~'width "24"))
         (= key-name "height") (assoc m clean-key `(~'or ~'height "24"))
         :else (assoc m clean-key v))))
   {}
   attrs))

;; Convert XML element to Hiccup format
(defn- xml->hiccup [element]
  (if (string? element)
    element
    (let [{:keys [tag attrs content]} element
          hiccup-tag (keyword (name tag))
          hiccup-attrs (process-attrs attrs)
          hiccup-content (map xml->hiccup content)]
      (into [hiccup-tag hiccup-attrs] hiccup-content))))

;; Process a single SVG file and return its Hiccup representation
(defn process-svg-file [file]
  (let [parsed (xml/parse file)
        hiccup (xml->hiccup parsed)
        icon-name (-> (.getName file)
                      (str/replace #"\.svg$" "")
                      (str/replace #"-" "-"))]
    {:name icon-name
     :hiccup hiccup}))

;; Generate a Clojure function definition for an icon
(defn generate-icon-function [{:keys [name hiccup]}]
  `(~'defn ~(symbol name)
     ([] (~(symbol name) {}))
     ([~'{:keys [width height]}]
      ~hiccup)))

;; Main function to process all SVGs in a directory and generate output
(defn convert-svg-directory [input-dir output-file]
  (let [svg-files (->> (io/file input-dir)
                       file-seq
                       (filter #(str/ends-with? (.getName %) ".svg"))
                       (sort))
        icons (mapv process-svg-file svg-files)
        ns-form `(ns lucide.icons)
        def-forms (map generate-icon-function icons)]
    (with-open [writer (io/writer output-file)]
      (binding [*out* writer]
        (pp/pprint ns-form)
        (doseq [form def-forms]
          (pp/pprint form))))))

;; Example usage:
;; (convert-svg-directory "path/to/lucide/icons" "src/lucide/icons.clj")

;; Function to test with a single SVG file
(defn test-with-file [file-path]
  (let [result (process-svg-file (io/file file-path))]
    (pp/pprint (generate-icon-function result))))

(comment
  (convert-svg-directory "../../icons" "src/lucide/icons.clj")
  (test-with-file "../../icons/x.svg"))

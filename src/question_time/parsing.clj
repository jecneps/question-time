(ns question-time.parsing
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.data.json :as json]
            [clojure.core.match :refer [match]]
            [net.cgrand.enlive-html :refer [select text texts] :as enlive]))

(def ftext (comp text first))

(defn dotted-tag [tag-name]
  (enlive/pred #(= (:tag %) tag-name)))

(def talker-selector [#{:question :answer} :> (dotted-tag :talk.start) :> :talker])

(defn parse-talker [node]
  {:name (ftext (select node [:name]))
   :electorate (ftext (select node [:electorate]))
   :party (ftext (select node [:party]))
   :name-id (ftext (select node [(dotted-tag :name.id)]))})

(defn talk-paragraph [node]
  (let [a (first (select node [:a]))
        name-id (get-in a [:attrs :href])]
    (match [(get-in a [:attrs :type]) (seq (select node [:span.HPS-GeneralIInterjecting]))]
      ["MemberAnswer" _] {:type :start :name-id name-id :text (text node)}
      ["MemberQuestion" _] {:type :start :name-id name-id :text (text node)}
      ["MemberInterjecting" _] {:type :member-interjection :name-id name-id :text (text node)}
      ["MemberContinuation" _] {:type :continuation :name-id name-id :text (text node)}
      [nil nil] {:type :formating-break :text (text node)}
      [nil _] {:type :general-interjection :text (text node)})))

(defn qa [q]
  {:type :q|a
   :plaintext (text (first (select q [:body])))
   :speaker (parse-talker (first (select q talker-selector)))
   :turns (map talk-paragraph (select q [:body :p]))})


(defn hansard->metadata [hansard]
  {:date (ftext (select hansard [(dotted-tag :session.header) :date]))
   :chamber (ftext (select hansard [(dotted-tag :session.header) :chamber]))})

(defn qwn? [node]
  (and (= :debate (node :tag))
       (= "QUESTIONS WITHOUT NOTICE" (get-in node [:content 0 :content 0 :content 0]))))

(defn subdebate->qa-pairs [node]
  (->> (:content node)
       (drop 2)
       (partition 2)))

(defn parse-subdebate [node]
  (let [q&a-pairs (subdebate->qa-pairs node)]
    {:type :subdebate
     :title (ftext (select node [:subdebateinfo :title]))
     :q&as (map (fn [[q a]] {:type :q&a :question (qa q) :answer (qa a)}) q&a-pairs)}))

(defn join-turn-text [m]
  (clojure.string/join "\n\n" (map :text (:turns m))))


(defn hansard->data [hansard]
  (let [subdebates (->> (select hansard [(enlive/pred qwn?)])
                        (mapcat #(select % [(dotted-tag :subdebate.1)]))
                        (map parse-subdebate))]
    {:type :hansard
     :metadata (hansard->metadata hansard)
     :subdebates subdebates}))

(defmulti stringify :type)

(defmethod stringify :q|a [q|a] (clojure.string/join "\n\n" (map :text (:turns q|a))))
(defmethod stringify :q&a [q&a] (str (stringify (:question q&a)) "\n\n" (stringify (:answer q&a))))
(defmethod stringify :subdebate
  [subdebate]
  (str "## " (:title subdebate) "\n\n" (clojure.string/join "\n\n"
                                        ;; "\n\n:hiccup [:hr {:style {:border \"solid #ff6300 1px\"}}]\n\n"
                                                            (map stringify (:q&as subdebate)))))
(defmethod stringify :hansard [hansard] (str "# "
                                             (get-in hansard [:metadata :chamber])
                                             " "
                                             (get-in hansard [:metadata :date])
                                             "\n\n"
                                             (clojure.string/join "\n\n" (map stringify (:subdebates hansard)))))


(defn questions [hansard]
  (->> hansard
	   :subdebates
	   (mapcat :q&as)
	   (map :question)))

(defn zip-file [filename]
  (zip/xml-zip (xml/parse (io/file filename))))


(def x (xml/parse (io/file "resources/hansards/Senate_2024_02_06_Official.xml")))


(defn questions->label-studio [questions]
  (->> (map (fn [q]
              {:text (:plaintext q)})
            questions)
       (into [])
       (json/write-str)))

(defn hansard->markdown [path]
  (->> (io/file path)
       xml/parse
       hansard->data
       stringify
       (spit (clojure.string/replace path #"\.xml$" ".md"))))


(defn path->hansard-data [path]
  (->> (io/file path)
	   xml/parse
	   hansard->data))
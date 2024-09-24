(ns question-time.llm
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clj-wrap-indent.core :as wrap]))


(def OPEN_AI_URL "https://api.openai.com/v1/chat/completions")

(def gpt4 "gpt-4-turbo-preview")

(def Test-Q "Senator BIRMINGHAM (South Australia—Leader of the Opposition in the Senate) (14:00):  My question is to the minister representing the Prime Minister, Senator Wong. Minister, rather than using the weasel words we've become so accustomed to from Labor ministers of, 'We have no plans,' which the Australian public now know will precipitate yet another Labor broken promise, can you, and will the Prime Minister, categorically rule out any change or changes that limit the use of negative gearing or franking credits by Australians?")

(defn api-request [model prompt]
  (json/write-str {"model" model
                   "messages" [{"role" "system"
                                "content" prompt}]}))

(defn completion-text [data]
  (get-in data ["choices" 0 "message" "content"]))

(defn chat-completion [model prompt]
  (let [body (api-request model prompt)
        resp (client/post OPEN_AI_URL
                          {:content-type :json
                           :oauth-token (System/getenv "OPEN_AI_API_KEY")
                           :body body})
        resp-body (json/read-str (:body resp))] 
    {:req-body body
     :resp-body resp-body
   	 :completion-text (completion-text resp-body)}))

(defn prompt->text [prompt]
  (let [completion (chat-completion gpt4 prompt)]
	(get completion :completion-text)))

(def highlight-format-prompts {:terminal "raw terminal color code syntax (do not add extra escape slashes, use this format \u001b[31m)"
                     :custom-annotation ""
                     :markdown "markdown syntax"})

(defn question-highlighting-prompt [prompt-path q-text color highlight-format]
  (-> (slurp (str "resources/prompts/questions/" prompt-path))
      (format color (highlight-format-prompts highlight-format) q-text)))

(defn resp->category [resp]
  (let [x (.toUpperCase resp)]
    (cond
      (= x "CLEAR") :clear
      (= x "CLEAR + RHETORIC") :clear+rhetoric
      (= x "CLEAR WITH REPHRASING") :clear-with-rephrasing
      (= x "AMBIGUOUS") :ambiguous
      (= x "NULL") :null
      :else (throw (Exception. (str "Malformed category string: '" resp "'"))))))

(defn categorize-question [text]
  (-> (slurp "resources/prompts/questions/question_category.txt")
      (format text)
      prompt->text
      resp->category))

(defn judge-question [text anno-format]
  (let [category (categorize-question text)]
    (case category
      :clear {:category category
              :highlighted-q (prompt->text (-> (slurp "resources/prompts/questions/highlight_clear_question.txt") 
                                               (format "green" (highlight-format-prompts anno-format) text)))}
      :clear+rhetoric {:category category
                       :highlighted-q (prompt->text
                                       (-> (slurp "resources/prompts/questions/highlight_rhetoric.txt")
                                           (format "green" "yellow" (highlight-format-prompts anno-format) text)))}
      :clear-with-rephrasing {:category category
                              :highlighted-q (question-highlighting-prompt "highlight_question_syntactic.txt" text "yellow" anno-format)}
      :ambiguous {:category category
                  :highlighted-q (question-highlighting-prompt "highlight_question_syntactic.txt" text "orange" anno-format)}
      :null {:category category
             :highlighted-q (question-highlighting-prompt "highlight_question_syntactic.txt" text "red" anno-format)})))

(defn print-judgement [judgement]
  (println)
  (println (str "Category: " (:category judgement)))
  (wrap/println (:highlighted-q judgement) 80))



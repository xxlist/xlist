#!/usr/bin/env bb

(require '[babashka.fs :as fs])
(require '[babashka.http-client :as http])
(require '[taoensso.timbre :as log])

(set! *warn-on-reflection* true)

(defn jitted-delay
  "Return delay time that is jitted"
  [delay-ms jitter-ms]
  (-> (* 2 jitter-ms) inc rand-int (- jitter-ms) (+ delay-ms)))

(defn retry
  "Retry (apply f args) according to [retry-config]"
  [{:keys [retries delay-ms jitter-ms] :or {retries 0 delay-ms 0 jitter-ms 0} :as retry-config}
   f & args]
  (let [[ok-or-err value] (try [:ok (apply f args)]
                               (catch Exception e
                                 (if (<= retries 0)
                                   (throw e)
                                   [:err e])))]
    (case ok-or-err
      :ok value
      :err (let [sleep-ms
                 (max 0 (jitted-delay delay-ms jitter-ms))]
             (log/error "Error:" (ex-message value) "(Remaining retries:" retries ", will retry it after" sleep-ms "ms)")
             (Thread/sleep sleep-ms)
             (recur (update retry-config :retries dec) f args)))))

;; === Model [Info] ===

(defrecord Info [^String code
                 ^String title
                 ^String publish-date
                 ^String home-url
                 ^String cover-url
                 ^String preview-url
                 ^String play-url
                 ^boolean has-chinese-subtitle
                 ^boolean has-english-subtitle
                 ^boolean has-uncensored-leak])

;; === Convert [Info] list to Markdown Table ===

(defn info-keys
  "Returns keys of [Info]"
  []
  [:code :cover-url :title :publish-date])

(defn ^String info-markdown-table-header
  "Generate markdown table header for [Info]"
  []
  (str "|"
       (str/join "|" (info-keys))
       "|"))

(defn ^String info-markdown-table-seperator-line
  "Generate markdown table seperator line for [Info]"
  []
  (str "|"
       (->>
        (repeat (count (info-keys)) ":----:")
        (str/join "|"))
       "|"))

(defmulti ^String info-field-value-to-markdown-table-cell
  "Format value of [field-key] of [Info] to markdown table cell"
  (fn [field-key info] field-key))

(defmethod ^String info-field-value-to-markdown-table-cell :default
  [field-key info]
  (get info field-key ""))

;; "Format value of [code] of [Info] to markdown table cell"
(defmethod ^String info-field-value-to-markdown-table-cell :code
  [_ info]
  (format "<a href=\"%s\">%s</a>"
          (get info :home-url "")
          (get info :code "")))

;; "Format value of [cover-url] of [Info] to markdown table cell"
(defmethod ^String info-field-value-to-markdown-table-cell :cover-url
  [_ info]
  (format "[<img src=\"%s\">](%s)"
          (get info :cover-url "")
          (get info :play-url "")))

(defn ^String info->markdown-table-row
  "Convert [Info] to markdown table row"
  [^Info info]
  (str "| "
       (->> (info-keys)
            (map #(info-field-value-to-markdown-table-cell % info))
            (str/join " | "))
       " |"))

(defn ^String info-list->markdown-table-body
  "Generate markdown table body by the given [Info] list"
  [info-list]
  (->> info-list (map info->markdown-table-row) (str/join "\n")))

(defn ^String info-list->markdown-table
  "Convert [Info] list to markdown table"
  [info-list]
  (str
   (info-markdown-table-header)
   "\n"
   (info-markdown-table-seperator-line)
   "\n"
   (info-list->markdown-table-body info-list)))

(comment
  (let [data (repeat 2
                     (map->Info
                      {:code  "juq-933"
                       :home-url "https://missav.ai/cn/juq-933"
                       :preview-url "https://fourhoi.com/JUQ-933/preview.mp4"
                       :title "JUQ-933 补习老师小百合的不为人知的一面 叶山小百合 - 叶山さゆり"
                       :publish-date "2024-10-04"
                       :cover-url  "https://fourhoi.com/juq-933/cover-n.jpg"
                       :play-url "https://surrit.com/58421d0a-b514-4e71-ae8a-05c484bf059c/playlist.m3u8"}))]

    (-> data info-list->markdown-table println)))

;; === Parse [Info] from Html ===

(defmulti info-field-value-from-html
  "Parse value of [field-key] of [Info] from html"
  (fn [field-key html-content] field-key))

(defmethod info-field-value-from-html :default
  [_ _]
  (throw (ex-info "Unimplemented default method of multi-methods [info-field-value-from-html]")))

;; "Parse value of [title] of [Info] from html"
(defmethod info-field-value-from-html :title
  [_ html-content]
  (let [re #"og:title\" content=\"([\s\S]+?)\""]
    (some->> html-content (re-seq re) first last str/trim)))

;; "Parse value of [publish-date] of [Info] from html"
(defmethod info-field-value-from-html :publish-date
  [_ html-content]
  (let [re #"class=\"font-medium\">([\s\S]+?)</time>"]
    (some->> html-content (re-seq re) first last str/trim)))

;; "Parse value of [cover-url] of [Info] from html"
(defmethod info-field-value-from-html :cover-url
  [_ html-content]
  (let [re #"og:image\" content=\"([\s\S]+?cover-n.jpg)"]
    (some->> html-content (re-seq re) first last str/trim)))

;; "Parse value of [play-url] of [Info] from html"
(defmethod info-field-value-from-html :play-url
  [_ html-content]
  (let [re #"m3u8\|([\s\S]+?)\|video"
        parsed (some->> html-content (re-seq re) first last str/trim)
        [scheme domain2 domain1 & ids] (-> parsed (str/split #"\|") reverse)]
    (str scheme "://" domain2 "." domain1 "/" (str/join "-" ids) "/" "playlist.m3u8")))

;; "Parse value of  [has-chinese-subtitle] of [Info] from html"
(defmethod info-field-value-from-html :has-chinese-subtitle
  [_ html-content]
  (let [re #"切换中文字幕"]
    (not (nil? (some->> html-content (re-seq re))))))

;; "Parse value of  [has-english-subtitle] of [Info] from html"
(defmethod info-field-value-from-html :has-english-subtitle
  [_ html-content]
  (let [re #"切换英文字幕"]
    (not (nil? (some->> html-content (re-seq re))))))

;; "Parse value of  [has-uncensored-leak] of [Info] from html"
(defmethod info-field-value-from-html :has-uncensored-leak
  [_ html-content]
  (let [re #"切换无码"]
    (not (nil? (some->> html-content (re-seq re))))))

(defn ^Info info-from-html
  "Parse [Info] from html content"
  [^String code ^String home-url ^String html-content]
  (map->Info {:code code
              :home-url home-url
              :preview-url (str "https://fourhoi.com/" code "/preview.mp4")
              :title (info-field-value-from-html :title html-content)
              :publish-date (info-field-value-from-html :publish-date html-content)
              :cover-url (info-field-value-from-html :cover-url html-content)
              :play-url (info-field-value-from-html :play-url html-content)
              :has-chinese-subtitle (info-field-value-from-html :has-chinese-subtitle html-content)
              :has-uncensored-leak (info-field-value-from-html :has-uncensored-leak html-content)}))

(comment
  (for [field-key [:title :publish-date :cover-url :play-url]]
    (some->>
     (fs/read-all-bytes "test.html")
     (String.)
     (info-field-value-from-html field-key)))

  (->>
   (fs/read-all-bytes "test.html")
   (String.)
   (info-from-html "juq-933" "https://missav.ai/cn/juq-933")))

;; === Fetch [Info] ===

(def ^String base-uri "https://missav.ai/cn")
(def base-header {:accept "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
                  :accept-encoding "gzip, deflate, br"
                  :accept-language "zh-CN,zh-HK;q=0.9,zh;q=0.8"
                  :cache-control "max-age=0"
                  :user-agent "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Mobile Safari/537.36"})

(defn ^String fetch-html
  "Fetch html content by the given url"
  [^String url]
  (log/debug url)
  (-> (http/get url {:throw true :header base-header}) :body))

(defn ^Info update-play-url-if-need
  "Update play-url if need"
  [^Info info]
  (cond
    (:has-chinese-subtitle info) (assoc info :play-url (->> (str (:home-url info)  "-chinese-subtitle")
                                                            (retry {:retries 3 :delay-ms 1000 :jitter-ms 200}
                                                                   fetch-html)
                                                            (info-field-value-from-html :play-url)))

    (:has-english-subtitle info) (assoc info :play-url (->> (str (:home-url info)  "-english-subtitle")
                                                            (retry {:retries 3 :delay-ms 1000 :jitter-ms 200}
                                                                   fetch-html)
                                                            (info-field-value-from-html :play-url)))
    (:has-uncensored-leak info) (assoc info :play-url (->> (str (:home-url info)  "-uncensored-leak")
                                                           (retry {:retries 3 :delay-ms 1000 :jitter-ms 200}
                                                                  fetch-html)
                                                           (info-field-value-from-html :play-url)))
    :else info))

(defn ^Info fetch-info
  "Fetch [Info] by the given code"
  [^String code]
  (let [home-url (str base-uri "/" code)
        html-content (retry {:retries 3 :delay-ms 1000 :jitter-ms 200}
                            fetch-html home-url)]
    (-> (info-from-html code home-url html-content) update-play-url-if-need)))

(defn fetch-info-list
  "Fetch [Info] list by the given code list"
  [code-list]
  (for [code code-list]
    (fetch-info code)))

(comment
  (-> (str base-uri "/" "juq-933") fetch-html println)
  (-> (fetch-info "juq-933") println))

;; === Main ===

(defn -main [args]
  (->>
   (fs/read-all-lines "xlist.txt")
   sort
   fetch-info-list
   info-list->markdown-table
   (conj [])
   (fs/write-lines "README.md")))

(when (= *file* (System/getProperty "babashka.file"))
  (-main *command-line-args*))

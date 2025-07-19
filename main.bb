#!/usr/bin/env bb

(require '[clojure.string :as string])
(require '[babashka.fs :as fs])
(require '[babashka.http-client :as http])
(require '[taoensso.timbre :as log])

(set! *warn-on-reflection* true)

(defn jitted-delay
  "Return delay time that is jitted"
  [delay-ms jitter-ms]
  (-> (* 2 jitter-ms)
      inc
      rand-int
      (- jitter-ms)
      (+ delay-ms)))

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
                 ^String play-url])

;; === Convert [Info] list to Markdown Table ===

(defn info-keys
  "Returns keys of [Info]"
  []
  [:code :title :cover-url :preview-url :play-url :publish-date])

(defn ^String info-markdown-table-header
  "Generate markdown table header for [Info]"
  []
  (str "|"
       (string/join "|" (info-keys))
       "|"))

(defn ^String info-markdown-table-seperator-line
  "Generate markdown table seperator line for [Info]"
  []
  (str "|"
       (->>
        (repeat (count (info-keys)) ":----:")
        (string/join "|"))
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
  (format "<img src=\"%s\">"
          (get info :cover-url "")))

;; "Format value of [preview-url] of [Info] to markdown table cell"
(defmethod ^String info-field-value-to-markdown-table-cell :preview-url
  [_ info]
  (format "<video><source src=\"%s\" type=\"video/mp4\"></video>"
          (get info :preview-url "")))

;; "Format value of [play-url] of [Info] to markdown table cell"
(defmethod ^String info-field-value-to-markdown-table-cell :play-url
  [_ info]
  (format "<video><source src=\"%s\" type=\"application/x-mpegURL\"></video>"
          (get info :play-url "")))

(defn ^String info->markdown-table-row
  "Convert [Info] to markdown table row"
  [info]
  (str "| "
       (->> (info-keys)
            (map #(info-field-value-to-markdown-table-cell % info))
            (string/join " | "))
       " |"))

(defn ^String info-list->markdown-table-body
  "Generate markdown table body by the given [Info] list"
  [info-list]
  (->> info-list
       (map info->markdown-table-row)
       (string/join "\n")))

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

    (-> data
        info-list->markdown-table
        println)))

;; === Parse [Info] from Html ===

(defmulti info-field-value-from-html
  "Parse value of [field-key] of [Info] from html"
  (fn [field-key html-content] field-key))

(defmethod info-field-value-from-html :default
  [_ _]
  (throw (ex-info "Unimplemented default method of multi-methods [info-field-value-from-html]")))

;; "Parse value of [title] of [Info] from html"
(defmethod info-field-value-from-html
  :title
  [_ html-content]
  (let [re #"og:title\" content=\"([\s\S]+?)\""]
    (some->> html-content
             (re-seq re)
             first
             last
             string/trim)))

;; "Parse value of [publish-date] of [Info] from html"
(defmethod info-field-value-from-html :publish-date
  [_ html-content]
  (let [re #"class=\"font-medium\">([\s\S]+?)</time>"]
    (some->> html-content
             (re-seq re)
             first
             last
             string/trim)))

;; "Parse value of [cover-url] of [Info] from html"
(defmethod info-field-value-from-html :cover-url
  [_ html-content]
  (let [re #"og:image\" content=\"([\s\S]+?cover-n.jpg)"]
    (some->> html-content
             (re-seq re)
             first
             last
             string/trim)))

;; "Parse value of [play-url] of [Info] from html"
(defmethod info-field-value-from-html :play-url
  [_ html-content]
  (let [re #"m3u8\|([\s\S]+?)\|video"
        parsed (some->> html-content
                        (re-seq re)
                        first
                        last
                        string/trim)
        [scheme domain2 domain1 & ids] (-> parsed
                                           (string/split #"\|")
                                           reverse)]
    (str scheme "://" domain2 "." domain1 "/" (string/join "-" ids) "/" "playlist.m3u8")))

(defn ^Info info-from-html
  "Parse [Info] from html content"
  [code home-url html-content]
  (map->Info {:code code
              :home-url home-url
              :preview-url (str "https://fourhoi.com/" code "/preview.mp4")
              :title (info-field-value-from-html :title html-content)
              :publish-date (info-field-value-from-html :publish-date html-content)
              :cover-url (info-field-value-from-html :cover-url html-content)
              :play-url (info-field-value-from-html :play-url html-content)}))

(comment
  (for [field-key [:title :publish-date :cover-url :play-url]]
    (some->>
     (fs/read-all-bytes "./test.html")
     (String.)
     (info-field-value-from-html field-key)))

  (->>
   (fs/read-all-bytes "./test.html")
   (String.)
   (info-from-html "juq-933" "https://missav.ai/cn/juq-933")))

;; === Fetch [Info] ===

(def ^String base-uri "https://missav.ai/cn")
(def base-header {:accept "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
                  :accept-encoding "gzip, deflate, br"
                  :accept-language "zh-CN,zh-HK;q=0.9,zh;q=0.8"
                  :cache-control "max-age=0"
                  :cookie "user_uuid=abfc2364-87fb-499a-ac91-0dd1e78d475f; XSRF-TOKEN=eyJpdiI6IjQ3UjFFdERYcnZCTEJ3ajZSMW0xeFE9PSIsInZhbHVlIjoiSzNaa0RodUIvQmdGWjNJcmpTZC9ndnhUbmgxQUNwS0JzMkhmZTJzUHIya25uZ2EzZ3Z2Q2EvQ1graU5Wc2tkazQxeHJldVZmait6ZHZUSitYWklVdHpyZnA4aW9KZURmL1Q0c0QvcXhZc3pwZVVkNWEwbExWV1JQVGk2OEF1U28iLCJtYWMiOiI1ODJkZTczODVjZjViZTEzMzdiOTUwZmNiN2M0YTZlZTRkNDI2M2ExZDk3MTM2NWRhZDNhYjY1YTE3NWQwMWQxIiwidGFnIjoiIn0%3D; missav_session=eyJpdiI6IlVzaDIyZlluczZ0UmNLcElBYkpwZmc9PSIsInZhbHVlIjoiUE83VjkySXpwQTlXYVFzeXB5Z1Jlem9ic21OZEZySVBkOHU0WnhpNDlrS1dVSjlETXdISzhFc3N5QzB0S09lQk96U2ZzMWw0YzlxOVJUaVgvbnV1UFVhbnhINWg1M0s3cGxBeVhzQ2w0eklsbzVYUnM4Y1RsU2xGRlFjQm1vdDkiLCJtYWMiOiJlMTJkYmMwZDQwZWQxMWQ0Mzg2OGNjNjE1ZDU1MTBkMTczMDIzZjA1MDExYTdiMTc3NTA1ZmNiMjAyNWI0Yjk4IiwidGFnIjoiIn0%3D; rrd0DAJNLP5YYnm2cNuTI2VTYJokJIT3IALbtjE7=eyJpdiI6InA3RldoVkZ1L2ppUTJxR1R0QWl2M0E9PSIsInZhbHVlIjoiZFJ4SUxhSkRVbzVYV3lFU1hraXhvRGNydVZxZkNwS3JhK2p3cUg5QmhPRi9DeUp1UENVRWNBdXp6SmZ1NG5hWVk5YkN2ZlB2MnVPbmJ4ditVWmwrYnl5eDhlcFNEUHAyeUhER3JQNkx2ajJJT2FFUUovaEhaRlBLMFlRNEpsZVBxWEVDM2VYNVU0SlluL1FrQktRYjdhWFVaV3JQYjN1Sm1XUWY4TVhMSFI4RGNCOW5XeEU4aXdhMjRlOERIMGRsMEl2VGptOTJUKzJhaG5XNW1BM0ppeVdleU02cEZkZVg0d3B4bmJIb3BFempIYjl4NnZaZ01HazkrOVJ2Tk5wUzBFdXNoWm5jSElRYWM3Q2VmNDE5cFZjaVc4WEZqRWFCY2pvZDY4RjB2MkpRdnphZ1VxWTZRdVE3anpWRkpwNXNCcnowS2FMRlhFSVNQTEYyTFpwQnJVaHNEeVFwNWFqOWwvOWFPTTRha3BHN1RZUzBHa0pJL1FUdk1RV1JmTkx2TkNmdlFjYVVMVDBuZGhJa25iajNyQT09IiwibWFjIjoiNDFlYmY4YTEwN2VjNDY2ZTE2MDgxZWY1ZTEyZDYzNzk1YWM1OTExZTEwNjQ3ZDBjNmJlYjczNjNhNjdjOWFhMSIsInRhZyI6IiJ9"
                  :user-agent "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Mobile Safari/537.36"})

(defn ^String fetch-html
  "Fetch html content by the given url"
  [url]
  (log/debug url)
  (->
   (http/get url {:throw true :header base-header})
   :body))

(defn ^Info fetch-info
  "Fetch [Info] by the given code"
  [code]
  (let [home-url (str base-uri "/" code)
        html-content (retry {:retries 3 :delay-ms 1000 :jitter-ms 200}
                            fetch-html home-url)]
    (info-from-html code home-url html-content)))

(defn fetch-info-list
  "Fetch [Info] list by the given code list"
  [code-list]
  (for [code code-list]
    (fetch-info code)))

(comment
  (-> (str base-uri "/" "juq-933")
      fetch-html
      println)

  (->
   (fetch-info "juq-933")
   println))

;; === Main ===

(->>
 (fs/read-all-lines "./xlist.txt")
 sort
 fetch-info-list
 info-list->markdown-table
 (conj [])
 (fs/write-lines "./README.md"))

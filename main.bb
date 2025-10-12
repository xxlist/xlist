#!/usr/bin/env bb

(require '[clojure.string :as str])
(require '[babashka.fs :as fs])
(require '[clojure.java.io :as io])
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
                 ^boolean origin-is-chinese-subtitle
                 ^boolean has-chinese-subtitle
                 ^boolean has-english-subtitle
                 ^boolean has-uncensored-leak])

;; === Model [Channel] ===

(defrecord CoverImage [^String url
                       ^int width
                       ^int height])

(defrecord ChannelEntry [^String id
                         ^String title
                         ^String description
                         ^long timestamp
                         ^double duration
                         ^String url
                         cover-images
                         ^String video-url])

(defrecord Channel [^String id
                    ^String title
                    ^String description
                    ^String url
                    ^String avatar-url
                    entries])

;; === [Channel] to rss ===

(defn rfc1123-datetime-formatted
  [timestamp]
  (->
   (java.time.Instant/ofEpochSecond timestamp)
   (.atZone (java.time.ZoneId/of "UTC"))
   (.format  java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)))

(comment
  (rfc1123-datetime-formatted 1678886400))

(defn replace-and-char
  "Replace & to &amp;"
  [s]
  (str/replace s "&" "&amp;"))

(comment
  (replace-and-char "https://abc.xyz?foo=1&bar=2&baz=3"))

(defn write-cdata
  "Write [data] wrapped by [CDATA] into [writer]"
  [writer data]
  (doto writer
    (.write "<![CDATA[ ")
    (.write data)
    (.write " ]]>")))

(defn channel-entry->rss
  "Write [ChannelEntry] as rss xml"
  [writer {:keys [id
                  title
                  description
                  cover-images
                  url
                  video-url
                  duration
                  timestamp]}]

  (doto writer
    (.write "<item>")
    (.write "<guid>")
    (.write id)
    (.write "</guid>")
    (.write "<title>")
    (write-cdata title)
    (.write "</title>")
    (.write "<description>")
    (write-cdata description)
    (.write "</description>")
    (.write "<pubDate>")
    (.write (or (some-> timestamp rfc1123-datetime-formatted) ""))
    (.write "</pubDate>")
    (.write "<itunes:duration>")
    (.write (str duration))
    (.write "</itunes:duration>")
    (.write (format "<itunes:image href=\"%s\"/>" (-> (last cover-images) :url replace-and-char)))
    (.write (format "<enclosure url=\"%s\" type=\"video/mp4\"/>" (-> video-url replace-and-char)))
    (.write "</item>")))

(defn channel-entries->rss
  [writer entries]
  (doseq [e entries]
    (channel-entry->rss writer e))
  writer)

(defn channel->rss
  "Write [Channel] as rss xml"
  [save-file {:keys [id title description url avatar-url entries]}]
  (with-open [w (io/writer save-file)]
    (doto w
      (.write "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
      (.write "<rss xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\" xmlns:atom=\"http://www.w3.org/2005/Atom\" version=\"2.0\">")
      (.write "<channel>")
      (.write "<title>")
      (write-cdata title)
      (.write "</title>")
      (.write "<description>")
      (write-cdata description)
      (.write "</description>")
      (.write "<link>")
      (.write url)
      (.write "</link>")
      (.write (or (some->>
                   avatar-url
                   replace-and-char
                   (format "<itunes:image href=\"%s\"/>")) ""))
      (channel-entries->rss entries)
      (.write "</channel>")
      (.write "</rss>")
      (.flush))
    nil))

(defn time-str->epoch-second [time-str]
  (-> time-str
      (java.time.LocalDate/parse java.time.format.DateTimeFormatter/ISO_LOCAL_DATE)
      (.atStartOfDay (java.time.ZoneId/of "UTC"))
      (.toEpochSecond)))

(comment
  (time-str->epoch-second "2024-10-13"))

(defn info->channel-entry
  [info]
  (map->ChannelEntry {:id (:code info)
                      :title (:title info)
                      :description (:description info)
                      :timestamp (-> (:publish-date info) time-str->epoch-second)
                      :duration nil
                      :url (:home-url info)
                      :video-url (:play-url info)
                      :cover-images (->> (map->CoverImage {:url (:cover-url info)})
                                         (conj []))}))

(defn info-list->channel
  [info-list]
  (map->Channel {:id "github.com/xxlist/xlist"
                 :title "xlist-rss"
                 :description "xlist-rss"
                 :url "https://raw.githubusercontent.com/xxlist/xlist/refs/heads/doc/rss.xml"
                 :avatar-url nil
                 :entries (mapv info->channel-entry info-list)}))

(comment
  (->>
   (map->Info
    {:code  "juq-933"
     :home-url "https://missav.ai/cn/juq-933"
     :preview-url "https://fourhoi.com/JUQ-933/preview.mp4"
     :title "JUQ-933 补习老师小百合的不为人知的一面 叶山小百合 - 叶山さゆり"
     :description "今天放学后，我也听到走廊上有对学生大喊大叫的声音。这个声音是实习导师小百合老师。她是一位非常认真严格的老师，在我们学校争夺第一名和第二名，但作为班代表和一个专心学习的学生，我暗暗喜欢小百合老师的态度。有一天，当我爬上移动教室的楼梯时，我听到楼上传来小百合老师的声音。当我猛然抬头呼唤她时，小百合老师没有穿内裤。看到如此严肃的老师我惊呆了……我的裤裆也渐渐变硬了……"
     :publish-date "2024-10-04"
     :cover-url  "https://fourhoi.com/juq-933/cover-n.jpg"
     :play-url "https://surrit.com/58421d0a-b514-4e71-ae8a-05c484bf059c/playlist.m3u8"})
   (conj [])
   info-list->channel))

;; === Convert [Info] list to Markdown Table ===

(defn info-keys
  "Returns keys of [Info]"
  []
  [:code :cover-url :title])

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
  (throw (ex-info "Unimplemented default method of multi-methods [info-field-value-from-html]" {})))

;; "Parse value of [title] of [Info] from html"
(defmethod info-field-value-from-html :title
  [_ html-content]
  (let [re #"og:title\" content=\"([\s\S]+?)\""]
    (some->> html-content (re-seq re) first last str/trim)))

;; "Parse value of [description] of [Info] from html"
(defmethod info-field-value-from-html :description
  [_ html-content]
  (let [re #"og:description\" content=\"([\s\S]+?)\""]
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

;; "Parse value of  [origin-is-chinese-subtitle] of [Info] from html"
(defmethod info-field-value-from-html :origin-is-chinese-subtitle
  [_ html-content]
  (let [re #"<span>类型:</span>\s*<a href=\"https://missav.ai/cn/chinese-subtitle\" class=\"text-nord13 font-medium\">中文字幕</a>"]
    (not (nil? (some->> html-content (re-seq re))))))

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
              :description (info-field-value-from-html :description html-content)
              :publish-date (info-field-value-from-html :publish-date html-content)
              :cover-url (info-field-value-from-html :cover-url html-content)
              :play-url (info-field-value-from-html :play-url html-content)
              :origin-is-chinese-subtitle (info-field-value-from-html :origin-is-chinese-subtitle html-content)
              :has-chinese-subtitle (info-field-value-from-html :has-chinese-subtitle html-content)
              :has-english-subtitle (info-field-value-from-html :has-english-subtitle html-content)
              :has-uncensored-leak (info-field-value-from-html :has-uncensored-leak html-content)}))

(comment
  (for [field-key [:title :description :publish-date :cover-url :play-url]]
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
    (:origin-is-chinese-subtitle info) info
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

(defmulti print-info-list
  (fn [info-list file]
    (-> (fs/split-ext file) last)))

(defmethod print-info-list :default
  [_ _]
  (throw (ex-info "Unimplemented default method of multi-methods [print-info-list]" {})))

(defmethod print-info-list "xml"
  [info-list file]
  (->>
   info-list
   info-list->channel
   (channel->rss file)))

(defmethod print-info-list "md"
  [info-list file]
  (->> info-list
       info-list->markdown-table
       (conj [])
       (fs/write-lines file)))

(defn -main [args]
  (let [info-list
        (->>
         (fs/read-all-lines "xlist.txt")
         sort
         distinct
         fetch-info-list)]

    (log/info "writing rss.xml")
    (print-info-list info-list "rss.xml")

    (log/info "writing README.md")
    (print-info-list info-list "README.md")))

(when (= *file* (System/getProperty "babashka.file"))
  (-main *command-line-args*))

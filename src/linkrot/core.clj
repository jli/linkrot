
(ns linkrot.core
  (:use [clojure.tools.cli :only [cli]])
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as html])
  (:gen-class))

;;; util

;; replace stdout log with sweet interprize solution
(defn log [& args]
  ;; pretty terrible
  (let [timestamp (-> (java.util.Date.) .getTime java.sql.Timestamp. .toString)]
    (apply println timestamp "|" args)))



;;; arch

(def get-page client/get)

(defn absolute? [url] (re-matches #"^https?://.*" url))
(defn rooted? [url] (re-matches #"^/.*" url))
;; doesn't include trailing slash
(defn base [url]
  (second (re-matches #"^(https?://[^/]+).*" url)))
;; includes trailing slash
(defn trim-last [url]
  (second (re-matches #"(.*/).*$" url)))

;; erm, "..", "."? better way.
(defn rebase [url full-url]
  (cond
   (absolute? url) url
   (rooted? url) (str (base full-url) url)
   :else (str (trim-last full-url) url)))

(defn parse-links [url page]
  (let [nodes (-> page :body java.io.StringReader. html/html-resource)
        links (map #(-> % :attrs :href)
                   (html/select nodes [:a]))]
    (map #(rebase % url) links)))



;;; go

(defn separate-with [f coll]
  (reduce (fn [[pass fail] x]
            (if (f x)
              [(conj pass x) fail]
              [pass (conj fail x)]))
          [[] []]
          coll))

(defn status [url] (:status (client/head url {:throw-exceptions false})))

;; correct?
(defn same-domain? [u1 u2]
  (.startsWith u1 (base u2)))

;; statii are url->status code maps
(defn crawl [url statii]
  (log "crawling page" url)
  (let [page (get-page url)
        links (parse-links url page)
        [seen unseen] (separate-with #(contains? statii %) links)
        [int ext] (separate-with #(same-domain? % url) unseen)
        statii (reduce (fn [statii url]
                         (assoc statii url (status url)))
                       statii unseen)]
    (log "links:" links)
    (log "already seen:" seen)
    (log "new:" unseen)
    (log "new internal:" int)
    (log "external:" ext)
    (reduce (fn [acc next-url] (crawl next-url acc))
            statii
            int)))

(defn thundercats-are-go [page ttl archive?]
  (client/with-connection-pool {:timeout 5 :threads 2 :insecure? false}
    (log "on page" page "with crawl limit" ttl "(archiving:" archive? ")")
    (log "IGNORING TTL AND ARCHIVING")
    (let [res (crawl page {page (status page)})]
      (log "done:" res)
      (log "non-200s:" (filter (fn [[_ status]] (not= status 200)) res))
      res)))



;;; get set

(defn -main [& args]
  (let [[opts anon banner]
        (cli args
             ["-h" "--help" :default false :flag true]
             ["-d" "--depth"
              "Crawl up to n hops away from an initial page. Default is to crawl an entire site."
              :default nil :parse-fn #(Integer. %)]
             ["-a" "--[no-]archive"
              "Download a copy of all external links."
              :default false])]
    (cond
     (:help opts) (println banner)
     :else (if (not= 1 (count anon))
             (do (println "Takes exactly 1 anonymous argument: page to start at!")
                 (System/exit 1))
             (thundercats-are-go (first anon) (:depth opts) (:archive opts))))))

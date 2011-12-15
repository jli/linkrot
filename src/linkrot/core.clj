(ns linkrot.core
  (:use [clojure.tools.cli :only [cli]])
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as html])
  (:gen-class))

;;; page info type:
;;; * status code, :status
;;; * what links here, :from

;;; crawl site, building map from url to page info type.



;;; util

;; replace stdout log with sweet interprize solution
(defn log [& args]
  ;; pretty terrible
  (let [timestamp (-> (java.util.Date.) .getTime java.sql.Timestamp. .toString)]
    (apply println timestamp "|" args)))

(defn separate-with [f coll]
  (reduce (fn [[pass fail] x]
            (if (f x)
              [(conj pass x) fail]
              [pass (conj fail x)]))
          [[] []]
          coll))

(defn pair-with
  ([f xs] (pair-with f xs true))
  ([f xs parallel?]
     (let [map (if parallel? pmap map)]
       (map (fn [x] [x (f x)]) xs))))



;;; arch

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

(defn drop-id [url]
  (second (re-matches #"^([^#]*).*" url)))

(defn parse-links [url page]
  (let [nodes (-> page :body java.io.StringReader. html/html-resource)
        links (map #(-> % :attrs :href)
                   (html/select nodes [:a]))
        ;; nil when no href, like anchors with just names
        links (filter #(not (or (nil? %)
                                (re-matches #"^$|^(?:mailto|ftp|gopher):.+" %))) links)]
    (set (map #(rebase (drop-id %) url) links))))

;; correct?
(defn same-domain? [u1 u2]
  (.startsWith u1 (base u2)))

(defn get-page-prim [url]
  (client/get url {:throw-exceptions false}))

(def get-page (memoize get-page-prim))

(defn head-page [url]
  (client/head url {:throw-exceptions false}))

(defn status [url internal?]
  ;; internal pages are GETed, so use get-page (which caches). use
  ;; less expensive HEAD for external pages.
  (:status ((if internal? get-page head-page) url)))



;;; go

(defn page-info [url from-url internal?]
  {:status (status url internal?)
   :from [from-url]})

;; update :from field in page-infos with new url
(defn update-seen [statii seens from-url]
  (reduce (fn [statii seen]
            (update-in statii [seen :from] conj from-url))
          statii
          seens))

;; hmm. when url is http://blah.com/dir, there's a 301 redirecting to
;; /dir/. Need trailing slash, or else rebasing gets messed up!
(defn crawl [url statii]
  (log "crawling page" url)
  (let [page (get-page url)
        links (parse-links url page)
        [seen unseen] (separate-with #(contains? statii %) links)
        [int ext] (separate-with #(same-domain? % url) unseen)
        _ (do (log "links:" links)
              (log "already seen:" seen ", new:" unseen)
              (log "new internal:" int ", external:" ext))
        [istatii estatii] [(pair-with #(page-info % url true) int)
                           (pair-with #(page-info % url false) ext)]
        statii (merge (update-seen statii seen url)
                      (into {} istatii)
                      (into {} estatii))]
    (reduce (fn [acc next-url] (crawl next-url acc))
            statii
            int)))

(defn thundercats-are-go [page ttl archive?]
  (client/with-connection-pool {:timeout 5 :threads 4 :insecure? false}
    (log "on page" page "with crawl limit" ttl "(archiving:" archive? ")")
    (let [res (crawl page {page (page-info page nil true)})
          broken (filter (fn [[_ info]] (not= (:status info) 200)) res)]
      (log "done:" res)
      (log "brokens:")
      (doseq [[url {:keys [status from]}] broken]
        (println url status from))
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

(ns pricey.scrape
  "Web scraper for AWS instance and pricing info.

  Based off of <https://github.com/powdahound/ec2instances.info>"
  (require [hickory.core :refer :all]
           [hickory.select :as s]
           [clj-http.client :as client]
           [clojure.string :as str]
           [cheshire.core :as json])
  (:gen-class))

(defn- parse-int
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception x nil)))

(defn- parse-double
  [s]
  (try
    (Double/parseDouble s)
    (catch Exception x nil)))

(defn- fetch-data
  [url]
  (let [data (:body (client/get url))]
    (try
      (json/parse-string data)
      (catch Exception x (json/parse-string
                          (str/replace
                           (.substring data (+ 9 (.indexOf data "callback("))
                                       (- (.length data) 2))
                           #"(\w+):" "\"$1\":"))))))

(defn- fix-region
  [region]
  (let [region-map {"eu-ireland"   "eu-west-1"
                    "eu-frankfurt" "eu-central-1"
                    "apac-sin"     "ap-southeast-1"
                    "apac-syd"     "ap-southeast-2"
                    "apac-tokyo"   "ap-northeast-1"}]
    (if-let [r (get region-map region)]
      r
      (if-let [m (re-matches #"^([^0-9]*)(-(\d))?$" region)]
        (str (second m) "-" (or (nth m 3) "1"))
        region))))

(defn- parse-prevgen-instance
  [row]
  (let [cols (s/select (s/child (s/tag :td)) row)
        family (-> (first cols) :content first str/trim)
        instance-type (-> (second cols) :content first str/trim keyword)
        arch (filter #(not (nil? %))
                   [(when (.contains (-> (nth cols 2) :content first str/lower-case) "64-bit") "x86_64")
                    (when (.contains (-> (nth cols 2) :content first str/lower-case) "32-bit") "i386")])
        vcpu (-> (nth cols 3) :content first str/trim parse-int)
        memory (-> (nth cols 4) :content first str/trim parse-double)
        ssd (.contains (-> (nth cols 5) :content first str/lower-case) "ssd")
        ebs-only (.contains (-> (nth cols 5) :content first str/lower-case) "ebs only")
        [num-drives drive-size] (when (not ebs-only)
                                  (map parse-int
                                       (rest (re-matches #"(\d+)\s*x\s*([0-9,]+)?"
                                                         (-> (nth cols 5) :content first)))))
        ebs-optimized (.equalsIgnoreCase (-> (nth cols 6) :content first str/trim) "yes")
        network-performance (-> (nth cols 7) :content first str/trim)]
    {:instance-type instance-type
     :family family
     :arch arch
     :vcpu vcpu
     :memory memory
     :ssd ssd
     :ebs-only ebs-only
     :num-drives num-drives
     :drive-size drive-size
     :ebs-optimized ebs-optimized
     :generation :previous}))

(defn parse-instance
  [inst2family row]
  (let [cols (s/select (s/child (s/tag :td)) row)
        instance-type (-> (first cols) :content first keyword)
        family (get inst2family instance-type "Unknown")
        arch (if (some #{instance-type} [:t2.micro :t2.small])
               [:x86_64 :i386]
               [:x86_64])
        vcpu (-> (second cols) :content first parse-int)
        memory (-> (nth cols 2) :content first parse-double)
        ssd (.contains (-> (nth cols 3) :content first str/lower-case) "ssd")
        ebs-only (.contains (-> (nth cols 3) :content first str/lower-case) "ebs only")
        [num-drives drive-size] (when (not ebs-only)
                                  (map parse-int
                                       (rest (re-matches #"(\d+)\s*x\s*([0-9,]+)?"
                                                         (-> (nth cols 3) :content first)))))
        network-performance (-> (nth cols 4) :content first)
        clock-speed (-> (nth cols 6) :content first parse-double)
        intel-avx (.equalsIgnoreCase (-> (nth cols 7) :content first) "yes")
        intel-avx2 (.equalsIgnoreCase (-> (nth cols 8) :content first) "yes")
        intel-turbo (.equalsIgnoreCase (-> (nth cols 9) :content first) "yes")
        ebs-optimized (.equalsIgnoreCase (-> (nth cols 10) :content first) "yes")
        enhanced-networking (.equalsIgnoreCase (-> (nth cols 11) :content first) "yes")]
    {:instance-type instance-type
     :family family
     :arch arch
     :vcpu vcpu
     :memory memory
     :ssd ssd
     :ebs-only ebs-only
     :num-drives num-drives
     :drive-size drive-size
     :network-performance network-performance
     :clock-speed clock-speed
     :intel-avx intel-avx
     :intel-avx2 intel-avx2
     :intel-turbo intel-turbo
     :ebs-optimized ebs-optimized
     :enhanced-networking enhanced-networking
     :generation :current}))

(defn rindex-families
  [details]
  (let [rows (s/select (s/child (s/tag :tbody) (s/tag :tr)) details)
        fams (map (fn [deet]
                    (let [fam (-> (s/select (s/child (s/tag :td) (s/tag :p)) deet)
                                  first
                                  :content
                                  first)]
                      (into {}
                            (->> (s/select (s/child (s/tag :td) (s/tag :p) (s/tag :code)) deet)
                                 (map :content)
                                 flatten
                                 (map (fn [t] [(keyword t) fam])))))) rows)]
    (apply merge fams)))

(defn scrape-families
  []
  (let [tree (-> (client/get "http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html")
                 :body
                 parse
                 as-hickory)
        details (s/select (s/child (s/tag :div) (s/class "informaltable-contents") (s/tag :table)) tree)
        current-gen-hdrs (flatten (map :content (s/select (s/child (s/tag :thead) (s/tag :tr) (s/tag :th)) (first details))))
        current-gen (when (and (.equalsIgnoreCase (first current-gen-hdrs) "instance family")
                               (.contains (.toLowerCase (second current-gen-hdrs)) "current generation"))
                      (rindex-families (first details)))
        prev-gen-hdrs (flatten (map :content (s/select (s/child (s/tag :thead) (s/tag :tr) (s/tag :th)) (second details))))
        prev-gen (when (and (.equalsIgnoreCase (first prev-gen-hdrs) "instance family")
                            (.contains (.toLowerCase (second prev-gen-hdrs)) "previous generation"))
                   (rindex-families (second details)))]
    (merge current-gen prev-gen)))

(defn scrape-instances
  []
  (let [inst2family (scrape-families)
        tree (-> (client/get "http://aws.amazon.com/ec2/instance-types/")
                 :body
                 parse
                 as-hickory)
        details (nth (s/select (s/child (s/tag :table)) tree) 10)
        rows (rest (s/select (s/child (s/tag :tbody) (s/tag :tr)) details))
        current-gen (map (partial parse-instance inst2family) rows)
        prevgen-tree (-> (client/get "http://aws.amazon.com/ec2/previous-generation/")
                         :body
                         parse
                         as-hickory)
        prevgen-details (nth (s/select (s/child (s/tag :table)) prevgen-tree) 6)
        prevgen-rows (rest (rest (s/select (s/child (s/tag :tbody) (s/tag :tr)) prevgen-details)))
        previous-gen (map parse-prevgen-instance prevgen-rows)]
    (into current-gen previous-gen)))

(defn add-ondemand-pricing
  [instances pricing-data]
  (reduce (fn [m [platform pricing]]
            ())
          instances
          pricing-data))

(defn add-reserved-pricing
  [instances pricing-data])


(defn get-usd
  [size-info]
  (-> (get size-info "valueColumns")
      first
      (get "prices")
      (get "USD")
      parse-double))

(defn twist
  [pricing-info]
  (for [region (-> pricing-info (get "config") (get "regions"))]
    (do
      (println (get region "region"))
      (for [instance-type (get region "instanceTypes")]
        (do
          (println (get instance-type "type"))
          (for [size (get instance-type "sizes")]
            (println (get size "size"))))))))

(defn twist-od-pricing-info
  "Turn on-demand pricing inside-out."
  [pricing-info platform]
  (let [result (atom {})]
    (doall
     (for [region (-> pricing-info (get "config") (get "regions"))
           instance-type (get region "instanceTypes")
           size (get instance-type "sizes")]
       (let [region-name (keyword (get region "region"))
             size-name   (keyword (get size "size"))
             usd         (get-usd size)]
         (swap! result #(update-in % [size-name :pricing :on-demand platform]
                                   (fn [m] (assoc m region-name usd)))))))
    @result))

(defn name-term-option
  [term option]
  (keyword (str (get {"yrTerm1" "1-year" "yrTerm3" "3-year"} term term)
                "-"
                (get
                 {"noUpfront" "no-upfront"
                  "partialUpfront" "partial-upfront"
                  "allUpfront" "all-upfront"} option option))))

(defn get-effective-hourly
  [cols]
  (parse-double
   (->
    (first (filter (fn [col] (= (get col "name") "effectiveHourly")) cols))
    (get "prices")
    (get "USD"))))

(defn fetch-prices
  [term]
  (apply merge
   (flatten
    [(when-let [od (-> term (get "onDemandHourly") first (get "prices") (get "USD") parse-double)]
       {:on-demand od})
     (for [option (get term "purchaseOptions")]
       {(name-term-option (get term "term") (get option "purchaseOption"))
        (get-effective-hourly (get option "valueColumns"))})])))

(defn twist-ri-pricing-info
  "Turn reserved instance pricing inside-out"
  [prev-result pricing-info platform]
  (let [result (atom prev-result)]
    (doall
     (for [region        (-> pricing-info (get "config") (get "regions"))
           instance-type (get region "instanceTypes")
           term          (get instance-type "terms")]
       (let [region-name (keyword (fix-region (get region "region")))
             size-name   (keyword (get instance-type "type"))]
         (swap! result (fn [res] (update-in res [size-name :pricing platform region-name]
                                            (fn [m] (merge m (fetch-prices term)))))))))
    @result))

(defn add-pricing-info
  [instances]
  (let [pricing-modes [:ri :od]
        reserved-name-map {"linux" "linux-unix-shared"
                           "mswin" "windows-shared"
                           "mswinSQL" "windows-with-sql-server-standard-shared"
                           "mswinSQLWeb" "windows-with-sql-server-web-shared"}
        by-type (into {} (map (fn [t] [(:instance-type t) t]) instances))
        current-pricing (reduce (fn [m [data pf]] (twist-ri-pricing-info m data pf))
                                {}
                                (map (fn [pf] [(fetch-data (str "http://a0.awsstatic.com/pricing/1/ec2/ri-v2/"
                                                                (reserved-name-map pf)
                                                                ".min.js")) (keyword pf)])
                                     (keys reserved-name-map)))
        previous-pricing (reduce (fn [m [data pf]] (twist-ri-pricing-info m data pf))
                                 {}
                                 (map (fn [pf] [(fetch-data (str "http://a0.awsstatic.com/pricing/1/ec2/previous-generation/ri-v2/"
                                                                 (reserved-name-map pf)
                                                                 ".min.js"))
                                                (keyword pf)])
                                      (keys reserved-name-map)))
        pricing (merge current-pricing previous-pricing)]
    (reduce (fn [m [k v]] (update-in m [k] #(merge % v)))
            by-type
            pricing)))

;; TODO
(def add-eni-info identity)
(def add-ebs-info identity)
(def add-linux-ami-info identity)

(defn scrape
  []
  (-> (scrape-instances)
      add-pricing-info
      add-eni-info
      add-ebs-info
      add-linux-ami-info))

(defn- map-ebs-name
  [name]
  (let [n (str/lower-case name)]
    (cond
      (.contains n "general purpose") :gp2
      (.contains n "provisioned iops") :io1
      (.contains n "magnetic") :standard
      (.contains n "ebssnaps") :snapshot
      (.contains n "throughput optimized") :st1
      (.contains n "cold hdd") :sc1)))

(defn map-ebs-rate
  [rate]
  (cond
   (= "perGBmoProvStorage" rate) :gb-month
   (= "perPIOPSreq" rate)        :provisioned-iops
   (= "perMMIOreq" rate)         :million-ios
   (= "perGBmoDataStored" rate)  :gb-month))

(defn combine
  [& vals]
  (if (every? map? vals)
    (apply merge-with combine vals)
    (last vals)))

(defn- parse-ebs-pricing
  [url]
  (let [data (fetch-data url)]
    (into {}
          (map (fn [region]
                 [(keyword (fix-region (get region "region")))
                  (into {}
                        (map (fn [t]
                               [(map-ebs-name (get t "name"))
                                (into {}
                                      (map (fn [value]
                                             [(map-ebs-rate (get value "rate"))
                                              (-> value (get "prices") (get "USD") parse-double)])
                                           (get t "values")))])
                             (get region "types")))])
               (-> data (get "config") (get "regions"))))))

(defn scrape-ebs
  []
  (combine (parse-ebs-pricing "https://a0.awsstatic.com/pricing/1/ebs/pricing-ebs.min.js")
           (parse-ebs-pricing "https://a0.awsstatic.com/pricing/1/ebs/pricing-ebs-previous-generation.min.js")))

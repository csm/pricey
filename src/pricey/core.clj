(ns pricey.core
  (require [pricey.inventory :as inventory]
           [pricey.scrape :as scrape]
           [clojure.tools.cli :as cli]))

(defn prices
  [creds]
  (let [reservations (inventory/fetch-reservations creds)
        pricing-info (scrape/scrape)
        ebs-pricing-info (scrape/scrape-ebs)
        costs (inventory/get-app-costs reservations pricing-info ebs-pricing-info)]
    costs))

(def cli-options
  [["-p" "--profile PROFILE" "Specify AWS profile to use."]
   ["-r" "--region REGION" "Specify AWS region to query." :default "us-west-2"]
   ["-k" "--access-key KEY" "Specify AWS access key."]
   ["-s" "--secret-key KEY" "Specify AWS secret key."]
   ["-h" "--hourly" "Display hourly costs (default)."]
   ["-d" "--daily" "Display daily costs."]
   ["-m" "--monthly" "Display monthly costs (assumes 31 days/month)."]
   ["-?" "--help" "Show this help and exit."]])

(defn -main
  [& args]
  (let [opts (cli/parse-opts args cli-options)
        options (:options opts)
        creds (merge {:endpoint (:region options)}
                     (select-keys options [:profile :access-key :secret-key]))]
    (when (:help options)
      (println (:summary opts))
      (System/exit 0))
    (let [factor (cond
                   (:monthly options) (* 24 31)
                   (:daily options) 24
                   :else 1)
          costs (prices creds)]
      (println "App\tStack\tEC2 Cost\tEBS cost")
      (doseq [p costs]
        (println (if (nil? (first p)) "none" (first (first p))) "\t"
                 (if (nil? (first p)) "none" (second (first p))) "\t$"
                 (* factor (:ec2 (second p))) "\t$"
                 (* factor (:ebs (second p))))))))

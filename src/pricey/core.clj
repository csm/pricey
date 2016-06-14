(ns pricey.core
  (require [pricey.inventory :as inventory]
           [pricey.scrape :as scrape]
           [clojure.tools.cli :as cli]
           [clojure.pprint :refer [print-table]]))

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
          costs (sort (fn [a b] (compare (first a) (first b))) (prices creds))
          totals (reduce (fn [a b] {:ec2 (+ (:ec2 a) (:ec2 b)) :ebs (+ (:ebs a) (:ebs b))})
                         (map second costs))
          table (concat
                 (map (fn [e] {"App" (name (or (first (first e)) :none))
                               "Stack" (name (or (second (first e)) :none))
                               "EC2 Cost" (format "$ %.2f" (double (* factor (:ec2 (second e)))))
                               "EBS Cost" (format "$ %.2f" (double (* factor (:ebs (second e)))))}) costs)
                 [{"App" "Total" "Stack" "-"
                   "EC2 Cost" (format "$ %.2f" (double (* factor (:ec2 totals))))
                   "EBS Cost" (format "$ %.2f" (double (* factor (:ebs totals))))}])]
      (print-table table))))

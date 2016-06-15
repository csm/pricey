(ns pricey.core
  (require [amazonica.core :refer [with-credential]]
           [pricey.inventory :as inventory]
           [pricey.scrape :as scrape]
           [clojure.tools.cli :as cli]
           [pricey.util :refer [print-table]]
           [clansi :refer [style]]))

(defn prices
  [creds]
  (let [reservations (inventory/fetch-reservations creds)
        pricing-info (scrape/scrape)
        ebs-pricing-info (scrape/scrape-ebs)
        costs (with-credential creds
                (inventory/get-app-costs reservations pricing-info ebs-pricing-info))]
    costs))

(def cli-options
  [["-p" "--profile PROFILE" "Specify AWS profile to use."]
   ["-r" "--region REGION" "Specify AWS region to query." :default "us-west-2"]
   ["-k" "--access-key KEY" "Specify AWS access key."]
   ["-s" "--secret-key KEY" "Specify AWS secret key."]
   ["-h" "--hourly" "Display hourly costs (default)."]
   ["-d" "--daily" "Display daily costs."]
   ["-m" "--monthly" "Display monthly costs (assumes 31 days/month)."]
   ["-u" "--list-unknown" "List instances that don't have a known application/stack."]
   ["-c" "--color" "Use ANSI color codes when printing"]
   ["-v" "--verbose" "Print messages about progress."]
   [nil "--help" "Show this help and exit."]])

(defn- name-or-tag
  [instance]
  (let [tags (:tags instance)]
    (if-let [name-tag (first (filter #(= (:key %) "Name") tags))]
      (:value name-tag)
      (if-let [asg-tag (first (filter #(= (:key %) "aws:autoscaling:groupName") tags))]
        (:value asg-tag)
        (or (:value (first tags)) "-")))))

(defn -main
  [& args]
  (let [opts (cli/parse-opts args cli-options)
        options (:options opts)
        creds (merge {:endpoint (:region options)}
                     (select-keys options [:profile :access-key :secret-key]))]
    (when (:help options)
      (println "usage: pricey.core/-main [options]")
      (println)
      (println (:summary opts))
      (System/exit 0))
    (let [factor (cond
                   (:monthly options) (* 24 31)
                   (:daily options) 24
                   :else 1)
          _ (if (:verbose options) (println (style "Fetching all reservations..." :green)))
          reservations (inventory/fetch-reservations creds)
          _ (if (:verbose options) (println (style "Fetching all volumes..." :green)))
          volumes (inventory/fetch-volume-info creds reservations)
          _ (if (:verbose options) (println (style "Fetching EC2 pricing info..." :green)))
          pricing-info (scrape/scrape)
          _ (if (:verbose options) (println (style "Fetching EBS pricing info..." :green)))
          ebs-pricing-info (scrape/scrape-ebs)
          _ (if (:verbose options) (println (style "Computing app/stack costs..." :green)))
          costs (sort (fn [a b] (compare (first a) (first b)))
                      (with-credential creds
                        (inventory/get-app-costs reservations pricing-info ebs-pricing-info volumes)))
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
      (print-table table :colorize? (:color options))
      (when (:list-unknown options)
        (let [_ (if (:verbose options) (println (style "Computing non-app instance costs..." :gray)))
              results (filter #(not (nil? %))
                            (for [reservation reservations
                                  instance (:instances reservation)]
                              (if (nil? (inventory/get-app-name instance))
                                (let [cost (second
                                            (with-credential creds
                                              (inventory/get-instance-cost instance pricing-info ebs-pricing-info volumes)))]
                                  {"Instance" (:instance-id instance)
                                   "Type" (:instance-type instance)
                                   "Name/ASG" (name-or-tag instance)
                                   "EC2 Cost" (format "$ %.2f" (double (* factor (or (:ec2 cost) 0.0))))
                                   "EBS Cost" (format "$ %.2f" (double (* factor (or (:ebs cost) 0.0))))})
                                nil)))]
        (when (not (empty? results))
          (println "Instances with no app/stack info found:")
          (print-table results :colorize? (:color options))))))))

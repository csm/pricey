(ns pricey.inventory
  (require [amazonica.aws.ec2 :as ec2]
           [clojure.tools.logging :as log]
           [pricey.util :as u])
  (import [com.netflix.frigga Names]))

(defn- get-names
  [instance]
  (let [tags (:tags instance)
        asg-tag (filter (fn [t] (= "aws:autoscaling:groupName" (:key t))) tags)
        asg-name (:value (first asg-tag))]
    (if (not (nil? asg-name))
      (Names/parseName asg-name)
      nil)))

(defn get-app-name
  "Inspect an instance, return its app name (*if* it is managed by Asgard).

  Returns nil if the app name cannot be determined."
  [instance]
  (let [names (get-names instance)]
    (when (not (nil? names))
      [(keyword (.getApp names))
       (keyword (.getStack names))])))

(defn- gb-month->gb-hour
  [gb-month gb]
  (/ (* gb-month gb) 720))

(defn- az->region
  [az]
  (keyword (.substring az 0 (dec (count az)))))

(defn get-ebs-pricing
  "Fetch all EBS volumes attached to an instance, and estimate the hourly cost
   of those volumes."
  [instance ebs-pricing-info volume-info]
  (let [volume-ids (map (fn [v] (-> v :ebs :volume-id))
                        (filter (fn [bdm] (:ebs bdm)) (:block-device-mappings instance)))
        volumes (filter #(not (nil? %)) (map volume-info volume-ids))
        ;; volumes (flatten
        ;;          (map (fn [volume-id]
        ;;                 ;(Thread/sleep 250) ; avoid throttling ヽ(´ー｀)ノ
        ;;                 (u/try-with-backoff 10 10
        ;;                                     (try
        ;;                                       (:volumes (ec2/describe-volumes :volume-ids [volume-id]))
        ;;                                       (catch Exception _ []))))
        ;;               volume-ids))
        ]
    (/ (reduce (fn [sum volume]
                 (let [size (float (:size volume))
                       volume-type (keyword (:volume-type volume))
                       region (az->region (:availability-zone volume))
                       pricing-info (-> ebs-pricing-info region volume-type)
                       iops (:iops volume)]
                   (log/debug "type" volume-type "size" size "iops" iops)
                   (if (= :io1 volume-type)
                     (+ sum
                        (* size
                           (:gb-month pricing-info))
                        (* iops
                           (:provisioned-iops pricing-info)))
                     (+ sum
                        (* size
                           (:gb-month pricing-info))))))
               0
               volumes)
       720 ; scale to average hours per month
       )))

(defn get-instance-cost
  [instance pricing-info ebs-pricing-info volumes]
  (let [instance-type (keyword (:instance-type instance))
        zone (-> instance :placement :availability-zone)
        region (keyword (.substring zone 0 (dec (count zone))))
        platform :linux
        allocation-type :on-demand  ; FIXME, there should be a way to figure this out.
        is-running (= "running" (:name (:state instance)))]
    [(get-app-name instance) {:ec2 (if is-running
                                     (-> pricing-info instance-type :pricing platform region allocation-type)
                                     0.0)
                              :ebs (get-ebs-pricing instance ebs-pricing-info volumes)}]))

(defn- sum-pricings
  "Sum two dicts of keys->numbers."
  [a b]
  (reduce
   (fn [m [k v]] (update-in m [k] #(+ (or % 0) v)))
   a
   b))

(defn get-app-costs
  [reservations pricing-info ebs-pricing-info volumes]
  (let [result (atom {})]
    (doall
     (for [reservation reservations
           instance (:instances reservation)]
       (let [[app-id cost] (get-instance-cost instance pricing-info ebs-pricing-info volumes)]
         (swap! result #(update-in % [app-id] sum-pricings cost)))))
    @result))

(defn filter-by-names
  "Filter out reservations by frigga Names.

  acceptor is passed a frigga Names object (or nil), and should return true if that name
  should be kept."
  [reservations acceptor]
  (filter (fn [reservation]
            (assoc reservation
              :instances
              (filter (fn [instance] (acceptor (get-names instance)))
                      (:instances reservation))))
          reservations))

(defn fetch-reservations
  [creds]
  (:reservations (ec2/describe-instances creds)))

(defn fetch-volume-info
  [creds reservations]
  (into {}
        (map (fn [e] [(:volume-id e) e])
             (:volumes (ec2/describe-volumes
                        creds
                        :volume-ids
                        (map #(-> (:ebs %) (:volume-id))
                             (flatten (map :block-device-mappings
                                           (flatten (map :instances reservations))))))))))

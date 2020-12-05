(ns aoclj2020
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def input
  (slurp "day4.input"))

(defn parse [inp]
  (->> inp
       (re-seq #"(\w{3}):(\S+)")
       (map (comp vec next))
       (into {})))

(defn valid? [pp]
  (= 7 (count (dissoc pp :cid))))

(defn byr-valid? [{:keys [byr]}]
  (when (some? byr)
    (<= 1920 (Integer/parseInt byr) 2002)))

(defn iyr-valid? [{:keys [iyr]}]
  (when (some? iyr)
    (<= 2010 (Integer/parseInt iyr) 2020)))

(defn eyr-valid? [{:keys [eyr]}]
  (when (some? eyr)
    (<= 2020 (Integer/parseInt eyr) 2030)))

(defn hgt-valid? [{:keys [hgt]}]
  (when-let
      [m (re-find #"(\d+)(in|cm)" hgt)]
    (let [h (Integer/parseInt (second m))]
        (if (= "cm" (last m))
            (<= 150 h 193)
            (<= 59 h 76)))))

(defn hcl-valid? [{:keys [hcl]}]
  (not (empty?
        (re-find #"(#[0-9a-z]{6})" hcl))))

(defn ecl-valid? [{:keys [ecl]}]
  (not (empty?
        (re-find #"(amb|blu|brn|gry|grn|hzl|oth)" ecl))))

(defn pid-valid? [{:keys [pid]}]
  (not (empty?
        (re-find #"(^\d{9}$)" pid))))

(defn all-valid? [m]
  ((every-pred byr-valid? iyr-valid? eyr-valid? hgt-valid? hcl-valid? ecl-valid? pid-valid?) m))

(defn solve1 [inp]
  (->> 
    (str/split inp #"\R\R")
    (map parse)
    (walk/keywordize-keys)
    (filter valid?)
    count))

(defn solve2 [inp]
  (->> 
    (str/split inp #"\R\R")
    (map parse)
    (walk/keywordize-keys)
    (filter valid?)
    (filter all-valid?)
    count))

(solve1 input)
(solve2 input)


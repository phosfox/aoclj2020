(ns aoclj2020
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def test-input
    "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
    byr:1937 iyr:2017 cid:147 hgt:183cm

    iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
    hcl:#cfa07d byr:1929

    hcl:#ae17e1 iyr:2013
    eyr:2024
    ecl:brn pid:760753108 byr:1931
    hgt:179cm

    hcl:#cfa07d eyr:2025 pid:166559648
    iyr:2011 ecl:brn hgt:59in")

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
  (<= 1920 (Integer/parseInt byr) 2002))

(defn iyr-valid? [{:keys [iyr]}]
  (<= 2010 (Integer/parseInt iyr) 2020))

(defn eyr-valid? [{:keys [eyr]}]
  (<= 2030 (Integer/parseInt eyr) 2030))

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
        (re-find #"(^\d{9}$)" pid)))

 f)

(->> 
 (str/split test-input #"\R\R")
 (map parse)
 (walk/keywordize-keys)
 (filter valid?)
 count)

(->> 
 (str/split input #"\R\R")
 (map parse)
 (walk/keywordize-keys)
 (filter valid?)
 count)

(def fields
  ["byr"
   "iyr"
   "eyr"
   "hgt"
   "hcl"
   "ecl"
   "pid"
   "cid"])
   

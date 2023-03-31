#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pandas as pd
import re
import requests
import shutil
import os

CSV_301 = "301.csv"

HEADERS = {
    "referer": "https://tenhou.net/",
    "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36 Edg/111.0.1661.54",
}

TEHAIONLY = False  # 手牌のみ
BGTRANS = False  # 透過
NOLOGO = True  # ロゴなし
TWIIMG = False  # TWIIMG


## clean symbols that is used by MahGen, but not used by tenhou
def clean(t):
    return re.sub(r"\^|_|\|", "", t)


## function `MPSZ.expand` in https://tenhou.net/2/img/mpsz.js
def expand(t):
    t = re.sub(
        r"(\d)(\d{0,8})(\d{0,8})(\d{0,8})(\d{0,8})(\d{0,8})(\d{0,8})(\d{8})(m|p|s|z)",
        r"\1\9\2\9\3\9\4\9\5\9\6\9\7\9\8\9",
        t,
    )
    t = re.sub(
        r"(\d?)(\d?)(\d?)(\d?)(\d?)(\d?)(\d)(\d)(m|p|s|z)",
        r"\1\9\2\9\3\9\4\9\5\9\6\9\7\9\8\9",
        t,
    )
    t = re.sub(r"(m|p|s|z)(m|p|s|z)+", r"\1", t)
    t = re.sub(r"^[^\d]", "", t)
    return t


## function `extract34` in https://tenhou.net/2/img/
def extract34(t):
    t = re.sub(r"0m", "51", t)
    t = re.sub(r"0p", "52", t)
    t = re.sub(r"0s", "53", t)
    t = re.sub(r"(\d)m", r"1\1", t)
    t = re.sub(r"(\d)p", r"2\1", t)
    t = re.sub(r"(\d)s", r"3\1", t)
    t = re.sub(r"(\d)z", r"4\1", t)
    return t


def encode(t):
    return extract34(expand(clean(t)))


## function `compile` in https://tenhou.net/2/img/
def compile(row):
    q = extract34(expand(row.tehai))
    if len(q) != 14 * 2:
        raise Exception("INVALID TEHAI LENGTH")
    if not TEHAIONLY:
        dora = extract34(expand(row.dora))
        kyoku = str(row.kyoku - 1).zfill(2)  # 局数
        step = str(row.step).zfill(2)  # 巡目
        rot = str(row.rot - 1)  # 自风
        q += dora + kyoku + step + rot
    twiimg = str(TWIIMG).lower()
    bgtrans = str(BGTRANS).lower()
    nologo = str(NOLOGO).lower()
    return (
        "https://mjv.jp/2/img/n"
        + q
        + ".png?twiimg="
        + twiimg
        + "&bgtrans="
        + bgtrans
        + "&nologo="
        + nologo
    )


def download(csv_file):
    dir_name = os.path.splitext(csv_file)[0]
    if not os.path.isdir(dir_name):
        os.mkdir(dir_name)
    df = pd.read_csv(csv_file)
    for row in df.itertuples():
        image_name = str(row.Index + 1).zfill(3) + ".png"
        image_path = os.path.join(dir_name, image_name)
        if os.path.isfile(image_path):
            # print("Skip: ", image_name)
            continue
        link = compile(row)
        # print(image_name, link)
        res = requests.get(link, headers=HEADERS, stream=True)
        if res.status_code == 200:
            with open(image_path, "wb") as f:
                shutil.copyfileobj(res.raw, f)
                print("Image sucessfully Downloaded: ", image_name)
        else:
            print("Image Couldn't be retrieved.")


def main():
    download(CSV_301)


if __name__ == "__main__":
    main()

import argparse
import shutil
import fontfreeze

parser = argparse.ArgumentParser()

parser.add_argument("-i", "--input-file", type=str, required=True)
parser.add_argument("-o", "--output-file", type=str, required=True)
parser.add_argument("-f", "--features", type=str)

args = parser.parse_args()

shutil.copy(args.input_file, "temp")
font = fontfreeze.loadTtfFont("temp")
info = fontfreeze.loadFont("temp")

# https://learn.microsoft.com/en-us/typography/opentype/spec/name#name-ids
subfamily = font["name"].getDebugName(2)
typo_subfamily = font["name"].getDebugName(17)

# https://github.com/MuTsunTsai/fontfreeze/blob/5758c7ea4bd1162692356ec646f214d9bd850faf/src/main.js#L408
poptions = {
    "family": info["family"],
    "subfamily": subfamily,
    "typo_subfamily": typo_subfamily,
    "fixContour": False,
    "singleSub": True,
    "target": "calt",
    "format": "ttf"
}

# https://github.com/MuTsunTsai/fontfreeze/blob/5758c7ea4bd1162692356ec646f214d9bd850faf/src/main.js#L419
variations = {a["tag"]: a["default"] for a in info["fvar"]} if info["fvar"] != None else {}

features = args.features.split(",") if args.features != None else []
pargs = {
    "options": poptions,
    "version": " v1.7.4", # https://img.shields.io/github/package-json/v/mutsuntsai/fontfreeze.json
    "unicodes": "",
    "variations": variations,
    "features": features,
    "disables": []
}

fontfreeze.main(pargs, "input", args.output_file)

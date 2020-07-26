#!/usr/bin/env python

from typing import Final, List, Tuple, Optional
from argparse import ArgumentParser
from dataclasses import dataclass
from pathlib import Path
from random import choice

import re
import os

VERSION_PATCH: Final = 1
VERSION_MINOR: Final = 0
VERSION_MAJOR: Final = 1
VERSION: Final = f"{VERSION_MAJOR}.{VERSION_MINOR}.{VERSION_PATCH}"

SATCHEL_PATH: Final = f"{Path.home()}/.config/satchel/bookmarks.satchel"


@dataclass
class Bookmark:
    """Class that holds the url, and tags for a website"""
    url: str
    tags: List[str]

    def __repr__(self):
        return f"{self.url} {' '.join(self.tags)}"


def stripSchemeFromUrl(url: str) -> str:
    rurl = re.compile(r"https?://(www\.)?")
    return rurl.sub('', url).strip().strip('/')


def bookmarkFromString(line: str) -> Bookmark:
    return Bookmark(*splitToUrlAndTags(stripSchemeFromUrl(line)))


def splitToUrlAndTags(line: str) -> Tuple[str, List[str]]:
    line = line.rstrip()
    splits = line.split(' ')
    url = splits[0]
    tags = splits[1:] if len(splits) > 1 else []
    return (url, tags)


def parseCommandLineArguments():
    arg_parser = ArgumentParser(description="Book mark manager")
    arg_parser.add_argument(
        '-l', '--list', help="Lists all of the bookmarks in a grep'able form", action="store_true")
    arg_parser.add_argument(
        '-a', '--add', help="Add a new book mark with tags", type=str, nargs="*")
    arg_parser.add_argument(
        '-c', '--contains', help="Check to see if url is in bookmarks", type=str)
    arg_parser.add_argument(
        '-q', '--query', help="Query for website using tags", type=str, nargs="*")
    arg_parser.add_argument(
        '-r', '--rand', help="Get a random url using an optional list of tags", type=str, nargs="*")
    arg_parser.add_argument(
        '-u', '--url', help="Print the url without the tags", action="store_true")
    arg_parser.add_argument(
        '-v', '--version', help="Lists the version", action="store_true")
    return arg_parser.parse_args()


def parseBookmarksFile() -> List[Bookmark]:
    lines = []
    with open(SATCHEL_PATH, 'r') as f:
        lines = f.readlines()
    bookmarks = list(map(bookmarkFromString, lines))
    return bookmarks


def removeBookmarkWithUrl(bookmarks, url):
    for b in bookmarks:
        if stripSchemeFromUrl(url) in b.url:
            bookmarks.remove(b)


def addBookMark(bookmarks, url, tags=[]):
    if doesBookmarkExist(bookmarks, url):
        print("Url already exists, replace? ", end="")
        if input("(y/n): ") != 'y':
            return
        else:
            removeBookmarkWithUrl(bookmarks, url)
    bookmarks.append(Bookmark(url, tags))


def queryForBookmarksUsingTags(bookmarks: List[Bookmark], tags: List[str]) -> Optional[List[Bookmark]]:
    return list(filter(lambda bm: all(el in bm.tags for el in tags), bookmarks))


def doesBookmarkExist(bookmarks: List[Bookmark], url: str) -> bool:
    for b in bookmarks:
        if b.url in stripSchemeFromUrl(url):
            return True
    return False


if __name__ == "__main__":
    args: Final = parseCommandLineArguments()

    if not os.path.exists(f'{os.path.dirname(SATCHEL_PATH)}'):
        os.makedirs(f'{os.path.dirname(SATCHEL_PATH)}')

    bookmarks = parseBookmarksFile()

    if args.version:
        print(f"Version: {VERSION}")

    if args.list:
        for b in bookmarks:
            print(b.url if args.url else b)

    if args.add:
        addBookMark(bookmarks, *splitToUrlAndTags(' '.join(args.add)))

    if args.contains:
        print("1" if doesBookmarkExist(bookmarks, args.contains) else "0")

    if args.query:
        bs = queryForBookmarksUsingTags(bookmarks, args.query)
        for b in bs:
            print(b.url if args.url else b)

    if args.rand:
        bs = queryForBookmarksUsingTags(bookmarks, args.rand) if len(
            args.rand) > 0 else bookmarks
        b = choice(bs)
        print(b.url if args.url else b)

    with open(SATCHEL_PATH, "w") as f:
        urls = [f"{str(b)}\n" for b in bookmarks]
        f.writelines(urls)

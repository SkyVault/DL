import yaml

with (config.configdir / 'colors.yml').open() as f:
    yaml_data = yaml.load(f)
    def dict_attrs(obj, path=''):
        if isinstance(obj, dict):
            for k, v in obj.items():
                yield from dict_attrs(v, '{}.{}'.format(path, k) if path else k)
        else:
            yield path, obj
    for k, v in dict_attrs(yaml_data):
        config.set(k, v)

import subprocess

# Load existing settings
config.load_autoconfig()

config.bind("zR", "jseval Array.from(document.getElementsByClassName('expando-button')).forEach((b) => b.click())")

c.backend = 'webengine'

c.scrolling.smooth = True

c.colors.webpage.darkmode.algorithm = 'lightness-cielab'
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.bg = "black"

c.url.searchengines = {"DEFAULT": "https://google.com/search?q={}"}

from qutebrowser.api import interceptor

def filter_yt(info: interceptor.Request):
	"""Block the given request if necessary."""
	url = info.request_url
	if (url.host() == 'www.youtube.com' and
			url.path() == '/get_video_info' and
			'&adformat=' in url.query()):
		info.block()


interceptor.register(filter_yt)

# c.content.user_stylesheets = "solarized-dark-all-sites.css"

def getBookmarkedUrl():
    return subprocess.run(["dmenu-bookmark"], stdout=subprocess.PIPE).stdout.decode('utf-8')

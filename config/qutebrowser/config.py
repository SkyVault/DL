import dracula.draw
import subprocess

# Load existing settings
config.load_autoconfig()

config.bind("zR", "jseval Array.from(document.getElementsByClassName('expando-button')).forEach((b) => b.click())") 

dracula.draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8,
    }
})

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

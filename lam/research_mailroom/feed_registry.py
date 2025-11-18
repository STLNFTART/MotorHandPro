"""
Research Feed Registry
Top 50+ research institutions worldwide with their RSS feeds, APIs, and update channels
"""

from .mail_room import ResearchFeed

# ============================================================================
# UNIVERSITIES - Top Research Institutions
# ============================================================================

UNIVERSITY_FEEDS = [
    # MIT
    ResearchFeed(
        name="MIT CSAIL News",
        url="https://www.csail.mit.edu/news/rss.xml",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="MIT News - AI",
        url="https://news.mit.edu/topic/mitartificial-intelligence2-rss.xml",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Stanford
    ResearchFeed(
        name="Stanford AI Lab",
        url="https://ai.stanford.edu/blog/feed.xml",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="Stanford Engineering News",
        url="https://engineering.stanford.edu/news-and-stories/feed",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Carnegie Mellon
    ResearchFeed(
        name="CMU Robotics Institute",
        url="https://www.ri.cmu.edu/feed/",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="CMU AI News",
        url="https://www.cs.cmu.edu/news/feed",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Berkeley
    ResearchFeed(
        name="UC Berkeley AI Research",
        url="https://bair.berkeley.edu/blog/feed.xml",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Caltech
    ResearchFeed(
        name="Caltech News",
        url="https://www.caltech.edu/rss/news",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Oxford
    ResearchFeed(
        name="Oxford Computer Science",
        url="https://www.cs.ox.ac.uk/news/rss",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Cambridge
    ResearchFeed(
        name="Cambridge Computer Lab",
        url="https://www.cl.cam.ac.uk/news/feed/",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # ETH Zurich
    ResearchFeed(
        name="ETH Zurich Robotics",
        url="https://ethz.ch/en/news-and-events/eth-news/news/feeds/rss.xml",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Imperial College London
    ResearchFeed(
        name="Imperial College Computing",
        url="https://www.imperial.ac.uk/computing/news/rss/",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Georgia Tech
    ResearchFeed(
        name="Georgia Tech Computing",
        url="https://www.cc.gatech.edu/news/feed",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # University of Washington
    ResearchFeed(
        name="UW CSE News",
        url="https://www.cs.washington.edu/news/rss",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),

    # Princeton
    ResearchFeed(
        name="Princeton CS",
        url="https://www.cs.princeton.edu/news/feed",
        feed_type="rss",
        category="university",
        fetch_interval=3600
    ),
]

# ============================================================================
# NATIONAL LABORATORIES - Government Research
# ============================================================================

NATIONAL_LAB_FEEDS = [
    # NASA
    ResearchFeed(
        name="NASA Breaking News",
        url="https://www.nasa.gov/rss/dyn/breaking_news.rss",
        feed_type="rss",
        category="national_lab",
        fetch_interval=1800
    ),
    ResearchFeed(
        name="NASA Technology",
        url="https://www.nasa.gov/rss/dyn/technology.rss",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="NASA JPL",
        url="https://www.jpl.nasa.gov/news/rss/news.xml",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),

    # NIST
    ResearchFeed(
        name="NIST News",
        url="https://www.nist.gov/news-events/news/rss.xml",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),

    # DOE National Labs
    ResearchFeed(
        name="Oak Ridge National Lab",
        url="https://www.ornl.gov/news/rss.xml",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="Lawrence Livermore",
        url="https://www.llnl.gov/news/rss.xml",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="Los Alamos National Lab",
        url="https://discover.lanl.gov/news/feed/",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="Argonne National Lab",
        url="https://www.anl.gov/rss/all/news",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),

    # ESA (European)
    ResearchFeed(
        name="ESA News",
        url="https://www.esa.int/rssfeed/Our_Activities/Space_Engineering_Technology",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),

    # CERN
    ResearchFeed(
        name="CERN News",
        url="https://home.cern/news/news/feed",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),

    # JAXA (Japan)
    ResearchFeed(
        name="JAXA News",
        url="https://global.jaxa.jp/feed/press_e.xml",
        feed_type="rss",
        category="national_lab",
        fetch_interval=3600
    ),
]

# ============================================================================
# PRIVATE RESEARCH INSTITUTIONS
# ============================================================================

PRIVATE_RESEARCH_FEEDS = [
    # arXiv categories relevant to MotorHandPro
    ResearchFeed(
        name="arXiv - Robotics",
        url="http://export.arxiv.org/rss/cs.RO",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="arXiv - Systems & Control",
        url="http://export.arxiv.org/rss/eess.SY",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="arXiv - Machine Learning",
        url="http://export.arxiv.org/rss/cs.LG",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="arXiv - Artificial Intelligence",
        url="http://export.arxiv.org/rss/cs.AI",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="arXiv - Computer Vision",
        url="http://export.arxiv.org/rss/cs.CV",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),

    # OpenAI
    ResearchFeed(
        name="OpenAI Research",
        url="https://openai.com/blog/rss.xml",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),

    # Google DeepMind
    ResearchFeed(
        name="DeepMind Research",
        url="https://deepmind.google/feed/basic/",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),

    # Microsoft Research
    ResearchFeed(
        name="Microsoft Research Blog",
        url="https://www.microsoft.com/en-us/research/feed/",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),

    # Meta AI
    ResearchFeed(
        name="Meta AI Research",
        url="https://ai.facebook.com/blog/rss/",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),

    # Allen Institute
    ResearchFeed(
        name="Allen Institute AI",
        url="https://allenai.org/blog/feed",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),

    # IBM Research
    ResearchFeed(
        name="IBM Research Blog",
        url="https://research.ibm.com/blog/rss",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),

    # Sony AI
    ResearchFeed(
        name="Sony AI Blog",
        url="https://ai.sony/feed/",
        feed_type="rss",
        category="private",
        fetch_interval=3600
    ),
]

# ============================================================================
# INDUSTRY RESEARCH LABS
# ============================================================================

INDUSTRY_FEEDS = [
    # Tesla
    ResearchFeed(
        name="Tesla AI & Robotics",
        url="https://www.tesla.com/blog/rss",
        feed_type="rss",
        category="industry",
        fetch_interval=3600
    ),

    # Boston Dynamics
    ResearchFeed(
        name="Boston Dynamics News",
        url="https://www.bostondynamics.com/news/rss",
        feed_type="rss",
        category="industry",
        fetch_interval=3600
    ),

    # SpaceX
    ResearchFeed(
        name="SpaceX Updates",
        url="https://www.spacex.com/feeds/news",
        feed_type="rss",
        category="industry",
        fetch_interval=1800
    ),

    # Blue Origin
    ResearchFeed(
        name="Blue Origin News",
        url="https://www.blueorigin.com/news/rss",
        feed_type="rss",
        category="industry",
        fetch_interval=3600
    ),
]

# ============================================================================
# BIOMEDICAL RESEARCH
# ============================================================================

BIOMEDICAL_FEEDS = [
    # bioRxiv
    ResearchFeed(
        name="bioRxiv - Bioengineering",
        url="https://connect.biorxiv.org/biorxiv_xml.php?subject=bioengineering",
        feed_type="rss",
        category="biomedical",
        fetch_interval=3600
    ),
    ResearchFeed(
        name="bioRxiv - Neuroscience",
        url="https://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience",
        feed_type="rss",
        category="biomedical",
        fetch_interval=3600
    ),

    # NIH
    ResearchFeed(
        name="NIH News",
        url="https://www.nih.gov/feeds/news.xml",
        feed_type="rss",
        category="biomedical",
        fetch_interval=3600
    ),

    # Mayo Clinic
    ResearchFeed(
        name="Mayo Clinic Research",
        url="https://www.mayo.edu/research/feed",
        feed_type="rss",
        category="biomedical",
        fetch_interval=3600
    ),
]

# ============================================================================
# JOURNALS & PUBLICATIONS
# ============================================================================

JOURNAL_FEEDS = [
    # Nature
    ResearchFeed(
        name="Nature - Latest Research",
        url="https://www.nature.com/nature.rss",
        feed_type="rss",
        category="journal",
        fetch_interval=3600
    ),

    # Science Magazine
    ResearchFeed(
        name="Science - Latest News",
        url="https://www.science.org/action/showFeed?type=etoc&feed=rss&jc=science",
        feed_type="rss",
        category="journal",
        fetch_interval=3600
    ),

    # IEEE Spectrum
    ResearchFeed(
        name="IEEE Spectrum - Robotics",
        url="https://spectrum.ieee.org/feeds/feed.rss",
        feed_type="rss",
        category="journal",
        fetch_interval=3600
    ),

    # ACM TechNews
    ResearchFeed(
        name="ACM TechNews",
        url="https://technews.acm.org/news.rss",
        feed_type="rss",
        category="journal",
        fetch_interval=3600
    ),
]

# ============================================================================
# GOVERNMENT & DEFENSE
# ============================================================================

GOVERNMENT_FEEDS = [
    # DARPA
    ResearchFeed(
        name="DARPA News",
        url="https://www.darpa.mil/news/rss",
        feed_type="rss",
        category="government",
        fetch_interval=3600
    ),

    # NSF
    ResearchFeed(
        name="NSF News",
        url="https://www.nsf.gov/rss/rss_www_news.xml",
        feed_type="rss",
        category="government",
        fetch_interval=3600
    ),

    # DOD
    ResearchFeed(
        name="Defense Innovation Unit",
        url="https://www.diu.mil/latest/rss",
        feed_type="rss",
        category="government",
        fetch_interval=3600
    ),
]

# ============================================================================
# MASTER FEED REGISTRY
# ============================================================================

ALL_FEEDS = (
    UNIVERSITY_FEEDS +
    NATIONAL_LAB_FEEDS +
    PRIVATE_RESEARCH_FEEDS +
    INDUSTRY_FEEDS +
    BIOMEDICAL_FEEDS +
    JOURNAL_FEEDS +
    GOVERNMENT_FEEDS
)


def get_all_feeds() -> list:
    """Get all registered research feeds"""
    return ALL_FEEDS


def get_feeds_by_category(category: str) -> list:
    """Get feeds by category"""
    return [feed for feed in ALL_FEEDS if feed.category == category]


def get_feed_by_name(name: str) -> ResearchFeed:
    """Get specific feed by name"""
    for feed in ALL_FEEDS:
        if feed.name == name:
            return feed
    return None


# Feed statistics
def get_feed_stats() -> dict:
    """Get statistics about registered feeds"""
    return {
        "total_feeds": len(ALL_FEEDS),
        "by_category": {
            "university": len(UNIVERSITY_FEEDS),
            "national_lab": len(NATIONAL_LAB_FEEDS),
            "private": len(PRIVATE_RESEARCH_FEEDS),
            "industry": len(INDUSTRY_FEEDS),
            "biomedical": len(BIOMEDICAL_FEEDS),
            "journal": len(JOURNAL_FEEDS),
            "government": len(GOVERNMENT_FEEDS),
        },
        "categories": list(set(feed.category for feed in ALL_FEEDS))
    }

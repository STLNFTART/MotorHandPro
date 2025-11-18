"""
Research Mail Room
Intelligence aggregation and synergy detection for research collaboration
"""

from .mail_room import MailRoom, ResearchItem, ResearchFeed, research_mailroom
from .feed_registry import (
    get_all_feeds,
    get_feeds_by_category,
    get_feed_by_name,
    get_feed_stats,
    ALL_FEEDS
)

__all__ = [
    "MailRoom",
    "ResearchItem",
    "ResearchFeed",
    "research_mailroom",
    "get_all_feeds",
    "get_feeds_by_category",
    "get_feed_by_name",
    "get_feed_stats",
    "ALL_FEEDS"
]

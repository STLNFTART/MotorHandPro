"""
Research Mail Room - Intelligence Aggregation System

Collects newsletters, RSS feeds, repository updates, and research announcements
from top laboratories worldwide to identify collaboration opportunities and synergies.
"""

import asyncio
import feedparser
import hashlib
from typing import Dict, Any, List, Optional, Set
from datetime import datetime, timedelta
from dataclasses import dataclass, field
import logging
import httpx
from bs4 import BeautifulSoup
import re

logger = logging.getLogger(__name__)


@dataclass
class ResearchItem:
    """A research item (paper, announcement, update, etc.)"""
    id: str
    source: str  # e.g., "MIT CSAIL", "arXiv", "NASA"
    category: str  # e.g., "paper", "news", "dataset", "code"
    title: str
    summary: str
    url: str
    published: datetime
    authors: List[str] = field(default_factory=list)
    tags: List[str] = field(default_factory=list)
    keywords: List[str] = field(default_factory=list)
    raw_data: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ResearchFeed:
    """Configuration for a research feed"""
    name: str
    url: str
    feed_type: str  # "rss", "atom", "api", "scrape"
    category: str  # "university", "national_lab", "private", "government"
    enabled: bool = True
    last_fetched: Optional[datetime] = None
    fetch_interval: int = 3600  # seconds


class MailRoom:
    """Central research intelligence aggregation system"""

    def __init__(self):
        self.feeds: Dict[str, ResearchFeed] = {}
        self.items: Dict[str, ResearchItem] = {}  # id -> item
        self.seen_ids: Set[str] = set()
        self.keywords_of_interest: Set[str] = {
            # MotorHandPro related
            "primal logic", "control theory", "robotics", "autonomous",
            "quantum control", "orbital mechanics", "satellite",

            # Technologies
            "machine learning", "reinforcement learning", "neural networks",
            "computer vision", "sensor fusion", "slam",

            # Domains
            "aerospace", "biomedical", "prosthetics", "exoskeletons",
            "drones", "uav", "space", "constellation",

            # Methods
            "optimization", "stability", "lyapunov", "adaptive control",
            "model predictive", "kalman filter", "pid control"
        }

        self.synergy_scores: Dict[str, float] = {}  # item_id -> score
        self._running = False
        self._task: Optional[asyncio.Task] = None

    def register_feed(self, feed: ResearchFeed):
        """Register a research feed to monitor"""
        self.feeds[feed.name] = feed
        logger.info(f"Registered feed: {feed.name} ({feed.url})")

    async def start(self):
        """Start the mail room monitoring"""
        if self._running:
            logger.warning("Mail room already running")
            return

        self._running = True
        self._task = asyncio.create_task(self._monitoring_loop())
        logger.info("🔬 Research Mail Room started")

    async def stop(self):
        """Stop the mail room monitoring"""
        self._running = False
        if self._task:
            self._task.cancel()
            try:
                await self._task
            except asyncio.CancelledError:
                pass
        logger.info("🛑 Research Mail Room stopped")

    async def _monitoring_loop(self):
        """Main monitoring loop"""
        while self._running:
            try:
                # Fetch all enabled feeds
                tasks = [
                    self._fetch_feed(feed)
                    for feed in self.feeds.values()
                    if feed.enabled and self._should_fetch(feed)
                ]

                if tasks:
                    results = await asyncio.gather(*tasks, return_exceptions=True)

                    # Process results
                    total_new = sum(r for r in results if isinstance(r, int))
                    if total_new > 0:
                        logger.info(f"📬 Collected {total_new} new research items")

                        # Analyze for synergies
                        await self._analyze_synergies()

                # Wait before next cycle
                await asyncio.sleep(60)  # Check every minute

            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Error in monitoring loop: {e}")
                await asyncio.sleep(60)

    def _should_fetch(self, feed: ResearchFeed) -> bool:
        """Check if feed should be fetched now"""
        if not feed.last_fetched:
            return True

        elapsed = (datetime.now() - feed.last_fetched).total_seconds()
        return elapsed >= feed.fetch_interval

    async def _fetch_feed(self, feed: ResearchFeed) -> int:
        """Fetch and process a single feed"""
        try:
            logger.debug(f"Fetching feed: {feed.name}")

            if feed.feed_type in ["rss", "atom"]:
                new_items = await self._fetch_rss(feed)
            elif feed.feed_type == "api":
                new_items = await self._fetch_api(feed)
            elif feed.feed_type == "scrape":
                new_items = await self._fetch_scrape(feed)
            else:
                logger.warning(f"Unknown feed type: {feed.feed_type}")
                return 0

            # Update last fetched time
            feed.last_fetched = datetime.now()

            # Store new items
            for item in new_items:
                if item.id not in self.seen_ids:
                    self.items[item.id] = item
                    self.seen_ids.add(item.id)

            return len(new_items)

        except Exception as e:
            logger.error(f"Error fetching feed {feed.name}: {e}")
            return 0

    async def _fetch_rss(self, feed: ResearchFeed) -> List[ResearchItem]:
        """Fetch RSS/Atom feed"""
        try:
            async with httpx.AsyncClient() as client:
                response = await client.get(feed.url, timeout=30.0)
                response.raise_for_status()

            parsed = feedparser.parse(response.text)
            items = []

            for entry in parsed.entries:
                # Generate unique ID
                item_id = self._generate_id(feed.name, entry.get("link", entry.get("id", "")))

                if item_id in self.seen_ids:
                    continue

                # Extract data
                published = entry.get("published_parsed")
                if published:
                    published = datetime(*published[:6])
                else:
                    published = datetime.now()

                # Extract authors
                authors = []
                if "authors" in entry:
                    authors = [a.get("name", "") for a in entry.authors]
                elif "author" in entry:
                    authors = [entry.author]

                # Create item
                item = ResearchItem(
                    id=item_id,
                    source=feed.name,
                    category="rss_entry",
                    title=entry.get("title", ""),
                    summary=entry.get("summary", ""),
                    url=entry.get("link", ""),
                    published=published,
                    authors=authors,
                    tags=entry.get("tags", []),
                    raw_data=dict(entry)
                )

                # Extract keywords
                item.keywords = self._extract_keywords(item.title + " " + item.summary)

                items.append(item)

            return items

        except Exception as e:
            logger.error(f"Error parsing RSS feed {feed.name}: {e}")
            return []

    async def _fetch_api(self, feed: ResearchFeed) -> List[ResearchItem]:
        """Fetch from API endpoint"""
        # This would be customized per API
        # For now, return empty list
        return []

    async def _fetch_scrape(self, feed: ResearchFeed) -> List[ResearchItem]:
        """Scrape website for updates"""
        try:
            async with httpx.AsyncClient() as client:
                response = await client.get(feed.url, timeout=30.0)
                response.raise_for_status()

            soup = BeautifulSoup(response.text, "html.parser")

            # This would be customized per site
            # Example: find article elements
            items = []
            for article in soup.find_all("article")[:10]:  # Limit to 10
                title_elem = article.find(["h1", "h2", "h3"])
                link_elem = article.find("a")

                if not title_elem or not link_elem:
                    continue

                item_id = self._generate_id(feed.name, link_elem.get("href", ""))
                if item_id in self.seen_ids:
                    continue

                item = ResearchItem(
                    id=item_id,
                    source=feed.name,
                    category="web_article",
                    title=title_elem.get_text(strip=True),
                    summary=article.get_text(strip=True)[:500],
                    url=link_elem.get("href", ""),
                    published=datetime.now(),
                    keywords=[]
                )

                item.keywords = self._extract_keywords(item.title + " " + item.summary)
                items.append(item)

            return items

        except Exception as e:
            logger.error(f"Error scraping {feed.name}: {e}")
            return []

    def _generate_id(self, source: str, identifier: str) -> str:
        """Generate unique ID for item"""
        content = f"{source}:{identifier}"
        return hashlib.md5(content.encode()).hexdigest()

    def _extract_keywords(self, text: str) -> List[str]:
        """Extract keywords from text"""
        text_lower = text.lower()
        found_keywords = []

        for keyword in self.keywords_of_interest:
            if keyword.lower() in text_lower:
                found_keywords.append(keyword)

        return found_keywords

    async def _analyze_synergies(self):
        """Analyze research items for synergies with MotorHandPro"""
        for item_id, item in self.items.items():
            if item_id in self.synergy_scores:
                continue  # Already analyzed

            score = self._calculate_synergy_score(item)
            self.synergy_scores[item_id] = score

            # If high synergy, emit event
            if score > 0.7:  # High synergy threshold
                await self._emit_synergy_event(item, score)

    def _calculate_synergy_score(self, item: ResearchItem) -> float:
        """Calculate synergy score (0.0 - 1.0)"""
        score = 0.0

        # Keyword matching (0-0.5 points)
        if item.keywords:
            keyword_score = len(item.keywords) / len(self.keywords_of_interest)
            score += min(keyword_score, 0.5)

        # Source weighting (0-0.2 points)
        high_value_sources = ["MIT CSAIL", "Stanford AI Lab", "NASA", "arXiv", "DARPA"]
        if any(source in item.source for source in high_value_sources):
            score += 0.2

        # Recency (0-0.3 points)
        age_days = (datetime.now() - item.published).days
        if age_days < 7:
            score += 0.3
        elif age_days < 30:
            score += 0.2
        elif age_days < 90:
            score += 0.1

        return min(score, 1.0)

    async def _emit_synergy_event(self, item: ResearchItem, score: float):
        """Emit high-synergy event for notification"""
        logger.info(f"🎯 High synergy detected ({score:.2f}): {item.title}")

        # This would integrate with the webhook manager
        from lam.api.webhook_manager import webhook_manager
        await webhook_manager.emit("research.synergy_detected", {
            "item_id": item.id,
            "source": item.source,
            "title": item.title,
            "summary": item.summary,
            "url": item.url,
            "keywords": item.keywords,
            "synergy_score": score,
            "published": item.published.isoformat()
        })

    def search(self, query: str, limit: int = 50) -> List[ResearchItem]:
        """Search collected research items"""
        query_lower = query.lower()
        results = []

        for item in self.items.values():
            if (query_lower in item.title.lower() or
                query_lower in item.summary.lower() or
                any(query_lower in keyword.lower() for keyword in item.keywords)):
                results.append(item)

        # Sort by synergy score and recency
        results.sort(key=lambda x: (
            self.synergy_scores.get(x.id, 0),
            x.published
        ), reverse=True)

        return results[:limit]

    def get_recent(self, days: int = 7, min_synergy: float = 0.0) -> List[ResearchItem]:
        """Get recent items with minimum synergy score"""
        cutoff = datetime.now() - timedelta(days=days)
        results = []

        for item in self.items.values():
            if item.published >= cutoff:
                synergy = self.synergy_scores.get(item.id, 0)
                if synergy >= min_synergy:
                    results.append((item, synergy))

        # Sort by synergy score
        results.sort(key=lambda x: x[1], reverse=True)
        return [item for item, _ in results]

    def get_by_source(self, source: str) -> List[ResearchItem]:
        """Get all items from a specific source"""
        return [
            item for item in self.items.values()
            if source.lower() in item.source.lower()
        ]

    def get_synergy_report(self, min_score: float = 0.5) -> Dict[str, Any]:
        """Generate synergy report"""
        high_synergy = [
            (item, self.synergy_scores.get(item.id, 0))
            for item in self.items.values()
            if self.synergy_scores.get(item.id, 0) >= min_score
        ]

        high_synergy.sort(key=lambda x: x[1], reverse=True)

        return {
            "total_items": len(self.items),
            "high_synergy_count": len(high_synergy),
            "min_synergy_threshold": min_score,
            "top_opportunities": [
                {
                    "title": item.title,
                    "source": item.source,
                    "url": item.url,
                    "synergy_score": score,
                    "keywords": item.keywords,
                    "published": item.published.isoformat()
                }
                for item, score in high_synergy[:20]
            ],
            "synergy_by_source": self._group_by_source(high_synergy)
        }

    def _group_by_source(self, items: List[tuple]) -> Dict[str, int]:
        """Group items by source"""
        by_source = {}
        for item, score in items:
            by_source[item.source] = by_source.get(item.source, 0) + 1
        return by_source


# Global mail room instance
research_mailroom = MailRoom()

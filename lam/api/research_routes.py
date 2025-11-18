"""
Research Mail Room API Routes
FastAPI endpoints for research intelligence and collaboration discovery
"""

from typing import Dict, Any, List, Optional
from datetime import datetime, timedelta
from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel

from lam.research_mailroom import (
    research_mailroom,
    get_all_feeds,
    get_feeds_by_category,
    get_feed_stats
)


# Pydantic models
class SynergyReportRequest(BaseModel):
    """Request for synergy report"""
    min_score: float = 0.5
    days: int = 30


class SearchRequest(BaseModel):
    """Search request"""
    query: str
    limit: int = 50


# Router
router = APIRouter(prefix="/research", tags=["research"])


@router.get("/status")
async def get_mail_room_status():
    """
    Get research mail room status

    Returns current statistics about collected research items,
    active feeds, and monitoring status.
    """
    feed_stats = get_feed_stats()

    return {
        "status": "running" if research_mailroom._running else "stopped",
        "feeds": feed_stats,
        "items_collected": len(research_mailroom.items),
        "unique_sources": len(set(item.source for item in research_mailroom.items.values())),
        "latest_item": max(
            (item.published for item in research_mailroom.items.values()),
            default=None
        ),
        "monitoring_since": research_mailroom.feeds[list(research_mailroom.feeds.keys())[0]].last_fetched
        if research_mailroom.feeds else None
    }


@router.post("/start")
async def start_mail_room():
    """
    Start the research mail room monitoring

    Begins automated collection of research updates from all registered feeds.
    """
    if research_mailroom._running:
        return {"status": "already_running", "message": "Mail room is already monitoring"}

    await research_mailroom.start()

    return {
        "status": "started",
        "message": "Research mail room monitoring started",
        "feeds_count": len(research_mailroom.feeds),
        "timestamp": datetime.now().isoformat()
    }


@router.post("/stop")
async def stop_mail_room():
    """
    Stop the research mail room monitoring

    Stops automated collection but retains all collected items.
    """
    if not research_mailroom._running:
        return {"status": "already_stopped", "message": "Mail room is not running"}

    await research_mailroom.stop()

    return {
        "status": "stopped",
        "message": "Research mail room monitoring stopped",
        "items_collected": len(research_mailroom.items),
        "timestamp": datetime.now().isoformat()
    }


@router.get("/feeds")
async def list_feeds(category: Optional[str] = None):
    """
    List all registered research feeds

    Optionally filter by category:
    - university
    - national_lab
    - private
    - industry
    - biomedical
    - journal
    - government
    """
    if category:
        feeds = get_feeds_by_category(category)
    else:
        feeds = get_all_feeds()

    return {
        "feeds": [
            {
                "name": feed.name,
                "url": feed.url,
                "category": feed.category,
                "feed_type": feed.feed_type,
                "enabled": feed.enabled,
                "last_fetched": feed.last_fetched.isoformat() if feed.last_fetched else None,
                "fetch_interval": feed.fetch_interval
            }
            for feed in feeds
        ],
        "total": len(feeds)
    }


@router.post("/search")
async def search_research(request: SearchRequest):
    """
    Search collected research items

    Search across titles, summaries, and keywords of all collected items.
    """
    results = research_mailroom.search(request.query, limit=request.limit)

    return {
        "query": request.query,
        "results": [
            {
                "id": item.id,
                "source": item.source,
                "category": item.category,
                "title": item.title,
                "summary": item.summary[:500],  # Truncate
                "url": item.url,
                "published": item.published.isoformat(),
                "authors": item.authors,
                "keywords": item.keywords,
                "synergy_score": research_mailroom.synergy_scores.get(item.id, 0.0)
            }
            for item in results
        ],
        "total": len(results)
    }


@router.get("/recent")
async def get_recent_research(
    days: int = Query(7, ge=1, le=90),
    min_synergy: float = Query(0.0, ge=0.0, le=1.0)
):
    """
    Get recent research items

    Returns items from the last N days with minimum synergy score.
    """
    items = research_mailroom.get_recent(days=days, min_synergy=min_synergy)

    return {
        "days": days,
        "min_synergy": min_synergy,
        "items": [
            {
                "id": item.id,
                "source": item.source,
                "title": item.title,
                "summary": item.summary[:300],
                "url": item.url,
                "published": item.published.isoformat(),
                "keywords": item.keywords,
                "synergy_score": research_mailroom.synergy_scores.get(item.id, 0.0)
            }
            for item in items
        ],
        "total": len(items)
    }


@router.get("/synergies")
async def get_synergy_report(min_score: float = Query(0.5, ge=0.0, le=1.0)):
    """
    Get collaboration synergy report

    Identifies research items with high synergy scores indicating
    potential collaboration opportunities with MotorHandPro.

    Synergy scoring considers:
    - Keyword matching with MotorHandPro domains
    - Source reputation
    - Recency of research
    """
    report = research_mailroom.get_synergy_report(min_score=min_score)

    return {
        "report": report,
        "generated_at": datetime.now().isoformat()
    }


@router.get("/sources/{source}")
async def get_by_source(source: str):
    """
    Get all items from a specific research source

    Examples:
    - MIT CSAIL
    - NASA
    - arXiv - Robotics
    - Stanford AI Lab
    """
    items = research_mailroom.get_by_source(source)

    return {
        "source": source,
        "items": [
            {
                "id": item.id,
                "title": item.title,
                "summary": item.summary[:300],
                "url": item.url,
                "published": item.published.isoformat(),
                "keywords": item.keywords,
                "synergy_score": research_mailroom.synergy_scores.get(item.id, 0.0)
            }
            for item in items
        ],
        "total": len(items)
    }


@router.get("/keywords")
async def get_keywords_of_interest():
    """
    Get list of keywords used for synergy detection

    These keywords define areas of interest for MotorHandPro
    and are used to identify relevant research.
    """
    return {
        "keywords": sorted(list(research_mailroom.keywords_of_interest)),
        "total": len(research_mailroom.keywords_of_interest)
    }


@router.post("/keywords/add")
async def add_keyword(keyword: str):
    """
    Add a keyword to the synergy detection list

    New keyword will be used to identify relevant research going forward.
    """
    keyword_lower = keyword.lower()
    if keyword_lower in research_mailroom.keywords_of_interest:
        return {"status": "exists", "message": f"Keyword '{keyword}' already exists"}

    research_mailroom.keywords_of_interest.add(keyword_lower)

    return {
        "status": "added",
        "keyword": keyword_lower,
        "total_keywords": len(research_mailroom.keywords_of_interest)
    }


@router.get("/trending")
async def get_trending_topics(days: int = Query(7, ge=1, le=30)):
    """
    Get trending research topics

    Analyzes recent items to identify frequently appearing keywords
    and research themes.
    """
    cutoff = datetime.now() - timedelta(days=days)
    recent_items = [
        item for item in research_mailroom.items.values()
        if item.published >= cutoff
    ]

    # Count keyword frequencies
    keyword_counts = {}
    for item in recent_items:
        for keyword in item.keywords:
            keyword_counts[keyword] = keyword_counts.get(keyword, 0) + 1

    # Sort by frequency
    trending = sorted(keyword_counts.items(), key=lambda x: x[1], reverse=True)

    return {
        "days": days,
        "total_items_analyzed": len(recent_items),
        "trending_topics": [
            {"keyword": keyword, "count": count, "percentage": (count / len(recent_items)) * 100}
            for keyword, count in trending[:20]
        ]
    }


@router.get("/analytics")
async def get_analytics():
    """
    Get research collection analytics

    Provides insights into:
    - Collection rates
    - Source distribution
    - Synergy distribution
    - Category breakdown
    """
    items = list(research_mailroom.items.values())

    # Source distribution
    source_dist = {}
    for item in items:
        source_dist[item.source] = source_dist.get(item.source, 0) + 1

    # Category distribution
    category_dist = {}
    for item in items:
        category_dist[item.category] = category_dist.get(item.category, 0) + 1

    # Synergy score distribution
    synergy_dist = {
        "high (>0.7)": len([s for s in research_mailroom.synergy_scores.values() if s > 0.7]),
        "medium (0.4-0.7)": len([s for s in research_mailroom.synergy_scores.values() if 0.4 <= s <= 0.7]),
        "low (<0.4)": len([s for s in research_mailroom.synergy_scores.values() if s < 0.4])
    }

    # Collection timeline (last 30 days)
    timeline = {}
    for item in items:
        date_key = item.published.strftime("%Y-%m-%d")
        timeline[date_key] = timeline.get(date_key, 0) + 1

    return {
        "total_items": len(items),
        "sources": {
            "distribution": source_dist,
            "unique_count": len(source_dist)
        },
        "categories": category_dist,
        "synergy_scores": synergy_dist,
        "collection_timeline": dict(sorted(timeline.items())[-30:]),  # Last 30 days
        "average_synergy": sum(research_mailroom.synergy_scores.values()) / max(len(research_mailroom.synergy_scores), 1),
        "generated_at": datetime.now().isoformat()
    }

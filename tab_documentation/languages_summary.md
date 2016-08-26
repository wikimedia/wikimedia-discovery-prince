# Traffic to Wikipedia from Wikipedia.org Portal

This dashboard shows the number of clicks (and users) to Wikipedia (across all the languages it is in) as recorded by our [Portal event logging](https://meta.wikimedia.org/wiki/Schema:WikipediaPortal), which users are randomly selected into at a sampling rate of 1 in 200 -- 0.5%. When the user comes to wikipedia.org and are randomly selected to be anonymously tracked via event logging, we set a timer of 15 minutes. Every time the user comes back to the page (lands), the timer is reset. After 15 minutes of not returning, the user's session is no longer tracked. For this reason, a single session may have one or dozens (sometimes hundreds!) of visits, each with a click or multiple clicks. We have seen sessions with as many as hundred clicks following the first and only landing. In cases where almost every session only has a single click associated with it, the graphs for *clicks* and *users* will look VERY similar, if not exactly the same.

When looking at clicks, you can view counts or proportions to see how much of the traffic comes from the three sections:

- **search**: wikipedia.org visitors can search Wikipedia's in different languages and end up on specific articles and we log the language of the Wikipedia they visited. If the visitor did not find a specific article during their initial query from the search metadata that is displayed, or by hitting 'Enter', they will be redirected to a default search results page in the language that they searched in (even if they changed the language in the small dropdown in the search box while on the Portal). However, at this time the search-box language selection change is not logged.
- **primary**: the links around the Wikipedia globe logo, which are dynamically placed and sorted according to each visitor's browser's language preferences.
- **secondary**: the links below the Wikipedia globe logo, which the wikipedia.org visitor can use to find a version of Wikipedia in any of the almost 300 languages.

**Note**: Sister project link clickthroughs are not tracked on this page, see [this page](http://discovery.wmflabs.org/portal/#most_common) for more info.

Until 27 June 2016, traffic to Wikipedias was almost evenly split between people searching and people using the primary links. Since then, search has been the more dominant section. However, this is due to a rise in Portal visitors searching EnWiki from the Portal. If we exclude EnWiki from the total, we see that clicks on primary links consistently account for 50-60% of the traffic to Wikipedias in languages other than English, searches account for ~35% of the traffic, and clicks on secondary links account for ~8% of the traffic.

## Outages and notes

- **A**: Languages visited data backfilled
    1. The data we used for a retrospective study of Portal deployments started on 16 November 2016, although there were filters applied to the data used in the analysis. Specifically, known spiders were excluded and only data from the first 10 visits per session was kept for data storage space reasons.
    2. When we began work on this part of the dashboard, we could only backfill data from 2016-05-10 due to the 90-day restriction our event logging system has. Therefore, we had to use the previously saved (slightly filtered) data to backfill visited language counts from November 16th to May 9th. We checked how the filtered data (post May 10th) compared to the unfiltered data and some counts were off by 1-8 clicks, hence why we are noting the difference here.

## Questions, bug reports, and feature suggestions

For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  An experimental dashboard for looking at patterns of <a href= "https://www.mediawiki.org/wiki/Wikipedia.org_Portal">Wikipedia.org Portal</a> usage pertaining to users' languages. See <a href="https://phabricator.wikimedia.org/T140816">T140816</a> for more information.
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#languages_summary">
    http://discovery.wmflabs.org/portal/#languages_summary
  </a>
</p>

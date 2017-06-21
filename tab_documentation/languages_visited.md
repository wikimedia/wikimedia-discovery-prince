# Traffic to Wikipedia in a particular language

This dashboard shows the number of clicks (and users) from Wikipedia.org Portal to Wikipedia in any language (that the user may be interested in) as recorded by our [Portal event logging](https://meta.wikimedia.org/wiki/Schema:WikipediaPortal), which users are randomly selected into at a sampling rate of 1 in 200 -- 0.5%. When the user comes to wikipedia.org and are randomly selected to be anonymously tracked via event logging, we set a timer of 15 minutes. Every time the user comes back to the page (lands), the timer is reset. After 15 minutes of not returning, the user's session is no longer tracked. For this reason, a single session may have one or dozens (sometimes hundreds!) of visits, each with a click or multiple clicks. We have seen sessions with as many as hundred clicks following the first and only landing. In cases where almost every session only has a single click associated with it, the graphs for *clicks* and *users* will look VERY similar, if not exactly the same.

While the [summary panel](http://discovery-experimental.wmflabs.org/portal/#summary) shows the total traffic from the Portal to Wikipedia across all the available languages, this panel allows you to view traffic from the Portal to a specific language and compare several languages. There are several sorting options to choose from, which affects the order of the languages listed when you click to select additional languages. Previously selected languages can be removed with the backspace key. When switching from one sorting option to another, the selected language(s) may change because they might not be part of the new subset of languages (e.g. switching from "Top 10" to "Bottom 50"), in which case the first language in the list will be selected by default.

When looking at clicks, you can view counts or proportions to see how much of the traffic comes from the three sections:

- **search**: wikipedia.org visitors can search Wikipedia's in different languages and end up on specific articles and we log the language of the Wikipedia they visited. If the visitor did not find a specific article during their initial query from the search metadata that is displayed, or by hitting 'Enter', they will be redirected to a default search results page in the language that they searched in (even if they changed the language in the small dropdown in the search box while on the Portal). However, at this time the search-box language selection change is not logged.
- **primary**: the links around the Wikipedia globe logo, which are dynamically placed and sorted according to each visitor's browser's language preferences.
- **secondary**: the links below the Wikipedia globe logo, which the wikipedia.org visitor can use to find a version of Wikipedia in any of the almost 300 languages.

**Note**: Sister project link clickthroughs are not tracked on this page, see [this page](http://discovery.wmflabs.org/portal/#most_common) for more info.

Viewing count/proportional breakdown of clicks is only available when a single language is selected. When multiple languages are selected, only the total number of clicks of the three sections is shown for each language. When multiple languages are selected (e.g. Top 10 or Bottom 50), the order they appear in inside the legend corresponds to the total number of clicks (or users), from highest to lowest. If several languages have the same number of historical clicks/users, they are then sorted further alphabetically by name.

By far, the English Wikipedia is the most visited one. Majority of the traffic to EnWiki is from Portal visitors searching (consistently >50% and as high as 70%), while *primary link* clicks account for 30-40% of the traffic to EnWiki from the Portal.

## Outages and notes

* '__A__': Languages visited data backfilled
    1. The data we used for a retrospective study of Portal deployments started on 16 November 2016, although there were filters applied to the data used in the analysis. Specifically, known spiders were excluded and only data from the first 10 visits per session was kept for data storage space reasons.
    2. When we began work on this part of the dashboard, we could only backfill data from 2016-05-10 due to the 90-day restriction our event logging system has. Therefore, we had to use the previously saved (slightly filtered) data to backfill visited language counts from November 16th to May 9th. We checked how the filtered data (post May 10th) compared to the unfiltered data and some counts were off by 1-8 clicks, hence why we are noting the difference here.
* '__B__' (2016-09-13): Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.
* '__R__': on 2017-01-01 we started calculating all of Discovery's metrics using a new version of [our data retrieval and processing codebase](https://phabricator.wikimedia.org/diffusion/WDGO/) that we migrated to [Wikimedia Analytics](https://www.mediawiki.org/wiki/Analytics)' [Reportupdater infrastructure](https://wikitech.wikimedia.org/wiki/Analytics/Reportupdater). See [T150915](https://phabricator.wikimedia.org/T150915) for more details.

## Questions, bug reports, and feature suggestions

For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question) or [Chelsy](mailto:cxie@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small;">
  <strong>Link to this dashboard:</strong> <a href="https://discovery.wmflabs.org/portal/#languages_visited">https://discovery.wmflabs.org/portal/#languages_visited</a>
  | Page is available under <a href="https://creativecommons.org/licenses/by-sa/3.0/" title="Creative Commons Attribution-ShareAlike License">CC-BY-SA 3.0</a>
  | <a href="https://phabricator.wikimedia.org/diffusion/WDPR/" title="Wikipedia.org Portal Dashboard source code repository">Code</a> is licensed under <a href="https://phabricator.wikimedia.org/diffusion/WDPR/browse/master/LICENSE.md" title="MIT License">MIT</a>
  | Part of <a href="https://discovery.wmflabs.org/">Discovery Dashboards</a>
</p>

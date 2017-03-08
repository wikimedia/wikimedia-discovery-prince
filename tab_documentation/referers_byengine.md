Traffic to Wikipedia Portal from external search engines, broken down
=======

This dashboard simply breaks down the [summary data](http://discovery.wmflabs.org/portal/#referrals_summary) to investigate how much of search engine traffic is coming from each engine individually.

General trends
------

On average, Google (the largest engine in our dataset) accounts for 80-81% of the search-engine-referred traffic. Since search-engine-referred traffic accounts for 1.42% of the overall Portal traffic, Google referrals accounts for approximately 1.14% of the overall traffic.

Outages and inaccuracies
------
* '__R__': on 2017-01-01 we started calculating all of Discovery's metrics using a new version of [our data retrieval and processing codebase](https://phabricator.wikimedia.org/diffusion/WDGO/) that we migrated to [Wikimedia Analytics](https://www.mediawiki.org/wiki/Analytics)' [Reportupdater infrastructure](https://wikitech.wikimedia.org/wiki/Analytics/Reportupdater). See [T150915](https://phabricator.wikimedia.org/T150915) for more details.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#search_engines">
    http://discovery.wmflabs.org/portal/#search_engines
  </a>
</p>

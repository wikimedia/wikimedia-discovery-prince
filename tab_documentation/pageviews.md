Pageviews (PVs) to Wikipedia.org Portal
=======

This looks at, without sampling, the number of pageviews the Wikipedia Portal gets per day. This is expressed as raw values. See [our Hive query](https://github.com/wikimedia/wikimedia-discovery-golden/blob/master/portal/pageviews.R#L9) for the technical definition of a Wikipedia.org pageview.

Notes
------
* '__A__': Started filtering out search-redirect.php requests. See [T138411](https://phabricator.wikimedia.org/T138411) for more information.
* '__B__':Rise could not be determined due to deleted data. See [write-up](https://github.com/wikimedia-research/Discovery-Research-Portal/blob/master/Analyses/Pageviews%20Rise/README.md) and [T143045](https://phabricator.wikimedia.org/T143045) for more information.
* '__C__': The investigation of pageviews rise caused us to [redefine](https://gerrit.wikimedia.org/r/#/c/306261/) how we count wikipedia.org pageviews. Pageviews from 2016-06-22 to 2016-08-22 were then recounted using the new definition. See [T143064](https://phabricator.wikimedia.org/T143064) for more information.
* '__D__': on 2016-07-11 we started to split pageview counts into pageviews from "low-volume" clients and "high-volume" clients. A "high-volume" client is a client whose wikipedia.org pageviews are equal to or greater than the 99.99th percentile for the whole population on any particular day. The rationale for this being that the low-volume clients' PV counts would be more stable and the high-volume clients' PV counts would soak up outliers and bots. See [T143605](https://phabricator.wikimedia.org/T143605) for more details.
* '__R__': on 2017-01-01 we started calculating all of Discovery's metrics using a new version of [our data retrieval and processing codebase](https://phabricator.wikimedia.org/diffusion/WDGO/) that we migrated to [Wikimedia Analytics](https://www.mediawiki.org/wiki/Analytics)' [Reportupdater infrastructure](https://wikitech.wikimedia.org/wiki/Analytics/Reportupdater). See [T150915](https://phabricator.wikimedia.org/T150915) for more details.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#pageview_tab">
    http://discovery.wmflabs.org/portal/#pageview_tab
  </a>
</p>

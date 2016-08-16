Traffic to Wikipedia Portal from external search engines
=======

This dashboard simply looks at, very broadly, where our Portal pageviews are coming from. Direct traffic is users typing "wikipedia.org" directly into their browser or using a bookmark. It should also be noted that our request refinery only tracks top global search engines, so some search engines will make it into the "referred by something other than search engine" category despite being a search engine.

General trends
------

On average (or "average" because we use medians), traffic from search engines accounts for 1.42% of overall traffic to the Portal.

Outages and notes
------
- **A**: We switched to a finalized version of the UDF that extracts internal traffic (see [T130083](https://phabricator.wikimedia.org/T130083))
- **B**: Started filtering out search-redirect.php requests. See [T138411](https://phabricator.wikimedia.org/T138411) for more information.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#referrals_summary">
    http://discovery.wmflabs.org/portal/#referrals_summary
  </a>
</p>

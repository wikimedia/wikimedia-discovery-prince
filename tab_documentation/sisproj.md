Visits to other projects
=======

Wikipedia.org is not just a portal to Wikipedia in 200+ languages -- it is also a portal to other projects hosted by the Wikimedia Foundation (e.g. [Commons](https://commons.wikimedia.org/) and [Wiktionary](https://www.wiktionary.org/)). This dashboard tracks the number of clicks (and users making those clicks) on the links to Wikipedia's sister sites as recorded by our [Portal event logging](https://meta.wikimedia.org/wiki/Schema:WikipediaPortal), which users are randomly selected into at a sampling rate of 1 in 200 -- 0.5%. When the user comes to wikipedia.org and are randomly selected to be anonymously tracked via event logging, we set a timer of 15 minutes. Every time the user comes back to the page (lands), the timer is reset. After 15 minutes of not returning, the user's session is no longer tracked. For this reason, a single session may have one or dozens (sometimes hundreds!) of visits, each with a click or multiple clicks. We have seen sessions with as many as hundred clicks following the first and only landing. In cases where almost every session only has a single click associated with it, the graphs for *clicks* and *users* will look VERY similar, if not exactly the same.

Outages and inaccuracies
------

Broadly-speaking, it's worth noting that (as with all data based on JavaScript logging) the code that gathers this information requires a certain amount of browser capabilities to function. It's probably not going to work on 10 year old Nokia brick phones, and so the data will be biased against users using those kinds of devices.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#clickthrough_rate">
    http://discovery.wmflabs.org/portal/#clickthrough_rate
  </a>
</p>

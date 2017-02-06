Clicks on Wikipedia mobile app links
=======

On 3 November 2016, we added links to the Wikipedia iOS and Android apps (see [T137495](https://phabricator.wikimedia.org/T137495)), as well as a link to [the page listing all the mobile apps](https://en.wikipedia.org/wiki/List_of_Wikipedia_mobile_applications). This dashboard tracks the number of clicks those links received from users enrolled into event logging (see notes below) on their desktop and mobile devices as recorded by our [Portal event logging](https://meta.wikimedia.org/wiki/Schema:WikipediaPortal), which users are randomly selected into at a sampling rate of 1 in 200 -- 0.5%. When the user comes to wikipedia.org and are randomly selected to be anonymously tracked via event logging, we set a timer of 15 minutes. Every time the user comes back to the page (lands), the timer is reset. After 15 minutes of not returning, the user's session is no longer tracked.

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

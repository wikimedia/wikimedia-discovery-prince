Wikimedia Portal clickthrough rate
=======

A lot of people visit the [Wikipedia portal](https://www.wikipedia.org) - but what proportion actually go anywhere within our sites from it?

This dashboard tracks the proportion of visits to the portal that end in a click through to one of our projects - via search or via one of the links. This is expressed as a percentage - so a value of "39.4" means that 39.4% of visits end in a clickthrough.

Notes
------

Specifically, the clickthrough rate presented here is the overall rate without any filtering. It is the proportion of click events (by section) over the total number of landing events. By design, a single session is at least 15 minutes but can last indefinitely. **For example**: a single session can last for hours if the "user" (e.g. a computer in a public library) keeps returning to the page before the 15 minute expiration time, thus resetting the timer; and if that single session has 1000 page visits and 500 clicks, then all 1500 of those events will be used in the calculation of the clickthrough rate. We are [considering](https://phabricator.wikimedia.org/T134199) surfacing the "clickthrough rate on first visit" that some of our A/B test reports use.

Outages and inaccuracies
------

* From 7 December (marked "A") the sampling changed to exclude a broader range of browsers, resulting in alterations to things
like clickthrough rate and dwell time. We expect this to resolve itself on 4 January when a new schema version is launched.

Broadly-speaking, it's worth noting that (as with all data based on JavaScript logging) the code that gathers this information requires a certain amount of browser capabilities to function. It's probably not going to work on 10 year old Nokia brick phones, and so the data will be biased against users using those kinds of devices.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Dan](mailto:dgarry@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#clickthrough_rate">
    http://discovery.wmflabs.org/portal/#clickthrough_rate
  </a>
</p>

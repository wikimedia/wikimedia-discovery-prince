Breakdown of user actions on the Wikipedia Portal
=======

In [the clickthroughs tab](http://discovery.wmflabs.org/portal/#clickthrough_rate) we look at the actual rate of clickthroughs.
*This* graph, on the other hand, breaks user actions down into sub-categories so we can see what features, in particular, people
are using, and how it varies over time. The options are:

1. **Language search**: the "find a language" box.
2. **No action**: Not taking any action on the page.
3. **Other projects**: clicking the links to Wiktionary, Wikisource, etc. on the bottom of the page.
4. **Primary links**: the prominent links to languages around the globe.
5. **Search**: using the Search box and button.
6. **Other languages**: using the "Other languages" links near the bottom of the page which take you to a list of Wikipedias.
7. **Secondary links**: the less-prominent plaintext links to languages.

The values are expressed as percentages - so if a feature has the value "61.3" it is being used 61.3% of the time.

Outages and inaccuracies
------

* From 7 December (marked "A") the sampling changed to exclude a broader range of browsers, resulting in alterations to things like clickthrough rate and dwell time. We expect this to resolve itself on 4 January when a new schema version is launched.

Broadly-speaking, it's worth noting that (as with all data based on JavaScript logging) the code that gathers this information requires a certain amount of browser capabilities to function. It's probably not going to work on 10 year old Nokia brick phones, and so the data will be biased against users using those kinds of devices.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#action_breakdown">
    http://discovery.wmflabs.org/portal/#action_breakdown
  </a>
</p>

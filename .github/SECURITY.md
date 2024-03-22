# Security Policy

## Supported Versions

We are currently providing security updates to the following versions:

| Version | Supported          |
| ------- | ------------------ |
| 1.11.x  | :white_check_mark: |
| 1.10.x  | :white_check_mark: |
| < 1.10  | :x: |

## Reporting a Vulnerability

We will use [github vulnerability reporting feature](https://docs.github.com/en/code-security/security-advisories/guidance-on-reporting-and-writing-information-about-vulnerabilities/privately-reporting-a-security-vulnerability#privately-reporting-a-security-vulnerability) or [matrix](https://matrix.org/) (at your convenience) as the vehicle for reporting security issues as that gives us a forum to discuss, analyze, and remediate the threat before an exploit is published.
[Responsible disclosure](https://en.wikipedia.org/wiki/Responsible_disclosure) enhances security for the entire community.

If the issue is deemed a vulnerability, we will release a patch version of our software
and make sure that finds it way to Maven Central before we push the patch to github.
After the patch is available on Maven Central, we will also provide a [security advisory](https://github.com/gnieh/fs2-data/security/advisories) through github.
As with every release, the source jars are published to maven central at the same time as the binaries.

We strongly recommend users of our libraries to use [Scala Steward](https://github.com/fthomas/scala-steward) or something similar to
automatically receive updates.

### Security Maintainer list:

|name | github | matrix |
|-----|--------|---------|
| Lucas Satabin | @satabin | @luxas:matrix.gnieh.org |
| Yannick Heiber | @ybasket | @ybasket:matrix.gnieh.org |

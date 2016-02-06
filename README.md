Web2RSS
=======

A feed generator, that keeps tabs on web sites (or JSON feeds or other things
reachable by HTTP) and generates an Atom feed with entries for every time the
site has been noticed to change.

This is Alpha quality software that seems to work for me, but is missing a ton
of features (see the [TODO](TODO.md)) and future updates will break database
compatibility and do Bad Things™.

### Installation

* Install a MySQL compatible server and client libraries (for example MariaDB)
* Install the Haskell Tool
  [Stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
* Clone this repository and in the root directory run

```
$ stack setup && stack build
```

### How to run locally

* To run locally from the source directory

```
$ stack exec web2rss-exe
```

### How to run on a server (in ~production)

* Copy the file `web2rss-exe` in an architecture dependent subdirectory under
  `.stack-work/install` to the machine where you want to run it. The target
  machine needs to have the MySQL libraries installed, but other than that the
  requirements are minimal.
* To run e.g., behind Nginx, the following works (under an existing server setup with whatever TLS or other things)

```
proxy_pass http://127.0.0.1:PORT/;
```

* To run behind Nginx on a non-root path

```
location /web2rss {
  rewrite /web2rss/(.*) /$1 break;
  rewrite ^/web2rss$ /web2rss/ permanent;
  proxy_pass  http://127.0.0.1:PORT/;
  proxy_redirect / /web2rss/;
}
```

* For running under Systemd the following unit file (place it at `/usr/lib/systemd/system/web2rss.service`)

```
[Unit]
Description=Web2RSS

[Service]
EnvironmentFile=-/PATH-TO/web2rssenv
Type=simple
ExecStart=/PATH-TO/web2rss-exe
Restart=on-abort

[Install]
WantedBy=multi-user.target
```

with a related web2rssenv file, e.g.,

```
USER="web2rss"
PASSWORD=""
DB="web2rss"
PORT=""
URLS="http://first.example.com https://second.example.com"
SOURCE_CODE_URL=""
```

To start up, issue

```
# systemctl start web2rss.service
```

### How to configure

For the time being, Web2RSS is configured using environment variables.
Understood variables are:

* `USER` for database username
* `DB` for database name
* `PASSWORD` for database password
* `PORT` for port to listen on
* `URLS` for a space separated list of URLs to watch
* `SOURCE_CODE_URL` for a link to finding your customized version

## Contributing

Contributions are welcome and I will try my best to answer quickly to all
suggestions. If you start working on something mentioned in the [TODO](TODO.md),
let me know so I don't scoop you.

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Note that currently this program does not "prominently offer all users
interacting with it remotely through a computer network (if your version
supports such interaction) an opportunity to receive the Corresponding Source"
as required by the license, so that will need to be implemented first by me or
by you in order for you to be able to provide access to others.

## Aknowledgements

I thank my employer [Futurice](https://github.com/futurice/) for providing
monetary sponsorship for this work through the
[Spice Program](http://spiceprogram.org/oss-sponsorship/)

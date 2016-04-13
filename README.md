# Erlang bot for FreeFeed.net #

## Build and deploy ##

```
git clone https://github.com/yashrk/erlfrfbot
cd erlfrfbot
wget https://github.com/rebar/rebar/wiki/rebar
chmod +x rebar
./rebar compile
cd rel
vim files/sys.config # About config file see below
../rebar generate
```

That's all. Now you have a ready to work Erlang release of erlfrfbot in `$PROJECTDIR/rel/frfbot`. You can copy it to your server and run or just run on the same machine.

## What to edit in sys.config ##

```
 %% FreeFeed bot config
 {frfbot, [
           {user, "<YOUR_LOGIN_HERE>"},
           {password, "<YOUR_PASSWORD_HERE>"}
          ]},
 %% Scheduled tasks
 {erlcron, [
            {crontab, [
                       {{daily, {6, 00, am}}, {utils, habits_post, []}}
            ]}
           ]}
```

Change `<YOUR_LOGIN_HERE>` and `<YOUR_PASSWORD_HERE>` to your bot login and password. If you don't need scheduled tasks, comment out `erlcron` section, otherwise edit it.

## Usage ##

### How to run and connect ###

Go to `$RELEASE_DIR/bin`. Run

```
./frfbot start
```

Now your bot is (probably) up and running.

If you need a console, type
```
./frfbot attach
```

To leave console, press `Ctrl-D`.

### API example ###

<pre>
(frfbot@127.0.0.1)1> <strong>frfbot:is_logged_in().</strong>
true

(frfbot@127.0.0.1)2> <strong>frfbot:post(<<"mybot">>, <<"This is a test"/utf8>>).</strong>
{ok,{{"HTTP/1.1",200,"OK"},
...
    "{\"posts\":{\"id\":\"b93b1b68-2325-490c-99c8-7318a15e9536\",
...

(frfbot@127.0.0.1)3> <strong>frfbot:comment(<<"b93b1b68-2325-490c-99c8-7318a15e9536">>, <<"This is a test comment"/utf8>>).</strong>
{"comment":{"postId":"b93b1b68-2325-490c-99c8-7318a15e9536","body":"This is a test comment"}}{{"HTTP/1.1",200,"OK"},
"{\"comments\":{\"id\":\"0ef6ee44-88d9-4647-a3a8-99fe68757b7a\",\"body\":\"This is a test comment\",\"createdAt\":1

(frfbot@127.0.0.1)4> <strong>frfbot:get_posts("mybot").</strong>
[#{<<"body">> => <<"This is a test">>,
...
   <<"id">> => <<"b93b1b68-2325-490c-99c8-7318a15e9536">>,
...

(frfbot@127.0.0.1)5> <strong>frfbot:login().</strong>
ok
</pre>

## License ##

This code is available under the terms of GNU Affero General Public License (http://www.gnu.org/licenses/agpl-3.0.en.html).

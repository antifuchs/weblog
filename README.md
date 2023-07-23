# My new blog

This is my new hugo blog. Mental note: Document how stuff works
because being unable to even operate this machinery when you want to
write a post is not going to make you want to write more posts.

## How to deploy

This blog is hosted on [netlify](https://app.netlify.com), and it's
set up to deploy the site whenever the master branch gets pushed.

## How to test out stuff

The `develop` branch deploys to the test blog setup in netlify,
definitely check that out too.

## Redirects

These live in `static/_redirects`; They
follow [netlify's scheme](https://www.netlify.com/docs/redirects/).

## Shortcodes

### `video`

This allows embedding videos, screencasts.

To create the compatible videos from a QuickTime screen recording, use:

``` sh
INPUT=the-video-file.mov   # set this to your filename
MP4="$(basename "$INPUT" .mov).mp4"
ffmpeg -i "$INPUT" ~blog/static/assets/videos/"$MP4"
```


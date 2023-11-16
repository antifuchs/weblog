---
title: "{{ replaceRE `[-_]` " " (replaceRE `^([0-9]{4}-[0-9]{2}-[0-9]{2}-)?` "" .Name) | title }}"
date: {{ .Date }}
draft: true # TODO: un-draft to publish this

# categories: ["Hacks", "Security"]
---

<!--more-->

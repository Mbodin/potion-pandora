#!/bin/bash

# Deploying the content of _build/default/web under localhost:8000
cd _build/default/web
python3 -m http.server 8000


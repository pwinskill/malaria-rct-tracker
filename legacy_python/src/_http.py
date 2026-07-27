"""Small, polite HTTP helper with retries and a shared session."""
import time
import requests

_SESSION = requests.Session()
_SESSION.headers.update({"User-Agent": "malaria-rct-tracker (research; contact via config)"})


def get(url, params=None, timeout=30, retries=3, backoff=2.0, headers=None):
    """GET with simple exponential backoff. Returns requests.Response or raises."""
    last_exc = None
    for attempt in range(retries):
        try:
            r = _SESSION.get(url, params=params, timeout=timeout, headers=headers)
            if r.status_code == 429 or r.status_code >= 500:
                raise requests.HTTPError(f"status {r.status_code}")
            r.raise_for_status()
            return r
        except Exception as exc:  # noqa: BLE001 - deliberately broad, we retry
            last_exc = exc
            if attempt < retries - 1:
                time.sleep(backoff * (attempt + 1))
    raise last_exc

<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Tracking pixels for $pkgname$ | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">
<h2>Adding a tracking pixel to <a href="/package/$pkgname$">$pkgname$</a></h2>

<form method="POST" class="box" action="/package/$pkgname$/tracking-pixels">
    <label for="tracking-pixel">Tracking Image URL</label>
    <input name="tracking-pixel" type="text" />
    <input type="submit" />
</form>

<h2>Existing tracking pixels for <a href="/package/$pkgname$">$pkgname$</a></h2>

<ul>
  $trackingPixels:{trackingPixel|
    <li>
        <form method="POST" action="/package/$pkgname$/tracking-pixels">
            <label for="tracking-pixel">$trackingPixel$</label>
            <input type="hidden" name="tracking-pixel" value="$trackingPixel$"/>
            <input type="hidden" name="_method" value="DELETE" />
            <input type="submit" value="Delete" />
        </form>
    </li>
  }; separator=""$
</ul>

</div>
</body>
</html>

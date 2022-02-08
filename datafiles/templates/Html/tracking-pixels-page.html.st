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

<form method="post" target="/package/$pkgname$/tracking-pixels">
    <label for="tracking-pixel">Tracking Image URL</label>
    <input name="tracking-pixel" type="text" />
    <input type="submit" />
<form>

<h2>Tracking pixels for <a href="/package/$pkgname$">$pkgname$</a></h2>

<ul>
  $trackingPixels:{trackingPixel|
    <li>$trackingPixel$</li>
  }; separator=""$
</ul>

</div>
</body>
</html>

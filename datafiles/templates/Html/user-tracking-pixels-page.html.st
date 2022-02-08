<!DOCTYPE html>
<html>
<head>
$hackageCssTheme()$
<title>Tracking pixels for all of $username$'s packages | Hackage</title>
</head>

<body>
$hackagePageHeader()$

<div id="content">

<h2>Create a tracking pixel</h2>

<form method="POST" class="box" action="/user/$username$/tracking-pixels">
    <p>    
        <label for="package">Package</label>
        <select name="package">
        $pkgs:{pkg|
            <option value="$pkg$">$pkg$</option>      
        }; separator=""$
        </select>
    </p>
    <p>
        <label for="tracking-pixel">Tracking Image URL</label>
        <input name="tracking-pixel" type="text" />
    </p>
    <input type="submit" />
</form>

<h2>Existing tracking pixels</h2>

$pkgpixels:{pkgpixel|
<h3><a href="/package/$pkgpixel.0$">$pkgpixel.0$</a></h3>
<ul>
  $pkgpixel.1:{trackingPixel|
    <li>
        <form method="POST" action="/user/$username$/tracking-pixels">
            <label for="tracking-pixel">$trackingPixel$</label>
            <input type="hidden" name="package" value="$pkgpixel.0$" />
            <input type="hidden" name="tracking-pixel" value="$trackingPixel$"/>
            <input type="hidden" name="_method" value="DELETE" />
            <input type="submit" value="Delete" />
        <form>
    </li>
  }; separator=""$
</ul>
}; separator=""$

</div>
</body>
</html>

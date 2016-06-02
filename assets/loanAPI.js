
var getLoans = function(onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/loans', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var postLoans = function(body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/loans', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.setRequestHeader("Content-Type","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

var getLoansById = function(id, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/loans/' + encodeURIComponent(id) + '', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var deleteLoansById = function(id, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', '/loans/' + encodeURIComponent(id) + '', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}

var putLoansById = function(id, body, onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('PUT', '/loans/' + encodeURIComponent(id) + '', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.setRequestHeader("Content-Type","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(JSON.stringify(body)
);
}

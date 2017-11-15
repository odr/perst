
var getPositionByXByY = function(x, y, onSuccess, onError)
{
  $.ajax(
    { url: '/position/' + encodeURIComponent(x) + '/' + encodeURIComponent(y) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getHello = function(name, onSuccess, onError)
{
  $.ajax(
    { url: '/hello' + '?name=' + encodeURIComponent(name)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postMarketing = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/marketing'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var postTcustomerGetList = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/tcustomer/getList'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getTcustomerByPk = function(pk, onSuccess, onError)
{
  $.ajax(
    { url: '/tcustomer/' + encodeURIComponent(pk) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postTcustomer = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/tcustomer'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var putTcustomer = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/tcustomer'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'PUT'
    });
}

var putTcustomerDiff = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/tcustomer/diff'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'PUT'
    });
}

var deleteTcustomerByPk = function(pk, onSuccess, onError)
{
  $.ajax(
    { url: '/tcustomer/' + encodeURIComponent(pk) + ''
    , success: onSuccess
    , error: onError
    , type: 'DELETE'
    });
}

var postCustomerGetList = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/customer/getList'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getCustomerByPk = function(pk, onSuccess, onError)
{
  $.ajax(
    { url: '/customer/' + encodeURIComponent(pk) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postCustomer = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/customer'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var putCustomer = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/customer'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'PUT'
    });
}

var putCustomerDiff = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/customer/diff'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'PUT'
    });
}

var deleteCustomerByPk = function(pk, onSuccess, onError)
{
  $.ajax(
    { url: '/customer/' + encodeURIComponent(pk) + ''
    , success: onSuccess
    , error: onError
    , type: 'DELETE'
    });
}

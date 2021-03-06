var tvMode = null;

$(function () {
  var $header = $('body > h2').hide(),
    $mainMenu = $('#mainMenu'),
    $mainMenuItems = $mainMenu.find('ul > li'),
    $musicMenu = $('#musicMenu'),
    $musicMenuItems = $musicMenu.find('ul > li'),
    $musicTabItems = $('#content').find('div[data-tab="music"]');

  $mainMenuItems.click(function () {
    $mainMenuItems.removeClass('selected');
    this.classList.add('selected');

    //$header.text(this.textContent);

    if (this.childNodes.item(0).attributes.getNamedItem('href').nodeValue.search('music') > -1) {
      $musicMenu.show();
      $musicTabItems.removeClass('hidden');
      if (! $('#content').hasClass('tv-mode')) {
        var musicTab = $musicMenuItems.find('[class="selected"]').attr('href');
        console.log('Selected music tab: %s', musicTab);
        if (musicTab === '#nowPlaying') {
          $('#nowPlaying').show();
          $('#upNext').removeClass('hidden');
          $('#history').hide();
        } else if (musicTab === '#history') {
          $('#nowPlaying').hide();
          $('#upNext').addClass('hidden');
          $('#history').show();
        }
      }
    } else {
      $musicMenu.hide();
      $musicTabItems.addClass('hidden');
    }
  });

  $musicMenuItems.click(function () {
    $musicMenuItems.removeClass('selected');
    this.classList.add('selected');

    if (! $('#content').hasClass('tv-mode')) {
      var musicTab = $(this).find('a').attr('href');
      $('#content').data('show', musicTab.substring(1));
    }
  });

  // Only do this when @media (display-mode: standalone) ?
  $('#query').focus(function () {
    $musicMenu.hide();
    $mainMenu.hide();
  }).blur(function () {
    setTimeout(function () {
      $musicMenu.show();
      $mainMenu.show();
    }, 250);
  });

  $('#contribute').find('form').submit(function (event) {
    event.preventDefault();

    var query = $(this).find('input[name=q]').val();
    var $results = $('#results').find('ul.tracks');

    $.getJSON('http://api.chancesnow.me/party/search?query=' + encodeURIComponent(query), function (data) {
      if (data.items && data.items.length > 0) {
        $results.empty();
        for (var i = 0; i < 7; i += 1) {
          var track = data.items[i];
          console.log(track);
          var $track = $('<li>').addClass('track');
          var $songInfo = $('<div class="song-info">');
          $track.append($('<img>').attr('src', track.album.images[1].url));
          $songInfo.append($('<span class="title">').text(track.name));
          $songInfo.append($('<span class="artist">').text(track.artists[0].name));
          $track.append($songInfo);
          $results.append($track);
        }
      }
    });

    return false;
  });

  tvMode = function tvMode() {
    $('#content').addClass('tv-mode');
    $header.show();
    $mainMenu.hide();
    $musicMenu.hide();
  };
});

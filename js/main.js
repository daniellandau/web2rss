// Placeholder copied from documentation
import Cycle from '@cycle/core';
import {div, button, h1, h4, a, makeDOMDriver} from '@cycle/dom';
import {makeHTTPDriver} from '@cycle/http';

function main(sources) {
  const USERS_URL = 'http://jsonplaceholder.typicode.com/users/';
  const getRandomUser$ = sources.DOM.select('.get-random').events('click')
    .map(() => {
      const randomNum = Math.round(Math.random()*9)+1;
      return {
        url: USERS_URL + String(randomNum),
        method: 'GET'
      };
    });

  const user$ = sources.HTTP
    .filter(res$ => res$.request.url.indexOf(USERS_URL) === 0)
    .mergeAll()
    .map(res => res.body)
    .startWith(null);

  const vtree$ = user$.map(user =>
    div('.users', [
      button('.get-random', 'Get random user'),
      user === null ? null : div('.user-details', [
        h1('.user-name', user.name),
        h4('.user-email', user.email),
        a('.user-website', {href: user.website}, user.website)
      ])
    ])
  );

  const sinks = {
    DOM: vtree$,
    HTTP: getRandomUser$
  };
  return sinks;
}

Cycle.run(main, {
  DOM: makeDOMDriver('#app'),
  HTTP: makeHTTPDriver()
});

import Cycle from '@cycle/core';
import {label, div, dl, dd, dt, p, button, h1, h4, a, input, makeDOMDriver} from '@cycle/dom';
import { Observable} from 'rx';
import {makeHTTPDriver} from '@cycle/http';

function main(sources) {
  const urlInputChange$ = sources.DOM.select('.add-input-text').events('input')
    .map(ev => {return { newUrlInput: ev.target.value }})

  const feedHashChange$ = sources.DOM.select('.feed-hash').events('input')
    .map(ev => {return { feedHashInput: ev.target.value }})

  const urls$ = sources.HTTP
    .filter(res$ => {
      return res$.request.method === 'GET'
    })
    .mergeAll()
    .map(res => res.body)
    .startWith(null)

  const deletes$ = sources.HTTP
    .filter(res$ => res$.request.method === 'DELETE')
    .mergeAll()
    .map(res => {
      const fragments = res.req.url.split('/');
      const id = fragments[fragments.length - 1];
      return Number(id);
    });

  const adds$ = sources.HTTP
    .filter(res$ => {
      return res$.request.method === 'POST';
    })
    .mergeAll()
    .map(res => {return {newUrl: res.req._data} });

  const state$ = Observable.of({ urls: [], newUrlInput: '', feedHashInput: '' })
    .concat(Observable.merge(urls$, deletes$, adds$, urlInputChange$, feedHashChange$))
    .scan((oldState, update) => {
      if (Array.isArray(update)) {
        oldState.urls = update;
        return oldState;
      } else if (Number(update) === update){
        const index = oldState.urls.findIndex(url => url.id == update);
        oldState.urls.splice(index, 1);
        return oldState;
      } else if (update && update.newUrl) {
        oldState.urls.push(update.newUrl);
        return oldState;
      } else if (update && update.newUrlInput !== undefined) {
        oldState.newUrlInput = update.newUrlInput;
        return oldState;
      } else if (update && update.feedHashInput !== undefined) {
        oldState.feedHashInput = update.feedHashInput;
        return oldState;
      } else {
        return oldState;
      }
    })
    .share();

  const stateWithFeed$ = state$.filter(state => state.feedHashInput.length > 0)

  const getUrls$ = sources.DOM.select('.get-feed').events('click')
    .withLatestFrom(stateWithFeed$, (x, state) => {
      return {
        url: FEEDSURL({feedHash: state.feedHashInput}),
        method: 'GET'
      }
    })

  const deleteUrl$ = sources.DOM.select('.delete-url').events('click')
    .withLatestFrom(stateWithFeed$, (ev, state) => {
      const id = ev.target.getAttribute('data-id');
      return {
        url: FEEDURL({feedHash: state.feedHashInput, urlId: id}),
        method: 'DELETE'
      }
    });

  const addUrl$ = sources.DOM.select('.add-input-button').events('click')
    .withLatestFrom(stateWithFeed$, (x, state) => {
      return {
        url: FEEDSURL({feedHash: state.feedHashInput}),
        method: 'POST',
        send: { id: 1, url: state.newUrlInput }
      }
    });

  const vtree$ = state$.map((state) => {
    const urls = state.urls
    const getFeedEnabled = state.feedHashInput.length > 0
    const addUrlEnabled = getFeedEnabled && state.newUrlInput.length > 0
    return div('.container', [
      label('Feed hash'),
      input('.feed-hash'),
      button({ className: 'get-feed', attributes: getFeedEnabled ? {} : { disabled: true} }, 'Get URLs'),
      div('.feed', [
        !urls ? p('nothing yet') : div('.urls-details', urls.map(url => {
          return [p("Url"), p(url.url), button({ className: 'delete-url', attributes: {'data-id': url.id} }, 'delete')];
        })),
        label('Add url'),
        input('.add-input-text'),
        button({className: 'add-input-button', attributes: addUrlEnabled ? {} : { disabled: true} }, 'Add URL')
      ])
    ])
  });

  const sinks = {
    DOM: vtree$,
    HTTP: Observable.merge(getUrls$, addUrl$, deleteUrl$)
  };
  return sinks;
}

Cycle.run(main, {
  DOM: makeDOMDriver('#app'),
  HTTP: makeHTTPDriver()
});

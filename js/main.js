import Cycle from '@cycle/core';
import {div, dl, dd, dt, p, button, h1, h4, a, input, makeDOMDriver} from '@cycle/dom';
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
      } else if (update && update.newUrlInput) {
        oldState.newUrlInput = update.newUrlInput;
        return oldState;
      } else if (update && update.feedHashInput) {
        oldState.feedHashInput = update.feedHashInput;
        return oldState;
      } else {
        return oldState;
      }
    })
    .share();


  const getUrls$ = sources.DOM.select('.get-feed').events('click')
    .withLatestFrom(state$, (x, state) => {
      return {
        url: FEEDSURL({feedHash: state.feedHashInput}),
        method: 'GET'
      }
    })

  const deleteUrl$ = sources.DOM.select('.delete-url').events('click')
    .withLatestFrom(state$, (ev, state) => {
      const id = ev.target.getAttribute('data-id');
      return {
        url: FEEDURL({feedHash: state.feedHashInput, urlId: id}),
        method: 'DELETE'
      }
    });

  const addUrl$ = sources.DOM.select('.add-input-button').events('click')
    .withLatestFrom(state$, (x, state) => {
      return {
        url: FEEDSURL({feedHash: state.feedHashInput}),
        method: 'POST',
        send: { id: 1, url: state.newUrlInput }
      }
    });

  const vtree$ = state$.map((state) => {
    const urls = state.urls;
    return div('.container', [
      p('Input feed hash'),
      input('.feed-hash'),
      button('.get-feed', 'Get URLs'),
      div('.feed', [
        !urls ? p('nothing yet') : div('.urls-details', urls.map(url => {
          return [p("Url"), p(url.url), button({ className: 'delete-url', attributes: {'data-id': url.id} }, 'delete')];
        })),
        p('Add url'),
        input('.add-input-text'),
        button('.add-input-button', 'Add URL')
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

import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;
const saveButton = document.querySelector('#saveButton')!;
const titleEle = document.querySelector('#titleText')! as HTMLInputElement;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value })),
);

const title$: Observable<Action> = fromEvent<KeyboardEvent>(
    titleEle,
    "input",
).pipe(
    map((event) => (s) => { 
        const ele = event.target as HTMLInputElement
        return {...s, HTML: replaceTitle(s.HTML, ele)} 
    }),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

const saveStream$: Observable<Action> = fromEvent(saveButton, "click").pipe(
    map((_) =>(s) => ({ ...s, save: true }))
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            const ele = document.querySelector('#titleText')! as HTMLInputElement;
            return {
                ...s,
                HTML: replaceTitle(data.html, ele),
            };
        }),
        first(),
    );
}

function replaceTitle(html: string, titleEle: HTMLInputElement): string {
    const title = titleEle.value == "" ? "Converted HTML" : titleEle.value;
    const titleTagOpen = "<title>";
    const titleTagClose = "</title>";

    const titleIdxStart = html.indexOf(titleTagOpen) + titleTagOpen.length;
    const titleIdxEnd = html.indexOf(titleTagClose);
    
    const beforeTitle = html.substring(0, titleIdxStart);
    const afterTitle = html.substring(titleIdxEnd, html.length);

    return beforeTitle + title + afterTitle;

}

function saveHtml(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ success: string }>({
        url: "/api/saveHtml",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.HTML
    }).pipe(
        map((_) => {
            return {
                ...s,
                save: false
            };
        }),
        first(),
    );
}


const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$, saveStream$, title$)
        .pipe(
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                if (newState.save == true){
                    return merge(getHTML(newState), saveHtml(newState));
                } else {
                    return getHTML(newState);
                }
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    htmlOutput.style.whiteSpace = "normal"

                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.style.whiteSpace = "pre"
                    htmlOutput.textContent = value.HTML;
                }
            }

            if (value.save) {

                alert("File saved! Please see ")
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}

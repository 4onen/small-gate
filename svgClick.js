"use strict";
//Code adapted from https://discourse.elm-lang.org/t/custom-elements-extend-svg-real-coordinates-on-mouse-events/1762
var observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (mutation) {
        if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
            Array
                .from(mutation.addedNodes)
                .forEach(function (node) {
                    let notableChildren = false;
                    if (node.getElementsByTagName)
                        notableChildren = node.getElementsByTagName('svg');
                    if (notableChildren) {
                        Array
                            .from(notableChildren)
                            .forEach(function (svg) {
                                svg.addEventListener('mousedown', function (event) {
                                    let point = svg.createSVGPoint();
                                    point.x = event.clientX;
                                    point.y = event.clientY;
                                    point = point.matrixTransform(svg.getScreenCTM().inverse());
                                    let svgClickEvent = new CustomEvent('svgdown', {
                                        detail: {
                                            x: point.x,
                                            y: point.y
                                        }
                                    });
                                    event.currentTarget.dispatchEvent(svgClickEvent);
                                });
                                svg.addEventListener('mouseup', function (event) {
                                    let point = svg.createSVGPoint();
                                    point.x = event.clientX;
                                    point.y = event.clientY;
                                    point = point.matrixTransform(svg.getScreenCTM().inverse());
                                    let svgClickEvent = new CustomEvent('svgup', {
                                        detail: {
                                            x: point.x,
                                            y: point.y
                                        }
                                    });
                                    event.currentTarget.dispatchEvent(svgClickEvent);
                                });
                                svg.addEventListener('mousemove', function (event) {
                                    let point = svg.createSVGPoint();
                                    point.x = event.clientX;
                                    point.y = event.clientY;
                                    point = point.matrixTransform(svg.getScreenCTM().inverse());
                                    let svgClickEvent = new CustomEvent('svgmove', {
                                        detail: {
                                            x: point.x,
                                            y: point.y
                                        }
                                    });
                                    event.currentTarget.dispatchEvent(svgClickEvent);
                                });
                            });
                    }
                });
        }
    });
});

observer.observe(document.body, { childList: true, subtree: true });
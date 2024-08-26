const fs = require('fs');
const path = require('path');
const directoryPath = './_book';

fs.readdir(directoryPath, (err, files) => {
    if (err) {
        console.error('Unable to scan directory:', err);
        return;
    }

    files.forEach(file => {
        if (path.extname(file) === '.html') {
            const filePath = path.join(directoryPath, file);
            
            fs.readFile(filePath, 'utf8', (err, data) => {
                if (err) {
                    console.error(`Error reading file ${file}:`, err);
                    return;
                }

                // Define your JavaScript function
                const scriptContent = `
                    <script>
                        function insertAudioFiles() {
                            const headers = document.querySelectorAll('h1, h2, h3, h4, h5, h6');
                            const pagePath = window.location.pathname;
                            const fileName = pagePath.substring(pagePath.lastIndexOf('/') + 1, pagePath.lastIndexOf('.'));

                            const audioDir = \`./audio/\${fileName}\`;
                            let audioIndex = 1;

                            headers.forEach((header, index) => {
                                if (header.id === 'toc-title') {
                                    return;
                                }
                                const audioFileName = \`chunk_\${audioIndex}.mp3\`; // Assumes audio files are named sequentially
                                const audioPath = \`\${audioDir}/\${audioFileName}\`;
                                const audioElement = document.createElement('audio');
                                console.log('Audio Path:', audioPath);
                                // Set audio attributes
                                audioElement.controls = true;
                                audioElement.style.height = '40px';
                                audioElement.style.width = '240px';

                                const sourceElement = document.createElement('source');
                                sourceElement.src = audioPath;
                                sourceElement.type = 'audio/mpeg';

                                audioElement.appendChild(sourceElement);

                                // Create a wrapper to insert the audio player into the margin
                                const marginDiv = document.createElement('div');
                                marginDiv.classList.add('column-margin');
                                marginDiv.appendChild(audioElement);

                                header.insertAdjacentElement('afterend', marginDiv);
                                audioIndex++;
                            });
                        }

                        // Call the function after the DOM is fully loaded
                        document.addEventListener('DOMContentLoaded', insertAudioFiles);
                    </script>
                `;

                const modifiedData = data + scriptContent;
                
                fs.writeFile(filePath, modifiedData, 'utf8', err => {
                    if (err) {
                        console.error(`Error writing file ${file}:`, err);
                    }
                });
            });
        }
    });
});
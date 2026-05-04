import { readFileSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { argv, cwd } from 'node:process';

/**
 * Update Asset
 *
 * @description
 * Update asset files for version release.
 *
 * @returns {void}
 */
function updateAsset() {
  const VERSION = argv[2];
  const DATE = new Date().toISOString().slice(0, 10);
  const YEAR = DATE.slice(0, 4);

  updateCitation(VERSION, DATE, YEAR);
  updateDescription(VERSION);
  updateLicense(YEAR);
  updateInstaller(VERSION);
}

/**
 * Update Citation
 *
 * @description
 * Update CITATION files for version release.
 *
 * @param {string} version Release version.
 * @param {string} date Release date.
 * @param {string} year Release year.
 * @returns {void}
 */
function updateCitation(version, date, year) {
  let path, data;

  // CITATION.cff
  path = resolve(cwd(), 'CITATION.cff');
  data = readFileSync(path, { encoding: 'utf-8' });

  data = data.replace(/(?<!cff-)version: \d+\.\d+\.\d+/, `version: ${version}`);
  data = data.replace(
    /date-released: \d{4}-\d{2}-\d{2}/,
    `date-released: ${date}`,
  );

  writeFileSync(path, data);

  // CITATION
  path = resolve(cwd(), 'inst', 'CITATION');
  data = readFileSync(path, { encoding: 'utf-8' });

  data = data.replace(/version = "\d+\.\d+\.\d+"/, `version = "${version}"`);
  data = data.replace(/year = "\d{4}"/, `year = "${year}"`);

  writeFileSync(path, data);
}

/**
 * Update Description
 *
 * @description
 * Update DESCRIPTION file for version release.
 *
 * @param {string} version Release version.
 * @returns {void}
 */
function updateDescription(version) {
  let path, data;

  path = resolve(cwd(), 'DESCRIPTION');
  data = readFileSync(path, { encoding: 'utf-8' });

  data = data.replace(/Version: \d+\.\d+\.\d+/, `Version: ${version}`);

  writeFileSync(path, data);
}

/**
 * Update License
 *
 * @description
 * Update LICENSE files for version release.
 *
 * @param {string} year Release year.
 * @returns {void}
 */
function updateLicense(year) {
  let path, data;

  // LICENSE
  path = resolve(cwd(), 'LICENSE');
  data = readFileSync(path, { encoding: 'utf-8' });

  data = data.replace(/YEAR: \d{4}/, `YEAR: ${year}`);

  writeFileSync(path, data);

  // LICENSE.md
  path = resolve(cwd(), 'LICENSE.md');
  data = readFileSync(path, { encoding: 'utf-8' });

  data = data.replace(/Copyright \(c\) \d{4}/, `Copyright (c) ${year}`);

  writeFileSync(path, data);
}

/**
 * Update Installer
 *
 * @description
 * Update Inno Setup script version for release.
 *
 * @param {string} version Release version.
 * @returns {void}
 */
function updateInstaller(version) {
  let path, data;

  path = resolve(cwd(), 'inst', 'installer', 'setup.iss');
  data = readFileSync(path, { encoding: 'utf-8' });

  data = data.replace(
    /#define MyAppVersion "\d+\.\d+\.\d+"/,
    `#define MyAppVersion "${version}"`,
  );

  writeFileSync(path, data);
}

updateAsset();

import { describe, expect, it } from 'vitest';
import {
  dateTimeParams,
  dayOf,
  disabledApi,
  enableUrl,
  freshnessDay,
  issueEntries,
  metricRows,
  missingPlayGrant,
  reportEntries,
  searchQuery,
  timelineBody,
} from './android-crashes.mjs';

describe('Android crash collection', () => {
  it('names the project whose Reporting API is off, from ErrorInfo or from the prose', () => {
    const structured = Object.assign(new Error('denied'), {
      status: 403,
      details: [
        {
          reason: 'SERVICE_DISABLED',
          metadata: { consumer: 'projects/514023947371', service: 'playdeveloperreporting.googleapis.com' },
        },
      ],
    });
    expect(disabledApi(structured)).toEqual({
      api: 'playdeveloperreporting.googleapis.com',
      project: '514023947371',
    });
    expect(enableUrl(disabledApi(structured))).toBe(
      'https://console.developers.google.com/apis/api/playdeveloperreporting.googleapis.com/overview?project=514023947371',
    );

    const prose = Object.assign(
      new Error('Google Play Developer Reporting API has not been used in project 514023947371 before or it is disabled.'),
      { status: 403, details: [] },
    );
    expect(disabledApi(prose)?.project).toBe('514023947371');
  });

  it('separates the missing Play Console grant from the disabled API', () => {
    const grant = Object.assign(new Error('The caller does not have permission'), { status: 403, details: [] });
    expect(missingPlayGrant(grant)).toBe(true);
    expect(disabledApi(grant)).toBeUndefined();
    expect(missingPlayGrant(Object.assign(new Error('nope'), { status: 404 }))).toBe(false);
    expect(
      missingPlayGrant(
        Object.assign(new Error('off'), { status: 403, details: [{ reason: 'SERVICE_DISABLED', metadata: { consumer: 'projects/1' } }] }),
      ),
    ).toBe(false);
  });

  it('asks for whole hours in UTC, because the API rejects finer granularity', () => {
    const query = searchQuery({ days: 2, limit: 5, now: new Date('2026-08-17T13:47:12Z'), orderBy: 'errorReportCount desc' });
    expect(query).toMatchObject({
      'interval.startTime.year': '2026',
      'interval.startTime.month': '8',
      'interval.startTime.day': '15',
      'interval.startTime.hours': '13',
      'interval.startTime.minutes': '0',
      'interval.endTime.day': '17',
      'interval.endTime.hours': '13',
      'interval.endTime.timeZone.id': 'UTC',
      pageSize: '5',
      orderBy: 'errorReportCount desc',
    });
    expect(query.filter).toBeUndefined();
    expect(dateTimeParams('x', new Date('2026-01-02T03:00:00Z'))['x.month']).toBe('1');
  });

  it('queries daily crash rate in the timezone the metric set is fixed to', () => {
    const body = timelineBody({ metrics: ['crashRate'], days: 3, now: new Date('2026-08-17T13:47:12Z') });
    expect(body.timelineSpec).toEqual({
      aggregationPeriod: 'DAILY',
      startTime: { year: 2026, month: 8, day: 14, timeZone: { id: 'America/Los_Angeles' } },
      endTime: { year: 2026, month: 8, day: 17, timeZone: { id: 'America/Los_Angeles' } },
    });
    expect(body.metrics).toEqual(['crashRate']);
  });

  // Regression: the API refuses `end_date` past the metric set's freshness, so a
  // window that ended "today" made every crash/ANR timeline a 400.
  it('ends the timeline at the metric set freshness, never past it', () => {
    const body = timelineBody({ metrics: ['crashRate'], days: 3, now: new Date('2026-08-17T13:47:12Z'), until: '2026-08-16' });
    expect(body.timelineSpec.endTime).toEqual({ year: 2026, month: 8, day: 16, timeZone: { id: 'America/Los_Angeles' } });
    expect(body.timelineSpec.startTime).toEqual({ year: 2026, month: 8, day: 13, timeZone: { id: 'America/Los_Angeles' } });
  });

  it('keeps today when freshness is not behind it', () => {
    const body = timelineBody({ metrics: ['anrRate'], days: 1, now: new Date('2026-08-17T13:47:12Z'), until: '2026-08-18' });
    expect(body.timelineSpec.endTime.day).toBe(17);
  });
  it('reads the freshest day the metric set admits to, per aggregation period', () => {
    const set = {
      freshnessInfo: {
        freshnesses: [
          { aggregationPeriod: 'HOURLY', latestEndTime: { year: 2026, month: 8, day: 17, hours: 9 } },
          { aggregationPeriod: 'DAILY', latestEndTime: { year: 2026, month: 8, day: 16 } },
        ],
      },
    };
    expect(freshnessDay(set)).toBe('2026-08-16');
    expect(freshnessDay(set, 'HOURLY')).toBe('2026-08-17');
    expect(freshnessDay({})).toBeUndefined();
    expect(dayOf('2026-08-16T10:00:00Z')).toBe('2026-08-16');
  });

  it('ranks issues by report count and keeps the console link that opens each one', () => {
    const response = {
      errorIssues: [
        {
          name: 'apps/com.blockether.viscompanion/errorIssues/quiet',
          type: 'APPLICATION_NOT_RESPONDING',
          cause: 'Input dispatching timed out',
          location: 'com.blockether.viscompanion.MainActivity',
          errorReportCount: '2',
          distinctUsers: '2',
          firstAppVersion: { versionCode: '4200' },
          lastAppVersion: { versionCode: '4311' },
        },
        {
          name: 'apps/com.blockether.viscompanion/errorIssues/loud',
          type: 'CRASH',
          cause: 'java.lang.NullPointerException',
          errorReportCount: '17',
          distinctUsers: '11',
          lastErrorReportTime: '2026-08-16T22:10:00Z',
          issueUri: 'https://play.google.com/console/developers/x/app/y/vitals/errors/loud',
        },
      ],
    };

    expect(issueEntries(response)).toEqual([
      expect.objectContaining({ id: 'loud', type: 'CRASH', reports: 17, users: 11, console: expect.stringContaining('vitals/errors/loud') }),
      expect.objectContaining({ id: 'quiet', type: 'APPLICATION_NOT_RESPONDING', reports: 2, versionCodes: ['4200', '4311'] }),
    ]);
    expect(issueEntries({})).toEqual([]);
  });

  it('truncates a stack trace: a sample report is context, not an archive', () => {
    const response = {
      errorReports: [
        {
          name: 'apps/com.blockether.viscompanion/errorReports/r1',
          type: 'CRASH',
          issueId: 'loud',
          eventTime: '2026-08-16T22:10:00Z',
          deviceModel: { marketingName: 'Pixel 9', deviceId: { buildBrand: 'google' } },
          osVersion: { apiLevel: 35 },
          appVersion: { versionCode: 4311 },
          reportText: ['line0', 'line1', 'line2', 'line3'].join('\n'),
        },
      ],
    };

    expect(reportEntries(response, 2)).toEqual([
      {
        id: 'r1',
        type: 'CRASH',
        issueId: 'loud',
        eventTime: '2026-08-16T22:10:00Z',
        device: 'google Pixel 9',
        apiLevel: '35',
        versionCode: '4311',
        stack: 'line0\nline1',
      },
    ]);
  });

  it('flattens a metric timeline to one row per day', () => {
    const response = {
      rows: [
        {
          startTime: { year: 2026, month: 8, day: 16, timeZone: { id: 'America/Los_Angeles' } },
          dimensions: [{ dimension: 'versionCode', int64Value: '4311' }],
          metrics: [
            { metric: 'crashRate', decimalValue: { value: '0.0123' } },
            { metric: 'distinctUsers', decimalValue: { value: '81' } },
          ],
        },
      ],
    };

    expect(metricRows(response)).toEqual([
      { day: '2026-08-16', dimensions: { versionCode: '4311' }, metrics: { crashRate: '0.0123', distinctUsers: '81' } },
    ]);
    expect(metricRows({})).toEqual([]);
  });
});

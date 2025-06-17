# PyPI 배포 가이드

이 문서는 PKPDsim Python 패키지를 PyPI에 배포하는 방법을 설명합니다.

## 사전 준비

### 1. PyPI 계정 생성
- [PyPI](https://pypi.org/)에서 계정 생성
- [Test PyPI](https://test.pypi.org/)에서도 계정 생성 (테스트용)

### 2. API 토큰 생성
- PyPI 계정 설정에서 API 토큰 생성
- GitHub 리포지토리 Settings > Secrets에 `PYPI_API_TOKEN` 추가

## 로컬 테스트

### 1. 패키지 빌드 테스트
```bash
# 빌드 도구 설치
pip install build twine

# 패키지 빌드
python -m build

# 빌드 결과 확인
ls dist/
# pkpdsim_python-1.4.1-py3-none-any.whl
# pkpdsim-python-1.4.1.tar.gz
```

### 2. 패키지 검증
```bash
# 패키지 검증
twine check dist/*

# 로컬 설치 테스트
pip install dist/pkpdsim_python-1.4.1-py3-none-any.whl
```

### 3. Test PyPI에 업로드
```bash
# Test PyPI 설정
twine upload --repository testpypi dist/*

# Test PyPI에서 설치 테스트
pip install --index-url https://test.pypi.org/simple/ pkpdsim-python
```

## GitHub 브랜치 및 PR 생성

### 1. 새 브랜치 생성
```bash
git checkout -b python-package
git add .
git commit -m "Add Python package implementation of PKPDsim

- Convert R functions to Python equivalents
- Add comprehensive test suite
- Create documentation and examples
- Setup PyPI deployment workflow"
git push origin python-package
```

### 2. Pull Request 생성
- GitHub에서 `python-package` → `master` PR 생성
- PR 설명에 변경사항 요약 포함

## PyPI 배포 방법

### 방법 1: GitHub Release (추천)
1. PR 병합 후 GitHub에서 Release 생성
2. 태그: `v1.4.1-python`
3. 릴리즈 생성시 자동으로 PyPI 배포됨

### 방법 2: 수동 배포
```bash
# 프로덕션 PyPI에 업로드
twine upload dist/*
```

## 배포 후 확인

### 1. PyPI에서 확인
- https://pypi.org/project/pkpdsim-python/ 접속
- 패키지 정보, 의존성, 파일 등 확인

### 2. 설치 테스트
```bash
# 새로운 환경에서 설치 테스트
pip install pkpdsim-python

# 임포트 테스트
python -c "from pkpdsim import sim, new_ode_model, new_regimen; print('Success!')"
```

### 3. 예제 실행
```bash
python examples/basic_simulation.py
```

## 버전 관리

### 버전 업데이트 시
1. `pyproject.toml`에서 버전 번호 수정
2. 변경사항을 `CHANGELOG.md`에 기록
3. 새로운 릴리즈 생성

### 버전 명명 규칙
- R 패키지 버전에 `-python` 접미사 추가
- 예: R이 1.4.1이면 Python은 1.4.1-python 또는 1.4.1.post1

## 주의사항

1. **테스트 완료**: 모든 테스트가 통과하는지 확인
2. **의존성 확인**: 요구사항이 올바른지 확인
3. **라이선스**: MIT 라이선스 유지
4. **문서화**: README와 예제가 최신인지 확인
5. **충돌 방지**: R 패키지와 구분되도록 패키지명 다르게 설정

## 트러블슈팅

### 패키지명 충돌
- PyPI에서 이름 확인: `pip install pkpdsim-python --dry-run`
- 필요시 다른 이름 사용 (예: `pkpdsim-py`, `python-pkpdsim`)

### 업로드 실패
- API 토큰 확인
- 버전 중복 확인 (이미 업로드된 버전은 재업로드 불가)
- 파일 크기 제한 확인

### 의존성 문제
- `requirements.txt` 확인
- Python 버전 호환성 확인
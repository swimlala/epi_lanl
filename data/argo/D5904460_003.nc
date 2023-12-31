CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  G   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:38Z AOML 3.0 creation; 2016-08-07T21:17:08Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       @   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  E    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       Fh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       K�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  P�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  W   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       XL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ]h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  b�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  h�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  oL   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    o|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    r|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    u|   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  x|   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    x�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    x�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    x�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    x�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  x�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    x�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    y   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    y   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         y   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         y    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        y$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    y(Argo profile    3.1 1.2 19500101000000  20150226221238  20160807141708  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_003                   2C  D   APEX                            6487                            072314                          846 @�]L/�1   @��Q�@-�33333�c���R1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�3Ct�fCy�fC  C�  C�s3C�  C�� C�  C�� C�  C���C��3C�s3C��3C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  Cŀ C�  Cʀ C��3Cπ C�  CԀ C��3Cٌ�C�  Cހ C�  C� C�  C� C�  C� C�  C� C�  C�� C�  C�� C�  D � D  D@ D� D� D  D@ D	� D
� D  D@ D� D�fD  D@ D� D� D  D9�D� D� D  D@ D� D� D   D!@ D"� D#� D%  D&@ D'� D(� D*  D+@ D,� D-� D/  D0@ D1� D2��D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm�fDn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  Dz��D|� D  D�� D�  D�<�D�� D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D�� D�  D�@ D�|�D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2�\B:�\BB(�BJ(�BQBYBb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B�{B�{B�{B�{B�{B�{C �=C�=C�=C�=C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr=pCup�Czp�C�=C�EC��RC�EC��C�EC��C�EC�޹C�8RC��RC�8RC��C�EC��C�EC��C�EC��C�EC��C�EC��C�EC��C�EC��C�EC��C�EC��C�8RC��C�EC��C�8RC���C�EC��C�EC��C�EC��C�EC��C�EC��C�EC��C�EC��C�ED �D"�Db�D��D�D"�Db�D	��D
�D"�Db�D��D��D"�Db�D��D�D"�D\)D��D�D"�Db�D��D�D "�D!b�D"��D#�D%"�D&b�D'��D(�D*"�D+b�D,��D-�D/"�D0b�D1��D2�\D4"�D5b�D6��D7�D9"�D:b�D;��D<�D>"�D?b�D@��DA�DC"�DDb�DE��DF�DH"�DIb�DJ��DK�DM"�DNb�DO��DP�DR"�DSb�DT��DU�DW"�DXb�DY��DZ�D\"�D]b�D^��D_�Da"�Dbb�Dc��Dd�Df"�Dgb�Dh��Di�Dk"�Dlb�Dm��Dn�Dp"�Dqb�Dr��Ds�Du"�Dvb�Dw��Dx�Dz"�D{\D|��D"�D��HD�HD�ND��HD��HD�HD�T{D��HD��D�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��D��HD�HD�QHD��D��HD�D�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�T{D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�%A���A���A��A��A��A��A��TA�Aڣ�Aڝ�Aڏ\AځA�XA�9XA�&�A�A���AټjA��HA���A�K�A�^5A�oA�n�A͏\A���A�/A�33AȅAǡ�A�
=A���A�bAÁA��TA��A�
=A��9A���A���A��mA���A��FA��HA���A��A�`BA���A��PA���A�ƨA���A�C�A�ĜA�^5A�l�A�O�A���A���A��wA�{A��wA���A��PA�(�A�+A�C�A�z�A��/A�jA�M�A�G�A�{A��-A�oA��RA�r�A��7A�ȴA�v�A�~�A��yA��^A�A��A{��Au��Ap~�Am�-AkVAf��A]x�A[+AZ�/AZQ�AY�hAV�yAQ&�AO�mAN��AM��AJ{AG�FAE`BAC��AA�mA@^5A?K�A;x�A8��A7�^A5�-A49XA3hsA2I�A1��A1\)A0v�A.ffA-�-A+�-A)dZA$�DA"�\A!;dAl�AdZA�AȴA{A�A(�AƨA|�A�/A�A
=AXA�#A��AS�A��AJA��AoA�!AĜA�PA
ZA
VA	�TA��A-A/Az�A�A��AA�AVA��AoA ��A @�5?@��h@�9X@��F@���@��@�bN@�5?@�u@��@�Z@��@�C�@�\@�`B@�|�@��@���@ާ�@ݺ^@ܣ�@�
=@��@�bN@�5?@�j@�ȴ@҇+@�^5@�/@���@�K�@�{@Ͳ-@̋D@���@ʏ\@ț�@�t�@�
=@Ƨ�@�=q@�r�@�+@���@��@��P@��!@��T@�1'@��@���@�?}@��@��@���@�J@��@�/@���@��
@���@�;d@�M�@��7@�O�@�V@��@�V@���@�/@�r�@��
@�t�@���@���@���@��D@�A�@��
@��@��@�%@���@��\@���@���@���@�t�@���@�^5@���@��D@���@���@���@�@�A�@���@���@��+@���@�Ĝ@�b@�@�-@�/@�(�@��+@���@�O�@��;@�@��@�`B@��j@�dZ@���@��@���@��F@��!@�@��`@�P@}`B@z�H@x�u@v��@t��@st�@q��@o�w@m�-@l1@j^5@h�u@gK�@e��@cƨ@b�@a7L@_��@^ȴ@\�j@Z�@Y&�@V�y@U/@St�@Q��@Pr�@N��@M�T@L�D@J��@J�@G�@E��@C33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111 A���A���A�%A���A���A��A��A��A��A��TA�Aڣ�Aڝ�Aڏ\AځA�XA�9XA�&�A�A���AټjA��HA���A�K�A�^5A�oA�n�A͏\A���A�/A�33AȅAǡ�A�
=A���A�bAÁA��TA��A�
=A��9A���A���A��mA���A��FA��HA���A��A�`BA���A��PA���A�ƨA���A�C�A�ĜA�^5A�l�A�O�A���A���A��wA�{A��wA���A��PA�(�A�+A�C�A�z�A��/A�jA�M�A�G�A�{A��-A�oA��RA�r�A��7A�ȴA�v�A�~�A��yA��^A�A��A{��Au��Ap~�Am�-AkVAf��A]x�A[+AZ�/AZQ�AY�hAV�yAQ&�AO�mAN��AM��AJ{AG�FAE`BAC��AA�mA@^5A?K�A;x�A8��A7�^A5�-A49XA3hsA2I�A1��A1\)A0v�A.ffA-�-A+�-A)dZA$�DA"�\A!;dAl�AdZA�AȴA{A�A(�AƨA|�A�/A�A
=AXA�#A��AS�A��AJA��AoA�!AĜA�PA
ZA
VA	�TA��A-A/Az�A�A��AA�AVA��AoA ��A @�5?@��h@�9X@��F@���@��@�bN@�5?@�u@��@�Z@��@�C�@�\@�`B@�|�@��@���@ާ�@ݺ^@ܣ�@�
=@��@�bN@�5?@�j@�ȴ@҇+@�^5@�/@���@�K�@�{@Ͳ-@̋D@���@ʏ\@ț�@�t�@�
=@Ƨ�@�=q@�r�@�+@���@��@��P@��!@��T@�1'@��@���@�?}@��@��@���@�J@��@�/@���@��
@���@�;d@�M�@��7@�O�@�V@��@�V@���@�/@�r�@��
@�t�@���@���@���@��D@�A�@��
@��@��@�%@���@��\@���@���@���@�t�@���@�^5@���@��D@���@���@���@�@�A�@���@���G�O�@���@�Ĝ@�b@�@�-@�/@�(�@��+@���@�O�@��;@�@��@�`B@��j@�dZ@���@��@���@��F@��!@�@��`@�P@}`B@z�H@x�u@v��@t��@st�@q��@o�w@m�-@l1@j^5@h�u@gK�@e��@cƨ@b�@a7L@_��@^ȴ@\�j@Z�@Y&�@V�y@U/@St�@Q��@Pr�@N��@M�T@L�D@J��@J�@G�@E��@C33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�dB
�sBP�Bv�B{�B}�B}�B~�B� B�B�B�B�%B�1B�1B�%B�B�B�%B�1B�Bo�B[#B<jB7LBF�B33B)�B.B<jBE�BO�BZB_;B�\B��B�B�RBȴB�B.BG�BP�BZBo�B�PB�bB��B��B��B��B��B�XB�qB�RB�?B�-B��B��B��B}�B^5BN�BD�B9XB5?B(�B�BuBoBPB%B�B��B�XB��By�Bl�BaHBG�B�BB
�ZB
��B
dZB
F�B
bB	��B	�'B	�B	bNB	O�B	>wB	%�B	�B	{B	uB	hB	PB	%B��B��B��B��B�B�B�TB�B��B��B�jB�XB�RB�RB�jBBĜB��B��B��B�
B��B	B	bB	�B	�B	oB	
=B	+B	�B	&�B	(�B	9XB	Q�B	e`B	[#B	E�B	E�B	F�B	]/B	�PB	��B	��B	��B	�B	�-B	�qB	ǮB	��B	�B	�B	��B	�mB	�B	�sB	�sB	�sB	�`B	��B	��B	��B	��B	��B
  B
B
%B
B
B
B
B	�B	�TB	�mB	�B	�yB	�fB	�NB	�TB	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�yB	�TB	�NB	�HB	�NB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B

=B

=B

=B
DB
DB
JB
PB
VB
VB
\B
bB
bB
bB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
#�B
$�B
&�B
'�B
)�B
)�B
)�B
,B
-B
.B
.B
0!B
1'B
2-B
33B
49B
6FB
7LB
9XB
:^B
;dB
<jB
>wB
?}B
@�B
A�B
B�B
C�B
D�B
E�B
E�B
F�B
G�B
H�B
H�B
J�B
K�B
L�B
M�B
N�B
O�B
P�B
P�B
Q�B
Q�B
S�B
T�B
W
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111 B
�;B
�MBP�Bv�B{�B}�B}�B~�B�B��B��B��B��B�B�
B��B��B��B��B�B��BotBZ�B<AB7!BF�B3B)�B-�B<@BEsBO�BY�B_B�/B��B��B�%BȉB�B-�BG�BP�BY�BooB�$B�3B�}B�zB��B��B��B�+B�BB�$B�B� B��B�{B�OB}�B]�BN�BDiB9&B5B(�BuBCB?BB�B�jB��B�#B�bBy�BlZBaBG|B`B�B
�(B
�~B
d*B
FzB
3B	��B	��B	��B	b!B	O�B	>NB	%�B	XB	QB	JB	?B	'B	�B��B��B��B��B�B�`B�+B��BʘB�aB�AB�0B�+B�*B�?B�fB�sBʗB��B��B��B��B	�B	5B	^B	[B	>B	
B	�B	�B	&�B	(�B	9%B	Q�B	e,B	Z�B	EoB	EpB	FsB	\�B	�B	��B	�qB	��B	��B	��B	�9B	�wB	ϥB	��B	��B	��B	�5B	�MB	�:B	�9B	�9B	�'B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B	�rB	�B	�1B	�CB	�?B	�+B	�B	�B	�IB	�LB	�HB	�`B	�MB	�JB	�PB	�:B	�=B	�BB	�PB	�OB	�=B	�B	�B	�B	�B	�'B	�7B	�AB	�HB	�MB	�MB	�QB	�VB	�PB	�NB	�MB	�OB	�NB	�TB	�ZB	�`B	�fB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
B
B

B
B
B
B
B
$B
$B
$B
(B
(B
.B
8B
:B
BB
CB
MB
TB
UG�O�B
[B
`B
eB
jB
qB
wB
}B
 �B
!�B
!�B
"�B
#�B
$�B
&�B
'�B
)�B
)�B
)�B
+�B
,�B
-�B
-�B
/�B
0�B
1�B
2�B
3�B
6B
7
B
9B
:B
;$B
<*B
>7B
?;B
@AB
AFB
BNB
CVB
D\B
EaB
EaB
FfB
GoB
HrB
HrB
J�B
K�B
L�B
M�B
N�B
O�B
P�B
P�B
Q�B
Q�B
S�B
T�B
V�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417082016080714170820160807141708  AO  ARCAADJP                                                                    20150226221238    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221238  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221238  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141708  IP                  G�O�G�O�G�O�                
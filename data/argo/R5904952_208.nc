CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:52Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190552  20181005190552  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��j�8�1   @��j��� @1�O�;dZ�c��E���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @��@�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A���A�  B   B��B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bg��Bo��Bw��B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C�fC  C  C  C  C  C  C  C �C"�C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  Dy�D  D� D  D� D  D� D  Dy�D  D� D  D�fDfD� D	  D	� D	��D
y�D
��Dy�D  D� D  Dy�D��D� D  D�fD  D� D  D�fD  D� DfD� D  D� DfD� D  D� D��D� D  D� D��Dy�D  D� D��D� DfD�fDfD�fD  Dy�D  D� D fD � D ��D!� D"  D"� D"��D#y�D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*y�D+  D+� D,  D,� D-fD-� D.  D.�fD/  D/� D0  D0y�D1  D1y�D2  D2�fD3  D3� D4  D4�fD5  D5y�D6  D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DD� DE  DEy�DE��DF� DG  DG�fDH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU�fDVfDV�fDWfDW� DW��DX� DY  DY� DZ  DZy�D[  D[� D\  D\�fD]  D]�fD^fD^� Dj�fDkfDk�fDl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq�fDr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw� Dy�{D�D)D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�=p@�
=@�
=A�A?�A_�A}�A�A�A�A�A�A��\A�A�Bz�B�HB�HB�HB'�HB/�HB7�HB?�HBGz�BO�HBW�HB_�HBgz�Boz�Bwz�B�HB�#�B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B�#�B��B��B��B��B�qB��B��B��B��B��B��C�RC�RC�RC�RC	�RC�C�RC޸C�RC�RC�RC�RC�RC�RC�RC �C"�C#�RC%�RC'�RC*�C+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{޸C}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��\C��\C��\C��\C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��\C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\D ~D �Dw�D�D~D�D~D�D~D�Dw�D�D~D�D�zDzD~D�D	~D	��D
w�D
��Dw�D�D~D�Dw�D��D~D�D�zD�D~D�D�zD�D~DzD~D�D~DzD~D�D~D��D~D�D~D��Dw�D�D~D��D~DzD�zDzD�zD�Dw�D�D~D zD ~D ��D!~D!�D"~D"��D#w�D#�D$�zD$�D%~D%�D&~D&�D'~D'�D(~D(��D)~D)�D*w�D*�D+~D+�D,~D-zD-~D-�D.�zD.�D/~D/�D0w�D0�D1w�D1�D2�zD2�D3~D3�D4�zD4�D5w�D5�D6~D7zD7�zD7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D?zD?~D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DD~DD�DEw�DE��DF~DF�DG�zDG�DHw�DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DNzDN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DSw�DS�DT~DT�DU�zDVzDV�zDWzDW~DW��DX~DX�DY~DY�DZw�DZ�D[~D[�D\�zD\�D]�zD^zD^~Dj�zDkzDk�zDk�Dl~Dl�Dm~Dm�Dn~DozDo~Do�Dp~Dp�Dq�zDq�Dr~DszDs~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dy��D�C3D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�\)A�`BA�bNA�ffA�hsA�hsA�hsA�n�A�r�A�v�A�z�A�~�AȁA�n�A�^5A�p�A�ZA�VA�^5A�^5A�`BA�\)A�l�A�v�A�Q�A�XA�Q�A�Q�A�VA�XA�XA�\)A�jA�v�A�t�A�l�A�^5A�S�A�^5A�XA�&�A�
=A�1'A�z�A�t�A�VA�M�A�M�A�M�A�;dA�;dAǾwA�
=Aƛ�A��/AƸRA��AƾwAōPA�p�A�hsA�hsA�hsA�n�A�t�A�|�A�~�A�v�A�9XA�  AĸRA�ZA°!A�=qA��A���A��A��A�M�A�1'A��9A��FA�+A�ffA��jA�r�A��#A�VA�G�A�t�A�n�A��A��A�\)A���A�S�A�&�A�ĜA���A���A�hsA��`A���A��^A�A�A�bA��A�jA�ffA���A�oA��-A���A�1A�$�A��-A��-A��A��A�;dA�G�A���A��`A��RA�  A}�A|Q�A{�Az�jAzn�Ax  Au��Aq�mAj^5Ah�uAe/Ab��A^ZAY&�AW�
AU�PAS
=AMAH��AGG�AD(�A@bNA<�`A:I�A9�A8-A7��A7`BA7S�A7C�A7A6�A3��A3�A2�!A0�`A.�/A+��A*1'A)��A'K�A%�A$^5A �`A��A��A
=A��A�AbNAbA��A-A�PA"�A�`A�RA�+A�A"�AĜA�\AA��A�/A=qA�TA�-A|�A+A�RA�uA��A��AbNAjAȴA �A
�A	�^A�AƨAbNA�9A��A�FA�A�RAhsA�!AA�A�TA`BA;dA/A�A ��@��R@���@�p�@�9X@��y@�hs@�z�@�n�@��@��/@�|�@�v�@�@�`B@���@�bN@�V@��@畁@�M�@���@��m@�~�@�&�@߮@���@�~�@ޏ\@݉7@���@��@�M�@��
@߶F@�C�@�33@�5?@ݡ�@޸R@��@��@��@��@��@��@��@�33@�@�hs@�&�@�/@�&�@��`@�z�@׶F@ץ�@ץ�@�"�@��#@�V@�C�@���@љ�@�A�@�  @� �@�\)@���@���@��H@��#@�G�@���@�bN@��@ģ�@�A�@� �@�33@���@�V@��7@�V@��@�V@���@��-@��@�$�@��^@��@���@���@���@���@��;@��
@�ƨ@�+@���@��\@��+@��@�O�@�G�@�7L@�?}@�&�@�V@�I�@��w@���@�l�@�S�@�V@���@�G�@�%@��`@��j@��@�1'@�1@� �@�9X@�(�@��
@�|�@�C�@��y@�~�@��#@�X@��@�I�@�9X@�9X@�9X@�1'@��
@�;d@�ȴ@���@��+@�-@�V@��j@��u@�r�@�I�@�1'@��@��F@�+@��\@�-@�{@��T@���@��7@�O�@���@���@�Q�@�(�@��;@�+@���@��\@���@���@�@��-@�x�@�G�@��`@�A�@���@��@��@��F@�dZ@�K�@�C�@�;d@�+@�@��\@�{@��@���@��-@���@��@��9@�bN@�b@���@�\)@�C�@��@��H@��R@���@�~�@�M�@��#@��@�@�x�@��@��`@���@�Ĝ@��j@��D@�Q�@��@���@�\)@�@�x�@�hs@�7L@��j@���@��D@��@��@�z�@�bN@�Z@��@�ƨ@��P@���@�|�@�
=@�ff@�-@�J@��@��^@�`B@��`@��D@�bN@�b@��m@�s@|[�@m<611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�\)A�`BA�bNA�ffA�hsA�hsA�hsA�n�A�r�A�v�A�z�A�~�AȁA�n�A�^5A�p�A�ZA�VA�^5A�^5A�`BA�\)A�l�A�v�A�Q�A�XA�Q�A�Q�A�VA�XA�XA�\)A�jA�v�A�t�A�l�A�^5A�S�A�^5A�XA�&�A�
=A�1'A�z�A�t�A�VA�M�A�M�A�M�A�;dA�;dAǾwA�
=Aƛ�A��/AƸRA��AƾwAōPA�p�A�hsA�hsA�hsA�n�A�t�A�|�A�~�A�v�A�9XA�  AĸRA�ZA°!A�=qA��A���A��A��A�M�A�1'A��9A��FA�+A�ffA��jA�r�A��#A�VA�G�A�t�A�n�A��A��A�\)A���A�S�A�&�A�ĜA���A���A�hsA��`A���A��^A�A�A�bA��A�jA�ffA���A�oA��-A���A�1A�$�A��-A��-A��A��A�;dA�G�A���A��`A��RA�  A}�A|Q�A{�Az�jAzn�Ax  Au��Aq�mAj^5Ah�uAe/Ab��A^ZAY&�AW�
AU�PAS
=AMAH��AGG�AD(�A@bNA<�`A:I�A9�A8-A7��A7`BA7S�A7C�A7A6�A3��A3�A2�!A0�`A.�/A+��A*1'A)��A'K�A%�A$^5A �`A��A��A
=A��A�AbNAbA��A-A�PA"�A�`A�RA�+A�A"�AĜA�\AA��A�/A=qA�TA�-A|�A+A�RA�uA��A��AbNAjAȴA �A
�A	�^A�AƨAbNA�9A��A�FA�A�RAhsA�!AA�A�TA`BA;dA/A�A ��@��R@���@�p�@�9X@��y@�hs@�z�@�n�@��@��/@�|�@�v�@�@�`B@���@�bN@�V@��@畁@�M�@���@��m@�~�@�&�@߮@���@�~�@ޏ\@݉7@���@��@�M�@��
@߶F@�C�@�33@�5?@ݡ�@޸R@��@��@��@��@��@��@��@�33@�@�hs@�&�@�/@�&�@��`@�z�@׶F@ץ�@ץ�@�"�@��#@�V@�C�@���@љ�@�A�@�  @� �@�\)@���@���@��H@��#@�G�@���@�bN@��@ģ�@�A�@� �@�33@���@�V@��7@�V@��@�V@���@��-@��@�$�@��^@��@���@���@���@���@��;@��
@�ƨ@�+@���@��\@��+@��@�O�@�G�@�7L@�?}@�&�@�V@�I�@��w@���@�l�@�S�@�V@���@�G�@�%@��`@��j@��@�1'@�1@� �@�9X@�(�@��
@�|�@�C�@��y@�~�@��#@�X@��@�I�@�9X@�9X@�9X@�1'@��
@�;d@�ȴ@���@��+@�-@�V@��j@��u@�r�@�I�@�1'@��@��F@�+@��\@�-@�{@��T@���@��7@�O�@���@���@�Q�@�(�@��;@�+@���@��\@���@���@�@��-@�x�@�G�@��`@�A�@���@��@��@��F@�dZ@�K�@�C�@�;d@�+@�@��\@�{@��@���@��-@���@��@��9@�bN@�b@���@�\)@�C�@��@��H@��R@���@�~�@�M�@��#@��@�@�x�@��@��`@���@�Ĝ@��j@��D@�Q�@��@���@�\)@�@�x�@�hs@�7L@��j@���@��D@��@��@�z�@�bN@�Z@��@�ƨ@��P@���@�|�@�
=@�ff@�-@�J@��@��^@�`B@��`@��D@�bN@�b@��m@�s@|[�@m<611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B/B\)BhsBr�Bv�Bv�Bv�Bu�Bt�Bw�B{�Bx�Bz�Bx�Bx�Bw�Bt�By�B�B�B�B�+B�=B�\B�oB�hB�DB�B�B�5BB'�B;dB#�B!�B#�B"�B!�B.B:^B<jB>wBA�BE�BF�BC�B?}B;dB6FB33B0!B.B0!B/B/B,B&�B�B�B�BPBB��B�B��Bu�Bn�BiyBR�BF�B>wB/B�B
��B
�)B
�wB
��B
�1B
o�B
Q�B
9XB
-B
�B
+B	��B	�B	�mB	�mB	�`B	�B	ĜB	��B	w�B	jB	^5B	P�B	7LB	�B	bB	
=B	B��B�`B�;B��BɺBĜB��B��B�}B�wB�wB�wB�qB�}B�qB�wB�jB�^B�RB�!B��B��B��B�{B�bB�=B�B~�Bx�Bs�Br�Br�Bq�Bp�Bp�Br�Bs�Bs�Br�Br�Br�Bq�Bq�Bq�Bq�Bq�Bp�Bq�Bt�Bt�Bt�Bs�Bt�Bw�B�B�+B�1B��B��B��B��B��B��B��B��B�-B�XB�qB��B��BBÖBÖBÖBÖBĜBŢBŢBŢBĜBƨBĜBǮBƨBŢBƨBŢBŢBŢBɺB��B��B��B��B��B��B��B�
B�)B�/B�HB�ZB�B�B�yB�B�B��B��B��B	B	JB	 �B	+B	,B	(�B	0!B	I�B	\)B	bNB	bNB	bNB	bNB	bNB	aHB	_;B	_;B	cTB	n�B	n�B	o�B	r�B	r�B	q�B	s�B	|�B	�%B	�B	�%B	�+B	�B	�B	�B	�B	�B	�%B	�B	�B	~�B	� B	� B	�B	�B	� B	}�B	}�B	�B	�7B	�7B	�1B	�+B	�%B	�B	�B	� B	~�B	~�B	�B	�B	�%B	�B	�1B	�VB	�\B	�PB	�PB	�VB	�VB	�VB	�\B	�\B	�bB	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�?B	�FB	�FB	�FB	�^B	�jB	�wB	��B	��B	��B	B	ÖB	ŢB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
B
B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
DB
DB
JB
JB
JB
\B
bB
bB
bB
\B
\B
\B
\B
\B
bB
hB
�B
$ZB
/522222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B/B\)BhsBr�Bv�Bv�Bv�Bu�Bt�Bw�B{�Bx�Bz�Bx�Bx�Bw�Bt�By�B�B�B�B�+B�=B�\B�oB�hB�DB�B�B�5BB'�B;dB#�B!�B#�B"�B!�B.B:^B<jB>wBA�BE�BF�BC�B?}B;dB6FB33B0!B.B0!B/B/B,B&�B�B�B�BPBB��B�B��Bu�Bn�BiyBR�BF�B>wB/B�B
��B
�)B
�wB
��B
�1B
o�B
Q�B
9XB
-B
�B
+B	��B	�B	�mB	�mB	�`B	�B	ĜB	��B	w�B	jB	^5B	P�B	7LB	�B	bB	
=B	B��B�`B�;B��BɺBĜB��B��B�}B�wB�wB�wB�qB�}B�qB�wB�jB�^B�RB�!B��B��B��B�{B�bB�=B�B~�Bx�Bs�Br�Br�Bq�Bp�Bp�Br�Bs�Bs�Br�Br�Br�Bq�Bq�Bq�Bq�Bq�Bp�Bq�Bt�Bt�Bt�Bs�Bt�Bw�B�B�+B�1B��B��B��B��B��B��B��B��B�-B�XB�qB��B��BBÖBÖBÖBÖBĜBŢBŢBŢBĜBƨBĜBǮBƨBŢBƨBŢBŢBŢBɺB��B��B��B��B��B��B��B�
B�)B�/B�HB�ZB�B�B�yB�B�B��B��B��B	B	JB	 �B	+B	,B	(�B	0!B	I�B	\)B	bNB	bNB	bNB	bNB	bNB	aHB	_;B	_;B	cTB	n�B	n�B	o�B	r�B	r�B	q�B	s�B	|�B	�%B	�B	�%B	�+B	�B	�B	�B	�B	�B	�%B	�B	�B	~�B	� B	� B	�B	�B	� B	}�B	}�B	�B	�7B	�7B	�1B	�+B	�%B	�B	�B	� B	~�B	~�B	�B	�B	�%B	�B	�1B	�VB	�\B	�PB	�PB	�VB	�VB	�VB	�\B	�\B	�bB	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�?B	�FB	�FB	�FB	�^B	�jB	�wB	��B	��B	��B	B	ÖB	ŢB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
B
B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
DB
DB
JB
JB
JB
\B
bB
bB
bB
\B
\B
\B
\B
\B
bB
hB
�B
$ZB
/522222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190552                              AO  ARCAADJP                                                                    20181005190552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190552  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190552  QCF$                G�O�G�O�G�O�8000            
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-07-30T07:01:21Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220730070121  20220730070121  4903175 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @���3;�1   @��Q�v�@3�dZ��c�7KƧ�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dxy�Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C޸C�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC޸C�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D	zD	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK�zDK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw��Dxw�Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D���D�;�D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D¿
D��
D�?
D�
Dÿ
D��
D�?
D�
DĿ
D��
D�?
D�
Dſ
D��
D�?
D�
Dƿ
D��
D�?
D�
Dǿ
D��
D�?
D�
Dȿ
D��
D�?
D�
Dɿ
D��
D�?
D�
Dʿ
D��
D�?
D�
D˿
D��
D�?
D�
D̿
D��
D�?
D�
DͿ
D��
D�?
D�
Dο
D��
D�?
D�
DϿ
D��
D�?
D�
Dп
D��
D�?
D�
Dѿ
D��
D�?
D�
Dҿ
D��
D�?
D�
Dӿ
D��
D�?
D�
DԿ
D��
D�?
D�
Dտ
D��
D�?
D�
Dֿ
D��
D�?
D�
D׿
D��
D�?
D�
Dؿ
D��
D�?
D�
Dٿ
D��
D�?
D�
Dڿ
D��
D�?
D�
Dۿ
D��
D�?
D�
Dܿ
D��
D�?
D�
Dݿ
D��
D�?
D�
D޿
D��
D�?
D�
D߿
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�H�D�k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AظRAذ!AخAش9A�bNA׸RA�ZA�G�A�C�A�I�A�ZA�v�A�dZA�1A��#A��/A��HA־wA֗�A�~�A�hsA�ffA��A�ZA��yA�r�Aқ�AэPA�O�A�A�A�JA�bNA��AǼjA�%A�M�AŮA� �AļjAĕ�Aß�A�&�A��;A�VA�=qA�p�A�bNA���A��A�ƨA�G�A�5?A���A��+A�VA�M�A��A�C�A�v�A�O�A�Q�A��;A��A�{A��yA�n�A��A�S�A��!A�VA�K�A��+A��A�VA�dZA��DA���A���A���A��\A�n�A���A�5?A��A���A�ZA��mA��A�bNA�v�A�XA���A�|�A�Q�A��yA�|�A�(�A�-A�l�A���A�mA~^5A|�uAyK�Av=qAt1'Ar��Aq�mAq��Ao�PAn��Am;dAjbAiVAg�hAd�AbI�A`�A_oA\bNAZ�RAY�AWG�AUK�AR�AQ��AOK�AL��AK��AK|�AJ�AH��AEAD=qAC�AB�`A@��A>�/A>  A=��A=&�A8�A5?}A3"�A0~�A/�7A.�jA,n�A*(�A'|�A&�A%�A%\)A%VA#%A!%A ffA 9XA M�A �jA I�AAK�A�A��A��A33AXA��AC�A�A��A%A��A33A=qA7LAC�AI�A�mA�#AbAp�A��A1'A�;A��A?}A
�9A	�mA	`BA	;dA�AoAȴA�DAffAA��A�A��A�PA�
A�uA�
AA�A��A
=A @��-@�O�@�j@���@�"�@�G�@�n�@��T@�&�@�@�@���@�7L@�I�@��@��@��T@� �@���@�G�@��;@�\)@ᙚ@�C�@ް!@�5?@�x�@�Z@�+@��@��`@�j@׾w@ץ�@�C�@�~�@�hs@ԋD@�|�@ҸR@Ѳ-@�z�@���@��H@�^5@�&�@�I�@�\)@ʗ�@���@Ȭ@��@ǍP@ƸR@���@�`B@�j@�C�@�ff@���@��h@�&�@�z�@��m@���@�t�@���@��T@�x�@�7L@��`@��D@�Q�@���@��H@�n�@�/@���@� �@��P@�"�@��@�M�@�@���@�hs@���@��u@�I�@�1'@�1@���@�C�@��y@���@�^5@�{@��@��^@�hs@�7L@��@��9@�r�@��@���@�l�@�K�@�o@���@��!@�^5@��@�O�@�%@�bN@���@���@��\@��@��#@��-@���@���@�p�@���@��@��D@�1@���@��@��P@�dZ@���@��+@�ff@�^5@�E�@�{@���@�&�@��j@�1'@��;@�ƨ@���@�t�@�33@��@���@�~�@�5?@��@��@�`B@��@��@��@�%@���@��j@��@�Z@� �@��F@�K�@��H@��\@�n�@�V@�-@���@���@�G�@�&�@�%@��/@���@��9@��D@�r�@�Z@�Q�@� �@��w@���@�dZ@�"�@��y@��H@���@��+@�^5@�V@�M�@�-@���@��@���@��`@���@���@�ƨ@��@�o@���@�v�@��@���@�hs@���@���@��@�I�@��
@��
@�(�@�  @��P@��@��@�"�@��H@�v�@��@�J@�@���@��T@��-@��@��`@���@�  @�+@��H@���@�$�@��@��-@��@��@��`@���@�r�@�I�@�9X@��@�b@��@���@��@��@�o@���@�n�@�E�@�5?@��@��@��7@�x�@�X@�7L@��9@�z�@�Q�@�1@��F@�l�@�o@�v�@�5?@�5?@�5?@�J@���@���@���@��h@�&�@���@���@��9@�1'@��m@��w@�S�@�@��y@��H@��@���@��R@�n�@�-@��@���@���@��7@�p�@�O�@�O�@�X@�X@�X@�G�@�V@���@���@���@��D@�9X@�b@�@��@~�R@~E�@}p�@|�j@|��@}/@}?}@}/@|��@|Z@|Z@|Z@{ƨ@{o@{33@z�@z�@xr�@w\)@w�@w��@w��@w+@v��@v{@u�-@uO�@tj@s��@s�@s33@s"�@s"�@s@rn�@r=q@q��@p��@pA�@o;d@nff@nV@n5?@n$�@n@m�@m@l�/@kƨ@k@j�!@j~�@jM�@j�@i��@ihs@i&�@h�9@h1'@g�;@g�P@g;d@f��@f$�@ep�@eO�@e/@d�j@d��@d(�@c"�@b��@bn�@b^5@b^5@a��@ax�@`��@`�@`Q�@`A�@`  @_��@^��@^ff@^@]��@]@]O�@\��@\I�@[�m@[�F@[��@[��@[��@[��@[��@[��@[@Z=q@Y�#@Y��@X�`@X��@X��@Xr�@W��@WK�@W;d@W
=@Vv�@V{@U��@U/@T�/@T�j@T�D@T(�@T�@T1@S�
@S�F@S�F@S��@S33@RM�@Q��@Q�@Qhs@Q�@P�`@P �@O��@O�w@O��@O\)@O
=@N��@N�y@N�@Nȴ@NE�@M@M�@LI�@Kƨ@K��@K33@J�\@J^5@J=q@J-@I��@I��@I��@I�7@I�7@Ix�@I&�@I�@I%@I%@H��@H�@G�@Fȴ@Fff@E�@D�j@DI�@D1@CS�@B��@B~�@B-@A�@A�^@AG�@A%@@�u@@ �@@b@?�@?��@?��@?�P@?\)@?
=@>��@>�y@>�y@>�@>��@>v�@>V@>E�@>{@=�-@=O�@<��@<z�@<1@;ƨ@;�F@;�@;S�@;o@:��@:�\@:-@9�^@9X@8Ĝ@8�@8bN@81'@7�@7��@7K�@6ȴ@6�+@65?@6@5�@5��@5�-@5�h@5�@5/@5�@4�@4z�@4I�@49X@4(�@4�@3��@3ƨ@3C�@3o@2��@2^5@2-@2�@2�@1�@1�^@1hs@17L@1�@1%@0��@0��@0��@0bN@0 �@/�w@/|�@/l�@/K�@.��@.v�@.V@.V@.E�@.5?@.$�@.$�@.$�@.{@.{@.@-@-�@-/@,�j@,9X@+�m@+ƨ@+��@+�@+S�@+33@+33@+"�@*�H@*��@*=q@)��@)��@)hs@(�u@(1'@(b@(  @'�;@'K�@'+@'�@&�y@&ȴ@&v�@&5?@&$�@&{@%�T@%�h@%p�@$��@$�/@$�j@$z�@$(�@#t�@#o@#o@#o@"�!@"~�@"n�@"=q@"-@!�@!�7@!X@!&�@!�@!%@ ��@ ��@ ��@ ��@ �@ r�@ bN@ Q�@ 1'@�@|�@|�@\)@�@�@ȴ@�R@��@5?@�-@�h@�@p�@`B@O�@/@V@��@�@��@�D@z�@j@j@I�@I�@��@�m@�
@�F@��@�@S�@C�@"�@��@^5@M�@-@�@�@�#@��@��@X@7L@%@��@�@Q�@ �@  @�@��@�w@�@�P@l�@K�@+@+@
=@��@V@$�@@�-@`B@`B@O�@�@V@�@��@�D@j@j@Z@(�@ƨ@��@t�@"�@@M�@J@��@X@�`@Ĝ@�9@�@1'@��@K�@
=@ȴ@��@v�@v�@ff@ff@V@E�@E�@5?@5?@{@��@@�-@p�@V@��@��@��@�@��@�j@�@j@9X@(�@�@1@��@�m@�
@��@dZ@C�@
�@
�!@
n�@
n�@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AظRAذ!AخAش9A�bNA׸RA�ZA�G�A�C�A�I�A�ZA�v�A�dZA�1A��#A��/A��HA־wA֗�A�~�A�hsA�ffA��A�ZA��yA�r�Aқ�AэPA�O�A�A�A�JA�bNA��AǼjA�%A�M�AŮA� �AļjAĕ�Aß�A�&�A��;A�VA�=qA�p�A�bNA���A��A�ƨA�G�A�5?A���A��+A�VA�M�A��A�C�A�v�A�O�A�Q�A��;A��A�{A��yA�n�A��A�S�A��!A�VA�K�A��+A��A�VA�dZA��DA���A���A���A��\A�n�A���A�5?A��A���A�ZA��mA��A�bNA�v�A�XA���A�|�A�Q�A��yA�|�A�(�A�-A�l�A���A�mA~^5A|�uAyK�Av=qAt1'Ar��Aq�mAq��Ao�PAn��Am;dAjbAiVAg�hAd�AbI�A`�A_oA\bNAZ�RAY�AWG�AUK�AR�AQ��AOK�AL��AK��AK|�AJ�AH��AEAD=qAC�AB�`A@��A>�/A>  A=��A=&�A8�A5?}A3"�A0~�A/�7A.�jA,n�A*(�A'|�A&�A%�A%\)A%VA#%A!%A ffA 9XA M�A �jA I�AAK�A�A��A��A33AXA��AC�A�A��A%A��A33A=qA7LAC�AI�A�mA�#AbAp�A��A1'A�;A��A?}A
�9A	�mA	`BA	;dA�AoAȴA�DAffAA��A�A��A�PA�
A�uA�
AA�A��A
=A @��-@�O�@�j@���@�"�@�G�@�n�@��T@�&�@�@�@���@�7L@�I�@��@��@��T@� �@���@�G�@��;@�\)@ᙚ@�C�@ް!@�5?@�x�@�Z@�+@��@��`@�j@׾w@ץ�@�C�@�~�@�hs@ԋD@�|�@ҸR@Ѳ-@�z�@���@��H@�^5@�&�@�I�@�\)@ʗ�@���@Ȭ@��@ǍP@ƸR@���@�`B@�j@�C�@�ff@���@��h@�&�@�z�@��m@���@�t�@���@��T@�x�@�7L@��`@��D@�Q�@���@��H@�n�@�/@���@� �@��P@�"�@��@�M�@�@���@�hs@���@��u@�I�@�1'@�1@���@�C�@��y@���@�^5@�{@��@��^@�hs@�7L@��@��9@�r�@��@���@�l�@�K�@�o@���@��!@�^5@��@�O�@�%@�bN@���@���@��\@��@��#@��-@���@���@�p�@���@��@��D@�1@���@��@��P@�dZ@���@��+@�ff@�^5@�E�@�{@���@�&�@��j@�1'@��;@�ƨ@���@�t�@�33@��@���@�~�@�5?@��@��@�`B@��@��@��@�%@���@��j@��@�Z@� �@��F@�K�@��H@��\@�n�@�V@�-@���@���@�G�@�&�@�%@��/@���@��9@��D@�r�@�Z@�Q�@� �@��w@���@�dZ@�"�@��y@��H@���@��+@�^5@�V@�M�@�-@���@��@���@��`@���@���@�ƨ@��@�o@���@�v�@��@���@�hs@���@���@��@�I�@��
@��
@�(�@�  @��P@��@��@�"�@��H@�v�@��@�J@�@���@��T@��-@��@��`@���@�  @�+@��H@���@�$�@��@��-@��@��@��`@���@�r�@�I�@�9X@��@�b@��@���@��@��@�o@���@�n�@�E�@�5?@��@��@��7@�x�@�X@�7L@��9@�z�@�Q�@�1@��F@�l�@�o@�v�@�5?@�5?@�5?@�J@���@���@���@��h@�&�@���@���@��9@�1'@��m@��w@�S�@�@��y@��H@��@���@��R@�n�@�-@��@���@���@��7@�p�@�O�@�O�@�X@�X@�X@�G�@�V@���@���@���@��D@�9X@�b@�@��@~�R@~E�@}p�@|�j@|��@}/@}?}@}/@|��@|Z@|Z@|Z@{ƨ@{o@{33@z�@z�@xr�@w\)@w�@w��@w��@w+@v��@v{@u�-@uO�@tj@s��@s�@s33@s"�@s"�@s@rn�@r=q@q��@p��@pA�@o;d@nff@nV@n5?@n$�@n@m�@m@l�/@kƨ@k@j�!@j~�@jM�@j�@i��@ihs@i&�@h�9@h1'@g�;@g�P@g;d@f��@f$�@ep�@eO�@e/@d�j@d��@d(�@c"�@b��@bn�@b^5@b^5@a��@ax�@`��@`�@`Q�@`A�@`  @_��@^��@^ff@^@]��@]@]O�@\��@\I�@[�m@[�F@[��@[��@[��@[��@[��@[��@[@Z=q@Y�#@Y��@X�`@X��@X��@Xr�@W��@WK�@W;d@W
=@Vv�@V{@U��@U/@T�/@T�j@T�D@T(�@T�@T1@S�
@S�F@S�F@S��@S33@RM�@Q��@Q�@Qhs@Q�@P�`@P �@O��@O�w@O��@O\)@O
=@N��@N�y@N�@Nȴ@NE�@M@M�@LI�@Kƨ@K��@K33@J�\@J^5@J=q@J-@I��@I��@I��@I�7@I�7@Ix�@I&�@I�@I%@I%@H��@H�@G�@Fȴ@Fff@E�@D�j@DI�@D1@CS�@B��@B~�@B-@A�@A�^@AG�@A%@@�u@@ �@@b@?�@?��@?��@?�P@?\)@?
=@>��@>�y@>�y@>�@>��@>v�@>V@>E�@>{@=�-@=O�@<��@<z�@<1@;ƨ@;�F@;�@;S�@;o@:��@:�\@:-@9�^@9X@8Ĝ@8�@8bN@81'@7�@7��@7K�@6ȴ@6�+@65?@6@5�@5��@5�-@5�h@5�@5/@5�@4�@4z�@4I�@49X@4(�@4�@3��@3ƨ@3C�@3o@2��@2^5@2-@2�@2�@1�@1�^@1hs@17L@1�@1%@0��@0��@0��@0bN@0 �@/�w@/|�@/l�@/K�@.��@.v�@.V@.V@.E�@.5?@.$�@.$�@.$�@.{@.{@.@-@-�@-/@,�j@,9X@+�m@+ƨ@+��@+�@+S�@+33@+33@+"�@*�H@*��@*=q@)��@)��@)hs@(�u@(1'@(b@(  @'�;@'K�@'+@'�@&�y@&ȴ@&v�@&5?@&$�@&{@%�T@%�h@%p�@$��@$�/@$�j@$z�@$(�@#t�@#o@#o@#o@"�!@"~�@"n�@"=q@"-@!�@!�7@!X@!&�@!�@!%@ ��@ ��@ ��@ ��@ �@ r�@ bN@ Q�@ 1'@�@|�@|�@\)@�@�@ȴ@�R@��@5?@�-@�h@�@p�@`B@O�@/@V@��@�@��@�D@z�@j@j@I�@I�@��@�m@�
@�F@��@�@S�@C�@"�@��@^5@M�@-@�@�@�#@��@��@X@7L@%@��@�@Q�@ �@  @�@��@�w@�@�P@l�@K�@+@+@
=@��@V@$�@@�-@`B@`B@O�@�@V@�@��@�D@j@j@Z@(�@ƨ@��@t�@"�@@M�@J@��@X@�`@Ĝ@�9@�@1'@��@K�@
=@ȴ@��@v�@v�@ff@ff@V@E�@E�@5?@5?@{@��@@�-@p�@V@��@��@��@�@��@�j@�@j@9X@(�@�@1@��@�m@�
@��@dZ@C�@
�@
�!@
n�@
n�@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��/A���A��7A�jA�  A�Q�A��A�VA�A�^5A�?}A��
A��RA���A�bA�bA�A�A�l�A�ƨA�ƨA�K�A���A���A�-A��uA��A��uA�VA�
=A�ȴA��-A��-A��\A��yA�ȴA�$�A���A�  A�  A���A��\A���A�ƨA���A�A�Q�A��wA��A���A���A���A�ĜA���A�dZA�|�A���A���A�z�A�33A�|�A�x�A���AѬA�hsA�S�A�"�A�VA�FA��A�E�A�^5A�A���A��;A���A�oA�ZA���A��+A��uA���A���A�~�A�n�A�t�A�t�A��+A��PA��PA�|�A�~�A�Q�A�K�A��uA�z�A�S�A�Q�A�33A��`A�A�{A��A�ȴA���A���A��
A��A���A��PA�~�A�I�A��A�(�A���A���A���A���A��FA���A���A��A�p�A�dZA�\)A�dZA�I�A�Q�A�t�A�ffA�A�A�bA�JA�+A�/A�oA��A��A�JA�  A�ƨA�jA�p�A��DA�~�A���A�~�A�S�A�S�A�VA��A��+A�z�A�n�A�K�A�bNA��DA���A���A���A��A�~�A�~�A��A�t�A�`BA�\)A�A�A�XA�v�A�x�A�l�A�VA�x�A�p�A�VA�K�A�=qA�G�A�?}A��A�-A�G�A�I�A�G�A�O�A�K�A�A�A�;dA�33A�;dA�;dA�&�A�7LA�K�A�K�A�I�A�C�A�I�A�K�A�A�A�=qA�;dA� �A�-A�-A�5?A��A���A�bA�{A�&�A��A��A�JA���A���A��A�bA�%A�
=A��A�
=A���A��yA���A���A���A�A�A�A�
=A���A�A��A��A�{A�VA�VA�bA��A��A��A��A�{A�JA�1A�
=A�1A�
=A�%A�1A�bA�JA�JA�A�
=A�
=A�VA�VA�JA�{A��A�bA�oA��A�{A��A� �A�(�A�+A�(�A�&�A�+A�-A�(�A�"�A�&�A�/A�1'A�-A�-A�+A�"�A� �A�&�A�"�A�1'A�33A�5?A�9XA�9XA�7LA�=qA�=qA�9XA�;dA�=qA�?}A�A�A�?}A�;dA�?}A�A�A�C�A�C�A�E�A�E�A�C�A�A�A�C�A�A�A�A�A�A�A�=qA�=qA�A�A�A�A�=qA�;dA�=qA�7LA�5?A�1'A�5?A�/A�1'A�7LA�;dA�?}A�G�A�E�A�E�A�C�A�?}A�=qA�C�A�=qA�?}A�E�A�E�A�C�A�A�A�=qA�I�A�I�A�K�A�G�A�C�A�?}A�A�A�E�A�G�A�M�A�Q�A�O�A�O�A�K�A�K�A�K�A�I�A�I�A�M�A�I�A�G�A�M�A�Q�A�O�A�M�A�K�A�G�A�G�A�G�A�E�A�C�A�E�A�G�A�K�A�O�A�M�A�M�A�K�A�I�A�K�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�O�A�K�A�K�A�O�A�O�A�O�A�Q�A�S�A�S�A�Q�A�VA�ZA�ZA�VA�S�A�Q�A�ZA�ZA�VA�Q�A�K�A�Q�A�O�A�O�A�Q�A�O�A�O�A�M�A�O�A�S�A�VA�VA�XA�`BA�`BA�\)A�ZA�\)A�dZA�bNA�^5A�^5A�`BA�ffA�dZA�bNA�^5A�XA�S�A�VA�Q�A�M�A�M�A�S�A�Q�A�O�A�S�A�S�A�Q�A�O�A�S�A�Q�A�S�A�VA�VA�VA�VA�Q�A�O�A�M�A�K�A�G�A�K�A�K�A�M�A�M�A�K�A�I�A�G�A�K�A�G�A�E�A�A�A�E�A�E�A�A�A�?}A�?}A�=qA�=qA�C�A�E�A�C�A�?}A�?}A�A�A�?}A�;dA�7LA�9XA�;dA�5?A�1'A�5?A�5?A�1'A�33A�7LA�7LA�7LA�5?A�33A�/A�1'A�1'A�1'A�33A�33A�33A�1'A�33A�1'A�1'A�/A�+A�(�A�(�A�+A�&�A�&�A�"�A�$�A�"�A� �A��A��A��A��A�&�A�$�A�"�A� �A��A��A��A��A��A��A��A�oA�JA�
=A�bA�{A��A�bA�VA�VA�VA�VA�VA�
=A�JA�JA�VA�VA�JA�1A�A�A�  A���A�  A���A�  A�A�A�A���A���A���A��A��A���A���A���A���A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��yA��mA��TA��TA��;A��/A��TA��`A��`A��HA��;A��/A��/A��/A��;A��/A��A��
A���A��
A��
A��
A��
A���A���A���A���A��
A��
A���A���A���A���A���A�ȴA�ȴA���A���A�ȴA���A���A���A�ƨA�ȴA���A�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA�ȴA���A�ȴA�ȴA�ȴA�ƨA�ĜA���A��wA�ĜA�ĜA�A�A�A��wA�A�A�A���A���A�A���A��wA��jA��RA��FA��FA��-A��RA��^A��RA��RA��^A��jA��jA��^A��^A��^A��^A��^A��RA��FA��FA��9A��-A��!A���A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA���A���A���A���A���A���A���A���A��uA��uA��hA��hA��uA��uA��uA��hA��\A��PA��PA��\A��PA��PA��\A��\A��\A��PA��DA��DA��PA��PA��PA��DA��DA��7A��+A��+A��+A��7A��7A��+A��A��A��7A��DA��7A��7A��7A��7A��7A��+A��A��A��A��A�~�A�|�A�~�A�~�A��A��A�~�A�~�A�~�A�|�A�z�A�x�A�x�A�v�A�v�A�v�A�r�A�p�A�t�A�v�A�v�A�t�A�p�A�t�A�t�A�r�A�r�A�p�A�p�A�p�A�n�A�l�A�l�A�l�A�jA�jA�hsA�ffA�dZA�dZA�ffA�hsA�ffA�dZA�dZA�dZA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�^5A�^5A�\)A�\)A�\)A�ZA�XA�XA�S�A�XA�XA�S�A�S�A�S�A�S�A�Q�A�O�A�M�A�M�A�Q�A�Q�A�O�A�O�A�M�A�M�A�K�A�K�A�I�A�I�A�I�A�I�A�G�A�G�A�E�A�E�A�C�A�C�A�C�A�A�A�?}A�?}A�=qA�=qA�;dA�9XA�9XA�9XA�9XA�7LA�7LA�5?A�33A�1'A�1'A�/A�/A�-A�-A�+A�+A�(�A�(�A�(�A�&�A�$�A�$�A�"�A�"�A� �A� �A��A��A��A��A��A��A��A��A��A�{A�{A�oA�oA�oA�oA�bA�VA�JA�
=A�
=A�1A�%A�A�  A�A�  A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��yA��mA��mA��mA��`A��TA��TA��TA��TA��TA��`A��mA��A��TA�"�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   A��/A���A��7A�jA�  A�Q�A��A�VA�A�^5A�?}A��
A��RA���A�bA�bA�A�A�l�A�ƨA�ƨA�K�A���A���A�-A��uA��A��uA�VA�
=A�ȴA��-A��-A��\A��yA�ȴA�$�A���A�  A�  A���A��\A���A�ƨA���A�A�Q�A��wA��A���A���A���A�ĜA���A�dZA�|�A���A���A�z�A�33A�|�A�x�A���AѬA�hsA�S�A�"�A�VA�FA��A�E�A�^5A�A���A��;A���A�oA�ZA���A��+A��uA���A���A�~�A�n�A�t�A�t�A��+A��PA��PA�|�A�~�A�Q�A�K�A��uA�z�A�S�A�Q�A�33A��`A�A�{A��A�ȴA���A���A��
A��A���A��PA�~�A�I�A��A�(�A���A���A���A���A��FA���A���A��A�p�A�dZA�\)A�dZA�I�A�Q�A�t�A�ffA�A�A�bA�JA�+A�/A�oA��A��A�JA�  A�ƨA�jA�p�A��DA�~�A���A�~�A�S�A�S�A�VA��A��+A�z�A�n�A�K�A�bNA��DA���A���A���A��A�~�A�~�A��A�t�A�`BA�\)A�A�A�XA�v�A�x�A�l�A�VA�x�A�p�A�VA�K�A�=qA�G�A�?}A��A�-A�G�A�I�A�G�A�O�A�K�A�A�A�;dA�33A�;dA�;dA�&�A�7LA�K�A�K�A�I�A�C�A�I�A�K�A�A�A�=qA�;dA� �A�-A�-A�5?A��A���A�bA�{A�&�A��A��A�JA���A���A��A�bA�%A�
=A��A�
=A���A��yA���A���A���A�A�A�A�
=A���A�A��A��A�{A�VA�VA�bA��A��A��A��A�{A�JA�1A�
=A�1A�
=A�%A�1A�bA�JA�JA�A�
=A�
=A�VA�VA�JA�{A��A�bA�oA��A�{A��A� �A�(�A�+A�(�A�&�A�+A�-A�(�A�"�A�&�A�/A�1'A�-A�-A�+A�"�A� �A�&�A�"�A�1'A�33A�5?A�9XA�9XA�7LA�=qA�=qA�9XA�;dA�=qA�?}A�A�A�?}A�;dA�?}A�A�A�C�A�C�A�E�A�E�A�C�A�A�A�C�A�A�A�A�A�A�A�=qA�=qA�A�A�A�A�=qA�;dA�=qA�7LA�5?A�1'A�5?A�/A�1'A�7LA�;dA�?}A�G�A�E�A�E�A�C�A�?}A�=qA�C�A�=qA�?}A�E�A�E�A�C�A�A�A�=qA�I�A�I�A�K�A�G�A�C�A�?}A�A�A�E�A�G�A�M�A�Q�A�O�A�O�A�K�A�K�A�K�A�I�A�I�A�M�A�I�A�G�A�M�A�Q�A�O�A�M�A�K�A�G�A�G�A�G�A�E�A�C�A�E�A�G�A�K�A�O�A�M�A�M�A�K�A�I�A�K�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�O�A�K�A�K�A�O�A�O�A�O�A�Q�A�S�A�S�A�Q�A�VA�ZA�ZA�VA�S�A�Q�A�ZA�ZA�VA�Q�A�K�A�Q�A�O�A�O�A�Q�A�O�A�O�A�M�A�O�A�S�A�VA�VA�XA�`BA�`BA�\)A�ZA�\)A�dZA�bNA�^5A�^5A�`BA�ffA�dZA�bNA�^5A�XA�S�A�VA�Q�A�M�A�M�A�S�A�Q�A�O�A�S�A�S�A�Q�A�O�A�S�A�Q�A�S�A�VA�VA�VA�VA�Q�A�O�A�M�A�K�A�G�A�K�A�K�A�M�A�M�A�K�A�I�A�G�A�K�A�G�A�E�A�A�A�E�A�E�A�A�A�?}A�?}A�=qA�=qA�C�A�E�A�C�A�?}A�?}A�A�A�?}A�;dA�7LA�9XA�;dA�5?A�1'A�5?A�5?A�1'A�33A�7LA�7LA�7LA�5?A�33A�/A�1'A�1'A�1'A�33A�33A�33A�1'A�33A�1'A�1'A�/A�+A�(�A�(�A�+A�&�A�&�A�"�A�$�A�"�A� �A��A��A��A��A�&�A�$�A�"�A� �A��A��A��A��A��A��A��A�oA�JA�
=A�bA�{A��A�bA�VA�VA�VA�VA�VA�
=A�JA�JA�VA�VA�JA�1A�A�A�  A���A�  A���A�  A�A�A�A���A���A���A��A��A���A���A���A���A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��yA��mA��TA��TA��;A��/A��TA��`A��`A��HA��;A��/A��/A��/A��;A��/A��A��
A���A��
A��
A��
A��
A���A���A���A���A��
A��
A���A���A���A���A���A�ȴA�ȴA���A���A�ȴA���A���A���A�ƨA�ȴA���A�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA�ȴA���A�ȴA�ȴA�ȴA�ƨA�ĜA���A��wA�ĜA�ĜA�A�A�A��wA�A�A�A���A���A�A���A��wA��jA��RA��FA��FA��-A��RA��^A��RA��RA��^A��jA��jA��^A��^A��^A��^A��^A��RA��FA��FA��9A��-A��!A���A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA���A���A���A���A���A���A���A���A��uA��uA��hA��hA��uA��uA��uA��hA��\A��PA��PA��\A��PA��PA��\A��\A��\A��PA��DA��DA��PA��PA��PA��DA��DA��7A��+A��+A��+A��7A��7A��+A��A��A��7A��DA��7A��7A��7A��7A��7A��+A��A��A��A��A�~�A�|�A�~�A�~�A��A��A�~�A�~�A�~�A�|�A�z�A�x�A�x�A�v�A�v�A�v�A�r�A�p�A�t�A�v�A�v�A�t�A�p�A�t�A�t�A�r�A�r�A�p�A�p�A�p�A�n�A�l�A�l�A�l�A�jA�jA�hsA�ffA�dZA�dZA�ffA�hsA�ffA�dZA�dZA�dZA�bNA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�^5A�^5A�\)A�\)A�\)A�ZA�XA�XA�S�A�XA�XA�S�A�S�A�S�A�S�A�Q�A�O�A�M�A�M�A�Q�A�Q�A�O�A�O�A�M�A�M�A�K�A�K�A�I�A�I�A�I�A�I�A�G�A�G�A�E�A�E�A�C�A�C�A�C�A�A�A�?}A�?}A�=qA�=qA�;dA�9XA�9XA�9XA�9XA�7LA�7LA�5?A�33A�1'A�1'A�/A�/A�-A�-A�+A�+A�(�A�(�A�(�A�&�A�$�A�$�A�"�A�"�A� �A� �A��A��A��A��A��A��A��A��A��A�{A�{A�oA�oA�oA�oA�bA�VA�JA�
=A�
=A�1A�%A�A�  A�A�  A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��yA��mA��mA��mA��`A��TA��TA��TA��TA��TA��`A��mA��A��TA�"�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220730070121                              AO  ARCAADJP                                                                    20220730070121    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220730070121  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20220730070121  QCF$                G�O�G�O�G�O�8800            
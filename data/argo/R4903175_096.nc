CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-06-05T07:00:36Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210605070036  20210605070036  4903175 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               `A   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @�z���#1   @�zDDM0@0�n��O��c��E��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         `A   A   F   @���@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�s311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=@�
=A�A?�A_�A}�A�A�A�A�A�A�A�A�B�HBz�B�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B�qB��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dpw�Dp��Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D�H�D�r=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AС�AС�AУ�AЧ�AЧ�AХ�AП�AС�AХ�AС�AЧ�AЧ�AЬAЬAЬAЩ�AУ�AП�AС�AУ�AЧ�AЕ�A�z�A�z�A�~�AЁA�r�A�XA�VA�S�A�S�A�Q�A�M�A�M�A�I�A�G�A�;dA�+A��A�%A��A��A��mA��/Aϩ�A�bA�t�AͮA�-A�|�A�ZA�A�|�A�O�Aũ�A�jA�C�AĶFA�ƨA�9XA�VA�ffA�ZA���A�jA�hsA�`BA��A��A���A��PA�p�A�"�A�  A��uA��A��A��;A���A�XA���A�VA�  A��A�33A���A��A��FA���A��A���A��hA�ĜA��RA��PA�=qA���A���A�ȴA�n�A��RA���A���A�C�A�oA��A�bNA�A�A���A��A���A}�mAy
=Av�As�Arz�Ao��Al��AhQ�Ad��Ab�!A_�A^�A[�mAYx�AW`BAU�ASC�AP(�AN5?AM?}AJ�/AIO�AG��AF��AES�AB�9A@~�A?A=�hA:��A9&�A7ƨA6�yA6�A4��A1�A/��A-�mA,��A+G�A*ZA*A)�A)p�A)XA(E�A'�PA&��A&JA%�A%�A%��A$��A$9XA"��A!��A��A�FA;dAZAZAVA�^AS�AM�A1'A��A33A��Ap�A�AbNAffA�wA��A
�A��AAO�A
=A��A�yAM�A  AAt�A��A ��A 1@��@�{@�x�@���@��y@�@�O�@�(�@���@��y@�`B@�1@��@�  @웦@�S�@�"�@��@�b@�R@��@�;d@ް!@���@�33@�=q@ݲ-@���@�  @�V@؃@�K�@�{@Լj@��@�n�@��@�O�@�I�@�ȴ@�@��H@���@͉7@�hs@�G�@��@��@ёh@թ�@�Ĝ@Ӯ@��@�@��y@���@�j@�ƨ@�ƨ@�|�@��@�(�@ț�@�
=@ǍP@Ə\@�?}@� �@öF@å�@�;d@\@��-@�bN@��
@�"�@�
=@��y@�@��@���@�z�@�I�@�|�@��y@���@���@�M�@��-@��h@��@�7L@�7L@���@�r�@�Z@�9X@���@�33@���@���@��!@��@���@�M�@�-@���@�V@��D@�Q�@�(�@�b@�1@�  @���@�"�@���@��!@��T@��@�?}@���@��/@��D@�1'@��@��m@��w@��@�dZ@�\)@�
=@�{@��@��`@��@���@��9@���@�A�@��@��@��w@�|�@��@�n�@���@���@�hs@�&�@��j@��j@��j@�z�@�1@�t�@�;d@�+@���@��!@�M�@��@���@��@�&�@���@��@�9X@��
@�C�@���@���@�V@��T@��@�7L@�V@��`@���@��j@��D@�I�@� �@��P@�"�@�o@�@��@��y@���@��\@�-@��-@�7L@��@��u@�Q�@��@�S�@���@�^5@�M�@�M�@�E�@��@��h@�&�@���@���@��u@�j@�1'@��@��@�"�@���@��@�@�`B@��j@�Z@� �@��;@�dZ@��@�ff@�5?@��@��h@�G�@�%@��/@��u@�9X@��@��
@��F@�dZ@�+@�ȴ@��+@�V@��@��@���@��@���@��/@��@�j@�1'@�1@���@��@�dZ@�
=@�ȴ@��R@���@�^5@�$�@��T@��-@��h@�X@���@��j@�z�@�(�@��m@��P@�+@���@���@�v�@�M�@�{@��#@��^@���@�p�@��@��`@��@�z�@�b@�l�@�dZ@�\)@�C�@���@���@�V@�5?@��@��-@���@�X@�/@��@�Ĝ@��u@�Z@�9X@�b@�  @�w@|�@
=@~�R@~v�@}�T@}@|9X@{33@z�\@z�\@zn�@zM�@yhs@x�9@w�@vff@v$�@vv�@v�+@v�+@vv�@vff@v5?@v{@u��@u�@u�@tz�@t9X@t�@s��@sS�@s@r�H@r^5@q�@p�@o��@o+@nv�@nE�@nE�@m�T@l��@lz�@lZ@lZ@k��@k�
@k��@k33@j�H@j~�@j-@j�@jJ@i�#@i�^@i�7@iX@h�`@h�@hA�@hb@g�;@g\)@g�@f�@f��@f�+@fV@f@e��@e��@ep�@d��@dZ@d9X@d1@c�
@cdZ@cS�@c33@c@b�\@b�@a��@ax�@`�`@`�u@`A�@_�w@_;d@^�y@^��@^V@]��@]�@\��@\�D@\Z@\(�@[�
@[��@[33@Z�H@Z=q@Y�^@Y%@X��@XĜ@X�9@X1'@Wl�@V�@Vff@U�T@U��@U�@U?}@T��@T�/@TZ@S�F@St�@SS�@R�@R�@Q��@Q��@Q��@QG�@P�u@PA�@O�;@N��@Nȴ@N5?@M�@L�@K��@K�F@K��@KS�@K"�@J~�@I�#@Ix�@I�@H��@H��@H1'@G�;@G�P@F��@F�R@F5?@E�@E�@EV@D��@D(�@C�m@Cƨ@Ct�@B�H@B��@B^5@A��@A��@Ahs@A&�@@��@@r�@@bN@@A�@@ �@?�w@?;d@>�@>$�@=?}@<z�@<(�@;�m@;S�@;o@:�@:��@:��@:n�@:M�@:-@:J@9�^@9�7@9hs@9�@8��@8 �@7l�@7K�@7K�@7\)@7�@6��@6@5`B@4�/@4�D@4z�@4j@4Z@4�@3�m@3�@3t�@3C�@3o@3@2�!@2~�@2-@1��@1G�@0��@0�u@0Q�@0b@/l�@/;d@.��@.E�@.{@-�T@-�-@-��@-`B@-�@,��@,�@,�D@,(�@+��@+�
@+ƨ@+�@+C�@+"�@+o@+@*�@*�H@*��@*^5@*M�@*�@)��@)�^@)�7@(��@(�9@(r�@( �@(  @'�;@'��@'�w@'��@'l�@';d@'�@&ȴ@&V@&$�@&{@%�T@%�h@%O�@%?}@%/@$�@$�@$z�@$z�@$9X@$(�@#��@#��@#C�@#"�@#@"��@"�\@"=q@!��@!�#@!��@!�^@!hs@!G�@!%@ Ĝ@ r�@ A�@   @�w@�P@K�@
=@��@�y@�R@ff@E�@5?@@�@�-@`B@V@��@�j@�D@z�@z�@Z@�m@t�@o@�!@~�@^5@-@J@��@��@x�@X@7L@�@�`@Ĝ@�9@�@1'@  @�;@�@��@|�@K�@��@��@v�@5?@@�T@��@`B@?}@�@�@��@��@z�@I�@I�@(�@1@�m@�
@�@"�@�H@��@�!@��@�\@~�@^5@�@�^@X@��@�`@��@��@Ĝ@r�@ �@  @��@��@|�@\)@\)@K�@+@��@ȴ@��@V@5?@@�@��@O�@�@V@��@�@�j@z�@9X@1@��@t�@S�@S�@S�@"�@
�@
�!@
n�@
M�@
=q@
-@
�@	�#@	��@	x�@	X@	&�@	%@��@�u@A�@b@�;@��@|�@l�@K�@�@�y@ȴ@ȴ@�+@E�@$�@$�@$�@@�@@��@�h@p�@?}@/@�@�@��@�D@I�@9X@1@��@��@t�@C�@33@o@�@��@~�@^5@M�@=q@�@�#@��@x�@X@&�@�@ ��@ ��@ ��@ �@ Q�@ b@   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AС�AС�AУ�AЧ�AЧ�AХ�AП�AС�AХ�AС�AЧ�AЧ�AЬAЬAЬAЩ�AУ�AП�AС�AУ�AЧ�AЕ�A�z�A�z�A�~�AЁA�r�A�XA�VA�S�A�S�A�Q�A�M�A�M�A�I�A�G�A�;dA�+A��A�%A��A��A��mA��/Aϩ�A�bA�t�AͮA�-A�|�A�ZA�A�|�A�O�Aũ�A�jA�C�AĶFA�ƨA�9XA�VA�ffA�ZA���A�jA�hsA�`BA��A��A���A��PA�p�A�"�A�  A��uA��A��A��;A���A�XA���A�VA�  A��A�33A���A��A��FA���A��A���A��hA�ĜA��RA��PA�=qA���A���A�ȴA�n�A��RA���A���A�C�A�oA��A�bNA�A�A���A��A���A}�mAy
=Av�As�Arz�Ao��Al��AhQ�Ad��Ab�!A_�A^�A[�mAYx�AW`BAU�ASC�AP(�AN5?AM?}AJ�/AIO�AG��AF��AES�AB�9A@~�A?A=�hA:��A9&�A7ƨA6�yA6�A4��A1�A/��A-�mA,��A+G�A*ZA*A)�A)p�A)XA(E�A'�PA&��A&JA%�A%�A%��A$��A$9XA"��A!��A��A�FA;dAZAZAVA�^AS�AM�A1'A��A33A��Ap�A�AbNAffA�wA��A
�A��AAO�A
=A��A�yAM�A  AAt�A��A ��A 1@��@�{@�x�@���@��y@�@�O�@�(�@���@��y@�`B@�1@��@�  @웦@�S�@�"�@��@�b@�R@��@�;d@ް!@���@�33@�=q@ݲ-@���@�  @�V@؃@�K�@�{@Լj@��@�n�@��@�O�@�I�@�ȴ@�@��H@���@͉7@�hs@�G�@��@��@ёh@թ�@�Ĝ@Ӯ@��@�@��y@���@�j@�ƨ@�ƨ@�|�@��@�(�@ț�@�
=@ǍP@Ə\@�?}@� �@öF@å�@�;d@\@��-@�bN@��
@�"�@�
=@��y@�@��@���@�z�@�I�@�|�@��y@���@���@�M�@��-@��h@��@�7L@�7L@���@�r�@�Z@�9X@���@�33@���@���@��!@��@���@�M�@�-@���@�V@��D@�Q�@�(�@�b@�1@�  @���@�"�@���@��!@��T@��@�?}@���@��/@��D@�1'@��@��m@��w@��@�dZ@�\)@�
=@�{@��@��`@��@���@��9@���@�A�@��@��@��w@�|�@��@�n�@���@���@�hs@�&�@��j@��j@��j@�z�@�1@�t�@�;d@�+@���@��!@�M�@��@���@��@�&�@���@��@�9X@��
@�C�@���@���@�V@��T@��@�7L@�V@��`@���@��j@��D@�I�@� �@��P@�"�@�o@�@��@��y@���@��\@�-@��-@�7L@��@��u@�Q�@��@�S�@���@�^5@�M�@�M�@�E�@��@��h@�&�@���@���@��u@�j@�1'@��@��@�"�@���@��@�@�`B@��j@�Z@� �@��;@�dZ@��@�ff@�5?@��@��h@�G�@�%@��/@��u@�9X@��@��
@��F@�dZ@�+@�ȴ@��+@�V@��@��@���@��@���@��/@��@�j@�1'@�1@���@��@�dZ@�
=@�ȴ@��R@���@�^5@�$�@��T@��-@��h@�X@���@��j@�z�@�(�@��m@��P@�+@���@���@�v�@�M�@�{@��#@��^@���@�p�@��@��`@��@�z�@�b@�l�@�dZ@�\)@�C�@���@���@�V@�5?@��@��-@���@�X@�/@��@�Ĝ@��u@�Z@�9X@�b@�  @�w@|�@
=@~�R@~v�@}�T@}@|9X@{33@z�\@z�\@zn�@zM�@yhs@x�9@w�@vff@v$�@vv�@v�+@v�+@vv�@vff@v5?@v{@u��@u�@u�@tz�@t9X@t�@s��@sS�@s@r�H@r^5@q�@p�@o��@o+@nv�@nE�@nE�@m�T@l��@lz�@lZ@lZ@k��@k�
@k��@k33@j�H@j~�@j-@j�@jJ@i�#@i�^@i�7@iX@h�`@h�@hA�@hb@g�;@g\)@g�@f�@f��@f�+@fV@f@e��@e��@ep�@d��@dZ@d9X@d1@c�
@cdZ@cS�@c33@c@b�\@b�@a��@ax�@`�`@`�u@`A�@_�w@_;d@^�y@^��@^V@]��@]�@\��@\�D@\Z@\(�@[�
@[��@[33@Z�H@Z=q@Y�^@Y%@X��@XĜ@X�9@X1'@Wl�@V�@Vff@U�T@U��@U�@U?}@T��@T�/@TZ@S�F@St�@SS�@R�@R�@Q��@Q��@Q��@QG�@P�u@PA�@O�;@N��@Nȴ@N5?@M�@L�@K��@K�F@K��@KS�@K"�@J~�@I�#@Ix�@I�@H��@H��@H1'@G�;@G�P@F��@F�R@F5?@E�@E�@EV@D��@D(�@C�m@Cƨ@Ct�@B�H@B��@B^5@A��@A��@Ahs@A&�@@��@@r�@@bN@@A�@@ �@?�w@?;d@>�@>$�@=?}@<z�@<(�@;�m@;S�@;o@:�@:��@:��@:n�@:M�@:-@:J@9�^@9�7@9hs@9�@8��@8 �@7l�@7K�@7K�@7\)@7�@6��@6@5`B@4�/@4�D@4z�@4j@4Z@4�@3�m@3�@3t�@3C�@3o@3@2�!@2~�@2-@1��@1G�@0��@0�u@0Q�@0b@/l�@/;d@.��@.E�@.{@-�T@-�-@-��@-`B@-�@,��@,�@,�D@,(�@+��@+�
@+ƨ@+�@+C�@+"�@+o@+@*�@*�H@*��@*^5@*M�@*�@)��@)�^@)�7@(��@(�9@(r�@( �@(  @'�;@'��@'�w@'��@'l�@';d@'�@&ȴ@&V@&$�@&{@%�T@%�h@%O�@%?}@%/@$�@$�@$z�@$z�@$9X@$(�@#��@#��@#C�@#"�@#@"��@"�\@"=q@!��@!�#@!��@!�^@!hs@!G�@!%@ Ĝ@ r�@ A�@   @�w@�P@K�@
=@��@�y@�R@ff@E�@5?@@�@�-@`B@V@��@�j@�D@z�@z�@Z@�m@t�@o@�!@~�@^5@-@J@��@��@x�@X@7L@�@�`@Ĝ@�9@�@1'@  @�;@�@��@|�@K�@��@��@v�@5?@@�T@��@`B@?}@�@�@��@��@z�@I�@I�@(�@1@�m@�
@�@"�@�H@��@�!@��@�\@~�@^5@�@�^@X@��@�`@��@��@Ĝ@r�@ �@  @��@��@|�@\)@\)@K�@+@��@ȴ@��@V@5?@@�@��@O�@�@V@��@�@�j@z�@9X@1@��@t�@S�@S�@S�@"�@
�@
�!@
n�@
M�@
=q@
-@
�@	�#@	��@	x�@	X@	&�@	%@��@�u@A�@b@�;@��@|�@l�@K�@�@�y@ȴ@ȴ@�+@E�@$�@$�@$�@@�@@��@�h@p�@?}@/@�@�@��@�D@I�@9X@1@��@��@t�@C�@33@o@�@��@~�@^5@M�@=q@�@�#@��@x�@X@&�@�@ ��@ ��@ ��@ �@ Q�@ b@   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�1'A�1'A�1'A�/A�/A�/A�/A�/A�1'A�-A�/A�/A�/A�/A�/A�/A�-A�/A�/A�-A�/A�/A�/A�/A�/A�-A�/A�-A�-A�/A�/A�/A�/A�/A�/A�/A�/A�5?A�9XA�?}A�?}A�?}A�=qA�7LA�-A�"�A�{A���A�A��A�7LA�VA�A�1A�A�A�?}A�VA�FA�wA�^A��A��A�E�A�dZA�\)A�\)A�z�A�|�A�x�A�t�A�A�hA�DA�A��A�PA�r�A�z�A�+A�-A�^A�A�ƨA���A���A�ƨA�RA�+A�v�A�`BA�7LA�"�A�oA�oA�1A��A���A�A�A�z�A�z�A�v�A�n�A�bNA�I�A�;dA� �A���A�^A�A�^5A�&�A�
=A��A��TA���A�9A�\A�hsA�Q�A�1'A��A�
=A���A��TA���A���A��AAAA�uA�PA�A�A�A�v�A�l�A�dZA�bNA�`BA�`BA�bNA�ZA�S�A�G�A�=qA�/A�-A�(�A� �A�"�A�5?A�M�A�l�A�v�A�t�A�p�A�hsA�dZA�z�A��AAA�!AA�DA�A�l�A�^5A�E�A�1'A�-A�1'A�;dA�A�A�I�A�C�A�?}A�;dA�9XA�9XA�G�A�?}A�E�A�=qA�1'A��A�
=A�1A�oA�$�A�-A�(�A�$�A�"�A�"�A��A��A��A��A��A��A�/A�7LA�-A�"�A�7LA�5?A�-A�&�A� �A��A��A��A� �A�"�A� �A��A�oA�JA�{A�(�A�-A�1'A�33A�1'A�5?A�;dA�A�A�E�A�I�A�I�A�M�A�O�A�O�A�Q�A�XA�\)A�hsA�n�A�v�A�x�A�x�A�z�A�AA�ƨA���A���A��A��A��A��A��A��mA���A�%A�A�  A���A�A�A�oA��A��A��A��A��A�{A�oA�oA�bA�oA�{A�{A��A��A�"�A�(�A�+A�+A�+A�1'A�7LA�;dA�?}A�?}A�?}A�C�A�I�A�M�A�\)A�bNA�bNA�hsA�n�A�v�A�z�A�|�A�A�DA�PA�\A�uA�uAAAAAAAAAAA��A��A��A�A�!A�!A�FA�RA�^A�^A�^A�jA�jA�wA�jA�wA�A�ĜA�ƨA���A�ȴA�ȴA���A���A���A���A���A���A���A��#A��#A��#A��/A��;A��;A��/A��;A��TA��TA��TA��TA��TA��`A��yA��yA��yA��yA��A��A��A��A��A��A��A���A���A���A���A���A�  A�  A�  A�  A�  A�A�A�%A�1A�1A�1A�1A�1A�1A�
=A�JA�JA�VA�JA�bA�VA�oA�oA�{A��A��A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A� �A�"�A�"�A�"�A�"�A�"�A�"�A�$�A�$�A�&�A�$�A�$�A�&�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�(�A�+A�(�A�+A�-A�-A�-A�-A�/A�/A�/A�/A�1'A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�;dA�=qA�=qA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�=qA�;dA�;dA�=qA�?}A�=qA�=qA�;dA�=qA�=qA�A�A�E�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�bNA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�dZA�dZA�ffA�ffA�dZA�dZA�dZA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�hsA�jA�jA�jA�jA�jA�jA�l�A�l�A�l�A�n�A�n�A�n�A�n�A�p�A�r�A�r�A�p�A�r�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�v�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�|�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��7A��7A��7A��7A��7A��7A��7A��7A��7A��7A��7A��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��PA��PA��PA��PA��PA��PA��PA��PA��\A��\A��PA��PA��\A��\A��\A��\A��\A�hA�hA�hA�hA�hA�hA�hA�uA�uA�uA�uA�uA�uA�uA�uA�uA�uA�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   A�/A�1'A�1'A�1'A�/A�/A�/A�/A�/A�1'A�-A�/A�/A�/A�/A�/A�/A�-A�/A�/A�-A�/A�/A�/A�/A�/A�-A�/A�-A�-A�/A�/A�/A�/A�/A�/A�/A�/A�5?A�9XA�?}A�?}A�?}A�=qA�7LA�-A�"�A�{A���A�A��A�7LA�VA�A�1A�A�A�?}A�VA�FA�wA�^A��A��A�E�A�dZA�\)A�\)A�z�A�|�A�x�A�t�A�A�hA�DA�A��A�PA�r�A�z�A�+A�-A�^A�A�ƨA���A���A�ƨA�RA�+A�v�A�`BA�7LA�"�A�oA�oA�1A��A���A�A�A�z�A�z�A�v�A�n�A�bNA�I�A�;dA� �A���A�^A�A�^5A�&�A�
=A��A��TA���A�9A�\A�hsA�Q�A�1'A��A�
=A���A��TA���A���A��AAAA�uA�PA�A�A�A�v�A�l�A�dZA�bNA�`BA�`BA�bNA�ZA�S�A�G�A�=qA�/A�-A�(�A� �A�"�A�5?A�M�A�l�A�v�A�t�A�p�A�hsA�dZA�z�A��AAA�!AA�DA�A�l�A�^5A�E�A�1'A�-A�1'A�;dA�A�A�I�A�C�A�?}A�;dA�9XA�9XA�G�A�?}A�E�A�=qA�1'A��A�
=A�1A�oA�$�A�-A�(�A�$�A�"�A�"�A��A��A��A��A��A��A�/A�7LA�-A�"�A�7LA�5?A�-A�&�A� �A��A��A��A� �A�"�A� �A��A�oA�JA�{A�(�A�-A�1'A�33A�1'A�5?A�;dA�A�A�E�A�I�A�I�A�M�A�O�A�O�A�Q�A�XA�\)A�hsA�n�A�v�A�x�A�x�A�z�A�AA�ƨA���A���A��A��A��A��A��A��mA���A�%A�A�  A���A�A�A�oA��A��A��A��A��A�{A�oA�oA�bA�oA�{A�{A��A��A�"�A�(�A�+A�+A�+A�1'A�7LA�;dA�?}A�?}A�?}A�C�A�I�A�M�A�\)A�bNA�bNA�hsA�n�A�v�A�z�A�|�A�A�DA�PA�\A�uA�uAAAAAAAAAAA��A��A��A�A�!A�!A�FA�RA�^A�^A�^A�jA�jA�wA�jA�wA�A�ĜA�ƨA���A�ȴA�ȴA���A���A���A���A���A���A���A��#A��#A��#A��/A��;A��;A��/A��;A��TA��TA��TA��TA��TA��`A��yA��yA��yA��yA��A��A��A��A��A��A��A���A���A���A���A���A�  A�  A�  A�  A�  A�A�A�%A�1A�1A�1A�1A�1A�1A�
=A�JA�JA�VA�JA�bA�VA�oA�oA�{A��A��A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A� �A� �A�"�A�"�A�"�A�"�A�"�A�"�A�$�A�$�A�&�A�$�A�$�A�&�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�(�A�+A�(�A�+A�-A�-A�-A�-A�/A�/A�/A�/A�1'A�1'A�1'A�33A�33A�33A�33A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�9XA�9XA�7LA�9XA�9XA�9XA�9XA�;dA�=qA�=qA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�=qA�;dA�;dA�=qA�?}A�=qA�=qA�;dA�=qA�=qA�A�A�E�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�bNA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�bNA�dZA�dZA�ffA�ffA�dZA�dZA�dZA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�hsA�jA�jA�jA�jA�jA�jA�l�A�l�A�l�A�n�A�n�A�n�A�n�A�p�A�r�A�r�A�p�A�r�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�x�A�v�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�|�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��+A��7A��7A��7A��7A��7A��7A��7A��7A��7A��7A��7A��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��DA��PA��PA��PA��PA��PA��PA��PA��PA��\A��\A��PA��PA��\A��\A��\A��\A��\A�hA�hA�hA�hA�hA�hA�hA�uA�uA�uA�uA�uA�uA�uA�uA�uA�uA�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210605070036                              AO  ARCAADJP                                                                    20210605070036    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210605070036  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210605070036  QCF$                G�O�G�O�G�O�8000            
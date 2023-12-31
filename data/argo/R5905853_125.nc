CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-15T03:44:02Z creation;2022-07-15T03:44:02Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220715034402  20220715040208  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               }A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��lwwww1   @��l�5��@-$�/�c��C��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��AC33Ac33A~ffA�  A�33A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�ffB�  Bә�B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC�fC  C
  C  C�C  C�fC  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C633C8  C:�C<  C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\ffC]��C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@~{@�
=@�
=A!�AB�RAb�RA}�A�A���A�A���A���A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBpG�Bw�HB�HB��B��B��B��B��B�W
B�W
B��qB��qB��B��qB��B��B��B��B��B��B��B�W
B��Bӊ>B��B۽qB��B��B��B��B��B��B��B��B��C�RC޸C޸C�RC	�RC�RC�C�RC޸C�RC�RC�RC�RC�RC�RC�RC!�RC$�C%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC6+�C7�RC:�C;�RC=޸C?޸CA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC\^�C]�C_�RCa޸Cc޸Ce�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�Dw�D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D�zD�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'w�D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D-zD-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~DozDo~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D�{�D��
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
D��=D��
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
D�{�D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D�=D�?
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
D݂=Dݿ
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
D�=D�
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
D�?
D�
D��
D��
D�?
D�h�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�*�A�(XA�$�A�33A�;dA�-�A�+�A�:^A�9XA�.�A�33A�4�A�>wA�A A�JXA�K�A�L0A�L�A�LdA�M�A�OA�P�A�M�A�LdA�N<A�OBA�P}A�M�A�N<A�M�A�K�A�K�A�K^A�JXA�GEA�G�A�H�A�IA�@�A�xA�~A��A���A��TA�6A�s�A��A�XA��A�=<A�
�A�9XA�x�A�� A�u%A�4A���A�ʌA�YA��fA���A�`A��EA���A�`A���A�*eA�u�A��A��A�'RA��5A�-A�VA�j�A���A�Q�A�h>A��$A��A�QNA���A��A�	�A�o A��FA��A��A���A���A�A��A�YA���Aw,�As!-AqPHAl��Ag͟Ac6zAao A`(�A^��AZ�0AW��ATDgAP[�AL2aAG��AC�nA?S&A;��A;9�A:1�A7��A6�jA5�UA4�A4��A3P�A1��A0��A/�DA/�1A/e,A/.IA/5�A/[WA/7LA.��A,�AA+h
A*�A)�A(��A(�A'$tA&�nA&�A&�ZA%�,A%�mA&K�A%��A$.IA#�A"�8A"��A"MjA"�mA"cA!��A"ffA"��A"�*A �~A��AjA��A�A�A��A�}A�{A��A	A� A#:A��A~�A�AQA�A�4A�AS&A�A�4A<�A��A��A�A�9A��AoiAMA�!A9XAݘA�hAXAAqvAW�A�zAA�{AaA33A�A�A�/A��A�PA!-A
��A
�4A
�A	�*A	qvA		lA��A�KA`�A��AC�A�jAl�A�bAE9A�AϫA�A�MA ݘA e�@��#@�8�@��5@��@�{@��	@�Z�@�j@�J@��b@���@��@�J@�^�@��@��@��@��@��D@�'R@�ϫ@��@�@�@��@��[@�hs@��K@�!@�O�@�֡@�3�@�@@�@@迱@���@�_@�1�@�$@�@�y�@���@�4�@�U2@�4@��W@���@�U�@���@��@�r�@��@��a@�q@��@�ϫ@ݬq@ݓ�@݊	@�w2@�+@�z@���@��M@�7@ٍP@��@ر�@�.�@��@׮@��@��@�1�@֪e@�@�@՟V@�<6@ԣ@�M@���@�t�@��@Ҭ�@�kQ@��@��Q@Ѱ�@�)_@н<@�l�@�7@ό~@�@΢4@�$�@�s�@�C@�ѷ@�\�@˖S@�O@��@���@ʞ�@�xl@�&�@��>@�"�@�M�@�%F@Ƽj@Ƨ�@�ff@���@��@�e@�b�@¡b@�{@�>�@�n�@��Z@��k@�%@��F@��D@���@�0U@�@�ݘ@�e,@���@���@��@��?@�d�@�ݘ@��n@�L�@��@���@��j@�j@��@���@�v�@�'R@�hs@�+@��@�ȴ@�~@�p�@��y@�  @�c@�/�@��@���@���@��@�D�@���@�j@��@���@�8�@�1@�ݘ@��@�U�@�q@���@��h@���@�h
@�C-@��r@�a@�_�@��9@�u�@�J�@���@���@��@�
�@��d@��h@��@���@��4@���@�R�@�>B@��@��Q@���@�O@�8@� \@��@��j@�r�@��@��@��?@��x@�]d@�+k@��#@���@���@�|@�t�@�B�@��A@�u�@�Q@���@�e�@�)_@��@��.@�C�@�
�@��@�k�@�=�@�!�@��	@�h�@��@��~@�.I@���@�R�@�@�|@��m@�Xy@�G@��d@��"@��@�v�@��.@���@�A�@���@��@�($@���@���@�S&@�(@��@�r�@�A�@��@���@��q@�t�@�,�@�ѷ@���@�B[@�@���@���@���@���@�J�@��@���@���@���@��r@�i�@�V@�(�@�1@��r@��:@�.I@��@���@���@���@�tT@�;�@��g@�u�@���@�[�@�	@��@�@��=@�|@�e�@�J�@��@��H@���@��@�� @��t@��k@��@�n/@�F@��]@��@�~�@�D�@�-�@���@��@���@�[W@�6z@��@���@�l"@�.�@��#@�v`@�e,@�.I@��@��@��B@���@��@���@�`B@��@�͟@�xl@�a@~�8@~=q@}��@}<6@|��@|��@|H@{y�@z��@zOv@z �@x��@x��@x4n@x �@w��@wl�@v� @v;�@u�C@u:�@uV@tA�@s��@r�'@q��@qu�@p_@oF�@n�@nV@n$�@m�@m�@m�@m�@mT�@lN�@k�g@k�@k@O@j�M@j��@j�@i��@i|@i�@h�j@h~(@hM@g�:@g i@f��@e�@e:�@e%@d�@d��@d>B@db@c��@c�4@c=@c�@c�@b�@b�'@b{�@a��@a�#@a��@b�@b�@b�@a�z@a#�@`�v@`�j@`1'@_�q@_Z�@_O@_,�@^�@^�r@^@]�@]�~@]e,@\�P@]�@\u�@[�A@[��@[C�@[ i@Z�y@[
=@Z��@Z5?@Z($@Y�D@Y�N@Ya�@X��@X��@X�@W�0@W�4@WW?@W>�@W&@W�@Vȴ@V�A@Va|@U��@U�@T��@T�@TS�@TD�@T%�@R�H@RJ�@R_@Q�9@Q��@Qk�@Q�@P��@O˒@O@Ni�@M�o@M�~@MQ�@M!�@L�e@L~@K��@K4�@J�@J�\@J#:@I��@H�|@H�Y@H2�@G�
@G��@G��@GMj@G(@F��@F��@FW�@E��@E�@D�@D�e@DXy@D9X@DM@C�K@CP�@CC@B�s@A��@A��@A*0@@��@@PH@@  @?��@?��@?�@?_p@?;d@?o@>��@>�,@>��@>z@>@=k�@=�@<��@<�@<m�@<�@;�q@;n/@; i@:�+@:+k@9��@9Dg@8�_@8"h@7qv@7J#@733@6�y@6��@6c @6M�@6($@6�@5��@5�@5�M@4�|@4�?@4�z@4y>@4,=@41@3�+@3�@3ݘ@3��@3�0@3�k@3S�@3>�@36z@3'�@2�@2� @2($@1�T@1��@17L@1q@1�@0�@0��@0�U@0�@0�o@0K^@0�@/�W@/��@/�w@/qv@/W?@/J#@/C�@/(@.��@.Z�@.4@-��@-��@-8�@,��@,�O@,��@,S�@,�@+��@+�@@+��@+|�@+S�@+Y@*�@*��@*��@*_�@*�@)�o@)��@)\�@)0�@(�/@(�u@(Xy@(A�@(2�@(1@'��@'g�@'.I@&��@&	@%�@%��@%f�@% \@$Ɇ@$4n@#��@#�P@#A�@#9�@#)_@#�@"��@"xl@"6�@"�@!�o@!��@!e,@!N<@!#�@ ��@ ��@ y>@ x@��@�:@&@�@��@YK@C�@{@�@��@^�@7L@��@�@�@�@F�@�@�@�s@{�@6�@�@�N@�'@�"@Vm@�@�@�D@��@��@|�@h�@D�@�@ݘ@��@�$@]�@�@��@�h@�@GE@�#@rG@S&@:�@V@��@�@�@�@�A@ݘ@� @��@�w@�[@��@s@>�@�@�@�R@l�@J�@.�@��@�z@��@m]@IR@&�@�@��@�4@V�@�r@�
@��@�{@RT@�@�@��@��@\�@M�@=q@($@e@ �@��@q@�E@�E@��@�I@r�@c�@V�@1'@  @خ@�q@X�@>�@Y@
��@
��@
��@
xl@
=q@	��@	�z@	��@	�7@	u�@	m]@	5�@	�@�P@ѷ@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�*�A�(XA�$�A�33A�;dA�-�A�+�A�:^A�9XA�.�A�33A�4�A�>wA�A A�JXA�K�A�L0A�L�A�LdA�M�A�OA�P�A�M�A�LdA�N<A�OBA�P}A�M�A�N<A�M�A�K�A�K�A�K^A�JXA�GEA�G�A�H�A�IA�@�A�xA�~A��A���A��TA�6A�s�A��A�XA��A�=<A�
�A�9XA�x�A�� A�u%A�4A���A�ʌA�YA��fA���A�`A��EA���A�`A���A�*eA�u�A��A��A�'RA��5A�-A�VA�j�A���A�Q�A�h>A��$A��A�QNA���A��A�	�A�o A��FA��A��A���A���A�A��A�YA���Aw,�As!-AqPHAl��Ag͟Ac6zAao A`(�A^��AZ�0AW��ATDgAP[�AL2aAG��AC�nA?S&A;��A;9�A:1�A7��A6�jA5�UA4�A4��A3P�A1��A0��A/�DA/�1A/e,A/.IA/5�A/[WA/7LA.��A,�AA+h
A*�A)�A(��A(�A'$tA&�nA&�A&�ZA%�,A%�mA&K�A%��A$.IA#�A"�8A"��A"MjA"�mA"cA!��A"ffA"��A"�*A �~A��AjA��A�A�A��A�}A�{A��A	A� A#:A��A~�A�AQA�A�4A�AS&A�A�4A<�A��A��A�A�9A��AoiAMA�!A9XAݘA�hAXAAqvAW�A�zAA�{AaA33A�A�A�/A��A�PA!-A
��A
�4A
�A	�*A	qvA		lA��A�KA`�A��AC�A�jAl�A�bAE9A�AϫA�A�MA ݘA e�@��#@�8�@��5@��@�{@��	@�Z�@�j@�J@��b@���@��@�J@�^�@��@��@��@��@��D@�'R@�ϫ@��@�@�@��@��[@�hs@��K@�!@�O�@�֡@�3�@�@@�@@迱@���@�_@�1�@�$@�@�y�@���@�4�@�U2@�4@��W@���@�U�@���@��@�r�@��@��a@�q@��@�ϫ@ݬq@ݓ�@݊	@�w2@�+@�z@���@��M@�7@ٍP@��@ر�@�.�@��@׮@��@��@�1�@֪e@�@�@՟V@�<6@ԣ@�M@���@�t�@��@Ҭ�@�kQ@��@��Q@Ѱ�@�)_@н<@�l�@�7@ό~@�@΢4@�$�@�s�@�C@�ѷ@�\�@˖S@�O@��@���@ʞ�@�xl@�&�@��>@�"�@�M�@�%F@Ƽj@Ƨ�@�ff@���@��@�e@�b�@¡b@�{@�>�@�n�@��Z@��k@�%@��F@��D@���@�0U@�@�ݘ@�e,@���@���@��@��?@�d�@�ݘ@��n@�L�@��@���@��j@�j@��@���@�v�@�'R@�hs@�+@��@�ȴ@�~@�p�@��y@�  @�c@�/�@��@���@���@��@�D�@���@�j@��@���@�8�@�1@�ݘ@��@�U�@�q@���@��h@���@�h
@�C-@��r@�a@�_�@��9@�u�@�J�@���@���@��@�
�@��d@��h@��@���@��4@���@�R�@�>B@��@��Q@���@�O@�8@� \@��@��j@�r�@��@��@��?@��x@�]d@�+k@��#@���@���@�|@�t�@�B�@��A@�u�@�Q@���@�e�@�)_@��@��.@�C�@�
�@��@�k�@�=�@�!�@��	@�h�@��@��~@�.I@���@�R�@�@�|@��m@�Xy@�G@��d@��"@��@�v�@��.@���@�A�@���@��@�($@���@���@�S&@�(@��@�r�@�A�@��@���@��q@�t�@�,�@�ѷ@���@�B[@�@���@���@���@���@�J�@��@���@���@���@��r@�i�@�V@�(�@�1@��r@��:@�.I@��@���@���@���@�tT@�;�@��g@�u�@���@�[�@�	@��@�@��=@�|@�e�@�J�@��@��H@���@��@�� @��t@��k@��@�n/@�F@��]@��@�~�@�D�@�-�@���@��@���@�[W@�6z@��@���@�l"@�.�@��#@�v`@�e,@�.I@��@��@��B@���@��@���@�`B@��@�͟@�xl@�a@~�8@~=q@}��@}<6@|��@|��@|H@{y�@z��@zOv@z �@x��@x��@x4n@x �@w��@wl�@v� @v;�@u�C@u:�@uV@tA�@s��@r�'@q��@qu�@p_@oF�@n�@nV@n$�@m�@m�@m�@m�@mT�@lN�@k�g@k�@k@O@j�M@j��@j�@i��@i|@i�@h�j@h~(@hM@g�:@g i@f��@e�@e:�@e%@d�@d��@d>B@db@c��@c�4@c=@c�@c�@b�@b�'@b{�@a��@a�#@a��@b�@b�@b�@a�z@a#�@`�v@`�j@`1'@_�q@_Z�@_O@_,�@^�@^�r@^@]�@]�~@]e,@\�P@]�@\u�@[�A@[��@[C�@[ i@Z�y@[
=@Z��@Z5?@Z($@Y�D@Y�N@Ya�@X��@X��@X�@W�0@W�4@WW?@W>�@W&@W�@Vȴ@V�A@Va|@U��@U�@T��@T�@TS�@TD�@T%�@R�H@RJ�@R_@Q�9@Q��@Qk�@Q�@P��@O˒@O@Ni�@M�o@M�~@MQ�@M!�@L�e@L~@K��@K4�@J�@J�\@J#:@I��@H�|@H�Y@H2�@G�
@G��@G��@GMj@G(@F��@F��@FW�@E��@E�@D�@D�e@DXy@D9X@DM@C�K@CP�@CC@B�s@A��@A��@A*0@@��@@PH@@  @?��@?��@?�@?_p@?;d@?o@>��@>�,@>��@>z@>@=k�@=�@<��@<�@<m�@<�@;�q@;n/@; i@:�+@:+k@9��@9Dg@8�_@8"h@7qv@7J#@733@6�y@6��@6c @6M�@6($@6�@5��@5�@5�M@4�|@4�?@4�z@4y>@4,=@41@3�+@3�@3ݘ@3��@3�0@3�k@3S�@3>�@36z@3'�@2�@2� @2($@1�T@1��@17L@1q@1�@0�@0��@0�U@0�@0�o@0K^@0�@/�W@/��@/�w@/qv@/W?@/J#@/C�@/(@.��@.Z�@.4@-��@-��@-8�@,��@,�O@,��@,S�@,�@+��@+�@@+��@+|�@+S�@+Y@*�@*��@*��@*_�@*�@)�o@)��@)\�@)0�@(�/@(�u@(Xy@(A�@(2�@(1@'��@'g�@'.I@&��@&	@%�@%��@%f�@% \@$Ɇ@$4n@#��@#�P@#A�@#9�@#)_@#�@"��@"xl@"6�@"�@!�o@!��@!e,@!N<@!#�@ ��@ ��@ y>@ x@��@�:@&@�@��@YK@C�@{@�@��@^�@7L@��@�@�@�@F�@�@�@�s@{�@6�@�@�N@�'@�"@Vm@�@�@�D@��@��@|�@h�@D�@�@ݘ@��@�$@]�@�@��@�h@�@GE@�#@rG@S&@:�@V@��@�@�@�@�A@ݘ@� @��@�w@�[@��@s@>�@�@�@�R@l�@J�@.�@��@�z@��@m]@IR@&�@�@��@�4@V�@�r@�
@��@�{@RT@�@�@��@��@\�@M�@=q@($@e@ �@��@q@�E@�E@��@�I@r�@c�@V�@1'@  @خ@�q@X�@>�@Y@
��@
��@
��@
xl@
=q@	��@	�z@	��@	�7@	u�@	m]@	5�@	�@�P@ѷ@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B'�B(
B($B'�B'�B(
B($B'�B'�B(
B'�B(>B'�B'�B'�B'�B'�B'�B(
B'�B(
B(
B(>B(>B($B($B($B(�B(�B)DB)�B*0B*KB*�B*�B+B+B+B+�B-B3�B��B��B�B{B.�BEmBv�B�OB��B��B�jB�RB�*B�jB��B�2BҽB�.B��B�+B��B��BmBR�BC{B0�B"BB�B1BAB��B�B�B�B�B�	B��B��B��B��B��B|�BV�B(�B
�ZB
��B
�vB
{�B
\�B
>BB
&�B
�B	�VB	��B	�:B	��B	pB	T�B	JXB	CGB	8�B	'RB	SB		B��B�cB��B�B��B� B		7B	0B	�B	6B	B	$tB	1AB	($B	�B	�B	
�B	TB	mB	B	�B	"�B	2�B	=<B	;0B	/�B	-�B	,B	/�B	6�B	>�B	HfB	]~B	qAB	r�B	z�B	�dB	��B	��B	��B	��B	�,B	�B	�yB	��B	��B	�~B	��B	�qB	�EB	�B	�SB	�B	��B	��B	�	B	ںB	�#B	�;B	�|B	�4B	��B	��B	��B	�B	��B	�B	��B	�]B	�wB	��B	�CB	�}B	�IB	�/B	�/B	�B	�/B	��B	��B	�B	��B	�IB	��B	��B	�B	�UB	��B	�ZB	�B	��B	��B	�B	��B	��B	�zB	��B	��B	��B	��B	�FB	��B	��B	�|B	�OB	�B	�:B	�B	��B	�\B	�jB	ޞB	��B	�kB	ٚB	�7B	�B	��B	�@B	��B	ԯB	׍B	ܬB	߾B	�B	��B	�,B	��B	��B	��B	�8B	�zB	�B	�tB	�B	� B	�NB	�-B	�vB	�B	��B	׍B	�MB	ԯB	�,B	өB	��B	�:B	�hB	�4B	�hB	ѝB	ҽB	�uB	ڠB	�jB	ܬB	��B	�B	�-B	�/B	�/B	��B	��B	��B	�/B	�;B	��B	�HB	�4B	�B	�HB	�4B	�TB	�B	��B	��B	�B	�B	� B	�:B	�B	��B	�:B	��B	�B	��B	�B	��B	��B	�B	�)B	��B	��B	��B	�B	�B	��B	��B	�	B	�	B	�$B	��B	�>B	�XB	�>B	�>B	�rB	��B	��B	�rB	��B	�XB	�rB	�$B	��B	��B	��B	��B	��B	�LB	��B	��B	�LB	�LB	��B	�`B	�FB	��B	�fB	��B	��B	�B	��B	�B	��B	��B	��B	�fB	��B	�RB	��B	�lB	�lB	��B	�lB	�RB	��B	��B	�rB	�rB	��B	��B	��B	��B	�0B	�dB	�dB	��B	��B	�B	�6B	��B	�"B	�"B	�<B	�<B	�wB	�(B	��B	��B	�}B	�}B
 �B
 iB
 B
 �B
B
B
;B
�B
oB
UB
�B
�B
B
�B
�B
�B
�B
-B
aB
�B
�B
�B
�B
3B
MB
�B
9B
�B
B
B
B
+B
�B
KB
fB
�B
�B
	B
�B
	RB
	�B
	�B
	�B
	lB
	�B

�B

�B
)B
�B
DB

�B

�B
�B
�B
dB
�B
PB
�B
6B
PB
6B
6B
<B
�B
VB
VB
B
}B
bB
HB
HB
}B
�B
B
hB
�B
�B
�B
�B
�B
&B
@B
�B
,B
{B
�B
mB
SB
�B
�B
�B
�B
EB
�B
�B
�B
	B
�B
]B
�B
�B
/B
�B
5B
5B
OB
B
5B
B
;B
pB
VB
pB
 vB
 �B
 �B
 �B
 �B
!B
!bB
"B
"�B
"�B
"�B
"�B
"�B
"�B
# B
# B
"�B
#TB
#�B
$&B
$ZB
$ZB
$@B
$ZB
$�B
$�B
%,B
&LB
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(
B
'�B
(�B
)B
)_B
)B
(�B
)B
(�B
)B
)yB
)�B
*eB
*B
*�B
*B
+B
,=B
,�B
,WB
,"B
,qB
-]B
-]B
-�B
-�B
-�B
-�B
.cB
.�B
.�B
.�B
/B
/iB
/�B
/iB
/OB
0�B
1�B
1�B
2|B
3�B
3�B
4B
4nB
4�B
5B
5�B
5�B
5�B
6�B
7LB
7�B
7�B
7�B
8B
8�B
9�B
9�B
9�B
:B
:B
:B
9�B
:�B
:�B
:�B
;JB
:�B
:�B
:�B
:�B
;B
;dB
<6B
<PB
<PB
=<B
=VB
=VB
=qB
=qB
=qB
<�B
<�B
<�B
<�B
<�B
=VB
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>wB
>�B
>�B
?B
?�B
@OB
@�B
@iB
@�B
A�B
C�B
D�B
D�B
D�B
EB
D�B
EB
D�B
EB
E�B
E�B
F?B
F�B
F�B
G+B
GEB
GB
GzB
G�B
H�B
I7B
IB
H�B
G�B
G�B
HfB
IB
I�B
I�B
J�B
JXB
KDB
K�B
L0B
LJB
LdB
L~B
L�B
MB
MB
MB
MPB
MjB
M�B
MjB
M�B
N�B
N�B
N�B
N�B
N�B
N�B
P.B
PHB
P}B
PbB
P�B
P�B
P�B
P�B
QhB
Q�B
RB
RTB
R�B
R�B
R�B
S&B
S@B
S�B
S�B
TB
T,B
T�B
T�B
UgB
UMB
U�B
VB
VmB
V�B
V�B
W$B
WYB
W�B
W�B
XyB
X�B
YB
YKB
YB
YB
Y�B
Y�B
Z7B
ZQB
ZkB
[=B
[WB
[�B
[�B
\B
\]B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]/B
]IB
^B
^B
^jB
^�B
^�B
^�B
_B
_;B
_VB
_�B
_�B
_�B
_�B
^�B
_!B
_pB
_�B
_�B
`B
`�B
`�B
`�B
a-B
aHB
a|B
a�B
a�B
bNB
b�B
b�B
cB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dZB
dZB
dtB
dtB
d�B
eB
e,B
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
f�B
f�B
g8B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
h�B
h�B
iB
i�B
jB
j0B
jeB
j�B
j�B
k6B
kQB
kQB
kkB
k�B
k�B
k�B
k�B
l"B
lB
k�B
k�B
lB
k�B
k�B
l"B
lWB
lqB
l�B
l�B
l�B
l�B
l=B
lB
k�B
k�B
lB
l=B
l=B
l�B
m)B
m�B
m�B
nB
n�B
n�B
oOB
oiB
o5B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
p�B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
rGB
raB
r�B
r�B
s3B
sMB
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
utB
uZB
u�B
u�B
vzB
v�B
vzB
v`B
vzB
v�B
v�B
v�B
w2B
wLB
wfB
w�B
xB
xRB
xRB
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z^B
zxB
z�B
{B
{0B
{JB
{JB
{JB
{JB
{dB
{dB
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
}B
}<B
}VB
}�B
}�B
}�B
}�B
~(B
~BB
~�B
~�B
~�B
.B
cB
}B
�B
�B
� B
�OB
�iB
��B
��B
��B
��B
��B
�UB
��B
�B
�B
�B
�AB
�AB
�[B
�[B
�uB
��B
��B
��B
�aB
�GB
��B
�B
�3B
�gB
�MB
��B
�B
�9B
�mB
�mB
��B
��B
��B
�B
�%B
�YB
�Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B'�B(
B($B'�B'�B(
B($B'�B'�B(
B'�B(>B'�B'�B'�B'�B'�B'�B(
B'�B(
B(
B(>B(>B($B($B($B(�B(�B)DB)�B*0B*KB*�B*�B+B+B+B+�B-B3�B��B��B�B{B.�BEmBv�B�OB��B��B�jB�RB�*B�jB��B�2BҽB�.B��B�+B��B��BmBR�BC{B0�B"BB�B1BAB��B�B�B�B�B�	B��B��B��B��B��B|�BV�B(�B
�ZB
��B
�vB
{�B
\�B
>BB
&�B
�B	�VB	��B	�:B	��B	pB	T�B	JXB	CGB	8�B	'RB	SB		B��B�cB��B�B��B� B		7B	0B	�B	6B	B	$tB	1AB	($B	�B	�B	
�B	TB	mB	B	�B	"�B	2�B	=<B	;0B	/�B	-�B	,B	/�B	6�B	>�B	HfB	]~B	qAB	r�B	z�B	�dB	��B	��B	��B	��B	�,B	�B	�yB	��B	��B	�~B	��B	�qB	�EB	�B	�SB	�B	��B	��B	�	B	ںB	�#B	�;B	�|B	�4B	��B	��B	��B	�B	��B	�B	��B	�]B	�wB	��B	�CB	�}B	�IB	�/B	�/B	�B	�/B	��B	��B	�B	��B	�IB	��B	��B	�B	�UB	��B	�ZB	�B	��B	��B	�B	��B	��B	�zB	��B	��B	��B	��B	�FB	��B	��B	�|B	�OB	�B	�:B	�B	��B	�\B	�jB	ޞB	��B	�kB	ٚB	�7B	�B	��B	�@B	��B	ԯB	׍B	ܬB	߾B	�B	��B	�,B	��B	��B	��B	�8B	�zB	�B	�tB	�B	� B	�NB	�-B	�vB	�B	��B	׍B	�MB	ԯB	�,B	өB	��B	�:B	�hB	�4B	�hB	ѝB	ҽB	�uB	ڠB	�jB	ܬB	��B	�B	�-B	�/B	�/B	��B	��B	��B	�/B	�;B	��B	�HB	�4B	�B	�HB	�4B	�TB	�B	��B	��B	�B	�B	� B	�:B	�B	��B	�:B	��B	�B	��B	�B	��B	��B	�B	�)B	��B	��B	��B	�B	�B	��B	��B	�	B	�	B	�$B	��B	�>B	�XB	�>B	�>B	�rB	��B	��B	�rB	��B	�XB	�rB	�$B	��B	��B	��B	��B	��B	�LB	��B	��B	�LB	�LB	��B	�`B	�FB	��B	�fB	��B	��B	�B	��B	�B	��B	��B	��B	�fB	��B	�RB	��B	�lB	�lB	��B	�lB	�RB	��B	��B	�rB	�rB	��B	��B	��B	��B	�0B	�dB	�dB	��B	��B	�B	�6B	��B	�"B	�"B	�<B	�<B	�wB	�(B	��B	��B	�}B	�}B
 �B
 iB
 B
 �B
B
B
;B
�B
oB
UB
�B
�B
B
�B
�B
�B
�B
-B
aB
�B
�B
�B
�B
3B
MB
�B
9B
�B
B
B
B
+B
�B
KB
fB
�B
�B
	B
�B
	RB
	�B
	�B
	�B
	lB
	�B

�B

�B
)B
�B
DB

�B

�B
�B
�B
dB
�B
PB
�B
6B
PB
6B
6B
<B
�B
VB
VB
B
}B
bB
HB
HB
}B
�B
B
hB
�B
�B
�B
�B
�B
&B
@B
�B
,B
{B
�B
mB
SB
�B
�B
�B
�B
EB
�B
�B
�B
	B
�B
]B
�B
�B
/B
�B
5B
5B
OB
B
5B
B
;B
pB
VB
pB
 vB
 �B
 �B
 �B
 �B
!B
!bB
"B
"�B
"�B
"�B
"�B
"�B
"�B
# B
# B
"�B
#TB
#�B
$&B
$ZB
$ZB
$@B
$ZB
$�B
$�B
%,B
&LB
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(
B
'�B
(�B
)B
)_B
)B
(�B
)B
(�B
)B
)yB
)�B
*eB
*B
*�B
*B
+B
,=B
,�B
,WB
,"B
,qB
-]B
-]B
-�B
-�B
-�B
-�B
.cB
.�B
.�B
.�B
/B
/iB
/�B
/iB
/OB
0�B
1�B
1�B
2|B
3�B
3�B
4B
4nB
4�B
5B
5�B
5�B
5�B
6�B
7LB
7�B
7�B
7�B
8B
8�B
9�B
9�B
9�B
:B
:B
:B
9�B
:�B
:�B
:�B
;JB
:�B
:�B
:�B
:�B
;B
;dB
<6B
<PB
<PB
=<B
=VB
=VB
=qB
=qB
=qB
<�B
<�B
<�B
<�B
<�B
=VB
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>wB
>�B
>�B
?B
?�B
@OB
@�B
@iB
@�B
A�B
C�B
D�B
D�B
D�B
EB
D�B
EB
D�B
EB
E�B
E�B
F?B
F�B
F�B
G+B
GEB
GB
GzB
G�B
H�B
I7B
IB
H�B
G�B
G�B
HfB
IB
I�B
I�B
J�B
JXB
KDB
K�B
L0B
LJB
LdB
L~B
L�B
MB
MB
MB
MPB
MjB
M�B
MjB
M�B
N�B
N�B
N�B
N�B
N�B
N�B
P.B
PHB
P}B
PbB
P�B
P�B
P�B
P�B
QhB
Q�B
RB
RTB
R�B
R�B
R�B
S&B
S@B
S�B
S�B
TB
T,B
T�B
T�B
UgB
UMB
U�B
VB
VmB
V�B
V�B
W$B
WYB
W�B
W�B
XyB
X�B
YB
YKB
YB
YB
Y�B
Y�B
Z7B
ZQB
ZkB
[=B
[WB
[�B
[�B
\B
\]B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]/B
]IB
^B
^B
^jB
^�B
^�B
^�B
_B
_;B
_VB
_�B
_�B
_�B
_�B
^�B
_!B
_pB
_�B
_�B
`B
`�B
`�B
`�B
a-B
aHB
a|B
a�B
a�B
bNB
b�B
b�B
cB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dZB
dZB
dtB
dtB
d�B
eB
e,B
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
f�B
f�B
g8B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
hsB
h�B
h�B
h�B
iB
i�B
jB
j0B
jeB
j�B
j�B
k6B
kQB
kQB
kkB
k�B
k�B
k�B
k�B
l"B
lB
k�B
k�B
lB
k�B
k�B
l"B
lWB
lqB
l�B
l�B
l�B
l�B
l=B
lB
k�B
k�B
lB
l=B
l=B
l�B
m)B
m�B
m�B
nB
n�B
n�B
oOB
oiB
o5B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
pUB
p�B
p�B
p�B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
rGB
raB
r�B
r�B
s3B
sMB
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
utB
uZB
u�B
u�B
vzB
v�B
vzB
v`B
vzB
v�B
v�B
v�B
w2B
wLB
wfB
w�B
xB
xRB
xRB
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z^B
zxB
z�B
{B
{0B
{JB
{JB
{JB
{JB
{dB
{dB
{�B
{�B
|B
|B
|PB
|�B
|�B
|�B
}B
}<B
}VB
}�B
}�B
}�B
}�B
~(B
~BB
~�B
~�B
~�B
.B
cB
}B
�B
�B
� B
�OB
�iB
��B
��B
��B
��B
��B
�UB
��B
�B
�B
�B
�AB
�AB
�[B
�[B
�uB
��B
��B
��B
�aB
�GB
��B
�B
�3B
�gB
�MB
��B
�B
�9B
�mB
�mB
��B
��B
��B
�B
�%B
�YB
�Y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220715034147  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220715034402  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220715034402  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220715034402                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220715124407  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220715124407  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220715040208                      G�O�G�O�G�O�                
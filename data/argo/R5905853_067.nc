CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:34:36Z creation;2022-06-04T17:34:37Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173436  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               CA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�L��i1   @�L�_�Q�@,j~��"��c7�
=p�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BA��BH  BP��BW33B_33Bj  BnffBw��B��B�  B�  B�ffB�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�33B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@{@�=p@�
=@�
=A�A?�A_�A}�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HBAz�BG�HBP�BW{B_{Bi�HBnG�Bwz�Bz�B��B��B�W
B��B�#�B��qB��B��B��B��B��B��B��B��B�#�B��qB��B�#�B�#�BϽqBӽqB׽qB��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC޸C�RC�RC�RC�RC�RC�RC�RC޸C�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA��DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ�zD[zD[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl�zDl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D�;�D�
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
D�;�D�
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
D�
D��
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;0A�A A�G�A�S�A�R�A�S�A�T�A�VA�V9A�V�A�W�A�X�A�X�A�YA�X�A�Y�A�[�A�]/A�^�A�_;A�`vA�b�A�d�A�A�X�AƥzAĴ�A��ZA��KA���A�s�A���A�!-A��A��*A�&�A���A�,qA�O�A�uZA��Au�Ak�hAg��Ab��A`�HA^��A^�A]FAX�jAS�AOl�AM{AJ�AGz�AEeAC��AA�HA?�'A>+kA<dZA;zxA;I�A;�3A<��A<	A;��A;f�A9��A8�|A7�cA70�A6CA4��A3iDA3xA2�A1�A14A1x�A1^�A1)�A1�A0��A0>BA/b�A.��A.�jA.L�A-��A-�+A-(A-�UA-��A-�A,��A,cA,[�A,?A,�A+��A+IRA*�A)��A)&A(��A(+�A'GEA&�+A%�A$��A#��A#�A"4�A"�A"_�A!��A ��A4A�A��AXA�A��A��A�6A�A�6A�A��A��AaA+A�At�A�A�?A@AHA�!A]dAOA�A9�A��A��As�A��AݘA�!A��A�4AzAr�A$�Am�A;dA
�6A
��A	��A&A��A�AMAԕA��A;�A�A�9AU2A��A��A@OA��A'RA�A��A�"AGEA]dA �AiDA:�A;A ��@�h
@���@��Q@�&�@�W�@�'R@�j@�9X@��@�4@�I@�v�@�$�@�N�@�B[@���@�>�@�h@�"h@�;@�  @�e�@�D�@�*0@�+�@�ȴ@��U@�e@䟾@�*�@�t�@�A @�$t@�Mj@�@�R�@�~(@�@@��@�,�@�)_@��@���@�q@��	@�l"@�4@�V@ݵt@�_p@� \@�~�@ۭC@�@@�خ@�v`@��@��@�	@׊	@֏\@�U2@�O@���@Չ7@��@��@�H�@��'@ҧ@�]d@��>@ќ@�v`@�u%@��@�j@�4@�u�@��+@�b�@��?@�Xy@���@�7L@��@ɷ@�v`@��@��@Ȏ�@�  @ǀ4@��M@Ɯx@��c@�q@�A @�l�@�7L@��y@�xl@Ţ�@š�@��@���@��@��B@�h�@��@���@Á�@�(�@�y>@�H�@�-�@�	@�G@�ݘ@���@�C@��4@���@�l�@��@�g�@�A @���@�@�@��j@�-w@���@��@��@��@�g�@�(@�~�@���@���@�dZ@�҉@��D@���@��C@�  @��9@��=@�[W@��@�D�@��)@��P@�T�@��5@��n@��$@�g8@��@���@�F�@���@�Ta@��@���@�x�@�͟@��@�x@��@��
@��@�ں@�=q@�	@���@���@�5?@��Z@�v`@�)_@���@�YK@��W@�\�@��@���@�}V@� �@���@�_p@��@���@��@���@�)�@�ϫ@��@��@��V@�A�@��	@��E@��@�
�@���@��g@�y�@�=@�q@��5@���@�h
@��P@��@��=@���@��k@�e�@�!-@���@�J�@�@���@�x�@�_p@�S&@�*0@�v�@�@��&@��X@�k�@��@��'@�v�@�D�@��]@��z@���@�T�@��@���@�q@�e�@���@�O@�ff@��0@�N<@���@���@���@�>B@��@���@�1�@���@�@�@��N@�f�@��@�j@��@���@�'�@��]@��@�,=@��@�K^@�c @�R�@�@���@�A @�S@���@��o@�N�@��;@�s@�=@�	l@��u@�^5@��r@���@��@�ƨ@���@�t�@�@��@��v@���@���@�tT@�L0@��>@��C@�}�@�>�@��@��@��@��@�ں@���@���@�tT@�?@��@��@���@���@���@���@�t�@�A @���@��@��!@���@���@�C-@��)@��
@��@��9@�ϫ@���@��*@���@���@�~�@�qv@�IR@��	@���@��r@�v�@�_@�%�@��@�a@9�@~l�@~GE@}�T@}�X@}w2@}S&@}@|�j@||�@|Xy@{��@{��@{�P@{4�@zn�@z5?@z0U@y��@y�'@x�@w�V@v��@vZ�@u�>@u��@u|@uL�@u0�@u \@u�@u@@u;@t�p@tZ@s��@s�@se�@s,�@s�@r��@r5?@q�@q�@qJ�@p��@o�Q@ob�@o�@o�@n͟@n��@n_�@m�@m@l�@l~(@k�@k��@k��@k�P@k33@j�c@j�}@j&�@i�C@i/@i�@hh�@g��@g\)@g�@f��@e�D@e��@e#�@d�@d�@dѷ@d��@d��@dw�@dI�@d  @c��@c4�@c(@b�y@b�@bOv@bu@azx@`��@`I�@`1'@_�&@_s@^�R@^M�@^�@]�Z@]��@\�P@\�9@\U2@[�w@[A�@Zi�@Z&�@Y�9@Y��@Y2a@Yq@X�P@XU2@W��@W1�@V�L@Uw2@UA @U;@T��@T�@Tm�@Tx@Sv`@R�y@R�A@RTa@Re@Qc@Q+@P֡@P�e@PI�@P@P�@O�K@Oo�@O�@N��@N)�@M�@M`B@M5�@M�@L�_@L�@K�@@K�k@K�f@Kx@K]�@KS�@K6z@J�H@Jn�@J�@I��@I|@I[W@I�@Hr�@H�@G�g@G�:@Gx@Ga@G1�@F��@FV@F �@E�-@E|@D�)@D4n@C]�@B��@B�m@Bq�@A��@A?}@@�j@@�@@��@@��@@g8@@A�@?˒@?H�@>͟@>h
@>Ta@>C�@>5?@>1�@>.�@>1�@>3�@>3�@>@=o @<��@;�&@;��@;\)@;)_@;�@:��@:
�@9��@9`B@9 \@8�/@8��@8C-@7��@7s@7)_@6͟@6!�@5��@5Dg@5	l@4�e@4'R@3�q@39�@2�8@2 �@1e,@1�@0��@0�|@0�@0��@0C-@/�&@/��@/s@.�s@.z@-��@-u�@,��@,S�@,1'@+��@+n/@+C�@+�@*�B@*��@*ff@*C�@*?@*+k@*_@)�Z@)�@)j@(�4@'�&@'��@'C�@'&@'�@'@&ȴ@&�@%��@%p�@%Q�@%+@$�K@$�4@$z�@$V�@$@#;d@"�@"B[@"�@!�N@!S&@!;@ ��@ �?@ ~(@ K^@ M@�W@��@H�@�@�@�B@��@n�@
�@��@rG@hs@q@�I@e�@M@�@�F@��@+@ں@�@�b@Z�@��@�M@\�@0�@V@Ĝ@A�@!@��@iD@=@1�@Y@��@�s@�x@J�@4@@?}@��@�@�e@w�@*�@@�@��@�4@o@��@�L@� @�A@ff@M�@4@�H@��@�X@�'@��@rG@Vm@?}@�@��@��@h�@[�@>B@�@��@�F@��@��@�k@�:@�	@g�@�@��@�h@{�@n�@J�@-@($@#:@
�@	@�@u@��@�)@��@s�@7L@�@�K@ی@�[@��@��@j@S�@N�@K^@K^@H@-�@�@�6@��@|�@�@
�@
�s@
�B@
��@
�@
~�@
z@
kQ@
W�@
Q@
H�@
E�@
3�@
�@	��@	@	��@	@	�^@	�"@	hs@	<6@	q@	�@�v@��@u�@9X@�A@�w@��@dZ@"�@�@.I@8@Y@(@�@�@�8@��@�@��@q�@=q@_@�j@��@�3@��@�"@}�@\�@O�@Q�@?}@�@�O@��@7�@@�@� @�*@x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;0A�A A�G�A�S�A�R�A�S�A�T�A�VA�V9A�V�A�W�A�X�A�X�A�YA�X�A�Y�A�[�A�]/A�^�A�_;A�`vA�b�A�d�A�A�X�AƥzAĴ�A��ZA��KA���A�s�A���A�!-A��A��*A�&�A���A�,qA�O�A�uZA��Au�Ak�hAg��Ab��A`�HA^��A^�A]FAX�jAS�AOl�AM{AJ�AGz�AEeAC��AA�HA?�'A>+kA<dZA;zxA;I�A;�3A<��A<	A;��A;f�A9��A8�|A7�cA70�A6CA4��A3iDA3xA2�A1�A14A1x�A1^�A1)�A1�A0��A0>BA/b�A.��A.�jA.L�A-��A-�+A-(A-�UA-��A-�A,��A,cA,[�A,?A,�A+��A+IRA*�A)��A)&A(��A(+�A'GEA&�+A%�A$��A#��A#�A"4�A"�A"_�A!��A ��A4A�A��AXA�A��A��A�6A�A�6A�A��A��AaA+A�At�A�A�?A@AHA�!A]dAOA�A9�A��A��As�A��AݘA�!A��A�4AzAr�A$�Am�A;dA
�6A
��A	��A&A��A�AMAԕA��A;�A�A�9AU2A��A��A@OA��A'RA�A��A�"AGEA]dA �AiDA:�A;A ��@�h
@���@��Q@�&�@�W�@�'R@�j@�9X@��@�4@�I@�v�@�$�@�N�@�B[@���@�>�@�h@�"h@�;@�  @�e�@�D�@�*0@�+�@�ȴ@��U@�e@䟾@�*�@�t�@�A @�$t@�Mj@�@�R�@�~(@�@@��@�,�@�)_@��@���@�q@��	@�l"@�4@�V@ݵt@�_p@� \@�~�@ۭC@�@@�خ@�v`@��@��@�	@׊	@֏\@�U2@�O@���@Չ7@��@��@�H�@��'@ҧ@�]d@��>@ќ@�v`@�u%@��@�j@�4@�u�@��+@�b�@��?@�Xy@���@�7L@��@ɷ@�v`@��@��@Ȏ�@�  @ǀ4@��M@Ɯx@��c@�q@�A @�l�@�7L@��y@�xl@Ţ�@š�@��@���@��@��B@�h�@��@���@Á�@�(�@�y>@�H�@�-�@�	@�G@�ݘ@���@�C@��4@���@�l�@��@�g�@�A @���@�@�@��j@�-w@���@��@��@��@�g�@�(@�~�@���@���@�dZ@�҉@��D@���@��C@�  @��9@��=@�[W@��@�D�@��)@��P@�T�@��5@��n@��$@�g8@��@���@�F�@���@�Ta@��@���@�x�@�͟@��@�x@��@��
@��@�ں@�=q@�	@���@���@�5?@��Z@�v`@�)_@���@�YK@��W@�\�@��@���@�}V@� �@���@�_p@��@���@��@���@�)�@�ϫ@��@��@��V@�A�@��	@��E@��@�
�@���@��g@�y�@�=@�q@��5@���@�h
@��P@��@��=@���@��k@�e�@�!-@���@�J�@�@���@�x�@�_p@�S&@�*0@�v�@�@��&@��X@�k�@��@��'@�v�@�D�@��]@��z@���@�T�@��@���@�q@�e�@���@�O@�ff@��0@�N<@���@���@���@�>B@��@���@�1�@���@�@�@��N@�f�@��@�j@��@���@�'�@��]@��@�,=@��@�K^@�c @�R�@�@���@�A @�S@���@��o@�N�@��;@�s@�=@�	l@��u@�^5@��r@���@��@�ƨ@���@�t�@�@��@��v@���@���@�tT@�L0@��>@��C@�}�@�>�@��@��@��@��@�ں@���@���@�tT@�?@��@��@���@���@���@���@�t�@�A @���@��@��!@���@���@�C-@��)@��
@��@��9@�ϫ@���@��*@���@���@�~�@�qv@�IR@��	@���@��r@�v�@�_@�%�@��@�a@9�@~l�@~GE@}�T@}�X@}w2@}S&@}@|�j@||�@|Xy@{��@{��@{�P@{4�@zn�@z5?@z0U@y��@y�'@x�@w�V@v��@vZ�@u�>@u��@u|@uL�@u0�@u \@u�@u@@u;@t�p@tZ@s��@s�@se�@s,�@s�@r��@r5?@q�@q�@qJ�@p��@o�Q@ob�@o�@o�@n͟@n��@n_�@m�@m@l�@l~(@k�@k��@k��@k�P@k33@j�c@j�}@j&�@i�C@i/@i�@hh�@g��@g\)@g�@f��@e�D@e��@e#�@d�@d�@dѷ@d��@d��@dw�@dI�@d  @c��@c4�@c(@b�y@b�@bOv@bu@azx@`��@`I�@`1'@_�&@_s@^�R@^M�@^�@]�Z@]��@\�P@\�9@\U2@[�w@[A�@Zi�@Z&�@Y�9@Y��@Y2a@Yq@X�P@XU2@W��@W1�@V�L@Uw2@UA @U;@T��@T�@Tm�@Tx@Sv`@R�y@R�A@RTa@Re@Qc@Q+@P֡@P�e@PI�@P@P�@O�K@Oo�@O�@N��@N)�@M�@M`B@M5�@M�@L�_@L�@K�@@K�k@K�f@Kx@K]�@KS�@K6z@J�H@Jn�@J�@I��@I|@I[W@I�@Hr�@H�@G�g@G�:@Gx@Ga@G1�@F��@FV@F �@E�-@E|@D�)@D4n@C]�@B��@B�m@Bq�@A��@A?}@@�j@@�@@��@@��@@g8@@A�@?˒@?H�@>͟@>h
@>Ta@>C�@>5?@>1�@>.�@>1�@>3�@>3�@>@=o @<��@;�&@;��@;\)@;)_@;�@:��@:
�@9��@9`B@9 \@8�/@8��@8C-@7��@7s@7)_@6͟@6!�@5��@5Dg@5	l@4�e@4'R@3�q@39�@2�8@2 �@1e,@1�@0��@0�|@0�@0��@0C-@/�&@/��@/s@.�s@.z@-��@-u�@,��@,S�@,1'@+��@+n/@+C�@+�@*�B@*��@*ff@*C�@*?@*+k@*_@)�Z@)�@)j@(�4@'�&@'��@'C�@'&@'�@'@&ȴ@&�@%��@%p�@%Q�@%+@$�K@$�4@$z�@$V�@$@#;d@"�@"B[@"�@!�N@!S&@!;@ ��@ �?@ ~(@ K^@ M@�W@��@H�@�@�@�B@��@n�@
�@��@rG@hs@q@�I@e�@M@�@�F@��@+@ں@�@�b@Z�@��@�M@\�@0�@V@Ĝ@A�@!@��@iD@=@1�@Y@��@�s@�x@J�@4@@?}@��@�@�e@w�@*�@@�@��@�4@o@��@�L@� @�A@ff@M�@4@�H@��@�X@�'@��@rG@Vm@?}@�@��@��@h�@[�@>B@�@��@�F@��@��@�k@�:@�	@g�@�@��@�h@{�@n�@J�@-@($@#:@
�@	@�@u@��@�)@��@s�@7L@�@�K@ی@�[@��@��@j@S�@N�@K^@K^@H@-�@�@�6@��@|�@�@
�@
�s@
�B@
��@
�@
~�@
z@
kQ@
W�@
Q@
H�@
E�@
3�@
�@	��@	@	��@	@	�^@	�"@	hs@	<6@	q@	�@�v@��@u�@9X@�A@�w@��@dZ@"�@�@.I@8@Y@(@�@�@�8@��@�@��@q�@=q@_@�j@��@�3@��@�"@}�@\�@O�@Q�@?}@�@�O@��@7�@@�@� @�*@x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BraBrGBr-Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�BrBq�Bq�Bq�Bq�Bq�BÖB�.Bl�B��B	p�B
#:B
!�B
�B
5B
gmB
JrB
{B
B	�}B	�oB	�dB	�qB	q'B	R B	G_B	>B	/iB	)yB	'�B	,qB	,"B	<�B	GzB	IRB	H�B	F�B	J�B	O�B	S�B	L�B	R�B	XyB	R B	Q�B	bB	��B	�B	�}B	�B	��B	��B	�%B	�B	�B	��B	�B	�B	��B	��B	��B	�oB
fB
�B
B
QB
)B
 �B
 B
�B
VB
!-B
&B
-�B
-�B
KB
Q�B
RB
S@B
S�B
T,B
T�B
T�B
UgB
V�B
T�B
P.B
L~B
I�B
F�B
D�B
A�B
:�B
5�B
1'B
./B
,�B
0!B
=�B
CB
<�B
9XB
;�B
@4B
C�B
F�B
FB
B�B
;�B
-�B
B
�B	�.B
;B
UB
�B	��B	�xB	�B	��B	��B	��B	�B	�B	��B	��B	�zB	��B	��B	��B	�B	��B	�$B	�XB	�B	��B	��B	��B	��B	�xB	��B	��B	�9B	��B	�=B	�B	�B	�B	�B	��B	�0B	�B	��B	�B	��B	�$B	��B	�[B	��B	��B	��B
%B
�B

�B
B

�B
	B
tB	��B	��B	�B	��B	�xB	�]B	��B	�!B	ބB	�B	��B	�HB	�B	�B	�B	��B	�B	�B	��B	�hB	ބB	�7B	�@B	�B	�B	�VB	�<B	��B	�hB	ԯB	��B	��B	өB	ںB	�)B	��B	�'B	�B	�_B	�"B	��B	�B	�=B	�"B	�B	�B	�6B	��B	��B	�B	�B	�B	�B	�@B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�nB	�`B	�B	�B	�LB	�B	�B	�sB	�B	�*B	�B	�_B	�*B	�DB	�DB	�*B	�*B	�_B	��B	��B	�B	�B	��B	�B	�KB	�B	�B	�B	�B	��B	�qB	�UB	�B	��B	��B	��B	��B	�2B	��B	�*B	��B	�PB	�(B
 �B
�B
�B
[B
�B
�B
SB
�B
�B
�B
�B
�B
�B
?B
�B
�B
EB
_B
�B
�B
KB
+B
YB
?B
%B
B
?B
%B
?B
9B
�B
�B
gB
�B
3B
�B
�B
�B
�B
�B
^B
B

�B

�B
B

�B

�B

	B
�B
B
GB
[B
�B
�B
�B
�B
%B
9B
mB
�B
B
B
�B
�B
-B
3B
3B
�B
�B
+B
?B
%B
tB
�B
�B
?B
�B
�B
�B
�B
�B
?B
YB
�B
�B
EB
�B
+B
�B
�B
B
�B
�B
�B
�B
�B
�B

=B
	�B
	�B
DB
xB
�B
VB
�B
�B
�B
�B
9B
�B
EB
�B
EB
�B
�B
�B
�B
yB
EB
EB
�B
qB
�B
�B
)B
CB
IB
/B
�B
~B
B
�B
�B
B
;B
�B
�B
 'B
 �B
 vB
�B
 'B
;B
�B
�B
�B
 vB
 �B
 \B
!bB
!�B
!�B
!|B
!HB
 �B
 B
�B
 B
 �B
!B
"NB
#�B
$B
&2B
'�B
(
B
)B
*B
+B
+B
,WB
,=B
,=B
,�B
-B
-B
-)B
-�B
-�B
.IB
.cB
.IB
.�B
.�B
.�B
/OB
/iB
/iB
/�B
/�B
/�B
/�B
0oB
0�B
0�B
1B
1AB
1AB
1'B
1'B
1[B
1[B
1vB
1�B
1�B
2B
2B
2GB
2aB
2|B
2|B
2|B
2�B
33B
33B
3hB
3�B
3�B
4B
4nB
4�B
49B
4nB
4�B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
5?B
5tB
5tB
5�B
5�B
5�B
5�B
5�B
6`B
6�B
6�B
7B
7B
72B
7LB
7�B
7�B
7�B
7�B
8RB
8RB
8B
8�B
9XB
9$B
8�B
8�B
9>B
9�B
:�B
:xB
;B
;JB
;JB
;dB
;dB
;B
;�B
;�B
;JB
;�B
;�B
<6B
<�B
<jB
<�B
<�B
<�B
=B
=VB
=�B
=qB
=�B
>]B
>�B
?HB
?HB
?HB
?}B
?cB
?�B
@4B
@�B
@�B
@�B
A;B
AUB
AUB
AoB
A�B
A�B
A�B
B'B
B�B
B�B
CB
C�B
C�B
DMB
DMB
D�B
D�B
E�B
E�B
FB
F%B
E�B
F%B
FB
F%B
FYB
FtB
F�B
GB
GB
G+B
G+B
G�B
G�B
HfB
IlB
I�B
I�B
J	B
JXB
K)B
K^B
KDB
KDB
K^B
K�B
K�B
L0B
L�B
L�B
M�B
M�B
M�B
M�B
NB
M�B
M�B
NpB
NB
NVB
N"B
MjB
L�B
L�B
L�B
L�B
L~B
L�B
M6B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
OB
OB
N�B
OBB
OvB
O�B
P.B
PHB
P}B
P�B
P�B
P�B
Q B
Q4B
Q�B
QhB
Q�B
Q�B
QhB
Q�B
Q�B
R:B
RoB
S@B
S@B
S�B
S�B
S�B
T�B
T{B
T�B
UB
T�B
UB
T�B
U2B
T�B
UB
U2B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VSB
V�B
W
B
W?B
W
B
W$B
W$B
W?B
W�B
X+B
X�B
X�B
X�B
YB
X�B
YB
Y1B
X�B
YB
X�B
Y1B
Y�B
Z�B
[#B
[WB
[�B
[�B
[�B
\CB
\�B
\�B
]IB
]dB
]dB
]~B
]�B
]�B
^�B
^�B
^�B
_�B
_�B
_�B
`'B
`vB
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
bB
a�B
bhB
bhB
bhB
b�B
cnB
c�B
c�B
c�B
dtB
d�B
d�B
eFB
e�B
e�B
fB
fLB
fLB
gB
g�B
h>B
h�B
h�B
h�B
h�B
iB
iyB
i�B
i�B
i�B
jKB
j0B
jKB
jKB
j�B
j�B
kB
kB
k6B
kkB
k�B
lB
lB
lqB
l�B
m�B
m�B
m�B
m�B
nB
nIB
ncB
nIB
n�B
n�B
n�B
n�B
o B
oB
oOB
oiB
oiB
o�B
o�B
poB
q'B
qAB
qvB
q�B
raB
r�B
r�B
r�B
sB
r�B
sMB
s�B
s�B
tB
tTB
t�B
t�B
uB
uB
u%B
u�B
vB
vB
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
{0B
{0B
{�B
|B
|B
|B
|B
|B
|6B
|jB
|jB
|jB
|�B
|�B
}"B
}<B
}qB
}�B
~B
~]B
~BB
~wB
~]B
~wB
~wB
~�B
.B
HB
�B
�B
�B
�B
�4B
�4B
�4B
�iB
�OB
�OB
�iB
��B
�OB
��B
��B
��B
�UB
�UB
��B
�oB
�oB
��B
�B
�[B
�AB
�AB
�[B
�[B
�[B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
�3B
�B
�B
�B
�MB
�MB
�MB
�MB
�MB
��B
��B
�B
�B
�B
�SB
�9B
�B
�9B
�B
�9B
�B
�9B
�mB
�B
�tB
�tB
��B
��B
�_B
��B
�B
�KB
��B
��B
��B
�B
�B
�B
�lB
��B
��B
��B
�#B
�rB
�rB
��B
��B
��B
��B
�^B
�^B
�)B
�^B
��B
��B
��B
�~B
��B
��B
��B
��B
�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BraBrGBr-Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�BrBq�Bq�Bq�Bq�Bq�BÖB�.Bl�B��B	p�B
#:B
!�B
�B
5B
gmB
JrB
{B
B	�}B	�oB	�dB	�qB	q'B	R B	G_B	>B	/iB	)yB	'�B	,qB	,"B	<�B	GzB	IRB	H�B	F�B	J�B	O�B	S�B	L�B	R�B	XyB	R B	Q�B	bB	��B	�B	�}B	�B	��B	��B	�%B	�B	�B	��B	�B	�B	��B	��B	��B	�oB
fB
�B
B
QB
)B
 �B
 B
�B
VB
!-B
&B
-�B
-�B
KB
Q�B
RB
S@B
S�B
T,B
T�B
T�B
UgB
V�B
T�B
P.B
L~B
I�B
F�B
D�B
A�B
:�B
5�B
1'B
./B
,�B
0!B
=�B
CB
<�B
9XB
;�B
@4B
C�B
F�B
FB
B�B
;�B
-�B
B
�B	�.B
;B
UB
�B	��B	�xB	�B	��B	��B	��B	�B	�B	��B	��B	�zB	��B	��B	��B	�B	��B	�$B	�XB	�B	��B	��B	��B	��B	�xB	��B	��B	�9B	��B	�=B	�B	�B	�B	�B	��B	�0B	�B	��B	�B	��B	�$B	��B	�[B	��B	��B	��B
%B
�B

�B
B

�B
	B
tB	��B	��B	�B	��B	�xB	�]B	��B	�!B	ބB	�B	��B	�HB	�B	�B	�B	��B	�B	�B	��B	�hB	ބB	�7B	�@B	�B	�B	�VB	�<B	��B	�hB	ԯB	��B	��B	өB	ںB	�)B	��B	�'B	�B	�_B	�"B	��B	�B	�=B	�"B	�B	�B	�6B	��B	��B	�B	�B	�B	�B	�@B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�nB	�`B	�B	�B	�LB	�B	�B	�sB	�B	�*B	�B	�_B	�*B	�DB	�DB	�*B	�*B	�_B	��B	��B	�B	�B	��B	�B	�KB	�B	�B	�B	�B	��B	�qB	�UB	�B	��B	��B	��B	��B	�2B	��B	�*B	��B	�PB	�(B
 �B
�B
�B
[B
�B
�B
SB
�B
�B
�B
�B
�B
�B
?B
�B
�B
EB
_B
�B
�B
KB
+B
YB
?B
%B
B
?B
%B
?B
9B
�B
�B
gB
�B
3B
�B
�B
�B
�B
�B
^B
B

�B

�B
B

�B

�B

	B
�B
B
GB
[B
�B
�B
�B
�B
%B
9B
mB
�B
B
B
�B
�B
-B
3B
3B
�B
�B
+B
?B
%B
tB
�B
�B
?B
�B
�B
�B
�B
�B
?B
YB
�B
�B
EB
�B
+B
�B
�B
B
�B
�B
�B
�B
�B
�B

=B
	�B
	�B
DB
xB
�B
VB
�B
�B
�B
�B
9B
�B
EB
�B
EB
�B
�B
�B
�B
yB
EB
EB
�B
qB
�B
�B
)B
CB
IB
/B
�B
~B
B
�B
�B
B
;B
�B
�B
 'B
 �B
 vB
�B
 'B
;B
�B
�B
�B
 vB
 �B
 \B
!bB
!�B
!�B
!|B
!HB
 �B
 B
�B
 B
 �B
!B
"NB
#�B
$B
&2B
'�B
(
B
)B
*B
+B
+B
,WB
,=B
,=B
,�B
-B
-B
-)B
-�B
-�B
.IB
.cB
.IB
.�B
.�B
.�B
/OB
/iB
/iB
/�B
/�B
/�B
/�B
0oB
0�B
0�B
1B
1AB
1AB
1'B
1'B
1[B
1[B
1vB
1�B
1�B
2B
2B
2GB
2aB
2|B
2|B
2|B
2�B
33B
33B
3hB
3�B
3�B
4B
4nB
4�B
49B
4nB
4�B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
5?B
5tB
5tB
5�B
5�B
5�B
5�B
5�B
6`B
6�B
6�B
7B
7B
72B
7LB
7�B
7�B
7�B
7�B
8RB
8RB
8B
8�B
9XB
9$B
8�B
8�B
9>B
9�B
:�B
:xB
;B
;JB
;JB
;dB
;dB
;B
;�B
;�B
;JB
;�B
;�B
<6B
<�B
<jB
<�B
<�B
<�B
=B
=VB
=�B
=qB
=�B
>]B
>�B
?HB
?HB
?HB
?}B
?cB
?�B
@4B
@�B
@�B
@�B
A;B
AUB
AUB
AoB
A�B
A�B
A�B
B'B
B�B
B�B
CB
C�B
C�B
DMB
DMB
D�B
D�B
E�B
E�B
FB
F%B
E�B
F%B
FB
F%B
FYB
FtB
F�B
GB
GB
G+B
G+B
G�B
G�B
HfB
IlB
I�B
I�B
J	B
JXB
K)B
K^B
KDB
KDB
K^B
K�B
K�B
L0B
L�B
L�B
M�B
M�B
M�B
M�B
NB
M�B
M�B
NpB
NB
NVB
N"B
MjB
L�B
L�B
L�B
L�B
L~B
L�B
M6B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
OB
OB
N�B
OBB
OvB
O�B
P.B
PHB
P}B
P�B
P�B
P�B
Q B
Q4B
Q�B
QhB
Q�B
Q�B
QhB
Q�B
Q�B
R:B
RoB
S@B
S@B
S�B
S�B
S�B
T�B
T{B
T�B
UB
T�B
UB
T�B
U2B
T�B
UB
U2B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VSB
V�B
W
B
W?B
W
B
W$B
W$B
W?B
W�B
X+B
X�B
X�B
X�B
YB
X�B
YB
Y1B
X�B
YB
X�B
Y1B
Y�B
Z�B
[#B
[WB
[�B
[�B
[�B
\CB
\�B
\�B
]IB
]dB
]dB
]~B
]�B
]�B
^�B
^�B
^�B
_�B
_�B
_�B
`'B
`vB
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
bB
a�B
bhB
bhB
bhB
b�B
cnB
c�B
c�B
c�B
dtB
d�B
d�B
eFB
e�B
e�B
fB
fLB
fLB
gB
g�B
h>B
h�B
h�B
h�B
h�B
iB
iyB
i�B
i�B
i�B
jKB
j0B
jKB
jKB
j�B
j�B
kB
kB
k6B
kkB
k�B
lB
lB
lqB
l�B
m�B
m�B
m�B
m�B
nB
nIB
ncB
nIB
n�B
n�B
n�B
n�B
o B
oB
oOB
oiB
oiB
o�B
o�B
poB
q'B
qAB
qvB
q�B
raB
r�B
r�B
r�B
sB
r�B
sMB
s�B
s�B
tB
tTB
t�B
t�B
uB
uB
u%B
u�B
vB
vB
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
{0B
{0B
{�B
|B
|B
|B
|B
|B
|6B
|jB
|jB
|jB
|�B
|�B
}"B
}<B
}qB
}�B
~B
~]B
~BB
~wB
~]B
~wB
~wB
~�B
.B
HB
�B
�B
�B
�B
�4B
�4B
�4B
�iB
�OB
�OB
�iB
��B
�OB
��B
��B
��B
�UB
�UB
��B
�oB
�oB
��B
�B
�[B
�AB
�AB
�[B
�[B
�[B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
�3B
�B
�B
�B
�MB
�MB
�MB
�MB
�MB
��B
��B
�B
�B
�B
�SB
�9B
�B
�9B
�B
�9B
�B
�9B
�mB
�B
�tB
�tB
��B
��B
�_B
��B
�B
�KB
��B
��B
��B
�B
�B
�B
�lB
��B
��B
��B
�#B
�rB
�rB
��B
��B
��B
��B
�^B
�^B
�)B
�^B
��B
��B
��B
�~B
��B
��B
��B
��B
�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104911  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173436  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173437  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173437                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023444  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023444  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                
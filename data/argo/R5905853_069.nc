CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:34:58Z creation;2022-06-04T17:34:58Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173458  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�Q���-"1   @�Q�!�n@,�G�z��cX�t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B��B  B  B ��B(  B2  B6ffBB  BH  BO33BW33Ba33Bg��Bp  BzffB~��B�ffB�ffB���B�  B�33B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ DɃ3D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A��\A�Bz�B�HB�HB �B'�HB1�HB6G�BA�HBG�HBO{BW{Ba{Bgz�Bo�HBzG�B~�B�W
B�W
B��>B��B�#�B��B��B��B��qB��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC
�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D�zD�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�Dw�D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9��D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DNzDN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D���D�?
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
Dɂ=Dɿ
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
D��=D��
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
D�?
D�
D��
D��
D�?
D�
D��
D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ӏA��aA��}A���A��A��<A��A��NA�ѷA���A��aA�֡A�خA���A��^A���A���Aα�Aγ�A΋�A�{JA�kA�^5Aŋ�A�l�A�R�A���A���A�o�A�&LA���A�_�A�rA�s�A���A|�A{�,Ay��Aq]�AhXyAc��A]��AZ�AVqAS֡AQ2�AN��AJ�AG�'AF33AD�#AC-AA,=A?��A>|�A=�A=S�A:��A9K�A7 �A6��A7CA6�oA433A2=qA1� A1�IA1��A1�}A1�A2^�A2�A1��A2~A1�"A1]�A0��A0�FA033A/�mA/��A.҉A-m�A,H�A*��A*R�A*�A)zA(��A'ɆA'A�A&��A%��A%	A$��A$bNA#��A#H�A"��A"�A"��A"H�A"1�A!�A!v�A!1�A �A ��A �A��A(A(A��A�AtTA-A-A��A�AffA�.A�zAq�A(�A��A0UA� A7LA�A�A	lAA1�A��A�{AOA33A��AbA��A($A�WAa|A�[A`�A
�A	�A�&A�5A�nAE�AA�^A�eA��Ab�A�"A��Aq�A@A�A��A\�A'RA|Aw2A ��A /@�B�@��c@�n�@��#@��)@�e,@�@���@�[�@�5?@�	@��@�ԕ@��q@��@�w2@�N<@�)_@�S@��|@���@��a@�s�@��-@���@�*�@�!@�ϫ@�C@�ff@�7@�iD@�e,@�O�@�+�@�	l@��6@��.@�@�	@�@�P@��@�j@�C@��B@�@�)�@��;@��@��@�@�"@��M@�z@�F@�l�@�˒@�%�@�A @�f�@���@⤩@�v�@�?�@��@��@���@�%�@ݠ'@�f�@܌@�'�@� �@ِ�@سh@�PH@��@��@�2a@ֆY@� �@�2a@�خ@�j@�s�@���@���@�Dg@�~�@�/@��,@̪e@�G@�y�@�bN@�	l@Ⱦ@�R�@��z@�W?@��`@Ƈ�@�/�@�
�@��@žw@Ŋ�@�1�@�S@ĩ�@�l"@��@��f@Ě�@�7@��@¹�@�C�@��C@�9�@��f@��v@��`@��@��@��@�|�@���@��@�h�@�&�@���@���@��7@���@�4@��H@���@��@�_@�H@��@���@�v`@��@��@�y>@�~@��@�2a@�i�@�7@�ݘ@��0@���@���@�8�@�k�@���@�r�@��@���@�t�@���@��]@�>�@�R�@���@��*@��q@�H�@��6@���@��n@�;d@��@��@�B�@��s@�,=@��@��@���@�Y�@��@�c�@�O@�|�@���@�2�@��
@�zx@�C@��M@���@�J@��*@�f�@�;d@��@��5@��j@�q�@�%�@��A@��'@�l�@�Y�@�8�@��@�W�@�S�@�M@���@�O@�ی@�s�@�b@�@�C�@��@��Y@�V�@�GE@��@���@��[@�b�@��@���@���@�u%@��@�8@��@���@��@�x@��"@�L0@�@�خ@��P@�9�@�@���@���@���@��@��N@�%F@�ѷ@�l"@�>B@�4n@��m@��{@�b�@�J�@�A�@�#�@�@@��|@���@�$@��q@�/@���@��@���@���@���@�M�@�*�@�
�@���@�S@���@���@�{�@�Q�@�Ft@�;�@�+k@�1@��T@��3@��f@��@���@��o@�S�@�خ@�Q�@�@��$@���@�~(@�|�@�v�@�V@�5?@�1@���@�X@�8�@�(�@��@�@@�%@���@�PH@��j@��3@���@�j@�>�@�5�@�$t@���@��<@��F@�bN@�Ft@�'R@��W@�˒@��'@�x@�a@�=@��@��<@�Q@��6@��t@��C@���@���@�s�@�f�@�e,@�\�@�S&@��@���@���@��_@�Ta@��@��S@�A @��	@��/@�bN@��@��H@���@��@�k�@�'�@�@��X@��m@��@�@�P@/�@~ں@~��@~xl@~$�@}�@}��@}�M@|�	@|PH@{ƨ@{b�@{K�@{(@z�B@zW�@y�n@x��@x7�@wخ@wy�@v��@vC�@ve@u��@uzx@u�@tM@tG@s{J@r��@r��@q�D@q%F@o�&@oP�@n�@n�,@nz@n
�@m�9@m��@mk�@m*0@l�e@lU2@l!@k~�@k8@j�,@jxl@jZ�@j)�@ju@iB�@h�)@h(�@g��@gv`@gC�@f��@e��@e}�@e?}@e%@d��@d�D@c�[@c�@b��@bM�@aG�@`��@`tT@`I�@`7@_��@_��@_�k@_+@^��@^��@]�@]�h@]B�@\�@\��@[��@[=@Z�L@Z��@Z�+@ZJ�@Y�@Y�h@Y`B@Y%F@Xl"@X~@W|�@V��@V��@Vp;@VGE@U�T@US&@T�@T�?@T$@S�@@S@R�'@R��@RGE@R
�@Q��@Q��@P�p@PD�@O"�@N��@N}V@N.�@M��@M�@L�$@Lm�@K�@KE9@J��@JQ@I�o@I��@H�/@HG@Gƨ@G�0@G��@Gj�@G4�@F��@F�B@E��@E(�@D�P@D��@D1'@C��@C�
@C�k@C��@Cs@C_p@B�H@B��@B��@B��@Bd�@B=q@B	@A�@A4@@Ĝ@@�@@1'@?�@?s@?J#@?o@>��@>q�@>B[@>0U@>�@=�@=�X@=5�@=�@<�@<��@<l"@<7�@;�A@;�@@;�@:͟@:��@:Z�@9��@9�d@9�@9X@8��@8Ĝ@8��@8�9@8��@8y>@8!@7��@7��@6��@6R�@6E�@6:*@61�@6+k@5��@5�@4�@4ѷ@4��@4b@3��@3_p@3!-@2�H@2v�@2W�@2?@2e@1�@1��@1`B@1@@0�j@0>B@0�@/�K@/qv@/U�@/J#@/C�@.�!@.h
@-�T@-*0@,��@,Ft@+��@+Mj@+@*�@*s�@*Q@*0U@*+k@*($@*
�@)��@)?}@(�v@(�O@(Q�@'�@'P�@'o@&͟@&��@&H�@&�@%�#@%�"@%X@%V@$��@$~@#��@#��@#��@#!-@"�]@"�<@"d�@"1�@!�>@!�@!4@!�@ �p@ q@�q@o�@W?@�]@�@l�@:*@�@�@
�@u@�@�@�d@�n@;@��@j@*�@	�@�+@��@o@B[@	@�@�.@��@�X@�M@O�@�@�@q@Xy@6@ݘ@��@��@{J@Mj@C@�@��@�F@n�@Z�@?@e@@�@��@��@|@�@c�@C-@~@1@�@� @�w@�k@O@@�+@;�@��@m]@?}@-w@q@@�f@��@D�@	�@�&@�q@y�@RT@>�@��@��@p;@GE@3�@�@4@��@�>@�z@�@��@�^@�t@�t@�^@�n@�7@c@c�@&�@�@q@q@��@�v@ѷ@��@�@9X@��@�a@��@��@y�@�@
��@
1�@	�@	�Z@	�@	��@	��@	��@	��@	��@	��@	hs@	�@�@�e@�o@_@Ft@x@��@��@��@��@��@�@�P@��@s@iD@_p@9�@�@�H@�L@�r@{�@u%@p;@kQ@^5@M�@C�@	@}�@X@G�@-w@@@��@�$@~(@]d@7�@@�m@�
@�
@�
@�g@�
@��@��@qv@U�@J#@J#@Mj@P�@U�@W?@H�@�@�"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ӏA��aA��}A���A��A��<A��A��NA�ѷA���A��aA�֡A�خA���A��^A���A���Aα�Aγ�A΋�A�{JA�kA�^5Aŋ�A�l�A�R�A���A���A�o�A�&LA���A�_�A�rA�s�A���A|�A{�,Ay��Aq]�AhXyAc��A]��AZ�AVqAS֡AQ2�AN��AJ�AG�'AF33AD�#AC-AA,=A?��A>|�A=�A=S�A:��A9K�A7 �A6��A7CA6�oA433A2=qA1� A1�IA1��A1�}A1�A2^�A2�A1��A2~A1�"A1]�A0��A0�FA033A/�mA/��A.҉A-m�A,H�A*��A*R�A*�A)zA(��A'ɆA'A�A&��A%��A%	A$��A$bNA#��A#H�A"��A"�A"��A"H�A"1�A!�A!v�A!1�A �A ��A �A��A(A(A��A�AtTA-A-A��A�AffA�.A�zAq�A(�A��A0UA� A7LA�A�A	lAA1�A��A�{AOA33A��AbA��A($A�WAa|A�[A`�A
�A	�A�&A�5A�nAE�AA�^A�eA��Ab�A�"A��Aq�A@A�A��A\�A'RA|Aw2A ��A /@�B�@��c@�n�@��#@��)@�e,@�@���@�[�@�5?@�	@��@�ԕ@��q@��@�w2@�N<@�)_@�S@��|@���@��a@�s�@��-@���@�*�@�!@�ϫ@�C@�ff@�7@�iD@�e,@�O�@�+�@�	l@��6@��.@�@�	@�@�P@��@�j@�C@��B@�@�)�@��;@��@��@�@�"@��M@�z@�F@�l�@�˒@�%�@�A @�f�@���@⤩@�v�@�?�@��@��@���@�%�@ݠ'@�f�@܌@�'�@� �@ِ�@سh@�PH@��@��@�2a@ֆY@� �@�2a@�خ@�j@�s�@���@���@�Dg@�~�@�/@��,@̪e@�G@�y�@�bN@�	l@Ⱦ@�R�@��z@�W?@��`@Ƈ�@�/�@�
�@��@žw@Ŋ�@�1�@�S@ĩ�@�l"@��@��f@Ě�@�7@��@¹�@�C�@��C@�9�@��f@��v@��`@��@��@��@�|�@���@��@�h�@�&�@���@���@��7@���@�4@��H@���@��@�_@�H@��@���@�v`@��@��@�y>@�~@��@�2a@�i�@�7@�ݘ@��0@���@���@�8�@�k�@���@�r�@��@���@�t�@���@��]@�>�@�R�@���@��*@��q@�H�@��6@���@��n@�;d@��@��@�B�@��s@�,=@��@��@���@�Y�@��@�c�@�O@�|�@���@�2�@��
@�zx@�C@��M@���@�J@��*@�f�@�;d@��@��5@��j@�q�@�%�@��A@��'@�l�@�Y�@�8�@��@�W�@�S�@�M@���@�O@�ی@�s�@�b@�@�C�@��@��Y@�V�@�GE@��@���@��[@�b�@��@���@���@�u%@��@�8@��@���@��@�x@��"@�L0@�@�خ@��P@�9�@�@���@���@���@��@��N@�%F@�ѷ@�l"@�>B@�4n@��m@��{@�b�@�J�@�A�@�#�@�@@��|@���@�$@��q@�/@���@��@���@���@���@�M�@�*�@�
�@���@�S@���@���@�{�@�Q�@�Ft@�;�@�+k@�1@��T@��3@��f@��@���@��o@�S�@�خ@�Q�@�@��$@���@�~(@�|�@�v�@�V@�5?@�1@���@�X@�8�@�(�@��@�@@�%@���@�PH@��j@��3@���@�j@�>�@�5�@�$t@���@��<@��F@�bN@�Ft@�'R@��W@�˒@��'@�x@�a@�=@��@��<@�Q@��6@��t@��C@���@���@�s�@�f�@�e,@�\�@�S&@��@���@���@��_@�Ta@��@��S@�A @��	@��/@�bN@��@��H@���@��@�k�@�'�@�@��X@��m@��@�@�P@/�@~ں@~��@~xl@~$�@}�@}��@}�M@|�	@|PH@{ƨ@{b�@{K�@{(@z�B@zW�@y�n@x��@x7�@wخ@wy�@v��@vC�@ve@u��@uzx@u�@tM@tG@s{J@r��@r��@q�D@q%F@o�&@oP�@n�@n�,@nz@n
�@m�9@m��@mk�@m*0@l�e@lU2@l!@k~�@k8@j�,@jxl@jZ�@j)�@ju@iB�@h�)@h(�@g��@gv`@gC�@f��@e��@e}�@e?}@e%@d��@d�D@c�[@c�@b��@bM�@aG�@`��@`tT@`I�@`7@_��@_��@_�k@_+@^��@^��@]�@]�h@]B�@\�@\��@[��@[=@Z�L@Z��@Z�+@ZJ�@Y�@Y�h@Y`B@Y%F@Xl"@X~@W|�@V��@V��@Vp;@VGE@U�T@US&@T�@T�?@T$@S�@@S@R�'@R��@RGE@R
�@Q��@Q��@P�p@PD�@O"�@N��@N}V@N.�@M��@M�@L�$@Lm�@K�@KE9@J��@JQ@I�o@I��@H�/@HG@Gƨ@G�0@G��@Gj�@G4�@F��@F�B@E��@E(�@D�P@D��@D1'@C��@C�
@C�k@C��@Cs@C_p@B�H@B��@B��@B��@Bd�@B=q@B	@A�@A4@@Ĝ@@�@@1'@?�@?s@?J#@?o@>��@>q�@>B[@>0U@>�@=�@=�X@=5�@=�@<�@<��@<l"@<7�@;�A@;�@@;�@:͟@:��@:Z�@9��@9�d@9�@9X@8��@8Ĝ@8��@8�9@8��@8y>@8!@7��@7��@6��@6R�@6E�@6:*@61�@6+k@5��@5�@4�@4ѷ@4��@4b@3��@3_p@3!-@2�H@2v�@2W�@2?@2e@1�@1��@1`B@1@@0�j@0>B@0�@/�K@/qv@/U�@/J#@/C�@.�!@.h
@-�T@-*0@,��@,Ft@+��@+Mj@+@*�@*s�@*Q@*0U@*+k@*($@*
�@)��@)?}@(�v@(�O@(Q�@'�@'P�@'o@&͟@&��@&H�@&�@%�#@%�"@%X@%V@$��@$~@#��@#��@#��@#!-@"�]@"�<@"d�@"1�@!�>@!�@!4@!�@ �p@ q@�q@o�@W?@�]@�@l�@:*@�@�@
�@u@�@�@�d@�n@;@��@j@*�@	�@�+@��@o@B[@	@�@�.@��@�X@�M@O�@�@�@q@Xy@6@ݘ@��@��@{J@Mj@C@�@��@�F@n�@Z�@?@e@@�@��@��@|@�@c�@C-@~@1@�@� @�w@�k@O@@�+@;�@��@m]@?}@-w@q@@�f@��@D�@	�@�&@�q@y�@RT@>�@��@��@p;@GE@3�@�@4@��@�>@�z@�@��@�^@�t@�t@�^@�n@�7@c@c�@&�@�@q@q@��@�v@ѷ@��@�@9X@��@�a@��@��@y�@�@
��@
1�@	�@	�Z@	�@	��@	��@	��@	��@	��@	��@	hs@	�@�@�e@�o@_@Ft@x@��@��@��@��@��@�@�P@��@s@iD@_p@9�@�@�H@�L@�r@{�@u%@p;@kQ@^5@M�@C�@	@}�@X@G�@-w@@@��@�$@~(@]d@7�@@�m@�
@�
@�
@�g@�
@��@��@qv@U�@J#@J#@Mj@P�@U�@W?@H�@�@�"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BňBňB�mB�9B�SB�9B�SB�9B�SB�9B�9B�SB�9B�mB�9B�mBŢB�B�BȴBɠB�.B�=BRTBcBb�B�*B	fB	�B	E�B	�B	�B	��B	��B	�B	_�B	]�B	W?B	F�B	6zB	,�B	%B	%`B	�B	�B	�B	 �B	B	�B	,�B	4�B	?�B	MPB	TB	T�B	VmB	WsB	e,B	o5B	cnB	nB	��B	��B	�$B	��B	�DB	�B	�4B	��B	��B
UB
B
�B
$�B
,qB
6�B
8�B
<jB
?�B
?�B
>�B
:�B
7�B
5B
2-B
1B
2B
2B
0UB
,�B
.�B
0!B
-�B
+�B
*�B
*0B
)_B
'�B
(sB
./B
3�B
33B
9�B
A�B
A�B
@�B
@OB
>�B
<�B
8�B
/�B
4nB
8lB
5�B
2�B
1vB
1'B
/OB
-�B
,�B
+�B
*�B
*eB
)DB
(�B
'�B
(XB
(>B
)B
+�B
#�B
!|B
�B
�B
�B
�B
�B
B
�B

�B
�B
�B
�B	��B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�XB	��B	�B	�B	�B	�KB	�B	�_B	�_B	�KB	��B	�DB	�B	�HB	��B	�B	�B	��B	�LB	�B	�nB	�zB	�RB	�B	��B	�>B	�B	�B	��B	�B	�B	�>B	�B	�B	��B	�OB	�B	�TB	��B	��B	��B	��B	��B	�"B	�dB	��B	�DB	��B	�B	��B	�PB	��B	��B	��B	�TB	�TB	�tB	�B	�B	�B	�|B	�B	�B	�OB	�B	�*B	�B	�B	�4B	�hB	�4B	�hB	�B	��B	�B	�B	�sB	�$B	�B	�B	�B	�B	� B	�-B	��B	��B	�'B	�B	�B	��B	�-B	��B	�B	��B	�B	��B	��B	�B	�$B	�B	�mB	��B	��B	��B	�*B	�WB	��B	��B	�]B	�)B	�B	�]B	��B	��B	�"B	�B	�B	�]B	�B	� B	��B	�B	��B	�B	�B	��B	��B	�B
 OB
  B	��B	��B	�dB	��B	��B	��B	��B	��B	�^B	��B	��B	��B	�(B
UB
�B
B
�B
;B
;B
 B
{B
�B
B
�B
�B
�B
�B
�B
�B
+B
�B
%B
�B
+B
�B
�B
�B
�B
�B
B
�B
-B
UB
  B	�}B	��B	��B	��B
 B
;B
YB
�B
AB
 B
 B
�B
�B
  B	��B	��B	��B	�]B	��B	��B	�wB	�B
  B
  B
 �B
  B
 B
 B
 4B
 �B
 �B
�B
�B
[B
�B
uB
�B
�B
3B
3B
�B
9B
9B
mB
YB
_B
�B
�B
B
�B
	�B

XB
�B
"B
�B
B
VB
�B
�B
�B
�B
�B
B
.B
�B
�B
�B
�B
�B
aB
�B
�B
{B
B
{B
�B
�B
,B
�B
B
�B
�B
�B
�B
sB
B
EB
�B
B
�B
7B
�B
B
B
�B
�B
�B
�B
 vB
 �B
 �B
 �B
!bB
!-B
!bB
!�B
!�B
"B
!�B
!�B
!�B
!�B
"B
!�B
"4B
"B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#TB
#�B
#:B
#�B
$B
#�B
$�B
$�B
$ZB
$ZB
$@B
$@B
#�B
$&B
$tB
%,B
$�B
%,B
%B
%FB
%,B
$�B
%FB
%zB
%�B
'mB
(�B
*B
)�B
*0B
*�B
*�B
+B
+6B
+�B
,�B
-]B
-CB
-�B
-�B
-�B
-�B
.IB
.�B
/OB
/�B
/�B
0B
0UB
0;B
0oB
0�B
0oB
0oB
0UB
0�B
1'B
1AB
0�B
1vB
2-B
2GB
2�B
2�B
2�B
4B
4nB
4�B
4�B
4�B
4�B
5?B
5%B
5tB
5ZB
5�B
6`B
6+B
6zB
6zB
6�B
6�B
7B
6�B
6�B
6�B
7LB
7�B
8B
88B
8RB
8lB
8�B
8�B
9>B
9�B
:B
:DB
:^B
;0B
;dB
;dB
;�B
;�B
<B
<�B
<�B
<�B
="B
=qB
=�B
=�B
>�B
?.B
?.B
?B
?}B
?�B
?�B
?�B
?�B
@B
@�B
@�B
@�B
A�B
AoB
A�B
BB
A�B
BuB
BB
B�B
B�B
C�B
C�B
C�B
C�B
D�B
ESB
ESB
E�B
E�B
E�B
E�B
F�B
F�B
F�B
GEB
H1B
H�B
H�B
H�B
H�B
IB
I7B
IRB
I�B
J	B
J#B
J�B
K)B
KDB
KxB
K�B
L�B
L�B
L�B
MB
MB
L�B
L�B
L~B
LJB
L~B
L0B
K�B
L0B
LB
LB
K�B
K�B
L0B
L~B
L~B
L�B
MB
MPB
M�B
M�B
M�B
N"B
NVB
NVB
N�B
O(B
OBB
O�B
PB
P.B
P.B
PbB
Q4B
Q4B
QhB
Q�B
Q�B
R:B
R�B
R�B
SB
S�B
T{B
TB
TB
T,B
TB
S�B
TB
T,B
T�B
T�B
T�B
UB
UgB
U�B
UgB
U�B
U�B
U�B
U�B
VmB
VmB
V�B
VmB
V�B
V�B
V�B
V�B
W�B
XEB
XyB
X�B
X�B
YB
YKB
YKB
Y�B
Y�B
ZB
ZB
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
[WB
[WB
[qB
[�B
[�B
\xB
\xB
\]B
\�B
\�B
]B
]B
]~B
]�B
]�B
]�B
]�B
]�B
]�B
^5B
^OB
^�B
_B
_�B
_�B
_�B
_VB
_pB
_�B
`vB
`\B
`\B
`\B
`�B
`�B
aHB
a|B
bB
cB
cB
cB
c B
c�B
c�B
c�B
c�B
dZB
d�B
d�B
d�B
e,B
e,B
e,B
eB
e�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h>B
hsB
hsB
h�B
h�B
hsB
h�B
h�B
iyB
iyB
iyB
i�B
jeB
j�B
j�B
j�B
kB
kQB
kQB
k�B
k�B
k�B
lB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n/B
m�B
nIB
n�B
o B
o B
oB
o5B
p;B
p;B
p;B
p�B
p�B
p�B
qAB
q[B
q[B
qAB
q[B
qAB
qvB
q[B
qvB
r-B
r-B
r|B
r�B
r�B
r�B
r�B
s�B
tB
tnB
tB
t9B
t�B
t�B
t�B
t�B
u%B
utB
u�B
u�B
u�B
vFB
v`B
v+B
vzB
v�B
v�B
wB
w2B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
y	B
y$B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
{dB
{�B
|B
|B
|B
|B
|B
|�B
|�B
}B
}"B
}"B
}qB
}�B
}�B
~B
~BB
~�B
~�B
~�B
~�B
~�B
B
.B
}B
HB
cB
HB
}B
}B
HB
�B
�B
�B
�B
�B
�OB
��B
�iB
��B
��B
��B
��B
��B
�;B
��B
��B
�B
�[B
�AB
��B
��B
��B
�B
��B
�B
�MB
�gB
�MB
�MB
�gB
�gB
��B
��B
�9B
�mB
�9B
�SB
�mB
��B
�%B
�%B
�?B
��B
��B
��B
��B
��B
��B
��B
�B
�_B
�_B
��B
�1B
�1B
�KB
�fB
�KB
�fB
��B
��B
��B
�B
��B
��B
��B
�	B
�#B
�rB
��B
�DB
�xB
��B
��B
��B
�B
��B
��B
�B
��B
�B
�dB
�~B
�~B
��B
��B
�~B
��B
��B
�~B
��B
��B
�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BňBňB�mB�9B�SB�9B�SB�9B�SB�9B�9B�SB�9B�mB�9B�mBŢB�B�BȴBɠB�.B�=BRTBcBb�B�*B	fB	�B	E�B	�B	�B	��B	��B	�B	_�B	]�B	W?B	F�B	6zB	,�B	%B	%`B	�B	�B	�B	 �B	B	�B	,�B	4�B	?�B	MPB	TB	T�B	VmB	WsB	e,B	o5B	cnB	nB	��B	��B	�$B	��B	�DB	�B	�4B	��B	��B
UB
B
�B
$�B
,qB
6�B
8�B
<jB
?�B
?�B
>�B
:�B
7�B
5B
2-B
1B
2B
2B
0UB
,�B
.�B
0!B
-�B
+�B
*�B
*0B
)_B
'�B
(sB
./B
3�B
33B
9�B
A�B
A�B
@�B
@OB
>�B
<�B
8�B
/�B
4nB
8lB
5�B
2�B
1vB
1'B
/OB
-�B
,�B
+�B
*�B
*eB
)DB
(�B
'�B
(XB
(>B
)B
+�B
#�B
!|B
�B
�B
�B
�B
�B
B
�B

�B
�B
�B
�B	��B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�XB	��B	�B	�B	�B	�KB	�B	�_B	�_B	�KB	��B	�DB	�B	�HB	��B	�B	�B	��B	�LB	�B	�nB	�zB	�RB	�B	��B	�>B	�B	�B	��B	�B	�B	�>B	�B	�B	��B	�OB	�B	�TB	��B	��B	��B	��B	��B	�"B	�dB	��B	�DB	��B	�B	��B	�PB	��B	��B	��B	�TB	�TB	�tB	�B	�B	�B	�|B	�B	�B	�OB	�B	�*B	�B	�B	�4B	�hB	�4B	�hB	�B	��B	�B	�B	�sB	�$B	�B	�B	�B	�B	� B	�-B	��B	��B	�'B	�B	�B	��B	�-B	��B	�B	��B	�B	��B	��B	�B	�$B	�B	�mB	��B	��B	��B	�*B	�WB	��B	��B	�]B	�)B	�B	�]B	��B	��B	�"B	�B	�B	�]B	�B	� B	��B	�B	��B	�B	�B	��B	��B	�B
 OB
  B	��B	��B	�dB	��B	��B	��B	��B	��B	�^B	��B	��B	��B	�(B
UB
�B
B
�B
;B
;B
 B
{B
�B
B
�B
�B
�B
�B
�B
�B
+B
�B
%B
�B
+B
�B
�B
�B
�B
�B
B
�B
-B
UB
  B	�}B	��B	��B	��B
 B
;B
YB
�B
AB
 B
 B
�B
�B
  B	��B	��B	��B	�]B	��B	��B	�wB	�B
  B
  B
 �B
  B
 B
 B
 4B
 �B
 �B
�B
�B
[B
�B
uB
�B
�B
3B
3B
�B
9B
9B
mB
YB
_B
�B
�B
B
�B
	�B

XB
�B
"B
�B
B
VB
�B
�B
�B
�B
�B
B
.B
�B
�B
�B
�B
�B
aB
�B
�B
{B
B
{B
�B
�B
,B
�B
B
�B
�B
�B
�B
sB
B
EB
�B
B
�B
7B
�B
B
B
�B
�B
�B
�B
 vB
 �B
 �B
 �B
!bB
!-B
!bB
!�B
!�B
"B
!�B
!�B
!�B
!�B
"B
!�B
"4B
"B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#TB
#�B
#:B
#�B
$B
#�B
$�B
$�B
$ZB
$ZB
$@B
$@B
#�B
$&B
$tB
%,B
$�B
%,B
%B
%FB
%,B
$�B
%FB
%zB
%�B
'mB
(�B
*B
)�B
*0B
*�B
*�B
+B
+6B
+�B
,�B
-]B
-CB
-�B
-�B
-�B
-�B
.IB
.�B
/OB
/�B
/�B
0B
0UB
0;B
0oB
0�B
0oB
0oB
0UB
0�B
1'B
1AB
0�B
1vB
2-B
2GB
2�B
2�B
2�B
4B
4nB
4�B
4�B
4�B
4�B
5?B
5%B
5tB
5ZB
5�B
6`B
6+B
6zB
6zB
6�B
6�B
7B
6�B
6�B
6�B
7LB
7�B
8B
88B
8RB
8lB
8�B
8�B
9>B
9�B
:B
:DB
:^B
;0B
;dB
;dB
;�B
;�B
<B
<�B
<�B
<�B
="B
=qB
=�B
=�B
>�B
?.B
?.B
?B
?}B
?�B
?�B
?�B
?�B
@B
@�B
@�B
@�B
A�B
AoB
A�B
BB
A�B
BuB
BB
B�B
B�B
C�B
C�B
C�B
C�B
D�B
ESB
ESB
E�B
E�B
E�B
E�B
F�B
F�B
F�B
GEB
H1B
H�B
H�B
H�B
H�B
IB
I7B
IRB
I�B
J	B
J#B
J�B
K)B
KDB
KxB
K�B
L�B
L�B
L�B
MB
MB
L�B
L�B
L~B
LJB
L~B
L0B
K�B
L0B
LB
LB
K�B
K�B
L0B
L~B
L~B
L�B
MB
MPB
M�B
M�B
M�B
N"B
NVB
NVB
N�B
O(B
OBB
O�B
PB
P.B
P.B
PbB
Q4B
Q4B
QhB
Q�B
Q�B
R:B
R�B
R�B
SB
S�B
T{B
TB
TB
T,B
TB
S�B
TB
T,B
T�B
T�B
T�B
UB
UgB
U�B
UgB
U�B
U�B
U�B
U�B
VmB
VmB
V�B
VmB
V�B
V�B
V�B
V�B
W�B
XEB
XyB
X�B
X�B
YB
YKB
YKB
Y�B
Y�B
ZB
ZB
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
[WB
[WB
[qB
[�B
[�B
\xB
\xB
\]B
\�B
\�B
]B
]B
]~B
]�B
]�B
]�B
]�B
]�B
]�B
^5B
^OB
^�B
_B
_�B
_�B
_�B
_VB
_pB
_�B
`vB
`\B
`\B
`\B
`�B
`�B
aHB
a|B
bB
cB
cB
cB
c B
c�B
c�B
c�B
c�B
dZB
d�B
d�B
d�B
e,B
e,B
e,B
eB
e�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h>B
hsB
hsB
h�B
h�B
hsB
h�B
h�B
iyB
iyB
iyB
i�B
jeB
j�B
j�B
j�B
kB
kQB
kQB
k�B
k�B
k�B
lB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n/B
m�B
nIB
n�B
o B
o B
oB
o5B
p;B
p;B
p;B
p�B
p�B
p�B
qAB
q[B
q[B
qAB
q[B
qAB
qvB
q[B
qvB
r-B
r-B
r|B
r�B
r�B
r�B
r�B
s�B
tB
tnB
tB
t9B
t�B
t�B
t�B
t�B
u%B
utB
u�B
u�B
u�B
vFB
v`B
v+B
vzB
v�B
v�B
wB
w2B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
y	B
y$B
yXB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
{dB
{�B
|B
|B
|B
|B
|B
|�B
|�B
}B
}"B
}"B
}qB
}�B
}�B
~B
~BB
~�B
~�B
~�B
~�B
~�B
B
.B
}B
HB
cB
HB
}B
}B
HB
�B
�B
�B
�B
�B
�OB
��B
�iB
��B
��B
��B
��B
��B
�;B
��B
��B
�B
�[B
�AB
��B
��B
��B
�B
��B
�B
�MB
�gB
�MB
�MB
�gB
�gB
��B
��B
�9B
�mB
�9B
�SB
�mB
��B
�%B
�%B
�?B
��B
��B
��B
��B
��B
��B
��B
�B
�_B
�_B
��B
�1B
�1B
�KB
�fB
�KB
�fB
��B
��B
��B
�B
��B
��B
��B
�	B
�#B
�rB
��B
�DB
�xB
��B
��B
��B
�B
��B
��B
�B
��B
�B
�dB
�~B
�~B
��B
��B
�~B
��B
��B
�~B
��B
��B
�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104912  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173458  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173458  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173458                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023506  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023506  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                
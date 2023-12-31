CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-23T09:43:50Z creation;2023-06-23T09:43:51Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230623094350  20230623095839  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�5
�8!`1   @�5�?V@0�7KƧ��c��G�{1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B���B�  B�  B�  B�  B���B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<ffC=��C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@w�@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBPG�BXG�B_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��qB��B��B��qB��B��B��qB��B��B��B��B̽qB�#�B��B׽qB��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC
�C�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC<^�C=�C?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC~�C��C��)C��)C��)C��\C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D���D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�?
D�
D��
D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϖSAϘ+AϜCAϙ�Aϕ�Aϑ�AϓuAϑhAύ�Aϕ�Aϕ�AϚAϚ�AϗYAϕ�AωA�|�A�y>A��Aϊ�A�w�A�;�A�5�A�2�A�33A�4A�4�A�4nA�2aA�.}A�/OA�.}A�/A�.�A�1�A�-�A�)�A�Aξ�A͒:A��6A�P�A�A��A�2�A�@�A�V�A��;A�͟A��A���A��mA�ںA���A�~�A�g�A��A�^�A���A�^�A���A�A�AA���A���A��A�;dA�p;A��A���A��dA�]�A���A�бA�hsA�b�A���A�{A�lWA���A��}A�,=A���A��eA��3A���A��;A�bNA�-wA�[#A�A��qA�a|A�t�A��A���A��BA�D3A���A}!Az�.Au�Aq��An��AjxAeu�Ab�uAa�A`�<A`�hA`[�A_�]A_�A]uAZ6�AW�&AUbNAR��ARIRAQ�AL��AI|AH�AF��AC��A@:�A>��A;ԕA:o�A6+A2;�A/�9A-�_A+SA(~�A'�A&��A&xA&2aA$~�A#�A#�fA#	A#p�A!]�A�zA�FAںA��A�9A�AFAjA?}A�.A�zA��AE�A��Ap�A�NA�AAr�A'�A+A|�A�9A��A{JAoARTA��A�!A�A�:A4�A7LA,=A{A��A�{A<6A�Am�A|A}VA�A��A�A�qA)_A��A�jA�A
�5A	�_A�A}�Ae�AMjAc A6�A��A�RA_A	lA�A�A��Ay>AoA j@��@��u@�J�@�A�@��@��.@��@��D@�V@��$@��F@�PH@��:@�&@�]�@�&�@�$t@�"�@���@��@�J�@��s@��*@�@�+@�u@�h@��5@���@��@�{@�  @��z@�e�@�"@�k@�6@� i@��@�x@��?@�k�@�u@�7@�I@�d�@��@��f@��@�V@�	@ޞ�@ݘ�@�b�@�g8@���@ې�@��@���@��D@��@ֶ�@ֿ�@֡b@�E�@�ԕ@�J#@���@�ѷ@Ԍ@�]d@���@��@�l"@�@Ѵ�@�Q�@�
=@���@Ч�@�H�@��@Ϧ�@�{J@�2a@��@Γu@�4@��@͒:@��@���@�ȴ@̹$@��B@��X@̗�@�^5@��@�c�@�+@�q@���@���@ʕ@�u%@�Xy@�6@ɵt@��f@�u�@�7@ǘ�@�G�@��M@�~(@��@�L�@�}V@�"h@â�@�IR@��v@�"h@��@�h�@�J@�o @���@���@�g8@�)�@��
@��H@��@�0�@��@��@�F�@���@���@���@�y�@�)_@�rG@��@�&�@���@��@�ԕ@���@�$t@�@��p@���@��@�,�@��2@�]d@��@��)@�j@�*0@��@��@���@���@�h
@�S�@��@��'@��@��t@��@��@�	�@��@��@�e�@��@��@�?}@��5@��O@���@���@��A@��@�IR@��@��c@���@�H�@��7@��@�PH@�!�@��9@��@�o�@�.I@��2@���@�f�@�2a@���@��L@��u@��o@�M@�b�@�&@��P@���@��o@�p;@��@��@�@�s@�:�@���@��O@�z�@�_@�e�@�C�@�&�@���@�c @�1'@��@�?}@��8@�ی@���@�L0@�@�_@��j@��0@�\�@��@��@�n�@�Q@�G@��T@��z@��@�o @���@�p;@���@���@�2a@��v@���@�Ov@�3�@�(�@�{@�-@��/@�,�@�(@��/@��D@��@�ƨ@���@��f@��@��c@���@�+k@��@���@�4�@�]�@�A @��@��@���@�u%@�'R@�	�@��z@�t�@�o@��M@��U@�w�@��o@�`�@���@��C@�6z@�d�@�Ta@�v`@���@��@��B@��R@���@��@��q@�y�@���@�a�@�\�@��@��@�oi@�9X@�M@�5?@���@���@�~�@�s�@�f�@�:�@��@��s@���@�d�@�\�@�%�@��@�	@���@��@��3@�[W@�o@��[@��U@���@��6@���@�Z�@�M@���@��7@�iD@�q@���@��$@�H@��@���@�S�@���@�?@��@x@=@~Ov@}��@}�@}zx@}�@|�@|��@|q@|A�@{��@{�@zں@z�R@z�\@y�@yO�@x$@w��@w��@w��@wt�@w]�@wO@w!-@w�@v�]@v��@vL0@v�@u�N@u�h@tɆ@s��@se�@s'�@r�H@r{�@qs�@p�/@p��@o��@oZ�@o33@o�@n��@n��@nl�@nu@m�3@m��@m�@l�j@l��@lU2@k��@k.I@j�}@jR�@j@i�j@ix�@i5�@h��@h��@h%�@g�f@f�6@e��@ex�@eo @ea�@ea�@eT�@d�?@d<�@c��@cj�@c�@bߤ@b��@bC�@bO@b	@b_@a!�@_�@_�:@_,�@_"�@_�@^�y@^҉@^��@^v�@]�3@]X@]IR@]+@]%@\�D@\$@\,=@\*�@\7@[��@[�a@[��@[�@[g�@[F�@Z�@Z{�@Z:*@Y5�@X��@X��@XK^@X �@W_p@V҉@V0U@U��@Um]@TɆ@T�@TI�@S�w@R�"@R��@S�@S$t@S.I@S33@S�@R�@RV@Q�S@Pی@P��@P�?@PA�@OK�@N�L@Nv�@Na|@N	@M�@M8�@M	l@L�j@L�I@Le�@LC-@L	�@Kn/@K8@J��@J� @Jh
@Ja|@JW�@JQ@JC�@J	@I�@I�M@H��@H�O@H�I@HXy@H2�@G�@Gl�@G i@Fu%@F�@E��@Ew2@EN<@EV@D֡@D�O@D��@D~(@DZ@D7@C��@C�@B�8@B��@B0U@A�@A�-@A�=@Ao @AVm@A?}@@��@@tT@@]d@@M@?�K@?v`@>�@>��@>^5@=�o@=k�@=%@<�.@<1'@;�}@;�F@;�@;l�@;�@:͟@:�6@:� @:xl@:W�@9��@9[W@9O�@9G�@9:�@9+�@8�@8�Y@7��@7�@7qv@6�y@6d�@5�@5rG@5A @4�P@4�j@4y>@4M@4$@3��@3ݘ@3ƨ@3�F@3�F@3��@3��@3�	@3�@3|�@3j�@2�]@1�T@1�@1�@1��@1L�@0�5@0~(@0oi@0N�@0b@/��@/��@/O@/)_@.�@.^5@-�@-��@-|@,�@,�.@,~@,  @+v`@*��@*}V@*Z�@*R�@*C�@*!�@)��@)�@(tT@(7�@(-�@(	�@'�+@'�@'˒@'j�@&�L@&Ta@&�@%�N@%�'@%*0@$�?@$c�@#خ@#�@#g�@#Mj@#>�@#�@"�@"�<@"� @"��@"Q@"�@!�H@!��@![W@!?}@ ��@ �9@ ��@ q@ �@ @��@�@��@�V@.I@҉@��@V@B[@5?@.�@@u@�T@�X@��@T�@�@��@�z@�@U2@�@��@�w@�a@ƨ@��@ƨ@x@a@K�@'�@�@�R@��@�@��@��@�@֡@Ĝ@��@V�@�@�@�k@o�@J#@,�@C@�@ߤ@�@d�@B[@	@�@�9@�^@��@��@�@x�@rG@4@V@��@ی@��@��@c�@:�@�@��@dZ@C@҉@��@�\@xl@\�@�@��@��@��@�@�7@|@Y�@4@@@�P@֡@�@<�@�@�a@��@��@W?@�@�@��@p;@C�@?@O@u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϖSAϘ+AϜCAϙ�Aϕ�Aϑ�AϓuAϑhAύ�Aϕ�Aϕ�AϚAϚ�AϗYAϕ�AωA�|�A�y>A��Aϊ�A�w�A�;�A�5�A�2�A�33A�4A�4�A�4nA�2aA�.}A�/OA�.}A�/A�.�A�1�A�-�A�)�A�Aξ�A͒:A��6A�P�A�A��A�2�A�@�A�V�A��;A�͟A��A���A��mA�ںA���A�~�A�g�A��A�^�A���A�^�A���A�A�AA���A���A��A�;dA�p;A��A���A��dA�]�A���A�бA�hsA�b�A���A�{A�lWA���A��}A�,=A���A��eA��3A���A��;A�bNA�-wA�[#A�A��qA�a|A�t�A��A���A��BA�D3A���A}!Az�.Au�Aq��An��AjxAeu�Ab�uAa�A`�<A`�hA`[�A_�]A_�A]uAZ6�AW�&AUbNAR��ARIRAQ�AL��AI|AH�AF��AC��A@:�A>��A;ԕA:o�A6+A2;�A/�9A-�_A+SA(~�A'�A&��A&xA&2aA$~�A#�A#�fA#	A#p�A!]�A�zA�FAںA��A�9A�AFAjA?}A�.A�zA��AE�A��Ap�A�NA�AAr�A'�A+A|�A�9A��A{JAoARTA��A�!A�A�:A4�A7LA,=A{A��A�{A<6A�Am�A|A}VA�A��A�A�qA)_A��A�jA�A
�5A	�_A�A}�Ae�AMjAc A6�A��A�RA_A	lA�A�A��Ay>AoA j@��@��u@�J�@�A�@��@��.@��@��D@�V@��$@��F@�PH@��:@�&@�]�@�&�@�$t@�"�@���@��@�J�@��s@��*@�@�+@�u@�h@��5@���@��@�{@�  @��z@�e�@�"@�k@�6@� i@��@�x@��?@�k�@�u@�7@�I@�d�@��@��f@��@�V@�	@ޞ�@ݘ�@�b�@�g8@���@ې�@��@���@��D@��@ֶ�@ֿ�@֡b@�E�@�ԕ@�J#@���@�ѷ@Ԍ@�]d@���@��@�l"@�@Ѵ�@�Q�@�
=@���@Ч�@�H�@��@Ϧ�@�{J@�2a@��@Γu@�4@��@͒:@��@���@�ȴ@̹$@��B@��X@̗�@�^5@��@�c�@�+@�q@���@���@ʕ@�u%@�Xy@�6@ɵt@��f@�u�@�7@ǘ�@�G�@��M@�~(@��@�L�@�}V@�"h@â�@�IR@��v@�"h@��@�h�@�J@�o @���@���@�g8@�)�@��
@��H@��@�0�@��@��@�F�@���@���@���@�y�@�)_@�rG@��@�&�@���@��@�ԕ@���@�$t@�@��p@���@��@�,�@��2@�]d@��@��)@�j@�*0@��@��@���@���@�h
@�S�@��@��'@��@��t@��@��@�	�@��@��@�e�@��@��@�?}@��5@��O@���@���@��A@��@�IR@��@��c@���@�H�@��7@��@�PH@�!�@��9@��@�o�@�.I@��2@���@�f�@�2a@���@��L@��u@��o@�M@�b�@�&@��P@���@��o@�p;@��@��@�@�s@�:�@���@��O@�z�@�_@�e�@�C�@�&�@���@�c @�1'@��@�?}@��8@�ی@���@�L0@�@�_@��j@��0@�\�@��@��@�n�@�Q@�G@��T@��z@��@�o @���@�p;@���@���@�2a@��v@���@�Ov@�3�@�(�@�{@�-@��/@�,�@�(@��/@��D@��@�ƨ@���@��f@��@��c@���@�+k@��@���@�4�@�]�@�A @��@��@���@�u%@�'R@�	�@��z@�t�@�o@��M@��U@�w�@��o@�`�@���@��C@�6z@�d�@�Ta@�v`@���@��@��B@��R@���@��@��q@�y�@���@�a�@�\�@��@��@�oi@�9X@�M@�5?@���@���@�~�@�s�@�f�@�:�@��@��s@���@�d�@�\�@�%�@��@�	@���@��@��3@�[W@�o@��[@��U@���@��6@���@�Z�@�M@���@��7@�iD@�q@���@��$@�H@��@���@�S�@���@�?@��@x@=@~Ov@}��@}�@}zx@}�@|�@|��@|q@|A�@{��@{�@zں@z�R@z�\@y�@yO�@x$@w��@w��@w��@wt�@w]�@wO@w!-@w�@v�]@v��@vL0@v�@u�N@u�h@tɆ@s��@se�@s'�@r�H@r{�@qs�@p�/@p��@o��@oZ�@o33@o�@n��@n��@nl�@nu@m�3@m��@m�@l�j@l��@lU2@k��@k.I@j�}@jR�@j@i�j@ix�@i5�@h��@h��@h%�@g�f@f�6@e��@ex�@eo @ea�@ea�@eT�@d�?@d<�@c��@cj�@c�@bߤ@b��@bC�@bO@b	@b_@a!�@_�@_�:@_,�@_"�@_�@^�y@^҉@^��@^v�@]�3@]X@]IR@]+@]%@\�D@\$@\,=@\*�@\7@[��@[�a@[��@[�@[g�@[F�@Z�@Z{�@Z:*@Y5�@X��@X��@XK^@X �@W_p@V҉@V0U@U��@Um]@TɆ@T�@TI�@S�w@R�"@R��@S�@S$t@S.I@S33@S�@R�@RV@Q�S@Pی@P��@P�?@PA�@OK�@N�L@Nv�@Na|@N	@M�@M8�@M	l@L�j@L�I@Le�@LC-@L	�@Kn/@K8@J��@J� @Jh
@Ja|@JW�@JQ@JC�@J	@I�@I�M@H��@H�O@H�I@HXy@H2�@G�@Gl�@G i@Fu%@F�@E��@Ew2@EN<@EV@D֡@D�O@D��@D~(@DZ@D7@C��@C�@B�8@B��@B0U@A�@A�-@A�=@Ao @AVm@A?}@@��@@tT@@]d@@M@?�K@?v`@>�@>��@>^5@=�o@=k�@=%@<�.@<1'@;�}@;�F@;�@;l�@;�@:͟@:�6@:� @:xl@:W�@9��@9[W@9O�@9G�@9:�@9+�@8�@8�Y@7��@7�@7qv@6�y@6d�@5�@5rG@5A @4�P@4�j@4y>@4M@4$@3��@3ݘ@3ƨ@3�F@3�F@3��@3��@3�	@3�@3|�@3j�@2�]@1�T@1�@1�@1��@1L�@0�5@0~(@0oi@0N�@0b@/��@/��@/O@/)_@.�@.^5@-�@-��@-|@,�@,�.@,~@,  @+v`@*��@*}V@*Z�@*R�@*C�@*!�@)��@)�@(tT@(7�@(-�@(	�@'�+@'�@'˒@'j�@&�L@&Ta@&�@%�N@%�'@%*0@$�?@$c�@#خ@#�@#g�@#Mj@#>�@#�@"�@"�<@"� @"��@"Q@"�@!�H@!��@![W@!?}@ ��@ �9@ ��@ q@ �@ @��@�@��@�V@.I@҉@��@V@B[@5?@.�@@u@�T@�X@��@T�@�@��@�z@�@U2@�@��@�w@�a@ƨ@��@ƨ@x@a@K�@'�@�@�R@��@�@��@��@�@֡@Ĝ@��@V�@�@�@�k@o�@J#@,�@C@�@ߤ@�@d�@B[@	@�@�9@�^@��@��@�@x�@rG@4@V@��@ی@��@��@c�@:�@�@��@dZ@C@҉@��@�\@xl@\�@�@��@��@��@�@�7@|@Y�@4@@@�P@֡@�@<�@�@�a@��@��@W?@�@�@��@p;@C�@?@O@u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�}B
�bB
�.B
�HB
�}B
��B
��B
��B
��B
��B
��B
�}B
�}B
��B
��B
�NB
�NB
��B
�hB
� B
�B
��B
��B
�,B
�{B
��B
�B
�2B
��B
��B
�gB
��B
��B
�SB
�EB
��B
�jB
��B
��B
��B
��BKB BB$�BKBX�BW�Bk�B��B��B�OB�-B�	B��B��B�BB��B��B�gB��B�JBΥB�VB�B��B��B��B�B�@B�BǔB��B�rB��B��B��B�9B�aB}�Be,Ba�B^�BT,BHKB8B$�B6B
�qB
�cB
�1B
ĶB
�B
�/B
��B
y�B
c�B
OBB
1vB
gB
�B	�B	ΥB	�*B	�|B	�xB	z�B	v�B	qvB	p�B	o5B	m�B	lB	cnB	P�B	EB	?}B	8B	7�B	E�B	FYB	>B	<PB	4�B	%B	�B	(
B	&LB	# B		�B�vB�B��B��B�B�JB�0B��B�B�>B��B�B��B՛B�JBƨB�SBʦB�B�B	eB	)_B	?cB	I7B	N�B	P�B	T�B	V�B	X�B	WsB	VB	U�B	Y1B	[qB	[qB	[=B	[�B	[=B	[�B	[	B	^�B	g�B	r�B	vzB	zDB	��B	��B	��B	��B	��B	�_B	�B	��B	�\B	�)B	�QB	��B	��B	�B	�pB	��B	��B	��B	��B	�[B	�B	{�B	z�B	z�B	z�B	cB	�KB	��B	�oB	~BB	��B	�"B	��B	~BB	qvB	nIB	l�B	m�B	q'B	v+B	yrB	|PB	~�B	�oB	�tB	��B	��B	��B	�]B	��B	��B	�JB	�B	�tB	�B	|PB	zxB	zxB	yXB	z�B	z^B	|B	�B	�gB	��B	��B	��B	�B	��B	�SB	��B	�KB	��B	��B	��B	��B	�=B	��B	��B	�jB	�/B	��B	��B	�4B	�&B	��B	�OB	��B	�-B	�'B	�[B	��B	�B	��B	��B	�;B	�-B	��B	�?B	��B	��B	�[B	�B	��B	�[B	�B	��B	ĶB	��B	��B	�_B	ǮB	ȀB	��B	��B	�VB	�B	�HB	ѝB	҉B	�[B	��B	��B	��B	�WB	�~B	ߤB	�B	�B	�B	�0B	�6B	�WB	�B	�"B	�B	�B	�wB	�CB	��B	�oB	��B	�aB	��B	��B	��B	��B	��B	��B	�fB	�B	��B	��B	�>B	��B	��B	��B	�DB	�0B	��B	�dB	�B	�qB	�VB	�}B	��B
  B	��B
 B
 �B
 �B
 �B
UB
B
GB
aB
�B
'B
 OB	�cB	��B	��B	�lB	�zB	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�rB	��B	��B	�B	�"B	��B	��B	��B
 B
'B
uB
-B
gB
�B
�B
�B
�B
�B
�B
:B
�B
hB
�B
[B
�B
�B
�B
sB
�B
�B
QB
�B
kB
QB
	B
�B
)B
�B
�B
IB
IB
~B
IB
B
IB
CB
)B
xB
�B
B
B
�B
jB
�B
B
�B
 \B
 \B
 B
�B
"B
#�B
#�B
#�B
#�B
$&B
$�B
$@B
#�B
#�B
#nB
"�B
#�B
%,B
#�B
#�B
#:B
#TB
#�B
#�B
#�B
#�B
#�B
$ZB
$tB
$�B
$�B
$�B
%`B
%�B
&B
&fB
&�B
&�B
&�B
&�B
&�B
'B
'8B
(>B
)DB
)�B
*B
*�B
+�B
1[B
6�B
7�B
8RB
:B
:�B
;B
<jB
<�B
<jB
<B
<�B
<PB
;�B
;B
:DB
<PB
<PB
;�B
<B
<PB
;B
:�B
:xB
:B
9rB
9�B
9>B
;JB
;B
=�B
>�B
=�B
>�B
=VB
;�B
;�B
;dB
9�B
9�B
9�B
9XB
9XB
9>B
9XB
;B
=�B
@4B
A�B
A�B
AoB
A�B
A�B
B�B
C{B
DB
D�B
DgB
D3B
C�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
FtB
FYB
FYB
F�B
G+B
IB
J�B
KxB
K�B
K�B
K�B
L�B
MjB
M�B
M�B
NpB
N�B
N�B
NpB
N�B
N�B
N<B
N<B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
P�B
Q4B
Q B
P�B
P�B
QNB
R�B
R�B
R�B
SuB
T�B
U�B
U�B
U�B
U�B
WYB
X+B
X�B
YeB
Y�B
ZkB
Z�B
[WB
\B
\]B
\�B
\�B
\�B
\�B
\xB
\)B
\B
\)B
\CB
\�B
]�B
^5B
^5B
]�B
]�B
^5B
^�B
_�B
_�B
`�B
aHB
`vB
`�B
aHB
a�B
bB
bB
b4B
b4B
b4B
bNB
a�B
a�B
a�B
b4B
b�B
c:B
cTB
d&B
e,B
dtB
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eFB
e`B
e`B
e�B
e�B
e�B
fB
f2B
f2B
ffB
ffB
ffB
fLB
f�B
ffB
gB
h�B
h�B
iDB
i�B
jKB
jKB
jB
k�B
l=B
l=B
lWB
l"B
k�B
mCB
mB
l�B
mCB
mB
m�B
m�B
m�B
n/B
n}B
m�B
m�B
mB
l�B
l�B
mCB
m�B
nB
n�B
o5B
o5B
oOB
o�B
oiB
n/B
o B
o�B
o�B
o�B
p�B
p�B
p�B
qvB
q�B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t9B
tB
tB
tnB
uB
u%B
vFB
vB
v+B
v`B
v`B
v�B
wB
wfB
x8B
xlB
x�B
x�B
x�B
y	B
y$B
y	B
y	B
x�B
x�B
y	B
y�B
z*B
z*B
zDB
zDB
z^B
z^B
zxB
z�B
z�B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
z�B
{0B
{�B
{�B
{�B
{�B
|B
|B
|B
|PB
|PB
|jB
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}qB
}�B
~B
}�B
}�B
}�B
~�B
~�B
.B
.B
.B
HB
}B
�B
�B
�B
�B
� B
�4B
�4B
�4B
�4B
�iB
�OB
�OB
�OB
�B
��B
�oB
�oB
��B
��B
��B
��B
�AB
�'B
�AB
�uB
��B
��B
��B
��B
��B
�aB
��B
��B
��B
�MB
��B
��B
��B
�SB
��B
�%B
�?B
�?B
�?B
�%B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�KB
��B
�B
�7B
�7B
�RB
��B
�#B
��B
�B
�)B
�^B
�^B
�xB
��B
��B
��B
�B
�B
�JB
��B
�B
�jB
��B
��B
�"B
�VB
�VB
�pB
��B
��B
��B
�(B
�vB
�\B
��B
�HB
�}B
��B
��B
� B
�B
�hB
��B
��B
� B
�B
�TB
��B
�&B
�@B
�uB
��B
�B
�FB
�,B
�,B
�,B
�,B
�,B
�{B
�aB
�{B
��B
��B
��B
�2B
��B
�B
�9B
��B
��B
��B
��B
��B
��B
�$B
�?B
�sB
��B
��B
��B
��B
�+B
�yB
��B
��B
��B
�KB
�eB
��B
��B
��B
��B
��B
��B
�7B
�7B
�QB
�QB
�kB
�kB
��B
��B
��B
�=B
��B
�)B
�)B
�]B
�xB
�xB
�xB
��B
�B
�IB
�IB
�IB
�IB
�IB
�dB
��B
��B
��B
��B
��B
�OB
�jB
��B
��B
��B
��B
�!B
�B
��B
��B
��B
��B
�'B
�'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�}B
�bB
�.B
�HB
�}B
��B
��B
��B
��B
��B
��B
�}B
�}B
��B
��B
�NB
�NB
��B
�hB
� B
�B
��B
��B
�,B
�{B
��B
�B
�2B
��B
��B
�gB
��B
��B
�SB
�EB
��B
�jB
��B
��B
��B
��BKB BB$�BKBX�BW�Bk�B��B��B�OB�-B�	B��B��B�BB��B��B�gB��B�JBΥB�VB�B��B��B��B�B�@B�BǔB��B�rB��B��B��B�9B�aB}�Be,Ba�B^�BT,BHKB8B$�B6B
�qB
�cB
�1B
ĶB
�B
�/B
��B
y�B
c�B
OBB
1vB
gB
�B	�B	ΥB	�*B	�|B	�xB	z�B	v�B	qvB	p�B	o5B	m�B	lB	cnB	P�B	EB	?}B	8B	7�B	E�B	FYB	>B	<PB	4�B	%B	�B	(
B	&LB	# B		�B�vB�B��B��B�B�JB�0B��B�B�>B��B�B��B՛B�JBƨB�SBʦB�B�B	eB	)_B	?cB	I7B	N�B	P�B	T�B	V�B	X�B	WsB	VB	U�B	Y1B	[qB	[qB	[=B	[�B	[=B	[�B	[	B	^�B	g�B	r�B	vzB	zDB	��B	��B	��B	��B	��B	�_B	�B	��B	�\B	�)B	�QB	��B	��B	�B	�pB	��B	��B	��B	��B	�[B	�B	{�B	z�B	z�B	z�B	cB	�KB	��B	�oB	~BB	��B	�"B	��B	~BB	qvB	nIB	l�B	m�B	q'B	v+B	yrB	|PB	~�B	�oB	�tB	��B	��B	��B	�]B	��B	��B	�JB	�B	�tB	�B	|PB	zxB	zxB	yXB	z�B	z^B	|B	�B	�gB	��B	��B	��B	�B	��B	�SB	��B	�KB	��B	��B	��B	��B	�=B	��B	��B	�jB	�/B	��B	��B	�4B	�&B	��B	�OB	��B	�-B	�'B	�[B	��B	�B	��B	��B	�;B	�-B	��B	�?B	��B	��B	�[B	�B	��B	�[B	�B	��B	ĶB	��B	��B	�_B	ǮB	ȀB	��B	��B	�VB	�B	�HB	ѝB	҉B	�[B	��B	��B	��B	�WB	�~B	ߤB	�B	�B	�B	�0B	�6B	�WB	�B	�"B	�B	�B	�wB	�CB	��B	�oB	��B	�aB	��B	��B	��B	��B	��B	��B	�fB	�B	��B	��B	�>B	��B	��B	��B	�DB	�0B	��B	�dB	�B	�qB	�VB	�}B	��B
  B	��B
 B
 �B
 �B
 �B
UB
B
GB
aB
�B
'B
 OB	�cB	��B	��B	�lB	�zB	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�rB	��B	��B	�B	�"B	��B	��B	��B
 B
'B
uB
-B
gB
�B
�B
�B
�B
�B
�B
:B
�B
hB
�B
[B
�B
�B
�B
sB
�B
�B
QB
�B
kB
QB
	B
�B
)B
�B
�B
IB
IB
~B
IB
B
IB
CB
)B
xB
�B
B
B
�B
jB
�B
B
�B
 \B
 \B
 B
�B
"B
#�B
#�B
#�B
#�B
$&B
$�B
$@B
#�B
#�B
#nB
"�B
#�B
%,B
#�B
#�B
#:B
#TB
#�B
#�B
#�B
#�B
#�B
$ZB
$tB
$�B
$�B
$�B
%`B
%�B
&B
&fB
&�B
&�B
&�B
&�B
&�B
'B
'8B
(>B
)DB
)�B
*B
*�B
+�B
1[B
6�B
7�B
8RB
:B
:�B
;B
<jB
<�B
<jB
<B
<�B
<PB
;�B
;B
:DB
<PB
<PB
;�B
<B
<PB
;B
:�B
:xB
:B
9rB
9�B
9>B
;JB
;B
=�B
>�B
=�B
>�B
=VB
;�B
;�B
;dB
9�B
9�B
9�B
9XB
9XB
9>B
9XB
;B
=�B
@4B
A�B
A�B
AoB
A�B
A�B
B�B
C{B
DB
D�B
DgB
D3B
C�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
FtB
FYB
FYB
F�B
G+B
IB
J�B
KxB
K�B
K�B
K�B
L�B
MjB
M�B
M�B
NpB
N�B
N�B
NpB
N�B
N�B
N<B
N<B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
P�B
Q4B
Q B
P�B
P�B
QNB
R�B
R�B
R�B
SuB
T�B
U�B
U�B
U�B
U�B
WYB
X+B
X�B
YeB
Y�B
ZkB
Z�B
[WB
\B
\]B
\�B
\�B
\�B
\�B
\xB
\)B
\B
\)B
\CB
\�B
]�B
^5B
^5B
]�B
]�B
^5B
^�B
_�B
_�B
`�B
aHB
`vB
`�B
aHB
a�B
bB
bB
b4B
b4B
b4B
bNB
a�B
a�B
a�B
b4B
b�B
c:B
cTB
d&B
e,B
dtB
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eFB
e`B
e`B
e�B
e�B
e�B
fB
f2B
f2B
ffB
ffB
ffB
fLB
f�B
ffB
gB
h�B
h�B
iDB
i�B
jKB
jKB
jB
k�B
l=B
l=B
lWB
l"B
k�B
mCB
mB
l�B
mCB
mB
m�B
m�B
m�B
n/B
n}B
m�B
m�B
mB
l�B
l�B
mCB
m�B
nB
n�B
o5B
o5B
oOB
o�B
oiB
n/B
o B
o�B
o�B
o�B
p�B
p�B
p�B
qvB
q�B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t9B
tB
tB
tnB
uB
u%B
vFB
vB
v+B
v`B
v`B
v�B
wB
wfB
x8B
xlB
x�B
x�B
x�B
y	B
y$B
y	B
y	B
x�B
x�B
y	B
y�B
z*B
z*B
zDB
zDB
z^B
z^B
zxB
z�B
z�B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
z�B
{0B
{�B
{�B
{�B
{�B
|B
|B
|B
|PB
|PB
|jB
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}qB
}�B
~B
}�B
}�B
}�B
~�B
~�B
.B
.B
.B
HB
}B
�B
�B
�B
�B
� B
�4B
�4B
�4B
�4B
�iB
�OB
�OB
�OB
�B
��B
�oB
�oB
��B
��B
��B
��B
�AB
�'B
�AB
�uB
��B
��B
��B
��B
��B
�aB
��B
��B
��B
�MB
��B
��B
��B
�SB
��B
�%B
�?B
�?B
�?B
�%B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�KB
��B
�B
�7B
�7B
�RB
��B
�#B
��B
�B
�)B
�^B
�^B
�xB
��B
��B
��B
�B
�B
�JB
��B
�B
�jB
��B
��B
�"B
�VB
�VB
�pB
��B
��B
��B
�(B
�vB
�\B
��B
�HB
�}B
��B
��B
� B
�B
�hB
��B
��B
� B
�B
�TB
��B
�&B
�@B
�uB
��B
�B
�FB
�,B
�,B
�,B
�,B
�,B
�{B
�aB
�{B
��B
��B
��B
�2B
��B
�B
�9B
��B
��B
��B
��B
��B
��B
�$B
�?B
�sB
��B
��B
��B
��B
�+B
�yB
��B
��B
��B
�KB
�eB
��B
��B
��B
��B
��B
��B
�7B
�7B
�QB
�QB
�kB
�kB
��B
��B
��B
�=B
��B
�)B
�)B
�]B
�xB
�xB
�xB
��B
�B
�IB
�IB
�IB
�IB
�IB
�dB
��B
��B
��B
��B
��B
�OB
�jB
��B
��B
��B
��B
�!B
�B
��B
��B
��B
��B
�'B
�'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230623094342  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230623094350  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230623094351  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230623094351                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230623094352  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230623094352  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230623095839                      G�O�G�O�G�O�                
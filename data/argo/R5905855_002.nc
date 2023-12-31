CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:10:49Z creation;2022-06-04T19:10:50Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20220604191049  20220610151507  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @تs�d��1   @تs����@/Ձ$�/�d�n��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  Aa��A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�ffB���B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C�C�C  C�C�C�fC"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @G�@~{@�
=@�
=A�A?�Aa�A�A��\A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B�W
B�#�B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B�W
B��B��B�W
B۽qB��B��B��B��B�qB��B��B��B��C�RC�RC�RC޸C	�RC�RC�RC�RC�RC�RC�C�C�RC�C�C޸C!�RC#�RC%�RC'�RC)޸C+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCB�CD�CE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&�zD'zD'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>w�D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT�zDT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
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
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
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
D�{�Dʿ
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
D�+�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AɟVAɞAɟVAɥ�Aɟ�Aɜ�Aɝ~Aɝ~AɝIAɝ�Aɟ!Aɢ�Aɢ�AɥAɹ$A���A���A��aAɷ�Aɤ�AɌ�A�y>A�V�A��AȿA�v�A�kQAȓ@AȨ$A��'A�"A�H�AɂuA�>�A��A�{A�Y�A�1A�g�A�ɺAˤ�A�(�A�1'A�>A���A�bAɾBA���AǍ�A�_AŗYAş�A�,�Aē�A�>A���A���A�&LA�aHA�FtA�!-A��A�4A�$�A��fA��A��A��}A�<jA���A��A��A���A���A�<�A��A�%A�4�A��A�(A�\�A�9�A��LA�ݘA�ԕA�b�A~ƨAy�YAv�Ap��AlGAj�9Aj{�Aj(�Ag�Ac$�A`=A^�AA]U2A[0�AYAV�AS{AN\�AM�AK�qAJqvAH��AF�AD��AC2�AAA>��A;oA9�A8��A7�PA7�nA7�A7FtA5�jA5w2A5*0A5L0A4�A4��A4GA1ݘA0K^A/ �A.�~A.��A..IA-
=A,_A*��A)�jA*~�A*�BA+�A*~A)^5A(��A'��A'N<A&�A$��A%%�A%�A#�A"�pA!��A!8�A �A x�A �A�A�PA:�A	�A��A��Ay�A��AE9A�dA��A-�A�A�AS�A�uAqvAT�A�A��Au�Ac AHAl�A=�A��AK�AjA,�A�A�A^5A��A�EA@OA�^A�cAkQA�AD�AK�A-A�A
�A
�!A	�XA	6�A�xA&�A��A�A�AA��A �A��AaA;dAl"AS&A�9AJ�A �jA ��A b@���@�S&@�S�@�a�@�~�@�0U@��@��@���@�9�@���@��4@��4@���@��W@��@��>@�h
@�;d@�xl@��@�=q@�&�@��d@��@맇@�5?@��@�ff@�h@�6@�'R@��@���@�i�@��@�9�@�W�@�7L@��v@߮�@��K@�7�@���@ݣn@�(�@�I�@��@�x@�#�@�	@�x�@��@�*0@�ی@�e�@֋D@փ@�a|@�3�@���@�ȴ@��@ѧ�@ч�@љ�@�iD@�6z@��@е�@Ы6@�kQ@�2�@σ{@�6z@��v@�%�@���@��m@͹�@�hs@�Ĝ@�S�@�4@�o�@ʗ�@���@ɴ�@�@�S�@�@��z@�l�@��@�J@ŵt@�/@��@�6�@��A@ÖS@�/@�ѷ@�q@��@�\�@�'�@���@���@�˒@�<6@���@���@�6@�0�@�S�@�K�@� \@�m�@���@��@���@�P�@��x@��N@�:�@���@���@�P�@�(@���@��L@���@��o@�1@�ϫ@��y@��Z@���@�c@�C�@��@�@��R@��@�i�@�\�@�ϫ@���@�4�@�c�@��@�\�@�dZ@�K�@��<@�r�@�\�@���@�qv@�j@��Y@��T@���@���@���@�X@��@��/@��+@�=q@�	�@��h@�>�@�>�@��@��@���@��j@�� @�R�@��@���@�c�@��[@�($@��[@��@��@�1@��@��4@�:�@��M@���@�	l@�ں@��@�$@�^�@��v@���@���@�YK@�M@��=@�x@�b�@��@��]@��u@�Q�@��@���@�N<@��@��@�tT@�kQ@�I�@��D@��@@��@�ی@��@�m�@�O@��@��@��@��@��+@�Ov@�e@��+@���@��3@�u�@��@��@���@�-@��@���@���@���@�t�@�,�@��!@�5?@��W@���@�O@��@��x@�Xy@��@���@�<6@��@��@���@��F@���@�oi@�8�@�#:@��.@���@���@�{J@�e,@�C@���@�u�@���@��&@��@���@�n/@��@��O@�}V@�i�@�1'@���@�x@�5�@�V@��M@��2@�҉@���@��F@��Y@�w�@�a|@�6@� �@���@���@���@��@�H�@�@��`@�s�@�x@��@��@�w2@��@��2@���@���@��@�H@�O@��T@���@��n@��	@�&�@���@��e@�B[@��.@��^@�t�@��@��@���@���@��@�l"@�A�@�<�@��@��@�@~��@~Q@}��@}?}@}�@|�@|��@|ѷ@|��@{��@{U�@z�y@zE�@y��@y@y4@x�4@xQ�@x2�@x(�@x�@w�A@w�m@we�@v�y@vTa@u�Z@u�j@u�=@u[W@t��@tK^@s�Q@s�0@sU�@r�}@q��@qG�@p�@p�@p6@o��@o�$@o'�@n�@n^5@m�@mm]@m+�@m	l@l�@l�j@l|�@l6@k�@k=@j�m@j6�@i�@i&�@h��@hH@h/�@h�@h�@g��@g�V@g@f��@fV@f@ew2@e*0@d��@d<�@c�a@cx@c.I@b҉@b�1@b5?@a��@`�@`q@`Q�@`?�@`7�@`"h@_�&@_ƨ@_��@^�y@^Ov@^�@]�@]��@]S&@\�f@\��@\��@\��@\Q�@\!@[�@[W?@Z�B@Z�@Z;�@Y��@Y7L@X��@X��@X"h@W�@W��@W
=@Vz@Ve@V@U�j@Up�@T��@TQ�@S�0@SMj@S
=@R�'@R^5@R�@Q��@Q	l@P`�@O�r@O��@Oe�@ORT@O
=@N@�@M�@Mu�@M(�@Mq@M�@L��@L�E@L��@LtT@KJ#@J�2@J��@I�Z@IrG@H��@HI�@G��@G�@F�\@Fff@F��@F�@E��@EIR@D�|@Dѷ@D�5@D��@Dy>@C�W@C��@C]�@B�@B�h@B�r@Bc @B;�@Bu@A�z@A�n@Ahs@A(�@A�@@�)@@bN@?��@?��@?�V@?��@?�@>n�@>=q@>�@=�T@=�@=e,@=B�@=�@<�?@<V�@;�{@;4�@;�@;�@:�]@:u%@:$�@9�@9�@9c@9j@8ѷ@8y>@87�@7�m@7��@7�@6�H@6n�@64@5�@5�=@5��@5J�@5*0@4��@4ѷ@4��@4[�@3�@3��@3��@3ƨ@3�f@3|�@3;d@2�@2?@1��@1�N@1��@1��@1N<@0�|@0�@0�I@0M@/�W@/�q@/F�@.��@.q�@.!�@-�D@-��@-�=@-�M@-O�@,��@,�?@,tT@,Ft@,7@+��@+�W@+s@*��@*��@*�6@*ff@*$�@)��@)�@)|@)!�@(��@(��@(��@(�@(g8@([�@'��@'�@'b�@''�@'�@&�@&�!@&�}@&��@&�@&$�@%c�@%X@%4@$��@$ѷ@$Ĝ@$�?@$�@$I�@$'R@#�@#��@#��@#j�@#F�@#o@"�2@"��@"�b@"�A@"a|@"#:@!�o@!��@!�H@!�^@!u�@!X@!c�@!O�@!4@ �5@ �u@ A�@��@��@s@C�@��@�@Q@_@�>@�3@��@4@��@�I@q@PH@(�@��@33@�@�@�!@�A@p;@Ta@:*@	@��@��@rG@!�@�p@��@bN@A�@ �@�@�}@�@�k@iD@]�@P�@�@�<@�@@�D@��@��@e,@F@(�@�@�p@�@bN@(�@�&@�@@v`@F�@�@�@��@�R@xl@;�@0U@e@�D@�j@�9@�^@�X@��@w2@Q�@<6@%@�E@Ĝ@�9@�@��@_@1'@�@�@�@v`@g�@]�@P�@!-@�@� @c @Ta@GE@B[@1�@��@�j@�t@zx@Q�@5�@(�@@;@��@�.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AɟVAɞAɟVAɥ�Aɟ�Aɜ�Aɝ~Aɝ~AɝIAɝ�Aɟ!Aɢ�Aɢ�AɥAɹ$A���A���A��aAɷ�Aɤ�AɌ�A�y>A�V�A��AȿA�v�A�kQAȓ@AȨ$A��'A�"A�H�AɂuA�>�A��A�{A�Y�A�1A�g�A�ɺAˤ�A�(�A�1'A�>A���A�bAɾBA���AǍ�A�_AŗYAş�A�,�Aē�A�>A���A���A�&LA�aHA�FtA�!-A��A�4A�$�A��fA��A��A��}A�<jA���A��A��A���A���A�<�A��A�%A�4�A��A�(A�\�A�9�A��LA�ݘA�ԕA�b�A~ƨAy�YAv�Ap��AlGAj�9Aj{�Aj(�Ag�Ac$�A`=A^�AA]U2A[0�AYAV�AS{AN\�AM�AK�qAJqvAH��AF�AD��AC2�AAA>��A;oA9�A8��A7�PA7�nA7�A7FtA5�jA5w2A5*0A5L0A4�A4��A4GA1ݘA0K^A/ �A.�~A.��A..IA-
=A,_A*��A)�jA*~�A*�BA+�A*~A)^5A(��A'��A'N<A&�A$��A%%�A%�A#�A"�pA!��A!8�A �A x�A �A�A�PA:�A	�A��A��Ay�A��AE9A�dA��A-�A�A�AS�A�uAqvAT�A�A��Au�Ac AHAl�A=�A��AK�AjA,�A�A�A^5A��A�EA@OA�^A�cAkQA�AD�AK�A-A�A
�A
�!A	�XA	6�A�xA&�A��A�A�AA��A �A��AaA;dAl"AS&A�9AJ�A �jA ��A b@���@�S&@�S�@�a�@�~�@�0U@��@��@���@�9�@���@��4@��4@���@��W@��@��>@�h
@�;d@�xl@��@�=q@�&�@��d@��@맇@�5?@��@�ff@�h@�6@�'R@��@���@�i�@��@�9�@�W�@�7L@��v@߮�@��K@�7�@���@ݣn@�(�@�I�@��@�x@�#�@�	@�x�@��@�*0@�ی@�e�@֋D@փ@�a|@�3�@���@�ȴ@��@ѧ�@ч�@љ�@�iD@�6z@��@е�@Ы6@�kQ@�2�@σ{@�6z@��v@�%�@���@��m@͹�@�hs@�Ĝ@�S�@�4@�o�@ʗ�@���@ɴ�@�@�S�@�@��z@�l�@��@�J@ŵt@�/@��@�6�@��A@ÖS@�/@�ѷ@�q@��@�\�@�'�@���@���@�˒@�<6@���@���@�6@�0�@�S�@�K�@� \@�m�@���@��@���@�P�@��x@��N@�:�@���@���@�P�@�(@���@��L@���@��o@�1@�ϫ@��y@��Z@���@�c@�C�@��@�@��R@��@�i�@�\�@�ϫ@���@�4�@�c�@��@�\�@�dZ@�K�@��<@�r�@�\�@���@�qv@�j@��Y@��T@���@���@���@�X@��@��/@��+@�=q@�	�@��h@�>�@�>�@��@��@���@��j@�� @�R�@��@���@�c�@��[@�($@��[@��@��@�1@��@��4@�:�@��M@���@�	l@�ں@��@�$@�^�@��v@���@���@�YK@�M@��=@�x@�b�@��@��]@��u@�Q�@��@���@�N<@��@��@�tT@�kQ@�I�@��D@��@@��@�ی@��@�m�@�O@��@��@��@��@��+@�Ov@�e@��+@���@��3@�u�@��@��@���@�-@��@���@���@���@�t�@�,�@��!@�5?@��W@���@�O@��@��x@�Xy@��@���@�<6@��@��@���@��F@���@�oi@�8�@�#:@��.@���@���@�{J@�e,@�C@���@�u�@���@��&@��@���@�n/@��@��O@�}V@�i�@�1'@���@�x@�5�@�V@��M@��2@�҉@���@��F@��Y@�w�@�a|@�6@� �@���@���@���@��@�H�@�@��`@�s�@�x@��@��@�w2@��@��2@���@���@��@�H@�O@��T@���@��n@��	@�&�@���@��e@�B[@��.@��^@�t�@��@��@���@���@��@�l"@�A�@�<�@��@��@�@~��@~Q@}��@}?}@}�@|�@|��@|ѷ@|��@{��@{U�@z�y@zE�@y��@y@y4@x�4@xQ�@x2�@x(�@x�@w�A@w�m@we�@v�y@vTa@u�Z@u�j@u�=@u[W@t��@tK^@s�Q@s�0@sU�@r�}@q��@qG�@p�@p�@p6@o��@o�$@o'�@n�@n^5@m�@mm]@m+�@m	l@l�@l�j@l|�@l6@k�@k=@j�m@j6�@i�@i&�@h��@hH@h/�@h�@h�@g��@g�V@g@f��@fV@f@ew2@e*0@d��@d<�@c�a@cx@c.I@b҉@b�1@b5?@a��@`�@`q@`Q�@`?�@`7�@`"h@_�&@_ƨ@_��@^�y@^Ov@^�@]�@]��@]S&@\�f@\��@\��@\��@\Q�@\!@[�@[W?@Z�B@Z�@Z;�@Y��@Y7L@X��@X��@X"h@W�@W��@W
=@Vz@Ve@V@U�j@Up�@T��@TQ�@S�0@SMj@S
=@R�'@R^5@R�@Q��@Q	l@P`�@O�r@O��@Oe�@ORT@O
=@N@�@M�@Mu�@M(�@Mq@M�@L��@L�E@L��@LtT@KJ#@J�2@J��@I�Z@IrG@H��@HI�@G��@G�@F�\@Fff@F��@F�@E��@EIR@D�|@Dѷ@D�5@D��@Dy>@C�W@C��@C]�@B�@B�h@B�r@Bc @B;�@Bu@A�z@A�n@Ahs@A(�@A�@@�)@@bN@?��@?��@?�V@?��@?�@>n�@>=q@>�@=�T@=�@=e,@=B�@=�@<�?@<V�@;�{@;4�@;�@;�@:�]@:u%@:$�@9�@9�@9c@9j@8ѷ@8y>@87�@7�m@7��@7�@6�H@6n�@64@5�@5�=@5��@5J�@5*0@4��@4ѷ@4��@4[�@3�@3��@3��@3ƨ@3�f@3|�@3;d@2�@2?@1��@1�N@1��@1��@1N<@0�|@0�@0�I@0M@/�W@/�q@/F�@.��@.q�@.!�@-�D@-��@-�=@-�M@-O�@,��@,�?@,tT@,Ft@,7@+��@+�W@+s@*��@*��@*�6@*ff@*$�@)��@)�@)|@)!�@(��@(��@(��@(�@(g8@([�@'��@'�@'b�@''�@'�@&�@&�!@&�}@&��@&�@&$�@%c�@%X@%4@$��@$ѷ@$Ĝ@$�?@$�@$I�@$'R@#�@#��@#��@#j�@#F�@#o@"�2@"��@"�b@"�A@"a|@"#:@!�o@!��@!�H@!�^@!u�@!X@!c�@!O�@!4@ �5@ �u@ A�@��@��@s@C�@��@�@Q@_@�>@�3@��@4@��@�I@q@PH@(�@��@33@�@�@�!@�A@p;@Ta@:*@	@��@��@rG@!�@�p@��@bN@A�@ �@�@�}@�@�k@iD@]�@P�@�@�<@�@@�D@��@��@e,@F@(�@�@�p@�@bN@(�@�&@�@@v`@F�@�@�@��@�R@xl@;�@0U@e@�D@�j@�9@�^@�X@��@w2@Q�@<6@%@�E@Ĝ@�9@�@��@_@1'@�@�@�@v`@g�@]�@P�@!-@�@� @c @Ta@GE@B[@1�@��@�j@�t@zx@Q�@5�@(�@@;@��@�.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BX�BX�BX�BX�BX�BXyBX�BX�BX�BYB[	B^B]/B^Bq'B|�B��B�B��B�AB��B��BٚB�B��B��B��B	�B	B	9B	.�B	FB	f�B	k�B	mwB	��B	�}B
�B
@iB
p�B
w�B
v�B
�zB
�,B
��B
��B
ߊB
֡B
�WB?B��B��B�!BҽB�.BVBKB!�B�B�aB�B(XB+�B�B��B�B��B?B��B��B�2B՛B�WB�B��Bz�BS�BB
�B
zxB	��B	��B	��B	�oB	�GB	��B	�vB	.B	oB	a�B	T�B	[qB	\B	\]B	U�B	CGB	0�B	*KB	"B	�B	(B	GB��B	�B�B��B��B��B�B��B��B�2BԯB�B�{B�uB�mBуB�OB	$B	�B	kB	%�B	*eB	,qB	2�B	7LB	4�B	/�B	+�B	+�B	/�B	.}B	'B	!�B	�B	?B	5%B	C-B	R�B	W�B	X�B	[WB	o�B	z�B	v�B	}<B	��B	��B	��B	��B	�7B	��B	�rB	�=B	�B	֡B	��B	�B	�B	�"B	��B	�cB	�B	��B	�!B	�B	�/B	�B	�B	�QB	�B	��B	��B	ݘB	�jB	��B	�!B	ߤB	�hB	��B	�TB	�,B	��B	�hB	�hB	�B	��B	��B	�-B	��B	�B	�@B	��B	�mB	�>B	�B	�B	�B	�RB	��B	�B	��B	�B	�B	�NB	�|B	�B	�B	�QB	��B	ՁB	�B	��B	�?B	��B	��B	�	B	��B	��B	�B	��B	��B	��B	�fB	��B	��B	�>B	��B	��B	�(B	�oB	�MB	ĶB	��B	�	B	��B	�B	ƨB	�YB	�mB	�oB	��B	�}B	��B	��B	��B	�B	��B	�.B	�oB	�'B	��B	�1B	�xB	�DB	��B	�\B	�"B	͹B	οB	��B	��B	��B	��B	�hB	��B	�TB	��B	ѝB	�B	��B	οB	̳B	��B	�~B	��B	�bB	��B	�(B	�B	��B	��B	��B	�}B	�oB	�,B	ԯB	��B	�2B	��B	ּB	רB	�B	�B	��B	�B	�B	�7B	�B	�B	ںB	��B	��B	��B	ۦB	ۦB	یB	�qB	��B	ݲB	߾B	��B	��B	�B	�B	��B	�;B	�;B	��B	�|B	�|B	�B	�ZB	��B	�B	�B	��B	�B	�B	�B	��B	�[B	�B	��B	�B	�B	�nB	��B	�B	�ZB	�%B	��B	�%B	�B	��B	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�9B	�B	�zB	�fB	��B	��B	�>B	��B	��B	�RB	�RB	��B	��B	�FB	�ZB	�%B	�2B	�B	�B	��B	�lB	��B	��B	�dB	�DB	��B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	�jB	��B	��B	��B
 �B
 �B
oB
;B
oB
[B
�B
oB
;B
 �B
  B
 OB	��B	��B	�BB	��B	�B	��B
�B
�B
�B
gB
MB
�B
SB
�B
�B
�B
%B
�B
�B
�B
_B
�B
	�B
	�B

�B
^B

�B

XB
	�B

	B

XB

	B

#B

�B

�B

rB

�B

�B
B
�B
dB
�B
�B
�B
�B
B
~B
�B
PB
"B
�B
(B
�B
B
�B
4B
�B
�B
�B
�B
�B
�B
�B
�B
@B
aB
�B
{B
�B
B
�B
�B
YB
�B
�B
yB
�B
�B
B
1B
KB
eB
�B
B
eB
�B
KB
B
7B
B
B
7B
�B
qB
�B
WB
�B
B
�B
B
�B
B
OB
�B
�B
!B
!B
VB
;B
�B
�B
 B
 B
 B
�B
�B
 'B
 BB
!bB
!�B
!�B
"NB
"�B
#�B
#�B
$B
$B
$tB
%zB
%�B
&fB
&�B
&�B
'RB
($B
(>B
(�B
)�B
)�B
*0B
*�B
*�B
*�B
*�B
+B
*�B
*�B
+6B
+�B
,qB
,=B
,�B
-B
-)B
-]B
,�B
-CB
-�B
./B
/OB
/5B
.�B
/�B
0�B
0oB
0�B
0�B
0�B
0�B
1B
1[B
1�B
1�B
1�B
1�B
2GB
2GB
3MB
3�B
4B
4TB
4nB
49B
49B
4�B
4�B
5B
5tB
5�B
5�B
5�B
5�B
6B
6`B
6�B
72B
7�B
88B
9$B
:B
:*B
:^B
:xB
:�B
:�B
:�B
:�B
;B
;�B
;JB
<�B
=B
=<B
=<B
="B
="B
="B
<�B
=qB
>B
>]B
>wB
>BB
>(B
=�B
=qB
>B
?HB
?�B
?�B
?�B
@ B
@4B
@�B
@4B
@OB
@OB
@OB
A B
A�B
A�B
A�B
A�B
B'B
B�B
B�B
B�B
CB
CaB
CaB
CGB
CGB
CaB
C�B
C�B
C�B
C�B
D�B
D�B
EmB
FB
FB
FtB
FtB
F�B
G+B
GEB
G�B
HKB
H�B
H�B
H�B
H�B
IlB
I�B
I�B
J#B
JXB
JXB
J�B
J�B
J�B
KDB
K�B
K�B
L0B
LB
LB
LdB
MB
L�B
M6B
MB
MB
M6B
M6B
MjB
M�B
M�B
M�B
M�B
NB
M�B
MPB
MjB
M�B
MPB
LdB
MPB
MjB
NVB
N"B
MjB
M�B
M�B
NpB
O\B
O�B
O�B
O�B
O�B
PbB
PbB
PbB
PHB
P}B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
R�B
R�B
S@B
S@B
S[B
S[B
TB
TB
TFB
TaB
T�B
T�B
U2B
VB
VSB
VSB
VmB
V�B
V�B
W$B
WYB
WYB
WsB
WsB
X+B
XEB
X_B
X�B
X�B
YeB
YB
Y�B
Y�B
Z7B
Z7B
Z7B
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[WB
[�B
[�B
[�B
\)B
\CB
\�B
\�B
]~B
]dB
]~B
]�B
]�B
^B
^OB
^5B
^jB
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
`\B
`BB
`\B
`vB
`�B
`�B
aHB
abB
a�B
a�B
a|B
a�B
b4B
b4B
b4B
bhB
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
d�B
d�B
d�B
d�B
d�B
eB
d�B
d�B
e`B
f2B
fB
fLB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h>B
hsB
h�B
h�B
h�B
h�B
iB
iB
i*B
iB
i_B
i�B
i�B
i�B
jKB
j0B
jeB
j�B
j�B
k6B
k�B
k�B
k�B
k�B
lqB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
nB
n/B
nIB
ncB
ncB
n�B
n�B
n�B
o B
n�B
n�B
o B
oB
oOB
o�B
p;B
p�B
p�B
p�B
qAB
qvB
q�B
rB
rGB
r-B
raB
r�B
r�B
sB
s3B
s3B
sMB
shB
s�B
s�B
s�B
t9B
tnB
t�B
t�B
t�B
t�B
uB
uB
uZB
u�B
utB
u�B
u�B
u�B
u�B
u�B
vB
u�B
vB
v+B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
wLB
w�B
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y>B
yrB
y�B
y�B
y�B
y�B
zB
zB
zxB
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BX�BX�BX�BX�BX�BXyBX�BX�BX�BYB[	B^B]/B^Bq'B|�B��B�B��B�AB��B��BٚB�B��B��B��B	�B	B	9B	.�B	FB	f�B	k�B	mwB	��B	�}B
�B
@iB
p�B
w�B
v�B
�zB
�,B
��B
��B
ߊB
֡B
�WB?B��B��B�!BҽB�.BVBKB!�B�B�aB�B(XB+�B�B��B�B��B?B��B��B�2B՛B�WB�B��Bz�BS�BB
�B
zxB	��B	��B	��B	�oB	�GB	��B	�vB	.B	oB	a�B	T�B	[qB	\B	\]B	U�B	CGB	0�B	*KB	"B	�B	(B	GB��B	�B�B��B��B��B�B��B��B�2BԯB�B�{B�uB�mBуB�OB	$B	�B	kB	%�B	*eB	,qB	2�B	7LB	4�B	/�B	+�B	+�B	/�B	.}B	'B	!�B	�B	?B	5%B	C-B	R�B	W�B	X�B	[WB	o�B	z�B	v�B	}<B	��B	��B	��B	��B	�7B	��B	�rB	�=B	�B	֡B	��B	�B	�B	�"B	��B	�cB	�B	��B	�!B	�B	�/B	�B	�B	�QB	�B	��B	��B	ݘB	�jB	��B	�!B	ߤB	�hB	��B	�TB	�,B	��B	�hB	�hB	�B	��B	��B	�-B	��B	�B	�@B	��B	�mB	�>B	�B	�B	�B	�RB	��B	�B	��B	�B	�B	�NB	�|B	�B	�B	�QB	��B	ՁB	�B	��B	�?B	��B	��B	�	B	��B	��B	�B	��B	��B	��B	�fB	��B	��B	�>B	��B	��B	�(B	�oB	�MB	ĶB	��B	�	B	��B	�B	ƨB	�YB	�mB	�oB	��B	�}B	��B	��B	��B	�B	��B	�.B	�oB	�'B	��B	�1B	�xB	�DB	��B	�\B	�"B	͹B	οB	��B	��B	��B	��B	�hB	��B	�TB	��B	ѝB	�B	��B	οB	̳B	��B	�~B	��B	�bB	��B	�(B	�B	��B	��B	��B	�}B	�oB	�,B	ԯB	��B	�2B	��B	ּB	רB	�B	�B	��B	�B	�B	�7B	�B	�B	ںB	��B	��B	��B	ۦB	ۦB	یB	�qB	��B	ݲB	߾B	��B	��B	�B	�B	��B	�;B	�;B	��B	�|B	�|B	�B	�ZB	��B	�B	�B	��B	�B	�B	�B	��B	�[B	�B	��B	�B	�B	�nB	��B	�B	�ZB	�%B	��B	�%B	�B	��B	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�9B	�B	�zB	�fB	��B	��B	�>B	��B	��B	�RB	�RB	��B	��B	�FB	�ZB	�%B	�2B	�B	�B	��B	�lB	��B	��B	�dB	�DB	��B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	�jB	��B	��B	��B
 �B
 �B
oB
;B
oB
[B
�B
oB
;B
 �B
  B
 OB	��B	��B	�BB	��B	�B	��B
�B
�B
�B
gB
MB
�B
SB
�B
�B
�B
%B
�B
�B
�B
_B
�B
	�B
	�B

�B
^B

�B

XB
	�B

	B

XB

	B

#B

�B

�B

rB

�B

�B
B
�B
dB
�B
�B
�B
�B
B
~B
�B
PB
"B
�B
(B
�B
B
�B
4B
�B
�B
�B
�B
�B
�B
�B
�B
@B
aB
�B
{B
�B
B
�B
�B
YB
�B
�B
yB
�B
�B
B
1B
KB
eB
�B
B
eB
�B
KB
B
7B
B
B
7B
�B
qB
�B
WB
�B
B
�B
B
�B
B
OB
�B
�B
!B
!B
VB
;B
�B
�B
 B
 B
 B
�B
�B
 'B
 BB
!bB
!�B
!�B
"NB
"�B
#�B
#�B
$B
$B
$tB
%zB
%�B
&fB
&�B
&�B
'RB
($B
(>B
(�B
)�B
)�B
*0B
*�B
*�B
*�B
*�B
+B
*�B
*�B
+6B
+�B
,qB
,=B
,�B
-B
-)B
-]B
,�B
-CB
-�B
./B
/OB
/5B
.�B
/�B
0�B
0oB
0�B
0�B
0�B
0�B
1B
1[B
1�B
1�B
1�B
1�B
2GB
2GB
3MB
3�B
4B
4TB
4nB
49B
49B
4�B
4�B
5B
5tB
5�B
5�B
5�B
5�B
6B
6`B
6�B
72B
7�B
88B
9$B
:B
:*B
:^B
:xB
:�B
:�B
:�B
:�B
;B
;�B
;JB
<�B
=B
=<B
=<B
="B
="B
="B
<�B
=qB
>B
>]B
>wB
>BB
>(B
=�B
=qB
>B
?HB
?�B
?�B
?�B
@ B
@4B
@�B
@4B
@OB
@OB
@OB
A B
A�B
A�B
A�B
A�B
B'B
B�B
B�B
B�B
CB
CaB
CaB
CGB
CGB
CaB
C�B
C�B
C�B
C�B
D�B
D�B
EmB
FB
FB
FtB
FtB
F�B
G+B
GEB
G�B
HKB
H�B
H�B
H�B
H�B
IlB
I�B
I�B
J#B
JXB
JXB
J�B
J�B
J�B
KDB
K�B
K�B
L0B
LB
LB
LdB
MB
L�B
M6B
MB
MB
M6B
M6B
MjB
M�B
M�B
M�B
M�B
NB
M�B
MPB
MjB
M�B
MPB
LdB
MPB
MjB
NVB
N"B
MjB
M�B
M�B
NpB
O\B
O�B
O�B
O�B
O�B
PbB
PbB
PbB
PHB
P}B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
R�B
R�B
S@B
S@B
S[B
S[B
TB
TB
TFB
TaB
T�B
T�B
U2B
VB
VSB
VSB
VmB
V�B
V�B
W$B
WYB
WYB
WsB
WsB
X+B
XEB
X_B
X�B
X�B
YeB
YB
Y�B
Y�B
Z7B
Z7B
Z7B
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[WB
[�B
[�B
[�B
\)B
\CB
\�B
\�B
]~B
]dB
]~B
]�B
]�B
^B
^OB
^5B
^jB
^�B
^�B
_B
_VB
_�B
_�B
_�B
_�B
`\B
`BB
`\B
`vB
`�B
`�B
aHB
abB
a�B
a�B
a|B
a�B
b4B
b4B
b4B
bhB
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
d�B
d�B
d�B
d�B
d�B
eB
d�B
d�B
e`B
f2B
fB
fLB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h>B
hsB
h�B
h�B
h�B
h�B
iB
iB
i*B
iB
i_B
i�B
i�B
i�B
jKB
j0B
jeB
j�B
j�B
k6B
k�B
k�B
k�B
k�B
lqB
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
nB
n/B
nIB
ncB
ncB
n�B
n�B
n�B
o B
n�B
n�B
o B
oB
oOB
o�B
p;B
p�B
p�B
p�B
qAB
qvB
q�B
rB
rGB
r-B
raB
r�B
r�B
sB
s3B
s3B
sMB
shB
s�B
s�B
s�B
t9B
tnB
t�B
t�B
t�B
t�B
uB
uB
uZB
u�B
utB
u�B
u�B
u�B
u�B
u�B
vB
u�B
vB
v+B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
wLB
w�B
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y>B
yrB
y�B
y�B
y�B
y�B
zB
zB
zxB
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105226  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191049  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191050  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191050                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041058  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041058  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151507                      G�O�G�O�G�O�                
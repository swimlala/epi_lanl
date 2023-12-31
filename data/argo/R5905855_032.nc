CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:16:16Z creation;2022-06-04T19:16:17Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604191616  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL                A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���b	�Z1   @����Y�k@.C��%�c�z�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  @���A!��A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�ffB�ffB�ffB�  B�33B���B�  B�  B�  B�  B�  B�  B���B̙�B���B�  B���B�  B�  B�  B�ffB���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@~{@�
=@��
A!�A?�A_�A�A�A�A�A�AЏ\A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B�W
B�W
B�W
B��B�#�B��qB��B��B��B��B��B��BǽqB̊>BνqB��B׽qB��B��B��B�W
B�qB��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�C�C޸C�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCJ�CK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D�zD�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@w�D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
Dǂ=Dǿ
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
D��
D�?
D�x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%zA�.�A�/OA�1�A�3�A�5A�+�A�'�A�+kA�/�A�:^A�9XA�<jA�@OA�>�A�>A�B�A�C�A�B�A�DgA�EA�FtA�GA�H�A�I�A�J�A�K^A�L�A�M�A�M6A�K�A�J�A�DgA�>A�>A�?}A�<jA��^A�NA��jÄ́A�T�A���AǜCA�7A�c A�8A��XA�c�A�ںA��A���A��A�2-A��A�33A�5�A��NA�\�A�rGA�1�A��VA��MA�4nA��ZA�y	A�P�A���A�бA�6�A�A�d�A�0�A~h
AzVAtn/Ap��Al?�Aj��Ai��Ag�Aa4A_A]A\\�A[~�AY iAU	AOG�AFxADXyAC�gAB�A>�A:��A9��A8�8A6:*A5|A4��A3��A3]�A3;A2�A1��A/�pA.�QA-QA*�xA)�hA(��A(l�A'�PA'<�A%��A#��A"sA��A�A�9A��Ag8AHA��A�AĜA��A�A� AMjAVmA��A�>A��AJ#A;A��ATaAx�A5?A�A�$A�mAI�A	��A	.IA�A͟A	S�AK^A͟A
֡A�)A7A��A/A
�0A
p;A
(�A�Ac�A�&Ab�A�A33A;A��A �R@��V@���@� �@���@�:�@��@�J#@��)@�u%@��@�@��@�$@��@��r@��@�@@�GE@��@�F@�J�@�b@��@�x@��@�_@鋬@�~�@��3@���@��r@�u@�@��@��@�B�@��@撣@傪@��`@��@�_@�ff@��@��@��@�4�@�$�@ߴ�@�iD@���@�A�@�R�@�iD@�ϫ@��E@�Ta@� i@���@�iD@��@���@�C�@��@Ի�@���@ӷ@ӂ�@�w2@��@��'@Қ@�!@�X@��@���@�s�@�(�@ϖS@���@�Z�@��@͠'@�1�@̑�@�9X@���@�x�@��@��E@ʚ@�3�@ɠ'@�33@��)@�0U@Ǖ�@��@��@���@�j�@�!-@İ�@�H@�خ@�o�@��@��]@@�PH@��=@��@�R�@���@��@��y@��O@��3@�.I@��@���@���@�J#@�@��@��.@�=q@���@�H�@��|@��5@�֡@�l�@�.�@��@��@���@�F@��@���@��@�v�@��X@��@��U@�}V@��@���@�)_@��x@�-�@��@���@���@��@���@���@�M�@�!@���@�t�@��@��[@��'@��b@�~@��3@���@�g�@�O�@�#�@��/@���@�r�@�W�@�$@�ƨ@��P@�v`@�O�@�;@���@���@��@�Xy@��@���@�@��@�d�@���@���@��j@��*@�\)@�4�@�
=@�%@��@�4@��@��h@�r�@�`�@�Q@��@��~@�|@�v`@�e�@�u�@��g@���@�qv@�O�@�&�@��@���@�oi@��@���@�*0@��@��@��j@�[�@��;@�x@���@��@�,=@���@��{@�-w@��|@���@�u�@�	@��@�:�@���@��.@�*�@���@��@�8�@��P@���@�B[@���@�c@�S&@�G�@�&�@��}@�S�@�3�@��o@���@�X@�B�@��@���@�Ov@�!�@�_@��K@���@�a�@�L�@��@���@�,=@���@���@��*@���@�zx@�j@�7L@���@���@��!@�xl@�GE@�J@��P@�g�@�RT@�&@���@��U@��@�oi@�C�@�u@�� @�f�@�@��6@�A�@�~@��@��#@���@�p�@�@�+@���@��Q@��"@�H�@���@���@���@�:�@��@��>@��H@���@�Y�@�F�@�33@��@���@�ѷ@���@�l"@��@��@��@���@��h@��4@�$t@�@��@���@�U2@�O@�ݘ@��'@�dZ@� \@��|@��/@���@�YK@�*�@��@���@���@�RT@�&�@���@�͟@��}@��\@�}V@�z�@�z@�D�@�-�@�m@H�@~�s@~q�@~.�@}�.@}��@}�@|��@|6@{��@{�{@{�@z��@z �@y0�@x��@x�Y@x�@w��@w��@wZ�@w!-@w�@w,�@wC�@v�@v~�@vi�@v�@uzx@t�o@t@sخ@s��@r�2@rO@q�~@qS&@q�@p$@o1�@nȴ@n��@nxl@n0U@mT�@lĜ@l$@k��@k�k@k i@jOv@i��@i��@iS&@i	l@hɆ@h��@hV�@h(�@g�r@g�
@gMj@fM�@fe@e��@e��@e�@e%F@d��@d�O@dl"@dI�@d1@c��@c�@b��@b��@b��@bOv@a�-@a(�@a+@`��@`��@`��@`��@`��@`tT@_�W@_�k@_A�@^�1@^4@]��@]f�@]S&@]*0@\��@\m�@\%�@\1@[�6@[��@[1�@Z��@Zd�@Z5?@Z�@Z �@Z�@Y��@Y\�@Y \@X�@X��@X`�@X%�@W�@W�P@WA�@V�M@V��@V�A@V&�@U�M@U�@T�@S��@S�V@S9�@RW�@R+k@R�@RJ@Q�^@Q@P�@O�@O��@O��@O�4@O�@N�6@NZ�@N	@M�z@M��@MN<@L��@L�4@LXy@L(�@LG@K��@J~�@J3�@J+k@J($@J�@I�@I}�@I/@H��@HA�@G��@G�k@G]�@G"�@F�m@F�h@F�b@F��@F� @F.�@E��@D��@E�@D[�@C�@C�k@CC@Bȴ@B^5@B0U@A��@A(�@@�@@Ft@?��@?�@?��@?b�@?@O@>�@>��@>c @>:*@=�>@=��@=[W@=q@<K^@;6z@9��@9��@9��@9^�@8�v@8�o@8I�@8"h@8�@7�K@74�@7(@6�@6��@6ff@6u@5x�@5�@4��@4w�@4�@3~�@2�2@2��@3�@2p;@1��@1�^@1IR@0�v@0�z@0��@06@/�W@/�}@/�f@/o�@/=@/S@.��@.ں@.�B@.�'@.�r@._�@.8�@.@-�H@-��@-u�@-A @,ی@,r�@,(�@+�@+��@+��@+�@+�$@+4�@*�@*�@*n�@*M�@)�@)��@)5�@(�@(��@(��@(C-@'��@'�	@&�@&�@& �@%ԕ@%�C@%�7@%T�@$�[@$w�@$"h@#�+@#�}@#��@#��@#dZ@#�@"�R@"�}@"�@"h
@"R�@"B[@"8�@"
�@!�N@!�=@!�@!p�@!B�@ ��@ �v@ �9@ ��@ ��@ w�@ `�@ "h@�r@�@b�@>�@+@�@�@�@�+@Ta@
�@_@�N@��@�7@c@u�@`B@B�@�@��@�@��@��@~(@[�@2�@7@�@��@�f@t�@F�@$t@�@�}@��@�+@\�@O@�@��@��@��@a�@\�@B�@�@�)@�@��@y>@7�@7@�@�r@�g@�w@�[@��@y�@o�@X�@1�@��@�6@�@�A@kQ@E�@{@��@��@��@��@x�@j@S&@X@\�@c�@c�@j@k�@S&@L�@Dg@:�@(�@@�f@��@bN@x@��@��@A�@�@�@�@�@�'@�<@�L@~�@i�@R�@�@�@�H@�@�@a�@S&@G�@A @<6@*0@�f@�@�)@�e@��@g8@1'@�@� @��@6z@��@�s@�@�2@�@p;@Q@?@$�@�@�'@�M@F@�|@�@�@ѷ@��@��@��@z�@e�@`�@S�@>B@%�@�@�&@�k@W?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%zA�.�A�/OA�1�A�3�A�5A�+�A�'�A�+kA�/�A�:^A�9XA�<jA�@OA�>�A�>A�B�A�C�A�B�A�DgA�EA�FtA�GA�H�A�I�A�J�A�K^A�L�A�M�A�M6A�K�A�J�A�DgA�>A�>A�?}A�<jA��^A�NA��jÄ́A�T�A���AǜCA�7A�c A�8A��XA�c�A�ںA��A���A��A�2-A��A�33A�5�A��NA�\�A�rGA�1�A��VA��MA�4nA��ZA�y	A�P�A���A�бA�6�A�A�d�A�0�A~h
AzVAtn/Ap��Al?�Aj��Ai��Ag�Aa4A_A]A\\�A[~�AY iAU	AOG�AFxADXyAC�gAB�A>�A:��A9��A8�8A6:*A5|A4��A3��A3]�A3;A2�A1��A/�pA.�QA-QA*�xA)�hA(��A(l�A'�PA'<�A%��A#��A"sA��A�A�9A��Ag8AHA��A�AĜA��A�A� AMjAVmA��A�>A��AJ#A;A��ATaAx�A5?A�A�$A�mAI�A	��A	.IA�A͟A	S�AK^A͟A
֡A�)A7A��A/A
�0A
p;A
(�A�Ac�A�&Ab�A�A33A;A��A �R@��V@���@� �@���@�:�@��@�J#@��)@�u%@��@�@��@�$@��@��r@��@�@@�GE@��@�F@�J�@�b@��@�x@��@�_@鋬@�~�@��3@���@��r@�u@�@��@��@�B�@��@撣@傪@��`@��@�_@�ff@��@��@��@�4�@�$�@ߴ�@�iD@���@�A�@�R�@�iD@�ϫ@��E@�Ta@� i@���@�iD@��@���@�C�@��@Ի�@���@ӷ@ӂ�@�w2@��@��'@Қ@�!@�X@��@���@�s�@�(�@ϖS@���@�Z�@��@͠'@�1�@̑�@�9X@���@�x�@��@��E@ʚ@�3�@ɠ'@�33@��)@�0U@Ǖ�@��@��@���@�j�@�!-@İ�@�H@�خ@�o�@��@��]@@�PH@��=@��@�R�@���@��@��y@��O@��3@�.I@��@���@���@�J#@�@��@��.@�=q@���@�H�@��|@��5@�֡@�l�@�.�@��@��@���@�F@��@���@��@�v�@��X@��@��U@�}V@��@���@�)_@��x@�-�@��@���@���@��@���@���@�M�@�!@���@�t�@��@��[@��'@��b@�~@��3@���@�g�@�O�@�#�@��/@���@�r�@�W�@�$@�ƨ@��P@�v`@�O�@�;@���@���@��@�Xy@��@���@�@��@�d�@���@���@��j@��*@�\)@�4�@�
=@�%@��@�4@��@��h@�r�@�`�@�Q@��@��~@�|@�v`@�e�@�u�@��g@���@�qv@�O�@�&�@��@���@�oi@��@���@�*0@��@��@��j@�[�@��;@�x@���@��@�,=@���@��{@�-w@��|@���@�u�@�	@��@�:�@���@��.@�*�@���@��@�8�@��P@���@�B[@���@�c@�S&@�G�@�&�@��}@�S�@�3�@��o@���@�X@�B�@��@���@�Ov@�!�@�_@��K@���@�a�@�L�@��@���@�,=@���@���@��*@���@�zx@�j@�7L@���@���@��!@�xl@�GE@�J@��P@�g�@�RT@�&@���@��U@��@�oi@�C�@�u@�� @�f�@�@��6@�A�@�~@��@��#@���@�p�@�@�+@���@��Q@��"@�H�@���@���@���@�:�@��@��>@��H@���@�Y�@�F�@�33@��@���@�ѷ@���@�l"@��@��@��@���@��h@��4@�$t@�@��@���@�U2@�O@�ݘ@��'@�dZ@� \@��|@��/@���@�YK@�*�@��@���@���@�RT@�&�@���@�͟@��}@��\@�}V@�z�@�z@�D�@�-�@�m@H�@~�s@~q�@~.�@}�.@}��@}�@|��@|6@{��@{�{@{�@z��@z �@y0�@x��@x�Y@x�@w��@w��@wZ�@w!-@w�@w,�@wC�@v�@v~�@vi�@v�@uzx@t�o@t@sخ@s��@r�2@rO@q�~@qS&@q�@p$@o1�@nȴ@n��@nxl@n0U@mT�@lĜ@l$@k��@k�k@k i@jOv@i��@i��@iS&@i	l@hɆ@h��@hV�@h(�@g�r@g�
@gMj@fM�@fe@e��@e��@e�@e%F@d��@d�O@dl"@dI�@d1@c��@c�@b��@b��@b��@bOv@a�-@a(�@a+@`��@`��@`��@`��@`��@`tT@_�W@_�k@_A�@^�1@^4@]��@]f�@]S&@]*0@\��@\m�@\%�@\1@[�6@[��@[1�@Z��@Zd�@Z5?@Z�@Z �@Z�@Y��@Y\�@Y \@X�@X��@X`�@X%�@W�@W�P@WA�@V�M@V��@V�A@V&�@U�M@U�@T�@S��@S�V@S9�@RW�@R+k@R�@RJ@Q�^@Q@P�@O�@O��@O��@O�4@O�@N�6@NZ�@N	@M�z@M��@MN<@L��@L�4@LXy@L(�@LG@K��@J~�@J3�@J+k@J($@J�@I�@I}�@I/@H��@HA�@G��@G�k@G]�@G"�@F�m@F�h@F�b@F��@F� @F.�@E��@D��@E�@D[�@C�@C�k@CC@Bȴ@B^5@B0U@A��@A(�@@�@@Ft@?��@?�@?��@?b�@?@O@>�@>��@>c @>:*@=�>@=��@=[W@=q@<K^@;6z@9��@9��@9��@9^�@8�v@8�o@8I�@8"h@8�@7�K@74�@7(@6�@6��@6ff@6u@5x�@5�@4��@4w�@4�@3~�@2�2@2��@3�@2p;@1��@1�^@1IR@0�v@0�z@0��@06@/�W@/�}@/�f@/o�@/=@/S@.��@.ں@.�B@.�'@.�r@._�@.8�@.@-�H@-��@-u�@-A @,ی@,r�@,(�@+�@+��@+��@+�@+�$@+4�@*�@*�@*n�@*M�@)�@)��@)5�@(�@(��@(��@(C-@'��@'�	@&�@&�@& �@%ԕ@%�C@%�7@%T�@$�[@$w�@$"h@#�+@#�}@#��@#��@#dZ@#�@"�R@"�}@"�@"h
@"R�@"B[@"8�@"
�@!�N@!�=@!�@!p�@!B�@ ��@ �v@ �9@ ��@ ��@ w�@ `�@ "h@�r@�@b�@>�@+@�@�@�@�+@Ta@
�@_@�N@��@�7@c@u�@`B@B�@�@��@�@��@��@~(@[�@2�@7@�@��@�f@t�@F�@$t@�@�}@��@�+@\�@O@�@��@��@��@a�@\�@B�@�@�)@�@��@y>@7�@7@�@�r@�g@�w@�[@��@y�@o�@X�@1�@��@�6@�@�A@kQ@E�@{@��@��@��@��@x�@j@S&@X@\�@c�@c�@j@k�@S&@L�@Dg@:�@(�@@�f@��@bN@x@��@��@A�@�@�@�@�@�'@�<@�L@~�@i�@R�@�@�@�H@�@�@a�@S&@G�@A @<6@*0@�f@�@�)@�e@��@g8@1'@�@� @��@6z@��@�s@�@�2@�@p;@Q@?@$�@�@�'@�M@F@�|@�@�@ѷ@��@��@��@z�@e�@`�@S�@>B@%�@�@�&@�k@W?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�gB��B��B��B��B��B��B��B��B��B�-B�GB�GB�GB��B�aB�-B�GB�{B��B��B��B�%B�YB��B��B��B��B�B�B�B��B�DB��B�B�B��B�yB�B	$B	l�B	o�B	sB	��B	��B	��B	��B	�1B	ںB	�1B	��B	āB	�?B	�jB	�oB
VB
|�B
WYB
�tB
�B
{�B
^�B
;�B
2aB
'�B
9B	��B	��B	�iB	��B	��B	��B	�IB	�)B	�VB	�WB	�SB	x�B	m�B	gmB	Z�B	5�B	%�B	�B	MB	B	SB	 �B�@B��B�KB��B��B�WB�B�dB��B�;B�5B�B��B�bB��B�:B��B��B�zB�B��B�4B��B�{BňB�JB�B�$B�gBѷB�#BɺB̘B̈́B��B�BרB�	B�$B��B��B	 4B		�B	�B	\B	bB	TB	�B	MB	%�B	1�B	,�B	)DB	�B	�B�cB�B�rB�DB	�B	.B	B	'�B	U�B	{B	��B	�B	��B	�iB	}<B	yXB	rB	i*B	n�B	� B	{�B	v�B	t�B	kkB	b�B	]IB	Z�B	W$B	R�B	J�B	EmB	A�B	?}B	7�B	>�B	A�B	D�B	J�B	N�B	S�B	W�B	\�B	\xB	VmB	VSB	V�B	V�B	VmB	[	B	fB	l"B	oB	sB	u�B	u�B	xlB	{JB	~�B	~�B	�B	��B	��B	�~B	��B	�BB	�<B	�B	�:B	��B	��B	�,B	� B	��B	��B	��B	�YB	�B	�?B	��B	�B	��B	��B	�	B	��B	�5B	�B	��B	��B	�
B	�sB	��B	�5B	�nB	��B	��B	��B	��B	��B	�xB	��B	��B	�B	��B	��B	�qB	��B	��B	��B	��B	��B	��B	ÖB	��B	��B	ňB	�B	ňB	�EB	�B	�xB	�~B	�DB	�)B	��B	��B	�B	�B	ѝB	�oB	�:B	҉B	��B	�MB	յB	�B	�B	�?B	�sB	�yB	��B	�)B	�/B	�5B	�5B	�pB	��B	ߊB	�HB	�4B	�NB	�NB	�B	�:B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�$B	��B	�*B	�DB	�B	�B	��B	�6B	�B	��B	�B	�)B	�B	�IB	�B	�B	�B	�OB	�B	�oB	�B	�B	�[B	��B	�|B	�B	�MB	�B	�B	��B	�B	�?B	�tB	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	�*B	�DB	��B	�B	��B	�qB	�]B	��B	��B	��B	�wB	�]B	�]B	�]B	��B	�HB	��B	�B	�}B	�HB	�HB	�}B	�cB	�HB
 �B
;B
;B
�B
B
'B
�B
[B
�B
�B
�B
�B
�B
�B
9B
B
�B
tB
�B
�B
_B
�B
1B
fB
	lB

	B
	�B

=B

�B
^B
�B
�B
dB
dB
dB
�B
B
jB
jB
PB
<B
VB
�B
B
�B
�B
hB
�B
�B
&B
@B
[B
�B
�B
�B
B
gB
gB
�B
�B
�B
B
9B
9B
�B
�B
�B
YB
+B
+B
+B
�B
�B
B
KB
�B
�B
QB
�B
qB
�B
)B
�B
]B
]B
xB
xB
IB
�B
B
OB
�B
�B
 BB
 �B
 �B
 �B
!bB
!|B
!�B
!�B
"B
"�B
"hB
"�B
"�B
"�B
"�B
# B
#�B
$ZB
$�B
$�B
%B
$�B
$�B
%�B
%�B
%�B
&B
&�B
&�B
'B
'8B
'�B
'�B
($B
(
B
(XB
(�B
)*B
)yB
)�B
*KB
+B
+�B
,=B
,�B
-)B
./B
/B
0;B
1�B
2�B
3hB
3�B
4B
4�B
4�B
4�B
4�B
5B
5B
4�B
5ZB
5�B
5�B
5�B
6B
6�B
72B
6�B
6`B
6+B
6FB
6+B
6�B
6�B
7B
7�B
9rB
:B
:B
9�B
:DB
:xB
;JB
;B
;dB
;B
<B
<jB
=<B
=qB
=qB
=�B
>B
>wB
>�B
>wB
>�B
?B
?}B
?�B
@B
@B
@�B
A B
AUB
AUB
A�B
BB
BuB
B�B
B�B
B�B
B�B
BuB
B�B
CaB
CGB
CGB
CGB
CGB
C�B
C�B
C�B
C�B
C�B
C�B
DB
D�B
E9B
EB
E9B
E9B
E�B
E�B
E�B
E�B
F%B
F?B
F�B
F�B
F�B
GB
F�B
G_B
HKB
HB
HKB
H�B
H�B
H�B
IB
IRB
IRB
IRB
I�B
I�B
I�B
J=B
J�B
KDB
K^B
K^B
K�B
K�B
LB
K�B
K�B
LB
L0B
LdB
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
NVB
N�B
O(B
O(B
OB
O�B
P}B
P�B
QNB
Q�B
Q�B
Q�B
R:B
R�B
R�B
S&B
S@B
SuB
SuB
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T{B
T�B
UB
UB
UMB
U�B
VB
VmB
VB
U�B
UgB
UgB
UgB
U�B
VSB
W�B
W�B
WYB
W
B
W�B
W�B
W�B
X�B
X�B
YB
Y1B
X�B
YeB
YKB
Y�B
Z7B
ZkB
[WB
]�B
^B
^B
]�B
]~B
]�B
]�B
]�B
]�B
]~B
]/B
]�B
]IB
\�B
\�B
\�B
\�B
]IB
]~B
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
_pB
_�B
_�B
`BB
`'B
`�B
`vB
`'B
`BB
`�B
`�B
`'B
`'B
`�B
`�B
abB
a�B
bB
b�B
b�B
c B
c:B
c�B
c�B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h
B
h$B
h$B
h$B
hXB
h�B
h�B
h�B
h�B
iDB
i�B
i�B
jB
j�B
j�B
kB
kQB
k�B
k�B
l"B
lqB
l�B
l�B
mB
l�B
mCB
m�B
nB
n}B
ncB
n}B
n�B
n�B
n�B
n�B
o B
oB
oOB
oiB
o�B
o�B
pB
p!B
p!B
p!B
p;B
p;B
pUB
p�B
p�B
p�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
sB
s3B
s3B
s3B
sMB
sMB
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
u?B
utB
utB
u�B
u�B
u�B
v+B
vFB
vFB
v�B
v�B
v�B
wB
w2B
w2B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xRB
xRB
x8B
xRB
xRB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
yXB
yXB
yrB
y�B
y�B
zDB
z^B
zxB
zxB
z^B
z*B
zB
zB
z*B
zDB
zDB
z*B
zDB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{�B
|6B
|6B
|PB
|PB
|B
|6B
|6B
|PB
|PB
|jB
|jB
|�B
|�B
|�B
}B
|�B
}"B
}�B
~B
~wB
~�B
~�B
~�B
~�B
~�B
HB
.B
HB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�4B
��B
�;B
��B
��B
��B
�oB
��B
�B
�B
�AB
�[B
�uB
��B
�uB
��B
��B
��B
��B
�B
�-B
�-B
�-B
�GB
�aB
��B
�gB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�gB��B��B��B��B��B��B��B��B��B�-B�GB�GB�GB��B�aB�-B�GB�{B��B��B��B�%B�YB��B��B��B��B�B�B�B��B�DB��B�B�B��B�yB�B	$B	l�B	o�B	sB	��B	��B	��B	��B	�1B	ںB	�1B	��B	āB	�?B	�jB	�oB
VB
|�B
WYB
�tB
�B
{�B
^�B
;�B
2aB
'�B
9B	��B	��B	�iB	��B	��B	��B	�IB	�)B	�VB	�WB	�SB	x�B	m�B	gmB	Z�B	5�B	%�B	�B	MB	B	SB	 �B�@B��B�KB��B��B�WB�B�dB��B�;B�5B�B��B�bB��B�:B��B��B�zB�B��B�4B��B�{BňB�JB�B�$B�gBѷB�#BɺB̘B̈́B��B�BרB�	B�$B��B��B	 4B		�B	�B	\B	bB	TB	�B	MB	%�B	1�B	,�B	)DB	�B	�B�cB�B�rB�DB	�B	.B	B	'�B	U�B	{B	��B	�B	��B	�iB	}<B	yXB	rB	i*B	n�B	� B	{�B	v�B	t�B	kkB	b�B	]IB	Z�B	W$B	R�B	J�B	EmB	A�B	?}B	7�B	>�B	A�B	D�B	J�B	N�B	S�B	W�B	\�B	\xB	VmB	VSB	V�B	V�B	VmB	[	B	fB	l"B	oB	sB	u�B	u�B	xlB	{JB	~�B	~�B	�B	��B	��B	�~B	��B	�BB	�<B	�B	�:B	��B	��B	�,B	� B	��B	��B	��B	�YB	�B	�?B	��B	�B	��B	��B	�	B	��B	�5B	�B	��B	��B	�
B	�sB	��B	�5B	�nB	��B	��B	��B	��B	��B	�xB	��B	��B	�B	��B	��B	�qB	��B	��B	��B	��B	��B	��B	ÖB	��B	��B	ňB	�B	ňB	�EB	�B	�xB	�~B	�DB	�)B	��B	��B	�B	�B	ѝB	�oB	�:B	҉B	��B	�MB	յB	�B	�B	�?B	�sB	�yB	��B	�)B	�/B	�5B	�5B	�pB	��B	ߊB	�HB	�4B	�NB	�NB	�B	�:B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�$B	��B	�*B	�DB	�B	�B	��B	�6B	�B	��B	�B	�)B	�B	�IB	�B	�B	�B	�OB	�B	�oB	�B	�B	�[B	��B	�|B	�B	�MB	�B	�B	��B	�B	�?B	�tB	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	��B	��B	�*B	�DB	��B	�B	��B	�qB	�]B	��B	��B	��B	�wB	�]B	�]B	�]B	��B	�HB	��B	�B	�}B	�HB	�HB	�}B	�cB	�HB
 �B
;B
;B
�B
B
'B
�B
[B
�B
�B
�B
�B
�B
�B
9B
B
�B
tB
�B
�B
_B
�B
1B
fB
	lB

	B
	�B

=B

�B
^B
�B
�B
dB
dB
dB
�B
B
jB
jB
PB
<B
VB
�B
B
�B
�B
hB
�B
�B
&B
@B
[B
�B
�B
�B
B
gB
gB
�B
�B
�B
B
9B
9B
�B
�B
�B
YB
+B
+B
+B
�B
�B
B
KB
�B
�B
QB
�B
qB
�B
)B
�B
]B
]B
xB
xB
IB
�B
B
OB
�B
�B
 BB
 �B
 �B
 �B
!bB
!|B
!�B
!�B
"B
"�B
"hB
"�B
"�B
"�B
"�B
# B
#�B
$ZB
$�B
$�B
%B
$�B
$�B
%�B
%�B
%�B
&B
&�B
&�B
'B
'8B
'�B
'�B
($B
(
B
(XB
(�B
)*B
)yB
)�B
*KB
+B
+�B
,=B
,�B
-)B
./B
/B
0;B
1�B
2�B
3hB
3�B
4B
4�B
4�B
4�B
4�B
5B
5B
4�B
5ZB
5�B
5�B
5�B
6B
6�B
72B
6�B
6`B
6+B
6FB
6+B
6�B
6�B
7B
7�B
9rB
:B
:B
9�B
:DB
:xB
;JB
;B
;dB
;B
<B
<jB
=<B
=qB
=qB
=�B
>B
>wB
>�B
>wB
>�B
?B
?}B
?�B
@B
@B
@�B
A B
AUB
AUB
A�B
BB
BuB
B�B
B�B
B�B
B�B
BuB
B�B
CaB
CGB
CGB
CGB
CGB
C�B
C�B
C�B
C�B
C�B
C�B
DB
D�B
E9B
EB
E9B
E9B
E�B
E�B
E�B
E�B
F%B
F?B
F�B
F�B
F�B
GB
F�B
G_B
HKB
HB
HKB
H�B
H�B
H�B
IB
IRB
IRB
IRB
I�B
I�B
I�B
J=B
J�B
KDB
K^B
K^B
K�B
K�B
LB
K�B
K�B
LB
L0B
LdB
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
NVB
N�B
O(B
O(B
OB
O�B
P}B
P�B
QNB
Q�B
Q�B
Q�B
R:B
R�B
R�B
S&B
S@B
SuB
SuB
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T{B
T�B
UB
UB
UMB
U�B
VB
VmB
VB
U�B
UgB
UgB
UgB
U�B
VSB
W�B
W�B
WYB
W
B
W�B
W�B
W�B
X�B
X�B
YB
Y1B
X�B
YeB
YKB
Y�B
Z7B
ZkB
[WB
]�B
^B
^B
]�B
]~B
]�B
]�B
]�B
]�B
]~B
]/B
]�B
]IB
\�B
\�B
\�B
\�B
]IB
]~B
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
_pB
_�B
_�B
`BB
`'B
`�B
`vB
`'B
`BB
`�B
`�B
`'B
`'B
`�B
`�B
abB
a�B
bB
b�B
b�B
c B
c:B
c�B
c�B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h
B
h$B
h$B
h$B
hXB
h�B
h�B
h�B
h�B
iDB
i�B
i�B
jB
j�B
j�B
kB
kQB
k�B
k�B
l"B
lqB
l�B
l�B
mB
l�B
mCB
m�B
nB
n}B
ncB
n}B
n�B
n�B
n�B
n�B
o B
oB
oOB
oiB
o�B
o�B
pB
p!B
p!B
p!B
p;B
p;B
pUB
p�B
p�B
p�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
sB
s3B
s3B
s3B
sMB
sMB
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
u?B
utB
utB
u�B
u�B
u�B
v+B
vFB
vFB
v�B
v�B
v�B
wB
w2B
w2B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xRB
xRB
x8B
xRB
xRB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
yXB
yXB
yrB
y�B
y�B
zDB
z^B
zxB
zxB
z^B
z*B
zB
zB
z*B
zDB
zDB
z*B
zDB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{�B
|6B
|6B
|PB
|PB
|B
|6B
|6B
|PB
|PB
|jB
|jB
|�B
|�B
|�B
}B
|�B
}"B
}�B
~B
~wB
~�B
~�B
~�B
~�B
~�B
HB
.B
HB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�4B
��B
�;B
��B
��B
��B
�oB
��B
�B
�B
�AB
�[B
�uB
��B
�uB
��B
��B
��B
��B
�B
�-B
�-B
�-B
�GB
�aB
��B
�gB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105233  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191616  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191617  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191617                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041624  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041624  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                
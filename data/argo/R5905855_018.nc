CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:13:53Z creation;2022-06-04T19:13:53Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191353  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�ҡ*���1   @�ҡ��s�@.�+J�d�
=p�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BjffBo��Bv��B�  B�  B�  B�  B�ffB���B���B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C  C	�fC  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2ffC3��C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Ci�fCk�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@~{@�
=@�
=A�AA�A_�A�A�A�A�A��\A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBjG�Boz�Bv�B�HB��B��B��B�W
B��>B��qB��qB��B��B�#�B��qB��B��B��B��B��B��B�#�B�#�B�#�B��B׽qB��B��B��B��B��B��B��B��B��B��C�RC�C�C�RC	޸C�RC�RC�RC�RC�RC�RC޸C�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC2^�C3�C5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCf�Ch�Ci޸Ck޸Cm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~DzD~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE�zDE�DF~DGzDG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
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
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AᔯAᕵA�7AᙚAᔯA�AሚA�~�A�m]A�G�A�
	A� �A���A���A��TA��ZA��MA��GA���A���A��DA��A��
A���A��dA�՛A��)A௸A�bNA�Aڱ'A�)*AכqA��A�_pA�`�A͊	Aʱ�Aȷ�A��A�9�A©*A�R�A�R A�p�A�ffA��!A���A���A�e�A���A�!A��A�{A�tTA��"A�T�A�h�A���A��A�OA��A� 4A��A���A�MA�hsA��AA���A���A��lA�e�A�P}A���A���A�!A��?A��A�bA��vA�	7A���A�'RA���A�2aA�o�A�'�Ak�A{ȴAv�IAn��Al�Ah�yAf��Ae��Ae;Aad�A]��AY�!AW��AU�]AT�XAS7LAO��AM7�AK+AG��AEp�ACy>A@�;A>1A<N�A;��A:��A9�,A9h
A8XA6($A3n/A13�A0��A0�jA0W�A.��A-�4A-�A*~�A)k�A(�A%��A$�/A#��A!��A֡AHA��A��Ap�A�A��AS�A��A��AɆA�xA iA��A�7Ax�A�A�A8�AOA��A*�A�A�;A��AC�A�A��A�A��A�'A}�A�A��ARTA�A�7Ab�AGAcAMAA�A�]A�HAqA��A��AB[A \A�A��A
�QA	��A	$A8�A�AA�"A
=A�]A��AoA:�A�dAYA��A� A�"A�0AS�A�A��A �A �UA L�A zxA �A �@�Y�@�z�@�0�@��@���@�q@��@�=�@�  @��c@�?�@���@���@��@�\�@��@�!@��@�B�@�	l@�~�@�*0@�Ta@��@��?@��@��P@�u%@��*@�X@��	@��a@�(@���@�h�@��@�1�@�L@�@���@�q�@�M�@�?�@�x@�&�@���@���@�U2@�ԕ@�Vm@�}V@��@�X@ܿ�@�N�@�M@��@�1�@�S@ښ�@ټ�@�[W@֝I@֋D@�tT@��@մ�@�O�@Ԩ�@�p;@�e@ҵ@�@���@� \@���@��v@�n�@Ϥ@@�Ɇ@���@�G@�Q�@���@̗�@�kQ@�ԕ@��@�K^@�O@��@ȗ�@�D�@ǥ�@�?}@���@ƭ�@�~(@�S�@���@�^�@�	l@�ߤ@Ķ�@�bN@���@ê�@�C�@ @��o@���@�@��@�,=@�ݘ@�O�@�"�@��@��x@�Q@� �@��D@���@�Z�@���@�GE@� �@��X@�~�@�2a@��8@��U@�C-@���@�x@��v@�M@�خ@�j�@�ی@���@��o@�q@���@���@�v`@�G�@� \@�o@��`@��@��4@�9X@��@���@�1�@�C-@��@�A�@��P@���@�6�@���@�!�@���@�l�@�E�@�7�@��@�A�@��@�C�@��r@���@���@�f�@�J�@�S@���@�tT@�4n@���@��4@�F�@��@�~�@�;�@��@�P�@���@�p;@�<�@���@�A @�҉@�~(@�e�@�@���@�[W@�33@��@�� @�Z�@��@��@���@�iD@�@O@��@���@�r�@�!�@���@�>�@���@���@��	@���@�v�@�Z@�8�@��o@���@�[W@�+�@�֡@�i�@�?�@���@�Q�@�6z@���@�n�@�"h@���@���@���@���@�o�@��@��O@��@�Ft@��r@��H@�l�@�0�@���@�>B@��@���@�f�@� \@��@��F@�c�@��@��w@���@�a�@�5�@��c@�q�@�1'@��@��z@�dZ@�Q�@�@���@�`�@�6�@�G@��H@��P@�S&@�6z@��"@���@�V@�R�@���@�ƨ@��^@�k�@�V@�Ɇ@�q@���@��~@�A�@��	@���@�Ov@��@��o@���@�A�@��]@�s�@�D�@��@���@��@��@��A@�_@�2�@�~@��@�ԕ@���@�9�@��@���@�ff@�C�@�6@���@��~@�+@��E@�ی@���@�z�@�c�@�C-@��@��@���@��V@���@���@�b�@�K�@�&@���@��@�bN@�;�@�!@��@��@n/@K�@~ں@~��@~��@~	@}��@}��@}e,@}@|ی@|�e@|!@{�q@{.I@z�B@zM�@y�@y��@yj@y�@x�?@xZ@wݘ@w��@wt�@wF�@v��@v��@v#:@u��@t��@s�
@s�f@s_p@s_p@sW?@rں@rTa@q��@qY�@p��@p@ol�@oH�@o
=@n��@n�\@n6�@m��@mS&@l��@lm�@l	�@k6z@jYK@i��@is�@iG�@hɆ@h��@h��@h��@hq@gn/@f�@fp;@f@e�7@eN<@d�)@c��@c,�@b�2@bȴ@b��@b�\@bs�@b8�@a�#@aQ�@`�v@`�9@`�@`!@_��@_U�@^��@^��@^\�@]��@]|@\�@\�j@\?�@\	�@[�+@[�*@[,�@Z��@Z=q@Y�"@Yhs@Y:�@Y \@X�/@X$@W�@W+@V�@V�L@Vi�@V�@U��@UV@T�z@T~@S�
@S��@S]�@S�@R�H@R��@R;�@Q@Q��@Q(�@P�U@P�D@PFt@O��@Oo@Nh
@N$�@M��@Mx�@M4@L�@L�@LQ�@L@K��@KE9@K@J�X@J��@I��@I��@I�~@I(�@H��@H~(@HD�@H7�@H~@G�}@G~�@GC�@F�"@F�@F}V@E��@E��@EX@E�@D��@Dc�@D  @C�
@C�@Cn/@C)_@B�@B�X@B� @B}V@BO@A��@A��@Af�@A�@@�/@@~(@@�@?�@?��@?@>��@>E�@>{@=�t@=4@<�|@<��@<��@<e�@<$@<1@;��@;iD@;4�@:��@:n�@:8�@:.�@:)�@:e@:_@9�j@9�-@9e,@9&�@8�@8��@8��@8u�@8H@8$@7�@7.I@7&@6��@6�,@6�X@6��@6� @6a|@6J�@6O@5�@5��@5+�@5+@5�@4��@4�Y@4c�@4I�@4�@3��@3�P@3v`@3g�@3;d@3�@2��@2�,@2�h@2��@2p;@2H�@2	@1�@1�M@1O�@1+@0�@0�I@0�D@0l"@0-�@/��@/�q@/'�@/@.҉@.��@.�1@.}V@.GE@-�)@-�@-��@-�@-A @-�@,�@,Ĝ@,�_@,tT@,!@+�W@+��@+��@+~�@+iD@+H�@+ i@*�R@*��@*R�@)�@)4@)2a@)0�@)*0@(��@(�4@(w�@(U2@($@'��@'��@'y�@'H�@''�@'!-@&��@&��@&�F@&!�@%�>@%��@%!�@$�O@$u�@$bN@$>B@$�@#��@#X�@#o@"�@"�h@"��@"�r@"}V@"Z�@"1�@!�9@!�h@!p�@!J�@!8�@! \@ �@ ��@ Z@ <�@ (�@ ~@�
@��@��@Z�@@O@!-@��@�x@p;@�@�@��@�7@e,@/@��@�@Z@�A@��@U�@/�@S@��@B[@�j@�h@k�@-w@�v@��@bN@$@��@�@�@dZ@�@��@�@��@�R@h
@+k@�3@��@Dg@*0@�@�_@%�@�@�{@s@]�@9�@�c@��@�1@z@Ov@1�@�@�t@�@w2@@@��@��@h�@U2@$@x@�}@v`@$t@ i@҉@�@�b@W�@�@�@ �@�#@�7@4@��@��@9X@x@�@��@�P@|�@X�@8@.I@@
�@
�1@
�A@
h
@
_�@
6�@
4@	�@	��@	��@	��@	p�@	e,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AᔯAᕵA�7AᙚAᔯA�AሚA�~�A�m]A�G�A�
	A� �A���A���A��TA��ZA��MA��GA���A���A��DA��A��
A���A��dA�՛A��)A௸A�bNA�Aڱ'A�)*AכqA��A�_pA�`�A͊	Aʱ�Aȷ�A��A�9�A©*A�R�A�R A�p�A�ffA��!A���A���A�e�A���A�!A��A�{A�tTA��"A�T�A�h�A���A��A�OA��A� 4A��A���A�MA�hsA��AA���A���A��lA�e�A�P}A���A���A�!A��?A��A�bA��vA�	7A���A�'RA���A�2aA�o�A�'�Ak�A{ȴAv�IAn��Al�Ah�yAf��Ae��Ae;Aad�A]��AY�!AW��AU�]AT�XAS7LAO��AM7�AK+AG��AEp�ACy>A@�;A>1A<N�A;��A:��A9�,A9h
A8XA6($A3n/A13�A0��A0�jA0W�A.��A-�4A-�A*~�A)k�A(�A%��A$�/A#��A!��A֡AHA��A��Ap�A�A��AS�A��A��AɆA�xA iA��A�7Ax�A�A�A8�AOA��A*�A�A�;A��AC�A�A��A�A��A�'A}�A�A��ARTA�A�7Ab�AGAcAMAA�A�]A�HAqA��A��AB[A \A�A��A
�QA	��A	$A8�A�AA�"A
=A�]A��AoA:�A�dAYA��A� A�"A�0AS�A�A��A �A �UA L�A zxA �A �@�Y�@�z�@�0�@��@���@�q@��@�=�@�  @��c@�?�@���@���@��@�\�@��@�!@��@�B�@�	l@�~�@�*0@�Ta@��@��?@��@��P@�u%@��*@�X@��	@��a@�(@���@�h�@��@�1�@�L@�@���@�q�@�M�@�?�@�x@�&�@���@���@�U2@�ԕ@�Vm@�}V@��@�X@ܿ�@�N�@�M@��@�1�@�S@ښ�@ټ�@�[W@֝I@֋D@�tT@��@մ�@�O�@Ԩ�@�p;@�e@ҵ@�@���@� \@���@��v@�n�@Ϥ@@�Ɇ@���@�G@�Q�@���@̗�@�kQ@�ԕ@��@�K^@�O@��@ȗ�@�D�@ǥ�@�?}@���@ƭ�@�~(@�S�@���@�^�@�	l@�ߤ@Ķ�@�bN@���@ê�@�C�@ @��o@���@�@��@�,=@�ݘ@�O�@�"�@��@��x@�Q@� �@��D@���@�Z�@���@�GE@� �@��X@�~�@�2a@��8@��U@�C-@���@�x@��v@�M@�خ@�j�@�ی@���@��o@�q@���@���@�v`@�G�@� \@�o@��`@��@��4@�9X@��@���@�1�@�C-@��@�A�@��P@���@�6�@���@�!�@���@�l�@�E�@�7�@��@�A�@��@�C�@��r@���@���@�f�@�J�@�S@���@�tT@�4n@���@��4@�F�@��@�~�@�;�@��@�P�@���@�p;@�<�@���@�A @�҉@�~(@�e�@�@���@�[W@�33@��@�� @�Z�@��@��@���@�iD@�@O@��@���@�r�@�!�@���@�>�@���@���@��	@���@�v�@�Z@�8�@��o@���@�[W@�+�@�֡@�i�@�?�@���@�Q�@�6z@���@�n�@�"h@���@���@���@���@�o�@��@��O@��@�Ft@��r@��H@�l�@�0�@���@�>B@��@���@�f�@� \@��@��F@�c�@��@��w@���@�a�@�5�@��c@�q�@�1'@��@��z@�dZ@�Q�@�@���@�`�@�6�@�G@��H@��P@�S&@�6z@��"@���@�V@�R�@���@�ƨ@��^@�k�@�V@�Ɇ@�q@���@��~@�A�@��	@���@�Ov@��@��o@���@�A�@��]@�s�@�D�@��@���@��@��@��A@�_@�2�@�~@��@�ԕ@���@�9�@��@���@�ff@�C�@�6@���@��~@�+@��E@�ی@���@�z�@�c�@�C-@��@��@���@��V@���@���@�b�@�K�@�&@���@��@�bN@�;�@�!@��@��@n/@K�@~ں@~��@~��@~	@}��@}��@}e,@}@|ی@|�e@|!@{�q@{.I@z�B@zM�@y�@y��@yj@y�@x�?@xZ@wݘ@w��@wt�@wF�@v��@v��@v#:@u��@t��@s�
@s�f@s_p@s_p@sW?@rں@rTa@q��@qY�@p��@p@ol�@oH�@o
=@n��@n�\@n6�@m��@mS&@l��@lm�@l	�@k6z@jYK@i��@is�@iG�@hɆ@h��@h��@h��@hq@gn/@f�@fp;@f@e�7@eN<@d�)@c��@c,�@b�2@bȴ@b��@b�\@bs�@b8�@a�#@aQ�@`�v@`�9@`�@`!@_��@_U�@^��@^��@^\�@]��@]|@\�@\�j@\?�@\	�@[�+@[�*@[,�@Z��@Z=q@Y�"@Yhs@Y:�@Y \@X�/@X$@W�@W+@V�@V�L@Vi�@V�@U��@UV@T�z@T~@S�
@S��@S]�@S�@R�H@R��@R;�@Q@Q��@Q(�@P�U@P�D@PFt@O��@Oo@Nh
@N$�@M��@Mx�@M4@L�@L�@LQ�@L@K��@KE9@K@J�X@J��@I��@I��@I�~@I(�@H��@H~(@HD�@H7�@H~@G�}@G~�@GC�@F�"@F�@F}V@E��@E��@EX@E�@D��@Dc�@D  @C�
@C�@Cn/@C)_@B�@B�X@B� @B}V@BO@A��@A��@Af�@A�@@�/@@~(@@�@?�@?��@?@>��@>E�@>{@=�t@=4@<�|@<��@<��@<e�@<$@<1@;��@;iD@;4�@:��@:n�@:8�@:.�@:)�@:e@:_@9�j@9�-@9e,@9&�@8�@8��@8��@8u�@8H@8$@7�@7.I@7&@6��@6�,@6�X@6��@6� @6a|@6J�@6O@5�@5��@5+�@5+@5�@4��@4�Y@4c�@4I�@4�@3��@3�P@3v`@3g�@3;d@3�@2��@2�,@2�h@2��@2p;@2H�@2	@1�@1�M@1O�@1+@0�@0�I@0�D@0l"@0-�@/��@/�q@/'�@/@.҉@.��@.�1@.}V@.GE@-�)@-�@-��@-�@-A @-�@,�@,Ĝ@,�_@,tT@,!@+�W@+��@+��@+~�@+iD@+H�@+ i@*�R@*��@*R�@)�@)4@)2a@)0�@)*0@(��@(�4@(w�@(U2@($@'��@'��@'y�@'H�@''�@'!-@&��@&��@&�F@&!�@%�>@%��@%!�@$�O@$u�@$bN@$>B@$�@#��@#X�@#o@"�@"�h@"��@"�r@"}V@"Z�@"1�@!�9@!�h@!p�@!J�@!8�@! \@ �@ ��@ Z@ <�@ (�@ ~@�
@��@��@Z�@@O@!-@��@�x@p;@�@�@��@�7@e,@/@��@�@Z@�A@��@U�@/�@S@��@B[@�j@�h@k�@-w@�v@��@bN@$@��@�@�@dZ@�@��@�@��@�R@h
@+k@�3@��@Dg@*0@�@�_@%�@�@�{@s@]�@9�@�c@��@�1@z@Ov@1�@�@�t@�@w2@@@��@��@h�@U2@$@x@�}@v`@$t@ i@҉@�@�b@W�@�@�@ �@�#@�7@4@��@��@9X@x@�@��@�P@|�@X�@8@.I@@
�@
�1@
�A@
h
@
_�@
6�@
4@	�@	��@	��@	��@	p�@	e,111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	H�B	H�B	H�B	H�B	HfB	HfB	HB	GzB	F�B	E�B	E9B	EB	D�B	DgB	D�B	EmB	E9B	E�B	E�B	ESB	E�B	FtB	FB	E�B	EB	D�B	D�B	BAB	}�B	B	�WB	�RB	��B	ݘB	��B
�B	��B	�QB
9B
;0B
I�B
��B
�B
��B
ƨB
��B
�[B
רB
�B{B(>B8BEBL�B^�BhsBn�BoOBk�Bd&Ba�B[�B^BM�B<BO�Bf�Ba|BX�BYKB\CBU�BHKB4�B./B&�B�B
�B
�B
�0B
��B
�B
��B
`B
0oB
�B	�B	�BB	�^B	��B	p�B	^jB	L�B	>�B	8lB	1vB	�B	�B��B�PB��B�FB�;B�B�B�B�B�0B��B�DB��B�OB��B� B�5B� B�5B��B�LB��B��B��B�$B�B�B��B��B��B��B��BʌB��BȚBԕB�4B�0B�iB�B��B��B�$B	B	0B	[B	�B	./B	9$B	=�B	>�B	N<B	N�B	NVB	YB	_VB	c B	fB	k�B	tnB	|�B	~�B	��B	�MB	�7B	�fB	��B	�QB	�]B	��B	��B	��B	�6B	��B	��B	��B	�B	��B	�aB	��B	āB	�[B	�AB	�B	��B	��B	�B	��B	��B	��B	�TB	��B	�)B	�
B	��B	��B	�B	��B	�=B	��B	�AB	�B	�MB	�;B	��B	��B	��B	��B	��B	��B	�-B	��B	��B	�kB	�B	ªB	�B	��B	�fB	ȀB	��B	�gB	�iB	�}B	��B	��B	�B	�TB	�B	��B	��B	�tB	��B	ϑB	�bB	�HB	� B	�B	�uB	ӏB	�B	�B	��B	�uB	�FB	�&B	ңB	ңB	�{B	�MB	��B	՛B	յB	ևB	ևB	�B	�SB	�B	՛B	ԯB	�B	��B	ѷB	�4B	� B	��B	�HB	�HB	�}B	�bB	��B	�[B	��B	�B	��B	�vB	�\B	�B	��B	ϑB	�\B	��B	�aB	��B	�MB	��B	�
B	�1B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�~B	�;B	��B	�'B	�\B	�|B	�-B	�-B	�B	�B	��B	�B	�nB	�B	�B	�,B	�`B	�`B	�`B	�`B	�B	��B	��B	��B	�2B	��B	�B	�B	�>B	�sB	��B	�B	�B	�B	�B	�eB	�KB	�KB	�B	�6B	�=B	�WB	�B	�B	�B	�B	��B	�]B	�cB	��B	�B	�B	�IB	�B	�IB	�5B	�UB	��B	�B	��B	�B	�[B	�B	��B	�B	�aB	�|B	�aB	��B	��B	�B	�B	�B	��B	�B	��B	��B	�B	��B	��B	�B	�ZB	�B	��B	��B	��B	��B	�LB	��B	��B	��B	��B	��B	�B	�B	�lB	�RB	��B	�$B	�>B	�rB	��B	��B	�JB	�VB	��B	�(B	�(B	��B	�cB
  B
 �B
�B
AB
�B
�B
�B
�B
uB
�B
B
�B
�B
�B
�B
gB
�B
B
9B
�B
YB
�B
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
�B
fB
�B
	�B
�B
�B
	lB
	�B

�B
)B
B
B
�B
�B
�B
�B
<B
�B
�B
BB
�B
�B
�B
bB
hB
�B
�B
 B
�B
�B
&B
[B
B
,B
FB
{B
�B
�B
9B
SB
9B
�B
?B
�B
+B
B
B
_B
yB
�B
eB
1B
eB
kB
�B
/B
5B
B
B
�B
�B
dB
�B
~B
B
dB
dB
�B
�B
!B
pB
 'B
 �B
 \B
!-B
!B
!�B
"NB
#�B
#�B
%,B
%FB
%�B
%�B
%�B
%�B
%�B
&B
&LB
&�B
&�B
'�B
(XB
(�B
)DB
(�B
)*B
)�B
*0B
*B
*B
*�B
+B
+�B
+�B
+QB
+QB
+�B
,�B
,�B
-B
.IB
./B
.cB
.�B
.�B
/B
/�B
/�B
0;B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
/�B
/�B
0oB
1AB
1'B
1B
1'B
1vB
1�B
1�B
1�B
1�B
1�B
2GB
2�B
2�B
2|B
2aB
2�B
3MB
3�B
3�B
49B
5B
5B
5B
5?B
5�B
5�B
5�B
5�B
6+B
5�B
6+B
6zB
6zB
6�B
6zB
6zB
72B
72B
7�B
8B
8B
8�B
9XB
9�B
9�B
9�B
9�B
9�B
9�B
:^B
<B
=�B
>wB
=�B
>B
>�B
>BB
>BB
>BB
>]B
>�B
>wB
>]B
>wB
>wB
>wB
>�B
>�B
>�B
?.B
?.B
?.B
?cB
?�B
@B
@iB
@�B
@�B
@�B
AUB
A�B
A�B
B'B
B'B
BB
B[B
B�B
B�B
CGB
C�B
C�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
EB
EB
EB
EmB
E�B
E�B
F�B
F�B
F�B
G+B
G_B
G�B
G�B
G�B
H1B
H�B
IRB
IlB
I7B
IB
I�B
I�B
J	B
J=B
J=B
J�B
K�B
K�B
K�B
K�B
LdB
L�B
L�B
L�B
M6B
M�B
M�B
NB
N"B
N�B
N�B
N�B
OB
O(B
OB
O\B
O�B
O�B
O�B
PB
P.B
P�B
P�B
QB
QhB
QhB
Q�B
R B
R:B
R:B
RoB
R�B
R�B
R�B
S&B
SB
S�B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
UB
U�B
U�B
VB
VB
V�B
V�B
W
B
W?B
WsB
W�B
W�B
W�B
X+B
X+B
XEB
X�B
X�B
YB
Y1B
Y1B
Y1B
Y1B
YKB
YB
Y�B
Y�B
ZB
ZB
Z7B
Z7B
ZkB
ZQB
[	B
[#B
[#B
[=B
[WB
[=B
[qB
[WB
[�B
[�B
[�B
[�B
[�B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
\�B
]IB
]IB
]dB
]dB
]�B
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^jB
^�B
^�B
^�B
_;B
_VB
_VB
_pB
_�B
_�B
_�B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
aHB
abB
abB
a|B
a�B
a�B
bB
b4B
b�B
b�B
b�B
c B
c:B
cTB
cTB
cTB
cnB
cnB
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e`B
e`B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
ffB
ffB
f�B
f�B
f�B
gB
g8B
g8B
g�B
h>B
hXB
h�B
h�B
h�B
i*B
iDB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
kkB
k�B
k�B
k�B
k�B
k�B
l"B
l"B
l"B
lqB
l�B
l�B
l�B
m)B
mCB
m]B
mwB
m�B
m�B
m�B
nIB
n�B
o B
o5B
o5B
oiB
o�B
pB
poB
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
r-B
rGB
rGB
r|B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
u?B
uZB
uZB
u�B
u�B
u�B
vB
v+B
v`B
v`B
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
xB
xB
xB
xRB
x�B
y	B
y>B
yXB
yXB
yrB
y�B
zB
zB
zB
z*B
z^B
z�B
{0B
{0B
{�B
{�B
|B
|6B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}VB
}VB
}VB
}VB
}�B
}�B
}�B
~B
~B
~(B
~BB
~(111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	H�B	H�B	H�B	H�B	HfB	HfB	HB	GzB	F�B	E�B	E9B	EB	D�B	DgB	D�B	EmB	E9B	E�B	E�B	ESB	E�B	FtB	FB	E�B	EB	D�B	D�B	BAB	}�B	B	�WB	�RB	��B	ݘB	��B
�B	��B	�QB
9B
;0B
I�B
��B
�B
��B
ƨB
��B
�[B
רB
�B{B(>B8BEBL�B^�BhsBn�BoOBk�Bd&Ba�B[�B^BM�B<BO�Bf�Ba|BX�BYKB\CBU�BHKB4�B./B&�B�B
�B
�B
�0B
��B
�B
��B
`B
0oB
�B	�B	�BB	�^B	��B	p�B	^jB	L�B	>�B	8lB	1vB	�B	�B��B�PB��B�FB�;B�B�B�B�B�0B��B�DB��B�OB��B� B�5B� B�5B��B�LB��B��B��B�$B�B�B��B��B��B��B��BʌB��BȚBԕB�4B�0B�iB�B��B��B�$B	B	0B	[B	�B	./B	9$B	=�B	>�B	N<B	N�B	NVB	YB	_VB	c B	fB	k�B	tnB	|�B	~�B	��B	�MB	�7B	�fB	��B	�QB	�]B	��B	��B	��B	�6B	��B	��B	��B	�B	��B	�aB	��B	āB	�[B	�AB	�B	��B	��B	�B	��B	��B	��B	�TB	��B	�)B	�
B	��B	��B	�B	��B	�=B	��B	�AB	�B	�MB	�;B	��B	��B	��B	��B	��B	��B	�-B	��B	��B	�kB	�B	ªB	�B	��B	�fB	ȀB	��B	�gB	�iB	�}B	��B	��B	�B	�TB	�B	��B	��B	�tB	��B	ϑB	�bB	�HB	� B	�B	�uB	ӏB	�B	�B	��B	�uB	�FB	�&B	ңB	ңB	�{B	�MB	��B	՛B	յB	ևB	ևB	�B	�SB	�B	՛B	ԯB	�B	��B	ѷB	�4B	� B	��B	�HB	�HB	�}B	�bB	��B	�[B	��B	�B	��B	�vB	�\B	�B	��B	ϑB	�\B	��B	�aB	��B	�MB	��B	�
B	�1B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�~B	�;B	��B	�'B	�\B	�|B	�-B	�-B	�B	�B	��B	�B	�nB	�B	�B	�,B	�`B	�`B	�`B	�`B	�B	��B	��B	��B	�2B	��B	�B	�B	�>B	�sB	��B	�B	�B	�B	�B	�eB	�KB	�KB	�B	�6B	�=B	�WB	�B	�B	�B	�B	��B	�]B	�cB	��B	�B	�B	�IB	�B	�IB	�5B	�UB	��B	�B	��B	�B	�[B	�B	��B	�B	�aB	�|B	�aB	��B	��B	�B	�B	�B	��B	�B	��B	��B	�B	��B	��B	�B	�ZB	�B	��B	��B	��B	��B	�LB	��B	��B	��B	��B	��B	�B	�B	�lB	�RB	��B	�$B	�>B	�rB	��B	��B	�JB	�VB	��B	�(B	�(B	��B	�cB
  B
 �B
�B
AB
�B
�B
�B
�B
uB
�B
B
�B
�B
�B
�B
gB
�B
B
9B
�B
YB
�B
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
�B
fB
�B
	�B
�B
�B
	lB
	�B

�B
)B
B
B
�B
�B
�B
�B
<B
�B
�B
BB
�B
�B
�B
bB
hB
�B
�B
 B
�B
�B
&B
[B
B
,B
FB
{B
�B
�B
9B
SB
9B
�B
?B
�B
+B
B
B
_B
yB
�B
eB
1B
eB
kB
�B
/B
5B
B
B
�B
�B
dB
�B
~B
B
dB
dB
�B
�B
!B
pB
 'B
 �B
 \B
!-B
!B
!�B
"NB
#�B
#�B
%,B
%FB
%�B
%�B
%�B
%�B
%�B
&B
&LB
&�B
&�B
'�B
(XB
(�B
)DB
(�B
)*B
)�B
*0B
*B
*B
*�B
+B
+�B
+�B
+QB
+QB
+�B
,�B
,�B
-B
.IB
./B
.cB
.�B
.�B
/B
/�B
/�B
0;B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
/�B
/�B
0oB
1AB
1'B
1B
1'B
1vB
1�B
1�B
1�B
1�B
1�B
2GB
2�B
2�B
2|B
2aB
2�B
3MB
3�B
3�B
49B
5B
5B
5B
5?B
5�B
5�B
5�B
5�B
6+B
5�B
6+B
6zB
6zB
6�B
6zB
6zB
72B
72B
7�B
8B
8B
8�B
9XB
9�B
9�B
9�B
9�B
9�B
9�B
:^B
<B
=�B
>wB
=�B
>B
>�B
>BB
>BB
>BB
>]B
>�B
>wB
>]B
>wB
>wB
>wB
>�B
>�B
>�B
?.B
?.B
?.B
?cB
?�B
@B
@iB
@�B
@�B
@�B
AUB
A�B
A�B
B'B
B'B
BB
B[B
B�B
B�B
CGB
C�B
C�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
EB
EB
EB
EmB
E�B
E�B
F�B
F�B
F�B
G+B
G_B
G�B
G�B
G�B
H1B
H�B
IRB
IlB
I7B
IB
I�B
I�B
J	B
J=B
J=B
J�B
K�B
K�B
K�B
K�B
LdB
L�B
L�B
L�B
M6B
M�B
M�B
NB
N"B
N�B
N�B
N�B
OB
O(B
OB
O\B
O�B
O�B
O�B
PB
P.B
P�B
P�B
QB
QhB
QhB
Q�B
R B
R:B
R:B
RoB
R�B
R�B
R�B
S&B
SB
S�B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
UB
U�B
U�B
VB
VB
V�B
V�B
W
B
W?B
WsB
W�B
W�B
W�B
X+B
X+B
XEB
X�B
X�B
YB
Y1B
Y1B
Y1B
Y1B
YKB
YB
Y�B
Y�B
ZB
ZB
Z7B
Z7B
ZkB
ZQB
[	B
[#B
[#B
[=B
[WB
[=B
[qB
[WB
[�B
[�B
[�B
[�B
[�B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
\�B
]IB
]IB
]dB
]dB
]�B
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^jB
^�B
^�B
^�B
_;B
_VB
_VB
_pB
_�B
_�B
_�B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
aHB
abB
abB
a|B
a�B
a�B
bB
b4B
b�B
b�B
b�B
c B
c:B
cTB
cTB
cTB
cnB
cnB
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e`B
e`B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
ffB
ffB
f�B
f�B
f�B
gB
g8B
g8B
g�B
h>B
hXB
h�B
h�B
h�B
i*B
iDB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
kkB
k�B
k�B
k�B
k�B
k�B
l"B
l"B
l"B
lqB
l�B
l�B
l�B
m)B
mCB
m]B
mwB
m�B
m�B
m�B
nIB
n�B
o B
o5B
o5B
oiB
o�B
pB
poB
p�B
p�B
p�B
qB
qAB
q[B
q�B
q�B
r-B
rGB
rGB
r|B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
u?B
uZB
uZB
u�B
u�B
u�B
vB
v+B
v`B
v`B
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
xB
xB
xB
xRB
x�B
y	B
y>B
yXB
yXB
yrB
y�B
zB
zB
zB
z*B
z^B
z�B
{0B
{0B
{�B
{�B
|B
|6B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}VB
}VB
}VB
}VB
}�B
}�B
}�B
~B
~B
~(B
~BB
~(111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105229  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191353  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191353  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191353                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041401  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041401  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:40:48Z creation;2022-06-04T17:40:48Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174048  20220610131508  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @٢>��?1   @٢�Eg�@.�33333�cW
=p��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ��B  B  B  B   B*  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�ffB�33B���B�  B�  B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�ffB���B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�3C-ffC/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ�C\�C^�C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�B �B�HB�HB�HB�HB)�HB/z�B7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B�W
B�#�B��qB��B��B��qB��B��B��B��B��B��>B��qB��B��B��B��B��B��B�#�B��B۽qB��B��B��B�W
B�qB��B��B��B��C�RC�RC�RC޸C	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC*�C,��C-^�C/޸C1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCP�CQ�RCS�RCU�RCW�RCZ�C\�C^�C_޸Ca�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�Dw�D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DFzDF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
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
D��
D�?
D�
D��
D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��OA�ƨA�ԕA��mA�ԕA���A��A���A��|A���A��|A���A��sA��EA�ںA��A���A���AӨ�AӶAӜ�A�\�Aѕ�A�%zA�t�A��AϸA�`�A�/�A��AΓ�A���A�NAǋxA�($A�	�A��A���A��GA�S[A��A��A���A��1A�h�A�qvA���A�K�A��<A���A�Y�A���A�,qA��A�q�A���A��A�%A��TA��A�ѷA���A�l�A�.A���A�o5A��>A���A�&A��MA��oA�{A���A��A�l�A�K�A�o�A��A��A�ӏA�%�A��rA�XA�*�A��?A���A�)*A}=A|��A{kQAzPHAv��ArbAn�Ae_A`��A[QAU^�AP��AL�AI(AH��AGخAG@�AF�AE��AD�AC�AAB��AA�A>:�A;�A6�.A5�qA4��A42aA2�	A2y�A2&A1�A1�-A1�FA1_pA18A0�AA/�A-�A,͟A,GEA+OA)`BA&z�A$5?A#|�A$N<A$��A$��A#�A#:�A"$tA"5�A!��A!��A!qvA!�<A!�)A!�"A"(�A":*A"#�A!�PA!��A!��A!/�A �vA ��A��A/�A�A�AqAVA��A�!A�A� A"hA��A'RA�dAB[A��A7�A��AR�As�A�A��Aw�A^�A	lA��A�AAbNA��A4nA7LA�WA�=A�oA^�Ac�A
�hA
E9A	 �Aw2A@OA�A($AB[A?�A�-A�OA�A!A�QA �A �hA 0�@��	@��@�2�@�F�@��I@�@��-@���@��-@�IR@��d@�K^@�"�@��!@���@��!@��F@��o@�%@��@��}@�:�@��3@�)_@�@�z�@��@�=�@�a|@��o@�=@�\�@�%F@�	l@�{@�F@�=�@��@�Ta@�-@�@�2�@�~@�@�z�@�%�@���@�)_@�M@��+@�Dg@��@��)@�O�@�@�dZ@��@ޖ�@�3�@�c�@ܑ�@��r@��@��@�B�@�x@ײ-@�e,@��@�Xy@Ցh@��H@�c @�˒@�A�@�xl@�M@�qv@��@���@�0U@�s@�_p@�!-@�|�@�4n@�}�@� i@�͟@̃@�&�@��@ʖ�@�GE@���@��]@Ș_@�1�@Ǧ�@ǝ�@�c�@��/@Ɗr@�S�@�M@ũ*@ŏ�@�#�@�~@Úk@�{J@�iD@�T�@�@O@�+@���@@�#:@�?@��@���@�=�@�)_@�/@���@�E�@� �@�>�@�q@��[@�(@��+@�Z@�3�@��@���@���@�*0@�5�@�Dg@��H@�w�@�Q@�4@�p�@��4@�U2@�/�@��Z@���@�/@�+@���@�1�@��W@���@�A�@���@�@�@��	@�B�@�-w@��@���@�;�@���@�5�@��@���@��7@�&@���@��@��h@�O�@�/�@���@��@�d�@��@��@���@��^@�dZ@��@��H@��@�J�@�7@�x@��>@���@�33@���@���@�g8@��@�v`@�E9@���@�^5@�H�@��&@��h@�Y�@�(@���@��O@��x@���@���@�Z�@�A�@�$�@��m@��[@��@���@�s�@��"@���@��@�V@�
�@��@���@�t�@�N<@�S@��'@�c @��@��@���@�%F@��@���@�l"@�*�@���@���@�y�@�j@�J#@�(@�ѷ@�s�@�V@�/�@�˒@���@���@�+@��<@��@�v�@�W�@�M@���@��H@�x�@�O@�ߤ@���@�>B@�@���@�s@���@���@�]d@��@��@�J�@�o@��2@��@��x@�v�@�M@��@���@��@���@���@��+@�;�@�1@��@�@�u@��@��'@��@���@�j@�Q�@�*�@��+@���@�&�@�(@� i@��|@���@�q@��@���@�T�@���@��<@��1@�~(@�L0@�($@��g@���@���@�w2@�a@�/�@��@��O@��@�PH@�
�@�ԕ@�ƨ@���@��=@�s@��@��@��@��@�c�@���@��d@��n@�S&@�!-@��@��@��p@��j@���@�H@�  @��z@���@�n/@��@��c@��m@���@��.@�7�@��*@�a@�7L@� i@���@�Xy@�b@�	@���@�m]@�K�@�)_@��@��$@�q�@�m�@�N�@�?@�@J#@~�@~�@}�D@}��@}e,@}�@|��@|�z@|z�@|7@{˒@{�@{(@z�F@z5?@y��@yu�@y=�@xی@x!@w��@w�@v�@v\�@u�@u|@t�@t>B@s�@st�@s�@r�F@q�.@qϫ@q��@q��@q��@qB�@p~(@o�@@oW?@n��@nV@m��@l�@lr�@l�@k��@k�F@kH�@js�@i��@i��@iS&@iS&@iO�@i?}@h�	@hc�@h/�@h  @g��@g�@fM�@fu@e�-@eN<@d�v@d�@d�@c�}@co�@c@O@b��@bp;@a��@a��@ax�@a\�@a!�@a	l@`�4@`$@_�@_�6@__p@^�2@^��@^H�@^&�@]��@]!�@\��@\Ft@\@[��@[��@[Y@Z��@ZQ@Y�j@Y?}@Y�@XV�@Wx@V��@V��@V� @Vv�@Va|@U�h@TɆ@T|�@S�&@Sg�@R�@Ri�@Q��@Q��@Q;@Pj@P	�@O��@O'�@N�<@Np;@M��@M��@MY�@L�|@L�z@K��@Kl�@J�y@J��@I�N@I��@I��@I��@IA @H��@H �@H1@G�]@G�:@GC@F�@F{�@Fff@F?@E��@E��@EA @D��@D�@D��@DPH@D<�@C��@Cqv@C�@B��@BYK@B!�@A�@Am]@A+@@Ĝ@@�@@4n@?��@?�0@?�*@?��@?)_@>��@>��@>J�@=��@=�=@=��@=rG@=B�@<�K@<�.@;��@;��@;�P@;�4@;a@;.I@;�@:�@:�X@:�@:_�@:�@9�@9��@9V@8�`@8�[@8�9@8�D@8M@7�@7�	@7C@6;�@6e@5o @4��@4��@4ی@4��@4D�@3� @3��@3��@3��@31�@2͟@2.�@1��@1��@1�@1@@0�@0,=@07@/�g@/��@.�c@.�@.~�@.ff@.1�@-ϫ@-�7@-Vm@-@@,�@,�@,~@+ݘ@+�F@+��@+n/@+9�@*��@*	@)�@)��@)x�@)N<@(�p@(z�@( �@'�6@'��@'x@'(@&��@&\�@&0U@&@%��@%�"@%`B@$�E@$"h@#�:@#.I@"�@"�6@"�x@"z@"q�@"c @!��@!��@!j@!?}@ �z@ M@ݘ@��@�P@&@͟@� @s�@YK@�@�@�j@��@=�@�@H@��@�@��@��@g�@4�@(@�m@�L@xl@�@e@��@�#@@�S@hs@N<@?}@ \@�@�e@!@ݘ@��@{J@U�@'�@�@��@��@xl@H�@�@��@hs@<6@��@�j@��@��@�4@�.@u�@K^@*�@x@�W@�K@��@�4@j�@E9@�"@�@�<@��@&�@�S@B�@?}@7L@*0@�@�?@u�@:�@�V@dZ@�@�<@{�@a|@B[@#:@�@�@�t@��@Dg@ѷ@y>@S�@6@%�@~@�@�&@�
@�w@��@@O@'�@
��@
�,@
�F@
n�@
Ta@
M�@
E�@
-@	��@	x�@	?}@�5@��@y>@tT@oi@e�@�@�A@�:@Z�@@ں@҉@�'@��@��@h
@^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��OA�ƨA�ԕA��mA�ԕA���A��A���A��|A���A��|A���A��sA��EA�ںA��A���A���AӨ�AӶAӜ�A�\�Aѕ�A�%zA�t�A��AϸA�`�A�/�A��AΓ�A���A�NAǋxA�($A�	�A��A���A��GA�S[A��A��A���A��1A�h�A�qvA���A�K�A��<A���A�Y�A���A�,qA��A�q�A���A��A�%A��TA��A�ѷA���A�l�A�.A���A�o5A��>A���A�&A��MA��oA�{A���A��A�l�A�K�A�o�A��A��A�ӏA�%�A��rA�XA�*�A��?A���A�)*A}=A|��A{kQAzPHAv��ArbAn�Ae_A`��A[QAU^�AP��AL�AI(AH��AGخAG@�AF�AE��AD�AC�AAB��AA�A>:�A;�A6�.A5�qA4��A42aA2�	A2y�A2&A1�A1�-A1�FA1_pA18A0�AA/�A-�A,͟A,GEA+OA)`BA&z�A$5?A#|�A$N<A$��A$��A#�A#:�A"$tA"5�A!��A!��A!qvA!�<A!�)A!�"A"(�A":*A"#�A!�PA!��A!��A!/�A �vA ��A��A/�A�A�AqAVA��A�!A�A� A"hA��A'RA�dAB[A��A7�A��AR�As�A�A��Aw�A^�A	lA��A�AAbNA��A4nA7LA�WA�=A�oA^�Ac�A
�hA
E9A	 �Aw2A@OA�A($AB[A?�A�-A�OA�A!A�QA �A �hA 0�@��	@��@�2�@�F�@��I@�@��-@���@��-@�IR@��d@�K^@�"�@��!@���@��!@��F@��o@�%@��@��}@�:�@��3@�)_@�@�z�@��@�=�@�a|@��o@�=@�\�@�%F@�	l@�{@�F@�=�@��@�Ta@�-@�@�2�@�~@�@�z�@�%�@���@�)_@�M@��+@�Dg@��@��)@�O�@�@�dZ@��@ޖ�@�3�@�c�@ܑ�@��r@��@��@�B�@�x@ײ-@�e,@��@�Xy@Ցh@��H@�c @�˒@�A�@�xl@�M@�qv@��@���@�0U@�s@�_p@�!-@�|�@�4n@�}�@� i@�͟@̃@�&�@��@ʖ�@�GE@���@��]@Ș_@�1�@Ǧ�@ǝ�@�c�@��/@Ɗr@�S�@�M@ũ*@ŏ�@�#�@�~@Úk@�{J@�iD@�T�@�@O@�+@���@@�#:@�?@��@���@�=�@�)_@�/@���@�E�@� �@�>�@�q@��[@�(@��+@�Z@�3�@��@���@���@�*0@�5�@�Dg@��H@�w�@�Q@�4@�p�@��4@�U2@�/�@��Z@���@�/@�+@���@�1�@��W@���@�A�@���@�@�@��	@�B�@�-w@��@���@�;�@���@�5�@��@���@��7@�&@���@��@��h@�O�@�/�@���@��@�d�@��@��@���@��^@�dZ@��@��H@��@�J�@�7@�x@��>@���@�33@���@���@�g8@��@�v`@�E9@���@�^5@�H�@��&@��h@�Y�@�(@���@��O@��x@���@���@�Z�@�A�@�$�@��m@��[@��@���@�s�@��"@���@��@�V@�
�@��@���@�t�@�N<@�S@��'@�c @��@��@���@�%F@��@���@�l"@�*�@���@���@�y�@�j@�J#@�(@�ѷ@�s�@�V@�/�@�˒@���@���@�+@��<@��@�v�@�W�@�M@���@��H@�x�@�O@�ߤ@���@�>B@�@���@�s@���@���@�]d@��@��@�J�@�o@��2@��@��x@�v�@�M@��@���@��@���@���@��+@�;�@�1@��@�@�u@��@��'@��@���@�j@�Q�@�*�@��+@���@�&�@�(@� i@��|@���@�q@��@���@�T�@���@��<@��1@�~(@�L0@�($@��g@���@���@�w2@�a@�/�@��@��O@��@�PH@�
�@�ԕ@�ƨ@���@��=@�s@��@��@��@��@�c�@���@��d@��n@�S&@�!-@��@��@��p@��j@���@�H@�  @��z@���@�n/@��@��c@��m@���@��.@�7�@��*@�a@�7L@� i@���@�Xy@�b@�	@���@�m]@�K�@�)_@��@��$@�q�@�m�@�N�@�?@�@J#@~�@~�@}�D@}��@}e,@}�@|��@|�z@|z�@|7@{˒@{�@{(@z�F@z5?@y��@yu�@y=�@xی@x!@w��@w�@v�@v\�@u�@u|@t�@t>B@s�@st�@s�@r�F@q�.@qϫ@q��@q��@q��@qB�@p~(@o�@@oW?@n��@nV@m��@l�@lr�@l�@k��@k�F@kH�@js�@i��@i��@iS&@iS&@iO�@i?}@h�	@hc�@h/�@h  @g��@g�@fM�@fu@e�-@eN<@d�v@d�@d�@c�}@co�@c@O@b��@bp;@a��@a��@ax�@a\�@a!�@a	l@`�4@`$@_�@_�6@__p@^�2@^��@^H�@^&�@]��@]!�@\��@\Ft@\@[��@[��@[Y@Z��@ZQ@Y�j@Y?}@Y�@XV�@Wx@V��@V��@V� @Vv�@Va|@U�h@TɆ@T|�@S�&@Sg�@R�@Ri�@Q��@Q��@Q;@Pj@P	�@O��@O'�@N�<@Np;@M��@M��@MY�@L�|@L�z@K��@Kl�@J�y@J��@I�N@I��@I��@I��@IA @H��@H �@H1@G�]@G�:@GC@F�@F{�@Fff@F?@E��@E��@EA @D��@D�@D��@DPH@D<�@C��@Cqv@C�@B��@BYK@B!�@A�@Am]@A+@@Ĝ@@�@@4n@?��@?�0@?�*@?��@?)_@>��@>��@>J�@=��@=�=@=��@=rG@=B�@<�K@<�.@;��@;��@;�P@;�4@;a@;.I@;�@:�@:�X@:�@:_�@:�@9�@9��@9V@8�`@8�[@8�9@8�D@8M@7�@7�	@7C@6;�@6e@5o @4��@4��@4ی@4��@4D�@3� @3��@3��@3��@31�@2͟@2.�@1��@1��@1�@1@@0�@0,=@07@/�g@/��@.�c@.�@.~�@.ff@.1�@-ϫ@-�7@-Vm@-@@,�@,�@,~@+ݘ@+�F@+��@+n/@+9�@*��@*	@)�@)��@)x�@)N<@(�p@(z�@( �@'�6@'��@'x@'(@&��@&\�@&0U@&@%��@%�"@%`B@$�E@$"h@#�:@#.I@"�@"�6@"�x@"z@"q�@"c @!��@!��@!j@!?}@ �z@ M@ݘ@��@�P@&@͟@� @s�@YK@�@�@�j@��@=�@�@H@��@�@��@��@g�@4�@(@�m@�L@xl@�@e@��@�#@@�S@hs@N<@?}@ \@�@�e@!@ݘ@��@{J@U�@'�@�@��@��@xl@H�@�@��@hs@<6@��@�j@��@��@�4@�.@u�@K^@*�@x@�W@�K@��@�4@j�@E9@�"@�@�<@��@&�@�S@B�@?}@7L@*0@�@�?@u�@:�@�V@dZ@�@�<@{�@a|@B[@#:@�@�@�t@��@Dg@ѷ@y>@S�@6@%�@~@�@�&@�
@�w@��@@O@'�@
��@
�,@
�F@
n�@
Ta@
M�@
E�@
-@	��@	x�@	?}@�5@��@y>@tT@oi@e�@�@�A@�:@Z�@@ں@҉@�'@��@��@h
@^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B?B>�B>]B>]B>�B>]B>BB>]B?�B?HBA�BA�B>�B>�B?.BKxB[	B��B�B��B	�XB
*0BS�B`BB��B�{B�!B�\B�B�@B��B��Ba|BYBFB@�B9$B;JB<BD�BS�Bv�B�B�=B�OB��B�ZB�hBچB�)B�B�	B�RB��BՁB��B	B6B�B
�BAB��B�B�EB�SB��B��B��B�3Bs�Bf�BV�BG�B2-B'�B�B	�B
��B
�CB
רB
ϑB
��B
��B
�7B
��B
WsB
>BB
-B	��B	��B	��B	چB	��B	�vB	xRB	�sB	�B	`�B	M�B	?B	<�B	C-B	V9B	_�B	h>B	g8B	`vB	U�B	J�B	G�B	W�B	r�B	wfB	w�B	~�B	��B	�5B	��B	�8B	�B	��B	��B	�B	��B	�RB
�B
B

rB

�B
B
�B	�B	�	B	�{B	� B
�B
�B
�B
~B
�B
_B
�B
JB
B
yB
IB
!B
%,B
7�B
<�B
B[B
IRB
H�B
I�B
H�B
IlB
H�B
GB
G_B
F�B
G�B
F%B
E�B
D�B
F�B
FB
E�B
D�B
@OB
;dB
0!B
'�B
&�B
$&B
 �B
�B
�B
�B
EB
_B
SB
�B
�B
�B
�B	�*B	��B	��B	�B	��B	�B	��B	ݘB	��B	�!B	�=B	�QB	�B	�B	յB	�hB	��B	��B	�_B	�mB	�B	�aB	ԯB	ԕB	ӏB	��B	�&B	��B	� B	��B	�=B	ܬB	߾B	�nB	�\B	ܒB	ٴB	�EB	�qB	��B	�~B	��B	�B	�B	�NB	��B	��B	�5B	�)B	��B	�B	��B	�ZB	�NB	�NB	��B	�B	�B	��B	�/B	�QB	�WB	��B	�B	ٴB	�EB	ؓB	�EB	ٚB	��B	�eB	��B	�+B	��B	��B	רB	��B	��B	�YB	�B	�B	��B	��B	�_B	��B	�yB	�B	��B	�yB	�B	�B	�B	�QB	�#B	�	B	�7B	��B	�eB	ںB	�kB	�]B	��B	ބB	�;B	ߊB	�'B	�B	�BB	�VB	�VB	�|B	��B	�vB	�B	��B	�nB	��B	�B	�B	�-B	�bB	�4B	��B	�B	�B	��B	�zB	��B	�2B	��B	�zB	��B	�B	�`B	��B	�2B	�2B	�2B	�B	�B	�5B	�}B	��B	�B	�|B	�B	�B	�3B	�B	�B	��B	�B	�nB	�aB	�aB	�B	�aB	�B	��B	��B	�zB	�2B	��B	�dB	�B	�PB	�6B	�6B	��B	�"B	�<B	�"B	��B	�B	�wB	�]B	�(B	��B	�HB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	�wB	�"B	��B	��B	�JB	�*B	�*B	�DB	��B	�DB	�0B	�B	��B	�HB
  B
 OB
 �B
 �B
 �B
�B
�B
�B
aB
�B
�B
�B
B
�B
�B
aB
�B
'B
 B
 iB
 OB	�B	��B	�(B	�BB	��B	��B	��B	��B	�B	��B	��B	��B	�cB	�cB	�}B
  B
 4B
 4B
 B	��B
 �B
 �B
 �B
 �B
 B
;B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
B
SB
�B
%B
?B
YB
�B
�B
�B
tB
�B
�B
EB
zB
�B
zB
�B
�B
�B
�B
�B
�B
B
fB
1B
�B
	RB
	�B
	B
	B
	B
	7B
	�B
	�B
B
�B
~B
�B
PB
jB
jB
�B
�B
�B
4B
�B
NB
�B
oB
 B
[B
{B
B
�B
B
B
B
qB
�B
�B
�B
IB
�B
VB
;B
;B
;B
�B
 �B
!-B
!�B
"4B
"�B
#B
#�B
#�B
$B
$B
#�B
$@B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%FB
%FB
%,B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'B
'B
'8B
($B
(>B
(sB
)*B
)_B
)�B
)�B
)�B
)�B
*KB
+B
+�B
+�B
,"B
,=B
,�B
-]B
-]B
-]B
-)B
-�B
.�B
.�B
/ B
/B
/�B
0!B
0�B
0�B
1vB
1�B
1�B
2GB
2|B
2GB
2B
2�B
3�B
3�B
3�B
3B
33B
3MB
3B
3�B
4nB
5tB
6FB
6FB
6zB
6�B
6�B
7LB
6�B
7B
7B
72B
7�B
7�B
8B
7�B
8B
7�B
7�B
7�B
7�B
7�B
7�B
88B
8RB
8RB
8lB
8�B
8�B
8�B
8�B
8�B
9	B
9	B
9>B
9�B
9�B
:�B
:�B
;0B
;�B
;�B
;�B
;�B
<6B
<�B
=�B
>(B
>�B
>�B
>�B
>�B
>�B
>�B
?cB
?}B
?}B
?�B
@B
@�B
@�B
@�B
A B
AoB
A�B
BB
B[B
B�B
BuB
B�B
CGB
CGB
C�B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
DgB
D�B
E9B
E9B
EmB
EmB
EmB
FYB
F�B
F�B
F�B
F�B
F�B
G_B
G�B
G�B
H1B
H�B
H�B
IB
I�B
JXB
J�B
J=B
JXB
JXB
K)B
K�B
K�B
K�B
LdB
L�B
MB
MjB
M�B
NB
NVB
N�B
N�B
OBB
O�B
O�B
P.B
P.B
PbB
Q B
QNB
Q�B
Q�B
R B
RTB
R�B
SB
SB
R�B
R�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VmB
VmB
V�B
V�B
V�B
V�B
W?B
W?B
WYB
WsB
WYB
W�B
W�B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YeB
YKB
YKB
ZB
Z7B
Y�B
ZB
ZB
ZQB
Z�B
Z7B
Z�B
Z�B
ZB
Z7B
Z�B
[=B
[	B
[=B
[WB
[�B
[�B
[�B
[�B
\B
\xB
\)B
\CB
\)B
\]B
\�B
]/B
]/B
]�B
]B
]B
]dB
]�B
^B
^jB
^�B
_�B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`'B
`vB
`\B
`�B
`�B
aB
`�B
aB
aB
a|B
a�B
a�B
a�B
a�B
a�B
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
dZB
d�B
eB
eFB
e,B
e,B
e�B
e�B
fB
fLB
f�B
ffB
f�B
g8B
g�B
g�B
g�B
g�B
h>B
hXB
i*B
i�B
jeB
j�B
kB
k�B
k�B
k�B
k�B
k�B
l=B
lqB
l�B
l�B
m�B
m�B
n/B
nIB
nIB
n�B
o5B
oOB
oOB
oiB
o�B
o�B
o�B
p!B
poB
p�B
qvB
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
tB
tB
tTB
tnB
tnB
t�B
t�B
uB
u�B
u�B
u�B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
xB
x8B
xRB
xRB
x�B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
yrB
y�B
y�B
y�B
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{�B
|B
|B
|6B
|PB
|�B
|�B
|�B
|�B
}"B
}qB
}�B
~BB
~�B
~�B
~�B
~�B
~�B
.B
.B
HB
�B
� B
� B
��B
��B
��B
� B
�;B
�UB
�oB
�oB
��B
�[B
�uB
�uB
��B
��B
�aB
�GB
�aB
��B
��B
��B
��B
�B
�MB
��B
��B
�B
�mB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B?B>�B>]B>]B>�B>]B>BB>]B?�B?HBA�BA�B>�B>�B?.BKxB[	B��B�B��B	�XB
*0BS�B`BB��B�{B�!B�\B�B�@B��B��Ba|BYBFB@�B9$B;JB<BD�BS�Bv�B�B�=B�OB��B�ZB�hBچB�)B�B�	B�RB��BՁB��B	B6B�B
�BAB��B�B�EB�SB��B��B��B�3Bs�Bf�BV�BG�B2-B'�B�B	�B
��B
�CB
רB
ϑB
��B
��B
�7B
��B
WsB
>BB
-B	��B	��B	��B	چB	��B	�vB	xRB	�sB	�B	`�B	M�B	?B	<�B	C-B	V9B	_�B	h>B	g8B	`vB	U�B	J�B	G�B	W�B	r�B	wfB	w�B	~�B	��B	�5B	��B	�8B	�B	��B	��B	�B	��B	�RB
�B
B

rB

�B
B
�B	�B	�	B	�{B	� B
�B
�B
�B
~B
�B
_B
�B
JB
B
yB
IB
!B
%,B
7�B
<�B
B[B
IRB
H�B
I�B
H�B
IlB
H�B
GB
G_B
F�B
G�B
F%B
E�B
D�B
F�B
FB
E�B
D�B
@OB
;dB
0!B
'�B
&�B
$&B
 �B
�B
�B
�B
EB
_B
SB
�B
�B
�B
�B	�*B	��B	��B	�B	��B	�B	��B	ݘB	��B	�!B	�=B	�QB	�B	�B	յB	�hB	��B	��B	�_B	�mB	�B	�aB	ԯB	ԕB	ӏB	��B	�&B	��B	� B	��B	�=B	ܬB	߾B	�nB	�\B	ܒB	ٴB	�EB	�qB	��B	�~B	��B	�B	�B	�NB	��B	��B	�5B	�)B	��B	�B	��B	�ZB	�NB	�NB	��B	�B	�B	��B	�/B	�QB	�WB	��B	�B	ٴB	�EB	ؓB	�EB	ٚB	��B	�eB	��B	�+B	��B	��B	רB	��B	��B	�YB	�B	�B	��B	��B	�_B	��B	�yB	�B	��B	�yB	�B	�B	�B	�QB	�#B	�	B	�7B	��B	�eB	ںB	�kB	�]B	��B	ބB	�;B	ߊB	�'B	�B	�BB	�VB	�VB	�|B	��B	�vB	�B	��B	�nB	��B	�B	�B	�-B	�bB	�4B	��B	�B	�B	��B	�zB	��B	�2B	��B	�zB	��B	�B	�`B	��B	�2B	�2B	�2B	�B	�B	�5B	�}B	��B	�B	�|B	�B	�B	�3B	�B	�B	��B	�B	�nB	�aB	�aB	�B	�aB	�B	��B	��B	�zB	�2B	��B	�dB	�B	�PB	�6B	�6B	��B	�"B	�<B	�"B	��B	�B	�wB	�]B	�(B	��B	�HB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	�wB	�"B	��B	��B	�JB	�*B	�*B	�DB	��B	�DB	�0B	�B	��B	�HB
  B
 OB
 �B
 �B
 �B
�B
�B
�B
aB
�B
�B
�B
B
�B
�B
aB
�B
'B
 B
 iB
 OB	�B	��B	�(B	�BB	��B	��B	��B	��B	�B	��B	��B	��B	�cB	�cB	�}B
  B
 4B
 4B
 B	��B
 �B
 �B
 �B
 �B
 B
;B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
B
SB
�B
%B
?B
YB
�B
�B
�B
tB
�B
�B
EB
zB
�B
zB
�B
�B
�B
�B
�B
�B
B
fB
1B
�B
	RB
	�B
	B
	B
	B
	7B
	�B
	�B
B
�B
~B
�B
PB
jB
jB
�B
�B
�B
4B
�B
NB
�B
oB
 B
[B
{B
B
�B
B
B
B
qB
�B
�B
�B
IB
�B
VB
;B
;B
;B
�B
 �B
!-B
!�B
"4B
"�B
#B
#�B
#�B
$B
$B
#�B
$@B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%FB
%FB
%,B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'B
'B
'8B
($B
(>B
(sB
)*B
)_B
)�B
)�B
)�B
)�B
*KB
+B
+�B
+�B
,"B
,=B
,�B
-]B
-]B
-]B
-)B
-�B
.�B
.�B
/ B
/B
/�B
0!B
0�B
0�B
1vB
1�B
1�B
2GB
2|B
2GB
2B
2�B
3�B
3�B
3�B
3B
33B
3MB
3B
3�B
4nB
5tB
6FB
6FB
6zB
6�B
6�B
7LB
6�B
7B
7B
72B
7�B
7�B
8B
7�B
8B
7�B
7�B
7�B
7�B
7�B
7�B
88B
8RB
8RB
8lB
8�B
8�B
8�B
8�B
8�B
9	B
9	B
9>B
9�B
9�B
:�B
:�B
;0B
;�B
;�B
;�B
;�B
<6B
<�B
=�B
>(B
>�B
>�B
>�B
>�B
>�B
>�B
?cB
?}B
?}B
?�B
@B
@�B
@�B
@�B
A B
AoB
A�B
BB
B[B
B�B
BuB
B�B
CGB
CGB
C�B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
DgB
D�B
E9B
E9B
EmB
EmB
EmB
FYB
F�B
F�B
F�B
F�B
F�B
G_B
G�B
G�B
H1B
H�B
H�B
IB
I�B
JXB
J�B
J=B
JXB
JXB
K)B
K�B
K�B
K�B
LdB
L�B
MB
MjB
M�B
NB
NVB
N�B
N�B
OBB
O�B
O�B
P.B
P.B
PbB
Q B
QNB
Q�B
Q�B
R B
RTB
R�B
SB
SB
R�B
R�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VmB
VmB
V�B
V�B
V�B
V�B
W?B
W?B
WYB
WsB
WYB
W�B
W�B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YeB
YKB
YKB
ZB
Z7B
Y�B
ZB
ZB
ZQB
Z�B
Z7B
Z�B
Z�B
ZB
Z7B
Z�B
[=B
[	B
[=B
[WB
[�B
[�B
[�B
[�B
\B
\xB
\)B
\CB
\)B
\]B
\�B
]/B
]/B
]�B
]B
]B
]dB
]�B
^B
^jB
^�B
_�B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`'B
`vB
`\B
`�B
`�B
aB
`�B
aB
aB
a|B
a�B
a�B
a�B
a�B
a�B
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
dZB
d�B
eB
eFB
e,B
e,B
e�B
e�B
fB
fLB
f�B
ffB
f�B
g8B
g�B
g�B
g�B
g�B
h>B
hXB
i*B
i�B
jeB
j�B
kB
k�B
k�B
k�B
k�B
k�B
l=B
lqB
l�B
l�B
m�B
m�B
n/B
nIB
nIB
n�B
o5B
oOB
oOB
oiB
o�B
o�B
o�B
p!B
poB
p�B
qvB
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
tB
tB
tTB
tnB
tnB
t�B
t�B
uB
u�B
u�B
u�B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
xB
x8B
xRB
xRB
x�B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
yrB
y�B
y�B
y�B
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{�B
|B
|B
|6B
|PB
|�B
|�B
|�B
|�B
}"B
}qB
}�B
~BB
~�B
~�B
~�B
~�B
~�B
.B
.B
HB
�B
� B
� B
��B
��B
��B
� B
�;B
�UB
�oB
�oB
��B
�[B
�uB
�uB
��B
��B
�aB
�GB
�aB
��B
��B
��B
��B
�B
�MB
��B
��B
�B
�mB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104925  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174048  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174048  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174048                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024055  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024055  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131508                      G�O�G�O�G�O�                
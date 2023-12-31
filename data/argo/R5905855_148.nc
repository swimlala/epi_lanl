CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-04T21:42:10Z creation;2023-03-04T21:42:11Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230304214210  20230304215640  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�k�fO1   @�lP6�@0�������d�`A�71   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B���B�  B�33B˙�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  ChL�Ci��Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @7�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��>B��qB��qB��B��B��qB��B�#�Bˊ>B��B��B��B��B��B��B��B��B��B��B��B��>B��C�RC�RC�RC�RC	޸C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC0�C1޸C3޸C5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RChECi�Ck�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD�zDD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dqw�Dq��Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
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
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�b=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AɸRA��)A��aA��A��EA���A���A��5A��`A��A��A��A���A���A��5A��oA���A��A��|A��A���A��A���A���A���A���A��fA���A��>A��JA���A���A��A���A��AA��|A���A��A��BAɴ�A�;�AǮ�A��AİUA��cA�poA�_�A�#�A�HKA�K�A��	A��fA�[�A���A�	�A���A��*A��eA���A�=qA�jA��wA��dA���A���A�3�A��A�x8A�|�A���A�n/A��zA��SA���A� iA��A���A���A�چA��A� �A��MA�g8A��A�	�A���A�W
A�YA��A���AC�A{�jAy�.Ay�MAwیAw�AvqvAs)�Ap5�An*�AiT�Ag�AdϫAa�wA]�4A[j�AY��AW+AT�^AS�AQ�AO�AL͟AJ�*AIrGAG�qAD��A?��A=>BA<�A;N�A9�A8��A76A5ߤA4��A3��A1�dA0@OA.S�A+��A+?�A*�A(�rA&�A$=A"C-A!�A �A2aAK�A�7A�A�A�XAoiAjAC�AQ�A*�A��A�]AƨAjAffAYKA%FA�Ay�A,�A�A4A�	A$�A��A�^A��A��Aw�A��A#:AںA�A�;A�SA�A
��A
�FA
o A
[�A	��A	Z�A�"A��AF�A��A��Am]A\�AVAJ�A�A��A:�A�|A� A�A=�A�oA�ZAaA@A�A��A�A�zA��A|AO�A�A ��A 5�A (@���@�4@��@��@��@�]d@��@��:@��"@���@�W�@���@���@�v�@��)@�rG@��@�:*@��+@�C�@��f@���@��@���@��@�#�@�@�A�@���@��^@��@��@���@�dZ@��@��@�{@�*0@�m�@��3@�c�@��@��@�M�@�*�@�<�@�,=@�_@���@�l�@��@�>B@�$@�@”@�+k@��}@�l"@߃{@�@O@��@ުe@�Ft@��.@ݬq@��@�GE@ۉ7@��E@�z@���@پw@�t�@�#�@؎�@���@׹�@�Z�@��@��@�@�S@ԑ�@�($@�X�@�d�@�:�@�[�@ѨX@�;@�!@��a@ϥ@�z@��E@�qv@�}V@ɠ'@�"�@�w�@��@Ǜ=@�+�@��@�p;@��@őh@�J�@�+@�֡@Ĉ�@�O@��@���@�{@���@���@�Y�@�҉@�v�@��Z@�~�@��8@��D@�V@��@�a�@���@���@�Xy@�~@��@���@��@�m�@�(�@��A@�ƨ@���@���@��$@�A�@�&@��@��9@���@�`B@�F@��@���@���@�
�@�\�@��@���@��D@���@��@��_@���@���@�Ɇ@�Z�@�!@��@���@�q@�ں@�+k@��Q@���@�(�@���@�m�@��t@�<6@�&@� i@���@��B@�҉@���@��e@�"h@�$t@��@��|@��H@�ߤ@���@���@��T@�S@���@�~(@�-�@��@�f�@� \@��@���@���@���@�!�@���@���@��@���@��@��@��@���@�ƨ@�x�@�8�@��f@���@��@��@�@@���@��s@��F@�n�@�I�@���@���@���@�o�@�~�@�/�@���@�2�@���@��@���@�p�@��@���@�`�@�e@��>@���@���@���@���@�{�@�h�@��@�Vm@�*0@��@��}@��.@��A@�C-@�!�@���@��q@�c@�Mj@�,�@�%@��[@��O@���@�Ta@�_@��@��@��g@���@�T�@���@�u%@�>B@�!@�@���@�g�@�+�@��@��`@��U@�l"@�=q@�J@��3@��F@��h@�J�@�/�@��@��p@��z@�j@��^@��=@���@�T�@��@��@�H�@�  @���@�_p@�J#@�&@��@�|�@�J�@�/�@� �@�@���@�� @�Dg@��@���@�0U@�  @��@��+@���@���@�~(@�c�@�B[@�%�@��>@�@���@���@��@�n/@�a�@�J#@�33@��@���@�D�@��)@��-@���@�c@�N<@�,�@��@��@��@���@�� @�*�@�)�@��@�� @��@�f�@�A�@�ߤ@��@���@�Q@�  @9�@~�@~�R@~B[@}�j@}��@}:�@|��@|�@|�$@|w�@|	�@{\)@zں@z
�@y7L@y%@x�P@x�|@x֡@xw�@x�@w�q@w�{@wdZ@w4�@v�]@vz@u�@u8�@t�?@te�@t�@s��@s�0@s�k@s4�@r��@r_�@r	@qx�@p~(@p9X@p�@p	�@o�]@o�@o�;@o�{@o1�@n��@n�1@nTa@n)�@m��@m�z@m��@m8�@ll"@k��@j�2@i�"@h�9@h�@htT@h2�@g��@gZ�@g�@f��@f��@e�X@e^�@d�5@d4n@c�@bs�@bJ@a��@a��@a*0@a�@`�@`S�@_�@_4�@_C@^�b@]�d@]m]@\�v@\/�@[��@[O@[�@@[�f@[s@[@O@[.I@[o@[�@Z�8@Z�}@Z.�@Z4@Z	@Y��@Y�9@Y��@Y�@Y�@YN<@X�I@Xb@WX�@V҉@V:*@U�#@U�M@U+�@U�@T�P@T��@T�?@T��@T��@T��@Tr�@TQ�@Tb@S�:@SRT@R�!@Q�@Q��@Qc@Qu�@Qo @P��@O�r@O�{@O��@OdZ@OA�@O33@N�M@N��@NB[@M��@M	l@L��@L��@LI�@K�{@Jff@I/@H֡@H�_@H��@H|�@H[�@H@G��@G&@F�}@E��@E�H@E�=@E��@ErG@E�@D�@D�@C˒@CO@C/�@B��@B~�@A�#@A7L@@�Y@?��@?j�@?iD@?.I@>�r@>xl@>GE@=�@=�H@=�n@=u�@<�)@<<�@;��@:�]@:��@:~�@9��@9��@9�C@9rG@9=�@9 \@9�@9	l@8֡@8�.@8I�@8(�@8�@7�P@6ȴ@6�\@6z@6+k@5�)@5�~@5	l@4�@4�j@4y>@4e�@4G@3�q@3RT@2�<@2�@2u%@2	@2u@1�@1N<@0�@0 �@0  @/�W@/ƨ@/��@/�k@/g�@//�@/�@.�8@.�H@.�@.��@.�@.��@.�A@.+k@-�d@-��@-�7@- \@,�`@,�@,r�@,m�@+�r@+˒@+~�@+�@*҉@*d�@*^5@*��@*YK@*H�@*)�@)��@)��@)�~@)|@)J�@(��@(�D@(`�@(N�@(9X@(	�@'�@'��@'�{@'~�@'\)@'F�@'8@&�"@&��@&�b@&^5@&�@%ԕ@%�X@%��@%/@$��@$��@$��@$oi@$?�@#�@#��@#�@@#y�@#4�@#�@"��@"�6@"��@"$�@!��@!�X@!q@ �@ c�@�@�@��@��@t�@_p@�@�R@xl@E�@GE@:*@($@{@�@w2@A @�@�|@��@�D@*�@  @ݘ@خ@�@Z�@(@ȴ@Q@�3@hs@�@�E@w�@>B@@�@�a@o�@,�@�B@��@c @5?@�@��@�@�n@`B@!�@��@�@ѷ@�@z�@I�@'R@  @�;@�K@��@�f@"�@��@҉@�'@��@��@�@�!@�+@\�@?@=q@;�@5?@�@`B@7L@@�@Ĝ@��@�@Xy@4n@�@�r@�r@�q@X�@�@�@��@v�@��@��@hs@S&@?}@7L@ \@��@�@�/@��@��@<�@b@�+@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AɸRA��)A��aA��A��EA���A���A��5A��`A��A��A��A���A���A��5A��oA���A��A��|A��A���A��A���A���A���A���A��fA���A��>A��JA���A���A��A���A��AA��|A���A��A��BAɴ�A�;�AǮ�A��AİUA��cA�poA�_�A�#�A�HKA�K�A��	A��fA�[�A���A�	�A���A��*A��eA���A�=qA�jA��wA��dA���A���A�3�A��A�x8A�|�A���A�n/A��zA��SA���A� iA��A���A���A�چA��A� �A��MA�g8A��A�	�A���A�W
A�YA��A���AC�A{�jAy�.Ay�MAwیAw�AvqvAs)�Ap5�An*�AiT�Ag�AdϫAa�wA]�4A[j�AY��AW+AT�^AS�AQ�AO�AL͟AJ�*AIrGAG�qAD��A?��A=>BA<�A;N�A9�A8��A76A5ߤA4��A3��A1�dA0@OA.S�A+��A+?�A*�A(�rA&�A$=A"C-A!�A �A2aAK�A�7A�A�A�XAoiAjAC�AQ�A*�A��A�]AƨAjAffAYKA%FA�Ay�A,�A�A4A�	A$�A��A�^A��A��Aw�A��A#:AںA�A�;A�SA�A
��A
�FA
o A
[�A	��A	Z�A�"A��AF�A��A��Am]A\�AVAJ�A�A��A:�A�|A� A�A=�A�oA�ZAaA@A�A��A�A�zA��A|AO�A�A ��A 5�A (@���@�4@��@��@��@�]d@��@��:@��"@���@�W�@���@���@�v�@��)@�rG@��@�:*@��+@�C�@��f@���@��@���@��@�#�@�@�A�@���@��^@��@��@���@�dZ@��@��@�{@�*0@�m�@��3@�c�@��@��@�M�@�*�@�<�@�,=@�_@���@�l�@��@�>B@�$@�@”@�+k@��}@�l"@߃{@�@O@��@ުe@�Ft@��.@ݬq@��@�GE@ۉ7@��E@�z@���@پw@�t�@�#�@؎�@���@׹�@�Z�@��@��@�@�S@ԑ�@�($@�X�@�d�@�:�@�[�@ѨX@�;@�!@��a@ϥ@�z@��E@�qv@�}V@ɠ'@�"�@�w�@��@Ǜ=@�+�@��@�p;@��@őh@�J�@�+@�֡@Ĉ�@�O@��@���@�{@���@���@�Y�@�҉@�v�@��Z@�~�@��8@��D@�V@��@�a�@���@���@�Xy@�~@��@���@��@�m�@�(�@��A@�ƨ@���@���@��$@�A�@�&@��@��9@���@�`B@�F@��@���@���@�
�@�\�@��@���@��D@���@��@��_@���@���@�Ɇ@�Z�@�!@��@���@�q@�ں@�+k@��Q@���@�(�@���@�m�@��t@�<6@�&@� i@���@��B@�҉@���@��e@�"h@�$t@��@��|@��H@�ߤ@���@���@��T@�S@���@�~(@�-�@��@�f�@� \@��@���@���@���@�!�@���@���@��@���@��@��@��@���@�ƨ@�x�@�8�@��f@���@��@��@�@@���@��s@��F@�n�@�I�@���@���@���@�o�@�~�@�/�@���@�2�@���@��@���@�p�@��@���@�`�@�e@��>@���@���@���@���@�{�@�h�@��@�Vm@�*0@��@��}@��.@��A@�C-@�!�@���@��q@�c@�Mj@�,�@�%@��[@��O@���@�Ta@�_@��@��@��g@���@�T�@���@�u%@�>B@�!@�@���@�g�@�+�@��@��`@��U@�l"@�=q@�J@��3@��F@��h@�J�@�/�@��@��p@��z@�j@��^@��=@���@�T�@��@��@�H�@�  @���@�_p@�J#@�&@��@�|�@�J�@�/�@� �@�@���@�� @�Dg@��@���@�0U@�  @��@��+@���@���@�~(@�c�@�B[@�%�@��>@�@���@���@��@�n/@�a�@�J#@�33@��@���@�D�@��)@��-@���@�c@�N<@�,�@��@��@��@���@�� @�*�@�)�@��@�� @��@�f�@�A�@�ߤ@��@���@�Q@�  @9�@~�@~�R@~B[@}�j@}��@}:�@|��@|�@|�$@|w�@|	�@{\)@zں@z
�@y7L@y%@x�P@x�|@x֡@xw�@x�@w�q@w�{@wdZ@w4�@v�]@vz@u�@u8�@t�?@te�@t�@s��@s�0@s�k@s4�@r��@r_�@r	@qx�@p~(@p9X@p�@p	�@o�]@o�@o�;@o�{@o1�@n��@n�1@nTa@n)�@m��@m�z@m��@m8�@ll"@k��@j�2@i�"@h�9@h�@htT@h2�@g��@gZ�@g�@f��@f��@e�X@e^�@d�5@d4n@c�@bs�@bJ@a��@a��@a*0@a�@`�@`S�@_�@_4�@_C@^�b@]�d@]m]@\�v@\/�@[��@[O@[�@@[�f@[s@[@O@[.I@[o@[�@Z�8@Z�}@Z.�@Z4@Z	@Y��@Y�9@Y��@Y�@Y�@YN<@X�I@Xb@WX�@V҉@V:*@U�#@U�M@U+�@U�@T�P@T��@T�?@T��@T��@T��@Tr�@TQ�@Tb@S�:@SRT@R�!@Q�@Q��@Qc@Qu�@Qo @P��@O�r@O�{@O��@OdZ@OA�@O33@N�M@N��@NB[@M��@M	l@L��@L��@LI�@K�{@Jff@I/@H֡@H�_@H��@H|�@H[�@H@G��@G&@F�}@E��@E�H@E�=@E��@ErG@E�@D�@D�@C˒@CO@C/�@B��@B~�@A�#@A7L@@�Y@?��@?j�@?iD@?.I@>�r@>xl@>GE@=�@=�H@=�n@=u�@<�)@<<�@;��@:�]@:��@:~�@9��@9��@9�C@9rG@9=�@9 \@9�@9	l@8֡@8�.@8I�@8(�@8�@7�P@6ȴ@6�\@6z@6+k@5�)@5�~@5	l@4�@4�j@4y>@4e�@4G@3�q@3RT@2�<@2�@2u%@2	@2u@1�@1N<@0�@0 �@0  @/�W@/ƨ@/��@/�k@/g�@//�@/�@.�8@.�H@.�@.��@.�@.��@.�A@.+k@-�d@-��@-�7@- \@,�`@,�@,r�@,m�@+�r@+˒@+~�@+�@*҉@*d�@*^5@*��@*YK@*H�@*)�@)��@)��@)�~@)|@)J�@(��@(�D@(`�@(N�@(9X@(	�@'�@'��@'�{@'~�@'\)@'F�@'8@&�"@&��@&�b@&^5@&�@%ԕ@%�X@%��@%/@$��@$��@$��@$oi@$?�@#�@#��@#�@@#y�@#4�@#�@"��@"�6@"��@"$�@!��@!�X@!q@ �@ c�@�@�@��@��@t�@_p@�@�R@xl@E�@GE@:*@($@{@�@w2@A @�@�|@��@�D@*�@  @ݘ@خ@�@Z�@(@ȴ@Q@�3@hs@�@�E@w�@>B@@�@�a@o�@,�@�B@��@c @5?@�@��@�@�n@`B@!�@��@�@ѷ@�@z�@I�@'R@  @�;@�K@��@�f@"�@��@҉@�'@��@��@�@�!@�+@\�@?@=q@;�@5?@�@`B@7L@@�@Ĝ@��@�@Xy@4n@�@�r@�r@�q@X�@�@�@��@v�@��@��@hs@S&@?}@7L@ \@��@�@�/@��@��@<�@b@�+@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�TB	�TB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�ZB	�tB	�?B	�tB	��B	�XB	��B
~B
=VB
h�B
}�B
�mB
��B
�mB
��B
�qBM�BCB}�B��B��B�0B��B�AB�bB� B{BfLBf�Bq�B��B�|B�+B�B՛B��B�?Bp�BaB1�B
��B
�B
�PB
�mB
�MB
�MB
�$B
��B
�B
~�B
`vB
O�B
CGB
1B
!�B
B	��B	��B	�WB	��B	یB	�B	ʦB	�9B	�nB	�ZB	��B	�OB	�ZB	�\B	t�B	\�B	QB	IRB	=�B	3�B	-]B	%�B	#�B	#nB	$�B	 �B	�B	�B	1B	[B	�B	dB	B	~B	^B		�B	1B	�B�}B�8B��B�QB��B��B��B�B�'B�bB�B�B��B�zB�cB�2B��B��B�B�B	 4B�wB	B	sB	�B	�B	KB	�B	�B	�B	)B	~B	�B	,WB	T�B	Z�B	Z�B	LdB	PbB	T�B	W$B	Y�B	`'B	e�B	h
B	h�B	h�B	i�B	j�B	lWB	o�B	q�B	x�B	y�B	��B	�B	�_B	��B	��B	�VB	�<B	��B	��B	�BB	��B	�SB	��B	�_B	�KB	��B	�WB	��B	�~B	��B	��B	�OB	�B	�:B	�nB	��B	��B	�zB	�B	��B	�KB	�B	�KB	�QB	�"B	�"B	��B	��B	�]B	��B	��B	��B	��B	��B	��B	�;B	��B	��B	��B	�B	�tB	��B	�ZB	�tB	��B	��B	��B	�8B	��B	�B	�VB	��B	�AB	��B	�B	āB	ĜB	��B	�1B	ȴB	��B	�)B	��B	ɆB	�7B	��B	�)B	��B	�0B	̘B	��B	�<B	ΥB	��B	ԯB	�B	�{B	�,B	�uB	�B	�MB	֡B	ּB	ּB	�sB	׍B	�?B	ՁB	�:B	�4B	�hB	�uB	ѷB	бB	��B	ҽB	ѝB	ѷB	�[B	�B	�gB	��B	��B	��B	֡B	�+B	�QB	��B	ܬB	�B	ބB	�B	ޞB	��B	�5B	�=B	��B	��B	уB	��B	�oB	��B	��B	�B	��B	�KB	��B	ބB	ߊB	ߤB	ߤB	ߤB	��B	ߊB	��B	��B	�B	��B	�-B	�B	�NB	�B	�B	�@B	�B	��B	�`B	��B	�LB	��B	�_B	��B	�B	��B	�;B	�B	�B	�B	�OB	��B	�ZB	��B	�$B	��B	�DB	��B	��B	��B	��B	�<B	��B	��B	��B	�cB
 B	�wB	��B	��B
 �B
�B
EB
	�B

�B
B
"B
�B
6B
B
�B
�B
�B
6B
"B
�B
�B
�B
DB
xB
B
�B
"B
�B
�B
B
�B
<B
0B
~B
�B
B
B
"B
�B
VB
�B
}B
bB
bB
�B
4B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
2B
2B
�B
�B
B
SB
SB
SB
�B
�B
$B

B
�B
�B
SB
SB
�B
TB
TB
B
�B
9B
B
B
9B
sB
�B
EB
B
7B
�B
�B
�B
kB
�B
�B
CB
xB
�B
�B
�B
�B
�B
 BB
 \B
 �B
 �B
 �B
 �B
 �B
!bB
!|B
!|B
!HB
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
%,B
&B
&�B
&�B
&�B
'�B
'mB
'�B
'mB
'mB
'�B
(�B
)DB
)�B
)DB
)DB
)_B
)_B
)_B
)*B
(�B
)B
)*B
)�B
)�B
)�B
)�B
*KB
+QB
+�B
,"B
,WB
,�B
,�B
-B
,�B
.�B
.}B
.}B
.}B
.IB
.B
.B
./B
/�B
/�B
/5B
.B
,�B
(�B
#�B
#�B
#�B
#�B
#B
"�B
"�B
"hB
#B
#nB
#�B
$@B
$�B
$�B
%,B
%,B
%zB
$�B
$�B
$�B
$�B
$�B
%,B
&B
&fB
'8B
'�B
($B
(sB
(>B
(sB
(�B
)�B
)�B
*eB
*KB
*B
)�B
)�B
)�B
)�B
*0B
*B
+6B
,"B
,�B
,"B
+�B
,"B
,WB
,�B
-�B
-�B
.�B
/B
/�B
0B
0�B
0�B
0�B
0oB
0�B
0�B
0�B
0�B
0�B
0�B
1AB
1vB
1�B
1�B
2B
1�B
1�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
5%B
5tB
5�B
6B
6B
6B
5�B
6�B
6�B
7B
7�B
8lB
8�B
9$B
9XB
9>B
9�B
9XB
8�B
9rB
9rB
9�B
:*B
:�B
:�B
;0B
;B
;0B
<6B
<�B
<6B
<6B
<�B
=B
=B
<�B
=�B
=�B
=�B
=�B
>B
>wB
>�B
?cB
@�B
A B
@�B
A�B
A;B
@�B
@�B
@�B
A�B
BuB
B�B
C�B
D3B
D�B
D�B
D�B
EB
E�B
E�B
F%B
FtB
GB
G+B
G�B
HB
HB
G�B
G�B
F�B
F%B
E�B
G�B
HB
HKB
H1B
H1B
HfB
HKB
HfB
HfB
HfB
H�B
H�B
IlB
J=B
J�B
K�B
MjB
N�B
N�B
N�B
N�B
OBB
PB
P�B
P�B
P�B
P�B
P�B
P�B
P}B
PbB
PbB
O�B
O�B
O�B
OB
N�B
NpB
OvB
O�B
P.B
PbB
PbB
P}B
P�B
Q�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S[B
T,B
T�B
T�B
T�B
T�B
U�B
VB
V�B
V�B
W�B
XyB
X�B
Y�B
[	B
[	B
[#B
[	B
[=B
[=B
[�B
[�B
[�B
\xB
[�B
\CB
\)B
\B
\CB
\�B
\�B
\�B
]dB
]IB
]~B
]�B
]�B
]�B
]~B
]�B
^�B
_VB
_!B
_!B
_!B
_B
_!B
_;B
_!B
_VB
_pB
_�B
`BB
`vB
aHB
a�B
a�B
a|B
abB
a�B
bhB
bB
b�B
b�B
cB
cB
cB
cB
c B
c�B
c�B
c�B
c�B
dB
d&B
dZB
d�B
eB
ezB
e�B
eFB
ezB
e�B
ezB
ezB
e�B
e�B
fB
fB
e�B
fB
f�B
f�B
gB
g�B
h�B
iB
i*B
i�B
i�B
jB
jKB
j0B
jB
j�B
kQB
kkB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
lB
lB
lB
lWB
l�B
l�B
m]B
m�B
m�B
m�B
m�B
nB
ncB
ncB
nIB
ncB
ncB
n�B
n�B
n�B
o5B
o�B
o�B
o�B
p;B
p;B
p�B
qB
p�B
q[B
qvB
q�B
r-B
rGB
r-B
rB
q�B
q�B
rB
raB
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
s�B
tTB
t�B
t�B
uB
uB
t�B
uZB
u�B
u�B
v+B
v�B
wB
wLB
w�B
xB
x8B
xlB
x�B
x�B
x�B
x�B
y$B
y>B
y�B
y�B
y�B
zB
zB
zB
z^B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{0B
{JB
{B
{dB
{dB
{B
{�B
|B
|B
|6B
|PB
|6B
|6B
|PB
|�B
|�B
|�B
}B
}B
|�B
}"B
}�B
}�B
~B
~B
~BB
~B
~B
~wB
~�B
~�B
~�B
~�B
.B
}B
�B
�B
� B
�OB
��B
�B
�;B
�;B
�oB
�oB
��B
��B
��B
��B
��B
��B
�'B
�'B
�[B
�'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�TB	�TB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�ZB	�tB	�?B	�tB	��B	�XB	��B
~B
=VB
h�B
}�B
�mB
��B
�mB
��B
�qBM�BCB}�B��B��B�0B��B�AB�bB� B{BfLBf�Bq�B��B�|B�+B�B՛B��B�?Bp�BaB1�B
��B
�B
�PB
�mB
�MB
�MB
�$B
��B
�B
~�B
`vB
O�B
CGB
1B
!�B
B	��B	��B	�WB	��B	یB	�B	ʦB	�9B	�nB	�ZB	��B	�OB	�ZB	�\B	t�B	\�B	QB	IRB	=�B	3�B	-]B	%�B	#�B	#nB	$�B	 �B	�B	�B	1B	[B	�B	dB	B	~B	^B		�B	1B	�B�}B�8B��B�QB��B��B��B�B�'B�bB�B�B��B�zB�cB�2B��B��B�B�B	 4B�wB	B	sB	�B	�B	KB	�B	�B	�B	)B	~B	�B	,WB	T�B	Z�B	Z�B	LdB	PbB	T�B	W$B	Y�B	`'B	e�B	h
B	h�B	h�B	i�B	j�B	lWB	o�B	q�B	x�B	y�B	��B	�B	�_B	��B	��B	�VB	�<B	��B	��B	�BB	��B	�SB	��B	�_B	�KB	��B	�WB	��B	�~B	��B	��B	�OB	�B	�:B	�nB	��B	��B	�zB	�B	��B	�KB	�B	�KB	�QB	�"B	�"B	��B	��B	�]B	��B	��B	��B	��B	��B	��B	�;B	��B	��B	��B	�B	�tB	��B	�ZB	�tB	��B	��B	��B	�8B	��B	�B	�VB	��B	�AB	��B	�B	āB	ĜB	��B	�1B	ȴB	��B	�)B	��B	ɆB	�7B	��B	�)B	��B	�0B	̘B	��B	�<B	ΥB	��B	ԯB	�B	�{B	�,B	�uB	�B	�MB	֡B	ּB	ּB	�sB	׍B	�?B	ՁB	�:B	�4B	�hB	�uB	ѷB	бB	��B	ҽB	ѝB	ѷB	�[B	�B	�gB	��B	��B	��B	֡B	�+B	�QB	��B	ܬB	�B	ބB	�B	ޞB	��B	�5B	�=B	��B	��B	уB	��B	�oB	��B	��B	�B	��B	�KB	��B	ބB	ߊB	ߤB	ߤB	ߤB	��B	ߊB	��B	��B	�B	��B	�-B	�B	�NB	�B	�B	�@B	�B	��B	�`B	��B	�LB	��B	�_B	��B	�B	��B	�;B	�B	�B	�B	�OB	��B	�ZB	��B	�$B	��B	�DB	��B	��B	��B	��B	�<B	��B	��B	��B	�cB
 B	�wB	��B	��B
 �B
�B
EB
	�B

�B
B
"B
�B
6B
B
�B
�B
�B
6B
"B
�B
�B
�B
DB
xB
B
�B
"B
�B
�B
B
�B
<B
0B
~B
�B
B
B
"B
�B
VB
�B
}B
bB
bB
�B
4B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
2B
2B
�B
�B
B
SB
SB
SB
�B
�B
$B

B
�B
�B
SB
SB
�B
TB
TB
B
�B
9B
B
B
9B
sB
�B
EB
B
7B
�B
�B
�B
kB
�B
�B
CB
xB
�B
�B
�B
�B
�B
 BB
 \B
 �B
 �B
 �B
 �B
 �B
!bB
!|B
!|B
!HB
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
%,B
&B
&�B
&�B
&�B
'�B
'mB
'�B
'mB
'mB
'�B
(�B
)DB
)�B
)DB
)DB
)_B
)_B
)_B
)*B
(�B
)B
)*B
)�B
)�B
)�B
)�B
*KB
+QB
+�B
,"B
,WB
,�B
,�B
-B
,�B
.�B
.}B
.}B
.}B
.IB
.B
.B
./B
/�B
/�B
/5B
.B
,�B
(�B
#�B
#�B
#�B
#�B
#B
"�B
"�B
"hB
#B
#nB
#�B
$@B
$�B
$�B
%,B
%,B
%zB
$�B
$�B
$�B
$�B
$�B
%,B
&B
&fB
'8B
'�B
($B
(sB
(>B
(sB
(�B
)�B
)�B
*eB
*KB
*B
)�B
)�B
)�B
)�B
*0B
*B
+6B
,"B
,�B
,"B
+�B
,"B
,WB
,�B
-�B
-�B
.�B
/B
/�B
0B
0�B
0�B
0�B
0oB
0�B
0�B
0�B
0�B
0�B
0�B
1AB
1vB
1�B
1�B
2B
1�B
1�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
5%B
5tB
5�B
6B
6B
6B
5�B
6�B
6�B
7B
7�B
8lB
8�B
9$B
9XB
9>B
9�B
9XB
8�B
9rB
9rB
9�B
:*B
:�B
:�B
;0B
;B
;0B
<6B
<�B
<6B
<6B
<�B
=B
=B
<�B
=�B
=�B
=�B
=�B
>B
>wB
>�B
?cB
@�B
A B
@�B
A�B
A;B
@�B
@�B
@�B
A�B
BuB
B�B
C�B
D3B
D�B
D�B
D�B
EB
E�B
E�B
F%B
FtB
GB
G+B
G�B
HB
HB
G�B
G�B
F�B
F%B
E�B
G�B
HB
HKB
H1B
H1B
HfB
HKB
HfB
HfB
HfB
H�B
H�B
IlB
J=B
J�B
K�B
MjB
N�B
N�B
N�B
N�B
OBB
PB
P�B
P�B
P�B
P�B
P�B
P�B
P}B
PbB
PbB
O�B
O�B
O�B
OB
N�B
NpB
OvB
O�B
P.B
PbB
PbB
P}B
P�B
Q�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S[B
T,B
T�B
T�B
T�B
T�B
U�B
VB
V�B
V�B
W�B
XyB
X�B
Y�B
[	B
[	B
[#B
[	B
[=B
[=B
[�B
[�B
[�B
\xB
[�B
\CB
\)B
\B
\CB
\�B
\�B
\�B
]dB
]IB
]~B
]�B
]�B
]�B
]~B
]�B
^�B
_VB
_!B
_!B
_!B
_B
_!B
_;B
_!B
_VB
_pB
_�B
`BB
`vB
aHB
a�B
a�B
a|B
abB
a�B
bhB
bB
b�B
b�B
cB
cB
cB
cB
c B
c�B
c�B
c�B
c�B
dB
d&B
dZB
d�B
eB
ezB
e�B
eFB
ezB
e�B
ezB
ezB
e�B
e�B
fB
fB
e�B
fB
f�B
f�B
gB
g�B
h�B
iB
i*B
i�B
i�B
jB
jKB
j0B
jB
j�B
kQB
kkB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
lB
lB
lB
lWB
l�B
l�B
m]B
m�B
m�B
m�B
m�B
nB
ncB
ncB
nIB
ncB
ncB
n�B
n�B
n�B
o5B
o�B
o�B
o�B
p;B
p;B
p�B
qB
p�B
q[B
qvB
q�B
r-B
rGB
r-B
rB
q�B
q�B
rB
raB
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
s�B
tTB
t�B
t�B
uB
uB
t�B
uZB
u�B
u�B
v+B
v�B
wB
wLB
w�B
xB
x8B
xlB
x�B
x�B
x�B
x�B
y$B
y>B
y�B
y�B
y�B
zB
zB
zB
z^B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{0B
{JB
{B
{dB
{dB
{B
{�B
|B
|B
|6B
|PB
|6B
|6B
|PB
|�B
|�B
|�B
}B
}B
|�B
}"B
}�B
}�B
~B
~B
~BB
~B
~B
~wB
~�B
~�B
~�B
~�B
.B
}B
�B
�B
� B
�OB
��B
�B
�;B
�;B
�oB
�oB
��B
��B
��B
��B
��B
��B
�'B
�'B
�[B
�'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230304214203  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230304214210  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230304214211  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230304214211                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230304214212  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230304214212  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230304215640                      G�O�G�O�G�O�                
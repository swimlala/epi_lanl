CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-29T03:42:16Z creation;2022-07-29T03:42:17Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220729034216  20220729040437  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��!Y��b1   @��"e�ó@/f�x����c�;dZ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@���@���A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh��Br  Bv��B33B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C�fC�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>L�C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
@��
A�A?�A_�A�A�A�A�A���A���A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB`G�Bh�Bq�HBv�B{B��B�W
B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�C�C	�RC�RC�RC�RC�RC�RC�RC޸C޸C޸C�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC>EC?޸CA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCn�Co�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~DzD�zD�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~DzzDz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�r|A�tTA�zDAԃ{AԆ�Aԅ�Aԅ�Aԇ�AԊ	Aԋ�AԋAԊ�Aԋ�AԌ~AԍAԏ(Aԏ�Aԑ4Aԏ�AԑhAԒ:Aԑ4Aԓ@AԔ{AԕAԕAԓuAԏ\A�xA�IA�|�A͙eA��A�5�AǇ_A�HKAĚ�AÏ�A�kA�wfA���A���A���A�1�A�A��lA�u%A�oA��TA���A���A��QA���A�cTA��A�e�A�0�A�چA�M�A�-A�8�A���A�ΥA�4A�&LA�HA�0�A�<�A���A��sA��A�{A���A��KA�
�A�`�A�kA�y�A��A��A��4A��0A���A���A��A��A���A��/A�{A��A��vA��rA���A�0�A��_A�A|qAz1Av�QAro�ApɆAo��An�Am�Ag�Ab�YA]��AZ"�AVMASYKAOh
ALOAKd�AG?�AE(�ADy�AD;AC�LAB��A@��A?,=A=�A:�A8�xA6��A5�A5a�A4h
A3GA0�$A0*0A0	lA/��A/J�A.�A.qA-|�A,HA+�A+�oA+ \A*��A)Z�A(SA'�A'9XA&�yA&�]A&6�A%��A%�OA%`BA$�PA#ȴA"	�A ��A S�A -wA �AaA��A��A�A_A~�A�A�XAh
A��A�A�A<�A˒A�9A-AĜA�+A��A~�An/AOA�MA;dA�/A�MAy>A"�AiDA�A{JAE9ASArGAƨAOA�A��AxlAA�AbA��A �A�<A�hAe�A�"A�NA~�A�A��AU�A�AĜAy�AC�A
�KA
�YA
!A	��A	.�A�A��AO�A�AW�A�Ap;A[�A-�A�LA�pA<6A��A��A��AZ�A�A��A-�A�AU2AOA A w2A 8A �@�m]@�]d@�خ@�y�@�Z�@���@��@�W?@��@�#:@�L�@��@�/�@��h@�H�@���@��6@��<@�6�@�?}@��@�h@��@�C-@�!�@�$@�&@�h�@�)_@�>B@���@�*0@�_�@��@�;@�V@�q@��@�}@�J�@�a@��5@�C�@�n@� i@�`�@㝲@��P@⻙@ᧇ@�^�@�@�M�@��@�c�@޳h@ݲ�@ܿ�@�xl@ۺ^@�8@���@�{�@٨X@�$t@��@���@؋D@� �@�r�@�	@ջ0@�f�@�ѷ@ӎ"@�/�@Ҍ�@��@��@П�@�_@�O@�X@Μx@���@�j@��@̣@̒�@�|@ʧ@�|�@�1�@���@�ϫ@ɡ�@�,�@��)@ȍ�@�M�@�PH@�;�@�x@ơb@Ɯx@�+k@��@��
@ŧ�@��@��f@��@�1'@��@Ü@�W?@��8@¾@�!@�"�@��,@���@�c�@�7�@�hs@��f@���@��U@��@�2�@��N@�a@�L0@�G@��}@��@@��B@�M@�K�@���@���@��.@��Y@�
�@���@�/�@��@��}@�B[@�?�@��S@�!-@�W�@��P@�<6@��f@���@�v�@�D�@�J@�خ@�j@��@��o@�5?@�_@��@��@@�iD@�&�@��@���@��@���@���@�x@���@��p@��z@�U2@� �@���@�Y�@�(@��@�Q@� �@���@��9@���@��@���@�j@�-w@��@�Ɇ@�]d@�-�@���@�zx@�f�@�*0@��@���@��'@��u@�g8@�6@��@��@��:@�]�@�(�@��[@�1@��S@�RT@�ی@��@�:*@�O@���@���@�|@��2@�_@�4@���@��:@��@��?@�E�@��@�� @���@�zx@�@��4@�N�@�7@���@�~�@�U�@�@��@�/�@��m@���@���@�j@�0�@���@��@���@�C�@�*�@��@��@�ԕ@���@��@���@�kQ@�	@��{@�33@��@��"@��@��[@��1@�R�@�$�@���@��@���@��@�^�@�S@��@���@�.�@���@���@��q@�m]@�:�@�&�@�!�@�C@�ی@��j@��9@��@���@�Ta@�2�@� �@�G@���@��	@�33@�	l@��|@�xl@�O@��;@���@�@O@��H@��)@���@���@�~�@�oi@�Q�@��C@�Q�@�"�@��@��.@�z�@�_�@�Ft@��@��a@�)_@��,@�n�@�N�@�:�@�($@� �@��.@�� @�[W@�,�@��@��@��@�Vm@�"�@��@���@��f@���@��M@���@�Ov@��m@���@�F�@�q@��@���@���@��@�PH@��@��@g�@&@~��@}�@}�@|�@{�+@{qv@z�!@z:*@y��@y5�@x�v@x�D@x�@w��@w@O@w�@v�c@v�]@v��@uԕ@u��@u+@tD�@s�r@s�}@s��@sU�@r�,@r��@rE�@q��@q�H@q��@qJ�@ph�@o�*@n�@n��@nGE@m��@m#�@l�p@lM@k��@k�{@k_p@j�M@j)�@i��@is�@i+@h�)@h  @f~�@fM�@f0U@e�-@d��@dtT@dc�@d�@cخ@b�L@b^5@b1�@b)�@b0U@b0U@a��@aG�@`��@`g8@_�k@_y�@_S�@^�8@^͟@^��@^E�@^�@]��@]��@]@@\��@\��@\g8@[��@[U�@[9�@Z�c@Z��@Z��@Zv�@Za|@ZV@ZGE@Z6�@Y�@Y0�@X�/@X�Y@Xw�@Xr�@Xy>@Xz�@XtT@XN�@X�@W��@W_p@W�@V�!@VkQ@V�@U�j@Uj@T�j@T��@T]d@T$@S�@S�@RkQ@Q��@Q+�@P�E@P6@O��@O�V@OW?@O1�@O�@N�2@N��@M�D@M�=@M=�@L��@L�I@Lw�@LbN@LN�@L"h@J�M@I�H@I�S@I�"@Izx@Iu�@Ij@IIR@I+�@H�E@H�u@HM@G@O@F��@F��@F\�@E�@Ee,@E�@D�u@D�@C�*@CiD@C]�@CC@B�B@B�@A�n@A/@A�@A@@�U@@Z@@�@?��@?��@?n/@?�@>��@>Ov@>�@=�T@=��@=hs@=0�@=�@<��@<�)@<w�@<V�@<:�@;��@;b�@;J#@;1�@;�@:��@:O@9}�@9\�@8�@8K^@7�@7v`@7'�@7�@6�@6@5�"@4�K@4�o@4S�@4/�@3��@3��@3E9@2��@2ff@2.�@1�S@14@0�@0m�@/��@/C�@.�s@.a|@.#:@-�@-Vm@-<6@-@,��@,��@,�_@,[�@,K^@,�@+�@+�@+8@*�c@*��@*C�@*@)�d@)��@)T�@)q@)�@(��@(�D@(�Y@(l"@(A�@(2�@("h@(1@'�r@'�m@'|�@'Y@&�H@&��@&L0@%��@%��@%e,@%:�@%+@$��@$_@$9X@#�Q@#��@#b�@#RT@"��@"}V@!�@!�N@!�t@!��@!Vm@!5�@!(�@!+@!�@ �`@ �z@ r�@ 'R@��@s@@O@�@��@�@҉@��@�!@��@��@��@^5@=q@ �@rG@@@ѷ@`�@�@��@]�@�@�\@a|@M�@
�@��@�'@k�@!�@�5@��@�@|�@tT@l"@bN@*�@�@�0@a@!-@S@�2@�}@~�@Ta@($@�@ԕ@*0@�K@��@�.@D�@9X@	�@��@�g@��@��@!-@
=@�@l�@�@��@�X@��@A @(�@�@��@�K@[�@  @�A@��@��@�:@~�@e�@)_@͟@q�@d�@Q@;�@	@�@�h@m]@`B@S&@/@@�/@��@�U@N�@�@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�r|A�tTA�zDAԃ{AԆ�Aԅ�Aԅ�Aԇ�AԊ	Aԋ�AԋAԊ�Aԋ�AԌ~AԍAԏ(Aԏ�Aԑ4Aԏ�AԑhAԒ:Aԑ4Aԓ@AԔ{AԕAԕAԓuAԏ\A�xA�IA�|�A͙eA��A�5�AǇ_A�HKAĚ�AÏ�A�kA�wfA���A���A���A�1�A�A��lA�u%A�oA��TA���A���A��QA���A�cTA��A�e�A�0�A�چA�M�A�-A�8�A���A�ΥA�4A�&LA�HA�0�A�<�A���A��sA��A�{A���A��KA�
�A�`�A�kA�y�A��A��A��4A��0A���A���A��A��A���A��/A�{A��A��vA��rA���A�0�A��_A�A|qAz1Av�QAro�ApɆAo��An�Am�Ag�Ab�YA]��AZ"�AVMASYKAOh
ALOAKd�AG?�AE(�ADy�AD;AC�LAB��A@��A?,=A=�A:�A8�xA6��A5�A5a�A4h
A3GA0�$A0*0A0	lA/��A/J�A.�A.qA-|�A,HA+�A+�oA+ \A*��A)Z�A(SA'�A'9XA&�yA&�]A&6�A%��A%�OA%`BA$�PA#ȴA"	�A ��A S�A -wA �AaA��A��A�A_A~�A�A�XAh
A��A�A�A<�A˒A�9A-AĜA�+A��A~�An/AOA�MA;dA�/A�MAy>A"�AiDA�A{JAE9ASArGAƨAOA�A��AxlAA�AbA��A �A�<A�hAe�A�"A�NA~�A�A��AU�A�AĜAy�AC�A
�KA
�YA
!A	��A	.�A�A��AO�A�AW�A�Ap;A[�A-�A�LA�pA<6A��A��A��AZ�A�A��A-�A�AU2AOA A w2A 8A �@�m]@�]d@�خ@�y�@�Z�@���@��@�W?@��@�#:@�L�@��@�/�@��h@�H�@���@��6@��<@�6�@�?}@��@�h@��@�C-@�!�@�$@�&@�h�@�)_@�>B@���@�*0@�_�@��@�;@�V@�q@��@�}@�J�@�a@��5@�C�@�n@� i@�`�@㝲@��P@⻙@ᧇ@�^�@�@�M�@��@�c�@޳h@ݲ�@ܿ�@�xl@ۺ^@�8@���@�{�@٨X@�$t@��@���@؋D@� �@�r�@�	@ջ0@�f�@�ѷ@ӎ"@�/�@Ҍ�@��@��@П�@�_@�O@�X@Μx@���@�j@��@̣@̒�@�|@ʧ@�|�@�1�@���@�ϫ@ɡ�@�,�@��)@ȍ�@�M�@�PH@�;�@�x@ơb@Ɯx@�+k@��@��
@ŧ�@��@��f@��@�1'@��@Ü@�W?@��8@¾@�!@�"�@��,@���@�c�@�7�@�hs@��f@���@��U@��@�2�@��N@�a@�L0@�G@��}@��@@��B@�M@�K�@���@���@��.@��Y@�
�@���@�/�@��@��}@�B[@�?�@��S@�!-@�W�@��P@�<6@��f@���@�v�@�D�@�J@�خ@�j@��@��o@�5?@�_@��@��@@�iD@�&�@��@���@��@���@���@�x@���@��p@��z@�U2@� �@���@�Y�@�(@��@�Q@� �@���@��9@���@��@���@�j@�-w@��@�Ɇ@�]d@�-�@���@�zx@�f�@�*0@��@���@��'@��u@�g8@�6@��@��@��:@�]�@�(�@��[@�1@��S@�RT@�ی@��@�:*@�O@���@���@�|@��2@�_@�4@���@��:@��@��?@�E�@��@�� @���@�zx@�@��4@�N�@�7@���@�~�@�U�@�@��@�/�@��m@���@���@�j@�0�@���@��@���@�C�@�*�@��@��@�ԕ@���@��@���@�kQ@�	@��{@�33@��@��"@��@��[@��1@�R�@�$�@���@��@���@��@�^�@�S@��@���@�.�@���@���@��q@�m]@�:�@�&�@�!�@�C@�ی@��j@��9@��@���@�Ta@�2�@� �@�G@���@��	@�33@�	l@��|@�xl@�O@��;@���@�@O@��H@��)@���@���@�~�@�oi@�Q�@��C@�Q�@�"�@��@��.@�z�@�_�@�Ft@��@��a@�)_@��,@�n�@�N�@�:�@�($@� �@��.@�� @�[W@�,�@��@��@��@�Vm@�"�@��@���@��f@���@��M@���@�Ov@��m@���@�F�@�q@��@���@���@��@�PH@��@��@g�@&@~��@}�@}�@|�@{�+@{qv@z�!@z:*@y��@y5�@x�v@x�D@x�@w��@w@O@w�@v�c@v�]@v��@uԕ@u��@u+@tD�@s�r@s�}@s��@sU�@r�,@r��@rE�@q��@q�H@q��@qJ�@ph�@o�*@n�@n��@nGE@m��@m#�@l�p@lM@k��@k�{@k_p@j�M@j)�@i��@is�@i+@h�)@h  @f~�@fM�@f0U@e�-@d��@dtT@dc�@d�@cخ@b�L@b^5@b1�@b)�@b0U@b0U@a��@aG�@`��@`g8@_�k@_y�@_S�@^�8@^͟@^��@^E�@^�@]��@]��@]@@\��@\��@\g8@[��@[U�@[9�@Z�c@Z��@Z��@Zv�@Za|@ZV@ZGE@Z6�@Y�@Y0�@X�/@X�Y@Xw�@Xr�@Xy>@Xz�@XtT@XN�@X�@W��@W_p@W�@V�!@VkQ@V�@U�j@Uj@T�j@T��@T]d@T$@S�@S�@RkQ@Q��@Q+�@P�E@P6@O��@O�V@OW?@O1�@O�@N�2@N��@M�D@M�=@M=�@L��@L�I@Lw�@LbN@LN�@L"h@J�M@I�H@I�S@I�"@Izx@Iu�@Ij@IIR@I+�@H�E@H�u@HM@G@O@F��@F��@F\�@E�@Ee,@E�@D�u@D�@C�*@CiD@C]�@CC@B�B@B�@A�n@A/@A�@A@@�U@@Z@@�@?��@?��@?n/@?�@>��@>Ov@>�@=�T@=��@=hs@=0�@=�@<��@<�)@<w�@<V�@<:�@;��@;b�@;J#@;1�@;�@:��@:O@9}�@9\�@8�@8K^@7�@7v`@7'�@7�@6�@6@5�"@4�K@4�o@4S�@4/�@3��@3��@3E9@2��@2ff@2.�@1�S@14@0�@0m�@/��@/C�@.�s@.a|@.#:@-�@-Vm@-<6@-@,��@,��@,�_@,[�@,K^@,�@+�@+�@+8@*�c@*��@*C�@*@)�d@)��@)T�@)q@)�@(��@(�D@(�Y@(l"@(A�@(2�@("h@(1@'�r@'�m@'|�@'Y@&�H@&��@&L0@%��@%��@%e,@%:�@%+@$��@$_@$9X@#�Q@#��@#b�@#RT@"��@"}V@!�@!�N@!�t@!��@!Vm@!5�@!(�@!+@!�@ �`@ �z@ r�@ 'R@��@s@@O@�@��@�@҉@��@�!@��@��@��@^5@=q@ �@rG@@@ѷ@`�@�@��@]�@�@�\@a|@M�@
�@��@�'@k�@!�@�5@��@�@|�@tT@l"@bN@*�@�@�0@a@!-@S@�2@�}@~�@Ta@($@�@ԕ@*0@�K@��@�.@D�@9X@	�@��@�g@��@��@!-@
=@�@l�@�@��@�X@��@A @(�@�@��@�K@[�@  @�A@��@��@�:@~�@e�@)_@͟@q�@d�@Q@;�@	@�@�h@m]@`B@S&@/@@�/@��@�U@N�@�@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�FB
��B
�B
��B
��B
��B
�B
�B
��B
��B
��B
�B
�zB
��B
��B
�fB
��B
�B
��B
�`B
�,B
��B
�B
�2B
�B
�B
��B
�B
�lB2�B}�B�PB��B�?BؓB��B �B�B1B:^B[=Bk�Bl�Bw�B�B��B�B��B��B�}B��B�YB��B��B�B}"ByXBjBa�B_�BYKB^jB]�BZ�BSBM�BG�BC�B5%B�B�B�B�TB�B�/BοB�OB��B��B��ButB`\BQNB<�B*KB
XB
��B
��B
��B
n�B
W�B
N�B
,�B
-B	�|B	�|B	�B	��B	��B	�dB	��B	�SB	i_B	J=B	0UB	�B	�B��B�B�TB�tB�CB�B��B�B�B��B�qB��B�cB�%B��B	�B	AB	'B	�B	gB	)B	?�B	E�B	J�B	R�B	U�B	_!B	j�B	r-B	tTB	u%B	v�B	x�B	�iB	�B	�B	�XB	��B	�AB	�dB	�<B	�mB	�JB	�vB	�vB	�B	͟B	��B	��B	��B	�LB	��B	�B	�B	��B
�B
(B
�B
B
VB
�B
�B	��B
bB
TB
�B
�B
\B
�B
B
�B
�B
dB
�B
�B
�B
vB
B
oB
�B
�B
�B
�B
SB
mB
�B
SB
�B
EB
yB
�B
1B
kB
�B
CB
�B
�B
�B
�B
�B
�B
B
QB
�B
QB
B
�B
�B
B
B
+B
�B
�B
$B
2B
�B
@B
2B
�B
�B
�B
�B
�B
[B
&B
�B
 B
NB
HB
�B
�B
�B
pB
<B
�B
�B
�B
�B
�B
~B
JB
�B
�B

rB

	B
	7B
KB
zB
�B
�B
B
�B
B
 �B	��B	��B	��B
 B
  B
 �B	��B
�B
�B
�B
UB	��B	��B	�wB	��B	�(B	��B	��B	�JB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�^B	��B	�rB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�RB	�	B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	�>B	�*B	��B	�XB	��B	�XB	��B	�$B	�rB	�rB	��B	��B	��B	�B	�JB	��B	��B	��B	�xB	�dB	�JB	�B	��B	�*B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�qB	��B	��B	�qB	��B
uB
;B
 �B
 OB
 �B
B
�B
3B
�B
3B
�B
B
�B
�B
�B
�B
�B
;B
aB
�B
SB
�B
�B
KB
�B
	�B
	�B
	RB
�B
�B
B
�B
�B
MB
�B
B
�B
�B
B
zB
�B
�B

�B

�B

�B
B

�B
�B
B
�B
JB
�B
�B
�B
B
\B
�B
vB
\B
�B
�B
�B
.B
�B
B
 B
bB
B
HB
�B
NB
NB
NB
�B
�B
�B
B
 B
�B
 B
TB
 B
&B
�B
uB
�B
�B
�B
�B
�B
2B
�B
�B
gB
2B
mB
�B
�B
�B
B
EB
_B
_B
yB
_B
_B
B
eB
�B
�B
�B
#B
	B
#B
=B
=B
�B
]B
�B
B
B
�B
�B
OB
jB
jB
�B
�B
�B
pB
�B
 B
 BB
 vB
 vB
 �B
!-B
!�B
!�B
!�B
"B
"NB
"hB
#B
#B
#B
#TB
#nB
#nB
#nB
#�B
#�B
$tB
$�B
$�B
%FB
%�B
&2B
&LB
&LB
&LB
&LB
&�B
&�B
&�B
'B
'RB
'mB
'�B
'�B
(
B
(
B
(
B
(�B
(�B
(�B
(�B
)�B
)�B
)DB
)*B
)B
)�B
*0B
*KB
*KB
*B
+B
+B
+B
+6B
+QB
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-CB
-�B
.B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
0UB
0�B
1'B
1[B
1[B
1vB
1[B
1vB
1vB
2aB
2|B
2|B
2GB
2�B
3�B
3�B
3hB
3MB
3�B
3�B
3�B
4B
3�B
3�B
49B
4�B
6B
6zB
72B
6�B
7B
7B
7LB
7�B
7�B
8B
8lB
8�B
9XB
9�B
9�B
:*B
:�B
:�B
:�B
;0B
;0B
;0B
:�B
:xB
:DB
;�B
<B
<B
<PB
<�B
<�B
<�B
=B
="B
=<B
=<B
=<B
=VB
=�B
=�B
>(B
>]B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
AoB
A�B
A�B
A B
@�B
?�B
?�B
@ B
?�B
?}B
>�B
>�B
>�B
>�B
?HB
?�B
?�B
@4B
@iB
AB
A�B
AUB
AB
C-B
C{B
C-B
CB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C{B
C�B
C�B
C�B
C�B
D3B
DMB
DgB
D�B
DgB
DgB
DMB
D�B
EB
E9B
ESB
ESB
EmB
ESB
ESB
E9B
ESB
ESB
E�B
FB
FB
F�B
F�B
F�B
F�B
G_B
G�B
G�B
G�B
G�B
H1B
H1B
HKB
H�B
H�B
IRB
I�B
J	B
J#B
J	B
I�B
I�B
I�B
I�B
KB
J�B
KDB
KxB
K�B
K�B
K�B
K�B
KxB
L~B
MPB
MPB
MPB
MjB
MPB
MPB
MPB
MPB
M�B
MjB
M�B
NVB
N�B
N�B
N�B
OB
OvB
O�B
O�B
P.B
PbB
PbB
P}B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
SB
S&B
SuB
S�B
T,B
T,B
TaB
T�B
T�B
U2B
U2B
UMB
UgB
U�B
U�B
U�B
U�B
V9B
VB
VB
VB
V9B
V�B
WYB
W?B
XB
X+B
XyB
X�B
X�B
X�B
YKB
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[�B
\B
\)B
\CB
\�B
]IB
]dB
]�B
^5B
^�B
^�B
_!B
_!B
_VB
_�B
_�B
_�B
`BB
`\B
`\B
`�B
`vB
`�B
`�B
`�B
abB
a�B
a�B
bB
bhB
b�B
cB
c:B
cTB
c:B
c�B
dB
dB
d&B
d@B
dZB
dtB
d�B
dZB
dZB
eB
eFB
e`B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h
B
h
B
h�B
h�B
i�B
i�B
i�B
i�B
j0B
jeB
jeB
jeB
jB
j�B
j�B
kB
kQB
k�B
k�B
l"B
lWB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
mCB
m�B
nB
nB
n}B
n�B
o B
oiB
o�B
pUB
p�B
p�B
qB
q'B
q�B
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
r�B
sMB
shB
s�B
tB
tB
tTB
tnB
tnB
t�B
t�B
u%B
uB
u?B
vB
vFB
v+B
v�B
v�B
v�B
wLB
wLB
wfB
w�B
w�B
x8B
xRB
xlB
x�B
y>B
yrB
y�B
y�B
zDB
zDB
z^B
z^B
z^B
{B
{JB
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
|�B
}B
}VB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~]B
~BB
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�FB
��B
�B
��B
��B
��B
�B
�B
��B
��B
��B
�B
�zB
��B
��B
�fB
��B
�B
��B
�`B
�,B
��B
�B
�2B
�B
�B
��B
�B
�lB2�B}�B�PB��B�?BؓB��B �B�B1B:^B[=Bk�Bl�Bw�B�B��B�B��B��B�}B��B�YB��B��B�B}"ByXBjBa�B_�BYKB^jB]�BZ�BSBM�BG�BC�B5%B�B�B�B�TB�B�/BοB�OB��B��B��ButB`\BQNB<�B*KB
XB
��B
��B
��B
n�B
W�B
N�B
,�B
-B	�|B	�|B	�B	��B	��B	�dB	��B	�SB	i_B	J=B	0UB	�B	�B��B�B�TB�tB�CB�B��B�B�B��B�qB��B�cB�%B��B	�B	AB	'B	�B	gB	)B	?�B	E�B	J�B	R�B	U�B	_!B	j�B	r-B	tTB	u%B	v�B	x�B	�iB	�B	�B	�XB	��B	�AB	�dB	�<B	�mB	�JB	�vB	�vB	�B	͟B	��B	��B	��B	�LB	��B	�B	�B	��B
�B
(B
�B
B
VB
�B
�B	��B
bB
TB
�B
�B
\B
�B
B
�B
�B
dB
�B
�B
�B
vB
B
oB
�B
�B
�B
�B
SB
mB
�B
SB
�B
EB
yB
�B
1B
kB
�B
CB
�B
�B
�B
�B
�B
�B
B
QB
�B
QB
B
�B
�B
B
B
+B
�B
�B
$B
2B
�B
@B
2B
�B
�B
�B
�B
�B
[B
&B
�B
 B
NB
HB
�B
�B
�B
pB
<B
�B
�B
�B
�B
�B
~B
JB
�B
�B

rB

	B
	7B
KB
zB
�B
�B
B
�B
B
 �B	��B	��B	��B
 B
  B
 �B	��B
�B
�B
�B
UB	��B	��B	�wB	��B	�(B	��B	��B	�JB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�^B	��B	�rB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�RB	�	B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	��B	�>B	�*B	��B	�XB	��B	�XB	��B	�$B	�rB	�rB	��B	��B	��B	�B	�JB	��B	��B	��B	�xB	�dB	�JB	�B	��B	�*B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�qB	��B	��B	�qB	��B
uB
;B
 �B
 OB
 �B
B
�B
3B
�B
3B
�B
B
�B
�B
�B
�B
�B
;B
aB
�B
SB
�B
�B
KB
�B
	�B
	�B
	RB
�B
�B
B
�B
�B
MB
�B
B
�B
�B
B
zB
�B
�B

�B

�B

�B
B

�B
�B
B
�B
JB
�B
�B
�B
B
\B
�B
vB
\B
�B
�B
�B
.B
�B
B
 B
bB
B
HB
�B
NB
NB
NB
�B
�B
�B
B
 B
�B
 B
TB
 B
&B
�B
uB
�B
�B
�B
�B
�B
2B
�B
�B
gB
2B
mB
�B
�B
�B
B
EB
_B
_B
yB
_B
_B
B
eB
�B
�B
�B
#B
	B
#B
=B
=B
�B
]B
�B
B
B
�B
�B
OB
jB
jB
�B
�B
�B
pB
�B
 B
 BB
 vB
 vB
 �B
!-B
!�B
!�B
!�B
"B
"NB
"hB
#B
#B
#B
#TB
#nB
#nB
#nB
#�B
#�B
$tB
$�B
$�B
%FB
%�B
&2B
&LB
&LB
&LB
&LB
&�B
&�B
&�B
'B
'RB
'mB
'�B
'�B
(
B
(
B
(
B
(�B
(�B
(�B
(�B
)�B
)�B
)DB
)*B
)B
)�B
*0B
*KB
*KB
*B
+B
+B
+B
+6B
+QB
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-CB
-�B
.B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
0UB
0�B
1'B
1[B
1[B
1vB
1[B
1vB
1vB
2aB
2|B
2|B
2GB
2�B
3�B
3�B
3hB
3MB
3�B
3�B
3�B
4B
3�B
3�B
49B
4�B
6B
6zB
72B
6�B
7B
7B
7LB
7�B
7�B
8B
8lB
8�B
9XB
9�B
9�B
:*B
:�B
:�B
:�B
;0B
;0B
;0B
:�B
:xB
:DB
;�B
<B
<B
<PB
<�B
<�B
<�B
=B
="B
=<B
=<B
=<B
=VB
=�B
=�B
>(B
>]B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
AoB
A�B
A�B
A B
@�B
?�B
?�B
@ B
?�B
?}B
>�B
>�B
>�B
>�B
?HB
?�B
?�B
@4B
@iB
AB
A�B
AUB
AB
C-B
C{B
C-B
CB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C{B
C�B
C�B
C�B
C�B
D3B
DMB
DgB
D�B
DgB
DgB
DMB
D�B
EB
E9B
ESB
ESB
EmB
ESB
ESB
E9B
ESB
ESB
E�B
FB
FB
F�B
F�B
F�B
F�B
G_B
G�B
G�B
G�B
G�B
H1B
H1B
HKB
H�B
H�B
IRB
I�B
J	B
J#B
J	B
I�B
I�B
I�B
I�B
KB
J�B
KDB
KxB
K�B
K�B
K�B
K�B
KxB
L~B
MPB
MPB
MPB
MjB
MPB
MPB
MPB
MPB
M�B
MjB
M�B
NVB
N�B
N�B
N�B
OB
OvB
O�B
O�B
P.B
PbB
PbB
P}B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
SB
S&B
SuB
S�B
T,B
T,B
TaB
T�B
T�B
U2B
U2B
UMB
UgB
U�B
U�B
U�B
U�B
V9B
VB
VB
VB
V9B
V�B
WYB
W?B
XB
X+B
XyB
X�B
X�B
X�B
YKB
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[�B
\B
\)B
\CB
\�B
]IB
]dB
]�B
^5B
^�B
^�B
_!B
_!B
_VB
_�B
_�B
_�B
`BB
`\B
`\B
`�B
`vB
`�B
`�B
`�B
abB
a�B
a�B
bB
bhB
b�B
cB
c:B
cTB
c:B
c�B
dB
dB
d&B
d@B
dZB
dtB
d�B
dZB
dZB
eB
eFB
e`B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h
B
h
B
h�B
h�B
i�B
i�B
i�B
i�B
j0B
jeB
jeB
jeB
jB
j�B
j�B
kB
kQB
k�B
k�B
l"B
lWB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
mCB
m�B
nB
nB
n}B
n�B
o B
oiB
o�B
pUB
p�B
p�B
qB
q'B
q�B
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
r�B
sMB
shB
s�B
tB
tB
tTB
tnB
tnB
t�B
t�B
u%B
uB
u?B
vB
vFB
v+B
v�B
v�B
v�B
wLB
wLB
wfB
w�B
w�B
x8B
xRB
xlB
x�B
y>B
yrB
y�B
y�B
zDB
zDB
z^B
z^B
z^B
{B
{JB
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
|�B
}B
}VB
}�B
}�B
}�B
}�B
~B
~BB
~BB
~]B
~BB
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220729034009  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220729034216  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220729034217  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220729034217                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220729124222  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220729124222  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220729040437                      G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:24:45Z creation;2022-06-04T19:24:46Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192445  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               QA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�qʆB1   @�q���@,8Q���d
�t�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B���B�  B�  B�  B�  B�33B�  B���B�  C � C�fC  C�C��C	�fC  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CG�fCJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr33Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dك3D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @$z�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBxG�B�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�BӽqB��B۽qB��B��B��B��B�#�B��B��qB��C xRC޸C�RC�C�C	޸C�RC�RC�RC�RC�RC�RC�RC�RC�RC�C�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCD�CE�RCG޸CI�RCK�RCM�RCO�RCQ�RCS޸CU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCp�Cr+�Cs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D�zD�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE�zDFzDF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��=D�=D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
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
D�=D�B=D�
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
D�B=Dق=Dٿ
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
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�s�A�m)A�O�A�<�A��A��jA�T,A��A�� Aʰ�Aʀ�A�� Aɝ�Aɖ�AɈ1AɅSAɃ{A�{�A�t�A�\�A�<�A�'RA�A�A��A��]A���A��A��A��A���A��NA�ϫA�ɺA��'AȾBAȹXAȯOAȡ-Aȑ�AȃA�v�A�f�A�UgA�F�A�49A�,=A��A�hA���Aǣ�A�&�AŞ�Aī�A�>wA��-A�o�A�HKA��WA¹XA�A���A�8�A��kA���A��>A��A���A�N�A�k�A�(�A�MA�U�A�G�A�g�A�I�A�x8A��]A���A�*�A�֡A�c�A���A���A�m]A��A�A A�$A��.A��hA��A�OA�v�A��A���A��A���A�W?A�ȀA�l"A��nA���A�6FA��mA��UA~8AvC-ArN�AlCAi>�Ah($Af|Acd�A`��A^�WA\�eAW��AU.�AR=�AO��AM�+AJ:�AG�^AF�AC��ABA@A?{JA?>BA?�AA<��A:cA9_�A8ƨA8�hA7��A6QA4��A4VA3~�A30�A2�KA2g8A1�aA1u�A1!�A0�	A/� A/E�A.�HA.s�A.ZA.*�A-�A-�9A-|�A-=qA,��A+�A*XyA)��A)��A)J#A)�A)	�A(�A'tTA'�A&l"A&�A%��A$��A$B[A#ĜA#�kA#p;A#<�A"��A"MA!MA Q�A��A�_A�A0UA>�A;dA�Ab�A/�A��A�A^5A�TAN�A�A�_AC-A�A��A��Az�Ao�Aa�A�A��A��AJA�A�YA/�A��Av`A5?A�,AdZA�AH�A��AhsAJ�A�A�9Ap;AݘA��A�A�~Aa|A1�A��A�	A��A!A^5AxA��A_�A(�A
��A
�-A
��A
CA	�$A	��A	xA	A�A��A�A�{A��A��Am�AL�A4nA*�A�A-�AݘA8�A�YAuA�A;A8�A rGA �@��@�ȴ@��@���@�(@��@�*�@��@�Z�@�7L@�_@���@��>@��@���@���@�'�@��Q@�"@�`B@��H@�F@��@�Ĝ@��@�f�@�'�@�;d@���@�f@�<@��@�@�8@�h
@�ݘ@��H@�J�@�@��@ℶ@���@�!-@�PH@��@�^�@�<�@��9@�o @ܞ�@��6@�4�@ژ_@�-�@��r@���@��@ؕ@�!@��}@׷@פ@@�}�@�>�@��@�ں@��@ջ0@�b�@��'@�/�@��g@ӑh@�O�@��y@�>B@�;@Ю}@Ѐ�@�C�@��@���@�x@�@Χ@�@��j@͓@�-w@̯O@�y>@�Ov@˯�@��@��]@ʗ�@�m�@�7�@ɫ�@�B�@ȉ�@��o@�J�@��@Ď�@Æ�@�=@�q�@�&�@��@��@��h@��N@�(�@���@�oi@�Ov@�&�@�@��@��2@��O@���@��,@�}V@���@�ԕ@��@�q@���@���@�S&@��@��r@�8�@��@��3@�[W@�'�@�S@��4@�?@��g@���@�4�@��+@��@��@��|@���@���@��@�u�@�W�@��@�b�@�5�@��R@�u%@�1'@��@�ݘ@���@�Dg@�ѷ@�H�@���@�@O@��@��@���@�v�@�
�@�o�@��y@��@�`�@�	�@��@��a@���@���@��$@�{J@��@�-@��N@���@�@O@�&�@��8@���@�a|@�J�@�?�@�;�@�6�@��@��6@��P@�@���@��d@�L�@��5@�p;@��m@��g@���@���@���@�p�@�9�@���@��@�u%@�A�@��+@��f@�|�@�e,@���@�e@��=@�dZ@��@��@��@���@��V@�o @�O�@�E9@�%F@��P@��<@���@�m�@�Ft@�<�@�-@��m@���@�V@��@���@��Y@�ԕ@�E9@��@���@�ȴ@���@�h�@�7�@��@��@��c@�|�@�$@��0@�@O@���@��Y@��@�:*@��@� �@��@���@�33@���@���@���@��F@�~�@�\�@�L0@�4n@��;@�a�@�
=@���@���@�(�@��C@��	@�H�@��@��p@���@���@�y>@�ff@�5?@���@���@�Vm@��\@�Ov@�-�@�	@��@��	@��@���@���@�#:@���@�K�@��@���@���@��.@�=q@�&�@��@��}@��@��@�a@�&�@��c@��'@��u@�!@�خ@��@�?}@�ȴ@�D�@��@�@iD@9�@~�L@~	@}��@}�C@}}�@}p�@}L�@|��@{|�@{Mj@{o@{ i@z��@y�-@yV@x�4@x/�@x@wخ@w&@w�@v�c@v�1@v�@ux�@u�@t]d@s�@s��@s/�@r�"@r��@r�@ru%@q�@qO�@p�j@p�@o�W@o�;@o��@o,�@n}V@m��@mzx@m�@l�p@l�o@l �@k�F@kb�@j�@j�B@j��@jGE@i�@izx@i!�@hr�@h	�@g��@g�:@gv`@gS�@g.I@f��@f^5@f�@e�#@e��@e�@d��@dA�@d�@c�w@c��@cF�@b��@bL0@arG@a#�@`��@`��@`-�@`@`x@_�+@_O@^��@^4@]�@]*0@\��@\�D@\C-@\,=@\%�@\  @[�@[�q@[iD@Z�2@Z�\@ZGE@Y��@Y*0@X7�@W��@W�@Vv�@U��@U�=@U}�@UB�@T��@T�e@T�@S��@S�@S��@S(@Rq�@Q��@Q��@QVm@Q�@P�U@Pc�@P:�@O�]@On/@N��@N��@Np;@NYK@NO@M�)@Mw2@L�@L��@Lr�@L6@L7@K��@K/�@K(@J��@Jxl@I@Ik�@H��@Hy>@Hb@G�
@G@O@F}V@FV@F=q@E��@E�@DbN@D%�@C�a@C�k@Cy�@C>�@B��@B_@A��@A�"@@��@@-�@@@?��@?��@?"�@>�]@>��@>ff@>W�@=��@=��@=5�@<�[@<�_@<?�@;� @;C�@;�@:ȴ@:��@:R�@:-@:O@9��@9�@9�7@9X@9�@8��@8H@8'R@7�[@7��@7�k@7~�@78@7�@6�2@6��@6B[@5�z@5c@5c�@57L@4��@4��@4�@4G@3��@3�&@3��@3�4@3P�@3S@2҉@2�m@2u%@2+k@1�h@17L@0�@0��@0�Y@/�r@/~�@.�M@.�s@.H�@.!�@.	@-�.@-�X@-L�@--w@-�@,��@,`�@,D�@,�@+�@+��@+A�@+S@*�r@*l�@*ff@*E�@*�@)��@)#�@(�@(>B@'�@'F�@&��@&��@&��@&��@&xl@&C�@%�D@%�d@%��@%V@$��@$4n@#��@#��@#��@#�f@#��@#v`@#F�@#
=@"�y@"��@"s�@"J�@"+k@"$�@"e@!�@!`B@!;@ �@ ~(@ C-@ �@��@�;@�w@�4@b�@e�@e�@O@)_@�@�@��@�B@�@�m@�!@��@_�@H�@6�@-@$�@�z@�~@s�@f�@Y�@/@@��@�/@��@PH@,=@�@��@v`@J#@@O@9�@�@��@u%@Ta@5?@J@�@=�@��@��@��@w�@PH@	�@� @�w@v`@8@�@�@�@�s@�@��@_�@6�@	@��@�@�@�@�@�X@��@f�@!�@�E@�@]d@$@�@{J@P�@'�@@�@�@�F@-@��@��@/@�@��@|�@l"@I�@�;@�4@g�@H�@!-@S@�@�]@�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�s�A�m)A�O�A�<�A��A��jA�T,A��A�� Aʰ�Aʀ�A�� Aɝ�Aɖ�AɈ1AɅSAɃ{A�{�A�t�A�\�A�<�A�'RA�A�A��A��]A���A��A��A��A���A��NA�ϫA�ɺA��'AȾBAȹXAȯOAȡ-Aȑ�AȃA�v�A�f�A�UgA�F�A�49A�,=A��A�hA���Aǣ�A�&�AŞ�Aī�A�>wA��-A�o�A�HKA��WA¹XA�A���A�8�A��kA���A��>A��A���A�N�A�k�A�(�A�MA�U�A�G�A�g�A�I�A�x8A��]A���A�*�A�֡A�c�A���A���A�m]A��A�A A�$A��.A��hA��A�OA�v�A��A���A��A���A�W?A�ȀA�l"A��nA���A�6FA��mA��UA~8AvC-ArN�AlCAi>�Ah($Af|Acd�A`��A^�WA\�eAW��AU.�AR=�AO��AM�+AJ:�AG�^AF�AC��ABA@A?{JA?>BA?�AA<��A:cA9_�A8ƨA8�hA7��A6QA4��A4VA3~�A30�A2�KA2g8A1�aA1u�A1!�A0�	A/� A/E�A.�HA.s�A.ZA.*�A-�A-�9A-|�A-=qA,��A+�A*XyA)��A)��A)J#A)�A)	�A(�A'tTA'�A&l"A&�A%��A$��A$B[A#ĜA#�kA#p;A#<�A"��A"MA!MA Q�A��A�_A�A0UA>�A;dA�Ab�A/�A��A�A^5A�TAN�A�A�_AC-A�A��A��Az�Ao�Aa�A�A��A��AJA�A�YA/�A��Av`A5?A�,AdZA�AH�A��AhsAJ�A�A�9Ap;AݘA��A�A�~Aa|A1�A��A�	A��A!A^5AxA��A_�A(�A
��A
�-A
��A
CA	�$A	��A	xA	A�A��A�A�{A��A��Am�AL�A4nA*�A�A-�AݘA8�A�YAuA�A;A8�A rGA �@��@�ȴ@��@���@�(@��@�*�@��@�Z�@�7L@�_@���@��>@��@���@���@�'�@��Q@�"@�`B@��H@�F@��@�Ĝ@��@�f�@�'�@�;d@���@�f@�<@��@�@�8@�h
@�ݘ@��H@�J�@�@��@ℶ@���@�!-@�PH@��@�^�@�<�@��9@�o @ܞ�@��6@�4�@ژ_@�-�@��r@���@��@ؕ@�!@��}@׷@פ@@�}�@�>�@��@�ں@��@ջ0@�b�@��'@�/�@��g@ӑh@�O�@��y@�>B@�;@Ю}@Ѐ�@�C�@��@���@�x@�@Χ@�@��j@͓@�-w@̯O@�y>@�Ov@˯�@��@��]@ʗ�@�m�@�7�@ɫ�@�B�@ȉ�@��o@�J�@��@Ď�@Æ�@�=@�q�@�&�@��@��@��h@��N@�(�@���@�oi@�Ov@�&�@�@��@��2@��O@���@��,@�}V@���@�ԕ@��@�q@���@���@�S&@��@��r@�8�@��@��3@�[W@�'�@�S@��4@�?@��g@���@�4�@��+@��@��@��|@���@���@��@�u�@�W�@��@�b�@�5�@��R@�u%@�1'@��@�ݘ@���@�Dg@�ѷ@�H�@���@�@O@��@��@���@�v�@�
�@�o�@��y@��@�`�@�	�@��@��a@���@���@��$@�{J@��@�-@��N@���@�@O@�&�@��8@���@�a|@�J�@�?�@�;�@�6�@��@��6@��P@�@���@��d@�L�@��5@�p;@��m@��g@���@���@���@�p�@�9�@���@��@�u%@�A�@��+@��f@�|�@�e,@���@�e@��=@�dZ@��@��@��@���@��V@�o @�O�@�E9@�%F@��P@��<@���@�m�@�Ft@�<�@�-@��m@���@�V@��@���@��Y@�ԕ@�E9@��@���@�ȴ@���@�h�@�7�@��@��@��c@�|�@�$@��0@�@O@���@��Y@��@�:*@��@� �@��@���@�33@���@���@���@��F@�~�@�\�@�L0@�4n@��;@�a�@�
=@���@���@�(�@��C@��	@�H�@��@��p@���@���@�y>@�ff@�5?@���@���@�Vm@��\@�Ov@�-�@�	@��@��	@��@���@���@�#:@���@�K�@��@���@���@��.@�=q@�&�@��@��}@��@��@�a@�&�@��c@��'@��u@�!@�خ@��@�?}@�ȴ@�D�@��@�@iD@9�@~�L@~	@}��@}�C@}}�@}p�@}L�@|��@{|�@{Mj@{o@{ i@z��@y�-@yV@x�4@x/�@x@wخ@w&@w�@v�c@v�1@v�@ux�@u�@t]d@s�@s��@s/�@r�"@r��@r�@ru%@q�@qO�@p�j@p�@o�W@o�;@o��@o,�@n}V@m��@mzx@m�@l�p@l�o@l �@k�F@kb�@j�@j�B@j��@jGE@i�@izx@i!�@hr�@h	�@g��@g�:@gv`@gS�@g.I@f��@f^5@f�@e�#@e��@e�@d��@dA�@d�@c�w@c��@cF�@b��@bL0@arG@a#�@`��@`��@`-�@`@`x@_�+@_O@^��@^4@]�@]*0@\��@\�D@\C-@\,=@\%�@\  @[�@[�q@[iD@Z�2@Z�\@ZGE@Y��@Y*0@X7�@W��@W�@Vv�@U��@U�=@U}�@UB�@T��@T�e@T�@S��@S�@S��@S(@Rq�@Q��@Q��@QVm@Q�@P�U@Pc�@P:�@O�]@On/@N��@N��@Np;@NYK@NO@M�)@Mw2@L�@L��@Lr�@L6@L7@K��@K/�@K(@J��@Jxl@I@Ik�@H��@Hy>@Hb@G�
@G@O@F}V@FV@F=q@E��@E�@DbN@D%�@C�a@C�k@Cy�@C>�@B��@B_@A��@A�"@@��@@-�@@@?��@?��@?"�@>�]@>��@>ff@>W�@=��@=��@=5�@<�[@<�_@<?�@;� @;C�@;�@:ȴ@:��@:R�@:-@:O@9��@9�@9�7@9X@9�@8��@8H@8'R@7�[@7��@7�k@7~�@78@7�@6�2@6��@6B[@5�z@5c@5c�@57L@4��@4��@4�@4G@3��@3�&@3��@3�4@3P�@3S@2҉@2�m@2u%@2+k@1�h@17L@0�@0��@0�Y@/�r@/~�@.�M@.�s@.H�@.!�@.	@-�.@-�X@-L�@--w@-�@,��@,`�@,D�@,�@+�@+��@+A�@+S@*�r@*l�@*ff@*E�@*�@)��@)#�@(�@(>B@'�@'F�@&��@&��@&��@&��@&xl@&C�@%�D@%�d@%��@%V@$��@$4n@#��@#��@#��@#�f@#��@#v`@#F�@#
=@"�y@"��@"s�@"J�@"+k@"$�@"e@!�@!`B@!;@ �@ ~(@ C-@ �@��@�;@�w@�4@b�@e�@e�@O@)_@�@�@��@�B@�@�m@�!@��@_�@H�@6�@-@$�@�z@�~@s�@f�@Y�@/@@��@�/@��@PH@,=@�@��@v`@J#@@O@9�@�@��@u%@Ta@5?@J@�@=�@��@��@��@w�@PH@	�@� @�w@v`@8@�@�@�@�s@�@��@_�@6�@	@��@�@�@�@�@�X@��@f�@!�@�E@�@]d@$@�@{J@P�@'�@@�@�@�F@-@��@��@/@�@��@|�@l"@I�@�;@�4@g�@H�@!-@S@�@�]@�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	�%B	�B	�B	�|B	�[B	��B	�B	�B	��B	�|B
�B
%B
	�B

rB

�B
�B
~B
B
pB
�B
B
�B
�B
B
B
�B
B
qB
�B
�B
jB
�B
�B
 �B
!HB
!�B
"�B
$�B
&B
'B
'mB
'�B
'�B
'�B
'8B
&�B
%�B
$�B
$ZB
'�B
gmB
~�B
��B
��B
�QB
�/B
�B
�B
��B
��B
�cB
��B)B)�Ba�BfBoiB�B��BңB�aB�*B�B
�B
=B
�B�B �B�B�BbB
�BB�VB��B߾B�{B�kB�B��Bq[BV�BE�B)�B�B
��B
��B
�B
��B
��B
~B
d�B
@�B
-B
B	ܬB	��B	�xB	��B	��B	{JB	jeB	\�B	P�B	FB	2�B	2GB	,�B	*�B	&fB	 vB	1B	�B	EB	<�B	R�B	U�B	a-B	pUB	x8B	��B	�2B	�B	�~B	�B	��B	��B	�.B
�B
B
�B
B
)�B
,WB
-)B
6�B
8�B
;�B
?cB
C�B
D�B
JrB
U�B
\CB
e�B
h�B
h�B
i*B
e�B
c�B
c�B
fLB
gRB
g�B
gB
d&B
b�B
abB
`'B
_�B
\)B
]~B
^�B
`�B
aB
a�B
bB
a�B
_B
[�B
ZB
X�B
W�B
U�B
W
B
XEB
W$B
W$B
W$B
VmB
U�B
T�B
UgB
S�B
RB
P�B
Q4B
P}B
N�B
OBB
QhB
Q�B
QNB
QNB
P�B
Q B
OB
N<B
MPB
L�B
MPB
N�B
P�B
P�B
O�B
N�B
LJB
GB
E�B
GEB
G�B
H1B
H�B
E�B
F�B
F�B
GzB
I�B
I�B
H�B
F�B
C{B
A�B
=�B
;�B
9�B
9XB
9	B
8�B
:B
:�B
<�B
;0B
:�B
:^B
:DB
8B
4�B
2�B
0!B
.�B
-�B
,�B
,=B
+kB
*�B
(�B
'�B
%�B
"�B
!bB
 �B
�B
WB
�B
�B
uB
�B
PB

#B
VB
4B
bB
�B
�B
}B
(B

�B
	�B
�B
�B
�B
B
�B
aB	�B
;B
-B
MB
+B
�B
�B
�B
	�B
PB
^B

�B

rB
�B
�B
�B
�B
EB
tB
�B
?B
�B
mB
SB
�B
aB
B
�B
�B
�B
{B
GB
�B
�B
�B
�B
�B
�B
'B
�B
uB
�B
[B
uB
[B
'B
B
[B
�B
B
�B
�B
�B
�B
oB
;B
 B
�B
oB
oB
oB
;B
 B
B
B
 �B
B
 �B
 �B
 �B
 B
 �B
 iB
B
 B
�B
;B
 �B
 �B
�B
B
oB
B
 �B
B
 B	��B	��B
 B
 4B	��B	��B
  B
B
B
UB
B
 �B
 �B
 �B
UB
 �B
 �B
�B
�B
�B
'B
B
[B
uB
�B
�B
B
MB
�B
B
B
SB
�B
�B
�B
B
tB
�B
�B
+B
�B
�B
fB
	7B
	7B
	7B
	7B
	7B
	B
	�B
	�B

	B

�B

�B

�B
B
B
B
xB
�B
JB
dB
B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
BB
\B
BB
BB
(B
�B
�B
.B
HB
bB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
bB
�B
.B
HB
�B
}B
 B
B
uB
aB
�B
B
MB
�B
�B
�B
�B
�B
�B
9B
B
B
B
�B
�B
+B
�B
1B
B
�B
�B
_B
�B
_B
EB
+B
+B
yB
_B
�B
1B
eB
KB
B
�B
7B
�B
�B
�B
)B
�B
�B
�B
B
dB
dB
~B
~B
B
;B
�B
 �B
 �B
!B
!�B
!�B
!�B
"4B
"NB
"4B
"B
"�B
# B
#:B
#nB
#nB
#�B
#�B
#�B
#�B
#�B
$B
$tB
$�B
%FB
%,B
%�B
&2B
&B
&�B
&�B
&�B
'B
'B
'B
'B
'B
'RB
'�B
'RB
(�B
(�B
(�B
(�B
(�B
)DB
)�B
*0B
)�B
*eB
*�B
+QB
+�B
+�B
+�B
,=B
,�B
,qB
,�B
,�B
,�B
,�B
-CB
-�B
./B
.B
./B
.�B
/iB
/�B
0�B
1B
1�B
1�B
1�B
1�B
1�B
2|B
3MB
3�B
3�B
4B
49B
4TB
4�B
4�B
4�B
4�B
4�B
5�B
6�B
7�B
7�B
8B
8B
8RB
8�B
8�B
9	B
9>B
9�B
:B
:*B
:�B
:�B
;B
;JB
;dB
;JB
;dB
;dB
;�B
<B
<jB
<�B
<�B
<�B
<�B
=<B
=�B
>B
>(B
>wB
>�B
>�B
?B
?HB
?}B
?�B
?�B
?�B
?�B
@B
@OB
@OB
A B
AB
AoB
AoB
A�B
A�B
A�B
B'B
B'B
BuB
B�B
B�B
CaB
C�B
C�B
C�B
D3B
DMB
DMB
D�B
DgB
D�B
D�B
D�B
D�B
EB
EB
D�B
D�B
EmB
EmB
E�B
F?B
F?B
F?B
F�B
F�B
F�B
F�B
F�B
GB
G+B
G_B
G�B
HB
HB
H�B
IB
I�B
IlB
I�B
I�B
I�B
I�B
I�B
J	B
J	B
J#B
J�B
J�B
J�B
JrB
J�B
K^B
KxB
K�B
K�B
LB
L�B
L�B
MB
L�B
M�B
N<B
NVB
N<B
N"B
N<B
N<B
N�B
N�B
N�B
OB
O(B
O(B
O\B
O�B
O�B
O�B
O�B
PHB
P�B
Q B
QNB
Q�B
R�B
SB
S&B
S�B
TB
S�B
T�B
T�B
U�B
VmB
VmB
V�B
V�B
W$B
WYB
W?B
W�B
W�B
W�B
W�B
XEB
XyB
X�B
YKB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[�B
[�B
T�B
\)B
\)B
\CB
\�B
\�B
]/B
]dB
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_!B
^�B
_B
_!B
^�B
^�B
_B
_VB
_�B
_�B
`�B
aHB
a�B
a�B
a�B
a�B
b4B
b4B
bNB
bhB
bB
aHB
a�B
a�B
b�B
c:B
cnB
cTB
cTB
b�B
c�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
f2B
f2B
f�B
f�B
gB
f�B
f�B
gB
gRB
gRB
g�B
g�B
g�B
h
B
h�B
i*B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jKB
j0B
j0B
j�B
j�B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
n/B
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
oB
oOB
o5B
o5B
o5B
o5B
oOB
oiB
o�B
oiB
oiB
o�B
o�B
o�B
o�B
pB
p!B
p!B
p!B
p;B
pUB
pUB
p�B
p�B
p�B
p�B
qB
qB
p�B
q[B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
s3B
s3B
s3B
s3B
sMB
tB
tB
tB
tnB
tnB
tnB
t�B
t�B
tnB
t�B
t�B
uB
uB
u�B
utB
utB
uZB
uZB
utB
utB
u�B
u�B
u�B
vB
vFB
v�B
v�B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
y$B
y>B
y�B
y�B
zB
zB
z*B
zDB
z�B
{B
{B
{0B
{dB
{B
{B
{B
|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	�%B	�B	�B	�|B	�[B	��B	�B	�B	��B	�|B
�B
%B
	�B

rB

�B
�B
~B
B
pB
�B
B
�B
�B
B
B
�B
B
qB
�B
�B
jB
�B
�B
 �B
!HB
!�B
"�B
$�B
&B
'B
'mB
'�B
'�B
'�B
'8B
&�B
%�B
$�B
$ZB
'�B
gmB
~�B
��B
��B
�QB
�/B
�B
�B
��B
��B
�cB
��B)B)�Ba�BfBoiB�B��BңB�aB�*B�B
�B
=B
�B�B �B�B�BbB
�BB�VB��B߾B�{B�kB�B��Bq[BV�BE�B)�B�B
��B
��B
�B
��B
��B
~B
d�B
@�B
-B
B	ܬB	��B	�xB	��B	��B	{JB	jeB	\�B	P�B	FB	2�B	2GB	,�B	*�B	&fB	 vB	1B	�B	EB	<�B	R�B	U�B	a-B	pUB	x8B	��B	�2B	�B	�~B	�B	��B	��B	�.B
�B
B
�B
B
)�B
,WB
-)B
6�B
8�B
;�B
?cB
C�B
D�B
JrB
U�B
\CB
e�B
h�B
h�B
i*B
e�B
c�B
c�B
fLB
gRB
g�B
gB
d&B
b�B
abB
`'B
_�B
\)B
]~B
^�B
`�B
aB
a�B
bB
a�B
_B
[�B
ZB
X�B
W�B
U�B
W
B
XEB
W$B
W$B
W$B
VmB
U�B
T�B
UgB
S�B
RB
P�B
Q4B
P}B
N�B
OBB
QhB
Q�B
QNB
QNB
P�B
Q B
OB
N<B
MPB
L�B
MPB
N�B
P�B
P�B
O�B
N�B
LJB
GB
E�B
GEB
G�B
H1B
H�B
E�B
F�B
F�B
GzB
I�B
I�B
H�B
F�B
C{B
A�B
=�B
;�B
9�B
9XB
9	B
8�B
:B
:�B
<�B
;0B
:�B
:^B
:DB
8B
4�B
2�B
0!B
.�B
-�B
,�B
,=B
+kB
*�B
(�B
'�B
%�B
"�B
!bB
 �B
�B
WB
�B
�B
uB
�B
PB

#B
VB
4B
bB
�B
�B
}B
(B

�B
	�B
�B
�B
�B
B
�B
aB	�B
;B
-B
MB
+B
�B
�B
�B
	�B
PB
^B

�B

rB
�B
�B
�B
�B
EB
tB
�B
?B
�B
mB
SB
�B
aB
B
�B
�B
�B
{B
GB
�B
�B
�B
�B
�B
�B
'B
�B
uB
�B
[B
uB
[B
'B
B
[B
�B
B
�B
�B
�B
�B
oB
;B
 B
�B
oB
oB
oB
;B
 B
B
B
 �B
B
 �B
 �B
 �B
 B
 �B
 iB
B
 B
�B
;B
 �B
 �B
�B
B
oB
B
 �B
B
 B	��B	��B
 B
 4B	��B	��B
  B
B
B
UB
B
 �B
 �B
 �B
UB
 �B
 �B
�B
�B
�B
'B
B
[B
uB
�B
�B
B
MB
�B
B
B
SB
�B
�B
�B
B
tB
�B
�B
+B
�B
�B
fB
	7B
	7B
	7B
	7B
	7B
	B
	�B
	�B

	B

�B

�B

�B
B
B
B
xB
�B
JB
dB
B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
BB
\B
BB
BB
(B
�B
�B
.B
HB
bB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
bB
�B
.B
HB
�B
}B
 B
B
uB
aB
�B
B
MB
�B
�B
�B
�B
�B
�B
9B
B
B
B
�B
�B
+B
�B
1B
B
�B
�B
_B
�B
_B
EB
+B
+B
yB
_B
�B
1B
eB
KB
B
�B
7B
�B
�B
�B
)B
�B
�B
�B
B
dB
dB
~B
~B
B
;B
�B
 �B
 �B
!B
!�B
!�B
!�B
"4B
"NB
"4B
"B
"�B
# B
#:B
#nB
#nB
#�B
#�B
#�B
#�B
#�B
$B
$tB
$�B
%FB
%,B
%�B
&2B
&B
&�B
&�B
&�B
'B
'B
'B
'B
'B
'RB
'�B
'RB
(�B
(�B
(�B
(�B
(�B
)DB
)�B
*0B
)�B
*eB
*�B
+QB
+�B
+�B
+�B
,=B
,�B
,qB
,�B
,�B
,�B
,�B
-CB
-�B
./B
.B
./B
.�B
/iB
/�B
0�B
1B
1�B
1�B
1�B
1�B
1�B
2|B
3MB
3�B
3�B
4B
49B
4TB
4�B
4�B
4�B
4�B
4�B
5�B
6�B
7�B
7�B
8B
8B
8RB
8�B
8�B
9	B
9>B
9�B
:B
:*B
:�B
:�B
;B
;JB
;dB
;JB
;dB
;dB
;�B
<B
<jB
<�B
<�B
<�B
<�B
=<B
=�B
>B
>(B
>wB
>�B
>�B
?B
?HB
?}B
?�B
?�B
?�B
?�B
@B
@OB
@OB
A B
AB
AoB
AoB
A�B
A�B
A�B
B'B
B'B
BuB
B�B
B�B
CaB
C�B
C�B
C�B
D3B
DMB
DMB
D�B
DgB
D�B
D�B
D�B
D�B
EB
EB
D�B
D�B
EmB
EmB
E�B
F?B
F?B
F?B
F�B
F�B
F�B
F�B
F�B
GB
G+B
G_B
G�B
HB
HB
H�B
IB
I�B
IlB
I�B
I�B
I�B
I�B
I�B
J	B
J	B
J#B
J�B
J�B
J�B
JrB
J�B
K^B
KxB
K�B
K�B
LB
L�B
L�B
MB
L�B
M�B
N<B
NVB
N<B
N"B
N<B
N<B
N�B
N�B
N�B
OB
O(B
O(B
O\B
O�B
O�B
O�B
O�B
PHB
P�B
Q B
QNB
Q�B
R�B
SB
S&B
S�B
TB
S�B
T�B
T�B
U�B
VmB
VmB
V�B
V�B
W$B
WYB
W?B
W�B
W�B
W�B
W�B
XEB
XyB
X�B
YKB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[qB
[�B
[�B
T�B
\)B
\)B
\CB
\�B
\�B
]/B
]dB
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_!B
^�B
_B
_!B
^�B
^�B
_B
_VB
_�B
_�B
`�B
aHB
a�B
a�B
a�B
a�B
b4B
b4B
bNB
bhB
bB
aHB
a�B
a�B
b�B
c:B
cnB
cTB
cTB
b�B
c�B
d�B
d�B
eB
e`B
e�B
e�B
e�B
f2B
f2B
f�B
f�B
gB
f�B
f�B
gB
gRB
gRB
g�B
g�B
g�B
h
B
h�B
i*B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jKB
j0B
j0B
j�B
j�B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
n/B
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
oB
oOB
o5B
o5B
o5B
o5B
oOB
oiB
o�B
oiB
oiB
o�B
o�B
o�B
o�B
pB
p!B
p!B
p!B
p;B
pUB
pUB
p�B
p�B
p�B
p�B
qB
qB
p�B
q[B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
s3B
s3B
s3B
s3B
sMB
tB
tB
tB
tnB
tnB
tnB
t�B
t�B
tnB
t�B
t�B
uB
uB
u�B
utB
utB
uZB
uZB
utB
utB
u�B
u�B
u�B
vB
vFB
v�B
v�B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
y$B
y>B
y�B
y�B
zB
zB
z*B
zDB
z�B
{B
{B
{0B
{dB
{B
{B
{B
|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105244  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192445  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192445  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192446                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042453  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042453  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                
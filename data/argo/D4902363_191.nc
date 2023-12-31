CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-20T21:35:14Z creation;2017-12-20T21:35:18Z conversion to V3.1;2019-12-19T07:53:46Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171220213514  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_191                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�>r���1   @�>swww�@;�hr� ��dX����?1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�33A   A   A@  A`  A�  A�  A�33A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@�=q@�
=A�A?�A_�A�A�A���A���A���A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)D {D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
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
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�;�D�{�Dտ
D��
D�?
D�
Dֿ
D��
D�?
D�
D׿
D���D�;�D�
Dؿ
D��
D�?
D�
Dٿ
D��
D�;�D�
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
D�=D�
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
D���D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��+A��DA��PA��A��A��A��A��7A��+A���A���A���A��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A��PA��7A�bA���A���A�z�A�hsA�hsA�dZA�I�A�A�A�;dA�oA���A���A�{A���A�|�A�/A���A�$�A�G�A��mA���A��hA��A���A�ȴA�33A���A���A�+A��+A�G�A���A�"�A�?}A�dZA��A��yA��A�oA���A��mA��A�1'A�|�A�ƨA�9XA~�\A}�A{��Az�Az�RAy�Axn�Av��Au�FAt��At�DAs�PArAqt�Aq"�Ap��Ap�An�Am�Ak��AiC�Ag��AgG�Af��Af=qAeC�Ac��AadZA_�mA^=qA\��A\I�A[��AZ�jAZE�AY7LAX�RAXE�AWp�AV{AUl�AU%AT~�AS�AS%ARI�AQ��AP�APVAP1AO��AN��AM��AL��AK�AJ��AJ(�AIXAH��AG�wAE�mAD��AC�;AB�AA��AAdZAA/A@��A@�!A@E�A?O�A>�A=��A<n�A;hsA:Q�A9��A9%A8��A8jA8E�A8�A7�#A7%A6��A6A�A61'A5A4�9A4^5A3�FA2VA1�A0VA0bA0bA0JA/�
A/�A.1'A-�A,�HA,JA+�PA+A*��A*ffA)�A(E�A&�+A%��A%S�A#K�A ��A E�A�FA
=A�A`BA�A�uA(�A��A\)A�A��A1'A�PA?}AVA��A�`A�/A�jA�\AbNAA�A�Ax�A�A^5A��A  A�wA
�A
r�A
bA	A9XA�7A �Ax�A�RAZA�A�!AffA-A �AbA��@�O�@��@���@�V@�J@��#@�O�@�\)@���@�ƨ@�@�I�@���@��@�h@�hs@���@��@�j@�@�+@�@��/@�R@���@��y@�hs@�  @�33@���@؋D@��@ׅ@��y@�n�@�b@ҧ�@д9@�
=@�5?@ͩ�@��@�r�@ʰ!@�`B@�ƨ@�;d@�@Ƨ�@�J@�`B@��@ċD@�Z@�I�@�1'@�9X@�(�@�(�@�(�@��@öF@�t�@�33@�M�@��#@�/@���@��!@���@�`B@�7L@�9X@�E�@�&�@�\)@��@��\@��@�7L@��D@��P@�-@��@�@��R@�E�@���@��h@��7@��7@��@�7L@��@���@��u@�Z@��m@���@�dZ@���@��^@���@�;d@��+@��#@���@��F@�33@�ȴ@�~�@�^5@�E�@�{@�p�@��@���@��@��@�5?@�%@�Z@�Q�@��m@���@�J@�hs@��D@��m@��P@�33@�
=@�
=@��@�ȴ@�M�@��h@���@���@�ƨ@�"�@�v�@�J@��-@�?}@���@�1'@�1@���@���@���@��w@�K�@�;d@�"�@���@�~�@�V@�^5@�M�@���@�Z@�1'@�  @��;@�ƨ@���@�|�@�|�@�t�@�S�@�"�@�o@��@���@�ff@�7L@�(�@�|�@���@���@��\@�v�@�n�@�~�@�v�@�M�@��@�`B@��/@�z�@�bN@�I�@�9X@�(�@�1@��F@�C�@�+@�+@�
=@��y@��\@�=q@�@��T@���@��^@���@��h@�p�@�/@���@��`@��j@�j@�@�P@�P@~�@~ff@}��@}?}@|��@|j@{��@{t�@{S�@{dZ@{t�@{�@{S�@z�!@z~�@y�7@x�@x  @w�@v��@vV@v5?@v{@u�T@u@uO�@t��@t�@tj@t9X@t�@s�m@sƨ@s��@s33@so@r�@r��@r^5@q��@q�@q�#@qhs@p��@p�9@p�9@p��@pr�@o�@pA�@p�`@p��@n�y@nff@nȴ@n�R@n�+@n5?@n�y@o�@o|�@o��@o|�@o�@n��@n��@mp�@l�/@j~�@i��@j�\@k@j�H@jM�@i�@i�^@i�7@iG�@h�`@hĜ@h�@hbN@hA�@h1'@h �@g��@g|�@gK�@fȴ@fV@f@e�-@e�@eV@d�D@d9X@d�@c�m@c��@cC�@b~�@bM�@a�#@a��@ahs@aG�@a�@`Ĝ@`  @_��@_l�@^v�@^@]@]?}@\��@[�m@Z�H@ZM�@Z-@Z�@Z�@Z�@Y��@Y��@Yhs@YG�@Y�@X��@X��@XbN@X1'@Xb@W|�@W+@V��@V�R@Vff@U�-@U/@U�@T��@T�@T��@TI�@S�F@S��@SC�@R�@R^5@Q�@Qhs@Q7L@P��@P�9@P�u@P�u@P�@PQ�@P1'@Pb@O�w@N�@M�@M@M��@M�@Mp�@M`B@MO�@M/@L�@Lj@L(�@K��@K�F@K�@Ko@J��@JM�@J�@Ihs@H��@G��@G\)@G;d@F�y@F��@Fv�@FV@F5?@F$�@F{@F@F@E�@E�@E�-@E/@D�@DZ@D9X@D(�@C�m@C��@C"�@B=q@B�@AX@@A�@@  @?�;@?�w@?+@>��@>$�@=�@=�T@=��@=@=�-@=p�@=�@<�@<�/@<��@<�@<9X@;ƨ@;��@;t�@;@:�H@:M�@9�#@9�7@9x�@9hs@9G�@8��@8  @7�w@7l�@6�y@6V@6E�@5�T@4��@4�/@4�D@4(�@3�F@3�@3dZ@2�H@2n�@2M�@2J@1�@1�#@1��@1��@1��@1�@0�@0�@0�@0Q�@0b@0b@/�w@/��@/��@/�P@/\)@/+@.�y@.�@.�@.�R@.�+@-��@.@.@-�h@,j@,I�@,�D@+ƨ@+dZ@*��@*�\@*^5@*J@)�@)��@)X@)7L@)G�@)�@(��@(�@(bN@(1'@'��@'|�@';d@&�y@&��@&V@&E�@&E�@&{@%��@%�@$�@$�D@$9X@$1@#ƨ@#��@#"�@"��@"�!@"�!@"��@"M�@!��@!�#@!��@!�^@!��@!x�@!G�@!%@ �`@ ��@ ��@ A�@�@��@�w@�w@�P@+@�@�y@ȴ@�R@�R@�R@�R@��@��@�+@V@E�@E�@V@5?@@@�h@p�@O�@�/@��@��@j@�m@�
@t�@33@@��@��@�\@n�@�#@7L@%@��@��@��@�u@bN@A�@ �@  @�@��@�P@|�@+@��@�@ȴ@ȴ@ȴ@ȴ@ȴ@��@�+@ff@E�@@O�@�@V@V@V@��@�/@�@z�@I�@I�@9X@��@C�@o@�H@�!@��@n�@M�@�@�@�^@�7@hs@G�@7L@�@��@Ĝ@�@�@�u@�u@r�@A�@1'@�@�@�@�P@|�@�P@\)@K�@+@��@V@E�@$�@@�@��@��@��@��@�h@`B@/@��@�/@��@�@�D@Z@Z@Z@Z@9X@�@�
@��@S�@C�@33@33@"�@o@@@
��@
�\@
M�@
J@
J@	��@	�@	�#@	�^@	��@	��@	��@	��@	�7@	�7@	�7@	�7@	x�@	x�@	hs@	�@�@A�@1'@  @�;@�@|�@\)@;d@+@�@�y@�@�R@��@ff@5?@$�@{@@�@�-@?}@�@�/@��@�D@�D@z�@z�@z�@j@I�@�m@�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��+A��DA��PA��A��A��A��A��7A��+A���A���A���A��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A��PA��7A�bA���A���A�z�A�hsA�hsA�dZA�I�A�A�A�;dA�oA���A���A�{A���A�|�A�/A���A�$�A�G�A��mA���A��hA��A���A�ȴA�33A���A���A�+A��+A�G�A���A�"�A�?}A�dZA��A��yA��A�oA���A��mA��A�1'A�|�A�ƨA�9XA~�\A}�A{��Az�Az�RAy�Axn�Av��Au�FAt��At�DAs�PArAqt�Aq"�Ap��Ap�An�Am�Ak��AiC�Ag��AgG�Af��Af=qAeC�Ac��AadZA_�mA^=qA\��A\I�A[��AZ�jAZE�AY7LAX�RAXE�AWp�AV{AUl�AU%AT~�AS�AS%ARI�AQ��AP�APVAP1AO��AN��AM��AL��AK�AJ��AJ(�AIXAH��AG�wAE�mAD��AC�;AB�AA��AAdZAA/A@��A@�!A@E�A?O�A>�A=��A<n�A;hsA:Q�A9��A9%A8��A8jA8E�A8�A7�#A7%A6��A6A�A61'A5A4�9A4^5A3�FA2VA1�A0VA0bA0bA0JA/�
A/�A.1'A-�A,�HA,JA+�PA+A*��A*ffA)�A(E�A&�+A%��A%S�A#K�A ��A E�A�FA
=A�A`BA�A�uA(�A��A\)A�A��A1'A�PA?}AVA��A�`A�/A�jA�\AbNAA�A�Ax�A�A^5A��A  A�wA
�A
r�A
bA	A9XA�7A �Ax�A�RAZA�A�!AffA-A �AbA��@�O�@��@���@�V@�J@��#@�O�@�\)@���@�ƨ@�@�I�@���@��@�h@�hs@���@��@�j@�@�+@�@��/@�R@���@��y@�hs@�  @�33@���@؋D@��@ׅ@��y@�n�@�b@ҧ�@д9@�
=@�5?@ͩ�@��@�r�@ʰ!@�`B@�ƨ@�;d@�@Ƨ�@�J@�`B@��@ċD@�Z@�I�@�1'@�9X@�(�@�(�@�(�@��@öF@�t�@�33@�M�@��#@�/@���@��!@���@�`B@�7L@�9X@�E�@�&�@�\)@��@��\@��@�7L@��D@��P@�-@��@�@��R@�E�@���@��h@��7@��7@��@�7L@��@���@��u@�Z@��m@���@�dZ@���@��^@���@�;d@��+@��#@���@��F@�33@�ȴ@�~�@�^5@�E�@�{@�p�@��@���@��@��@�5?@�%@�Z@�Q�@��m@���@�J@�hs@��D@��m@��P@�33@�
=@�
=@��@�ȴ@�M�@��h@���@���@�ƨ@�"�@�v�@�J@��-@�?}@���@�1'@�1@���@���@���@��w@�K�@�;d@�"�@���@�~�@�V@�^5@�M�@���@�Z@�1'@�  @��;@�ƨ@���@�|�@�|�@�t�@�S�@�"�@�o@��@���@�ff@�7L@�(�@�|�@���@���@��\@�v�@�n�@�~�@�v�@�M�@��@�`B@��/@�z�@�bN@�I�@�9X@�(�@�1@��F@�C�@�+@�+@�
=@��y@��\@�=q@�@��T@���@��^@���@��h@�p�@�/@���@��`@��j@�j@�@�P@�P@~�@~ff@}��@}?}@|��@|j@{��@{t�@{S�@{dZ@{t�@{�@{S�@z�!@z~�@y�7@x�@x  @w�@v��@vV@v5?@v{@u�T@u@uO�@t��@t�@tj@t9X@t�@s�m@sƨ@s��@s33@so@r�@r��@r^5@q��@q�@q�#@qhs@p��@p�9@p�9@p��@pr�@o�@pA�@p�`@p��@n�y@nff@nȴ@n�R@n�+@n5?@n�y@o�@o|�@o��@o|�@o�@n��@n��@mp�@l�/@j~�@i��@j�\@k@j�H@jM�@i�@i�^@i�7@iG�@h�`@hĜ@h�@hbN@hA�@h1'@h �@g��@g|�@gK�@fȴ@fV@f@e�-@e�@eV@d�D@d9X@d�@c�m@c��@cC�@b~�@bM�@a�#@a��@ahs@aG�@a�@`Ĝ@`  @_��@_l�@^v�@^@]@]?}@\��@[�m@Z�H@ZM�@Z-@Z�@Z�@Z�@Y��@Y��@Yhs@YG�@Y�@X��@X��@XbN@X1'@Xb@W|�@W+@V��@V�R@Vff@U�-@U/@U�@T��@T�@T��@TI�@S�F@S��@SC�@R�@R^5@Q�@Qhs@Q7L@P��@P�9@P�u@P�u@P�@PQ�@P1'@Pb@O�w@N�@M�@M@M��@M�@Mp�@M`B@MO�@M/@L�@Lj@L(�@K��@K�F@K�@Ko@J��@JM�@J�@Ihs@H��@G��@G\)@G;d@F�y@F��@Fv�@FV@F5?@F$�@F{@F@F@E�@E�@E�-@E/@D�@DZ@D9X@D(�@C�m@C��@C"�@B=q@B�@AX@@A�@@  @?�;@?�w@?+@>��@>$�@=�@=�T@=��@=@=�-@=p�@=�@<�@<�/@<��@<�@<9X@;ƨ@;��@;t�@;@:�H@:M�@9�#@9�7@9x�@9hs@9G�@8��@8  @7�w@7l�@6�y@6V@6E�@5�T@4��@4�/@4�D@4(�@3�F@3�@3dZ@2�H@2n�@2M�@2J@1�@1�#@1��@1��@1��@1�@0�@0�@0�@0Q�@0b@0b@/�w@/��@/��@/�P@/\)@/+@.�y@.�@.�@.�R@.�+@-��@.@.@-�h@,j@,I�@,�D@+ƨ@+dZ@*��@*�\@*^5@*J@)�@)��@)X@)7L@)G�@)�@(��@(�@(bN@(1'@'��@'|�@';d@&�y@&��@&V@&E�@&E�@&{@%��@%�@$�@$�D@$9X@$1@#ƨ@#��@#"�@"��@"�!@"�!@"��@"M�@!��@!�#@!��@!�^@!��@!x�@!G�@!%@ �`@ ��@ ��@ A�@�@��@�w@�w@�P@+@�@�y@ȴ@�R@�R@�R@�R@��@��@�+@V@E�@E�@V@5?@@@�h@p�@O�@�/@��@��@j@�m@�
@t�@33@@��@��@�\@n�@�#@7L@%@��@��@��@�u@bN@A�@ �@  @�@��@�P@|�@+@��@�@ȴ@ȴ@ȴ@ȴ@ȴ@��@�+@ff@E�@@O�@�@V@V@V@��@�/@�@z�@I�@I�@9X@��@C�@o@�H@�!@��@n�@M�@�@�@�^@�7@hs@G�@7L@�@��@Ĝ@�@�@�u@�u@r�@A�@1'@�@�@�@�P@|�@�P@\)@K�@+@��@V@E�@$�@@�@��@��@��@��@�h@`B@/@��@�/@��@�@�D@Z@Z@Z@Z@9X@�@�
@��@S�@C�@33@33@"�@o@@@
��@
�\@
M�@
J@
J@	��@	�@	�#@	�^@	��@	��@	��@	��@	�7@	�7@	�7@	�7@	x�@	x�@	hs@	�@�@A�@1'@  @�;@�@|�@\)@;d@+@�@�y@�@�R@��@ff@5?@$�@{@@�@�-@?}@�@�/@��@�D@�D@z�@z�@z�@j@I�@�m@�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BJB+B1B1B+B%B+B+B%BBB��B��B�mB��B�FB}�B<jB�B�)B��B�?B��BdZBT�BF�B;dB �B#�B�B�BVBB
��B
�B
�ZB
�HB
�HB
�)B
��B
ĜB
�3B
��B
��B
��B
��B
�VB
�+B
~�B
v�B
n�B
l�B
k�B
k�B
aHB
VB
P�B
N�B
K�B
D�B
:^B
9XB
8RB
49B
/B
"�B
�B
DB	��B	��B	��B	��B	�B	�TB	�)B	ȴB	B	�^B	�-B	�9B	�'B	��B	��B	��B	��B	��B	�uB	�DB	�DB	�7B	�%B	�B	|�B	w�B	v�B	r�B	o�B	n�B	k�B	ffB	^5B	XB	Q�B	M�B	K�B	E�B	B�B	8RB	-B	&�B	$�B	�B	�B	�B	�B	�B	uB	VB	+B��B��B��B�B�B�B�B�B�B�B�B�yB�ZB�TB�NB�NB�TB�BB�5B�#B��B��B��B��B��B��B��BǮBÖBÖB��B�}B�wB�jB�qB�XB�FB�B��B��B��B�hB�%B�VB�JB�7B�B~�Bt�BiyBx�Bx�Bw�Bv�Bv�Bs�Br�Bs�Bs�Bt�Bs�Bs�Br�Bq�Bp�Bo�Bm�BiyBgmBaHBT�BK�BO�BVBYBW
BP�BP�BP�BL�BO�BO�BN�BM�BH�BM�BN�BN�BL�BF�B8RB?}BG�BG�BE�BC�B@�B9XB5?B;dB6FB6FB7LB9XB<jB;dB9XB5?B.B49B6FB5?B1'B,B&�B-B-B-B0!B/B1'B33B49B2-B1'B+B.B-B2-B7LB;dB:^B:^B6FB8RB9XB>wB@�B?}B?}B>wB@�BA�BB�BB�BB�BC�BB�BB�BB�BA�BA�BA�B@�B@�BD�BE�BE�BG�BL�BN�BO�BK�BJ�BP�BN�BQ�BS�BS�BT�BVBT�BR�BQ�BZB^5B`BBbNBgmBgmBgmBgmBffBffBiyBiyBiyBiyBjBk�Bk�Bk�Bn�Bt�B}�B�B�B�B�B�B�B�%B�%B�B�B�B�B�B�B�7B�DB�bB��B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�9B�?B�RB�qB��BBÖBĜBÖBĜB��B��B��B��B��B�B�
B�B�HB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	+B	1B	1B	+B	+B	+B	
=B	VB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	'�B	)�B	+B	,B	-B	-B	.B	/B	2-B	33B	33B	33B	49B	9XB	<jB	=qB	@�B	B�B	D�B	G�B	H�B	H�B	I�B	M�B	O�B	O�B	O�B	N�B	O�B	T�B	S�B	VB	ZB	\)B	^5B	aHB	e`B	gmB	jB	iyB	iyB	k�B	m�B	p�B	r�B	s�B	t�B	v�B	v�B	v�B	y�B	z�B	{�B	|�B	|�B	~�B	� B	�B	�B	�=B	�JB	�JB	�PB	�VB	�hB	�uB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�-B	�'B	�'B	�'B	�?B	�dB	�qB	�wB	�wB	�wB	�}B	�}B	��B	��B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�;B	�;B	�BB	�;B	�BB	�ZB	�mB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
+B
%B
%B

=B
DB
DB
DB
JB
JB
PB
PB
PB
VB
\B
\B
\B
\B
hB
hB
oB
uB
uB
uB
oB
uB
uB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"�B
#�B
$�B
$�B
#�B
$�B
%�B
&�B
'�B
'�B
(�B
)�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
-B
-B
.B
/B
/B
/B
/B
0!B
1'B
33B
33B
33B
49B
49B
49B
49B
49B
49B
33B
49B
49B
33B
33B
49B
49B
2-B
33B
33B
49B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
A�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
L�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
VB
XB
XB
XB
YB
YB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
iyB
iyB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B9B<B1B�B�BzB?B+BEBYB-BAB��B��B�B�B��B�3BDB��B�jB�<B��B��BlqBX�BI7B>(B#�B%,B�B+BHB�B
��B
��B
�B
� B
�B
�IB
�.B
��B
��B
�6B
��B
��B
�EB
�B
��B
� B
x�B
p!B
m]B
l"B
l�B
c B
W�B
R:B
O�B
L�B
E�B
<B
:B
8�B
4�B
/�B
$�B
5B
�B
 �B	�DB	��B	�ZB	�B	��B	�5B	˒B	āB	�jB	��B	��B	�B	�B	��B	��B	�jB	�qB	��B	��B	�0B	��B	��B	��B	~B	x�B	w�B	s�B	pUB	oB	lWB	gmB	_�B	Y�B	SuB	N�B	L�B	F�B	CaB	:*B	/5B	(sB	%�B	 �B	B	�B	B	
B	�B	(B	�B	 �B��B�XB�B��B�B�OB�B��B��B��B��B�`B��B��B�B�&B�|B��B�CBյB�\BοB�NB�B�B�PB��B��B�gBªB��B�.B�"B��B�*B�fB�CB��B�B��B�,B�B�(B�6B�XB��B��Bw�BlWByXByrBxlBwLBwLBt�Bs�BtBtBt�BtBtBr�BrBp�Bo�Bn/BjKBh>Bb�BW�BO(BRoBW
BY�BW�BRTBR BR BN�BP�BP�BO�BN�BJ#BN<BO(BO(BMPBHB;�B@�BHBG�BE�BC�BA;B:�B6�B<B7�B7�B8RB9�B<�B;�B:B6�B0!B5%B6�B5�B2GB-�B(�B.B./B.B0�B0!B2B3�B4�B2�B1�B,�B/5B.}B3MB7�B<B:�B;B7�B9XB:^B>�B@�B?�B@ B>�B@�BA�BB�BB�BB�BC�BB�BB�BB�BA�BA�BA�B@�BA;BEBFYBF�BH�BMjBOBBPHBL�BL0BQ�BO�BRTBTFBT�BU�BV�BU�BT,BSuBZ�B^�B`�Bb�Bg�Bg�Bg�Bg�Bf�Bf�Bi�Bi�Bi�Bi�Bj�Bk�Bl"Bl�Bo�Bu�B~wB��B��B��B��B�mB�mB�YB�tB�gB��B�mB��B��B��B��B�B��B��B��B�MB�B�=B�kB�5B�B�:B��B��B�B�&B�:B�TB�$B��B��B��B��B��B��B��B��B��B��B��BðBĶB��B��B��B�B�B�B�B�B�YB��B�B�B�B�B�B��B��B��B��B��B��B�B�*B�*B�xB��B��B�jB�jB	 B	-B	9B	EB	1B	KB	zB	zB	�B	
�B	�B	�B	�B	�B	�B	�B	�B	B	�B	�B	B	B	!B	%,B	(>B	*0B	+B	,"B	-)B	-)B	.IB	/OB	2|B	3MB	3hB	3�B	4�B	9rB	<�B	=�B	@�B	B�B	D�B	G�B	H�B	H�B	I�B	M�B	O�B	O�B	O�B	OB	P.B	U2B	TaB	VmB	ZQB	\]B	^�B	a�B	ezB	g�B	j�B	i�B	i�B	k�B	m�B	p�B	r�B	s�B	t�B	v�B	v�B	v�B	y�B	z�B	|B	}"B	}"B	B	�B	�;B	�SB	�XB	�dB	�dB	�jB	��B	�NB	�[B	��B	�B	��B	�yB	��B	��B	��B	��B	��B	��B	��B	�CB	�[B	�MB	�aB	��B	��B	��B	�tB	�0B	�VB	��B	��B	��B	��B	��B	��B	��B	ĶB	żB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�B	� B	�&B	�,B	�B	�B	�9B	�?B	�YB	�7B	�kB	�=B	�=B	�=B	�=B	�WB	یB	�WB	�]B	�xB	�~B	�VB	ߊB	�vB	߾B	��B	�B	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	��B	�(B	�B	�B	�B	�"B	�<B	�BB
;B
'B
'B
'B
B
-B
-B
[B
3B
3B
3B
9B
9B
SB
YB
EB
fB
zB
�B
�B

XB
xB
xB
^B
~B
dB
jB
PB
jB
VB
\B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
"B
"�B
#�B
$�B
$�B
$&B
%B
%�B
'8B
($B
(>B
)*B
*0B
)DB
*B
*B
*0B
+6B
+6B
,"B
,WB
,=B
-)B
-)B
./B
.IB
./B
.B
.IB
-CB
-CB
.B
/5B
/5B
/5B
/5B
0;B
1AB
33B
3MB
3MB
4TB
4TB
49B
49B
4nB
4TB
3hB
4TB
4TB
3hB
3�B
4TB
4TG�O�B
3�B
3�B
4TB
5ZB
5ZB
5tB
6zB
6`B
7fB
8RB
8lB
8lB
8lB
8lB
8�B
8�B
8�B
8lB
8�B
9rB
:xB
;dB
;dB
;�B
;�B
;�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
@�B
A�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
NB
NB
MB
N�B
N�B
OB
O�B
O�B
Q B
Q B
Q B
P.B
QB
R B
R B
R B
R�B
SB
S&B
S&B
SB
SB
SB
SB
SB
TB
SB
T,B
T�B
UB
T�B
T�B
T�B
UB
UB
UB
UB
U2B
UMB
VB
VB
W$B
W
B
W
B
W$B
W$B
W?B
W?B
W?B
W$B
W$B
W$B
V9B
X+B
X+B
X+B
Y1B
YKB
XEB
Y1B
Y1B
Y1B
ZQB
ZQB
Z7B
ZB
[WB
[=B
[=B
\CB
]/B
]/B
]/B
]IB
]IB
]dB
]IB
]IB
^5B
^OB
^OB
^5B
^OB
^OB
^OB
^jB
^OB
_;B
`vB
`\B
`BB
`vB
`\B
`BB
`BB
`\B
`\B
`\B
`\B
`\B
abB
abB
abB
abB
bhB
bhB
bNB
bhB
abB
abB
bhB
cnB
cTB
dZB
dZB
dZB
dtB
dZB
dZB
cnB
c�B
d�B
dtB
e`B
e`B
e`B
e`B
e�B
e`B
e`B
e`B
e`B
e`B
e`B
ezB
ezB
ezB
e`B
ezB
ezB
e�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
iyB
iyB
i�B
j�B
i�B
i�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.03(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712250036032017122500360320171225003603201806221235102018062212351020180622123510201804050431272018040504312720180405043127  JA  ARFMdecpA19c                                                                20171221063512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171220213514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171220213516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171220213517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171220213517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171220213517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171220213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171220213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171220213518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171220213518                      G�O�G�O�G�O�                JA  ARUP                                                                        20171220215552                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171221154001  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20171222000000  CF  PSAL_ADJUSTED_QCD�@ D�@ G�O�                JM  ARCAJMQC2.0                                                                 20171224153603  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171224153603  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193127  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033510  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-11-22T10:01:11Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20211122100111  20211122100111  4903175 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               qA   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @٤�ww��1   @٤�F4@2�E�����dK�
=p�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         qA   B   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB@G�BG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D+zD+~D+�D,~D,�D-w�D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Duw�Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�{�D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�{�DԿ
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
D��=D��
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
D��D��
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
D�;�D�
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
D��D�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A׃A׋DAׇ+AדuAדuA׏\A׏\A׋DA׏\A׏\A׏\A׏\AבhA׏\A׉7AבhA׉7A�z�A�x�Aׇ+AׁAׁA׍PAׇ+A׏\A׉7A�|�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�x�A�x�A�x�A�v�A�v�A�v�A�t�A�p�A�n�A�n�A�n�A�l�A�1A�l�A�ƨA��A���A���A�A���A�1'A���A�XA�{A�Q�A�%A�1'A�jA��A���A���A���A��A��A�ȴA�hsA�M�A��PA��A���A���A� �A��mA�`BA��A� �A��;A��`A���A��DA�l�A���A���A���A�~�A�ȴA�1'A�33A��9A��TA���A�l�A�-A�bA�-A�-A�`BA�A���A�{A���A�^5A��
A�;dA�VA��HA�M�A��-A�\)A��A�t�A��TA�O�AG�A}��A{7LAx�At�`Aq%Ao
=An��Akp�Ag�PAe�Ad(�Aa��A^E�A[�#AZ�AY��AXbAS��AQAP��AO;dAL��AK&�AJ�AIl�AHA�AGG�AD��AC
=AB1'AAK�AA%A@ȴA?�A>��A>��A=��A<r�A:Q�A9"�A7;dA3O�A2�/A1��A0 �A-�FA-G�A,��A+�;A(�9A%\)A#C�A"��A"jA!��A!hsA!C�A!&�A VA��A��A=qA��A;dA^5A��A�PA�HA��AE�A%AhsA��AM�A��AĜA�mA��A��AffA{A�A�AXA��A��AA�A	�7A	&�A��A�A�A+A��A-AS�A�AjAZA�DA�A Q�@�C�@�/@��@��F@�|�@��R@�n�@��^@�9X@�{@��j@���@�A�@�C�@��@�;d@�9X@�^5@�?}@��@�;d@���@�b@�X@ߕ�@ߝ�@�"�@޸R@ݩ�@��@�Z@ۥ�@�l�@���@�V@ّh@��@؛�@�  @֧�@�-@�G�@�I�@��;@�t�@�@Л�@��@�  @���@���@�5?@�-@͡�@̃@��H@�v�@�S�@���@�r�@�|�@�=q@�?}@���@���@���@��/@��u@�l�@�dZ@�K�@��y@�\)@�5?@���@���@�Ĝ@��u@�j@���@�l�@�G�@��`@���@�  @�Q�@��m@�33@���@���@�33@�|�@��P@�K�@�S�@���@�ȴ@��+@�?}@���@� �@�|�@��@��@��H@���@�ff@���@���@��h@�p�@���@�Q�@��@��;@�  @�33@�ff@�dZ@��;@���@�=q@���@���@���@��m@��;@��;@��
@���@���@���@���@�
=@��P@�1'@�I�@�I�@�Q�@�Z@���@���@�z�@���@�~�@��T@��@�p�@��D@��w@��w@�b@�1@��;@�1@�1@��@�dZ@��@��R@���@���@��@�J@�7L@���@�I�@�33@���@��#@�/@�%@��@���@��@�z�@��@��P@�l�@�K�@�;d@�33@�33@�33@���@�E�@��-@�x�@�X@�7L@���@�r�@�1'@��
@�|�@�"�@���@��R@��R@���@��\@�V@�{@��@��-@��@���@��j@���@�r�@� �@�9X@�b@���@���@��P@��@��@�t�@��@��\@�M�@�-@���@�@���@���@���@�p�@���@��u@�r�@�1'@�ƨ@���@��@�C�@�
=@��@���@�^5@�5?@�5?@�$�@��^@�p�@�hs@�&�@��@��@�r�@�I�@�A�@���@�ƨ@��F@��@��@�dZ@�@���@�ff@�E�@�@���@��7@��@�hs@�&�@�Ĝ@���@�9X@�  @��w@���@�dZ@�C�@�;d@�33@�o@���@�^5@�E�@�@��7@�/@���@��@�bN@�I�@�1'@��@�\)@��@���@�v�@�M�@�$�@���@�x�@�X@��@�Ĝ@�r�@� �@��@\)@
=@}@|��@|Z@{dZ@z^5@z=q@y��@y��@y�@x�u@xA�@w��@w|�@wK�@vȴ@vE�@u�h@tj@s�m@s��@s��@sC�@s@r�H@r��@r�@q��@q�@p��@o�@o|�@oK�@n$�@m��@mp�@l��@l�D@l9X@l1@k�
@kt�@k33@k@j��@j�@i��@iG�@h�`@h�@hA�@hb@g�@g�w@gl�@g\)@gK�@f�R@fV@e�@e�h@eV@d��@d�j@d��@d1@c��@ct�@cS�@b�H@a��@aG�@`��@`��@`r�@`b@_�w@_\)@_�@^�@]�-@\�j@\�D@\Z@[ƨ@[�@[�@[��@[33@Z�@Z��@Z~�@Z^5@Y��@Yhs@X��@X�@Xr�@XbN@XQ�@Xb@W|�@V��@V�@Vv�@VE�@U�T@U@U��@UO�@T��@T�D@TI�@T(�@S�@SC�@R�@R�!@R��@Rn�@RJ@Q��@Q�7@Qx�@QG�@P��@PbN@Pb@O�@O;d@N�@Nv�@NV@NV@N{@M��@M�-@M�h@M�@L��@L�j@L�D@Lz�@LI�@L9X@L(�@K�
@Kt�@Ko@J�@J�H@J�H@J�!@JM�@JJ@I��@I��@I�@I�^@I�7@Ix�@IX@I7L@H��@H�u@H �@G�;@G��@G��@F�@Fv�@F5?@E�T@Ep�@E?}@E?}@D��@D�@D�/@D�@D9X@C��@C�
@Cƨ@C��@C��@CdZ@B�@B��@Bn�@B=q@A�@A��@AX@A&�@@�u@@�@@r�@@bN@@A�@?��@?;d@>�y@>��@>V@>E�@=�@=/@<�@<�j@<�@;�F@;��@;�@;t�@;C�@;33@;"�@;o@:��@:-@9��@9��@9x�@97L@97L@9�@8Ĝ@8��@8A�@8  @7�;@7�w@7�P@7�P@7|�@7|�@7;d@7
=@6�@6ff@5@5�h@5`B@4��@4I�@41@3�m@3��@3�
@3t�@2�@2��@2J@1�7@1X@17L@0��@0Ĝ@0�@/�;@/�P@.��@.��@.E�@-@-�@,��@,z�@+��@+C�@+"�@*��@*�!@*n�@*-@*J@)�@)��@)&�@)%@(��@( �@'��@'\)@';d@'+@&�R@&E�@%�T@%�-@%��@%�@%V@$��@$��@$9X@#��@#33@#o@#@"��@!��@!��@!��@!��@!�7@!x�@!G�@!&�@!&�@!�@ �`@ �u@ bN@�;@\)@��@ȴ@�R@��@��@ff@$�@��@��@�h@�@O�@�@��@�@��@j@I�@��@ƨ@��@��@33@o@o@@�H@��@��@n�@J@�@�#@�^@�7@&�@�`@��@�9@bN@A�@1'@ �@ �@��@�w@��@\)@�@
=@��@�R@V@5?@��@p�@/@V@�@��@�D@�D@�D@j@(�@�m@ƨ@�@t�@�@t�@S�@o@�@��@�\@n�@^5@-@��@�@�@��@�^@�7@x�@G�@&�@%@��@�@A�@b@�@�w@|�@;d@+@+@�@��@v�@$�@�@�h@/@�@V@��@��@�@�/@�/@��@�@j@(�@��@�m@�
@��@t�@33@@
�H@
��@
��@
n�@
=q@	��@	��@	�7@	hs@	G�@	&�@	�@��@��@bN@1'@b@��@�w@�@�@�P@l�@K�@+@��@�R@v�@v�@ff@$�@{1111111111114411111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A׃A׋DAׇ+AדuAדuA׏\A׏\A׋DA׏\A׏\A׏\A׏\AבhA׏\A׉7AבhA׉7A�z�A�x�Aׇ+AׁAׁA׍PAׇ+A׏\A׉7A�|�A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�x�A�x�A�x�A�v�A�v�A�v�A�t�A�p�A�n�A�n�A�n�A�l�A�1A�l�A�ƨA��A���A���A�A���A�1'A���A�XA�{A�Q�A�%A�1'A�jA��A���A���A���A��A��A�ȴA�hsA�M�A��PA��A���A���A� �A��mA�`BA��A� �A��;A��`A���A��DA�l�A���A���A���A�~�A�ȴA�1'A�33A��9A��TA���A�l�A�-A�bA�-A�-A�`BA�A���A�{A���A�^5A��
A�;dA�VA��HA�M�A��-A�\)A��A�t�A��TA�O�AG�A}��A{7LAx�At�`Aq%Ao
=An��Akp�Ag�PAe�Ad(�Aa��A^E�A[�#AZ�AY��AXbAS��AQAP��AO;dAL��AK&�AJ�AIl�AHA�AGG�AD��AC
=AB1'AAK�AA%A@ȴA?�A>��A>��A=��A<r�A:Q�A9"�A7;dA3O�A2�/A1��A0 �A-�FA-G�A,��A+�;A(�9A%\)A#C�A"��A"jA!��A!hsA!C�A!&�A VA��A��A=qA��A;dA^5A��A�PA�HA��AE�A%AhsA��AM�A��AĜA�mA��A��AffA{A�A�AXA��A��AA�A	�7A	&�A��A�A�A+A��A-AS�A�AjAZA�DA�A Q�@�C�@�/@��@��F@�|�@��R@�n�@��^@�9X@�{@��j@���@�A�@�C�@��@�;d@�9X@�^5@�?}@��@�;d@���@�b@�X@ߕ�@ߝ�@�"�@޸R@ݩ�@��@�Z@ۥ�@�l�@���@�V@ّh@��@؛�@�  @֧�@�-@�G�@�I�@��;@�t�@�@Л�@��@�  @���@���@�5?@�-@͡�@̃@��H@�v�@�S�@���@�r�@�|�@�=q@�?}@���@���@���@��/@��u@�l�@�dZ@�K�@��y@�\)@�5?@���@���@�Ĝ@��u@�j@���@�l�@�G�@��`@���@�  @�Q�@��m@�33@���@���@�33@�|�@��P@�K�@�S�@���@�ȴ@��+@�?}@���@� �@�|�@��@��@��H@���@�ff@���@���@��h@�p�@���@�Q�@��@��;@�  @�33@�ff@�dZ@��;@���@�=q@���@���@���@��m@��;@��;@��
@���@���@���@���@�
=@��P@�1'@�I�@�I�@�Q�@�Z@���@���@�z�@���@�~�@��T@��@�p�@��D@��w@��w@�b@�1@��;@�1@�1@��@�dZ@��@��R@���@���@��@�J@�7L@���@�I�@�33@���@��#@�/@�%@��@���@��@�z�@��@��P@�l�@�K�@�;d@�33@�33@�33@���@�E�@��-@�x�@�X@�7L@���@�r�@�1'@��
@�|�@�"�@���@��R@��R@���@��\@�V@�{@��@��-@��@���@��j@���@�r�@� �@�9X@�b@���@���@��P@��@��@�t�@��@��\@�M�@�-@���@�@���@���@���@�p�@���@��u@�r�@�1'@�ƨ@���@��@�C�@�
=@��@���@�^5@�5?@�5?@�$�@��^@�p�@�hs@�&�@��@��@�r�@�I�@�A�@���@�ƨ@��F@��@��@�dZ@�@���@�ff@�E�@�@���@��7@��@�hs@�&�@�Ĝ@���@�9X@�  @��w@���@�dZ@�C�@�;d@�33@�o@���@�^5@�E�@�@��7@�/@���@��@�bN@�I�@�1'@��@�\)@��@���@�v�@�M�@�$�@���@�x�@�X@��@�Ĝ@�r�@� �@��@\)@
=@}@|��@|Z@{dZ@z^5@z=q@y��@y��@y�@x�u@xA�@w��@w|�@wK�@vȴ@vE�@u�h@tj@s�m@s��@s��@sC�@s@r�H@r��@r�@q��@q�@p��@o�@o|�@oK�@n$�@m��@mp�@l��@l�D@l9X@l1@k�
@kt�@k33@k@j��@j�@i��@iG�@h�`@h�@hA�@hb@g�@g�w@gl�@g\)@gK�@f�R@fV@e�@e�h@eV@d��@d�j@d��@d1@c��@ct�@cS�@b�H@a��@aG�@`��@`��@`r�@`b@_�w@_\)@_�@^�@]�-@\�j@\�D@\Z@[ƨ@[�@[�@[��@[33@Z�@Z��@Z~�@Z^5@Y��@Yhs@X��@X�@Xr�@XbN@XQ�@Xb@W|�@V��@V�@Vv�@VE�@U�T@U@U��@UO�@T��@T�D@TI�@T(�@S�@SC�@R�@R�!@R��@Rn�@RJ@Q��@Q�7@Qx�@QG�@P��@PbN@Pb@O�@O;d@N�@Nv�@NV@NV@N{@M��@M�-@M�h@M�@L��@L�j@L�D@Lz�@LI�@L9X@L(�@K�
@Kt�@Ko@J�@J�H@J�H@J�!@JM�@JJ@I��@I��@I�@I�^@I�7@Ix�@IX@I7L@H��@H�u@H �@G�;@G��@G��@F�@Fv�@F5?@E�T@Ep�@E?}@E?}@D��@D�@D�/@D�@D9X@C��@C�
@Cƨ@C��@C��@CdZ@B�@B��@Bn�@B=q@A�@A��@AX@A&�@@�u@@�@@r�@@bN@@A�@?��@?;d@>�y@>��@>V@>E�@=�@=/@<�@<�j@<�@;�F@;��@;�@;t�@;C�@;33@;"�@;o@:��@:-@9��@9��@9x�@97L@97L@9�@8Ĝ@8��@8A�@8  @7�;@7�w@7�P@7�P@7|�@7|�@7;d@7
=@6�@6ff@5@5�h@5`B@4��@4I�@41@3�m@3��@3�
@3t�@2�@2��@2J@1�7@1X@17L@0��@0Ĝ@0�@/�;@/�P@.��@.��@.E�@-@-�@,��@,z�@+��@+C�@+"�@*��@*�!@*n�@*-@*J@)�@)��@)&�@)%@(��@( �@'��@'\)@';d@'+@&�R@&E�@%�T@%�-@%��@%�@%V@$��@$��@$9X@#��@#33@#o@#@"��@!��@!��@!��@!��@!�7@!x�@!G�@!&�@!&�@!�@ �`@ �u@ bN@�;@\)@��@ȴ@�R@��@��@ff@$�@��@��@�h@�@O�@�@��@�@��@j@I�@��@ƨ@��@��@33@o@o@@�H@��@��@n�@J@�@�#@�^@�7@&�@�`@��@�9@bN@A�@1'@ �@ �@��@�w@��@\)@�@
=@��@�R@V@5?@��@p�@/@V@�@��@�D@�D@�D@j@(�@�m@ƨ@�@t�@�@t�@S�@o@�@��@�\@n�@^5@-@��@�@�@��@�^@�7@x�@G�@&�@%@��@�@A�@b@�@�w@|�@;d@+@+@�@��@v�@$�@�@�h@/@�@V@��@��@�@�/@�/@��@�@j@(�@��@�m@�
@��@t�@33@@
�H@
��@
��@
n�@
=q@	��@	��@	�7@	hs@	G�@	&�@	�@��@��@bN@1'@b@��@�w@�@�@�P@l�@K�@+@��@�R@v�@v�@ff@$�@{1111111111114411111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�1A��A�$�A�7LA�5?A�1A���A���A���A�A��A��A���A���A��A��mA��TA��A���A��yA�7LA�%A��A��HA���A�A��/A���A��A��A��A���A��A��A��`A��mA��A��yA��mA��HA��TA��A�A�l�A�-A��A�v�A���A�ȴA�ĜA��A�A�-A�7LA�A��TA�ȴA�A��A�
=A�&�A�t�A�t�A��HA�33A䛦A���A��yA���A�;dA�z�A�9A�^A�ĜA��#A��#A���A�ĜA���A�A噚A�~�A�r�A�M�A�E�A�VA�Q�A�G�A�C�A�33A� �A�VA�  A��A���A�A䕁A�r�A�p�A�v�A�hsA�Q�A�S�A�M�A�=qA�A�A�;dA�-A�VA��A��#A���A�ȴA���A�9A�A��A�PA�x�A�VA�E�A�A�A�5?A�oA�%A���A��A��A�A�^A�9A��A�uA�z�A�x�A�p�A�bNA�S�A�S�A�O�A�G�A�?}A�5?A�&�A�+A�(�A�(�A�(�A�$�A��A� �A��A��A�bA�A���A��A��;A��A��HA��A���A��#A���A�ĜA�A�A�RA�A�ĜA�A�ƨA���A�ƨA�wA�ĜA���A���A���A���A���A���A���A�ƨA�jA���A���A���A��HA��TA��#A��A��
A��#A��mA��mA��mA��mA��mA��TA��#A���A���A�RA���A�^A�-A�A�-A�-A�-A�!A�RA�^A�A���A��A��A��;A��HA��HA��mA��A��A��A��A��yA���A���A���A���A���A��A��yA��A���A���A���A���A���A���A��A���A���A���A�%A�A�A�A�%A�bA�oA��A� �A�$�A�+A�-A�1'A�7LA�33A�7LA�=qA�=qA�=qA�E�A�O�A�Q�A�O�A�M�A�Q�A�S�A�M�A�I�A�K�A�S�A�M�A�Q�A�^5A�\)A�ZA�`BA�hsA�t�A�|�A�A�A�7A�uA╁A❲A�A�-A�9A�RA�RA�^A�ĜA�ƨA�ȴA���A���A���A���A���A��
A���A���A�ƨA�ȴA���A��#A��#A��;A��HA��TA��HA��;A��A��A��A��A��A��A��A��A���A���A�A�A�A�  A�%A�
=A�bA�oA�oA��A� �A��A� �A�$�A�$�A�+A�33A�5?A�5?A�7LA�7LA�9XA�=qA�=qA�E�A�O�A�S�A�XA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�`BA�`BA�\)A�^5A�`BA�ffA�jA�l�A�p�A�t�A�v�A�v�A�t�A�v�A�A�7A�\A�\A�PA�PA�PA�DA�7A�DA�DA�DA�PA�\A�\A�\A�PA�PA�uA㕁A㗍A㗍A㗍A㗍A㕁A�uA�uA㙚A㟾A��A��A��A��A��A��A��A�!A�-A�RA�jA�wA���A���A���A���A���A�wA�ĜA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A��A��
A���A��
A��A��
A��
A��#A��#A��#A��#A��A��#A��/A��#A��#A��#A��A��#A��#A��/A��/A��/A��;A��HA��HA��HA��HA��HA��HA��HA��TA��`A��`A��`A��`A��`A��TA��HA��TA��TA��HA��`A��TA��`A��`A��mA��mA��mA��`A��`A��mA��yA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A���A���A�  A���A���A���A���A���A�  A�  A�  A�  A���A�  A�A�  A�  A���A�  A�  A�  A�A�A�A�A�A�A�  A�  A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�%A�%A�A�A�A�A�%A�%A�%A�%A�%A�%A�%A�%A�1A�1A�1A�%A�1A�1A�1A�1A�1A�1A�1A�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�JA�JA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�bA�VA�VA�VA�VA�bA�bA�bA�bA�bA�VA�VA�VA�VA�VA�JA�JA�JA�JA�JA�JA�JA�JA�JA�JA�
=A�
=A�JA�VA�VA�VA�bA�bA�bA�bA�bA�VA�VA�bA�bA�bA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�bA�bA�bA�VA�VA�JA�VA�VA�VA�JA�JA�JA�JA�JA�
=A�JA�JA�JA�JA�JA�JA�JA�JA�
=A�
=A�JA�JA�JA�VA�VA�VA�JA�VA�JA�VA�VA�VA�VA�VA�VA�VA�JA�VA�JA�JA�JA�VA�JA�
=A�VA�VA�VA�VA�VA�JA�JA�VA�JA�VA�bA�bA�VA�VA�VA�VA�bA�bA�bA�bA�bA�bA�oA�oA�oA�oA��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A�"�A� �A� �A�"�A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A�(�A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A�&�A�&�A�(�A�(�A�(�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�(�A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�+A�+A�+A�+A�+A�-A�-A�+A�+A�+A�-A�-A�-A�-A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�/4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444A��A�1A��A�$�A�7LA�5?A�1A���A���A���A�A��A��A���A���A��A��mA��TA��A���A��yA�7LA�%A��A��HA���A�A��/A���A��A��A��A���A��A��A��`A��mA��A��yA��mA��HA��TA��A�A�l�A�-A��A�v�A���A�ȴA�ĜA��A�A�-A�7LA�A��TA�ȴA�A��A�
=A�&�A�t�A�t�A��HA�33A䛦A���A��yA���A�;dA�z�A�9A�^A�ĜA��#A��#A���A�ĜA���A�A噚A�~�A�r�A�M�A�E�A�VA�Q�A�G�A�C�A�33A� �A�VA�  A��A���A�A䕁A�r�A�p�A�v�A�hsA�Q�A�S�A�M�A�=qA�A�A�;dA�-A�VA��A��#A���A�ȴA���A�9A�A��A�PA�x�A�VA�E�A�A�A�5?A�oA�%A���A��A��A�A�^A�9A��A�uA�z�A�x�A�p�A�bNA�S�A�S�A�O�A�G�A�?}A�5?A�&�A�+A�(�A�(�A�(�A�$�A��A� �A��A��A�bA�A���A��A��;A��A��HA��A���A��#A���A�ĜA�A�A�RA�A�ĜA�A�ƨA���A�ƨA�wA�ĜA���A���A���A���A���A���A���A�ƨA�jA���A���A���A��HA��TA��#A��A��
A��#A��mA��mA��mA��mA��mA��TA��#A���A���A�RA���A�^A�-A�A�-A�-A�-A�!A�RA�^A�A���A��A��A��;A��HA��HA��mA��A��A��A��A��yA���A���A���A���A���A��A��yA��A���A���A���A���A���A���A��A���A���A���A�%A�A�A�A�%A�bA�oA��A� �A�$�A�+A�-A�1'A�7LA�33A�7LA�=qA�=qA�=qA�E�A�O�A�Q�A�O�A�M�A�Q�A�S�A�M�A�I�A�K�A�S�A�M�A�Q�A�^5A�\)A�ZA�`BA�hsA�t�A�|�A�A�A�7A�uA╁A❲A�A�-A�9A�RA�RA�^A�ĜA�ƨA�ȴA���A���A���A���A���A��
A���A���A�ƨA�ȴA���A��#A��#A��;A��HA��TA��HA��;A��A��A��A��A��A��A��A��A���A���A�A�A�A�  A�%A�
=A�bA�oA�oA��A� �A��A� �A�$�A�$�A�+A�33A�5?A�5?A�7LA�7LA�9XA�=qA�=qA�E�A�O�A�S�A�XA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�`BA�`BA�\)A�^5A�`BA�ffA�jA�l�A�p�A�t�A�v�A�v�A�t�A�v�A�A�7A�\A�\A�PA�PA�PA�DA�7A�DA�DA�DA�PA�\A�\A�\A�PA�PA�uA㕁A㗍A㗍A㗍A㗍A㕁A�uA�uA㙚A㟾A��A��A��A��A��A��A��A�!A�-A�RA�jA�wA���A���A���A���A���A�wA�ĜA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A��A��
A���A��
A��A��
A��
A��#A��#A��#A��#A��A��#A��/A��#A��#A��#A��A��#A��#A��/A��/A��/A��;A��HA��HA��HA��HA��HA��HA��HA��TA��`A��`A��`A��`A��`A��TA��HA��TA��TA��HA��`A��TA��`A��`A��mA��mA��mA��`A��`A��mA��yA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A���A���A�  A���A���A���A���A���A�  A�  A�  A�  A���A�  A�A�  A�  A���A�  A�  A�  A�A�A�A�A�A�A�  A�  A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�%A�%A�A�A�A�A�%A�%A�%A�%A�%A�%A�%A�%A�1A�1A�1A�%A�1A�1A�1A�1A�1A�1A�1A�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�JA�JA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�bA�VA�VA�VA�VA�bA�bA�bA�bA�bA�VA�VA�VA�VA�VA�JA�JA�JA�JA�JA�JA�JA�JA�JA�JA�
=A�
=A�JA�VA�VA�VA�bA�bA�bA�bA�bA�VA�VA�bA�bA�bA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�bA�bA�bA�VA�VA�JA�VA�VA�VA�JA�JA�JA�JA�JA�
=A�JA�JA�JA�JA�JA�JA�JA�JA�
=A�
=A�JA�JA�JA�VA�VA�VA�JA�VA�JA�VA�VA�VA�VA�VA�VA�VA�JA�VA�JA�JA�JA�VA�JA�
=A�VA�VA�VA�VA�VA�JA�JA�VA�JA�VA�bA�bA�VA�VA�VA�VA�bA�bA�bA�bA�bA�bA�oA�oA�oA�oA��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A�"�A� �A� �A�"�A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A�(�A�(�A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A�&�A�&�A�(�A�(�A�(�A�&�A�&�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�(�A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�+A�+A�+A�+A�+A�-A�-A�+A�+A�+A�-A�-A�-A�-A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�/A�/4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211122100111                              AO  ARCAADJP                                                                    20211122100111    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211122100111  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211122100111  QCF$                G�O�G�O�G�O�C000            
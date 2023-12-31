CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-10T15:43:14Z creation;2022-08-10T15:43:15Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220810154314  20220810155731  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�����>31   @���}'�}@2��j~���d=����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  BffBffB   B(  B0  B8  B@  BHffBP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&y�D'  D'y�D(  D(�fD)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  Dy�D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƃ3D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ D�|�D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D��3D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@QG�@��
@��
A�A%�AE�Ae�A���A���A���A���A���A�A���A���Bz�B	z�B�GB�GB!z�B)z�B1z�B9z�BAz�BI�GBQz�BY{Ba{Biz�Bqz�Byz�B��qB��qB��qB��qB��>B��qB��B��qB��qB��qB��qB��B��qB��qB��B��B��qBĽqBȽqB̽qBнqBԽqBؽqBܽqB�qB�qB�qB�qB�qB��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�CEC^�C ^�C"^�C$EC&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CFxRCHxRCJ^�CL^�CN^�CP^�CR^�CT^�CV^�CX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|xRC~^�C�<)C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�<)C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�<)C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�HD�D��D�D��DHD��D�D��D�D��D�D��D�D��DD�D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&HD&�HD'�D'�HD(�D(�D)�D)��D*�D*��D+�D+�D,�D,��D-�D-��D.�D.��D/�D/��D0HD0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7HD7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>HD>��D?�D?��D@�D@��DA�DA��DBDB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DLDL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS�DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DYHDY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��DwHDw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D�HD��D�K�D���D���D��D�K�D���D���D�
D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�O
D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D�ȤD��D�K�D���D���D��D�H�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D�ȤD��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�O
D���D���D��D�H�D���D���D��D�K�D���D���D��D�K�D���D��
D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�H�D���D���D��D�K�D���D�ȤD��D�K�D���D���D��D�K�D���D���D��D�K�D��
D���D��D�K�D��
D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D��
D���D��D�H�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D��
D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�K�D�D���D��D�K�DË�D���D��D�K�Dċ�D���D��D�O
Dŋ�D���D��D�K�DƏ
D���D��D�K�Dǋ�D���D��D�K�Dȋ�D���D��D�K�Dɋ�D���D��D�K�Dʋ�D���D��D�K�Dˋ�D���D��D�K�D̋�D���D��D�K�D͋�D���D��D�K�D΋�D���D��D�K�Dϋ�D���D��D�K�DЋ�D���D��D�K�Dы�D���D��D�K�Dҋ�D���D��D�H�DӋ�D���D��D�K�Dԋ�D���D��D�K�DՋ�D���D��D�K�D֋�D���D��D�K�D׋�D���D��D�K�D؋�D���D��D�K�Dو�D���D��D�K�Dڋ�D���D��D�K�Dۋ�D���D��D�K�D܈�D���D��D�K�D݋�D���D��D�K�Dދ�D���D��D�K�Dߋ�D���D��D�K�D���D��
D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D�
D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D�
D�K�D��D���D��D�K�D���D��
D�
D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D��D���D��D�K�D���D���D��D�K�D���D���D��D�K�D���D���D��D�O
D��
D��
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aק�Aץ�Aץ�Aק�Aף�A׬Aװ!A״9Aײ-Aי�AׁA�t�A�S�A�O�A�E�A��A��HA��
A֙�A�S�A��AԋDAҡ�A��/A�~�A�A�A͝�A�7LA�C�A�1A��Aƺ^A�/Aš�A�C�A�
=A��FA�{A��A���A�x�A��A�$�A�=qA�`BA��A�VA���A�7LA� �A��A���A�oA��A�|�A���A�VA�ȴA���A�/A���A�/A�{A�x�A�ȴA�VA��\A���A���A�|�A��A���A�p�A�1A���A�t�A��#A��A��A�33A�=qA�5?A�ĜA�A��mA��`A�E�A��A��A���A���A��A���A~Q�Az��Ay��Axv�Aw��AwVAv1AuO�AsdZAo+Am��Am�hAlv�Aj1'Ag�^Ac��AbVAb1A`$�A^ �A]�AZ�/AW?}ATM�AQ��AN��AM?}AL5?AJA�AH�/AHĜAHĜAF��AE�ACO�AB�+AA�AA;dA?p�A>��A>1'A=��A<VA:�/A9G�A7��A7�7A6v�A5ƨA5\)A3A1�A/|�A.�/A-�;A,�A,-A+��A*�HA(�yA&�`A%"�A$1A!��A (�A&�A��AO�A�AVA/Al�A�jA�AAAS�AjAdZAJA"�A?}AA7LA
��A	;dAƨA��A1A��AbNA�AA�
A
=A ��A ��A 5?@�;d@��^@�Z@�|�@���@�{@�E�@�$�@�/@��u@�K�@�@��@�Q�@��y@�7L@�E�@�1@�C�@��@�^5@�-@�z�@�@�%@�1@�\)@���@�-@�%@�j@�r�@�j@��@�{@�7L@��@�\)@��@���@�x�@�`B@�&�@�1'@ӍP@�
=@�-@�7L@���@ϥ�@�t�@�dZ@θR@̬@�ƨ@�;d@ʟ�@��@ɡ�@�x�@�%@ȓu@��@Ǿw@Ə\@���@Ł@�7L@�/@�%@�r�@�1'@�ƨ@�C�@+@�ff@�5?@��#@�O�@�bN@��m@�l�@���@�5?@�`B@�r�@�l�@�K�@�+@���@�E�@�@��h@���@�A�@��@��P@�n�@���@�p�@�V@�Q�@��@�K�@��@��@�^5@��@��h@��@�p�@�X@�7L@�&�@��@��u@�I�@��@�|�@�S�@�
=@��!@�ff@�-@�J@���@��^@���@�p�@�O�@���@���@���@�33@���@���@���@�^5@��@�7L@��@�Ĝ@�z�@�Z@�A�@�  @���@�|�@�o@��!@���@�E�@�@��^@��h@�p�@�hs@��@��/@�1'@���@�\)@��@�ȴ@�n�@��@��#@���@�x�@�`B@�?}@��@��`@��9@���@��@�?}@���@�b@�ƨ@��P@�+@�@��@���@���@�E�@��T@�@�ff@�$�@��h@�X@���@���@��@���@���@�Q�@�(�@��;@��P@�l�@�+@��y@�^5@�-@�-@��@�hs@��u@�j@�z�@��@�b@�"�@�t�@��@�1'@��@���@���@��m@�|�@��@�o@��y@���@�v�@�5?@�@�@�@��@���@��-@��h@���@�b@���@��@��@��P@���@�\)@�@�v�@�v�@���@�{@�x�@�I�@�9X@�(�@���@�|�@��y@�ȴ@���@�&�@���@�%@���@�%@�@���@���@�G�@���@��/@�z�@�bN@�Z@�z�@��@�z�@�j@�A�@�l�@���@��+@��@���@�hs@�?}@���@��@���@���@��F@��P@�S�@�33@�@���@��R@���@�ff@�-@�{@�J@��@��@��^@�G�@�/@��@���@��9@�Q�@��@�l�@�K�@��@��@���@�$�@�J@��@���@�?}@���@��/@��/@��/@��/@��/@���@���@�Ĝ@��j@���@�Z@��@�@\)@~ȴ@~��@~��@~v�@}�@}�h@}/@|��@|9X@{�m@{��@{��@{dZ@{C�@{o@z�@z�!@zn�@zJ@y�7@y%@x�u@w�w@w
=@v5?@v$�@u�@t�D@t1@sƨ@s��@s�@s�@sdZ@r�@r=q@q��@q�7@q�@p�`@pbN@p �@p  @o�@o�@o|�@n�y@m�-@m�h@m/@l��@kƨ@j�@j~�@i�#@i%@h�@hA�@g��@g;d@g
=@f��@eO�@d(�@ct�@cC�@co@b�!@a��@`��@`1'@` �@`  @_�;@_��@_��@_|�@^�@^v�@^E�@]��@]`B@]V@\�/@\�D@\�D@\(�@[��@[dZ@[33@Z��@Z��@Zn�@Y��@Y��@Y��@Y��@Y��@Yhs@Y&�@X��@XbN@W�w@W\)@WK�@W;d@W+@V��@VV@U��@U`B@U�@T��@T1@R�H@R^5@RM�@R-@Q�@Q7L@P�`@P��@Pb@O�@O+@Nȴ@N�+@Nff@NE�@NE�@N$�@N@M��@M?}@L�/@L�j@LI�@L�@L1@K��@Kƨ@KS�@K@J�!@J~�@J-@I�#@I&�@HbN@H �@H �@G��@GK�@G+@G
=@Fȴ@Fv�@FV@F5?@E�@D�@D�j@D�D@DZ@Dz�@D9X@C��@Cƨ@C"�@B�!@B�@A��@A�^@A��@A��@A7L@?�;@?;d@>�y@>��@>v�@>V@>E�@>@=�@=`B@=O�@=?}@=/@=�@<�@<�j@<�D@<�D@<z�@<j@<Z@;�m@;t�@;"�@:�@:�\@:^5@:M�@9��@9�#@9�^@9X@8r�@7��@6��@6ȴ@6��@6�+@6V@6$�@5��@5?}@5V@4�j@4Z@41@3��@2�@2��@2n�@2J@1��@0��@01'@0  @/�@/�P@/l�@/\)@/K�@/;d@/�@.��@-�@-@-�@-?}@-�@,��@,�j@,Z@,(�@,1@+��@+�m@+��@+S�@+@*��@*^5@*�@)��@)�@)�#@)��@)��@)x�@)&�@(��@(�`@(Ĝ@(��@(�@(A�@'��@'�P@'|�@'+@&�@&��@&v�@&$�@&@%�@%��@%`B@%V@$�/@$�j@$�D@$j@$I�@$(�@$1@#�m@#ƨ@#�F@#�@"��@"n�@"=q@"�@!��@!�@!��@!��@!�7@!G�@ �`@ A�@�@��@�w@�@�P@|�@K�@�@ȴ@V@@@@@��@p�@��@�@��@z�@9X@�@1@ƨ@t�@C�@o@�@��@�!@��@�\@M�@-@��@��@��@�u@�@Q�@�P@\)@K�@
=@��@�R@��@��@v�@E�@{@@�h@�@p�@p�@`B@`B@O�@/@/@��@�/@�/@�/@�j@�D@�D@�D@�D@z�@9X@1@�F@t�@33@@�@��@~�@n�@^5@^5@^5@M�@�@��@x�@x�@x�@hs@X@7L@�@%@Ĝ@Q�@Q�@A�@A�@A�@1'@b@\)@;d@
=@�R@��@��@��@�+@v�@ff@ff@E�@5?@5?@5?@5?@$�@@�T@��@�@?}@�@�@z�@Z@Z@Z@9X@9X@9X@(�@�@1@��@�m@�
@�@dZ@
��@
n�@
^5@
=q@	��@	�7@	hs@	G�@	&�@�`@r�@bN@Q�@bN@Q�@Q�@Q�@Q�@Q�@A�@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aק�Aץ�Aץ�Aק�Aף�A׬Aװ!A״9Aײ-Aי�AׁA�t�A�S�A�O�A�E�A��A��HA��
A֙�A�S�A��AԋDAҡ�A��/A�~�A�A�A͝�A�7LA�C�A�1A��Aƺ^A�/Aš�A�C�A�
=A��FA�{A��A���A�x�A��A�$�A�=qA�`BA��A�VA���A�7LA� �A��A���A�oA��A�|�A���A�VA�ȴA���A�/A���A�/A�{A�x�A�ȴA�VA��\A���A���A�|�A��A���A�p�A�1A���A�t�A��#A��A��A�33A�=qA�5?A�ĜA�A��mA��`A�E�A��A��A���A���A��A���A~Q�Az��Ay��Axv�Aw��AwVAv1AuO�AsdZAo+Am��Am�hAlv�Aj1'Ag�^Ac��AbVAb1A`$�A^ �A]�AZ�/AW?}ATM�AQ��AN��AM?}AL5?AJA�AH�/AHĜAHĜAF��AE�ACO�AB�+AA�AA;dA?p�A>��A>1'A=��A<VA:�/A9G�A7��A7�7A6v�A5ƨA5\)A3A1�A/|�A.�/A-�;A,�A,-A+��A*�HA(�yA&�`A%"�A$1A!��A (�A&�A��AO�A�AVA/Al�A�jA�AAAS�AjAdZAJA"�A?}AA7LA
��A	;dAƨA��A1A��AbNA�AA�
A
=A ��A ��A 5?@�;d@��^@�Z@�|�@���@�{@�E�@�$�@�/@��u@�K�@�@��@�Q�@��y@�7L@�E�@�1@�C�@��@�^5@�-@�z�@�@�%@�1@�\)@���@�-@�%@�j@�r�@�j@��@�{@�7L@��@�\)@��@���@�x�@�`B@�&�@�1'@ӍP@�
=@�-@�7L@���@ϥ�@�t�@�dZ@θR@̬@�ƨ@�;d@ʟ�@��@ɡ�@�x�@�%@ȓu@��@Ǿw@Ə\@���@Ł@�7L@�/@�%@�r�@�1'@�ƨ@�C�@+@�ff@�5?@��#@�O�@�bN@��m@�l�@���@�5?@�`B@�r�@�l�@�K�@�+@���@�E�@�@��h@���@�A�@��@��P@�n�@���@�p�@�V@�Q�@��@�K�@��@��@�^5@��@��h@��@�p�@�X@�7L@�&�@��@��u@�I�@��@�|�@�S�@�
=@��!@�ff@�-@�J@���@��^@���@�p�@�O�@���@���@���@�33@���@���@���@�^5@��@�7L@��@�Ĝ@�z�@�Z@�A�@�  @���@�|�@�o@��!@���@�E�@�@��^@��h@�p�@�hs@��@��/@�1'@���@�\)@��@�ȴ@�n�@��@��#@���@�x�@�`B@�?}@��@��`@��9@���@��@�?}@���@�b@�ƨ@��P@�+@�@��@���@���@�E�@��T@�@�ff@�$�@��h@�X@���@���@��@���@���@�Q�@�(�@��;@��P@�l�@�+@��y@�^5@�-@�-@��@�hs@��u@�j@�z�@��@�b@�"�@�t�@��@�1'@��@���@���@��m@�|�@��@�o@��y@���@�v�@�5?@�@�@�@��@���@��-@��h@���@�b@���@��@��@��P@���@�\)@�@�v�@�v�@���@�{@�x�@�I�@�9X@�(�@���@�|�@��y@�ȴ@���@�&�@���@�%@���@�%@�@���@���@�G�@���@��/@�z�@�bN@�Z@�z�@��@�z�@�j@�A�@�l�@���@��+@��@���@�hs@�?}@���@��@���@���@��F@��P@�S�@�33@�@���@��R@���@�ff@�-@�{@�J@��@��@��^@�G�@�/@��@���@��9@�Q�@��@�l�@�K�@��@��@���@�$�@�J@��@���@�?}@���@��/@��/@��/@��/@��/@���@���@�Ĝ@��j@���@�Z@��@�@\)@~ȴ@~��@~��@~v�@}�@}�h@}/@|��@|9X@{�m@{��@{��@{dZ@{C�@{o@z�@z�!@zn�@zJ@y�7@y%@x�u@w�w@w
=@v5?@v$�@u�@t�D@t1@sƨ@s��@s�@s�@sdZ@r�@r=q@q��@q�7@q�@p�`@pbN@p �@p  @o�@o�@o|�@n�y@m�-@m�h@m/@l��@kƨ@j�@j~�@i�#@i%@h�@hA�@g��@g;d@g
=@f��@eO�@d(�@ct�@cC�@co@b�!@a��@`��@`1'@` �@`  @_�;@_��@_��@_|�@^�@^v�@^E�@]��@]`B@]V@\�/@\�D@\�D@\(�@[��@[dZ@[33@Z��@Z��@Zn�@Y��@Y��@Y��@Y��@Y��@Yhs@Y&�@X��@XbN@W�w@W\)@WK�@W;d@W+@V��@VV@U��@U`B@U�@T��@T1@R�H@R^5@RM�@R-@Q�@Q7L@P�`@P��@Pb@O�@O+@Nȴ@N�+@Nff@NE�@NE�@N$�@N@M��@M?}@L�/@L�j@LI�@L�@L1@K��@Kƨ@KS�@K@J�!@J~�@J-@I�#@I&�@HbN@H �@H �@G��@GK�@G+@G
=@Fȴ@Fv�@FV@F5?@E�@D�@D�j@D�D@DZ@Dz�@D9X@C��@Cƨ@C"�@B�!@B�@A��@A�^@A��@A��@A7L@?�;@?;d@>�y@>��@>v�@>V@>E�@>@=�@=`B@=O�@=?}@=/@=�@<�@<�j@<�D@<�D@<z�@<j@<Z@;�m@;t�@;"�@:�@:�\@:^5@:M�@9��@9�#@9�^@9X@8r�@7��@6��@6ȴ@6��@6�+@6V@6$�@5��@5?}@5V@4�j@4Z@41@3��@2�@2��@2n�@2J@1��@0��@01'@0  @/�@/�P@/l�@/\)@/K�@/;d@/�@.��@-�@-@-�@-?}@-�@,��@,�j@,Z@,(�@,1@+��@+�m@+��@+S�@+@*��@*^5@*�@)��@)�@)�#@)��@)��@)x�@)&�@(��@(�`@(Ĝ@(��@(�@(A�@'��@'�P@'|�@'+@&�@&��@&v�@&$�@&@%�@%��@%`B@%V@$�/@$�j@$�D@$j@$I�@$(�@$1@#�m@#ƨ@#�F@#�@"��@"n�@"=q@"�@!��@!�@!��@!��@!�7@!G�@ �`@ A�@�@��@�w@�@�P@|�@K�@�@ȴ@V@@@@@��@p�@��@�@��@z�@9X@�@1@ƨ@t�@C�@o@�@��@�!@��@�\@M�@-@��@��@��@�u@�@Q�@�P@\)@K�@
=@��@�R@��@��@v�@E�@{@@�h@�@p�@p�@`B@`B@O�@/@/@��@�/@�/@�/@�j@�D@�D@�D@�D@z�@9X@1@�F@t�@33@@�@��@~�@n�@^5@^5@^5@M�@�@��@x�@x�@x�@hs@X@7L@�@%@Ĝ@Q�@Q�@A�@A�@A�@1'@b@\)@;d@
=@�R@��@��@��@�+@v�@ff@ff@E�@5?@5?@5?@5?@$�@@�T@��@�@?}@�@�@z�@Z@Z@Z@9X@9X@9X@(�@�@1@��@�m@�
@�@dZ@
��@
n�@
^5@
=q@	��@	�7@	hs@	G�@	&�@�`@r�@bN@Q�@bN@Q�@Q�@Q�@Q�@Q�@A�@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBBBBBBBBBB  BB  B  BB%BVB�B1'BB�BI�B_;Bq�B|�B�hB�B�3B�9B�FB�?B�3B��B�bB�=B�+BbNB`BBs�Bt�B�+B�B�FB�RB��B��B��B�?B�9B�B��B��B��B��B��B��B�JB�=B�=Bw�B{�B{�Bz�BjB]/BZB5?B�BPBB�yB�HB��B�^B�B��Bw�BhsB1'B{B
�B
�/B
�wB
��B
�B
t�B
R�B
N�B
C�B
?}B
5?B
�B
�B
bB
JB
+B
B	��B	�B	�`B	�HB	�`B	�TB	�
B	��B	�dB	�B	�B	��B	��B	�oB	�+B	t�B	dZB	S�B	D�B	;dB	6FB	1'B	'�B	%�B	$�B	!�B	�B	hB	PB	DB	1B	B��B��B��B�B�mB�HB�#B�
B��B��B��BȴBB��B��B��B��B��BB��B�wB�9B�B�B�B��B��B��B��B��B��B��B�oB�bB�hB�hB�{B�{B�uB��B�hB�\B�PB�JB�=B�1B�7B�1B�1B�1B�+B�%B�%B�1B�1B�=B�PB�\B�\B�PB�JB�=B�7B�JB�VB��B��B��B�B�B�B�!B�B�!B�'B�?B�LB�RB�RB�RB�XB�^B�}B�}B��BBÖBÖBƨBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�;B�;B�TB�TB�TB�fB�B�B�B�B��B��B��B��B��B	  B	B		7B	DB	PB	VB	\B	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	+B	+B	,B	-B	0!B	1'B	2-B	5?B	7LB	:^B	:^B	A�B	E�B	E�B	H�B	M�B	N�B	S�B	VB	XB	[#B	_;B	aHB	bNB	bNB	cTB	dZB	dZB	e`B	iyB	jB	m�B	p�B	q�B	s�B	u�B	x�B	y�B	z�B	|�B	|�B	|�B	}�B	~�B	�B	�%B	�1B	�DB	�JB	�JB	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�XB	�dB	�wB	��B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�
B	�
B	�
B	�B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�;B	�NB	�TB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B

=B

=B
DB
PB
VB
\B
PB
JB
VB
VB
PB
PB
JB
JB
JB
+B
%B
+B
DB
DB
PB
oB
hB
\B
\B
hB
bB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
(�B
'�B
'�B
'�B
'�B
(�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
,B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
6FB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
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
[#B
\)B
\)B
\)B
\)B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
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
s�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�bB
�hB
�oB
�oB
�oB
�uB
�oB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBBBBBBBBBB  BB  B  BB%BVB�B1'BB�BI�B_;Bq�B|�B�hB�B�3B�9B�FB�?B�3B��B�bB�=B�+BbNB`BBs�Bt�B�+B�B�FB�RB��B��B��B�?B�9B�B��B��B��B��B��B��B�JB�=B�=Bw�B{�B{�Bz�BjB]/BZB5?B�BPBB�yB�HB��B�^B�B��Bw�BhsB1'B{B
�B
�/B
�wB
��B
�B
t�B
R�B
N�B
C�B
?}B
5?B
�B
�B
bB
JB
+B
B	��B	�B	�`B	�HB	�`B	�TB	�
B	��B	�dB	�B	�B	��B	��B	�oB	�+B	t�B	dZB	S�B	D�B	;dB	6FB	1'B	'�B	%�B	$�B	!�B	�B	hB	PB	DB	1B	B��B��B��B�B�mB�HB�#B�
B��B��B��BȴBB��B��B��B��B��BB��B�wB�9B�B�B�B��B��B��B��B��B��B��B�oB�bB�hB�hB�{B�{B�uB��B�hB�\B�PB�JB�=B�1B�7B�1B�1B�1B�+B�%B�%B�1B�1B�=B�PB�\B�\B�PB�JB�=B�7B�JB�VB��B��B��B�B�B�B�!B�B�!B�'B�?B�LB�RB�RB�RB�XB�^B�}B�}B��BBÖBÖBƨBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�;B�;B�TB�TB�TB�fB�B�B�B�B��B��B��B��B��B	  B	B		7B	DB	PB	VB	\B	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	+B	+B	,B	-B	0!B	1'B	2-B	5?B	7LB	:^B	:^B	A�B	E�B	E�B	H�B	M�B	N�B	S�B	VB	XB	[#B	_;B	aHB	bNB	bNB	cTB	dZB	dZB	e`B	iyB	jB	m�B	p�B	q�B	s�B	u�B	x�B	y�B	z�B	|�B	|�B	|�B	}�B	~�B	�B	�%B	�1B	�DB	�JB	�JB	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�XB	�dB	�wB	��B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�
B	�
B	�
B	�B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�;B	�NB	�TB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
	7B
	7B
	7B

=B

=B
DB
PB
VB
\B
PB
JB
VB
VB
PB
PB
JB
JB
JB
+B
%B
+B
DB
DB
PB
oB
hB
\B
\B
hB
bB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
(�B
'�B
'�B
'�B
'�B
(�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
,B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
6FB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
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
[#B
\)B
\)B
\)B
\)B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
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
s�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�bB
�hB
�oB
�oB
�oB
�uB
�oB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220811004310  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220810154314  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220810154315  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220810154315                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220811004320  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220811004320  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220810155731                      G�O�G�O�G�O�                
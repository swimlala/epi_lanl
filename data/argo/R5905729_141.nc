CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-07T10:02:45Z creation      
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
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20220307100245  20220307100245  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @پ�q�F1   @پ�`�f@&�+I��d?S���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @���@���A   A   AA��A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bi��Bp  By33B~ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C�fC  C  C
  C  C  C�C�C�fC�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ D�|�D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�<�Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@�33@�ffA33A<��A[33Ay��A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B?33BF��BN��BV��B^��BhfgBn��Bx  B}33B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffC�3C��C�3C�3C	�3C�3C�3C��C��C��C��C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}��C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}��D~l�D~��Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��3D��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��3D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD���D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�vfDĶfD��fD�6fD�s3DŶfD��fD�6fD�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD��fD�6fD�vfDζfD��fD�6fD�vfD϶fD��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�33D�vfD޶fD��fD�6fD�vfD߶fD���D�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�s3D�3D��fD�6fD�vfD�fD��fD�9�D�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�s3D�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�y�D��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD���D�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�C�A�E�A�G�A�K�A�M�A�K�A�A�A�?}A�?}A�=qA���A���A���A���AվwAպ^AլAգ�A՛�AՋDA�z�A�l�A�(�A�%A�ĜA�ffAоwAк^A���Aė�A��yA�A���A���A�G�A��uA�`BA��9A�
=A�n�A��A��A��-A���A�S�A�1A��A�ZA��+A���A���A��/A��A�?}A�&�A�bA�VA���A�p�A~bAw�TAq\)An�jAjjAe�wAaA^�yAZ�+AV(�APȴAL��AH�`AG�AE�7AA�^A@�/A?`BA>��A>�A>M�A>1'A>1A=ƨA=l�A<r�A;�FA;33A:��A:E�A9A7A6�A6�A5p�A4VA3��A2��A1�TA1�A0��A0��A0JA/A/|�A/&�A.z�A-�A-�7A-�A,��A,z�A,M�A,9XA,1'A+��A+t�A+7LA*��A*$�A)��A)�;A)�A)�A)l�A)dZA)`BA)XA)XA)S�A)oA(�A(v�A'��A'/A&ĜA&z�A&M�A&JA%A%K�A$��A$�!A$ �A#��A#|�A"��A"I�A"�A!�A!��A!hsA!`BA!7LA �yA �9A�A�-Ax�A�A�yA��AffAA�A1Al�A�A�jA��A�AjA{A�wA`BA
=AȴA�A�wA��A��A|�A;dAAĜAffA��A�yA�A��A�AoA��Av�AbA�A7LAjA-A�
AdZA
=A��AQ�A(�A�;A�-AG�AVA%AA�A��A�jA�A��A�+AjAM�A�AA�A;dA1'A�PAK�A�A%A
�A
��A
��A
~�A
M�A
�A	�A	��A��A�uA �A�AXA�RAA�A��AhsAVA��A��A�+AQ�A��AA�+An�AQ�AA�A9XA�AA  A��A��A�A��AhsA ��A =qA 9XA (�A �A J@��m@��@�|�@�+@��@�%@� �@���@���@�Z@���@��R@��h@��9@��
@�o@�v�@�$�@���@���@�9@��@@�!@�`B@�bN@�1@�@�V@�G�@�I�@�F@�v�@�@�X@���@�Q�@�\)@�o@��@���@�X@�j@��@߶F@��H@�v�@�V@��@���@���@�hs@���@��m@�S�@���@�=q@���@ّh@�?}@�&�@�V@��`@؛�@� �@ץ�@�S�@�
=@ְ!@��@�p�@Ԭ@ԛ�@ԓu@ԃ@�I�@���@�l�@ҸR@�^5@�V@�V@�V@�-@���@Ѻ^@�7L@��@�j@�t�@�ȴ@Χ�@Η�@�@�1'@�
=@�E�@�J@Ɂ@���@ț�@���@��@ź^@�V@�b@�"�@¸R@§�@�5?@��-@���@���@��@���@�(�@�ƨ@�K�@�~�@��@�O�@���@�9X@��w@�C�@��\@�@��/@��j@���@�A�@��P@�@��R@��@�X@�Z@���@�\)@�
=@���@�5?@���@��^@�`B@��@���@�j@��@��;@��w@���@�;d@���@��\@�M�@���@�O�@�V@��j@�1@�K�@���@�n�@�5?@�{@�J@��T@���@��`@�1@�n�@�@�hs@���@���@�j@�I�@�9X@��@�b@�  @��@�ƨ@���@���@�S�@��y@���@�{@���@�x�@�X@�&�@��9@�9X@��m@�t�@�33@�"�@��y@���@�=q@��T@���@��7@���@��D@�b@�t�@��@�~�@��@��@��@��j@��@�j@�b@��w@�|�@�C�@��@���@���@�~�@�{@���@���@��^@��h@�p�@�/@��`@��D@�Q�@�A�@�1'@��@�ƨ@��F@��@���@���@��H@�^5@��T@���@��/@�1'@��
@���@��@�|�@�\)@�33@�@��y@��y@��@���@�^5@�@�&�@�z�@�(�@�  @��
@���@�ƨ@��w@���@�33@��@�~�@�V@���@���@��h@��7@��@�`B@���@�Z@���@���@�t�@��y@��!@�V@�-@�@��^@�G�@��@�Ĝ@�bN@�Q�@�1@��m@�t�@��@��H@���@�E�@���@���@��^@��-@���@�p�@���@���@�;@�@~�+@}�h@|�@{�F@{S�@{33@{o@z^5@y��@xbN@xb@w�w@v��@vv�@vV@vV@vE�@u��@t�@tI�@s��@s@r��@q�@qG�@q&�@q�@p��@pbN@o��@o\)@n��@nV@nE�@m�h@mV@l��@l��@k��@k@j=q@j�@i�@i�7@i&�@hĜ@hĜ@h�9@hr�@g�@g�P@g|�@gl�@gK�@g�@f�@fff@f@f@f@f@f@e�T@e�@d1@ct�@cC�@c@b~�@a�#@ahs@aG�@a&�@`�`@`A�@_�@_�w@_l�@_�@^�y@^��@^�+@^v�@^V@^5?@^$�@^{@]�@]�T@]�-@]�@]�@\�@\I�@[�m@[�
@[�@[dZ@[o@Z�!@Z~�@Y��@YG�@XĜ@X��@Xr�@W�@W�P@W\)@V�y@V5?@V@U�-@U�@U`B@U?}@U�@T�D@T�@SdZ@S"�@R�@P��@O�;@O;d@N�y@N{@M��@M`B@MO�@MO�@MV@L�j@Lj@K��@K��@K33@J��@J=q@I�@H��@H1'@G��@G�@F�@F��@FE�@E�@EV@D��@D9X@C��@C�F@C�@CdZ@C"�@B��@Bn�@BJ@Ahs@A%@@�9@@ �@?\)@>v�@=@=?}@<�j@<1@;�
@;��@;@:~�@:-@9�^@9�7@97L@8�`@8�u@8  @7�@7l�@7\)@7�@6�y@6ȴ@65?@5`B@4�/@4�@4z�@49X@3�
@3�F@3��@3S�@3C�@3"�@3@2��@2n�@2�@2�@1��@1��@1%@0�`@0Ĝ@0r�@0 �@/��@/�w@/�@/�@/l�@/+@/+@.��@.��@.��@.v�@.E�@.@-�T@-�T@-��@-?}@,�j@,I�@,�@+��@*~�@*M�@*-@*J@)��@)��@)x�@)�@(��@(  @'�;@'��@'l�@'�@&ȴ@&��@&�+@&v�@&$�@%��@%O�@$��@$z�@$I�@#��@#�m@#�m@#�F@#t�@#dZ@#S�@#S�@#o@"��@"~�@"n�@"M�@"=q@!��@!G�@ �`@ �9@ r�@ A�@  �@ b@�@�@|�@��@ff@5?@�@�T@��@�-@�h@p�@V@�@��@�D@j@j@j@j@Z@I�@9X@1@��@1@��@�F@�F@��@��@t�@C�@C�@33@o@�@��@��@��@��@�\@n�@n�@~�@n�@n�@n�@n�@n�@^5@M�@-@-@J@�#@�^@��@�7@hs@X@7L@7L@&�@&�@&�@&�@�@��@�`@Ĝ@�9@��@r�@1'@  @  @  @�@�w@�P@l�@+@�@�+@V@$�@{@�T@��@�h@�@��@j@��@ƨ@��@S�@"�@"�@o@�H@��@^5@��@�#@��@x�@7L@�@�9@r�@A�@  @  @�@��@�w@��@l�@�@�R@ff@E�@5?@5?@$�@{@@�T@��@�-@@O�@V@�j@�@�1111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�C�A�E�A�G�A�K�A�M�A�K�A�A�A�?}A�?}A�=qA���A���A���A���AվwAպ^AլAգ�A՛�AՋDA�z�A�l�A�(�A�%A�ĜA�ffAоwAк^A���Aė�A��yA�A���A���A�G�A��uA�`BA��9A�
=A�n�A��A��A��-A���A�S�A�1A��A�ZA��+A���A���A��/A��A�?}A�&�A�bA�VA���A�p�A~bAw�TAq\)An�jAjjAe�wAaA^�yAZ�+AV(�APȴAL��AH�`AG�AE�7AA�^A@�/A?`BA>��A>�A>M�A>1'A>1A=ƨA=l�A<r�A;�FA;33A:��A:E�A9A7A6�A6�A5p�A4VA3��A2��A1�TA1�A0��A0��A0JA/A/|�A/&�A.z�A-�A-�7A-�A,��A,z�A,M�A,9XA,1'A+��A+t�A+7LA*��A*$�A)��A)�;A)�A)�A)l�A)dZA)`BA)XA)XA)S�A)oA(�A(v�A'��A'/A&ĜA&z�A&M�A&JA%A%K�A$��A$�!A$ �A#��A#|�A"��A"I�A"�A!�A!��A!hsA!`BA!7LA �yA �9A�A�-Ax�A�A�yA��AffAA�A1Al�A�A�jA��A�AjA{A�wA`BA
=AȴA�A�wA��A��A|�A;dAAĜAffA��A�yA�A��A�AoA��Av�AbA�A7LAjA-A�
AdZA
=A��AQ�A(�A�;A�-AG�AVA%AA�A��A�jA�A��A�+AjAM�A�AA�A;dA1'A�PAK�A�A%A
�A
��A
��A
~�A
M�A
�A	�A	��A��A�uA �A�AXA�RAA�A��AhsAVA��A��A�+AQ�A��AA�+An�AQ�AA�A9XA�AA  A��A��A�A��AhsA ��A =qA 9XA (�A �A J@��m@��@�|�@�+@��@�%@� �@���@���@�Z@���@��R@��h@��9@��
@�o@�v�@�$�@���@���@�9@��@@�!@�`B@�bN@�1@�@�V@�G�@�I�@�F@�v�@�@�X@���@�Q�@�\)@�o@��@���@�X@�j@��@߶F@��H@�v�@�V@��@���@���@�hs@���@��m@�S�@���@�=q@���@ّh@�?}@�&�@�V@��`@؛�@� �@ץ�@�S�@�
=@ְ!@��@�p�@Ԭ@ԛ�@ԓu@ԃ@�I�@���@�l�@ҸR@�^5@�V@�V@�V@�-@���@Ѻ^@�7L@��@�j@�t�@�ȴ@Χ�@Η�@�@�1'@�
=@�E�@�J@Ɂ@���@ț�@���@��@ź^@�V@�b@�"�@¸R@§�@�5?@��-@���@���@��@���@�(�@�ƨ@�K�@�~�@��@�O�@���@�9X@��w@�C�@��\@�@��/@��j@���@�A�@��P@�@��R@��@�X@�Z@���@�\)@�
=@���@�5?@���@��^@�`B@��@���@�j@��@��;@��w@���@�;d@���@��\@�M�@���@�O�@�V@��j@�1@�K�@���@�n�@�5?@�{@�J@��T@���@��`@�1@�n�@�@�hs@���@���@�j@�I�@�9X@��@�b@�  @��@�ƨ@���@���@�S�@��y@���@�{@���@�x�@�X@�&�@��9@�9X@��m@�t�@�33@�"�@��y@���@�=q@��T@���@��7@���@��D@�b@�t�@��@�~�@��@��@��@��j@��@�j@�b@��w@�|�@�C�@��@���@���@�~�@�{@���@���@��^@��h@�p�@�/@��`@��D@�Q�@�A�@�1'@��@�ƨ@��F@��@���@���@��H@�^5@��T@���@��/@�1'@��
@���@��@�|�@�\)@�33@�@��y@��y@��@���@�^5@�@�&�@�z�@�(�@�  @��
@���@�ƨ@��w@���@�33@��@�~�@�V@���@���@��h@��7@��@�`B@���@�Z@���@���@�t�@��y@��!@�V@�-@�@��^@�G�@��@�Ĝ@�bN@�Q�@�1@��m@�t�@��@��H@���@�E�@���@���@��^@��-@���@�p�@���@���@�;@�@~�+@}�h@|�@{�F@{S�@{33@{o@z^5@y��@xbN@xb@w�w@v��@vv�@vV@vV@vE�@u��@t�@tI�@s��@s@r��@q�@qG�@q&�@q�@p��@pbN@o��@o\)@n��@nV@nE�@m�h@mV@l��@l��@k��@k@j=q@j�@i�@i�7@i&�@hĜ@hĜ@h�9@hr�@g�@g�P@g|�@gl�@gK�@g�@f�@fff@f@f@f@f@f@e�T@e�@d1@ct�@cC�@c@b~�@a�#@ahs@aG�@a&�@`�`@`A�@_�@_�w@_l�@_�@^�y@^��@^�+@^v�@^V@^5?@^$�@^{@]�@]�T@]�-@]�@]�@\�@\I�@[�m@[�
@[�@[dZ@[o@Z�!@Z~�@Y��@YG�@XĜ@X��@Xr�@W�@W�P@W\)@V�y@V5?@V@U�-@U�@U`B@U?}@U�@T�D@T�@SdZ@S"�@R�@P��@O�;@O;d@N�y@N{@M��@M`B@MO�@MO�@MV@L�j@Lj@K��@K��@K33@J��@J=q@I�@H��@H1'@G��@G�@F�@F��@FE�@E�@EV@D��@D9X@C��@C�F@C�@CdZ@C"�@B��@Bn�@BJ@Ahs@A%@@�9@@ �@?\)@>v�@=@=?}@<�j@<1@;�
@;��@;@:~�@:-@9�^@9�7@97L@8�`@8�u@8  @7�@7l�@7\)@7�@6�y@6ȴ@65?@5`B@4�/@4�@4z�@49X@3�
@3�F@3��@3S�@3C�@3"�@3@2��@2n�@2�@2�@1��@1��@1%@0�`@0Ĝ@0r�@0 �@/��@/�w@/�@/�@/l�@/+@/+@.��@.��@.��@.v�@.E�@.@-�T@-�T@-��@-?}@,�j@,I�@,�@+��@*~�@*M�@*-@*J@)��@)��@)x�@)�@(��@(  @'�;@'��@'l�@'�@&ȴ@&��@&�+@&v�@&$�@%��@%O�@$��@$z�@$I�@#��@#�m@#�m@#�F@#t�@#dZ@#S�@#S�@#o@"��@"~�@"n�@"M�@"=q@!��@!G�@ �`@ �9@ r�@ A�@  �@ b@�@�@|�@��@ff@5?@�@�T@��@�-@�h@p�@V@�@��@�D@j@j@j@j@Z@I�@9X@1@��@1@��@�F@�F@��@��@t�@C�@C�@33@o@�@��@��@��@��@�\@n�@n�@~�@n�@n�@n�@n�@n�@^5@M�@-@-@J@�#@�^@��@�7@hs@X@7L@7L@&�@&�@&�@&�@�@��@�`@Ĝ@�9@��@r�@1'@  @  @  @�@�w@�P@l�@+@�@�+@V@$�@{@�T@��@�h@�@��@j@��@ƨ@��@S�@"�@"�@o@�H@��@^5@��@�#@��@x�@7L@�@�9@r�@A�@  @  @�@��@�w@��@l�@�@�R@ff@E�@5?@5?@$�@{@@�T@��@�-@@O�@V@�j@�@�1111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	8RB	8RB	8RB	8RB	8RB	7LB	7LB	8RB	9XB	8RB	5?B	?}B	K�B	M�B	T�B	W
B	XB	\)B	`BB	bNB	hsB	k�B	jB	q�B	p�B	k�B	hsB	�B	�B
(�B
o�B
��B�B9XBaHBw�BbNBJB9XB�B;dBW
BP�B1'B�B�B
�B  B
�B
�#B
�9B
��B
� B
%�B
oB
D�B
6FB	��B	�ZB	�/B	�RB	�B	�B	��B	iyB	W
B	S�B	,B	!�B	�B	=qB	T�B	hsB	R�B	=qB	��B	��B	ƨB	�B	�BB	�ZB	�`B	�fB	�yB	�B
{B
5?B
I�B
Q�B
W
B
YB
�%B
�hB
��B
��B
�'B
�3B
�FB
�jB
ȴB
ǮB
B
ŢB
ŢB
��B
�jB
�^B
�wB
�qB
��B
B
ĜB
ƨB
ǮB
ŢB
��B
ĜB
��B
�wB
ǮB
��B
ɺB
ɺB
��B
��B
��B
��B
ɺB
ǮB
��B
�wB
�dB
�9B
�FB
�dB
�dB
�jB
�^B
�XB
�FB
�LB
�FB
�-B
�-B
�'B
��B
�B
�3B
�!B
�!B
�!B
�-B
�B
�B
��B
��B
�!B
�9B
�-B
�9B
�-B
�3B
�-B
�B
��B
��B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
�+B
�%B
�hB
�\B
�VB
�hB
�=B
�7B
�7B
� B
x�B
�B
�B
|�B
}�B
~�B
z�B
~�B
|�B
|�B
{�B
|�B
�B
�B
� B
~�B
~�B
~�B
~�B
|�B
{�B
y�B
w�B
u�B
n�B
gmB
_;B
cTB
m�B
n�B
p�B
o�B
n�B
k�B
l�B
iyB
ffB
dZB
_;B
W
B
[#B
W
B
W
B
W
B
P�B
Q�B
Q�B
N�B
N�B
S�B
S�B
R�B
P�B
H�B
A�B
H�B
R�B
R�B
R�B
S�B
Q�B
Q�B
Q�B
P�B
M�B
L�B
L�B
G�B
A�B
G�B
P�B
O�B
O�B
M�B
L�B
J�B
H�B
D�B
;dB
:^B
9XB
33B
8RB
2-B
9XB
7LB
49B
8RB
7LB
9XB
8RB
9XB
7LB
33B
5?B
1'B
49B
/B
.B
-B
5?B
/B
+B
+B
'�B
+B
$�B
+B
(�B
)�B
)�B
&�B
,B
)�B
&�B
&�B
&�B
%�B
&�B
#�B
'�B
+B
+B
+B
(�B
%�B
!�B
�B
!�B
#�B
 �B
#�B
&�B
%�B
'�B
&�B
$�B
#�B
 �B
 �B
"�B
!�B
�B
�B
�B
�B
&�B
&�B
$�B
!�B
�B
�B
�B
 �B
%�B
%�B
#�B
!�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
oB
+B
VB
�B
�B
�B
�B
�B
{B
bB
VB
uB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
oB
oB
hB
�B
�B
�B
uB
�B
�B
uB
bB
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
bB
VB
�B
 �B
 �B
!�B
$�B
&�B
'�B
&�B
'�B
'�B
&�B
%�B
%�B
%�B
#�B
"�B
$�B
 �B
!�B
'�B
&�B
%�B
"�B
"�B
$�B
&�B
'�B
+B
)�B
(�B
(�B
)�B
+B
,B
'�B
'�B
&�B
&�B
'�B
(�B
(�B
+B
-B
.B
/B
1'B
/B
1'B
1'B
2-B
33B
33B
1'B
2-B
1'B
1'B
49B
6FB
49B
49B
33B
33B
49B
5?B
7LB
6FB
5?B
6FB
8RB
7LB
6FB
49B
-B
/B
/B
2-B
-B
0!B
33B
6FB
8RB
8RB
7LB
7LB
7LB
9XB
:^B
9XB
7LB
49B
2-B
33B
49B
8RB
;dB
;dB
<jB
<jB
<jB
;dB
9XB
;dB
9XB
<jB
;dB
<jB
>wB
?}B
>wB
<jB
8RB
9XB
;dB
>wB
?}B
<jB
?}B
?}B
@�B
A�B
@�B
?}B
B�B
A�B
A�B
E�B
D�B
E�B
C�B
D�B
E�B
F�B
E�B
G�B
H�B
J�B
J�B
I�B
G�B
D�B
E�B
C�B
G�B
H�B
F�B
H�B
H�B
L�B
M�B
L�B
I�B
H�B
H�B
K�B
M�B
L�B
M�B
P�B
P�B
O�B
L�B
K�B
M�B
M�B
N�B
P�B
N�B
P�B
R�B
R�B
Q�B
P�B
O�B
Q�B
P�B
Q�B
R�B
P�B
Q�B
T�B
R�B
O�B
P�B
Q�B
VB
VB
T�B
T�B
VB
XB
XB
W
B
VB
W
B
ZB
ZB
ZB
YB
YB
YB
ZB
\)B
[#B
[#B
ZB
XB
T�B
S�B
XB
ZB
ZB
YB
W
B
YB
\)B
[#B
[#B
YB
[#B
\)B
\)B
\)B
^5B
]/B
_;B
_;B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
]/B
\)B
\)B
]/B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
^5B
]/B
_;B
bNB
aHB
`BB
aHB
cTB
bNB
aHB
dZB
dZB
e`B
e`B
dZB
cTB
bNB
aHB
aHB
aHB
_;B
]/B
cTB
e`B
ffB
dZB
gmB
iyB
jB
jB
iyB
hsB
hsB
hsB
iyB
iyB
iyB
hsB
iyB
gmB
hsB
k�B
l�B
m�B
m�B
l�B
k�B
l�B
m�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
n�B
o�B
o�B
n�B
m�B
m�B
o�B
q�B
q�B
q�B
s�B
s�B
r�B
r�B
t�B
s�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
v�B
u�B
t�B
w�B
y�B
z�B
z�B
y�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
{�B
{�B
|�B
{�B
z�B
y�B
|�B
|�B
{�B
{�B
|�B
~�B
}�B
}�B
|�B
|�B
}�B
}�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
|�B
{�B
{�B
|�B
~�B
}�B
{�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�+B
�%B
�%B
�B
�B
�%B
�+B
�%B
�%B
�B
�B
�B
�+B
�+B
�+B
�+B
�1B
�+B
�%B
�%B
�B
�B
�1B
�1B
�=B
�=B
�7B
�7B
�=B
�7B
�=B
�DB
�=B
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�DB
�JB
�JB
�JB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�JB
�PB
�PB
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
�VB
�VB
�VB
�PB
�VB
�VB
�PB
�VB
�VB
�VB
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�bB
�bB
�\B
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�\B
�PB
�\B
�PB
�\B
�bB
�hB
�bB
�hB
�oB
�oB
�hB
�hB
�bB
�hB
�oB
�oB
�oB
�oB
�uB
�oB
�uB
�{B
�uB
�{B
�{B
�{B
�{B
�{B
�uB
�uB
�oB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	8RB	8RB	8RB	8RB	8RB	7LB	7LB	8RB	9XB	8RB	5?B	?}B	K�B	M�B	T�B	W
B	XB	\)B	`BB	bNB	hsB	k�B	jB	q�B	p�B	k�B	hsB	�B	�B
(�B
o�B
��B�B9XBaHBw�BbNBJB9XB�B;dBW
BP�B1'B�B�B
�B  B
�B
�#B
�9B
��B
� B
%�B
oB
D�B
6FB	��B	�ZB	�/B	�RB	�B	�B	��B	iyB	W
B	S�B	,B	!�B	�B	=qB	T�B	hsB	R�B	=qB	��B	��B	ƨB	�B	�BB	�ZB	�`B	�fB	�yB	�B
{B
5?B
I�B
Q�B
W
B
YB
�%B
�hB
��B
��B
�'B
�3B
�FB
�jB
ȴB
ǮB
B
ŢB
ŢB
��B
�jB
�^B
�wB
�qB
��B
B
ĜB
ƨB
ǮB
ŢB
��B
ĜB
��B
�wB
ǮB
��B
ɺB
ɺB
��B
��B
��B
��B
ɺB
ǮB
��B
�wB
�dB
�9B
�FB
�dB
�dB
�jB
�^B
�XB
�FB
�LB
�FB
�-B
�-B
�'B
��B
�B
�3B
�!B
�!B
�!B
�-B
�B
�B
��B
��B
�!B
�9B
�-B
�9B
�-B
�3B
�-B
�B
��B
��B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
�+B
�%B
�hB
�\B
�VB
�hB
�=B
�7B
�7B
� B
x�B
�B
�B
|�B
}�B
~�B
z�B
~�B
|�B
|�B
{�B
|�B
�B
�B
� B
~�B
~�B
~�B
~�B
|�B
{�B
y�B
w�B
u�B
n�B
gmB
_;B
cTB
m�B
n�B
p�B
o�B
n�B
k�B
l�B
iyB
ffB
dZB
_;B
W
B
[#B
W
B
W
B
W
B
P�B
Q�B
Q�B
N�B
N�B
S�B
S�B
R�B
P�B
H�B
A�B
H�B
R�B
R�B
R�B
S�B
Q�B
Q�B
Q�B
P�B
M�B
L�B
L�B
G�B
A�B
G�B
P�B
O�B
O�B
M�B
L�B
J�B
H�B
D�B
;dB
:^B
9XB
33B
8RB
2-B
9XB
7LB
49B
8RB
7LB
9XB
8RB
9XB
7LB
33B
5?B
1'B
49B
/B
.B
-B
5?B
/B
+B
+B
'�B
+B
$�B
+B
(�B
)�B
)�B
&�B
,B
)�B
&�B
&�B
&�B
%�B
&�B
#�B
'�B
+B
+B
+B
(�B
%�B
!�B
�B
!�B
#�B
 �B
#�B
&�B
%�B
'�B
&�B
$�B
#�B
 �B
 �B
"�B
!�B
�B
�B
�B
�B
&�B
&�B
$�B
!�B
�B
�B
�B
 �B
%�B
%�B
#�B
!�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
oB
+B
VB
�B
�B
�B
�B
�B
{B
bB
VB
uB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
oB
oB
hB
�B
�B
�B
uB
�B
�B
uB
bB
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
bB
VB
�B
 �B
 �B
!�B
$�B
&�B
'�B
&�B
'�B
'�B
&�B
%�B
%�B
%�B
#�B
"�B
$�B
 �B
!�B
'�B
&�B
%�B
"�B
"�B
$�B
&�B
'�B
+B
)�B
(�B
(�B
)�B
+B
,B
'�B
'�B
&�B
&�B
'�B
(�B
(�B
+B
-B
.B
/B
1'B
/B
1'B
1'B
2-B
33B
33B
1'B
2-B
1'B
1'B
49B
6FB
49B
49B
33B
33B
49B
5?B
7LB
6FB
5?B
6FB
8RB
7LB
6FB
49B
-B
/B
/B
2-B
-B
0!B
33B
6FB
8RB
8RB
7LB
7LB
7LB
9XB
:^B
9XB
7LB
49B
2-B
33B
49B
8RB
;dB
;dB
<jB
<jB
<jB
;dB
9XB
;dB
9XB
<jB
;dB
<jB
>wB
?}B
>wB
<jB
8RB
9XB
;dB
>wB
?}B
<jB
?}B
?}B
@�B
A�B
@�B
?}B
B�B
A�B
A�B
E�B
D�B
E�B
C�B
D�B
E�B
F�B
E�B
G�B
H�B
J�B
J�B
I�B
G�B
D�B
E�B
C�B
G�B
H�B
F�B
H�B
H�B
L�B
M�B
L�B
I�B
H�B
H�B
K�B
M�B
L�B
M�B
P�B
P�B
O�B
L�B
K�B
M�B
M�B
N�B
P�B
N�B
P�B
R�B
R�B
Q�B
P�B
O�B
Q�B
P�B
Q�B
R�B
P�B
Q�B
T�B
R�B
O�B
P�B
Q�B
VB
VB
T�B
T�B
VB
XB
XB
W
B
VB
W
B
ZB
ZB
ZB
YB
YB
YB
ZB
\)B
[#B
[#B
ZB
XB
T�B
S�B
XB
ZB
ZB
YB
W
B
YB
\)B
[#B
[#B
YB
[#B
\)B
\)B
\)B
^5B
]/B
_;B
_;B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
]/B
\)B
\)B
]/B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
^5B
]/B
_;B
bNB
aHB
`BB
aHB
cTB
bNB
aHB
dZB
dZB
e`B
e`B
dZB
cTB
bNB
aHB
aHB
aHB
_;B
]/B
cTB
e`B
ffB
dZB
gmB
iyB
jB
jB
iyB
hsB
hsB
hsB
iyB
iyB
iyB
hsB
iyB
gmB
hsB
k�B
l�B
m�B
m�B
l�B
k�B
l�B
m�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
n�B
o�B
o�B
n�B
m�B
m�B
o�B
q�B
q�B
q�B
s�B
s�B
r�B
r�B
t�B
s�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
v�B
u�B
t�B
w�B
y�B
z�B
z�B
y�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
{�B
{�B
|�B
{�B
z�B
y�B
|�B
|�B
{�B
{�B
|�B
~�B
}�B
}�B
|�B
|�B
}�B
}�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
|�B
{�B
{�B
|�B
~�B
}�B
{�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�+B
�%B
�%B
�B
�B
�%B
�+B
�%B
�%B
�B
�B
�B
�+B
�+B
�+B
�+B
�1B
�+B
�%B
�%B
�B
�B
�1B
�1B
�=B
�=B
�7B
�7B
�=B
�7B
�=B
�DB
�=B
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�DB
�JB
�JB
�JB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�JB
�PB
�PB
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
�VB
�VB
�VB
�PB
�VB
�VB
�PB
�VB
�VB
�VB
�VB
�\B
�VB
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�bB
�bB
�\B
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�\B
�PB
�\B
�PB
�\B
�bB
�hB
�bB
�hB
�oB
�oB
�hB
�hB
�bB
�hB
�oB
�oB
�oB
�oB
�uB
�oB
�uB
�{B
�uB
�{B
�{B
�{B
�{B
�{B
�uB
�uB
�oB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220307100245                              AO  ARCAADJP                                                                    20220307100245    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220307100245  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220307100245  QCF$                G�O�G�O�G�O�4000            
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-02-11T19:01:38Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
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
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      
_FillValue               conventions       Argo reference table 23          7�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    7�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >�E�vQ�        7�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    7�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        7�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9    PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9$   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9(   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9,   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  90   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  Lh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  o    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �p   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    
_FillValue               conventions       YYYYMMDDHHMISS        ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     ��   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     �         �Argo profile    3.1 1.2 19500101000000  20170211190138  20181103100346  5904055 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               lA   AO  5004                            2C  D   NAVIS_A                         863 @��[,�+�1   @��[�Y.@6����m�e|�hs1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      lA   A   A   @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�fD�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�G�@�G�A ��A ��A@��A`��A
=A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C
=C
=C

=C
=C
=C
=C
=C
=C
=C
=C
=C
=C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Co�Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��{D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��{D��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�{D�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��{D�HD�AHD�HD��HD�HD�AHD��HD��HD��D�'�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��#A��A���A���A��A���A���A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A���A���A���A���A���A���A�A�A�1A�
=A�JA�VA�VA�bA�bA�bA�bA�bA�bA�VA�
=A�1A�A���A���A��`A���Aư!A�jA�9XA���A�G�A�r�A�JA�bNA�p�A�-A�-A�r�A��A���A��/A�p�A�v�A��!A�r�A���A�S�A�XA���A��/A��jA�`BA��9A�S�A���A��TA��RA�bNA���A��DA���A�33A�5?A�r�A�^5A�v�A���A�|�A�7LA���A�=qA���A���A��`A���A�1'A���A�M�A�ZA��-A��TA�5?A��HA�"�A��7A�\)A�ZA�oA��PA�ȴA�t�A�5?A���A�=qA��A��`A�oA�E�A~$�A|�A{/Az�AyK�AwAw`BAu��Aq�An�Am�Ak�TAi�Ah�Ahz�Ah �AgS�Af9XAd�Ad5?AcK�Ab �A`=qA_/A^��A]|�A\(�A[��AZ~�AY��AWS�AVĜAV  AU
=AT(�ASx�AR��AR�AR��AQ��AQ33AP��AO�TANv�AM�7ALjAKXAI�
AIXAH��AG\)AF��AE�wAC�AB�HABbAA�A@�\A?O�A>I�A=K�A<��A<1A9�
A7�A7l�A6�/A5ƨA4��A3C�A1�;A0�!A0Q�A/��A.��A-�
A-O�A,�DA,-A+��A*��A(VA&�A%t�A$�HA$z�A#�A!��A ffA 1At�A�yA�RA(�A�PA�9A�A+AZA~�AG�A �AK�A�`A�jA�+AM�A$�AoA�+AA�AdZA�+A�7A�!A��Av�AƨA
(�A	�A	G�A��A1AO�A�
A{At�A�9A�TA r�@�-@�V@� �@�$�@���@���@�w@�ȴ@�@���@�~�@���@�u@�h@�@旍@�-@���@�h@�A�@�|�@�;d@�33@��@��T@���@��@׾w@ղ-@�hs@��@Դ9@�(�@�b@��@Ӆ@��@�?}@υ@�33@�ff@��#@�Ĝ@�1@�\)@ʧ�@Ɂ@���@�  @�
=@ƸR@Ə\@�=q@��@� �@��H@��7@�z�@�K�@��y@��!@��\@��@��`@�bN@�1'@��
@���@�p�@�/@�%@���@���@��m@�+@��+@�-@��T@�G�@��@���@�Ĝ@�Z@�-@��@�V@��@�Z@�9X@�(�@��@��m@��P@�"�@�n�@���@�`B@�V@���@���@�X@�7L@�V@���@�Ĝ@�Z@�9X@�1'@� �@�ƨ@�dZ@�33@��y@�M�@��^@�z�@�@��h@�%@�A�@��m@��@�\)@�dZ@�|�@��@���@��\@�/@���@��@�dZ@��!@�~�@�v�@�-@���@�O�@�V@� �@�dZ@��\@�@���@���@�p�@�X@�/@��`@�Ĝ@�r�@�C�@�V@�{@��T@���@�hs@�/@�%@���@�r�@�Q�@�(�@���@�\)@�\)@�t�@��P@���@��@�+@�ȴ@��\@�ff@�@��#@���@��^@�`B@��`@�z�@� �@��m@��F@�;d@��@�~�@��@�@��-@���@�9X@��@�dZ@�@�@���@��y@���@�ȴ@��@��y@�ȴ@��\@�n�@�M�@�$�@��T@���@��@���@�v�@�ff@��-@��/@�Q�@��m@��
@���@�r�@�Z@�b@��;@��F@�ƨ@��F@��@���@�l�@�C�@��R@�^5@�n�@�M�@�J@��T@���@���@��j@� �@�@��@~�y@~ff@}�T@}?}@|��@}V@}V@}V@|�@|�j@|9X@|z�@|�D@|�D@|�D@|�D@|�D@|�D@|�D@{�
@z�@z�H@z��@yx�@y7L@y7L@{33@{�m@}?}@}�@}�-@}�-@}�-@}�@}?}@}�@|�j@|I�@|(�@|1@{�F@{"�@z�\@y�#@yhs@x��@xbN@w�@wl�@v�R@v5?@v{@u�T@u��@u��@u�h@u/@t��@t�j@t(�@s�
@s��@s�@sdZ@r�\@qG�@pĜ@pQ�@o�w@o;d@nv�@m@m��@m�@m?}@l�@l(�@kƨ@kC�@j��@j~�@j-@i�#@i&�@h�u@hQ�@g�;@g�@g\)@f�@f�+@f�+@f�+@fV@eO�@c�m@ct�@c33@c"�@b�!@bJ@a��@`r�@_��@_�@_|�@^��@]��@\�@[�m@[�F@[33@Z��@Z�\@Z^5@Y�@Y�^@Y7L@X��@X�u@XA�@W�;@W;d@Vȴ@Vv�@V5?@V$�@V{@V{@V{@U�T@U�h@Up�@U�@T�@T��@T�D@TZ@S��@S33@So@R�!@Rn�@R^5@R^5@Q��@Q��@Q�7@Q&�@P��@PQ�@P �@O�w@O��@O��@O\)@N�R@Nff@NV@Mp�@M/@MV@L�j@L(�@Kƨ@K��@K"�@J�H@J�!@J=q@J-@I�^@I�7@I7L@H�`@H �@G�P@G|�@Gl�@G�@F��@F�y@FE�@F@E��@E�T@E�T@E�T@E�@F@F@E��@Ep�@E?}@E/@EV@D�j@Cƨ@CC�@B�H@B=q@B=q@B-@BJ@A�@A�7@A�@@Ĝ@@bN@?K�@?
=@>�@>�R@>v�@>5?@=��@=@=�@=O�@=?}@=�@=V@=V@=V@<�@<�/@<��@<�j@<��@<j@<Z@<I�@<9X@<9X@<(�@;��@;�m@;�
@;�
@;S�@;"�@;o@:�H@:~�@:^5@9��@9��@97L@8��@8A�@81'@8b@8  @7��@7�@7��@7|�@7K�@7+@7
=@6�@6ȴ@6ȴ@6��@6��@6�+@6v�@6$�@5�@5��@5/@4��@4�@4�@4�D@4z�@3�m@3dZ@3C�@3o@3@2��@2��@2^5@1�@1��@1X@17L@0��@0��@0�9@0�@01'@0 �@0b@0  @/��@/�P@/l�@/\)@/+@.�@.�R@.v�@-�T@-�h@,��@,�@,I�@+�@+C�@+33@+"�@+o@*�H@*~�@)��@)hs@(bN@( �@'�;@'�w@'�@'K�@&�y@&�@&��@&@%@%�-@%�@%`B@%V@$��@$�D@$Z@$(�@#�
@#��@#dZ@#C�@#o@"�\@!�@!�7@!hs@!G�@!G�@!7L@!7L@!&�@!&�@!�@ ��@ ��@ ��@ �u@ b@�w@�w@�w@�@|�@|�@K�@ȴ@$�@��@�h@p�@��@�D@I�@1@ƨ@�@C�@@�H@~�@=q@�#@�#@�^@��@hs@��@�@r�@bN@bN@Q�@  @��@l�@K�@;d@�@��@�R@��@V@E�@5?@{@�@�T@�-@�@p�@`B@O�@?}@?}@�@��@�@��@�D@�D@j@�@��@��@��@t�@�H@�!@�\@M�@=q@��@hs@&�@%@Ĝ@�u@bN@Q�@A�@  @l�@ȴ@ff@@�-@�h@p�@?}@�@��@�/@�j@��@z�@Z@I�@9X@�@�m@�F@��@33@
�@
��@
��@
�!@
n�@
n�@
n�@
n�@
=q@
-@
-@
�@
�@
�@
J@	��@	��@	��@	�@	��@	x�@	&�@	%@	%@��@�`@Ĝ@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A��#A��A���A���A��A���A���A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A���A���A���A���A���A���A�A�A�1A�
=A�JA�VA�VA�bA�bA�bA�bA�bA�bA�VA�
=A�1A�A���A���A��`A���Aư!A�jA�9XA���A�G�A�r�A�JA�bNA�p�A�-A�-A�r�A��A���A��/A�p�A�v�A��!A�r�A���A�S�A�XA���A��/A��jA�`BA��9A�S�A���A��TA��RA�bNA���A��DA���A�33A�5?A�r�A�^5A�v�A���A�|�A�7LA���A�=qA���A���A��`A���A�1'A���A�M�A�ZA��-A��TA�5?A��HA�"�A��7A�\)A�ZA�oA��PA�ȴA�t�A�5?A���A�=qA��A��`A�oA�E�A~$�A|�A{/Az�AyK�AwAw`BAu��Aq�An�Am�Ak�TAi�Ah�Ahz�Ah �AgS�Af9XAd�Ad5?AcK�Ab �A`=qA_/A^��A]|�A\(�A[��AZ~�AY��AWS�AVĜAV  AU
=AT(�ASx�AR��AR�AR��AQ��AQ33AP��AO�TANv�AM�7ALjAKXAI�
AIXAH��AG\)AF��AE�wAC�AB�HABbAA�A@�\A?O�A>I�A=K�A<��A<1A9�
A7�A7l�A6�/A5ƨA4��A3C�A1�;A0�!A0Q�A/��A.��A-�
A-O�A,�DA,-A+��A*��A(VA&�A%t�A$�HA$z�A#�A!��A ffA 1At�A�yA�RA(�A�PA�9A�A+AZA~�AG�A �AK�A�`A�jA�+AM�A$�AoA�+AA�AdZA�+A�7A�!A��Av�AƨA
(�A	�A	G�A��A1AO�A�
A{At�A�9A�TA r�@�-@�V@� �@�$�@���@���@�w@�ȴ@�@���@�~�@���@�u@�h@�@旍@�-@���@�h@�A�@�|�@�;d@�33@��@��T@���@��@׾w@ղ-@�hs@��@Դ9@�(�@�b@��@Ӆ@��@�?}@υ@�33@�ff@��#@�Ĝ@�1@�\)@ʧ�@Ɂ@���@�  @�
=@ƸR@Ə\@�=q@��@� �@��H@��7@�z�@�K�@��y@��!@��\@��@��`@�bN@�1'@��
@���@�p�@�/@�%@���@���@��m@�+@��+@�-@��T@�G�@��@���@�Ĝ@�Z@�-@��@�V@��@�Z@�9X@�(�@��@��m@��P@�"�@�n�@���@�`B@�V@���@���@�X@�7L@�V@���@�Ĝ@�Z@�9X@�1'@� �@�ƨ@�dZ@�33@��y@�M�@��^@�z�@�@��h@�%@�A�@��m@��@�\)@�dZ@�|�@��@���@��\@�/@���@��@�dZ@��!@�~�@�v�@�-@���@�O�@�V@� �@�dZ@��\@�@���@���@�p�@�X@�/@��`@�Ĝ@�r�@�C�@�V@�{@��T@���@�hs@�/@�%@���@�r�@�Q�@�(�@���@�\)@�\)@�t�@��P@���@��@�+@�ȴ@��\@�ff@�@��#@���@��^@�`B@��`@�z�@� �@��m@��F@�;d@��@�~�@��@�@��-@���@�9X@��@�dZ@�@�@���@��y@���@�ȴ@��@��y@�ȴ@��\@�n�@�M�@�$�@��T@���@��@���@�v�@�ff@��-@��/@�Q�@��m@��
@���@�r�@�Z@�b@��;@��F@�ƨ@��F@��@���@�l�@�C�@��R@�^5@�n�@�M�@�J@��T@���@���@��j@� �@�@��@~�y@~ff@}�T@}?}@|��@}V@}V@}V@|�@|�j@|9X@|z�@|�D@|�D@|�D@|�D@|�D@|�D@|�D@{�
@z�@z�H@z��@yx�@y7L@y7L@{33@{�m@}?}@}�@}�-@}�-@}�-@}�@}?}@}�@|�j@|I�@|(�@|1@{�F@{"�@z�\@y�#@yhs@x��@xbN@w�@wl�@v�R@v5?@v{@u�T@u��@u��@u�h@u/@t��@t�j@t(�@s�
@s��@s�@sdZ@r�\@qG�@pĜ@pQ�@o�w@o;d@nv�@m@m��@m�@m?}@l�@l(�@kƨ@kC�@j��@j~�@j-@i�#@i&�@h�u@hQ�@g�;@g�@g\)@f�@f�+@f�+@f�+@fV@eO�@c�m@ct�@c33@c"�@b�!@bJ@a��@`r�@_��@_�@_|�@^��@]��@\�@[�m@[�F@[33@Z��@Z�\@Z^5@Y�@Y�^@Y7L@X��@X�u@XA�@W�;@W;d@Vȴ@Vv�@V5?@V$�@V{@V{@V{@U�T@U�h@Up�@U�@T�@T��@T�D@TZ@S��@S33@So@R�!@Rn�@R^5@R^5@Q��@Q��@Q�7@Q&�@P��@PQ�@P �@O�w@O��@O��@O\)@N�R@Nff@NV@Mp�@M/@MV@L�j@L(�@Kƨ@K��@K"�@J�H@J�!@J=q@J-@I�^@I�7@I7L@H�`@H �@G�P@G|�@Gl�@G�@F��@F�y@FE�@F@E��@E�T@E�T@E�T@E�@F@F@E��@Ep�@E?}@E/@EV@D�j@Cƨ@CC�@B�H@B=q@B=q@B-@BJ@A�@A�7@A�@@Ĝ@@bN@?K�@?
=@>�@>�R@>v�@>5?@=��@=@=�@=O�@=?}@=�@=V@=V@=V@<�@<�/@<��@<�j@<��@<j@<Z@<I�@<9X@<9X@<(�@;��@;�m@;�
@;�
@;S�@;"�@;o@:�H@:~�@:^5@9��@9��@97L@8��@8A�@81'@8b@8  @7��@7�@7��@7|�@7K�@7+@7
=@6�@6ȴ@6ȴ@6��@6��@6�+@6v�@6$�@5�@5��@5/@4��@4�@4�@4�D@4z�@3�m@3dZ@3C�@3o@3@2��@2��@2^5@1�@1��@1X@17L@0��@0��@0�9@0�@01'@0 �@0b@0  @/��@/�P@/l�@/\)@/+@.�@.�R@.v�@-�T@-�h@,��@,�@,I�@+�@+C�@+33@+"�@+o@*�H@*~�@)��@)hs@(bN@( �@'�;@'�w@'�@'K�@&�y@&�@&��@&@%@%�-@%�@%`B@%V@$��@$�D@$Z@$(�@#�
@#��@#dZ@#C�@#o@"�\@!�@!�7@!hs@!G�@!G�@!7L@!7L@!&�@!&�@!�@ ��@ ��@ ��@ �u@ b@�w@�w@�w@�@|�@|�@K�@ȴ@$�@��@�h@p�@��@�D@I�@1@ƨ@�@C�@@�H@~�@=q@�#@�#@�^@��@hs@��@�@r�@bN@bN@Q�@  @��@l�@K�@;d@�@��@�R@��@V@E�@5?@{@�@�T@�-@�@p�@`B@O�@?}@?}@�@��@�@��@�D@�D@j@�@��@��@��@t�@�H@�!@�\@M�@=q@��@hs@&�@%@Ĝ@�u@bN@Q�@A�@  @l�@ȴ@ff@@�-@�h@p�@?}@�@��@�/@�j@��@z�@Z@I�@9X@�@�m@�F@��@33@
�@
��@
��@
�!@
n�@
n�@
n�@
n�@
=q@
-@
-@
�@
�@
�@
J@	��@	��@	��@	�@	��@	x�@	&�@	%@	%@��@�`@Ĝ@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB&�B&�B&�B%�B$�B$�B#�B$�B$�B$�B%�B'�B)�B)�B(�B(�B(�B-B-B)�B(�B,B6FB9XB;dB<jB<jB>wBB�BD�BF�BF�BH�BH�BH�BH�BI�BI�BJ�BK�BK�BL�BN�BP�BR�BVBVB[#BcTBffBu�B�B�{B�RB��B��B��B�HB�yB�`B�B�yB�`B�sB�sB�/B��BÖB�dB�RB�jB�^B�LB�'B�B��B��B��B�{B~�Br�BhsBdZB\)BQ�BD�B7LB�BuBVB1BB  B��B�TBŢB�jB�FB��B�bB�Bt�B^5BQ�BH�BB�B8RB.B�BB
��B
��B
�yB
�HB
�B
��B
B
�LB
�B
��B
��B
�DB
�B
w�B
s�B
m�B
e`B
aHB
R�B
.B
�B
\B
B	�B	�B	�sB	�`B	�HB	�5B	�)B	�B	�B	��B	��B	ĜB	�wB	�?B	�B	�B	��B	��B	��B	��B	�{B	�hB	�PB	�7B	�+B	�%B	�B	�B	}�B	z�B	t�B	m�B	gmB	bNB	\)B	T�B	Q�B	L�B	F�B	C�B	=qB	6FB	2-B	.B	+B	%�B	�B	�B	�B	uB	\B	%B��B��B��B��B�B�yB�NB�B�#B�B�
B�
B�
B��B��B��BȴB�qB�9B�B�B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�7B�%B�B�B�B�B� B� B~�B|�B{�B{�By�By�Bx�Bw�Bx�By�Bw�Bv�Bw�Bw�Bv�Bu�Bu�Bt�Bo�Bn�Bt�Bu�Bt�Bs�Br�Br�Bq�Bp�Bo�Bm�Bm�Bl�Bl�Bk�Bl�Bk�BiyBhsBhsBhsBhsBffBhsBiyBiyBjBiyBhsBhsBiyBiyBk�Bn�Bn�Bn�Bn�Bo�Bo�Bn�Bn�Bp�Br�Bv�Bu�Bu�Bu�Bw�Bx�By�Bz�B~�B~�B�B�B�%B�%B�%B�7B�DB�PB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�9B�FB�LB�XB�^B�^B�dB�dB�dB�dB�jB��BÖBĜBĜB��B��B��B��B��B��B�B�
B�B�B�B�#B�B�)B�5B�NB�ZB�`B�B�B�B�B�B��B��B��B��B��B	B	B	B	+B	
=B	PB	hB	oB	hB	oB	�B	�B	�B	�B	#�B	+B	2-B	33B	33B	5?B	5?B	6FB	7LB	7LB	8RB	;dB	?}B	C�B	E�B	G�B	K�B	N�B	Q�B	S�B	VB	VB	T�B	YB	_;B	cTB	e`B	ffB	ffB	jB	n�B	n�B	n�B	n�B	o�B	q�B	t�B	v�B	x�B	y�B	y�B	{�B	z�B	y�B	z�B	z�B	z�B	{�B	{�B	{�B	�B	�+B	�1B	�\B	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�3B	�-B	�!B	�'B	�3B	�FB	�jB	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ƨB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�HB	�TB	�`B	�ZB	�mB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B
DB
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
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
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
#�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
'�B
'�B
(�B
+B
+B
+B
+B
+B
+B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
-B
-B
-B
.B
/B
0!B
0!B
1'B
2-B
33B
49B
49B
49B
49B
49B
6FB
7LB
7LB
7LB
6FB
7LB
8RB
8RB
8RB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
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
M�B
M�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
W
B
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
YB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
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
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
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
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B&�B&�B&�B%�B$�B$�B#�B$�B$�B$�B%�B'�B)�B)�B(�B(�B(�B-	B-B)�B(�B,B6FB9ZB;aB<[B<dB>`BB�BD�BF�BF�BH�BH�BH�BH�BI�BI�BJ�BK�BK�BL�BN�BP�BSBU�BVQB[�Bc�BgjBvzB��B��B�fB�6B�aB�B��B�7B��B��B�NB�B��B��B�*B�4BʟB�sB��B��B�B�sB��B�,B��B��B�B��B��By�Bj�BgFB`BV�BHDB?�B!KBGB.B	=B�B^B��B�BȿB��B��B��B�4B�B{�Ba�BT�BJBE�B:�B3�B%}BsB SB
�ZB
�B
�B
��B
ͶB
�*B
��B
�fB
�B
��B
�{B
�DB
yEB
w�B
q�B
f�B
f�B
_8B
4�B
�B
rB
^B	��B	�B	�B	�B	�IB	�fB	�eB	ېB	�>B	��B	ϋB	��B	�tB	��B	��B	��B	�QB	�%B	�B	��B	��B	��B	�B	�B	�\B	�;B	��B	��B	~�B	|�B	w�B	o�B	i�B	d�B	_EB	VB	S�B	O�B	H3B	E�B	BB	8B	3�B	/HB	-B	(�B	!�B	�B	B	�B	bB	
jB	 /B�MB�IB��B�B��B��B�B��B�hB��B�CB��B��B�TBђB�yB�8B��B��B�B��B��B�B��B�=B�B�!B�B�QB��B�\B��B��B��B��B�}B��B�.B��B��B��B�B�:B}�B|�B|�B|�B{�Bz�B| B}cBzB{�By�Bx�Bx�Bw�BxByBt�Bp+Bv�Bw�Bw�Bv�Bs�Bs�Bs�Bq�Bq�BqqBn�Bm�Bn�BmHBm\BmBmBj�Bi�Bi�Bi�Bj�BjBjwBi�Bj�Bi�Bi�Bk_Bl;Bl�BnNBn�Bo	Bo!Bo]Bo�Bo�Bo9Bo�BsBu
BwEBv�Bv�BwHBx�By�Bz�B|B�B�B�YB��B�]B��B��B�~B��B�B��B� B��B��B��B�[B��B�QB��B�(B�B�kB�,B�B�B�B��B��B��B�{B�qB��B��B�AB�!B��B�yB��B��B��B��B�B�lB�sB��B��B��B�PB�VB�B�BǬB�B�AB�B�/B�B�HBևB�1B�B�0BُBۤB�^BܐB�B�B�B�B�"B�UB�B�#B�7B��B��B��B�\B�XB	ZB	�B	�B	B	
�B	AB	�B	B	�B	(B	B	�B	�B	�B	$�B	,B	2IB	3<B	3sB	5]B	5vB	6�B	7wB	7�B	9�B	<�B	?�B	C�B	E�B	G�B	LB	OB	RBB	TpB	V,B	V?B	UB	Y�B	_3B	c(B	e7B	fVB	fGB	k9B	o/B	n�B	n�B	o)B	o�B	q�B	t�B	wCB	yoB	zVB	z;B	| B	{B	zhB	{2B	{eB	{�B	|B	{�B	|�B	��B	�SB	�B	��B	�kB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�.B	��B	�B	��B	�nB	�KB	�1B	�MB	��B	��B	�7B	��B	��B	��B	��B	��B	��B	ăB	ŲB	ƮB	��B	��B	��B	�rB	�B	ŇB	��B	�B	��B	��B	��B	�B	ɍB	��B	��B	�cB	�'B	�*B	�?B	�B	��B	��B	��B	��B	�B	�QB	��B	�B	�&B	�*B	�*B	�&B	�(B	�5B	޿B	��B	�RB	�B	�BB	�B	�RB	��B	�B	�B	��B	��B	��B	��B	��B	�	B	��B	�)B	�:B	�B	�B	�5B	�sB
 zB
�B
bB
fB
�B
dB
B
�B
�B
CB
OB
[B
	B
uB
�B
tB
�B
�B
�B
~B
uB
|B
B
hB
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&B
B
�B
�B
�B
�B
B
�B
�B
�B
�B
 �B
 �B
 B
�B
 �B
!	B
#0B
$B
$�B
$;B
#�B
#�B
$]B
#uB
"FB
"dB
!�B
"B
#B
"�B
"�B
#	B
#�B
$B
#�B
&B
&B
'B
&?B
'*B
'B
'B
'�B
'�B
(�B
'�B
(B
)&B
+B
+3B
+B
+B
+-B
+$B
*{B
*CB
*B
*=B
+1B
,B
,B
,MB
,,B
-;B
-VB
-oB
.LB
/>B
0fB
05B
1$B
2aB
3�B
4rB
4HB
4�B
4fB
4LB
6�B
7�B
7�B
7fB
6�B
7}B
8wB
8�B
8\B
7�B
7rB
7�B
7�B
8�B
8�B
8XB
9cB
9�B
9nB
9hB
9�B
9�B
:|B
:JB
;`B
;`B
;QB
;SB
;cB
;�B
;�B
<�B
<rB
<�B
<�B
=%B
<�B
<�B
<�B
<dB
<pB
<}B
<�B
<�B
<�B
<�B
=�B
>KB
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
AB
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J$B
KB
J�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
MB
L�B
M�B
M�B
M�B
M�B
OB
N�B
N�B
N�B
N�B
OB
O�B
O�B
PB
PB
P�B
QB
QRB
Q#B
RTB
R(B
R9B
S}B
T&B
S�B
TB
T B
TB
TGB
TYB
UqB
U�B
W=B
W7B
WB
WB
WWB
WSB
XB
W?B
W~B
X?B
Y B
Y9B
Y1B
YVB
YHB
YIB
Y;B
YBB
YXB
YGB
Z@B
Z6B
ZJB
Z�B
Z�B
[mB
\CB
\<B
\%B
\4B
]&B
]5B
](B
]5B
]EB
]HB
]TB
]=B
]�B
]kB
^0B
_8B
_GB
__B
_;B
_jB
_�B
_�B
_{B
_mB
_XB
_�B
_�B
`vB
`yB
`sB
`xB
`|B
`vB
`\B
`�B
aB
a�B
bLB
bgB
b]B
b�B
b�B
b�B
c_B
c]B
cOB
cfB
c�B
d�B
d�B
dqB
ejB
exB
eyB
e�B
f�B
f�B
foB
fnB
f~B
f|B
gxB
g�B
g�B
gvB
gyB
gzB
h|B
hlB
h�B
h�B
h�B
h{B
h{B
hrB
h�B
h�B
i�B
iB
itB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
lB
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
nB
o*B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
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
v�B
v�B
wB
w B
w�B
w�B
w�B
w�B
w�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<_B�<#�
<#�
<&|�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5^<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<M/�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.04 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310904242018103109042420181031090424  0303                            082713                          AO  ARCAADJP                                                                    20170211190138    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170211190138  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170211190138  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031090424  QC  PRES            @�  D�&fG�O�                PM  ARSQCTM V1.1                                                                20181031090424  QC  PSAL            @�  D�&fG�O�                PM  ARSQOWGUV1.0                                                                20181103100346  IP                  G�O�G�O�G�O�                
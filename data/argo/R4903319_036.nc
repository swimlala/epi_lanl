CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-04-04T11:00:34Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210404110034  20210404110034  4903319 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               $A   AO  8280                            2B  A   NAVIS_A                         1159                            170425                          863 @�j�<�
1   @�j��}56@8x���F�c��`A�71   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         $A   A   A   @���@���@���A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DK��DLy�DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@��@��A!p�AAp�A_�
A�
A��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C0�C
C

C
C
C
C
C
C
C
C
C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CM�pCO�pCR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK]DK�]DL]DM�DM��DN�DN��DO�DO]DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D�D���D��D�B�DÂ�D���D��D�B�DĂ�D���D��D�B�Dł�D���D��D�B�DƂ�D���D��D�B�Dǂ�D���D��D�B�DȂ�D���D��D�B�Dɂ�D���D��D�B�Dʂ�D���D��D�B�D˂�D���D��D�B�D̂�D���D��D�B�D͂�D���D��D�B�D΂�D���D��D�B�Dς�D���D��D�B�DЂ�D���D��D�B�Dт�D���D��D�B�D҂�D���D��D�B�Dӂ�D���D��D�B�DԂ�D���D��D�B�DՂ�D���D��D�B�Dւ�D���D��D�B�Dׂ�D���D��D�B�D؂�D���D��D�B�Dق�D���D��D�B�Dڂ�D���D��D�B�Dۂ�D���D��D�B�D܂�D���D��D�B�D݂�D���D��D�B�Dނ�D���D��D�B�D߂�D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D�D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D��D���D��D�B�D���D���D��D�B�D���D���D�D�B�D���D���D��D�B�D���D���D��D�B�D���D���D��D�B�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�I�A�O�A�A�A�VA�K�A�K�A�ZA�ZA�S�A�S�A�S�A�VA�Q�A�O�A�I�A�C�A�7LA�&�A��A��A�\)A��#A�dZA�(�A��A��TA���A���A��uA�r�A�I�A�(�A��A���A�K�A���A��PA�hsA��A��mA���A���A�S�A���A�~�A�(�A��A���A� �A���A�oA�ZA���A���A�
=A�|�A��A��A�jA�G�A�  A���A�O�A���A���A�p�A���A��-A�?}A�O�A��jA�ĜA�O�A�"�A�1A�r�A�`BA���A��7A���A��A�A��PA�(�A�&�A���A��wA��A�bNA���A�Q�A��FA���A�x�A��mA�%A��A�hsA�{A�?}A��#A��FA~v�A{�Ay?}Ax1'Aw?}Av��Au�7Aq�Amt�Al(�AhĜAd1'Ab��AbQ�Aa�A`��A]�mAY�mAY��AYl�AX��AW�ASƨAR�AP�AP��AO"�AN9XAN$�AM��AL��AKl�AJ�HAI��AH�AGdZAF^5AD�HABv�A@�A??}A>(�A=�FA=hsA<��A;�mA:bA9/A7C�A6 �A4  A2�DA0��A0jA/�FA.�A.9XA-��A-G�A,�A,$�A+&�A)��A)A'�A&�A%hsA$�+A#�^A#�A"A�A!��A bAdZA��AA|�A�\AK�AbNA�A�DAbNAbA�A�;A33A�A�yA+A�!A-A��A%A�A�A�A
ZA	�PA	7LA	
=AM�A��A�HA�A�A�A �A�A�A/A V@�@�@�z�@��@��!@��T@��@�I�@�l�@���@���@�&�@��@� �@�ȴ@�j@�V@�ff@��@��@�
=@�n�@�J@ߥ�@�^5@�@��@��T@��#@݁@��@ܣ�@��
@ڇ+@�hs@ج@�bN@�9X@�1@׾w@�
=@֟�@�?}@��;@��@Ӿw@�o@�n�@ҧ�@�J@Ѳ-@�@мj@��@�o@�7L@�\)@�V@ɲ-@ȴ9@Ɨ�@�~�@�ff@�ff@�G�@ļj@���@�^5@�&�@�r�@��P@�E�@��T@��@�x�@��@��j@��@���@�~�@�^5@��-@�j@��!@�hs@�bN@��
@�C�@�n�@�hs@���@�I�@���@��w@���@��@��@���@�E�@�{@���@���@��@�x�@���@��
@��@�n�@�O�@�/@��9@�1@�|�@�;d@�V@���@�O�@���@���@��@���@���@�bN@� �@��m@�dZ@�
=@���@�@�x�@�`B@��@��@��D@�Q�@���@��
@��w@��P@�dZ@�5?@��^@�O�@���@���@�I�@�b@��@��m@��
@���@�33@�-@�/@��9@���@�j@�b@��
@��F@�dZ@�
=@��y@��@��@���@��R@��+@�5?@�@�X@���@�(�@�t�@�33@��@��@��!@��+@�V@�-@���@��^@���@�G�@���@�Ĝ@�Z@�1'@� �@�  @���@��m@��;@��;@��
@���@��P@�l�@�33@��H@���@�5?@�J@��@���@�&�@���@�Ĝ@���@�j@�bN@�9X@��m@��@��R@�^5@�=q@��@��^@���@�hs@�Ĝ@�bN@�A�@�(�@��@�ƨ@�\)@�;d@�+@�o@��@��@���@�ȴ@��!@�~�@�^5@�E�@�5?@�{@��@��@��#@���@�@���@�hs@�?}@�V@��j@�bN@�A�@�  @
=@~$�@}O�@|��@|��@|Z@{�m@{t�@{C�@{"�@z�H@z�!@z��@zM�@yx�@yX@y%@xĜ@xb@w�w@wK�@v�R@vff@vE�@v5?@v5?@u�@u�@t�D@tI�@s��@s"�@r��@rJ@qG�@pĜ@pbN@pb@pb@o�@oK�@n�y@n$�@m��@m@m�h@m/@l��@l�@lz�@k�m@k�F@k��@k33@k@j�@j�H@j�\@j^5@j-@i��@i��@ihs@i7L@i%@h��@g�;@g�w@g��@f�@e��@e�@d�@b�H@b-@a��@aG�@a7L@a&�@a�@`Ĝ@`bN@`Q�@_�w@_�P@_+@^��@^�R@^$�@]��@]�-@\�/@\9X@\1@[�F@[��@[��@[�@[t�@[C�@Z��@ZM�@Z-@Y��@Y�#@Y��@YX@Y�@X��@W�;@W��@W|�@W|�@W+@V�@V�+@V$�@U@Up�@UO�@UO�@U/@T�@T�j@Tz�@Sƨ@SS�@S@R�\@R�\@R^5@R=q@R=q@R=q@R=q@R-@R�@Q�@Q��@Q7L@Q�@P�u@P  @O|�@Ol�@OK�@O�@N�R@NE�@M�@M�-@M/@Lj@L�@Kƨ@KC�@J��@J=q@I��@I&�@H��@H�@HQ�@G�@G�@G\)@G�@G
=@F��@F�@F�@F�@Fȴ@F�R@Fv�@F@E�@E@E�h@E�@EO�@EO�@EO�@E/@EV@D�/@D9X@C�
@C��@C��@C�@CdZ@C"�@B�!@B-@A��@A�7@AG�@@��@@��@@�`@@��@@b@?\)@>�y@>�R@>ff@>@=��@=@=�-@=�h@=p�@<�/@<�@;��@;�F@;S�@;@:�!@:��@:�\@:~�@:~�@:~�@:n�@:=q@9�@9�#@9�#@9�#@9��@9hs@9%@8�@8 �@8  @7�;@7�@7|�@7+@6V@5�@4�D@3��@3��@3�m@3ƨ@3��@2n�@1�#@1�7@1x�@1hs@1hs@1G�@1&�@1�@1%@1%@1%@0��@0��@0�u@/�@/�@/��@/l�@/K�@.��@.�y@.��@-@-�@-`B@-/@,��@,��@,Z@,(�@+�m@+��@+dZ@+C�@+"�@*��@*n�@*-@)��@)�#@)��@)hs@)&�@(��@(��@( �@'�@';d@&�@&�R@&�+@&E�@&E�@&@%�T@%@%�-@%��@%O�@$��@$z�@$9X@$(�@$1@#�F@#dZ@#o@"��@"��@"�!@"�!@"�!@"�\@"n�@"=q@!��@!�^@!��@!��@!��@!��@!�7@!X@!�@ ��@ ��@ r�@ Q�@ Q�@ A�@ 1'@  �@ b@ b@�@��@�P@�y@�R@�+@v�@ff@ff@ff@V@E�@$�@{@@�T@��@@�h@`B@�@V@��@�/@��@j@�
@t�@S�@33@"�@o@@��@~�@J@�#@��@�7@x�@x�@x�@x�@x�@x�@hs@X@&�@Ĝ@Ĝ@Ĝ@�u@r�@bN@Q�@1'@  @;d@
=@��@ȴ@ff@5?@$�@{@�-@O�@�@j@9X@�m@ƨ@��@��@dZ@33@�!@^5@^5@M�@=q@J@��@hs@G�@G�@X@&�@��@�`@�9@bN@Q�@ �@��@\)@K�@��@�@�R@��@ff@5?@@��@@��@�@p�@?}@V@��@��@�j@Z@(�@1@��@�m@�
@�F@��@��@t�@t�@t�@�@�@dZ@33@"�@o@@
�@
�H@
��@
-@	�#@	�^@	G�@	%@��@Ĝ@��@�u@�u@�u@r�@1'@ �@  @��@|�@+@
=@�@
=@��@�R@�+@v�@ff@ff@ff@E�@E�@5?@{@�@@p�@O�@/@V@�@��@�j@j@9X@(�@�@�m@ƨ@��@t�@dZ@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�C�A�I�A�O�A�A�A�VA�K�A�K�A�ZA�ZA�S�A�S�A�S�A�VA�Q�A�O�A�I�A�C�A�7LA�&�A��A��A�\)A��#A�dZA�(�A��A��TA���A���A��uA�r�A�I�A�(�A��A���A�K�A���A��PA�hsA��A��mA���A���A�S�A���A�~�A�(�A��A���A� �A���A�oA�ZA���A���A�
=A�|�A��A��A�jA�G�A�  A���A�O�A���A���A�p�A���A��-A�?}A�O�A��jA�ĜA�O�A�"�A�1A�r�A�`BA���A��7A���A��A�A��PA�(�A�&�A���A��wA��A�bNA���A�Q�A��FA���A�x�A��mA�%A��A�hsA�{A�?}A��#A��FA~v�A{�Ay?}Ax1'Aw?}Av��Au�7Aq�Amt�Al(�AhĜAd1'Ab��AbQ�Aa�A`��A]�mAY�mAY��AYl�AX��AW�ASƨAR�AP�AP��AO"�AN9XAN$�AM��AL��AKl�AJ�HAI��AH�AGdZAF^5AD�HABv�A@�A??}A>(�A=�FA=hsA<��A;�mA:bA9/A7C�A6 �A4  A2�DA0��A0jA/�FA.�A.9XA-��A-G�A,�A,$�A+&�A)��A)A'�A&�A%hsA$�+A#�^A#�A"A�A!��A bAdZA��AA|�A�\AK�AbNA�A�DAbNAbA�A�;A33A�A�yA+A�!A-A��A%A�A�A�A
ZA	�PA	7LA	
=AM�A��A�HA�A�A�A �A�A�A/A V@�@�@�z�@��@��!@��T@��@�I�@�l�@���@���@�&�@��@� �@�ȴ@�j@�V@�ff@��@��@�
=@�n�@�J@ߥ�@�^5@�@��@��T@��#@݁@��@ܣ�@��
@ڇ+@�hs@ج@�bN@�9X@�1@׾w@�
=@֟�@�?}@��;@��@Ӿw@�o@�n�@ҧ�@�J@Ѳ-@�@мj@��@�o@�7L@�\)@�V@ɲ-@ȴ9@Ɨ�@�~�@�ff@�ff@�G�@ļj@���@�^5@�&�@�r�@��P@�E�@��T@��@�x�@��@��j@��@���@�~�@�^5@��-@�j@��!@�hs@�bN@��
@�C�@�n�@�hs@���@�I�@���@��w@���@��@��@���@�E�@�{@���@���@��@�x�@���@��
@��@�n�@�O�@�/@��9@�1@�|�@�;d@�V@���@�O�@���@���@��@���@���@�bN@� �@��m@�dZ@�
=@���@�@�x�@�`B@��@��@��D@�Q�@���@��
@��w@��P@�dZ@�5?@��^@�O�@���@���@�I�@�b@��@��m@��
@���@�33@�-@�/@��9@���@�j@�b@��
@��F@�dZ@�
=@��y@��@��@���@��R@��+@�5?@�@�X@���@�(�@�t�@�33@��@��@��!@��+@�V@�-@���@��^@���@�G�@���@�Ĝ@�Z@�1'@� �@�  @���@��m@��;@��;@��
@���@��P@�l�@�33@��H@���@�5?@�J@��@���@�&�@���@�Ĝ@���@�j@�bN@�9X@��m@��@��R@�^5@�=q@��@��^@���@�hs@�Ĝ@�bN@�A�@�(�@��@�ƨ@�\)@�;d@�+@�o@��@��@���@�ȴ@��!@�~�@�^5@�E�@�5?@�{@��@��@��#@���@�@���@�hs@�?}@�V@��j@�bN@�A�@�  @
=@~$�@}O�@|��@|��@|Z@{�m@{t�@{C�@{"�@z�H@z�!@z��@zM�@yx�@yX@y%@xĜ@xb@w�w@wK�@v�R@vff@vE�@v5?@v5?@u�@u�@t�D@tI�@s��@s"�@r��@rJ@qG�@pĜ@pbN@pb@pb@o�@oK�@n�y@n$�@m��@m@m�h@m/@l��@l�@lz�@k�m@k�F@k��@k33@k@j�@j�H@j�\@j^5@j-@i��@i��@ihs@i7L@i%@h��@g�;@g�w@g��@f�@e��@e�@d�@b�H@b-@a��@aG�@a7L@a&�@a�@`Ĝ@`bN@`Q�@_�w@_�P@_+@^��@^�R@^$�@]��@]�-@\�/@\9X@\1@[�F@[��@[��@[�@[t�@[C�@Z��@ZM�@Z-@Y��@Y�#@Y��@YX@Y�@X��@W�;@W��@W|�@W|�@W+@V�@V�+@V$�@U@Up�@UO�@UO�@U/@T�@T�j@Tz�@Sƨ@SS�@S@R�\@R�\@R^5@R=q@R=q@R=q@R=q@R-@R�@Q�@Q��@Q7L@Q�@P�u@P  @O|�@Ol�@OK�@O�@N�R@NE�@M�@M�-@M/@Lj@L�@Kƨ@KC�@J��@J=q@I��@I&�@H��@H�@HQ�@G�@G�@G\)@G�@G
=@F��@F�@F�@F�@Fȴ@F�R@Fv�@F@E�@E@E�h@E�@EO�@EO�@EO�@E/@EV@D�/@D9X@C�
@C��@C��@C�@CdZ@C"�@B�!@B-@A��@A�7@AG�@@��@@��@@�`@@��@@b@?\)@>�y@>�R@>ff@>@=��@=@=�-@=�h@=p�@<�/@<�@;��@;�F@;S�@;@:�!@:��@:�\@:~�@:~�@:~�@:n�@:=q@9�@9�#@9�#@9�#@9��@9hs@9%@8�@8 �@8  @7�;@7�@7|�@7+@6V@5�@4�D@3��@3��@3�m@3ƨ@3��@2n�@1�#@1�7@1x�@1hs@1hs@1G�@1&�@1�@1%@1%@1%@0��@0��@0�u@/�@/�@/��@/l�@/K�@.��@.�y@.��@-@-�@-`B@-/@,��@,��@,Z@,(�@+�m@+��@+dZ@+C�@+"�@*��@*n�@*-@)��@)�#@)��@)hs@)&�@(��@(��@( �@'�@';d@&�@&�R@&�+@&E�@&E�@&@%�T@%@%�-@%��@%O�@$��@$z�@$9X@$(�@$1@#�F@#dZ@#o@"��@"��@"�!@"�!@"�!@"�\@"n�@"=q@!��@!�^@!��@!��@!��@!��@!�7@!X@!�@ ��@ ��@ r�@ Q�@ Q�@ A�@ 1'@  �@ b@ b@�@��@�P@�y@�R@�+@v�@ff@ff@ff@V@E�@$�@{@@�T@��@@�h@`B@�@V@��@�/@��@j@�
@t�@S�@33@"�@o@@��@~�@J@�#@��@�7@x�@x�@x�@x�@x�@x�@hs@X@&�@Ĝ@Ĝ@Ĝ@�u@r�@bN@Q�@1'@  @;d@
=@��@ȴ@ff@5?@$�@{@�-@O�@�@j@9X@�m@ƨ@��@��@dZ@33@�!@^5@^5@M�@=q@J@��@hs@G�@G�@X@&�@��@�`@�9@bN@Q�@ �@��@\)@K�@��@�@�R@��@ff@5?@@��@@��@�@p�@?}@V@��@��@�j@Z@(�@1@��@�m@�
@�F@��@��@t�@t�@t�@�@�@dZ@33@"�@o@@
�@
�H@
��@
-@	�#@	�^@	G�@	%@��@Ĝ@��@�u@�u@�u@r�@1'@ �@  @��@|�@+@
=@�@
=@��@�R@�+@v�@ff@ff@ff@E�@E�@5?@{@�@@p�@O�@/@V@�@��@�j@j@9X@(�@�@�m@ƨ@��@t�@dZ@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B%�B'�B2-BN�B\)BVBP�BP�BR�BXB_;BaHBffBk�Bo�Bu�B{�B�+B�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��Bt�BS�BI�B;dB6FB1'B$�B��B�B�yB�fB�`B�`B�;B�#B��B��BɺBƨB��B�-B��B�JB|�Bu�Bo�Bk�BdZBXB<jB/B�BPB
��B
��B
�B
�ZB
�;B
�
B
��B
�B
��B
��B
�oB
{�B
\)B
@�B
,B
�B
PB
B	��B	�B	�B	�dB	�!B	��B	~�B	t�B	q�B	l�B	e`B	W
B	=qB	<jB	:^B	5?B	-B	�B	uB	VB	VB	
=B	B	B	  B��B�B�B�B�mB�HB�)B��B��BŢB�}B�^B�RB�LB�?B�3B�B��B��B��B��B��B��B�{B�oB�bB�VB�JB�DB�=B�1B�B� B|�By�Bu�Bs�Bn�Bk�BhsBe`BbNB`BB\)B[#BXBVBT�BP�BO�BJ�BI�BH�BF�BG�BC�BA�B>wB<jB8RB6FB5?B33B33B/B-B+B,B.B,B+B(�B&�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBoBoBhBbB\BJBhB\BJBDBDBDBDB	7BDB
=B
=B
=B
=B
=B
=B
=B	7B
=B
=B
=BDBDBDB
=B
=BDB
=BJB\B{B�B�B�B�B�B!�B%�B2-B33B5?B8RB7LB49B1'B0!B.B-B-B1'B49B33B33B5?B:^B=qB@�BC�BF�BH�BP�BQ�BP�BQ�BVBYB]/BbNBbNBcTBdZBbNBdZBhsBk�Bo�Br�Bt�Bu�Bu�Bv�Bz�B�B� B�B�B�B�B�%B�1B�\B�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�LB�dB�wB�wB��B��BĜBĜBƨBǮBǮBȴBȴB��B��B��B�B�B�B�)B�)B�/B�/B�5B�HB�fB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	%B	DB	\B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	(�B	-B	/B	0!B	7LB	:^B	:^B	<jB	<jB	=qB	=qB	>wB	>wB	C�B	D�B	E�B	F�B	I�B	M�B	P�B	R�B	R�B	T�B	ZB	]/B	^5B	`BB	aHB	aHB	bNB	dZB	ffB	k�B	m�B	n�B	p�B	r�B	r�B	s�B	u�B	x�B	x�B	y�B	y�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�1B	�7B	�=B	�DB	�PB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�9B	�?B	�FB	�XB	�^B	�dB	�qB	��B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�;B	�NB	�TB	�TB	�`B	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
0!B
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
2-B
2-B
33B
2-B
2-B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
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
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
S�B
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
VB
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
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
dZB
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
gmB
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
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
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
l�B
l�B
l�B
m�B
m�B
m�B
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
q�B
q�B
r�B
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
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B%�B'�B2-BN�B\)BVBP�BP�BR�BXB_;BaHBffBk�Bo�Bu�B{�B�+B�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��Bt�BS�BI�B;dB6FB1'B$�B��B�B�yB�fB�`B�`B�;B�#B��B��BɺBƨB��B�-B��B�JB|�Bu�Bo�Bk�BdZBXB<jB/B�BPB
��B
��B
�B
�ZB
�;B
�
B
��B
�B
��B
��B
�oB
{�B
\)B
@�B
,B
�B
PB
B	��B	�B	�B	�dB	�!B	��B	~�B	t�B	q�B	l�B	e`B	W
B	=qB	<jB	:^B	5?B	-B	�B	uB	VB	VB	
=B	B	B	  B��B�B�B�B�mB�HB�)B��B��BŢB�}B�^B�RB�LB�?B�3B�B��B��B��B��B��B��B�{B�oB�bB�VB�JB�DB�=B�1B�B� B|�By�Bu�Bs�Bn�Bk�BhsBe`BbNB`BB\)B[#BXBVBT�BP�BO�BJ�BI�BH�BF�BG�BC�BA�B>wB<jB8RB6FB5?B33B33B/B-B+B,B.B,B+B(�B&�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBoBoBhBbB\BJBhB\BJBDBDBDBDB	7BDB
=B
=B
=B
=B
=B
=B
=B	7B
=B
=B
=BDBDBDB
=B
=BDB
=BJB\B{B�B�B�B�B�B!�B%�B2-B33B5?B8RB7LB49B1'B0!B.B-B-B1'B49B33B33B5?B:^B=qB@�BC�BF�BH�BP�BQ�BP�BQ�BVBYB]/BbNBbNBcTBdZBbNBdZBhsBk�Bo�Br�Bt�Bu�Bu�Bv�Bz�B�B� B�B�B�B�B�%B�1B�\B�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�9B�LB�dB�wB�wB��B��BĜBĜBƨBǮBǮBȴBȴB��B��B��B�B�B�B�)B�)B�/B�/B�5B�HB�fB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	%B	DB	\B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	(�B	-B	/B	0!B	7LB	:^B	:^B	<jB	<jB	=qB	=qB	>wB	>wB	C�B	D�B	E�B	F�B	I�B	M�B	P�B	R�B	R�B	T�B	ZB	]/B	^5B	`BB	aHB	aHB	bNB	dZB	ffB	k�B	m�B	n�B	p�B	r�B	r�B	s�B	u�B	x�B	x�B	y�B	y�B	{�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�1B	�7B	�=B	�DB	�PB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�9B	�?B	�FB	�XB	�^B	�dB	�qB	��B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�;B	�NB	�TB	�TB	�`B	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
0!B
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
2-B
2-B
33B
2-B
2-B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
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
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
S�B
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
VB
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
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
dZB
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
gmB
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
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
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
l�B
l�B
l�B
m�B
m�B
m�B
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
q�B
q�B
r�B
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
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210404110034                              AO  ARCAADJP                                                                    20210404110034    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210404110034  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210404110034  QCF$                G�O�G�O�G�O�0               
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-07-05T18:38:54Z creation;2020-07-05T18:38:57Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20200705183854  20200705185517  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               QA   JA                                  2B  A   APEX                            7906                            051216                          846 @�&d#Eg�1   @�&g����@3mV��dH�n��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C 33C"  C$  C&  C'�fC)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D �fD!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DIy�DI��DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dt��Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�C3D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D���D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�<�D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @P��@�Q�@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�G�A�{A�{A�{A�{B
=B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B�Q�B�Q�B�Q�B�B��B�B��B��RC B�CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C u�C"B�C$B�C&B�C((�C*(�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CV\)CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�.C�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D
>D�>D�D��D�D��D�D��D�D��D�D�
D�D��D�D��D �D �
D!
D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'�>D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>�>D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD
>DD��DE�DE��DF�DF��DG�DG��DH�DH�
DI�DI�>DJ
>DJ��DK�DK�>DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ
DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da
>Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�>Du
>Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~�
D�D��D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�D�ED��RD��RD�RD�K�D��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�K�D��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD���D�˅D�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD���D��RD�RD�HRD��RD��RD�RD�K�D��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD���D��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD���D��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRDRD��RD�RD�HRDÈRD��RD�RD�HRDĈRD��RD�RD�HRDňRD��RD�RD�HRDƈRD��RD�RD�HRDǈRD��RD�RD�HRDȈRD��RD�RD�HRDɈRD��RD�RD�HRDʈRD��RD�RD�HRDˈRD��RD�RD�HRD̈RD��RD�RD�K�D͈RD��RD�RD�HRDΈRD��RD�RD�HRDψRD��RD�D�HRDЈRD��RD�RD�HRDшRD��RD�RD�HRD҈RD��RD�RD�EDӈRD��RD�RD�HRDԈRD��RD�RD�HRDՈRD��RD�RD�HRDֈRD��RD�RD�HRD׈RD��RD�RD�HRD؈RD��RD�RD�HRDوRD��RD�RD�HRDڈRD��RD�RD�HRDۈRD��RD�RD�HRD܈RD��RD�RD�HRD݈RD��RD�RD�HRDވRD��RD�RD�HRD߈RD��RD�RD�HRD��RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��D�RD�HRD�RD��RD�RD�HRD�RD��RD�D�HRD�RD��RD�RD�HRD�RD��RD�RD�ED�RD��RD�RD�HRD�D��RD�RD�HRD�RD��RD�D�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD��RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD�RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD�RD�HRD��RD��RD��D�HRD��RD��RD�RD�ED��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��A��`A��`A��TA��/A��`A��yA��mA��TAڧ�Aډ7A�t�A�hsA�`BA�A�Aى7A�E�A���A���A؇+A�O�A�/Aײ-A��
A�$�A��TAոRA�=qA�M�A��;Aӗ�A�S�Aҧ�A��;A�  A�VA��A�~�A�t�A��#A�-Aʡ�A�`BA�+A��/Aɧ�A�z�A�`BA�{A���A�x�AǸRA�`BA�{A�l�A��A�p�A��Ać+A�+A���A�ȴA�bNA�%Aº^A�jA���A��FA���A�E�A��wA��A�M�A�ffA�  A��A�l�A�oA�9XA�oA�?}A�"�A���A��A��A�C�A��A���A��A��A���A��/A�7LA���A��!A���A��TA���A���A�&�A���A�A��A�;dA��wA��`A��uA�7LA�=qA���A�v�A�A��#A���A��uA��A��A�ffA��A���A��A���A���A�dZA�oA�x�A��wA�{A}XA{�AwdZAt  Ap-Al�AjZAi�PAf1'Ad9XAc7LAbjAa�Aa`BA_
=A\$�AXr�AT�AR��AQ&�AP�!AO�mAO��AO?}AMt�ALI�AJJAH�\AG�mAGl�AG
=AFbNAC�AB �A@�\A>�A=;dA;x�A9+A8{A7�A5�A49XA2A�A0ȴA.^5A,�jA+�A)�
A(r�A&�yA%;dA$��A$1A"ffA I�A�;A��AdZA��A33A��A�#Ax�A7LA�AO�A&�A�A1Al�A��A�hA�yA�9An�A��A��A�^AE�A��A�mA�A
��A	��A�A��Ap�A�DA��A/A�`A��An�A  An�@�|�@���@���@�&�@���@�@�V@���@�@�V@�Z@��@�-@�Q�@�5?@�/@�Z@߾w@�|�@�33@޸R@�v�@��@݉7@ܬ@�dZ@��`@�ƨ@׾w@�ȴ@֏\@Ցh@�ƨ@�
=@�n�@��@�`B@�7L@Гu@�9X@ϕ�@�K�@ύP@�j@�X@щ7@с@У�@�z�@�I�@�I�@�Q�@� �@�l�@θR@�V@�@���@��@͑h@̓u@��
@˕�@�dZ@�+@�v�@�p�@ȼj@�1@��@Ƈ+@�^5@��T@ř�@��@Ĭ@ă@�9X@�dZ@��@§�@�ff@��@��@��@�(�@���@��@�dZ@��H@�n�@���@��/@�Q�@��P@�Q�@�b@�@�V@��@���@��`@�I�@��F@���@�K�@�l�@��P@�K�@�S�@�K�@�;d@�5?@���@�(�@�K�@��\@�^5@���@�A�@�C�@�C�@�t�@��F@�^5@��@��y@��\@�M�@��@��T@���@�`B@�&�@���@��@�Q�@��F@��y@��#@��^@�G�@��@���@��@�  @�|�@�5?@��@��#@���@�@���@��@�p�@�`B@�?}@�%@��`@��@�(�@��@��y@��7@�?}@��@��9@�1'@���@��w@���@�dZ@�^5@���@�{@�E�@�M�@�5?@�E�@���@�\)@�t�@�C�@�dZ@�+@���@�-@�@��@�@��^@��7@�p�@�hs@�`B@�G�@���@�  @��;@��F@��P@�"�@��@���@�~�@�E�@���@�`B@�G�@��@�V@��j@�r�@�1'@��m@�t�@�n�@�@���@�Q�@��m@���@�S�@�
=@�E�@�{@���@��T@���@��-@�`B@��@�Ĝ@��@�Z@�(�@�ƨ@�t�@�dZ@�C�@���@���@���@��+@�n�@�E�@��@���@�x�@�?}@�V@���@���@���@���@�j@�(�@���@��w@��@���@�l�@��@���@��y@��@��!@���@�^5@��@��^@�hs@��@��@���@�(�@��@�t�@�|�@��P@�l�@��@���@�ȴ@�ȴ@��+@�$�@���@��h@���@�bN@�b@�ƨ@���@�l�@�\)@��@��R@��+@�=q@��-@�G�@��/@�z�@�9X@� �@�b@��m@��@�dZ@�33@��H@�n�@�5?@�$�@���@���@�p�@��@��@�Ĝ@���@�j@�Z@�Q�@�1'@�@\)@
=@~v�@~E�@~@}�-@}/@|z�@{�
@{�F@{��@{�@{S�@{C�@{33@z�H@z�!@z^5@y�@y�^@y�7@yhs@yG�@x��@xQ�@w�;@w+@v��@u�T@up�@t�/@s�m@st�@s@r�H@r�H@r�H@r��@r�!@rn�@r�@q�^@qG�@q%@pĜ@pr�@pA�@p  @o�w@o+@n��@n@mp�@l��@lZ@j��@j^5@j�@i��@i�#@iG�@i7L@h�u@h  @g��@g�w@g�@g��@g��@g�P@g
=@f�+@fv�@f5?@e��@eO�@d�/@dI�@cƨ@c�@cS�@c"�@b�@b��@b��@b^5@b�@bJ@a��@a��@a�7@aX@`Q�@_;d@^�@^�R@^��@^V@^{@]�@]��@]�h@]�@\z�@[�
@[��@[�@[dZ@[33@Z~�@Y�#@YX@Y&�@Y�@Y�@Y%@XbN@W�;@WK�@V�R@VE�@V@Up�@T��@T�D@T�@S�m@S��@St�@SC�@S33@S"�@So@R�@R��@Rn�@R�@Q��@Q��@QX@P �@O��@OK�@Nȴ@NE�@N@M�T@M�-@MO�@L�j@Lz�@Lj@LZ@L(�@K�m@K��@K�@Kt�@KC�@J��@J^5@I�#@IG�@H��@H�9@H�@HbN@H1'@H  @G�w@G\)@FE�@F@E�@EV@D��@DI�@C��@C�F@C��@C�@CdZ@C"�@B��@B~�@AX@@��@@Q�@@b@?�P@?�@>ȴ@>�R@>��@>�+@>�+@>�+@>�+@>��@>�+@>�+@>�+@>�+@>��@>�+@>v�@>ff@>{@=�h@<j@;dZ@:��@:M�@9�@9�^@9��@9x�@9x�@9x�@9X@97L@8��@8�`@8��@8��@8r�@7��@6��@6��@6�+@6��@6�+@6v�@6v�@6ff@6E�@6@5��@5�@4��@4(�@4(�@49X@4(�@4�@3�F@3S�@2�@2��@2��@2��@2n�@2^5@2M�@2M�@2�@2J@1�@1��@1�7@1X@17L@0�`@0Ĝ@0�9@0��@0�u@0�@01'@/�w@/\)@/+@/
=@.��@.ȴ@.�R@.��@.��@.�+@.v�@.ff@.E�@.$�@-�@-@-?}@,�/@,�@,�@,�D@+�
@+�F@+��@+�@+t�@+"�@*^5@*J@)�@)�#@)hs@)&�@(Ĝ@(��@(�u@(�u@(bN@(Q�@(1'@'�@'l�@'K�@';d@&�y@&ȴ@&��@&E�@%�T@%�-@%��@%`B@%/@%V@$�@$��@$�@$9X@#��@#S�@#o@"�H@"�!@"�\@"~�@"M�@"=q@!��@!��@!�7@!x�@!G�@!�@ ��@ �`@ Ĝ@ �9@ �9@ �@ r�@ 1'@ b@�;@�w@�@��@|�@+@�@ff@5?@{@{@�@@�-@��@�@O�@�@��@j@(�@��@�F@��@�@dZ@o@�H@��@�!@^5@��@��@G�@7L@�@��@�`@��@�9@�9@�@A�@1'@ �@b@  @�;@�w@\)@
=@ȴ@��@�+@�+@v�@�+@v�@E�@{@@��@��@��@�@�/@�j@��@z�@j@9X@(�@�@1@��@�m@ƨ@��@t�@dZ@dZ@dZ@S�@o@��@~�@M�@�@�#@�^@�7@X@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��A��`A��`A��TA��/A��`A��yA��mA��TAڧ�Aډ7A�t�A�hsA�`BA�A�Aى7A�E�A���A���A؇+A�O�A�/Aײ-A��
A�$�A��TAոRA�=qA�M�A��;Aӗ�A�S�Aҧ�A��;A�  A�VA��A�~�A�t�A��#A�-Aʡ�A�`BA�+A��/Aɧ�A�z�A�`BA�{A���A�x�AǸRA�`BA�{A�l�A��A�p�A��Ać+A�+A���A�ȴA�bNA�%Aº^A�jA���A��FA���A�E�A��wA��A�M�A�ffA�  A��A�l�A�oA�9XA�oA�?}A�"�A���A��A��A�C�A��A���A��A��A���A��/A�7LA���A��!A���A��TA���A���A�&�A���A�A��A�;dA��wA��`A��uA�7LA�=qA���A�v�A�A��#A���A��uA��A��A�ffA��A���A��A���A���A�dZA�oA�x�A��wA�{A}XA{�AwdZAt  Ap-Al�AjZAi�PAf1'Ad9XAc7LAbjAa�Aa`BA_
=A\$�AXr�AT�AR��AQ&�AP�!AO�mAO��AO?}AMt�ALI�AJJAH�\AG�mAGl�AG
=AFbNAC�AB �A@�\A>�A=;dA;x�A9+A8{A7�A5�A49XA2A�A0ȴA.^5A,�jA+�A)�
A(r�A&�yA%;dA$��A$1A"ffA I�A�;A��AdZA��A33A��A�#Ax�A7LA�AO�A&�A�A1Al�A��A�hA�yA�9An�A��A��A�^AE�A��A�mA�A
��A	��A�A��Ap�A�DA��A/A�`A��An�A  An�@�|�@���@���@�&�@���@�@�V@���@�@�V@�Z@��@�-@�Q�@�5?@�/@�Z@߾w@�|�@�33@޸R@�v�@��@݉7@ܬ@�dZ@��`@�ƨ@׾w@�ȴ@֏\@Ցh@�ƨ@�
=@�n�@��@�`B@�7L@Гu@�9X@ϕ�@�K�@ύP@�j@�X@щ7@с@У�@�z�@�I�@�I�@�Q�@� �@�l�@θR@�V@�@���@��@͑h@̓u@��
@˕�@�dZ@�+@�v�@�p�@ȼj@�1@��@Ƈ+@�^5@��T@ř�@��@Ĭ@ă@�9X@�dZ@��@§�@�ff@��@��@��@�(�@���@��@�dZ@��H@�n�@���@��/@�Q�@��P@�Q�@�b@�@�V@��@���@��`@�I�@��F@���@�K�@�l�@��P@�K�@�S�@�K�@�;d@�5?@���@�(�@�K�@��\@�^5@���@�A�@�C�@�C�@�t�@��F@�^5@��@��y@��\@�M�@��@��T@���@�`B@�&�@���@��@�Q�@��F@��y@��#@��^@�G�@��@���@��@�  @�|�@�5?@��@��#@���@�@���@��@�p�@�`B@�?}@�%@��`@��@�(�@��@��y@��7@�?}@��@��9@�1'@���@��w@���@�dZ@�^5@���@�{@�E�@�M�@�5?@�E�@���@�\)@�t�@�C�@�dZ@�+@���@�-@�@��@�@��^@��7@�p�@�hs@�`B@�G�@���@�  @��;@��F@��P@�"�@��@���@�~�@�E�@���@�`B@�G�@��@�V@��j@�r�@�1'@��m@�t�@�n�@�@���@�Q�@��m@���@�S�@�
=@�E�@�{@���@��T@���@��-@�`B@��@�Ĝ@��@�Z@�(�@�ƨ@�t�@�dZ@�C�@���@���@���@��+@�n�@�E�@��@���@�x�@�?}@�V@���@���@���@���@�j@�(�@���@��w@��@���@�l�@��@���@��y@��@��!@���@�^5@��@��^@�hs@��@��@���@�(�@��@�t�@�|�@��P@�l�@��@���@�ȴ@�ȴ@��+@�$�@���@��h@���@�bN@�b@�ƨ@���@�l�@�\)@��@��R@��+@�=q@��-@�G�@��/@�z�@�9X@� �@�b@��m@��@�dZ@�33@��H@�n�@�5?@�$�@���@���@�p�@��@��@�Ĝ@���@�j@�Z@�Q�@�1'@�@\)@
=@~v�@~E�@~@}�-@}/@|z�@{�
@{�F@{��@{�@{S�@{C�@{33@z�H@z�!@z^5@y�@y�^@y�7@yhs@yG�@x��@xQ�@w�;@w+@v��@u�T@up�@t�/@s�m@st�@s@r�H@r�H@r�H@r��@r�!@rn�@r�@q�^@qG�@q%@pĜ@pr�@pA�@p  @o�w@o+@n��@n@mp�@l��@lZ@j��@j^5@j�@i��@i�#@iG�@i7L@h�u@h  @g��@g�w@g�@g��@g��@g�P@g
=@f�+@fv�@f5?@e��@eO�@d�/@dI�@cƨ@c�@cS�@c"�@b�@b��@b��@b^5@b�@bJ@a��@a��@a�7@aX@`Q�@_;d@^�@^�R@^��@^V@^{@]�@]��@]�h@]�@\z�@[�
@[��@[�@[dZ@[33@Z~�@Y�#@YX@Y&�@Y�@Y�@Y%@XbN@W�;@WK�@V�R@VE�@V@Up�@T��@T�D@T�@S�m@S��@St�@SC�@S33@S"�@So@R�@R��@Rn�@R�@Q��@Q��@QX@P �@O��@OK�@Nȴ@NE�@N@M�T@M�-@MO�@L�j@Lz�@Lj@LZ@L(�@K�m@K��@K�@Kt�@KC�@J��@J^5@I�#@IG�@H��@H�9@H�@HbN@H1'@H  @G�w@G\)@FE�@F@E�@EV@D��@DI�@C��@C�F@C��@C�@CdZ@C"�@B��@B~�@AX@@��@@Q�@@b@?�P@?�@>ȴ@>�R@>��@>�+@>�+@>�+@>�+@>��@>�+@>�+@>�+@>�+@>��@>�+@>v�@>ff@>{@=�h@<j@;dZ@:��@:M�@9�@9�^@9��@9x�@9x�@9x�@9X@97L@8��@8�`@8��@8��@8r�@7��@6��@6��@6�+@6��@6�+@6v�@6v�@6ff@6E�@6@5��@5�@4��@4(�@4(�@49X@4(�@4�@3�F@3S�@2�@2��@2��@2��@2n�@2^5@2M�@2M�@2�@2J@1�@1��@1�7@1X@17L@0�`@0Ĝ@0�9@0��@0�u@0�@01'@/�w@/\)@/+@/
=@.��@.ȴ@.�R@.��@.��@.�+@.v�@.ff@.E�@.$�@-�@-@-?}@,�/@,�@,�@,�D@+�
@+�F@+��@+�@+t�@+"�@*^5@*J@)�@)�#@)hs@)&�@(Ĝ@(��@(�u@(�u@(bN@(Q�@(1'@'�@'l�@'K�@';d@&�y@&ȴ@&��@&E�@%�T@%�-@%��@%`B@%/@%V@$�@$��@$�@$9X@#��@#S�@#o@"�H@"�!@"�\@"~�@"M�@"=q@!��@!��@!�7@!x�@!G�@!�@ ��@ �`@ Ĝ@ �9@ �9@ �@ r�@ 1'@ b@�;@�w@�@��@|�@+@�@ff@5?@{@{@�@@�-@��@�@O�@�@��@j@(�@��@�F@��@�@dZ@o@�H@��@�!@^5@��@��@G�@7L@�@��@�`@��@�9@�9@�@A�@1'@ �@b@  @�;@�w@\)@
=@ȴ@��@�+@�+@v�@�+@v�@E�@{@@��@��@��@�@�/@�j@��@z�@j@9X@(�@�@1@��@�m@ƨ@��@t�@dZ@dZ@dZ@S�@o@��@~�@M�@�@�#@�^@�7@X@�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	YB	XB	XB	XB	XB	XB	XB	XB	XB	XB	XB	XB	W
B	W
B	VB	T�B	T�B	R�B	N�B	K�B	J�B	K�B	L�B	K�B	H�B	A�B	9XB	?}B	>wB	C�B	@�B	F�B	_;B	bNB	`BB	]/B	P�B	O�B	aHB	e`B	ffB	{�B	�PB	��B	�LB	��B	�/B	�B	��B
B
	7B
�B
'�B
8RB
P�B
ZB
`BB
k�B
p�B
w�B
~�B
�B
�7B
�=B
�DB
�=B
�=B
�bB
�uB
�uB
�VB
��B
��B
�wB
��B
�)B�B"�B%�BB�Bz�B�uB��B��B��B��B��B��B��B��B�B�-B�-B�3B�FB�LB��BĜBƨBƨB��B�`B�mB�fB�TBɺB�dB�^B��BÖB�qB�'B��B�DB~�Bs�BYB:^B �B�BoBB
�B
�ZB
��B
�XB
�B
��B
�%B
n�B
e`B
O�B
A�B
&�B
JB	��B	�BB	��B	��B	�jB	�B	��B	��B	��B	��B	��B	�%B	p�B	_;B	P�B	E�B	C�B	?}B	;dB	;dB	0!B	+B	"�B	�B	�B	oB	bB	JB	B��B�B�B�ZB�;B�B��B��B��BƨBB�qB�dB�FB�3B�B��B��B��B��B��B��B��B��B��B��B��B�uB�{B�oB�hB�\B��B�VB�PB�JB�=B�=B�7B�=B�7B�7B�7B�oB�uB�hB�VB�PB�PB�\B�oB�oB�uB�\B�VB�\B�\B�\B�\B�\B�bB�bB�uB�=B�7B�%B�B�7B�%B�1B�JB�VB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�FB�LB�dB��B��B��B��BBÖBŢBǮBɺB��B��B�B�5B�BB�TB�yB�B�B��B��B��B��B��B	B	B	%B	DB	hB	�B	#�B	$�B	&�B	'�B	,B	33B	7LB	@�B	F�B	J�B	K�B	M�B	O�B	Q�B	S�B	XB	YB	ZB	YB	YB	YB	ZB	\)B	^5B	aHB	dZB	ffB	hsB	jB	k�B	k�B	n�B	s�B	v�B	}�B	�B	�B	�+B	�JB	�JB	�=B	�=B	�+B	�1B	�DB	�VB	�hB	�hB	�{B	�{B	��B	��B	��B	��B	�{B	�hB	�bB	�VB	�7B	�B	�B	�7B	�JB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�!B	�'B	�'B	�'B	�'B	�-B	�3B	�3B	�3B	�9B	�?B	�?B	�FB	�9B	�3B	�-B	�?B	�?B	�?B	�FB	�LB	�LB	�LB	�dB	�wB	�}B	��B	ŢB	ȴB	ɺB	ɺB	��B	��B	�B	�B	�)B	�5B	�;B	�NB	�`B	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
oB
oB
uB
uB
uB
uB
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
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
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
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
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
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
_;B
_;B
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
aHB
bNB
bNB
bNB
bNB
bNB
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
iyB
iyB
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
jB
jB
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
p�B
q�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
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
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
�i11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	YB	XB	XB	XB	XB	XB	XB	XB	XB	XB	XB	XB	W
B	W
B	VB	T�B	T�B	R�B	N�B	K�B	J�B	K�B	L�B	K�B	H�B	A�B	9XB	?}B	>wB	C�B	@�B	F�B	_;B	bNB	`BB	]/B	P�B	O�B	aHB	e`B	ffB	{�B	�PB	��B	�LB	��B	�/B	�B	��B
B
	7B
�B
'�B
8RB
P�B
ZB
`BB
k�B
p�B
w�B
~�B
�B
�7B
�=B
�DB
�=B
�=B
�bB
�uB
�uB
�VB
��B
��B
�wB
��B
�)B�B"�B%�BB�Bz�B�uB��B��B��B��B��B��B��B��B�B�-B�-B�3B�FB�LB��BĜBƨBƨB��B�`B�mB�fB�TBɺB�dB�^B��BÖB�qB�'B��B�DB~�Bs�BYB:^B �B�BoBB
�B
�ZB
��B
�XB
�B
��B
�%B
n�B
e`B
O�B
A�B
&�B
JB	��B	�BB	��B	��B	�jB	�B	��B	��B	��B	��B	��B	�%B	p�B	_;B	P�B	E�B	C�B	?}B	;dB	;dB	0!B	+B	"�B	�B	�B	oB	bB	JB	B��B�B�B�ZB�;B�B��B��B��BƨBB�qB�dB�FB�3B�B��B��B��B��B��B��B��B��B��B��B��B�uB�{B�oB�hB�\B��B�VB�PB�JB�=B�=B�7B�=B�7B�7B�7B�oB�uB�hB�VB�PB�PB�\B�oB�oB�uB�\B�VB�\B�\B�\B�\B�\B�bB�bB�uB�=B�7B�%B�B�7B�%B�1B�JB�VB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�FB�LB�dB��B��B��B��BBÖBŢBǮBɺB��B��B�B�5B�BB�TB�yB�B�B��B��B��B��B��B	B	B	%B	DB	hB	�B	#�B	$�B	&�B	'�B	,B	33B	7LB	@�B	F�B	J�B	K�B	M�B	O�B	Q�B	S�B	XB	YB	ZB	YB	YB	YB	ZB	\)B	^5B	aHB	dZB	ffB	hsB	jB	k�B	k�B	n�B	s�B	v�B	}�B	�B	�B	�+B	�JB	�JB	�=B	�=B	�+B	�1B	�DB	�VB	�hB	�hB	�{B	�{B	��B	��B	��B	��B	�{B	�hB	�bB	�VB	�7B	�B	�B	�7B	�JB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�!B	�'B	�'B	�'B	�'B	�-B	�3B	�3B	�3B	�9B	�?B	�?B	�FB	�9B	�3B	�-B	�?B	�?B	�?B	�FB	�LB	�LB	�LB	�dB	�wB	�}B	��B	ŢB	ȴB	ɺB	ɺB	��B	��B	�B	�B	�)B	�5B	�;B	�NB	�`B	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
oB
oB
uB
uB
uB
uB
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
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
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
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
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
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
_;B
_;B
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
aHB
bNB
bNB
bNB
bNB
bNB
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
iyB
iyB
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
jB
jB
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
p�B
q�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
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
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
�i11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200706033838  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200705183854  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200705183855  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200705183855  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200705183856  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200705183856  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200705183856  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200705183856  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200705183857  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200705183857                      G�O�G�O�G�O�                JA  ARUP                                                                        20200705185517                      G�O�G�O�G�O�                
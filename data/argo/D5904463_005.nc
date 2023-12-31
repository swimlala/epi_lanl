CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:48Z AOML 3.0 creation; 2016-08-07T22:44:58Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150226221448  20160807154458  5904463 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5288_9005_005                   2C  D   APEX                            6530                            072314                          846 @�m}'�1   @�n"!��@)���E��cqV�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB��BffB��B��B(  B0  B8  B@  BHffBO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�fD� D�S3D�s3D�� D�fD�@ D�y�D���D�3D�C3D���D��fD���D�0 DچfD��fD��fD�0 D�Y�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @HQ�@�\)@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
BQ�B	�RBQ�B�RB�RB(�B0�B8�B@�BIQ�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�CT{C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�RDD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy�D�\D�Z�D�z�D��\D��D�G\D���D��)D��D�J�D��)D���D� �D�7\Dڍ�D���D���D�7\D�`�D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A�/A�5?A�;dA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�A�A�A�A�C�A�C�A�A�A�A�A�C�A���A���A���A�%A��HA�`BA�t�AƁA��A��;A�A��A���A��`A���A�M�A��A�jA�{A���A���A��A��hA�ffA��hA���A�-A�/A�9XA���A�n�A�bNA���A�$�A� �A��
A�^5A�ZA���A��FA�ZA���A�XA�$�A���A��#A�=qA�?}A�=qA�  A�A��^A���At�At��Aj��AiVAcG�A]/A[O�AX��AV-ASt�AQ�AO"�AI��AGXAFADbNAB�/AAC�A?��A?�A>1'A=;dA;C�A8��A7�A7O�A6ȴA6r�A6JA5dZA5&�A5x�A5�A5�^A6{A6bA5|�A3hsA1hsA0��A0E�A0VA0�DA133A2 �A1+A0�!A0ZA.��A,��A,�A(��A(jA'�A';dA&�9A&9XA%|�A%S�A%VA$��A#��A#7LA"�A"~�A!�A �A E�A $�A {A ^5A (�AhsA�;A&�A�A|�A�^A��A33A��A��AffA��A�wA�TAS�A"�A�HAn�AA�AJA|�AG�A�A��Av�A9XA1A\)Az�Al�A��Av�A�;A\)A;dA��A��A�9Ar�A1A&�A�HAv�AZA-A��AA�wA�FA��A��A��AbNAA�AƨA  A�A��A��A\)A7LA33A��A�\AI�AbA��A�hA+A�Av�A1A��A��A�AG�AVA
�A
E�A	;dA�jA�AI�A�#AG�A33A��A�TA�FA��Al�A��Ar�AA�A-A(�A1AdZA�HAr�A  AA`BAA �A M�@�ƨ@�"�@�=q@�/@�I�@�K�@���@�v�@��@���@�G�@��@��`@�Z@��m@��@���@�/@��9@�(�@�@�S�@�"�@���@�\@�^5@��@��#@�/@��;@��H@�n�@�5?@���@��`@���@��y@�J@�@陚@�?}@�j@� �@�@�K�@�n�@��@噚@�hs@�u@��
@�K�@���@�-@�X@�j@� �@�ƨ@�\)@��H@�^5@�$�@���@���@�(�@ۍP@���@ڇ+@��@��@ؼj@أ�@�Z@�1@��
@ץ�@�o@�^5@���@ղ-@�?}@�bN@���@��@��@с@�`B@�O�@�7L@���@�ƨ@�~�@ͺ^@���@̛�@�b@���@˅@�+@ʸR@��#@��@�1'@�  @ǅ@�
=@Ɵ�@��@��@Ĵ9@ċD@�bN@��m@Õ�@�C�@��@�~�@�V@�{@���@�p�@�&�@��j@��D@�A�@���@��@�^5@�{@���@���@��D@�  @�ƨ@���@�+@��@�v�@��@�x�@�7L@�%@��@�\)@�@���@�^5@�M�@�{@��^@��7@��@��@��u@�  @��;@�K�@���@��@�O�@��@�Z@�A�@�9X@� �@���@���@�t�@�
=@�v�@��@��#@���@�x�@�/@��u@�dZ@��H@�ȴ@��!@�n�@��@�hs@�%@��u@�1@��w@�\)@��@��R@�5?@��T@���@�O�@�?}@�&�@��@�Z@�A�@�1@���@���@�o@�ff@��@�?}@��`@��u@� �@���@�;d@��!@��+@�^5@�5?@��@���@��h@�&�@��@��@��@�dZ@�+@��@�ff@�J@��@�x�@���@���@�bN@��@��@�"�@���@�ȴ@���@�ff@�J@���@�(�@�J@�|�@��-@x�`@m@dI�@\1@Q��@K�F@D�D@?��@8Q�@1�@*��@"��@j@ȴ@@��@�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�"�A�/A�5?A�;dA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�A�A�A�A�C�A�C�A�A�A�A�A�C�A���A���A���A�%A��HA�`BA�t�AƁA��A��;A�A��A���A��`A���A�M�A��A�jA�{A���A���A��A��hA�ffA��hA���A�-A�/A�9XA���A�n�A�bNA���A�$�A� �A��
A�^5A�ZA���A��FA�ZA���A�XA�$�A���A��#A�=qA�?}A�=qA�  A�A��^A���At�At��Aj��AiVAcG�A]/A[O�AX��AV-ASt�AQ�AO"�AI��AGXAFADbNAB�/AAC�A?��A?�A>1'A=;dA;C�A8��A7�A7O�A6ȴA6r�A6JA5dZA5&�A5x�A5�A5�^A6{A6bA5|�A3hsA1hsA0��A0E�A0VA0�DA133A2 �A1+A0�!A0ZA.��A,��A,�A(��A(jA'�A';dA&�9A&9XA%|�A%S�A%VA$��A#��A#7LA"�A"~�A!�A �A E�A $�A {A ^5A (�AhsA�;A&�A�A|�A�^A��A33A��A��AffA��A�wA�TAS�A"�A�HAn�AA�AJA|�AG�A�A��Av�A9XA1A\)Az�Al�A��Av�A�;A\)A;dA��A��A�9Ar�A1A&�A�HAv�AZA-A��AA�wA�FA��A��A��AbNAA�AƨA  A�A��A��A\)A7LA33A��A�\AI�AbA��A�hA+A�Av�A1A��A��A�AG�AVA
�A
E�A	;dA�jA�AI�A�#AG�A33A��A�TA�FA��Al�A��Ar�AA�A-A(�A1AdZA�HAr�A  AA`BAA �A M�@�ƨ@�"�@�=q@�/@�I�@�K�@���@�v�@��@���@�G�@��@��`@�Z@��m@��@���@�/@��9@�(�@�@�S�@�"�@���@�\@�^5@��@��#@�/@��;@��H@�n�@�5?@���@��`@���@��y@�J@�@陚@�?}@�j@� �@�@�K�@�n�@��@噚@�hs@�u@��
@�K�@���@�-@�X@�j@� �@�ƨ@�\)@��H@�^5@�$�@���@���@�(�@ۍP@���@ڇ+@��@��@ؼj@أ�@�Z@�1@��
@ץ�@�o@�^5@���@ղ-@�?}@�bN@���@��@��@с@�`B@�O�@�7L@���@�ƨ@�~�@ͺ^@���@̛�@�b@���@˅@�+@ʸR@��#@��@�1'@�  @ǅ@�
=@Ɵ�@��@��@Ĵ9@ċD@�bN@��m@Õ�@�C�@��@�~�@�V@�{@���@�p�@�&�@��j@��D@�A�@���@��@�^5@�{@���@���@��D@�  @�ƨ@���@�+@��@�v�@��@�x�@�7L@�%@��@�\)@�@���@�^5@�M�@�{@��^@��7@��@��@��u@�  @��;@�K�@���@��@�O�@��@�Z@�A�@�9X@� �@���@���@�t�@�
=@�v�@��@��#@���@�x�@�/@��u@�dZ@��H@�ȴ@��!@�n�@��@�hs@�%@��u@�1@��w@�\)@��@��R@�5?@��T@���@�O�@�?}@�&�@��@�Z@�A�@�1@���@���@�o@�ff@��@�?}@��`@��u@� �@���@�;d@��!@��+@�^5@�5?@��@���@��h@�&�@��@��@��@�dZ@�+@��@�ff@�J@��@�x�@���@���@�bN@��@��@�"�@���@�ȴ@���@�ff@�JG�O�@�(�@�J@�|�@��-@x�`@m@dI�@\1@Q��@K�F@D�D@?��@8Q�@1�@*��@"��@j@ȴ@@��@�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�XB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�sB
�BF�Be`Bl�Bv�B�B�/BPB#�B/B-Bz�B��B��B�9B�'B��B��B��B��B�JBz�Bq�Bn�Bn�BhsBhsBbNBM�B;dB�B�B�HB��BȴBǮB��B�B�BjBT�BM�BT�B� BjB\)B?}B
��B
�^B
�\B
r�B
&�B	�B	�\B	P�B	H�B	-B	'�B	+B	/B	8RB	=qB	^5B	l�B	ZB	N�B	J�B	G�B	@�B	<jB	8RB	49B	2-B	1'B	2-B	D�B	O�B	W
B	gmB	�1B	��B	��B	��B	�3B	B	ɺB	�fB
1B
VB
B	��B	��B
+B
{B
�B
6FB
L�B
Q�B
T�B
P�B
E�B
?}B
E�B
7LB
2-B
1'B
1'B
5?B
>wB
H�B
J�B
K�B
M�B
R�B
M�B
J�B
G�B
B�B
F�B
D�B
H�B
M�B
YB
[#B
XB
L�B
H�B
I�B
W
B
]/B
\)B
ZB
[#B
YB
\)B
[#B
^5B
e`B
ffB
hsB
iyB
jB
jB
jB
m�B
m�B
o�B
n�B
k�B
iyB
iyB
hsB
cTB
ZB
VB
S�B
O�B
M�B
O�B
N�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
O�B
O�B
O�B
Q�B
T�B
VB
YB
\)B
ffB
n�B
m�B
k�B
q�B
u�B
t�B
u�B
t�B
t�B
s�B
s�B
q�B
q�B
p�B
o�B
n�B
m�B
k�B
jB
iyB
hsB
gmB
ffB
e`B
dZB
bNB
_;B
ZB
VB
VB
S�B
Q�B
O�B
N�B
L�B
H�B
H�B
G�B
F�B
E�B
D�B
C�B
C�B
B�B
B�B
B�B
A�B
@�B
?}B
=qB
<jB
:^B
9XB
9XB
8RB
7LB
6FB
5?B
33B
2-B
49B
5?B
5?B
5?B
49B
49B
33B
2-B
1'B
1'B
0!B
/B
/B
.B
.B
.B
.B
.B
.B
.B
.B
-B
.B
-B
-B
-B
,B
,B
)�B
'�B
&�B
&�B
%�B
&�B
%�B
&�B
&�B
&�B
%�B
%�B
%�B
$�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
uB
uB
oB
oB
uB
uB
oB
oB
oB
hB
\B
PB
JB
JB
JB
JB
PB
VB
PB
JB
DB
DB
PB
VB
VB
VB
PB
JB
VB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
hB
oB
hB
hB
bB
bB
\B
PB
JB
JB
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
bB
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
hB
bB
hB
hB
hB
hB
hB
hB
hB
uB
{B
{B
{B
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
"�B
"�B
(�B
1'B
6FB
=qB
D�B
J�B
N�B
S�B
XB
\)B
_;B
cTB
gmB
l�B
p�B
u�B
z�B
}�B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
�KB
�FB
�EB
�EB
�CB
�EB
�CB
�CB
�EB
�EB
�EB
�EB
�CB
�EB
�EB
�EB
�EB
�EB
�cB
�BF�BeOBlvBv�B��B�B<B#�B/B,�Bz�B��B��B�)B�B��B��B��B��B�4Bz�Bq�Bn�Bn�Bh_Bh`Bb9BM�B;SBwB�B�0B��BȠBǙB��B�B��BjiBT�BM�BT�B�BjjB\B?iB
��B
�KB
�KB
r�B
&�B	�B	�PB	P�B	H�B	-B	'�B	*�B	/B	8HB	=fB	^)B	l~B	ZB	N�B	J�B	G�B	@xB	<^B	8DB	4-B	2#B	1B	2!B	D�B	O�B	V�B	g_B	�#B	�qB	��B	��B	�#B	�}B	ɩB	�UB
 B
BB
�B	��B	��B
B
fB
�B
61B
L�B
Q�B
T�B
P�B
E�B
?hB
E�B
78B
2B
1B
1B
5+B
>cB
H�B
J�B
K�B
M�B
R�B
M�B
J�B
G�B
B|B
F�B
D�B
H�B
M�B
YB
[B
W�B
L�B
H�B
I�B
V�B
]B
\B
ZB
[B
YB
\B
[B
^B
eIB
fNB
h]B
i`B
jhB
jiB
jjB
mzB
m{B
o�B
n�B
knB
idB
ibB
h]B
c?B
ZB
U�B
S�B
O�B
M�B
O�B
N�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
O�B
O�B
O�B
Q�B
T�B
U�B
X�B
\B
fOB
n�B
mzB
klB
q�B
u�B
t�B
u�B
t�B
t�B
s�B
s�B
q�B
q�B
p�B
o�B
n�B
m{B
kmB
jgB
ibB
h[B
gTB
fOB
eIB
dCB
b9B
_"B
ZB
U�B
U�B
S�B
Q�B
O�B
N�B
L�B
H�B
H�B
G�B
F�B
E�B
D�B
C�B
CB
BzB
B{B
B|B
ArB
@nB
?fB
=]B
<TB
:HB
9BB
9AB
8:B
77B
6/B
5(B
3B
2B
4$B
5)B
5'B
5*B
4$B
4#B
3B
2B
1B
1B
0	B
/B
/B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
,�B
-�B
,�B
,�B
,�B
+�B
+�B
)�B
'�B
&�B
&�B
%�B
&�B
%�B
&�B
&�B
&�B
%�B
%�B
%�B
$�B
"�B
 �B
�B
�B
�B
�B
|B
}B
~B
B
xB
{B
}B
|B
}B
}B
}B
}B
vB
wB
rB
pB
sB
kB
kB
lB
mB
mB
dB
cB
dB
cB
_B
`B
YB
VB
_B
_B
WB
UB
UB
QB
HB
:B
3B
2B
4B
2B
:B
@B
9B
5B
.B
.B
;B
?B
?B
AB
:B
4B
?B
RB
PB
RB
QB
VB
VB
WB
WB
WB
XB
QB
WB
OB
NB
LB
MB
CB
9B
2B
1B
-B
-B
,B
2B
4B
3B
5B
9B
8B
:B
:B
;B
:B
6B
?B
=B
EB
EB
FB
EB
FB
FB
KB
GB
EB
MB
LB
LB
IB
RB
RB
RB
RB
OB
OB
OB
NB
QB
OB
MB
QB
PB
QB
OB
PB
OB
QB
_B
bB
bB
bB
]B
eB
aB
bB
aB
cB
bB
aB
eB
dB
iB
uB
}B
}B
}B
}B
B
|B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�G�O�B
"�B
(�B
1B
6.B
=WB
D�B
J�B
N�B
S�B
W�B
\B
_ B
c;B
gTB
lpB
p�B
u�B
z�B
}�B
�B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071544582016080715445820160807154458  AO  ARCAADJP                                                                    20150226221448    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221448  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221448  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807154458  IP                  G�O�G�O�G�O�                
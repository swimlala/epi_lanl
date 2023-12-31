CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:44Z AOML 3.0 creation; 2016-08-07T21:36:31Z UW 3.1 conversion     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221344  20160807143631  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_023                   2C  D   APEX                            6531                            072314                          846 @�/WFZ_�1   @�/W�b�@1�I�^5�dA�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�33B�  B���C   C  C  C  C  C
  C  C  C  C  C�C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�  D�#3D�i�D�� D���D�I�D�� D��3D��D�P D��3D��3D�3D�3Dڙ�D���D�fD�6fD�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B���B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B��)B�B�B�u�B�u�B�u�B�u�B���B�u�B�B�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�CT{C:�C!GC:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<!GC>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DEDE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW�DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��DtۅDy��D�\D�*�D�p�D��\D� �D�P�D��\D�ڏD�)D�W\D���D�ڏD�
�D��Dڠ�D��)D��D�=�D�z�D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AͰ!AͲ-Aʹ9A͸RA͸RA͸RAͶFAͶFAͶFAͶFAͶFA͸RA͸RAͺ^Aͺ^AͼjAͼjAͼjAͺ^AͼjA;wA;wAͼjA���A���A�A���A���A���A�A�A�ĜA���Aͺ^Aʹ9AͶFAͲ-AͲ-AͰ!AͲ-Aʹ9AͲ-A͡�A͡�Aͣ�Aͣ�Aͩ�A͟�A�A�K�AˮA�?}A���A��yAĮA�dZA���A�t�A�
=A��uA�(�A���A�ffA���A�C�A���A�1A���A��\A��A�1'A���A�&�A�+A�A�A�ĜA�(�A���A�^5A�r�A��;A��/A"�A{�hAz1Ax��Ap��Akp�Aj$�Ai\)Ah�+Ag�wAgO�Ae��A`�uA]�A[�hAYl�AU��AP��AO%AM&�AI�AG�AE��AD~�ACp�AA��A>��A<��A;��A:VA8��A7|�A6��A4�HA3�A3�7A2��A1��A/VA+/A)hsA(��A'��A&�!A%�A%hsA#�7A"�HA"�A"I�A!�FA ĜA 1A�A�/A��AO�Az�A��A�A�HAA�A  AG�A�9A�AO�AA�A�7A�AQ�AA�A�AA��A33A��AM�A��A��A(�A�AƨAS�A��AZAG�A �A �uA   @�K�@���@���@��PA $�A �@�ff@��@�V@��`@���@���@�I�@��m@�l�@�@���@���@�Z@�o@�z�@�P@�+@�p�@�Q�@�$�@陚@�X@�dZ@�j@��H@�^@�`B@�?}@��@�+@�-@�hs@�Ĝ@��m@ڟ�@�5?@���@�&�@ؓu@�r�@�1@�dZ@�@�l�@׾w@׍P@�bN@ٺ^@�^5@���@٩�@١�@�7L@�  @��@�x�@�Q�@ҸR@�V@�|�@�;d@�@ΰ!@�n�@͉7@�V@�%@�%@�7L@�hs@͙�@͑h@��@��@�Ĝ@̓u@�(�@��@˾w@�ƨ@�\)@�+@�C�@���@ʟ�@�
=@ˍP@��;@�9X@˝�@���@��@���@�1'@Ǿw@ǥ�@�9X@Ǖ�@�\)@�K�@�C�@�"�@��@�ȴ@�^5@�n�@ư!@���@ư!@�~�@�5?@��T@ũ�@ŉ7@�hs@�hs@�`B@ċD@���@��@���@�j@��9@�V@�G�@�7L@�/@���@�Ĝ@��@��m@��@�@�$�@�hs@��j@�(�@��P@�o@���@�ff@��@�?}@�r�@��@�l�@��y@�^5@�x�@�Z@� �@��m@��@�S�@��@�^5@��-@�V@�I�@�1@��
@��@���@�S�@��H@��\@�=q@�@��@�&�@��D@�Z@�Q�@�I�@��@�+@���@���@�M�@��-@�7L@�Ĝ@��D@�b@���@�@��R@�n�@�{@���@���@�x�@�G�@��@��@��@�I�@�(�@��;@���@���@�^5@��@��T@��#@��-@�X@�/@���@�Ĝ@�1@��F@���@���@���@���@�|�@�
=@�E�@��@���@�p�@�G�@���@���@�Z@�9X@�1'@� �@�1@�  @���@��
@���@�K�@��+@���@�hs@�hs@�/@��@�9X@��m@�  @���@���@���@�t�@�S�@�;d@��@��+@�5?@�@��^@��7@���@��j@��@�A�@��;@�ƨ@�|�@�dZ@��@��R@�^5@�$�@�J@��^@��@���@��@���@��D@�Q�@��@�t�@��@��H@���@�n�@�@���@���@��@�O�@�/@�/@�/@��`@�Q�@�1@���@�|�@�o@��y@��@�ȴ@���@�$�@���@�hs@�/@��@�V@��j@�z�@�Z@� �@���@�Ĝ@y�^@rn�@h��@]@S�F@K��@D�/@=p�@7\)@1G�@,(�@&ȴ@ ��@9X@��@o@A�@�D@	%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AͰ!AͲ-Aʹ9A͸RA͸RA͸RAͶFAͶFAͶFAͶFAͶFA͸RA͸RAͺ^Aͺ^AͼjAͼjAͼjAͺ^AͼjA;wA;wAͼjA���A���A�A���A���A���A�A�A�ĜA���Aͺ^Aʹ9AͶFAͲ-AͲ-AͰ!AͲ-Aʹ9AͲ-A͡�A͡�Aͣ�Aͣ�Aͩ�A͟�A�A�K�AˮA�?}A���A��yAĮA�dZA���A�t�A�
=A��uA�(�A���A�ffA���A�C�A���A�1A���A��\A��A�1'A���A�&�A�+A�A�A�ĜA�(�A���A�^5A�r�A��;A��/A"�A{�hAz1Ax��Ap��Akp�Aj$�Ai\)Ah�+Ag�wAgO�Ae��A`�uA]�A[�hAYl�AU��AP��AO%AM&�AI�AG�AE��AD~�ACp�AA��A>��A<��A;��A:VA8��A7|�A6��A4�HA3�A3�7A2��A1��A/VA+/A)hsA(��A'��A&�!A%�A%hsA#�7A"�HA"�A"I�A!�FA ĜA 1A�A�/A��AO�Az�A��A�A�HAA�A  AG�A�9A�AO�AA�A�7A�AQ�AA�A�AA��A33A��AM�A��A��A(�A�AƨAS�A��AZAG�A �A �uA   @�K�@���@���@��PA $�A �@�ff@��@�V@��`@���@���@�I�@��m@�l�@�@���@���@�Z@�o@�z�@�P@�+@�p�@�Q�@�$�@陚@�X@�dZ@�j@��H@�^@�`B@�?}@��@�+@�-@�hs@�Ĝ@��m@ڟ�@�5?@���@�&�@ؓu@�r�@�1@�dZ@�@�l�@׾w@׍P@�bN@ٺ^@�^5@���@٩�@١�@�7L@�  @��@�x�@�Q�@ҸR@�V@�|�@�;d@�@ΰ!@�n�@͉7@�V@�%@�%@�7L@�hs@͙�@͑h@��@��@�Ĝ@̓u@�(�@��@˾w@�ƨ@�\)@�+@�C�@���@ʟ�@�
=@ˍP@��;@�9X@˝�@���@��@���@�1'@Ǿw@ǥ�@�9X@Ǖ�@�\)@�K�@�C�@�"�@��@�ȴ@�^5@�n�@ư!@���@ư!@�~�@�5?@��T@ũ�@ŉ7@�hs@�hs@�`B@ċD@���@��@���@�j@��9@�V@�G�@�7L@�/@���@�Ĝ@��@��m@��@�@�$�@�hs@��j@�(�@��P@�o@���@�ff@��@�?}@�r�@��@�l�@��y@�^5@�x�@�Z@� �@��m@��@�S�@��@�^5@��-@�V@�I�@�1@��
@��@���@�S�@��H@��\@�=q@�@��@�&�@��D@�Z@�Q�@�I�@��@�+@���@���@�M�@��-@�7L@�Ĝ@��D@�b@���@�@��R@�n�@�{@���@���@�x�@�G�@��@��@��@�I�@�(�@��;@���@���@�^5@��@��T@��#@��-@�X@�/@���@�Ĝ@�1@��F@���@���@���@���@�|�@�
=@�E�@��@���@�p�@�G�@���@���@�Z@�9X@�1'@� �@�1@�  @���@��
@���@�K�@��+@���@�hs@�hs@�/@��@�9X@��m@�  @���@���@���@�t�@�S�@�;d@��@��+@�5?@�@��^@��7@���@��j@��@�A�@��;@�ƨ@�|�@�dZ@��@��R@�^5@�$�@�J@��^@��@���@��@���@��D@�Q�@��@�t�@��@��H@���@�n�@�@���@���@��@�O�@�/@�/@�/@��`@�Q�@�1@���@�|�@�o@��y@��@�ȴ@���@�$�@���@�hs@�/@��@�V@��j@�z�@�ZG�O�@���@�Ĝ@y�^@rn�@h��@]@S�F@K��@D�/@=p�@7\)@1G�@,(�@&ȴ@ ��@9X@��@o@A�@�D@	%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�BB
�HB
�HB
�BB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�HB
�NB
�TB
�ZB
�`B
�`B
�`B
�`B
�`B
�fB
�sB
�yB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��BB\B�B�B�B�B)�B'�B>wBI�BdZBn�BgmB[#BhsBt�BhsBhsBXBM�BF�B5?B)�B�B%B
�B
�}B
}�B
49B	�B	�NB	�B
(�B
%�B	��B	�/B	ŢB	�LB	��B	�B	hsB	`BB	[#B	VB	O�B	J�B	?}B	%�B	hB	1B��B�B�B��B��BǮBĜBĜBÖBÖB�qB�RB�LB�LB�XB�dB�dB�^B�}B��B��BBBŢB��B��B��BɺB��B��B��B��B��B��B��B��B��B��B��B�B�TB�`B�mB�sB�yB�yB�B�B��B	B	B	%B	PB	bB	{B	{B	{B	uB	hB	VB	DB	1B��B�B�B�sB�`B�mB�B�B�B�B�B��B��B��B��B��B��B	1B	bB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	VB	  B��B�B�B�B�sB�NB�BB�5B�B��B��B��B��B��B��B��B�
B�
B�B�)B�BB�`B�fB�yB�B�B�B��B��B		7B	�B	�B	�B	.B	:^B	>wB	=qB	D�B	H�B	B�B	9XB	7LB	5?B	6FB	2-B	-B	,B	,B	+B	+B	+B	.B	/B	49B	8RB	<jB	?}B	C�B	F�B	K�B	M�B	S�B	XB	XB	[#B	`BB	e`B	gmB	jB	iyB	jB	r�B	x�B	}�B	�B	�B	�B	� B	}�B	~�B	�B	�B	�=B	�JB	�JB	�PB	�PB	�PB	�PB	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�!B	�-B	�3B	�3B	�3B	�3B	�3B	�3B	�-B	�-B	�-B	�-B	�3B	�9B	�9B	�9B	�9B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�XB	�XB	�XB	�^B	�dB	�jB	�qB	�}B	��B	B	ŢB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�fB	�fB	�`B	�`B	�`B	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B

=B
DB
JB
JB
JB
JB
PB
VB
VB
hB
oB
�B
�B
"�B
)�B
1'B
8RB
>wB
C�B
J�B
P�B
T�B
ZB
^5B
dZB
gmB
k�B
p�B
r�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�8B
�:B
�:B
�6B
�8B
�8B
�8B
�4B
�:B
�:B
�1B
�=B
�:B
�8B
�8B
�8B
�:B
�8B
�=B
�:B
�9B
�9B
�:B
�7B
�>B
�FB
�HB
�QB
�SB
�PB
�QB
�PB
�WB
�cB
�lB
�gB
�mB
�mB
�tB
�tB
�oB
�vB
�B
�B
��B
��B
��B �BKB�B�BtB�B)�B'�B>cBI�BdGBn�Bg[B[BhaBt�Bh`BhbBW�BM�BF�B5,B)�B�BB
�oB
�kB
}�B
4'B	�B	�>B	�B
(�B
%�B	��B	� B	ŕB	�=B	��B	�B	hkB	`7B	[B	U�B	O�B	J�B	?rB	%�B	_B	*B��B�yB�B��B��BǨBĘBęBÐBÐB�jB�MB�GB�GB�OB�^B�^B�WB�vB��B��BBBŚB��B��BʺBɳB˾B��B˾B��B��B��B��B��B��B��B��B�B�NB�UB�_B�gB�pB�oB�B�B��B	 �B	B	B	EB	VB	pB	oB	oB	hB	]B	JB	6B	(B��B�zB�tB�fB�TB�_B�B�B�B�B�B��B��B��B��B��B��B	#B	VB	fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	}B	mB	GB��B��B�B�B�~B�fB�BB�8B�(B�B��B��B��B��B��B��B��B��B��B�B�B�7B�TB�ZB�mB�B�B�B��B��B		)B	~B	�B	�B	.B	:NB	>hB	=`B	D�B	H�B	BB	9GB	7:B	52B	63B	2B	,�B	+�B	+�B	*�B	*�B	*�B	.B	/
B	4(B	8AB	<[B	?lB	C�B	F�B	K�B	M�B	S�B	W�B	W�B	[B	`1B	eMB	g[B	jjB	ifB	jmB	r�B	x�B	}�B	��B	�B	� B	�B	}�B	~�B	��B	��B	�*B	�8B	�5B	�>B	�=B	�?B	�?B	�>B	�@B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�#B	�#B	�#B	�$B	�#B	�)B	�*B	�0B	�8B	�<B	�?B	�AB	�BB	�DB	�IB	�PB	�UB	�]B	�hB	�mB	�yB	ŎB	ƓB	ǘB	ǘB	ʬB	ͽB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�$B	�-B	�1B	�5B	�>B	�CB	�NB	�OB	�HB	�HB	�JB	�UB	�^B	�\B	�bB	�dB	�bB	�hB	�hB	�mB	�uB	�{B	�{B	�yB	�B	�B	�{B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
B
B
�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
	B
	 B

%B

$B
.B
3B
3B
3B
1B
7B
>B
>G�O�B
UB
�B
�B
"�B
)�B
1B
88B
>\B
C~B
J�B
P�B
T�B
ZB
^B
d@B
gSB
klB
p�B
r�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436312016080714363120160807143631  AO  ARCAADJP                                                                    20150226221344    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221344  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221344  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143631  IP                  G�O�G�O�G�O�                
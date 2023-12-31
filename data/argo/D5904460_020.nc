CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:58Z AOML 3.0 creation; 2016-08-07T21:17:31Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221258  20160807141732  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_020                   2C  D   APEX                            6487                            072314                          846 @�)��1   @�)�q� @,C��%�c���n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  @���A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�33B���B�  B�  B�  B�  B�  B���B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�3D��3D�VfD�l�D���D�fD�I�D�vfD��3D�	�D�@ D�|�DǼ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
A Q�A!�AA�A`Q�A���A���A���A���A���A���A���A���B �GBz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bh{Bpz�Bxz�B�=qB�p�B�
>B�=qB�=qB�=qB�=qB�=qB�
>B�p�B�
>B�=qB�=qB�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C8RC
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct8RCv�CxCz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�HDy��D��
D�Z=D�p�D�ФD�=D�MqD�z=D��
D�qD�C�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�$�A�(�A�$�A� �A�-A�5?A�5?A�5?A�5?A�=qA�9XA�7LA�9XA�9XA�9XA�9XA�;dA�?}A�?}A�?}A�Q�A�dZA�ffA�|�Aԣ�AԓuA�&�A��/AǛ�A�/A�JA�bNA�v�A�jA��A�7LA���A�%A�dZA�p�A���A���A���A���A���A��A�"�A��;A��-A���A��A��A���A��A�t�A�%A���A���A�x�A���A�\)A�l�A�\)A���A��\A��A��A~(�AtA�An�/Am��Ak��AfffAb�DA`�RA_l�A]��AW�-AUl�ATM�AQp�AP~�AO�hAO33AMC�AJ�`AH��AG"�AD��AB  A?�#A=l�A:�A:bA8Q�A7�A7�#A7�hA6�A6=qA4�A3t�A1hsA/G�A.(�A+G�A*n�A)�A'�TA'A%�A%?}A$�A#x�A!�#A  �A�9A��A�A��AhsAA|�A �A|�AoA-AC�A(�A��AffA�;AĜAn�A��A
��A
1'A	��A	XA�AM�A	��A
bNA	
=A��AXA��A�`A��A�A�+AZAA��AM�A-A�\A�hA��AJA�7A ��A (�A 5?A �+A=qA��At�Az�A��A ��A {A �uA ��A ^5A $�@�^5@��@�j@�J@���@�ȴ@��H@�A�@��w@�~�@�^@���@�P@�&�@��@@�w@�{@�J@���@@���@�5?@�J@�@�x�@�Z@�1'@�7L@�?}@�Q�@�"�@�!@�\@���@�\)@�t�@�R@�5?@�7@���@�Z@�t�@�+@柾@�5?@�@���@�hs@�  @���@�5?@��@�G�@�&�@��/@���@�Q�@��@ߍP@���@ݩ�@�hs@��/@܋D@�9X@�l�@��@�$�@�p�@��@ش9@�r�@�1'@�ƨ@�\)@֟�@�v�@��@���@ԣ�@��m@�S�@��@�ȴ@��@��/@�A�@��@϶F@υ@�"�@Η�@�^5@���@͙�@�X@�?}@��/@�z�@� �@ˮ@�C�@���@���@�v�@��@�hs@��@ȓu@�9X@��
@�S�@�+@�~�@�@őh@�G�@��`@�I�@��;@�t�@��y@�@�ff@�=q@�@�/@���@��@���@��u@�b@�l�@�
=@���@��-@�%@���@��/@��9@��D@�bN@�A�@��m@�dZ@��@�
=@�v�@�{@���@�x�@�&�@��u@�Q�@�I�@�I�@�9X@�1@���@��@�^5@�@�O�@��@��@� �@��m@��@���@�t�@�;d@��y@��!@�M�@��T@�X@���@�r�@�b@��@�;d@��+@�{@��-@�hs@�7L@���@��/@�Ĝ@�Q�@�(�@���@���@�t�@�l�@�;d@�@�ȴ@�n�@��#@�X@��9@�b@��F@��@�l�@�S�@�+@��y@�ff@��@�r�@�b@��m@���@�
=@��!@��\@�n�@�E�@���@��#@�@���@�`B@��@��9@�j@� �@�ƨ@�l�@��@��\@�{@�@��@��@��@��#@�@���@�hs@�G�@��@��@�I�@�b@��w@�S�@���@��!@��\@�v�@�=q@�{@��@�@�hs@�/@�Ĝ@�z�@�Q�@�9X@��m@�l�@�33@�o@�ȴ@�ȴ@���@�=q@��h@�/@�&�@��@�%@���@��9@�z�@�Z@�A�@��@��@��@��@�v�@���@�@���@��@�`B@�&�@��/@�Q�@�1'@��@�1@��@��@���@�E�@��^@��h@�x�@�G�@��`@��@��@�33@�"�@�o@���@�1@|�/@t�@j�!@d�@Z�!@P�`@H�u@@  @7�;@1��@,��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  A��A�$�A�(�A�$�A� �A�-A�5?A�5?A�5?A�5?A�=qA�9XA�7LA�9XA�9XA�9XA�9XA�;dA�?}A�?}A�?}A�Q�A�dZA�ffA�|�Aԣ�AԓuA�&�A��/AǛ�A�/A�JA�bNA�v�A�jA��A�7LA���A�%A�dZA�p�A���A���A���A���A���A��A�"�A��;A��-A���A��A��A���A��A�t�A�%A���A���A�x�A���A�\)A�l�A�\)A���A��\A��A��A~(�AtA�An�/Am��Ak��AfffAb�DA`�RA_l�A]��AW�-AUl�ATM�AQp�AP~�AO�hAO33AMC�AJ�`AH��AG"�AD��AB  A?�#A=l�A:�A:bA8Q�A7�A7�#A7�hA6�A6=qA4�A3t�A1hsA/G�A.(�A+G�A*n�A)�A'�TA'A%�A%?}A$�A#x�A!�#A  �A�9A��A�A��AhsAA|�A �A|�AoA-AC�A(�A��AffA�;AĜAn�A��A
��A
1'A	��A	XA�AM�A	��A
bNA	
=A��AXA��A�`A��A�A�+AZAA��AM�A-A�\A�hA��AJA�7A ��A (�A 5?A �+A=qA��At�Az�A��A ��A {A �uA ��A ^5A $�@�^5@��@�j@�J@���@�ȴ@��H@�A�@��w@�~�@�^@���@�P@�&�@��@@�w@�{@�J@���@@���@�5?@�J@�@�x�@�Z@�1'@�7L@�?}@�Q�@�"�@�!@�\@���@�\)@�t�@�R@�5?@�7@���@�Z@�t�@�+@柾@�5?@�@���@�hs@�  @���@�5?@��@�G�@�&�@��/@���@�Q�@��@ߍP@���@ݩ�@�hs@��/@܋D@�9X@�l�@��@�$�@�p�@��@ش9@�r�@�1'@�ƨ@�\)@֟�@�v�@��@���@ԣ�@��m@�S�@��@�ȴ@��@��/@�A�@��@϶F@υ@�"�@Η�@�^5@���@͙�@�X@�?}@��/@�z�@� �@ˮ@�C�@���@���@�v�@��@�hs@��@ȓu@�9X@��
@�S�@�+@�~�@�@őh@�G�@��`@�I�@��;@�t�@��y@�@�ff@�=q@�@�/@���@��@���@��u@�b@�l�@�
=@���@��-@�%@���@��/@��9@��D@�bN@�A�@��m@�dZ@��@�
=@�v�@�{@���@�x�@�&�@��u@�Q�@�I�@�I�@�9X@�1@���@��@�^5@�@�O�@��@��@� �@��m@��@���@�t�@�;d@��y@��!@�M�@��T@�X@���@�r�@�b@��@�;d@��+@�{@��-@�hs@�7L@���@��/@�Ĝ@�Q�@�(�@���@���@�t�@�l�@�;d@�@�ȴ@�n�@��#@�X@��9@�b@��F@��@�l�@�S�@�+@��y@�ff@��@�r�@�b@��m@���@�
=@��!@��\@�n�@�E�@���@��#@�@���@�`B@��@��9@�j@� �@�ƨ@�l�@��@��\@�{@�@��@��@��@��#@�@���@�hs@�G�@��@��@�I�@�b@��w@�S�@���@��!@��\@�v�@�=q@�{@��@�@�hs@�/@�Ĝ@�z�@�Q�@�9X@��m@�l�@�33@�o@�ȴ@�ȴ@���@�=q@��h@�/@�&�@��@�%@���@��9@�z�@�Z@�A�@��@��@��@��@�v�@���@�@���@��@�`B@�&�@��/@�Q�@�1'@��@�1@��@��@���@�E�@��^@��h@�x�@�G�@��`@��@��@�33@�"�G�O�@���@�1@|�/@t�@j�!@d�@Z�!@P�`@H�u@@  @7�;@1��@,��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
��B
ȴB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
��B
��B
�B
�mB
�mB
�BPB�B�B#�B,B2-B33B49B5?B33B1'B2-B<jB<jBS�B`BB:^B!�B�B�BC�B�B��B�B�#B��BȴBǮB�wB�LB�'B�B��B��B��B�+B[#B�B
�B
�wB
�B
J�B
�B	�/B	��B	{�B	r�B	bNB	C�B	/B	&�B	�B	{B��B�B�B�HB�B��B	PB	%B��B�B�yB�HB�B�B��B��BȴBƨBŢBĜBÖB��B�}B�jB�XB�LB�3B�B�!B�B�B�B�9B�LB�RB�LB�dB�qB��BŢBŢBÖB��B��B�dB�9B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B�B�B��B�-B�B�fB�HB��B��B��B�NB�B�B�B�B��B��B��B	B	�B	,B	33B	/B	,B	(�B	/B	6FB	@�B	dZB	�B	�B	|�B	w�B	r�B	r�B	�B	�1B	�+B	�+B	�B	�B	�B	z�B	t�B	|�B	�B	��B	��B	�\B	}�B	�B	��B	�oB	�JB	�oB	��B	��B	��B	��B	�B	�FB	�?B	�9B	�3B	�RB	B	��B	�;B	�NB	�TB	�TB	�TB	�ZB	�fB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�B	�yB	�yB	�B	�B	�B	�sB	�mB	�yB	�B	�B	�B	�B	�B	�yB	�sB	�sB	�yB	�sB	�sB	�mB	�fB	�`B	�fB	�B	�B	�B	�B	�B	�yB	�sB	�mB	�fB	�`B	�fB	�sB	�sB	�sB	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B

=B

=B

=B

=B
DB
DB
DB
JB
DB

=B

=B

=B

=B

=B
	7B
	7B
	7B
1B
1B

=B

=B

=B
DB
JB
JB
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
\B
\B
bB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
$�B
%�B
/B
6FB
>wB
D�B
I�B
N�B
S�B
YB
^5B
cTB
ffB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  B
ȭB
ȮB
ǨB
ȬB
ȮB
ȬB
ȬB
ȱB
ȯB
ȱB
ʺB
ȮB
ǧB
ȮB
ȯB
ȬB
ȮB
ȮB
ɵB
ʺB
��B
�B
�fB
�hB
�BIB�B�B#�B, B2%B3+B4/B56B3*B1B2'B<bB<aBS�B`9B:UB!�B�B�BC�B�B��B�B�B�wBȪBǥB�kB�>B�B��B��B��B��B�B[BwB
�wB
�pB
�
B
J�B
{B	�+B	��B	{�B	r�B	bMB	C�B	/B	&�B	�B	~B��B�B�B�MB�B��B	UB	(B��B�B�~B�NB�B�B��B��BȹBƬBŨBĢBÝB��B��B�pB�\B�QB�8B�!B�'B� B� B�!B�?B�RB�UB�RB�lB�wB��BŧBŦBÚB��B��B�kB�?B�-B�&B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�2B�B�kB�IB�B��B��B�QB�B�B�B�B��B��B��B	B	�B	,B	34B	/B	,	B	(�B	/B	6FB	@�B	dXB	�B	�B	|�B	w�B	r�B	r�B	�
B	�-B	�&B	�(B	�	B	�	B	�
B	z�B	t�B	|�B	�	B	��B	��B	�YB	}�B	�B	��B	�kB	�DB	�jB	��B	��B	��B	��B	�B	�@B	�:B	�3B	�.B	�LB	B	��B	�4B	�FB	�KB	�JB	�LB	�SB	�^B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�vB	�uB	�lB	�vB	�rB	�qB	�~B	�vB	�vB	�kB	�eB	�oB	�wB	�}B	�B	�B	�}B	�rB	�lB	�lB	�rB	�kB	�lB	�fB	�]B	�YB	�^B	�wB	�xB	�~B	�B	�wB	�qB	�jB	�eB	�]B	�YB	�]B	�lB	�mB	�kB	�qB	�qB	�vB	�tB	�sB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
"B
#B
(B
+B
	.B

2B

4B

4B

3B
:B
;B
:B
AB
<B

4B

3B

2B

2B

6B
	,B
	.B
	-B
'B
&B

6B

5B

4B
<B
CB
AB
GB
GB
EB
FB
FB
EB
MB
MB
QB
RB
SB
SB
SB
SB
RB
PB
YB
]B
fB
eB
fB
lB
qB
qB
uB
wB
vB
wB
yB
}B
}B
|B
�B
�B
�B
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
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�G�O�B
%�B
/B
6;B
>lB
D�B
I�B
N�B
S�B
YB
^(B
cGB
fZB
jr1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417322016080714173220160807141732  AO  ARCAADJP                                                                    20150226221258    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221258  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221258  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141732  IP                  G�O�G�O�G�O�                
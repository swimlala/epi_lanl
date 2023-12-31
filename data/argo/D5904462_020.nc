CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:21Z AOML 3.0 creation; 2016-08-07T21:51:12Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221421  20160807145112  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_020                   2C  D   APEX                            6529                            072314                          846 @�&��d�1   @�&���P@2h�9Xb�d�z�H1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDPfDP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D��D�VfD���D��3D�  D�P D�� D��3D�3D�@ D�vfD��fD��D�@ Dډ�D���D�fD�<�D� D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�p�A�RA&�RAF�RAf�RA��\A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B�
=B�
=B�
=B��
B��
B��
B��
BУ�B��
B�
=B��
B��
B��
B��
B��
B��
B��
B��
B�
=C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPQ�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D�{D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�{DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�GDP!GDP�GDQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D�'D�c�D��>D��D�qD�]qD��qD��D��D�MqD���D���D�'D�MqDڗD�>D�#�D�J>D�qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�33A�5?A�5?A�7LA�9XA�5?A�9XA�;dA�=qA�=qA�?}A�A�A�A�A�C�A�A�A�?}A�;dA�=qA�=qA�;dA�;dA�=qA�=qA�?}A�?}A�=qA�=qA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�=qA�?}A�=qA�=qA�=qA�=qA�9XA�/A�/A� �A��A�{A�JA�A���A��#Aͩ�A���Aȥ�AǑhA�hsA�p�AÝ�A�&�A�I�A�7LA��A�VA�n�A�t�A�I�A�1A�K�A��jA���A��HA�bA��+A��7A�
=A���A��A�bNA���A�
=A��#A��A���A�n�A�"�A�G�A�"�A���A� �A��;A�z�A���A���A�Q�A�"�A��A�r�A�E�A��A���A��mA�~�A�hsA��^A���A�E�A���A���A��A���A�1'A�A���A��`A~��A|��Az�AyO�Av�uAs?}Aq�;ApffAoVAkVAi��Af�DAc�A`��A^1'A]`BA\bAW��AU��AT�yAR��ANA�AH�AC�A@��A@�DA@JA?x�A>Q�A;��A8jA2�A0~�A/XA-�A+��A*n�A(�yA(A$$�A!�FA
=A��A�A��AI�A$�A��A�HA�yA�Ar�A�7AhsAG�A��AE�AA��A��A-A5?AE�A  A�A��Az�A��A33A~�A�A|�A��A �A��Al�A
�/A
r�A
JA	�A	A�AZA �A�A�
AVA��A�A$�A��A;dA�Az�A�A\)A33A�A�yA��A9XA7LA jA  �@��@�K�@�\)@��@���@�l�@��R@�O�@�b@�;d@���@��^@�z�@�\@�?}@�Ĝ@�(�@@��^@�I�@�t�@�C�@�o@�o@��@�
=@�ȴ@�7@�7@陚@���@�z�@� �@�ƨ@�+@�\@���@�@��@��H@��@�^@��@��D@�I�@�l�@ާ�@�n�@�=q@��T@���@�@��@�E�@��@���@ݙ�@݉7@�`B@�X@��@��/@�b@۝�@ۍP@�dZ@�ȴ@�@��`@؃@�1@׍P@ו�@ׅ@�C�@֟�@֏\@���@�hs@�`B@�`B@�G�@�V@Լj@�bN@�  @ӥ�@Ӆ@�dZ@�+@Ұ!@�=q@с@��/@У�@�j@���@��
@��
@Ϯ@υ@�dZ@�+@�~�@ͺ^@͑h@ͩ�@́@�`B@��@��`@̣�@�r�@�9X@���@˝�@�C�@���@ʰ!@�~�@�-@��@�p�@�G�@ȣ�@ȣ�@��`@�j@�r�@��m@�ƨ@�t�@�33@��@ư!@�n�@���@�O�@�?}@�&�@ēu@���@�|�@�
=@�@���@�/@��u@��;@�C�@��H@���@�M�@�$�@�@��^@�`B@�&�@�  @�ƨ@�33@�
=@�^5@�`B@�Ĝ@��
@�t�@�"�@��@���@��+@�~�@�E�@�@��@���@�?}@��@���@��`@���@�b@��m@�S�@���@�5?@�{@���@�7L@��/@�Q�@��@���@�K�@�"�@�@�ȴ@��+@��@�X@���@�j@��@���@�o@���@���@���@�hs@�V@���@��9@�r�@�  @��@�l�@��@�\)@��R@�ff@���@�X@��@�r�@�I�@�1'@�  @��m@�ƨ@��@�K�@���@�V@��@��^@�x�@�hs@�&�@���@�Ĝ@��u@�A�@�  @���@�t�@�o@�n�@�@��T@�@�x�@�O�@���@�Ĝ@�z�@��@��w@��@�dZ@��@��H@��R@���@�$�@�?}@��@�z�@�Z@�9X@��F@�V@��@��D@�;d@|��@w
=@lI�@a�@XĜ@PĜ@J��@B~�@=�@7\)@1hs@*�@#�@�-@bN@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�7LA�33A�5?A�5?A�7LA�9XA�5?A�9XA�;dA�=qA�=qA�?}A�A�A�A�A�C�A�A�A�?}A�;dA�=qA�=qA�;dA�;dA�=qA�=qA�?}A�?}A�=qA�=qA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�=qA�?}A�=qA�=qA�=qA�=qA�9XA�/A�/A� �A��A�{A�JA�A���A��#Aͩ�A���Aȥ�AǑhA�hsA�p�AÝ�A�&�A�I�A�7LA��A�VA�n�A�t�A�I�A�1A�K�A��jA���A��HA�bA��+A��7A�
=A���A��A�bNA���A�
=A��#A��A���A�n�A�"�A�G�A�"�A���A� �A��;A�z�A���A���A�Q�A�"�A��A�r�A�E�A��A���A��mA�~�A�hsA��^A���A�E�A���A���A��A���A�1'A�A���A��`A~��A|��Az�AyO�Av�uAs?}Aq�;ApffAoVAkVAi��Af�DAc�A`��A^1'A]`BA\bAW��AU��AT�yAR��ANA�AH�AC�A@��A@�DA@JA?x�A>Q�A;��A8jA2�A0~�A/XA-�A+��A*n�A(�yA(A$$�A!�FA
=A��A�A��AI�A$�A��A�HA�yA�Ar�A�7AhsAG�A��AE�AA��A��A-A5?AE�A  A�A��Az�A��A33A~�A�A|�A��A �A��Al�A
�/A
r�A
JA	�A	A�AZA �A�A�
AVA��A�A$�A��A;dA�Az�A�A\)A33A�A�yA��A9XA7LA jA  �@��@�K�@�\)@��@���@�l�@��R@�O�@�b@�;d@���@��^@�z�@�\@�?}@�Ĝ@�(�@@��^@�I�@�t�@�C�@�o@�o@��@�
=@�ȴ@�7@�7@陚@���@�z�@� �@�ƨ@�+@�\@���@�@��@��H@��@�^@��@��D@�I�@�l�@ާ�@�n�@�=q@��T@���@�@��@�E�@��@���@ݙ�@݉7@�`B@�X@��@��/@�b@۝�@ۍP@�dZ@�ȴ@�@��`@؃@�1@׍P@ו�@ׅ@�C�@֟�@֏\@���@�hs@�`B@�`B@�G�@�V@Լj@�bN@�  @ӥ�@Ӆ@�dZ@�+@Ұ!@�=q@с@��/@У�@�j@���@��
@��
@Ϯ@υ@�dZ@�+@�~�@ͺ^@͑h@ͩ�@́@�`B@��@��`@̣�@�r�@�9X@���@˝�@�C�@���@ʰ!@�~�@�-@��@�p�@�G�@ȣ�@ȣ�@��`@�j@�r�@��m@�ƨ@�t�@�33@��@ư!@�n�@���@�O�@�?}@�&�@ēu@���@�|�@�
=@�@���@�/@��u@��;@�C�@��H@���@�M�@�$�@�@��^@�`B@�&�@�  @�ƨ@�33@�
=@�^5@�`B@�Ĝ@��
@�t�@�"�@��@���@��+@�~�@�E�@�@��@���@�?}@��@���@��`@���@�b@��m@�S�@���@�5?@�{@���@�7L@��/@�Q�@��@���@�K�@�"�@�@�ȴ@��+@��@�X@���@�j@��@���@�o@���@���@���@�hs@�V@���@��9@�r�@�  @��@�l�@��@�\)@��R@�ff@���@�X@��@�r�@�I�@�1'@�  @��m@�ƨ@��@�K�@���@�V@��@��^@�x�@�hs@�&�@���@�Ĝ@��u@�A�@�  @���@�t�@�o@�n�@�@��T@�@�x�@�O�@���@�Ĝ@�z�@��@��w@��@�dZ@��@��H@��R@���@�$�@�?}@��@�z�@�Z@�9XG�O�@�V@��@��D@�;d@|��@w
=@lI�@a�@XĜ@PĜ@J��@B~�@=�@7\)@1hs@*�@#�@�-@bN@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�3B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�'B�!B�B��B�dBɺB��B��B�/B�B  B �BI�Bp�Bt�B�=B�hB��B��B��B��B��B��B�hB�PB�1B�B�B�B}�Bu�BffB]/BQ�BF�B:^B%�BhBB�B�yB�NB�;B�B��B�jB�9B�!B��B��B��B�oB�bB�VB�+Bw�BjBYB>wB#�B�B
�B
B
��B
�PB
cTB
0!B
�B
JB	��B	�B	�HB	��B	B	�LB	�B	��B	�=B	y�B	dZB	VB	F�B	@�B	5?B	!�B	�B	hB	%B�B�TB�/B�B�B��B��B��B��B��B��BȴBǮBŢB�}B�wB�qB�LB��B��B��B��B��B��B��B��B�{B�hB�bB�\B�bB�uB��B��B�3B�9B�3B�FBB��B��B�)B�BB�B��B	1B	
=B	JB	PB	VB	VB	\B	uB	�B	�B	&�B	0!B	7LB	;dB	?}B	A�B	B�B	D�B	M�B	Q�B	P�B	O�B	S�B	Q�B	R�B	S�B	VB	T�B	S�B	W
B	\)B	_;B	aHB	bNB	cTB	aHB	`BB	aHB	_;B	_;B	_;B	^5B	[#B	YB	W
B	VB	W
B	XB	XB	YB	[#B	^5B	_;B	_;B	_;B	_;B	aHB	dZB	jB	l�B	o�B	s�B	u�B	w�B	{�B	~�B	�B	�JB	�\B	�\B	�\B	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�-B	�3B	�FB	�RB	�^B	�^B	�XB	�LB	�FB	�?B	�?B	�FB	�FB	�RB	�^B	�XB	�dB	�}B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�BB	�BB	�HB	�BB	�HB	�NB	�TB	�TB	�TB	�NB	�NB	�NB	�NB	�HB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�HB	�BB	�BB	�BB	�BB	�;B	�5B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�sB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
PB
B

=B
�B
�B
�B
!�B
'�B
1'B
:^B
=qB
C�B
J�B
O�B
S�B
YB
`BB
e`B
jB
n�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�CBɕBʟB��B�B�B��B �BI�BpBt�B�B�EB��B��B��B��B��B�aB�DB�-B�B��B��B��B}�Bu�Bf>B]BQ�BF�B:6B%�BAB�B�B�QB�%B�B��B̣B�DB�B��B��B�rB�YB�IB�9B�.B�Bw�BjYBX�B>NB#�BxB
�B
�eB
��B
�,B
c.B
/�B
{B
%B	��B	�B	�&B	˥B	�oB	�+B	��B	�hB	�B	y�B	d;B	U�B	F�B	@dB	5!B	!�B	nB	KB	B�B�9B�B��B��B��B��BͷBͶBνBͷBȘBǎBńB�_B�\B�SB�/B��B��B�uB�rB�rB�sB�qB�cB�]B�NB�EB�@B�EB�XB��B��B�B�B�B�%B�rBͱB��B�
B�B�hB��B	B	
B	(B	/B	2B	5B	:B	PB	iB	�B	&�B	/�B	7'B	;@B	?WB	AdB	BiB	DwB	M�B	Q�B	P�B	O�B	S�B	Q�B	R�B	S�B	U�B	T�B	S�B	V�B	\B	_B	a B	b'B	c+B	a"B	`B	a!B	_B	_B	_B	^B	Z�B	X�B	V�B	U�B	V�B	W�B	W�B	X�B	Z�B	^B	_B	_B	_B	_B	a"B	d3B	jXB	lbB	owB	s�B	u�B	w�B	{�B	~�B	��B	�B	�3B	�1B	�2B	�=B	�=B	�<B	�KB	�WB	�oB	�|B	�}B	�{B	�zB	�uB	�oB	�hB	�nB	�wB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�	B	�B	�'B	�1B	�1B	�,B	�!B	�B	�B	�B	�B	�B	�&B	�4B	�+B	�;B	�RB	�dB	�jB	�iB	�oB	�uB	�uB	�tB	�tB	�uB	�yB	�}B	ǂB	ɏB	ʕB	ɎB	ȇB	ȇB	ʖB	̞B	ͧB	ͧB	ήB	ήB	ϲB	зB	жB	ϰB	ϱB	ѾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�&B	�)B	�'B	� B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	�B	�B	�%B	�,B	�3B	�)B	�0B	�-B	�*B	�+B	�3B	�9B	�7B	�1B	�2B	�9B	�9B	�@B	�>B	�>B	�CB	�JB	�MB	�IB	�DB	�4B	�RB	�UB	�UB	�TB	�XB	�\B	�[B	�UB	�TB	�VB	�]B	�YB	�YB	�UB	�VB	�OB	�PB	�VB	�WB	�OB	�OB	�PB	�YB	�UB	�]B	�\B	�aB	�bB	�aB	�iB	�hB	�jB	�tB	�}B	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
 �B

B
bB
UB
�B
!�B
'�B
0�B
:,B
=AB
CeB
J�B
O�B
S�B
X�B
`B
e-B
jNB
nfB
qxB
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451122016080714511220160807145112  AO  ARCAADJP                                                                    20150226221421    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221421  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221421  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145112  IP                  G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:34Z AOML 3.0 creation; 2016-08-07T21:51:14Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221434  20160807145114  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_031                   2C  D   APEX                            6529                            072314                          846 @�5ɓ��1   @�5[��@1��;dZ�d���-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4�C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�	�D�I�D�vfD��3D�  D�@ D�� D��fD���D�C3D��3D�ٚD��D�<�D�s3D�ɚD�  D�P D�fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
>@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
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
B��
B��
B��
B��
B��
B��
B��
C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4�C6�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�{Dy�{D�D�WD���D���D�qD�MqD��qD���D�
>D�P�D���D��D�'D�J>Dڀ�D��D�-qD�]qD��D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsA�?}A�$�A�VA��A���A���AʾwAʮAʧ�Aʗ�A�r�A�bNA�XA�VA�K�A�?}A�(�A�{A�A��Aɲ-A�`BA�/A�VAȮA�bNA�/A�A���AǓuA�~�A�=qA��AƧ�A�M�Aŧ�A�`BA�9XA�bAć+A�
=A��A���A�G�A�ĜA���A��7A��
A�XA���A��A�ĜA���A�S�A���A��A�ȴA�&�A���A��/A� �A�E�A�Q�A�$�A�=qA���A��A�O�A�A�A�A�{A�&�A�5?A���A��wA�?}A���A�dZA���A��!A�1'A���A���A��-A�G�A��7A���A�`BA���A���A�ffA�7LA�5?A���A�G�A�"�A�v�A�z�A�1A��+A��mA�bA~z�A|�jA|n�A|JA{�Aw�-At��Ar�DAl�Ag�Ad��Ad=qAc�hA^�uAT�AOdZAI��AES�AC+AA+A=?}A;�A9�A7�A5S�A4�A3�
A1�FA0 �A/O�A.�uA-�A-oA,1'A+oA*  A(�HA(1A'oA%�
A$v�A#�^A"^5A!l�A!/A�wA|�At�AbA��AA�A��Al�A��A-A�mA��A�A��AVA�
A��A��A�PA��A��A��AhsA��A�9A�\A��A�jA�+A^5A�A&�A
 �A	��A	�A	�FA	��A	�FA	�A	dZA	7LA	%A��A�A�A"�A��A�9A�DAbNA�A�;AC�A%A�A�!A1'A�FA��AjA-A��A ��A �D@�dZ@�=q@�@���@���@�$�@�@��#@��h@��@�+@��D@���@�o@�=q@��@�A�@��@�@�"�@��H@�E�@�u@�l�@�;d@@��#@�hs@��
@�R@�E�@��T@���@�^@��@�G�@��/@�1@�"�@��@�
=@�M�@�@�(�@���@⟾@��`@ߕ�@���@�M�@ݺ^@�`B@ܛ�@�I�@�bN@�(�@ۥ�@�\)@�@ڧ�@��@�O�@�(�@�9X@أ�@�j@ו�@�dZ@�\)@�33@�"�@�@���@ְ!@�5?@Ցh@���@ԋD@�9X@�ƨ@�;d@Ұ!@�{@���@Ѳ-@с@�7L@Л�@�j@�j@�I�@�(�@�1@υ@�33@�ȴ@�ff@�=q@��T@�p�@�7L@���@̴9@�1'@˝�@�K�@��@ʰ!@�^5@�-@��@��@ɺ^@ɉ7@�`B@�?}@�?}@��@ȓu@�9X@��@ǝ�@�|�@�+@�5?@�G�@Ĭ@�A�@�b@�  @���@�;d@�@�@��#@��^@�hs@���@�bN@�Q�@��m@�l�@�ȴ@�V@��@���@���@�p�@��@���@��@�  @���@�33@��@���@��+@�ff@��@��^@�X@���@��u@��@��P@�@���@��@�hs@��@��j@�z�@�Z@��@�
=@��H@���@�-@��7@��@��@���@���@�r�@��@�ƨ@�t�@�n�@���@�?}@��@�Q�@��@��F@�;d@�n�@�-@�J@��T@�hs@�?}@��@��@�  @�dZ@�@���@�V@���@�`B@�O�@�/@���@���@�
=@��#@�G�@��@�I�@� �@��F@�S�@��@��y@��H@���@���@��+@�$�@��@��^@�x�@��@�z�@�9X@��F@���@�|�@�C�@��y@�v�@��T@��-@���@��h@��@�X@�V@��`@�Ĝ@���@��D@��@�bN@�9X@�1@��w@���@�t�@�l�@�S�@�@��@��R@�n�@�V@��@��^@���@���@��9@��@���@��@�I�@�1'@��R@�@�hs@�G�@��@tz�@i�@`Ĝ@Y�@SdZ@K�@C��@<�@7�@2=q@+�F@%`B@�-@�@S�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�hsA�?}A�$�A�VA��A���A���AʾwAʮAʧ�Aʗ�A�r�A�bNA�XA�VA�K�A�?}A�(�A�{A�A��Aɲ-A�`BA�/A�VAȮA�bNA�/A�A���AǓuA�~�A�=qA��AƧ�A�M�Aŧ�A�`BA�9XA�bAć+A�
=A��A���A�G�A�ĜA���A��7A��
A�XA���A��A�ĜA���A�S�A���A��A�ȴA�&�A���A��/A� �A�E�A�Q�A�$�A�=qA���A��A�O�A�A�A�A�{A�&�A�5?A���A��wA�?}A���A�dZA���A��!A�1'A���A���A��-A�G�A��7A���A�`BA���A���A�ffA�7LA�5?A���A�G�A�"�A�v�A�z�A�1A��+A��mA�bA~z�A|�jA|n�A|JA{�Aw�-At��Ar�DAl�Ag�Ad��Ad=qAc�hA^�uAT�AOdZAI��AES�AC+AA+A=?}A;�A9�A7�A5S�A4�A3�
A1�FA0 �A/O�A.�uA-�A-oA,1'A+oA*  A(�HA(1A'oA%�
A$v�A#�^A"^5A!l�A!/A�wA|�At�AbA��AA�A��Al�A��A-A�mA��A�A��AVA�
A��A��A�PA��A��A��AhsA��A�9A�\A��A�jA�+A^5A�A&�A
 �A	��A	�A	�FA	��A	�FA	�A	dZA	7LA	%A��A�A�A"�A��A�9A�DAbNA�A�;AC�A%A�A�!A1'A�FA��AjA-A��A ��A �D@�dZ@�=q@�@���@���@�$�@�@��#@��h@��@�+@��D@���@�o@�=q@��@�A�@��@�@�"�@��H@�E�@�u@�l�@�;d@@��#@�hs@��
@�R@�E�@��T@���@�^@��@�G�@��/@�1@�"�@��@�
=@�M�@�@�(�@���@⟾@��`@ߕ�@���@�M�@ݺ^@�`B@ܛ�@�I�@�bN@�(�@ۥ�@�\)@�@ڧ�@��@�O�@�(�@�9X@أ�@�j@ו�@�dZ@�\)@�33@�"�@�@���@ְ!@�5?@Ցh@���@ԋD@�9X@�ƨ@�;d@Ұ!@�{@���@Ѳ-@с@�7L@Л�@�j@�j@�I�@�(�@�1@υ@�33@�ȴ@�ff@�=q@��T@�p�@�7L@���@̴9@�1'@˝�@�K�@��@ʰ!@�^5@�-@��@��@ɺ^@ɉ7@�`B@�?}@�?}@��@ȓu@�9X@��@ǝ�@�|�@�+@�5?@�G�@Ĭ@�A�@�b@�  @���@�;d@�@�@��#@��^@�hs@���@�bN@�Q�@��m@�l�@�ȴ@�V@��@���@���@�p�@��@���@��@�  @���@�33@��@���@��+@�ff@��@��^@�X@���@��u@��@��P@�@���@��@�hs@��@��j@�z�@�Z@��@�
=@��H@���@�-@��7@��@��@���@���@�r�@��@�ƨ@�t�@�n�@���@�?}@��@�Q�@��@��F@�;d@�n�@�-@�J@��T@�hs@�?}@��@��@�  @�dZ@�@���@�V@���@�`B@�O�@�/@���@���@�
=@��#@�G�@��@�I�@� �@��F@�S�@��@��y@��H@���@���@��+@�$�@��@��^@�x�@��@�z�@�9X@��F@���@�|�@�C�@��y@�v�@��T@��-@���@��h@��@�X@�V@��`@�Ĝ@���@��D@��@�bN@�9X@�1@��w@���@�t�@�l�@�S�@�@��@��R@�n�@�V@��@��^@���@���@��9@��@���@��@�I�G�O�@��R@�@�hs@�G�@��@tz�@i�@`Ĝ@Y�@SdZ@K�@C��@<�@7�@2=q@+�F@%`B@�-@�@S�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�1B�DB�=B�7B�7B�7B�=B�=B�7B�DB�\B�oB�uB��B��B��B��B��B��B��B�B�jB��B�B��B1BuB�B�B!�B"�B#�B'�B+B49B5?B9XB;dB;dB;dB=qB?}BP�BdZBgmBe`B[#BM�BG�B+B�B�B��B�}B��B��B��B�B�BB�#B�fBB"�B@�B?}B�mB�qB��B��B�B��B�{B��B��B�LB�qB�B��B��B��B��B�hB�BR�B�BPB�B��B�B�'Bs�BL�B9XBVB
��B
��B
�LB
�B
��B
�JB
� B
ffB
B�B
�B
VB
DB
+B
  B	�`B	��B	�qB	��B	s�B	dZB	bNB	\)B	:^B	  B�`B�#B�B��B��B��B��B��B��BƨBŢBÖBŢBƨBŢBŢBŢBŢBĜBŢBƨBƨBŢBĜBǮBĜBB��B��B�wB�qB�qB�FB�?B�RB�jB��B�}B�B�B�B�!BĜB��B�
B�ZB�yB�yB�yB�B��B��B	B	B	+B		7B	
=B	1B		7B		7B	1B	%B	+B	VB	�B	!�B	)�B	/B	1'B	1'B	2-B	33B	33B	6FB	8RB	<jB	?}B	?}B	@�B	A�B	B�B	B�B	G�B	H�B	H�B	I�B	K�B	M�B	M�B	O�B	O�B	P�B	Q�B	R�B	S�B	T�B	T�B	VB	YB	^5B	_;B	_;B	_;B	\)B	ZB	^5B	_;B	_;B	_;B	aHB	cTB	dZB	ffB	gmB	hsB	hsB	jB	k�B	l�B	l�B	m�B	m�B	n�B	p�B	r�B	s�B	t�B	u�B	v�B	x�B	{�B	� B	�B	�B	�B	�%B	�%B	�=B	�DB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�3B	�9B	�9B	�3B	�!B	�3B	�XB	�dB	�dB	�^B	�^B	�}B	��B	��B	B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�;B	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�fB	�mB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
	7B
\B
uB
�B
�B
%�B
,B
6FB
=qB
@�B
C�B
J�B
VB
[#B
^5B
cTB
hsB
n�B
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�$B�B�B�B�B�B�B�B�#B�9B�LB�UB�`B�mB�wB�B��B��B��B��B�HB��B�\B��BBTB}B�B!�B"�B#�B'�B*�B4B5!B98B;DB;@B;BB=NB?^BP�Bd:BgOBe>B[BM�BG�B*�B�pB��BʟB�YB��B��B��B�\B�B��B�?B�B"�B@`B?WB�JB�MB��B˥B��B�`B�SB��B��B�(B�NB��B��B��B��B�fB�BB��BR�B�B-BWB��B��B��Bs�BL�B9-B3B
��B
ͪB
�%B
��B
�oB
�"B
�B
fBB
BlB
�B
2B
B
B	��B	�>B	ͱB	�QB	�cB	s�B	d:B	b/B	\B	:?B��B�DB�B��B��B��B̰B˭B̲BʤBƋBņB�{BŅBƋBŅBłBŅBńB�BńBƊBƉBŅBĄBǐB�}B�rB�cB�dB�XB�RB�QB�(B� B�4B�MBʣB�_B��B��B��B�B�B��B��B�<B�XB�XB�YB�B��B��B	 �B	�B	B		B	
B	B		B		B	B	B	B	3B	}B	!�B	)�B	.�B	1B	1 B	2	B	3B	3B	6B	8-B	<BB	?VB	?WB	@_B	AcB	BjB	BlB	G�B	H�B	H�B	I�B	K�B	M�B	M�B	O�B	O�B	P�B	Q�B	R�B	S�B	T�B	T�B	U�B	X�B	^B	_B	_B	_B	\ B	Y�B	^B	_B	_B	_B	aB	c*B	d3B	f=B	gEB	hLB	hLB	jVB	k\B	leB	ldB	mjB	mhB	npB	p|B	r�B	s�B	t�B	u�B	v�B	x�B	{�B	�B	��B	��B	��B	��B	��B	�B	�B	�1B	�IB	�bB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�	B	�-B	�6B	�7B	�3B	�4B	�RB	�XB	�[B	�cB	�cB	�iB	�nB	�wB	�|B	ǃB	ǂB	ǁB	ȇB	ɌB	ʕB	ʔB	ɐB	ɐB	˚B	̡B	ͥB	ͨB	ͦB	ͧB	ΫB	ΫB	ϱB	ϲB	зB	иB	иB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	� B	�B	�!B	�&B	�%B	�(B	�&B	�(B	�'B	�B	�B	�&B	�,B	�+B	�+B	�+B	�1B	�1B	�1B	�2B	�1B	�2B	�9B	�3B	�3B	�1B	�4B	�8B	�8B	�@B	�?B	�7B	�?B	�5B	�8B	�=B	�?B	�?B	�@B	�>B	�?B	�EB	�EB	�DB	�KB	�PB	�WB	�WB	�WB	�TB	�WB	�UB	�[B	�\B	�PB	�OB	�KB	�JB	�OB	�YB	�\B	�\B	�QB	�KB	�OB	�QB	�OB	�NB	�LB	�OB	�bB	�cB	�cB	�bB	�cB	�iB	�pB	�vB	�nB	�hB	�mB	�nB	�oB	�oB	�qB	�sB	�mB	�mB	�oB	�nB	�oB	�tB	�wB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
	B
-B
CB
VB
�B
%�B
+�B
6B
=?B
@RB
CeB
J�B
U�B
Z�B
^B
c!B
hBB
ndB
ptB
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451142016080714511420160807145114  AO  ARCAADJP                                                                    20150226221434    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221434  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221434  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145114  IP                  G�O�G�O�G�O�                
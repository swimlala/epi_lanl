CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:37Z AOML 3.0 creation; 2016-08-07T21:51:14Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221437  20160807145114  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               !A   AO  5287_9017_033                   2C  D   APEX                            6529                            072314                          846 @�7�d� 1   @�7�����@2 ě��T�d��n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    !A   B   B   @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyl�D���D�@ D�� D���D�3D�<�D���D�� D� D�9�D��3D��fD�3D�6fDڙ�D��3D��D�@ D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�\)A{A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\�{C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DVRDV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dy��D��)D�O\D��\D��)D��D�L)D���D��\D�\D�H�D���D���D��D�E�Dڨ�D��D�)D�O\D�\D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�x�A�~�Aˇ+A˃AˁA˃A˅A˃AˁAˁA�|�A�|�A˃A˅Aˇ+A˅Aˇ+A˃A�t�A�jA�XA�O�A�M�A�I�A�C�A�?}A�5?A�/A�&�A�bA�%Aʺ^A�1'A���A���A��A�A�`BA�ZA�M�A�33A�$�A��A�oA�AƋDA��
A�9XAĥ�A�K�A�hsA�A��9A�;dA���A��HA�bNA�33A�p�A��7A�C�A�+A��HA�C�A�|�A�`BA��uA��A�+A��A��yA��A�jA�7LA��wA�A�A���A�~�A���A��A���A�33A��^A���A��mA���A�dZA�G�A�A��9A���A��uA��A�I�A���A�A��A��A��A��^A�I�A�ffA���A�G�A���A��A�A|1Azn�AvVAq�;ApbNAnn�AlffAk%Ah�RAg��Ae��Ac�^Aa�A_��AZ��AW��AU�AR��AQ�AP�uAN~�AJ�HAG�AD�DAC+A@ffA=ƨA<�A8�9A5�A4{A3XA2(�A/�
A+|�A*�A)G�A)
=A(�uA'�PA&A�A%/A$z�A#��A"�A!�hA �/A ZA��A�RA�A�-AG�A�Ar�A�Az�A|�Ar�Ax�A�9A��Av�A7LA=qA�hA�RA9XA�AA��A�jAA�A�A��A�A��AdZA	�A	�A��Az�AE�A=qA(�A�mA\)A1'A�A�A1'A�A{A1'A{A��A��A&�A��A��A33A ��A A�@��@���@�X@�%@��`@��@��m@��y@���@��-@�X@�I�@��@��!@��-@��u@��;@�
=@�ff@�-@�O�@���@�Q�@�@�;d@@�{@�x�@�&�@��@�@�C�@�M�@��@�G�@�%@���@��@�9X@�l�@�7@�D@�1'@�  @��@�C�@��@�v�@��@��@���@���@��@��@�A�@�(�@�b@���@��
@߮@���@�n�@�$�@ݺ^@�p�@�O�@��@���@���@ܣ�@�1'@�\)@�v�@ٲ-@٩�@���@�p�@���@ؓu@��;@�ȴ@�-@���@�hs@��`@�9X@���@ӥ�@�|�@�;d@��H@Ұ!@�=q@Ѳ-@�x�@�?}@��`@д9@Ѓ@�(�@��m@�l�@�+@�@��@ΰ!@Ώ\@�=q@��@��@�O�@���@�z�@� �@�C�@���@ʇ+@��#@ɑh@�/@ȃ@�A�@�1@���@�dZ@�
=@Ɨ�@��@ũ�@ŉ7@���@ģ�@�z�@��@§�@�p�@�V@��u@��@��F@�\)@�o@���@�5?@���@���@��@�Z@��@���@�ƨ@���@�S�@�@��y@��!@�n�@��T@���@�O�@��@�ƨ@�|�@�C�@��@�
=@�@�@��@��y@��H@���@���@�n�@�M�@��7@��@�%@��j@��D@�bN@�1'@��@��@�t�@�
=@��@��@���@��R@�ff@��#@�p�@�%@�r�@��@�C�@��R@�n�@�5?@���@���@��D@�z�@�j@�9X@�  @��w@�|�@�\)@�33@���@��+@�ff@�=q@��@��#@�O�@���@�bN@�A�@�(�@�  @��F@��@��@���@���@�M�@�7L@��@���@�(�@��P@�\)@�o@�n�@�@�x�@��@�9X@��
@�+@���@��\@�~�@�v�@�$�@���@��-@�O�@�V@��u@�b@���@�K�@�+@��@�@��H@��H@��\@�E�@�-@���@�Ĝ@�(�@�+@��@��!@���@�~�@�^5@�J@��-@��@�K�@��m@�+@��7@x��@nv�@f@]�h@T�D@I��@D��@=p�@8��@41@,�j@&@!�7@�h@��@ff@�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�x�A�x�A�~�Aˇ+A˃AˁA˃A˅A˃AˁAˁA�|�A�|�A˃A˅Aˇ+A˅Aˇ+A˃A�t�A�jA�XA�O�A�M�A�I�A�C�A�?}A�5?A�/A�&�A�bA�%Aʺ^A�1'A���A���A��A�A�`BA�ZA�M�A�33A�$�A��A�oA�AƋDA��
A�9XAĥ�A�K�A�hsA�A��9A�;dA���A��HA�bNA�33A�p�A��7A�C�A�+A��HA�C�A�|�A�`BA��uA��A�+A��A��yA��A�jA�7LA��wA�A�A���A�~�A���A��A���A�33A��^A���A��mA���A�dZA�G�A�A��9A���A��uA��A�I�A���A�A��A��A��A��^A�I�A�ffA���A�G�A���A��A�A|1Azn�AvVAq�;ApbNAnn�AlffAk%Ah�RAg��Ae��Ac�^Aa�A_��AZ��AW��AU�AR��AQ�AP�uAN~�AJ�HAG�AD�DAC+A@ffA=ƨA<�A8�9A5�A4{A3XA2(�A/�
A+|�A*�A)G�A)
=A(�uA'�PA&A�A%/A$z�A#��A"�A!�hA �/A ZA��A�RA�A�-AG�A�Ar�A�Az�A|�Ar�Ax�A�9A��Av�A7LA=qA�hA�RA9XA�AA��A�jAA�A�A��A�A��AdZA	�A	�A��Az�AE�A=qA(�A�mA\)A1'A�A�A1'A�A{A1'A{A��A��A&�A��A��A33A ��A A�@��@���@�X@�%@��`@��@��m@��y@���@��-@�X@�I�@��@��!@��-@��u@��;@�
=@�ff@�-@�O�@���@�Q�@�@�;d@@�{@�x�@�&�@��@�@�C�@�M�@��@�G�@�%@���@��@�9X@�l�@�7@�D@�1'@�  @��@�C�@��@�v�@��@��@���@���@��@��@�A�@�(�@�b@���@��
@߮@���@�n�@�$�@ݺ^@�p�@�O�@��@���@���@ܣ�@�1'@�\)@�v�@ٲ-@٩�@���@�p�@���@ؓu@��;@�ȴ@�-@���@�hs@��`@�9X@���@ӥ�@�|�@�;d@��H@Ұ!@�=q@Ѳ-@�x�@�?}@��`@д9@Ѓ@�(�@��m@�l�@�+@�@��@ΰ!@Ώ\@�=q@��@��@�O�@���@�z�@� �@�C�@���@ʇ+@��#@ɑh@�/@ȃ@�A�@�1@���@�dZ@�
=@Ɨ�@��@ũ�@ŉ7@���@ģ�@�z�@��@§�@�p�@�V@��u@��@��F@�\)@�o@���@�5?@���@���@��@�Z@��@���@�ƨ@���@�S�@�@��y@��!@�n�@��T@���@�O�@��@�ƨ@�|�@�C�@��@�
=@�@�@��@��y@��H@���@���@�n�@�M�@��7@��@�%@��j@��D@�bN@�1'@��@��@�t�@�
=@��@��@���@��R@�ff@��#@�p�@�%@�r�@��@�C�@��R@�n�@�5?@���@���@��D@�z�@�j@�9X@�  @��w@�|�@�\)@�33@���@��+@�ff@�=q@��@��#@�O�@���@�bN@�A�@�(�@�  @��F@��@��@���@���@�M�@�7L@��@���@�(�@��P@�\)@�o@�n�@�@�x�@��@�9X@��
@�+@���@��\@�~�@�v�@�$�@���@��-@�O�@�V@��u@�b@���@�K�@�+@��@�@��H@��H@��\@�E�@�-@���@�Ĝ@�(�@�+@��@��!@���@�~�@�^5@�J@��-G�O�@�K�@��m@�+@��7@x��@nv�@f@]�h@T�D@I��@D��@=p�@8��@41@,�j@&@!�7@�h@��@ff@�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBBBBBBBBBBB%B1B
=BJBVB\B\BbBoBuB�B�B�B$�B7LB\)B��B�9B�B�B@�BA�BC�BH�BL�BM�BK�BI�B?}B2-B-B9XBS�BM�BG�BE�BF�BF�BH�BJ�BL�BK�BN�BN�BL�BM�BK�BG�BE�BB�B<jB9XB8RB.B�BJB��B�BƨB��B�jB�RB�B��B��B��BW
B�B��B�NB�B�XB��B�oB�=Bx�B^5B8RB,B!�B1B
�B
��B
ŢB
�jB
��B
�B
O�B
5?B
hB
B	�B	�;B	ÖB	��B	��B	�hB	�B	z�B	l�B	ffB	ZB	M�B	C�B	49B	�B	bB	B��B�B�B�fB�/B�B�
B��B��B��B��BƨB��B�wB�dB�LB�3B�-B�RB�dB�dB�qB�jB�jB�wB��B�}B�}BŢBƨBǮBɺB��B��B��B��B��B��B��BɺB��B��B��B��B��B��B�B�)B�5B�)B�/B�5B�5B�5B�HB�TB�ZB�`B�`B�fB�B�B�B��B	  B	B	B	B	1B	
=B	bB	�B	 �B	$�B	$�B	&�B	,B	0!B	49B	49B	33B	B�B	R�B	K�B	J�B	L�B	L�B	M�B	R�B	T�B	YB	YB	XB	XB	[#B	[#B	[#B	]/B	\)B	^5B	`BB	bNB	cTB	ffB	hsB	iyB	jB	jB	l�B	n�B	o�B	r�B	s�B	v�B	w�B	z�B	{�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�JB	�VB	�\B	�\B	�bB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�'B	�B	�B	�B	�'B	�-B	�?B	�FB	�XB	�dB	�dB	�^B	�XB	�RB	�RB	�RB	�RB	�XB	�^B	�jB	�wB	B	ĜB	ŢB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�/B	�;B	�;B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
+B
hB
�B
�B
%�B
(�B
2-B
6FB
@�B
G�B
N�B
T�B
YB
^5B
bNB
ffB
hsB
l�B
p�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB
B#B-B1B4B;BHBMBYBgBzB$�B7&B\B�YB�B��B}B@\BAcBCpBH�BL�BM�BK�BI�B?XB2B,�B97BS�BM�BG�BE~BF�BF�BH�BJ�BL�BK�BN�BN�BL�BM�BK�BG�BE{BBiB<DB91B8,B-�BxB B��B��BƀB�^B�CB�&B��B��B��B�[BV�BgB��B�#B��B�*B��B�DB�Bx�B^
B8&B+�B!�BB
�xB
��B
�uB
�=B
��B
��B
O�B
5B
>B
�B	�iB	�B	�nB	��B	��B	�AB	��B	z�B	lfB	fAB	Y�B	M�B	CqB	4B	�B	AB	�B��B�B�oB�DB�B��B��B��B��BϽBʝBƅB�gB�TB�AB�+B�B�
B�+B�@B�AB�MB�FB�GB�QB�`B�WB�YB�}BƃBǇBɖBˢBͮBβBͯBͭB̧BʟBɓBͮBϻBˠBʙBβB��B��B�B�B�B�B�B�B�B�B�,B�0B�7B�6B�>B�dB�B�B��B��B	�B	�B	�B	B	
B	7B	bB	 �B	$�B	$�B	&�B	+�B	/�B	4B	4B	3B	BaB	R�B	K�B	J�B	L�B	L�B	M�B	R�B	T�B	X�B	X�B	W�B	W�B	Z�B	Z�B	Z�B	\�B	[�B	^B	`B	bB	c$B	f3B	hDB	iFB	jOB	jPB	l\B	ngB	ooB	rB	s�B	v�B	w�B	z�B	{�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�)B	�+B	�0B	�/B	�1B	�<B	�UB	�TB	�ZB	�UB	�ZB	�lB	�mB	�sB	�|B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�2B	�1B	�+B	�#B	�B	�B	�B	� B	�$B	�(B	�6B	�EB	�[B	�jB	�oB	�nB	�mB	�yB	ɇB	˒B	͟B	͞B	ΣB	ΤB	ΧB	ϫB	ϩB	ЯB	бB	гB	ҿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	��B	�B	�B	��B	� B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�,B	�*B	�-B	�*B	�)B	�-B	�)B	�1B	�*B	�*B	�(B	�3B	�0B	�1B	�.B	�6B	�>B	�;B	�AB	�FB	�AB	�AB	�FB	�BB	�BB	�BB	�CB	�BB	�BB	�NB	�PB	�NB	�PB	�MB	�SB	�TB	�OB	�PB	�VB	�TB	�VB	�UB	�TB	�UB	�VB	�VB	�TB	�VB	�VB	�VB	�WB	�VB	�RB	�UB	�PB	�QB	�HB	�IB	�IB	�HB	�IB	�UB	�[B	�`B	�cB	�hB	�iB	�iB	�iB	�oB	�mB	�lB	�qB	�rB	�sB	�sB	�rB	�sB	�xB	�yB	�wB	�rB	�sB	�zB	�xB	�B	�B	�B	�B	�yB	�xB	�xB	�tB	�sB	�nB	�sB	�rB	�rB	�sB	�{B	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�G�O�B
�B
.B
OB
fB
%�B
(�B
1�B
6
B
@LB
GtB
N�B
T�B
X�B
]�B
bB
f.B
h9B
lPB
piB
sB
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451142016080714511420160807145114  AO  ARCAADJP                                                                    20150226221437    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221437  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221437  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145114  IP                  G�O�G�O�G�O�                
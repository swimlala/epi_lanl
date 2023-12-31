CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:13Z AOML 3.0 creation; 2016-08-07T21:17:33Z UW 3.1 conversion     
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
_FillValue                 �  At   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
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
resolution        :�o     �  px   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  zD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221313  20160807141734  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL                A   AO  5285_8895_032                   2C  D   APEX                            6487                            072314                          846 @�9�<M_�1   @�9����@,��l�C��c���v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                     A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy` D�fD�@ D�� D���D�  D�@ D��fD�� D�3D�P D�� DǶf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�\)A{A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B�B�(�B�(�B���B���B�B���B���B���B���B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�C�{Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4�{C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cf�{Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�RDD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�RDy~�D��D�O\D��\D���D�\D�O\D���D��\D�"�D�_\D��\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA�;dA�ȴA�S�A���A�^5A���A�M�A�I�A�\)A�-A���A�9XA�1'A��A��A�bA���A͓uA�9XA��A���A��mA�ȴȦ+A˼jA�VA���A��A�AʾwAʁA�M�A��mA�A�A���A�t�A���Aǩ�A�dZA���AŶFA�p�A�ZA�Q�A�5?A��A���A��AîA�C�A�9XA���A�oA�I�A���A���A�K�A�ĜA��DA���A��mA���A���A�ȴA�jA��A�p�A�Q�A��A�z�A�XA�dZA���A���A�A���A��A�A�-A��A���A{�#Au&�AooAk��Ag7LAbA_��A^(�A\�AX�+AU��AR(�AM��AJQ�AF�A@��A?�-A?p�A=��A;��A6�HA4�9A3VA1�A0r�A-�TA,bNA*1'A)��A)O�A)�A'�A%|�A!�A!l�A!+A!�A"�+A#��A!�
A ffA�A�PAȴA��A33A�jAXAVA��A��AJAoA��AE�A�A��A�A��A&�AK�A7LAoA�RA��A��A
=A/AƨA��A��AS�AVAx�A�AdZA�A��AQ�A�
A�wA�AO�A�A5?AdZA�AA�A��A�A`BA��A�A�A�FA��A�FA��A�7A�AXA;dA
=A
ZA	�^AjA�-A�A
=A�A�A�wA��A  AA�Av�A��AG�A��A1'A�A�wA��Al�A?}A7LA"�A�`A�DAZA=qA�A��AXA ��A �@�@��@�A�@�dZ@�-@�`B@�O�@���@��j@� �@�o@�J@�?}@��u@���@��@��H@�ȴ@�!@�n�@�E�@�7@�9X@�;d@�n�@��-@��^@�/@�z�@��m@�"�@��@�E�@�x�@�9@��m@畁@�;d@�M�@�@�G�@���@�Q�@�dZ@�C�@���@�V@�V@���@�%@��D@�1'@�ƨ@�t�@�
=@�5?@ݙ�@�I�@���@���@�5?@ٺ^@���@��m@�l�@�@�ff@�&�@�bN@�  @ӝ�@�
=@ҟ�@��@љ�@щ7@�x�@���@Ѓ@�;d@��y@�~�@��@ͩ�@��@��/@̛�@�j@�Q�@�|�@��H@���@�G�@Ȭ@Ǯ@�dZ@���@�@őh@�?}@���@�j@�1@þw@�+@�o@�ȴ@�^5@��^@��@��@�z�@�j@�1'@��w@�l�@��@��@���@�$�@��^@�7L@�r�@��;@�C�@��H@��!@�ff@�^5@�E�@�X@�V@��@��@��@���@�@�J@���@�X@���@�(�@��w@�dZ@�"�@��H@�V@���@�7L@�Ĝ@�bN@���@���@�dZ@��!@�J@���@���@�X@�V@�Ĝ@�1'@��;@�t�@�o@��!@�~�@�=q@�@��^@��@�O�@���@��9@�j@�(�@��;@���@��@��+@�J@��@���@���@�p�@���@�j@�Q�@� �@��F@�+@�@��@���@�ff@�^5@�5?@���@���@�1'@���@���@���@�t�@�@��!@�ff@�$�@���@���@���@�G�@��`@�Ĝ@��@�z�@�I�@�9X@��@��@���@�C�@�"�@�
=@���@�J@��@���@��@�O�@��@��`@���@�z�@�A�@���@�|�@�33@���@��+@���@�X@��/@��D@�Z@�A�@�  @��F@��P@�dZ@�C�@���@���@�~�@�=q@��T@�O�@�G�@�X@�&�@��/@��u@�j@� �@���@�|�@�K�@��@���@��@��@�\)@�-@yG�@n�+@e��@^��@U@M�h@E�@=@7��@1��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   A�ƨA�;dA�ȴA�S�A���A�^5A���A�M�A�I�A�\)A�-A���A�9XA�1'A��A��A�bA���A͓uA�9XA��A���A��mA�ȴȦ+A˼jA�VA���A��A�AʾwAʁA�M�A��mA�A�A���A�t�A���Aǩ�A�dZA���AŶFA�p�A�ZA�Q�A�5?A��A���A��AîA�C�A�9XA���A�oA�I�A���A���A�K�A�ĜA��DA���A��mA���A���A�ȴA�jA��A�p�A�Q�A��A�z�A�XA�dZA���A���A�A���A��A�A�-A��A���A{�#Au&�AooAk��Ag7LAbA_��A^(�A\�AX�+AU��AR(�AM��AJQ�AF�A@��A?�-A?p�A=��A;��A6�HA4�9A3VA1�A0r�A-�TA,bNA*1'A)��A)O�A)�A'�A%|�A!�A!l�A!+A!�A"�+A#��A!�
A ffA�A�PAȴA��A33A�jAXAVA��A��AJAoA��AE�A�A��A�A��A&�AK�A7LAoA�RA��A��A
=A/AƨA��A��AS�AVAx�A�AdZA�A��AQ�A�
A�wA�AO�A�A5?AdZA�AA�A��A�A`BA��A�A�A�FA��A�FA��A�7A�AXA;dA
=A
ZA	�^AjA�-A�A
=A�A�A�wA��A  AA�Av�A��AG�A��A1'A�A�wA��Al�A?}A7LA"�A�`A�DAZA=qA�A��AXA ��A �@�@��@�A�@�dZ@�-@�`B@�O�@���@��j@� �@�o@�J@�?}@��u@���@��@��H@�ȴ@�!@�n�@�E�@�7@�9X@�;d@�n�@��-@��^@�/@�z�@��m@�"�@��@�E�@�x�@�9@��m@畁@�;d@�M�@�@�G�@���@�Q�@�dZ@�C�@���@�V@�V@���@�%@��D@�1'@�ƨ@�t�@�
=@�5?@ݙ�@�I�@���@���@�5?@ٺ^@���@��m@�l�@�@�ff@�&�@�bN@�  @ӝ�@�
=@ҟ�@��@љ�@щ7@�x�@���@Ѓ@�;d@��y@�~�@��@ͩ�@��@��/@̛�@�j@�Q�@�|�@��H@���@�G�@Ȭ@Ǯ@�dZ@���@�@őh@�?}@���@�j@�1@þw@�+@�o@�ȴ@�^5@��^@��@��@�z�@�j@�1'@��w@�l�@��@��@���@�$�@��^@�7L@�r�@��;@�C�@��H@��!@�ff@�^5@�E�@�X@�V@��@��@��@���@�@�J@���@�X@���@�(�@��w@�dZ@�"�@��H@�V@���@�7L@�Ĝ@�bN@���@���@�dZ@��!@�J@���@���@�X@�V@�Ĝ@�1'@��;@�t�@�o@��!@�~�@�=q@�@��^@��@�O�@���@��9@�j@�(�@��;@���@��@��+@�J@��@���@���@�p�@���@�j@�Q�@� �@��F@�+@�@��@���@�ff@�^5@�5?@���@���@�1'@���@���@���@�t�@�@��!@�ff@�$�@���@���@���@�G�@��`@�Ĝ@��@�z�@�I�@�9X@��@��@���@�C�@�"�@�
=@���@�J@��@���@��@�O�@��@��`@���@�z�@�A�@���@�|�@�33@���@��+@���@�X@��/@��D@�Z@�A�@�  @��F@��P@�dZ@�C�@���@���@�~�@�=q@��T@�O�@�G�@�X@�&�@��/@��u@�j@� �@���@�|�@�K�@��@���G�O�@��@�\)@�-@yG�@n�+@e��@^��@U@M�h@E�@=@7��@1��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB	P�B	Q�B	^5B	m�B	z�B	�\B	��B
'�B
bNB
��B
�BBuB`BB��B�?B�jB�}B�)B�fB�TB�HB�BBB��B�B�5B�5B�fB��B��BB1BVB�B%�B<jBK�BR�B~�B�+B�\B��B��B��B��B��B��B�B�B�B�ZBŢB�B�B�B�{B�3B��B�PBl�B.BB��B��B��B�B�B�B�
BĜB��B�=Bm�BYB>wB0!B'�BB
�)B
�VB
E�B	�B	ƨB	��B	��B	~�B	iyB	\)B	R�B	H�B	33B	%�B	oB	1B��B�B�#B�B�B��BɺB�wB�RB�RB�qBB��B�B�#B�HB�NB�B��B�B�/B�/B��B	�B	49B	J�B	B�B	F�B	Q�B	T�B	P�B	Q�B	H�B	<jB	H�B	O�B	R�B	S�B	XB	\)B	^5B	`BB	e`B	gmB	jB	t�B	� B	�=B	�JB	�VB	�PB	�B	�+B	�PB	�uB	��B	�RB	��B	�B
B
�B
�B
�B
�B
�B
�B
 �B
$�B
&�B
%�B
!�B
�B
�B
�B
bB

=B
B
  B
  B

=B
B
  B	��B
B
1B
+B
%B
B	�B	�TB	�)B	�
B	��B	��B	��B	��B	�B	��B	�
B	�HB	�fB	�B	�B	�B	�B	�sB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
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
JB
JB
JB
JB
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
bB
hB
hB
oB
oB
hB
oB
oB
oB
uB
oB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
$�B
'�B
/B
5?B
;dB
?}B
E�B
K�B
O�B
T�B
[#B
bNB
dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   B	P�B	Q�B	^B	m{B	z�B	�DB	νB
'�B
b+B
��B
�BQB`B�B�B�@B�VB��B�>B�-B� B�iB�B�B��B�WB�B�B�<B��B��B�B
B/BfB%�B<ABK�BR�B~�B�B�4B�rB��B��B��B��B��B��B��B��B�/B�zB��B�`B�B�NB�B�ZB�%BlaB-�B �BʔBͥB��B��B�B�]B��B�pB��B�BmcBX�B>IB/�B'�B�B
��B
�)B
EzB	�B	ƀB	��B	�aB	~�B	iTB	\B	R�B	H�B	3B	%�B	KB	B��B�]B��B��B��BϼBɖB�UB�.B�/B�MB�lBδB��B��B�"B�&B�kB��B�}B�
B�
B��B	kB	4B	J�B	BcB	F|B	Q�B	T�B	P�B	Q�B	H�B	<?B	H�B	O�B	R�B	S�B	W�B	[�B	^	B	`B	e3B	g@B	jQB	t�B	�B	�B	�B	�'B	�"B	��B	��B	�"B	�DB	��B	� B	ϮB	�kB
�B
fB
lB
tB
zB
�B
�B
 �B
$�B
&�B
%�B
!�B
~B
fB
TB
0B

	B
�B	��B	��B

	B
�B	��B	��B
�B
�B
�B
�B
�B	�yB	�"B	��B	��B	��B	��B	��B	ҾB	��B	��B	��B	�B	�2B	�]B	�}B	�mB	�YB	�AB	�2B	�5B	�1B	�<B	�@B	�KB	�LB	�IB	�^B	�_B	�WB	�XB	�RB	�EB	�9B	�4B	�SB	�;B	�8B	�FB	�RB	�IB	�pB	�vB	�wB	�pB	�cB	�eB	�dB	�eB	�[B	�YB	�^B	�^B	�cB	�eB	�eB	�hB	�\B	�PB	�VB	�_B	�eB	�iB	�]B	�[B	�WB	�RB	�VB	�WB	�cB	�cB	�cB	�eB	�]B	�VB	�XB	�UB	�VB	�]B	�gB	�hB	�nB	�oB	�wB	�{B	�zB	�B	�B	�B	�B	�xB	�vB	�jB	�ZB	�PB	�ZB	�]B	�\B	�dB	�[B	�VB	�UB	�OB	�HB	�IB	�IB	�IB	�KB	�TB	�VB	�VB	�bB	�cB	�dB	�cB	�jB	�dB	�cB	�cB	�gB	�fB	�gB	�kB	�hB	�`B	�aB	�aB	�lB	�eB	�hB	�gB	�eB	�nB	�tB	�tB	�sB	�lB	�tB	�yB	�~B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	 B
�B

B

B

B

B
B
	B
B
B
B
B
B
B
B
B
B
B
B
$B
#B
$B
#B
%B
%B
"B
+B
1B
.B
3B
6B
/B
5B
6B
4B
=B
6B
>B
?B
=B
AB
AB
@B
?B
AB
AB
AB
AB
BB
HB
JB
GB
IB
PB
VB
\B
[B
]B
]B
cB
`B
hB
gB
fB
gB
fB
gB
hB
uB
nB
mB
nB
oB
zB
yB
zB
xB
wB
xB
B
�B
�B
�B
 �B
�B
 �B
 �B
!�B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�G�O�B
$�B
'�B
.�B
5B
;,B
?CB
EjB
K�B
O�B
T�B
Z�B
bB
d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417342016080714173420160807141734  AO  ARCAADJP                                                                    20150226221313    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221313  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221313  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141734  IP                  G�O�G�O�G�O�                
CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-19T09:16:48Z AOML 3.0 creation; 2016-08-07T21:36:36Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150619091648  20160807143636  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               7A   AO  5286_8897_055                   2C  D   APEX                            6531                            072314                          846 @�Y��r�
1   @�Y�W?�@1��l�C��c�(�\1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    7A   B   B   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�  D�0 D�i�D���D�  D�VfD�� D�� D���D�33D�y�D�ɚD�	�D�6fDڀ D��3D�  D�9�D� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�ffA33A#33AC33Ac33A���A���A���A���A���A�fgA�fgA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB̙�BЙ�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:L�C<L�C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZL�C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=3D=��D>3D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�gDy�3D�fD�6fD�p D�� D�&fD�\�D��fD��fD�3D�9�D�� D�� D� D�<�DچfD�ٙD�fD�@ D�fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�33A�5?A�9XA�7LA�(�A�33A��A��;A���AҼjA���A�ȴAҏ\A�\)A�ZA�VA�K�A��A�JA�O�A�C�A��AΣ�A�oA�\)A���A�$�Aʗ�Aɗ�Aȥ�AǛ�A��`A�&�AŁA�A�+A���A���AÁA�/A´9A��A���A���A��\A�~�A���A�9XA���A��A��mA��;A�E�A�VA�7LA��A���A�A��A�;dA��\A��+A���A��9A��uA�7LA�C�A�?}A�E�A�A�^5A��RA��hA���A�1A��A��uA�?}A��A���A��\A�VA�|�A�bNA�ƨA�S�A�dZA��A��\A�A��PA�r�A�A�n�A�5?A�A�M�A���A��\A��/A�t�A�^A{VAsXAn{Am"�AkO�Af�!Ab��A_\)AZ��AX��AX9XAW�hAV9XAS��AQx�AO�TAL��AJA�AH�AF��AE�ACl�AB5?AA%A?A>�!A<�uA:��A9S�A9�A89XA77LA5�7A4A�A3�A0JA.  A-+A,�jA,ZA+ƨA+\)A*��A(�HA'K�A&��A%��A#�hA"{A!�mA"1AS�AbNA��A�!A1AC�A��AM�A�^A�A��AJA7LA-A�PA9XA�/A1AO�A~�A�7A�`AG�A
jA9XA�^A��AffA`BA�A�^A�7A|�A��A�\A�A z�@��@�$�@��H@��@���@�j@��@���@���@�\)@�ȴ@���@��@���@�+@�R@�@�^5@�^@��@���@��`@땁@��@�ff@�r�@�S�@��@�9X@���@�^5@�{@�`B@ߥ�@���@�E�@��@��#@ݲ-@���@�ƨ@۝�@�dZ@�+@ڟ�@��@١�@�`B@ؼj@��
@֏\@�p�@��`@Լj@ԛ�@�I�@��
@�dZ@��@�$�@���@���@Ѻ^@љ�@�1'@�b@�b@��@��@� �@Ϯ@�C�@�o@��H@Ο�@�5?@�O�@�r�@ˍP@��@�^5@��@ɺ^@��@�z�@�A�@� �@Ǖ�@�|�@��@�E�@��@š�@�hs@�O�@�hs@���@� �@�ƨ@ÍP@�K�@�@���@�`B@�Ĝ@��D@�b@���@�t�@�C�@�+@�
=@��@���@��@���@�bN@���@���@�C�@�+@���@�=q@�M�@�V@�M�@��^@�X@���@��j@�9X@��w@��@�t�@�C�@�C�@�o@��@���@��+@�=q@�$�@��T@�hs@��@���@��/@��j@�j@��
@��@��@�o@�n�@��#@���@��7@��^@��@��-@���@�Ĝ@�j@�b@�S�@��!@�5?@��@�A�@���@��F@�S�@�
=@��H@���@�M�@�=q@��T@��h@�x�@�O�@���@���@���@�j@�1'@�1@���@��@�|�@�S�@�K�@�"�@��H@�^5@�-@�@���@��j@��@��@��@��@��@���@�|�@�S�@�+@���@��@��F@���@�o@�=q@�@���@�V@��@��/@��j@�z�@��@��m@���@��w@��P@�dZ@�C�@�"�@�
=@��+@���@��^@��^@�x�@�G�@�%@��`@��@�bN@��@�t�@�S�@��!@�E�@���@�@��@�7L@���@��D@��@��@�r�@��;@�K�@�C�@��@���@�E�@�@��h@�X@�?}@��@�%@��/@���@��9@�1'@��m@���@�33@�
=@��H@�ȴ@���@�$�@��#@�x�@���@�bN@�(�@��m@�ƨ@�|�@�;d@�
=@��H@��!@�v�@�E�@�5?@�{@�@��#@��7@��;@�E�@�n�@~v�@v5?@k�
@_�@Xb@P �@HA�@A�7@9��@2�@,��@'\)@!�^@`B@��@j@�9@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�+A�33A�5?A�9XA�7LA�(�A�33A��A��;A���AҼjA���A�ȴAҏ\A�\)A�ZA�VA�K�A��A�JA�O�A�C�A��AΣ�A�oA�\)A���A�$�Aʗ�Aɗ�Aȥ�AǛ�A��`A�&�AŁA�A�+A���A���AÁA�/A´9A��A���A���A��\A�~�A���A�9XA���A��A��mA��;A�E�A�VA�7LA��A���A�A��A�;dA��\A��+A���A��9A��uA�7LA�C�A�?}A�E�A�A�^5A��RA��hA���A�1A��A��uA�?}A��A���A��\A�VA�|�A�bNA�ƨA�S�A�dZA��A��\A�A��PA�r�A�A�n�A�5?A�A�M�A���A��\A��/A�t�A�^A{VAsXAn{Am"�AkO�Af�!Ab��A_\)AZ��AX��AX9XAW�hAV9XAS��AQx�AO�TAL��AJA�AH�AF��AE�ACl�AB5?AA%A?A>�!A<�uA:��A9S�A9�A89XA77LA5�7A4A�A3�A0JA.  A-+A,�jA,ZA+ƨA+\)A*��A(�HA'K�A&��A%��A#�hA"{A!�mA"1AS�AbNA��A�!A1AC�A��AM�A�^A�A��AJA7LA-A�PA9XA�/A1AO�A~�A�7A�`AG�A
jA9XA�^A��AffA`BA�A�^A�7A|�A��A�\A�A z�@��@�$�@��H@��@���@�j@��@���@���@�\)@�ȴ@���@��@���@�+@�R@�@�^5@�^@��@���@��`@땁@��@�ff@�r�@�S�@��@�9X@���@�^5@�{@�`B@ߥ�@���@�E�@��@��#@ݲ-@���@�ƨ@۝�@�dZ@�+@ڟ�@��@١�@�`B@ؼj@��
@֏\@�p�@��`@Լj@ԛ�@�I�@��
@�dZ@��@�$�@���@���@Ѻ^@љ�@�1'@�b@�b@��@��@� �@Ϯ@�C�@�o@��H@Ο�@�5?@�O�@�r�@ˍP@��@�^5@��@ɺ^@��@�z�@�A�@� �@Ǖ�@�|�@��@�E�@��@š�@�hs@�O�@�hs@���@� �@�ƨ@ÍP@�K�@�@���@�`B@�Ĝ@��D@�b@���@�t�@�C�@�+@�
=@��@���@��@���@�bN@���@���@�C�@�+@���@�=q@�M�@�V@�M�@��^@�X@���@��j@�9X@��w@��@�t�@�C�@�C�@�o@��@���@��+@�=q@�$�@��T@�hs@��@���@��/@��j@�j@��
@��@��@�o@�n�@��#@���@��7@��^@��@��-@���@�Ĝ@�j@�b@�S�@��!@�5?@��@�A�@���@��F@�S�@�
=@��H@���@�M�@�=q@��T@��h@�x�@�O�@���@���@���@�j@�1'@�1@���@��@�|�@�S�@�K�@�"�@��H@�^5@�-@�@���@��j@��@��@��@��@��@���@�|�@�S�@�+@���@��@��F@���@�o@�=q@�@���@�V@��@��/@��j@�z�@��@��m@���@��w@��P@�dZ@�C�@�"�@�
=@��+@���@��^@��^@�x�@�G�@�%@��`@��@�bN@��@�t�@�S�@��!@�E�@���@�@��@�7L@���@��D@��@��@�r�@��;@�K�@�C�@��@���@�E�@�@��h@�X@�?}@��@�%@��/@���@��9@�1'@��m@���@�33@�
=@��H@�ȴ@���@�$�@��#@�x�@���@�bN@�(�@��m@�ƨ@�|�@�;d@�
=@��H@��!@�v�@�E�@�5?@�{@�@��#G�O�@��;@�E�@�n�@~v�@v5?@k�
@_�@Xb@P �@HA�@A�7@9��@2�@,��@'\)@!�^@`B@��@j@�9@�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�qB	�wB	�wB	�wB	�wB	�jB	�qB	�^B	�9B	�9B	�'B	�-B	�-B	�B	�B	�B	�B	�B	�B	�9B	�)B
|�B\B2-B:^B49B1'B9XB�B��B��B��B�B8RBI�B\)Bo�Bz�B�B�=B�uB��B��B�uB��BǮBɺB��B�?B�ZB�B��BPB�B�B	7B�B\B7LBC�BH�BO�BW
BYB[#BW
BO�B@�B33B�B
=BBC�BR�B<jB,B'�B�B�mB�bBL�B/B�B\B�B��B��B�B{�Bt�BjBR�B:^B,B�B
��B
�dB
m�B
"�B	�NB	��B	ǮB	�RB	�oB	bNB	M�B	]/B	Q�B	2-B	oB��B�mB�HB�;B�)B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�/B�HB�NB�5B�ZB�mB�sB�fB�ZB�NB�;B�;B�;B�;B�B��B��B��B�)B��B��B��BɺBȴBŢBŢBŢBƨBŢBȴBɺB��B��B��B��B�B�B�B�B�
B�/B�BB�)B��B��B��BɺBĜBĜB��B�sB�B�B�B�B�yB�BB��B	B	B	%B	VB	VB	
=B	1B		7B	B��B�B	  B		7B	DB	DB	
=B	1B	B	B	B��B��B��B��B��B��B��B��B��B��B��B	B	+B	
=B	DB	
=B	
=B		7B	hB	hB	oB	uB	{B	{B	�B	�B	�B	#�B	+B	0!B	33B	6FB	8RB	;dB	?}B	B�B	C�B	F�B	F�B	F�B	F�B	G�B	F�B	G�B	G�B	G�B	H�B	K�B	M�B	O�B	P�B	R�B	S�B	VB	[#B	\)B	_;B	bNB	e`B	e`B	ffB	gmB	hsB	hsB	hsB	k�B	r�B	x�B	x�B	x�B	y�B	y�B	y�B	|�B	�B	�B	�%B	�%B	�+B	�7B	�DB	�PB	�VB	�\B	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�9B	�9B	�FB	�^B	�jB	�jB	�qB	�qB	�wB	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�sB	�mB	�mB	�fB	�`B	�ZB	�fB	�mB	�mB	�fB	�fB	�fB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
B
%B
%B
%B
%B
+B
%B
%B
1B

=B
hB
�B
�B
$�B
'�B
/B
6FB
;dB
A�B
G�B
M�B
R�B
YB
]/B
bNB
ffB
iyB
n�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�mB	�tB	�uB	�tB	�uB	�fB	�jB	�YB	�5B	�6B	�%B	�)B	�(B	�B	��B	��B	��B	��B	� B	�3B	�!B
|�BMB2B:QB4)B1B9FB�B��B��B��B�B8>BI�B\Bo�Bz�B��B�-B�cB��B�tB�eB��BǟBɫB�uB�1B�NB�B��B@B�B�B	%B�BNB7>BC�BH�BO�BV�BY	B[BV�BO�B@sB3"B{B
.B�BC�BR�B<_B+�B'�B~B�_B�QBL�B/B�BIB�kB˲B��B�B{�Bt�BjpBR�B:OB+�BsB
��B
�SB
m�B
"�B	�@B	��B	ǧB	�EB	�dB	bFB	M�B	]'B	Q�B	2'B	jB��B�iB�GB�8B�&B�	B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�+B�AB�JB�/B�SB�iB�nB�aB�VB�JB�4B�7B�7B�4B�B��B��B��B�$B��B��B��BɷBȰBŞBŞBŜBƣBŝBȰBɴB��B��B��B��B��B��B��B�	B�B�(B�;B�!B��B��B��BɶBĖBĘB��B�kB�B�B�B�B�pB�9B��B	B	B	B	MB	KB	
5B	'B		-B	B��B�B��B		.B	:B	<B	
5B	)B	B	B	 �B��B��B��B��B��B��B��B��B��B��B��B	 �B	!B	
2B	9B	
2B	
3B		.B	`B	]B	dB	lB	qB	pB	�B	�B	�B	#�B	*�B	0B	3&B	69B	8FB	;WB	?qB	B�B	C�B	F�B	F�B	F�B	F�B	G�B	F�B	G�B	G�B	G�B	H�B	K�B	M�B	O�B	P�B	R�B	S�B	U�B	[B	\B	_.B	b?B	eTB	eRB	fYB	g_B	heB	hfB	hgB	kwB	r�B	x�B	x�B	x�B	y�B	y�B	y�B	|�B	��B	�B	�B	�B	�B	�'B	�3B	�@B	�GB	�PB	�LB	�RB	�^B	�lB	�qB	�rB	�uB	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�%B	�(B	�(B	�4B	�KB	�XB	�YB	�`B	�`B	�eB	�xB	�}B	ČB	őB	ƗB	ƗB	ƕB	ȣB	ɩB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ʰB	ȢB	ɧB	ɪB	ʯB	ʰB	ʰB	˵B	˶B	˴B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�B	�B	�B	�B	�B	�#B	�$B	�"B	�$B	�'B	�(B	�0B	�5B	�bB	�ZB	�[B	�TB	�NB	�GB	�UB	�XB	�YB	�QB	�UB	�VB	�OB	�PB	�PB	�LB	�SB	�TB	�SB	�TB	�YB	�fB	�fB	�hB	�gB	�jB	�kB	�iB	�uB	�qB	�sB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
B
�B
 B
B
B
B
B
B
B
B

B
B
B
B
B
B
B
B
G�O�B

*B
TB
rB
�B
$�B
'�B
/B
61B
;NB
AtB
G�B
M�B
R�B
YB
]B
b;B
fSB
icB
n�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436362016080714363620160807143636  AO  ARCAADJP                                                                    20150619091648    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150619091648  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150619091648  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143636  IP                  G�O�G�O�G�O�                
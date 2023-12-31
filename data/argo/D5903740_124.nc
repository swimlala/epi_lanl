CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-18T02:15:29Z AOML 3.0 creation; 2016-06-01T00:08:26Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150818021529  20160531170826  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               |A   AO  4055_7112_124                   2C  D   APEX                            5374                            041511                          846 @�hm}� 1   @�hn��@:#n��P�dOC��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    |A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B7��B@  BH  BPffBXffB`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�	�D�C3D�i�D���D��D�33D���D�ٚD�	�D�FfD�|�D�ٚD�	�D�33Dڙ�D�� D��fD�&fD� D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Tz�@�p�@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�BzB�B!�B)�B1�B9G�BA�BI�BRzBZzBa�BiG�Bq�By�B��
B��
B��
B��
B��
B�
=B��
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
=B��
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
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cj�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dy��D�D�P�D�wD��>D�>D�@�D��D��D�D�S�D��>D��D�D�@�DڧD��qD��D�3�D�qD�W111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�S�A�VA�VA�ZA�ZA�ZA�^5A�`BA�dZA�bNA�hsA�jA�hsA�|�AڋDAڛ�Aڥ�Aڴ9Aک�A�~�A�~�A�bNA٧�A�r�A�-A�9XA��A�1A���A�n�A�ȴA�l�A�9XA�`BA�
=A�bNA�A�A��9A��\A��;A�S�A��#A���A�ȴA���A�7LA�G�A�=qA���A��A�^5A���A��A���A�5?A���A�v�A���A��
A���A�\)A���A���A��hA��A���A��^A��!A��uA���A�\)A���A�n�A�XA���A��FA�ZA�Q�A���A��A�1'A���A��uA�33A�(�A��/A��PA�A�$�A��\A�C�A���A��mA��+A�bNA�{A��/A��hA�%A��\A�E�A�+A��hA�ȴA�XA��A~1'A|5?AzȴAy��Aw��Av�Au��Au��Aul�At�Aq�^Aq�Aq&�ApVAn�`Am��Am7LAl��Aj��Ai/AgG�Ae�Ad~�Ac�Ab�A`�A_%A\�+AZ��AYp�AX�AXAV�HAU
=ASdZAR1'AQdZAO��AM\)AL�AKAJ�AI�AFbNAE�AEK�ADffAC7LAA�hA@��A@A�A>��A>Q�A=ƨA<�/A<v�A;hsA:v�A:  A8�HA8�A7�wA6��A61A5��A5�A41A2��A1�A0��A/�A.�A.�A-�^A-VA,�A+�FA+oA*r�A*E�A)��A)XA'?}A$�9A#��A"�/A!��A!A A �AVAM�AVAM�AJA��A�hAXA�A�jA�uA��A �A�HAbNA�AdZA-A�`AM�A��A�A�A�RA��A�A1'Ax�A?}A"�A�A�+A
=A
 �A�A=qA�A/A�Ar�A�7AbNA�FA �A jA 1'@���@�K�@��@�7L@�t�@���@���@�&�@�  @���@�F@�+@��@�E�@�u@���@�?}@�(�@�ȴ@��#@���@��@��@��`@��@�j@�|�@��H@�V@��@�7@�V@�Z@�33@�X@ۮ@ۥ�@���@���@�(�@��@���@�1'@�;d@ҧ�@�-@�x�@�1@��@�=q@�X@���@��@˝�@�n�@ɩ�@���@ȃ@Ǿw@�\)@�+@Ɵ�@���@ŉ7@�7L@ċD@�+@��@��9@��y@���@���@��@�Z@��P@��@�^5@��/@�j@�@��@��9@���@�@���@�ff@��/@���@�5?@�x�@�O�@��@��D@�1@��@�dZ@�"�@��y@�n�@��7@���@�t�@��@�v�@���@���@�9X@�1'@�1'@�1'@�I�@�1@�X@� �@�dZ@��+@�V@�E�@�5?@�-@�$�@��@��@�{@��@��-@���@�`B@��@���@�Ĝ@���@�z�@�1'@�l�@�n�@���@�~�@��-@��/@�r�@�9X@��@���@�1@���@��h@�J@�-@��@��7@���@��@�`B@���@�j@�b@�  @��F@�+@���@���@�E�@���@���@�hs@��@���@��/@��D@�z�@� �@��
@��@��y@�E�@�J@��^@�/@�Ĝ@��9@��@�z�@�bN@��@�\)@�
=@��R@���@���@�~�@���@�/@���@�r�@��@��y@��!@�n�@��@�@���@���@��@�j@�j@�A�@�1'@�(�@�;@��@+@~�+@}��@}`B@}V@|�@|�/@|�D@{��@z��@y��@yG�@y7L@x�`@w�@vȴ@vff@v@u�h@u�@u?}@t�D@t�@s�F@so@r��@r�\@rM�@r=q@r�@q�#@q�^@qx�@p��@p�u@p�u@n�@e�@^��@Y�@T1@L1@J^5@B�\@=�T@97L@1&�@,��@'\)@"n�@E�@�9@��@��@/@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�VA�VA�ZA�ZA�ZA�^5A�`BA�dZA�bNA�hsA�jA�hsA�|�AڋDAڛ�Aڥ�Aڴ9Aک�A�~�A�~�A�bNA٧�A�r�A�-A�9XA��A�1A���A�n�A�ȴA�l�A�9XA�`BA�
=A�bNA�A�A��9A��\A��;A�S�A��#A���A�ȴA���A�7LA�G�A�=qA���A��A�^5A���A��A���A�5?A���A�v�A���A��
A���A�\)A���A���A��hA��A���A��^A��!A��uA���A�\)A���A�n�A�XA���A��FA�ZA�Q�A���A��A�1'A���A��uA�33A�(�A��/A��PA�A�$�A��\A�C�A���A��mA��+A�bNA�{A��/A��hA�%A��\A�E�A�+A��hA�ȴA�XA��A~1'A|5?AzȴAy��Aw��Av�Au��Au��Aul�At�Aq�^Aq�Aq&�ApVAn�`Am��Am7LAl��Aj��Ai/AgG�Ae�Ad~�Ac�Ab�A`�A_%A\�+AZ��AYp�AX�AXAV�HAU
=ASdZAR1'AQdZAO��AM\)AL�AKAJ�AI�AFbNAE�AEK�ADffAC7LAA�hA@��A@A�A>��A>Q�A=ƨA<�/A<v�A;hsA:v�A:  A8�HA8�A7�wA6��A61A5��A5�A41A2��A1�A0��A/�A.�A.�A-�^A-VA,�A+�FA+oA*r�A*E�A)��A)XA'?}A$�9A#��A"�/A!��A!A A �AVAM�AVAM�AJA��A�hAXA�A�jA�uA��A �A�HAbNA�AdZA-A�`AM�A��A�A�A�RA��A�A1'Ax�A?}A"�A�A�+A
=A
 �A�A=qA�A/A�Ar�A�7AbNA�FA �A jA 1'@���@�K�@��@�7L@�t�@���@���@�&�@�  @���@�F@�+@��@�E�@�u@���@�?}@�(�@�ȴ@��#@���@��@��@��`@��@�j@�|�@��H@�V@��@�7@�V@�Z@�33@�X@ۮ@ۥ�@���@���@�(�@��@���@�1'@�;d@ҧ�@�-@�x�@�1@��@�=q@�X@���@��@˝�@�n�@ɩ�@���@ȃ@Ǿw@�\)@�+@Ɵ�@���@ŉ7@�7L@ċD@�+@��@��9@��y@���@���@��@�Z@��P@��@�^5@��/@�j@�@��@��9@���@�@���@�ff@��/@���@�5?@�x�@�O�@��@��D@�1@��@�dZ@�"�@��y@�n�@��7@���@�t�@��@�v�@���@���@�9X@�1'@�1'@�1'@�I�@�1@�X@� �@�dZ@��+@�V@�E�@�5?@�-@�$�@��@��@�{@��@��-@���@�`B@��@���@�Ĝ@���@�z�@�1'@�l�@�n�@���@�~�@��-@��/@�r�@�9X@��@���@�1@���@��h@�J@�-@��@��7@���@��@�`B@���@�j@�b@�  @��F@�+@���@���@�E�@���@���@�hs@��@���@��/@��D@�z�@� �@��
@��@��y@�E�@�J@��^@�/@�Ĝ@��9@��@�z�@�bN@��@�\)@�
=@��R@���@���@�~�@���@�/@���@�r�@��@��y@��!@�n�@��@�@���@���@��@�j@�j@�A�@�1'@�(�@�;@��@+@~�+@}��@}`B@}V@|�@|�/@|�D@{��@z��@y��@yG�@y7L@x�`@w�@vȴ@vff@v@u�h@u�@u?}@t�D@t�@s�F@so@r��@r�\@rM�@r=q@r�@q�#@q�^@qx�@p��@p�u@p�u@n�@e�@^��@Y�@T1@L1@J^5@B�\@=�T@97L@1&�@,��@'\)@"n�@E�@�9@��@��@/@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB:^B;dB:^B;dB:^B:^B<jB<jB=qB=qB?}BC�BE�BP�BhsB��B�!B�}BƨB�)B�/B�B��B�#B�B��B+BJB{B$�BhsBo�Bp�Bq�Bt�Bp�BffB^5BT�BXB[#B\)B[#BS�B(�B�BJB�B�}B�qB�}B��BJBhBbBDBB��B��B�B�sB�/B��B��BȴB�FB��B��B�1B{�Br�B]/BD�B33B,B&�B�B\BB�B��B��B�1B�1B�PB�DB�1B�Bu�Bn�BhsBaHBN�B:^B)�B�BuBJB  B
��B
��B
��B
�B
�fB
�;B
�B
ǮB
�-B
��B
��B
�PB
�+B
�B
|�B
w�B
l�B
B�B
;dB
A�B
@�B
7LB
(�B
"�B
�B
B	�B	�BB	��B	ȴB	B	�FB	��B	��B	�=B	� B	u�B	o�B	jB	bNB	YB	O�B	J�B	E�B	;dB	,B	$�B	�B	{B��B�mB�fB�TB�;B�)B�5B�5B�B��B��B��B��B��BȴBǮBǮBŢBÖB��B�}B�wB�qB�^B�?B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�7B�B� B|�By�Bv�Br�Bn�Bl�Bk�BgmBffBe`BdZBdZBcTBbNBaHB_;B]/BZBZBZBYBXBVBVBS�BS�BR�BR�BT�BVBVBVBYBYBYBXBT�BVBS�BS�BS�BN�BJ�BL�BJ�BF�B@�B:^B49B2-B33B33B33B8RB8RB7LB6FB5?B49B1'B.B2-B49B33B/B-B1'B6FB9XB8RB5?B33B8RBA�BF�BJ�BO�BO�BO�BP�BQ�BR�BR�BS�BT�BYB\)B\)B^5BaHBbNBcTBgmBhsBiyBiyBiyBjBl�Bl�Bl�Bm�Bm�Bm�Bm�Bo�Bo�Bp�Bp�Bq�Br�Br�Bs�Bs�Bs�Br�Bq�Bp�Bo�Bo�Bo�Bq�Bs�Bt�Bt�Bu�Bt�Bq�Bm�BdZB`BB[#BW
BT�BS�BR�BP�BO�BS�BXB_;B`BBbNBffBgmBgmBhsBiyBjBk�Bn�Bo�Bt�Bx�B}�B~�B�B�+B�+B�+B�+B�%B�%B�+B�+B�7B�VB�bB�bB�hB�hB�hB�hB�hB�hB�oB�{B��B��B��B��B��B��B��B��B�B�B�3B�RB�qB�qB�qB�qB�qB�wBB��B��B�B�#B�B�5B�TB�ZB�mB�sB�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	B	+B	1B		7B	
=B	
=B	
=B	DB	\B	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	'�B	(�B	)�B	)�B	)�B	1'B	49B	7LB	8RB	8RB	9XB	9XB	9XB	;dB	;dB	=qB	?}B	B�B	E�B	G�B	J�B	J�B	K�B	N�B	Q�B	VB	W
B	VB	W
B	[#B	`BB	aHB	cTB	ffB	ffB	gmB	iyB	jB	l�B	o�B	p�B	p�B	q�B	r�B	u�B	y�B	z�B	y�B	y�B	y�B	{�B	�B	�B	B	�)B	�B	��B
�B
!�B
(�B
/B
9XB
@�B
G�B
M�B
S�B
[#B
_;B
e`B
iyB
n�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B:>B;BB:<B;EB:>B:>B<JB<HB=PB=RB?\BCrBE�BP�BhSB�uB��B�^BƅB�B�B��B��B�B�B��BB,B[B$�BhUBo�Bp�Bq�Bt�Bp�BfJB^BT�BW�B[B\	B[ BS�B(�BmB$B�ZB�YB�KB�XB��B(BAB>B"B�B��B��B�fB�PB�B��BϼBȑB�!B��B�tB�B{�Br�B]BDvB3	B+�B&�B�B2B �B�kB̦B�qB�B�	B�)B�B�
B��Bu�BnlBhIBaBN�B:4B)�BjBKB B
��B
��B
��B
��B
�B
�?B
�B
��B
ǄB
�B
��B
�yB
�(B
�B
��B
|�B
w�B
ldB
BkB
;@B
AbB
@\B
7%B
(�B
"�B
wB
�B	�tB	�B	��B	ȑB	�lB	�#B	��B	�zB	�B	�B	u�B	o{B	j^B	b-B	X�B	O�B	J�B	E�B	;EB	+�B	$�B	�B	_B��B�OB�HB�9B�B�
B�B�B��B��BͷBͷBλBμBșBǑBǑBņB�yB�mB�bB�^B�TB�AB�#B�B��B��B��B��B��B��B��B��B��B��B��B��B�~B�eB�B��B�B|�By�Bv�Br�Bn~BlsBkkBgRBfNBeGBd?Bd@Bc8Bb2Ba/B_!B]BZBZBZBX�BW�BU�BU�BS�BS�BR�BR�BT�BU�BU�BU�BX�BX�BX�BW�BT�BU�BS�BS�BS�BN�BJ�BL�BJ�BF�B@iB:DB4 B2B3B3B3B87B87B72B6*B5'B4!B0�B-�B2B4B3B/ B,�B1B6+B9;B86B5"B3B87BAkBF�BJ�BO�BO�BO�BP�BQ�BR�BR�BS�BT�BX�B\B\B^Ba)Bb/Bc4BgNBhWBiYBi[BiZBjcBllBllBllBmqBmrBmrBmpBoBoBp�Bp�Bq�Br�Br�Bs�Bs�Bs�Br�Bq�Bp�Bo}Bo~Bo�Bq�Bs�Bt�Bt�Bu�Bt�Bq�BmnBd7B`!B[BV�BT�BS�BR�BP�BO�BS�BW�B_B`"Bb-BfEBgOBgLBhRBiXBj]BkeBnvBo{Bt�Bx�B}�B~�B��B�
B�B�B�B��B�B�
B�	B�B�0B�?B�>B�EB�GB�FB�GB�FB�CB�MB�VB�\B�vB��B��B��B��B��B��B��B��B�B�+B�KB�MB�KB�IB�KB�QB�mBˡB��B��B��B��B�B�-B�5B�GB�LB�]B�hB�yB�B��B��B��B��B��B��B��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	B		B	
B	
B	
B	B	5B	=B	LB	RB	dB	tB	�B	�B	�B	�B	�B	�B	$�B	'�B	(�B	)�B	)�B	)�B	0�B	4B	7!B	8'B	8(B	9,B	9-B	9+B	;:B	;9B	=GB	?SB	BaB	EvB	G�B	J�B	J�B	K�B	N�B	Q�B	U�B	V�B	U�B	V�B	Z�B	`B	aB	c'B	f9B	f:B	g?B	iKB	jRB	l`B	oqB	pxB	pxB	q{B	r�B	u�B	y�B	z�B	y�B	y�B	y�B	{�B	��B	��B	�aB	��B	�`B	��B
OB
!�B
(�B
.�B
9#B
@QB
G|B
M�B
S�B
Z�B
_B
e.B
iEB
nfB
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708262016053117082620160531170826  AO  ARCAADJP                                                                    20150818021529    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150818021529  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150818021529  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170826  IP                  G�O�G�O�G�O�                
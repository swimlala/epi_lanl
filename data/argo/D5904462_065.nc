CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-22T09:16:23Z AOML 3.0 creation; 2016-08-07T21:51:19Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150722091623  20160807145119  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               AA   AO  5287_9017_065                   2C  D   APEX                            6529                            072314                          846 @�a˺�1   @�a�K� @1Z�1�d��\(��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    AA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy` D�3D�VfD�s3D��fD���D�C3D��fD�ٚD�	�D�Y�D��fDǰ D�fD�<�DچfD��fD�3D�<�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BZzBbzBi�Bq�By�B��
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
C k�C�C�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D�GD�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�GDyz�D� �D�c�D���D���D�D�P�D���D��D�D�gD���DǽqD��D�J>Dړ�D���D� �D�J>D�qD��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��#A�A�/A��A��`A��;A��#A���A���A�wA��A�33A�G�A�z�A�5?A�E�A���A�jA���A���A�-AۑhA��A�jA٩�A��TAؑhA�bA�\)A�r�A��A�C�A��A���AѸRA�bAЧ�Aϙ�Aβ-AΗ�A΅A�7LA���A���A��HA�ƨA�;dA�p�A��A���A��Aƣ�A�`BAú^A²-A+A�;dA���A���A�A���A��`A��`A�XA��A��FA��wA��A���A�33A�+A�"�A�?}A�+A�^5A��-A��A� �A���A��7A�oA���A�n�A�ƨA�33A��;A�ȴA��9A�Q�A�"�A��A�ffA��yA��A�ĜA�G�A�~�A��uA��A�G�A���A�33A�Q�A�ZA���A���A�\)A���A��A�ĜA`BA|bNAzȴAx��Aw�AtVArz�Aq`BAp5?AnȴAk&�Ag�AfAc��Aa�A`A]�-AW��AU�ATz�AO��AM�AK�FAH~�AF�!AD�AC�PAAO�A?;dA=&�A<z�A<E�A<$�A<JA;�-A9��A7G�A4�uA21A0�/A0�A/K�A.�RA.A�A.  A-x�A,Q�A+��A+oA)�A(A�A'ƨA&�A%33A#�A ȴA E�At�A;dAbNAt�AVA�AG�A;dAG�AK�A�+A��At�A"�A9XA?}AVA�
Al�AXAC�A
=A�A-A��A�+A�TAt�A
�A
A�A
  A	�A	�A�HA��Ar�AbA�PAG�AA�/AȴA��A5?A��AhsAĜAA�A�TAVA�uAx�A7LA�A
=A �A �A $�@�\)@�;d@��H@��\@�V@�7L@�j@���@�dZ@�
=@�ȴ@��!@���@���@���@��@�I�@�|�@�"�@�33@�@�?}@��@��@��#@���@��D@�j@���@�j@��m@�;d@��@�
=@��@�+@�{@�x�@�G�@�Ĝ@���@�t�@�@�h@�n�@�ȴ@��
@�j@�hs@�M�@�+@�+@�R@��@�!@���@�7L@�bN@�o@݁@܋D@ڇ+@���@��@��m@�@�J@�7L@�bN@�  @׾w@ו�@ו�@ְ!@Ցh@�%@��@�+@�j@�  @�t�@�+@�~�@�5?@�-@��@ѩ�@щ7@�hs@���@�Q�@�\)@��H@Η�@·+@�v�@�V@�J@���@�x�@��@���@�j@�Q�@�A�@� �@���@�C�@�~�@�-@ʏ\@ʸR@ʸR@ʰ!@ʧ�@�~�@�$�@�p�@�?}@���@�1'@ǍP@��y@�`B@ģ�@� �@��@��#@���@��^@���@�p�@�hs@�G�@�7L@��@���@��9@��D@�(�@�ƨ@�t�@��@��H@��R@�V@��T@��@�G�@�O�@��j@�  @���@��m@��F@���@�o@��+@�-@��#@�@��-@�X@�1'@�o@���@�V@�$�@��@�@���@�@��-@���@�hs@�?}@�/@��`@� �@��F@�dZ@�+@���@���@�V@�{@�Ĝ@��9@�A�@��w@�dZ@�
=@�n�@��@�X@��@�Ĝ@�9X@�b@��w@�+@���@��@�hs@��@�%@��`@��j@��@��u@�(�@��;@��w@�C�@��H@��!@�n�@�E�@�{@��@��#@��-@��@��@��@��h@���@���@�p�@�7L@�%@��/@��@�bN@��@�b@��
@��P@�C�@�o@��@���@��#@�G�@���@��u@�Z@�(�@��F@�S�@�"�@��y@���@���@��@�z�@�9X@�1@��m@��;@��F@�33@���@�|�@�V@�p�@�K�@��@x�u@lj@a�#@ZM�@Rn�@I��@@�9@:~�@5/@.5?@(r�@"�H@@��@�j@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A��#A�A�/A��A��`A��;A��#A���A���A�wA��A�33A�G�A�z�A�5?A�E�A���A�jA���A���A�-AۑhA��A�jA٩�A��TAؑhA�bA�\)A�r�A��A�C�A��A���AѸRA�bAЧ�Aϙ�Aβ-AΗ�A΅A�7LA���A���A��HA�ƨA�;dA�p�A��A���A��Aƣ�A�`BAú^A²-A+A�;dA���A���A�A���A��`A��`A�XA��A��FA��wA��A���A�33A�+A�"�A�?}A�+A�^5A��-A��A� �A���A��7A�oA���A�n�A�ƨA�33A��;A�ȴA��9A�Q�A�"�A��A�ffA��yA��A�ĜA�G�A�~�A��uA��A�G�A���A�33A�Q�A�ZA���A���A�\)A���A��A�ĜA`BA|bNAzȴAx��Aw�AtVArz�Aq`BAp5?AnȴAk&�Ag�AfAc��Aa�A`A]�-AW��AU�ATz�AO��AM�AK�FAH~�AF�!AD�AC�PAAO�A?;dA=&�A<z�A<E�A<$�A<JA;�-A9��A7G�A4�uA21A0�/A0�A/K�A.�RA.A�A.  A-x�A,Q�A+��A+oA)�A(A�A'ƨA&�A%33A#�A ȴA E�At�A;dAbNAt�AVA�AG�A;dAG�AK�A�+A��At�A"�A9XA?}AVA�
Al�AXAC�A
=A�A-A��A�+A�TAt�A
�A
A�A
  A	�A	�A�HA��Ar�AbA�PAG�AA�/AȴA��A5?A��AhsAĜAA�A�TAVA�uAx�A7LA�A
=A �A �A $�@�\)@�;d@��H@��\@�V@�7L@�j@���@�dZ@�
=@�ȴ@��!@���@���@���@��@�I�@�|�@�"�@�33@�@�?}@��@��@��#@���@��D@�j@���@�j@��m@�;d@��@�
=@��@�+@�{@�x�@�G�@�Ĝ@���@�t�@�@�h@�n�@�ȴ@��
@�j@�hs@�M�@�+@�+@�R@��@�!@���@�7L@�bN@�o@݁@܋D@ڇ+@���@��@��m@�@�J@�7L@�bN@�  @׾w@ו�@ו�@ְ!@Ցh@�%@��@�+@�j@�  @�t�@�+@�~�@�5?@�-@��@ѩ�@щ7@�hs@���@�Q�@�\)@��H@Η�@·+@�v�@�V@�J@���@�x�@��@���@�j@�Q�@�A�@� �@���@�C�@�~�@�-@ʏ\@ʸR@ʸR@ʰ!@ʧ�@�~�@�$�@�p�@�?}@���@�1'@ǍP@��y@�`B@ģ�@� �@��@��#@���@��^@���@�p�@�hs@�G�@�7L@��@���@��9@��D@�(�@�ƨ@�t�@��@��H@��R@�V@��T@��@�G�@�O�@��j@�  @���@��m@��F@���@�o@��+@�-@��#@�@��-@�X@�1'@�o@���@�V@�$�@��@�@���@�@��-@���@�hs@�?}@�/@��`@� �@��F@�dZ@�+@���@���@�V@�{@�Ĝ@��9@�A�@��w@�dZ@�
=@�n�@��@�X@��@�Ĝ@�9X@�b@��w@�+@���@��@�hs@��@�%@��`@��j@��@��u@�(�@��;@��w@�C�@��H@��!@�n�@�E�@�{@��@��#@��-@��@��@��@��h@���@���@�p�@�7L@�%@��/@��@�bN@��@�b@��
@��P@�C�@�o@��@���@��#@�G�@���@��u@�Z@�(�@��F@�S�@�"�@��y@���@���@��@�z�@�9X@�1@��m@��;@��F@�33G�O�@�|�@�V@�p�@�K�@��@x�u@lj@a�#@ZM�@Rn�@I��@@�9@:~�@5/@.5?@(r�@"�H@@��@�j@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�XB	�LB	�3B	�-B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�9B	�qB	��B	��B	��B	�NB	�5B	�B	�)B	�B	��B
{B
)�B
49B
8RB
E�B
]/B
\)B
]/B
v�B
q�B
~�B
�VB
�3B
�B%�B5?BXB�B��B��B��B�B�B��B��B-BC�BI�BI�BJ�BK�BhsB�B��B��B��B�)B�BɺB��B��BǮBB�-B� BVBI�B6FB/B,B$�B�B�B:^BZBl�BjBjBdZB\)BYB{�B}�Bo�B\)B9XB$�B�B{BDB�;B�B��Bx�BB�B-B$�B1B
�B
�B
�sB
�NB
��B
ȴB
�wB
��B
�uB
� B
q�B
XB
E�B
8RB
)�B
�B
B	��B	�B	�B	�TB	��B	�XB	��B	��B	�VB	�B	m�B	G�B	@�B	7LB	.B	$�B	�B	\B	1B	B��B��B�B�B�B�B�B�sB�fB�ZB�;B�#B�B�#B�)B�)B�B�B�B�B�B�B�B�B�;B�HB�;B�HB�B�B�5B�ZB�B�B�B�sB�BB�B�)B�TB�B�B�B��B��B��B��B	B	%B	DB	DB	PB	\B	\B	�B	�B	"�B	'�B	+B	-B	1'B	2-B	5?B	8RB	8RB	9XB	;dB	=qB	@�B	A�B	D�B	E�B	G�B	I�B	K�B	N�B	P�B	S�B	YB	^5B	_;B	cTB	iyB	k�B	o�B	p�B	r�B	u�B	w�B	z�B	{�B	{�B	{�B	z�B	{�B	|�B	~�B	~�B	� B	� B	� B	�B	�B	�B	�B	�%B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�3B	�B	��B	�uB	��B	��B	��B	�B	�?B	�LB	�RB	�dB	�RB	�?B	�3B	�!B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�!B	�-B	�XB	�}B	��B	��B	��B	�wB	�jB	�qB	ǮB	ǮB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�
B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�)B	�#B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�B	�B	�B	�#B	�B	�B	�B	�B	�5B	�TB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�sB	�sB	�mB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
	7B
hB
�B
#�B
.B
33B
:^B
@�B
E�B
K�B
R�B
XB
ZB
aHB
dZB
hsB
m�B
r�B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�DB	�6B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�]B	��B	��B	��B	�7B	�B	�B	�B	��B	��B
aB
)�B
4B
8:B
E�B
]B
\B
]B
v�B
q�B
~�B
�;B
�B
�B%�B5BW�B��B�`B��B�bB�gB�B��B��B,�BCvBI�BI�BJ�BK�BhPB��B�rB��B��B�B�kBɖB��BϺBǊB�oB�B�BU�BI�B6!B.�B+�B$�B�B�B::BY�BldBj[BjXBd3B\BX�B{�B}�BouB\B90B$�B�BWBB�B��B��Bx�BBiB,�B$�B	B
�B
�^B
�LB
�'B
��B
ȌB
�PB
��B
�LB
�B
q�B
W�B
E{B
8-B
)�B
�B
�B	��B	�B	�iB	�2B	ͱB	�6B	��B	�sB	�5B	��B	mqB	G�B	@fB	7.B	-�B	$�B	�B	?B	B	�B��B��B�B�|B�nB�jB�cB�UB�HB�?B�B�B�B�B�B�B�B�B��B��B��B��B��B��B�B�)B�B�(B��B��B�B�<B�lB�B��B�TB�"B��B�	B�2B�^B�B�B��B��B��B��B	 �B	B	!B	"B	-B	7B	9B	_B	�B	"�B	'�B	*�B	,�B	1 B	2B	5B	8-B	8-B	93B	;@B	=LB	@`B	AdB	DwB	E}B	G�B	I�B	K�B	N�B	P�B	S�B	X�B	^B	_B	c,B	iQB	k_B	owB	p|B	r�B	u�B	w�B	z�B	{�B	{�B	{�B	z�B	{�B	|�B	~�B	~�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�+B	�WB	��B	��B	��B	�{B	�{B	�oB	�rB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	��B	�JB	�]B	��B	��B	��B	�B	� B	�$B	�:B	�&B	�B	�	B	��B	��B	��B	��B	��B	�iB	�TB	�wB	��B	��B	��B	��B	��B	�B	�-B	�NB	�^B	�WB	�XB	�LB	�>B	�CB	ǂB	ǅB	ʕB	ʖB	ɏB	ʔB	ϯB	зB	иB	ѼB	��B	��B	ѿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	зB	άB	ϴB	ϱB	зB	йB	зB	зB	йB	зB	иB	жB	зB	жB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�&B	�AB	�WB	�WB	�]B	�WB	�YB	�iB	�kB	�jB	�pB	�kB	�fB	�^B	�XB	�RB	�OB	�LB	�?B	�EB	�EB	�?B	�7B	�?B	�@B	�?B	�>B	�?B	�>B	�=B	�EB	�FB	�EB	�KB	�WB	�\B	�]B	�]B	�[B	�[B	�_B	�^B	�dB	�jB	�iB	�mB	�uB	�{B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
�B
	B
9B
{B
#�B
-�B
3B
:-B
@SB
EpB
K�B
R�B
W�B
Y�B
aB
d'B
h?B
m_B
r{B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451192016080714511920160807145119  AO  ARCAADJP                                                                    20150722091623    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150722091623  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150722091623  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145119  IP                  G�O�G�O�G�O�                
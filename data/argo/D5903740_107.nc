CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-24T20:20:17Z AOML 3.0 creation; 2016-06-01T00:08:23Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150224202017  20160531170823  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               kA   AO  4055_7112_107                   2C  D   APEX                            5374                            041511                          846 @�<����1   @�<�a��@:|�hs�d;�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    kA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D��D�` D�|�D�ٚD��D�9�D��3D��fD���D�<�D�� Dǹ�D��D�P D�vfD��D���D�FfD�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��
@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
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
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_!GD_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dy��D�>D�mqD��>D��D�>D�GD���D���D�
>D�J>D��qD��D�>D�]qDڃ�D�>D�
>D�S�D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��
A��A��
A���A���A���A���A��^A��9A��-A��-A��A���A���A��+A�z�A�XA�/A���A�oA��TA��+A�x�A�t�A�^5A�
=A��-A�hsA��;A�n�A��\A�  A�~�A�+A�7LA�=qA�dZA�E�A�A�p�A��A��uA�O�A��A��/A�r�A���A�&�A�
=A��A��hA�/A���A��^A���A�^5A�Q�A��A�l�A���A��jA�"�A��PA�VA��A���A�JA�JA��A�bNA���A}XA{��A{��A{��A{ƨA{�^A{��A{p�A{G�A{Az�/Az�Az �Ay�^Ay��Ax�DAv�uAvI�AuƨAt�\ArJAm�PAh{Ad�Ac�mAa�^A`E�A^ȴA\5?AZ�jAY�hAX��AX�uAXjAXQ�AX�AW�AW?}AV��AVVAUAT�+AS�AQ��AP�AO�mAK�^AI�AH�AG�hAFn�AE�;AE�hAD��ADAB��AA�mAA�;AA��AAt�AA7LA@��A@v�A@M�A@9XA?�
A?�A>{A=�mA=A=��A=��A=?}A=�A<�A<��A<9XA:�jA:r�A8��A6��A5�PA41'A3�PA2I�A1�
A1�PA1G�A/��A-S�A,��A,9XA+��A+oA*M�A)�
A)?}A(��A(�A'O�A&��A%�-A%"�A%�A%%A$�A$A"{A!�wA �A��A�yA1AS�A�jA9XA�-A�+A�A�A&�A|�A\)AC�A33A��A��A�!Az�A�A|�A��A�AC�A�DA�A�yA��AZAJAƨAS�A��A
��A
�RA
z�A
VA	A�A�A�!A|�A��AJA��A/A ȴA ��A jA VA I�A (�A @�dZ@��R@�X@���@�5?@��7@��@�j@�%@��@��@@���@�@�`B@�\)@�^5@�O�@�C�@�v�@۾w@�hs@׶F@�S�@պ^@���@�X@�%@Гu@��m@��@���@ͩ�@͉7@�A�@�&�@�Q�@�9X@� �@��@��m@��;@���@Ǿw@�|�@��@��@���@�ff@�-@�@�x�@�b@�v�@��@��@��@�&�@��/@���@��@��9@�(�@��
@���@�S�@�V@���@�Q�@�t�@��@�^5@���@���@��@�j@�  @�+@��@��\@�=q@�@��@��@�1'@�-@�z�@�ƨ@�|�@��H@��@�/@���@�A�@��m@��P@�C�@�@���@���@�?}@���@�9X@��F@���@�1'@��
@���@�dZ@��@��H@��R@�~�@�@��-@�Ĝ@���@�{@���@���@�x�@�p�@�hs@�?}@���@��D@�S�@��\@�E�@�x�@�hs@�/@�%@��@�r�@�r�@�z�@�z�@�r�@�z�@�j@�z�@�z�@��@���@���@���@��9@��j@��j@��j@��j@�Ĝ@��j@��@�(�@� �@��
@���@�|�@�S�@��@�ȴ@�v�@�M�@�{@�X@�  @�
=@��y@��!@�M�@�{@���@�`B@�/@��@��9@��@�9X@��@���@��m@��w@���@�|�@�|�@�\)@�C�@�+@��@��!@�~�@�n�@�{@��-@�hs@�V@���@��`@��`@���@���@��@�bN@�1@���@��@��@�t�@�S�@�K�@�"�@��@���@��\@�n�@�V@�$�@���@�p�@��@��9@�1'@;d@|j@z��@zJ@y��@y�@y�#@y��@y��@y7L@x��@x��@x��@x�@x1'@x  @w��@w�@w�w@wl�@v�y@v�y@v��@v�+@v$�@v$�@v{@u@u/@s�
@k�m@_|�@W;d@N5?@F��@=�@;t�@8bN@-p�@*n�@$�@��@"�@�@��@��@�j@t�@ ��?�dZ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��
A��A��
A���A���A���A���A��^A��9A��-A��-A��A���A���A��+A�z�A�XA�/A���A�oA��TA��+A�x�A�t�A�^5A�
=A��-A�hsA��;A�n�A��\A�  A�~�A�+A�7LA�=qA�dZA�E�A�A�p�A��A��uA�O�A��A��/A�r�A���A�&�A�
=A��A��hA�/A���A��^A���A�^5A�Q�A��A�l�A���A��jA�"�A��PA�VA��A���A�JA�JA��A�bNA���A}XA{��A{��A{��A{ƨA{�^A{��A{p�A{G�A{Az�/Az�Az �Ay�^Ay��Ax�DAv�uAvI�AuƨAt�\ArJAm�PAh{Ad�Ac�mAa�^A`E�A^ȴA\5?AZ�jAY�hAX��AX�uAXjAXQ�AX�AW�AW?}AV��AVVAUAT�+AS�AQ��AP�AO�mAK�^AI�AH�AG�hAFn�AE�;AE�hAD��ADAB��AA�mAA�;AA��AAt�AA7LA@��A@v�A@M�A@9XA?�
A?�A>{A=�mA=A=��A=��A=?}A=�A<�A<��A<9XA:�jA:r�A8��A6��A5�PA41'A3�PA2I�A1�
A1�PA1G�A/��A-S�A,��A,9XA+��A+oA*M�A)�
A)?}A(��A(�A'O�A&��A%�-A%"�A%�A%%A$�A$A"{A!�wA �A��A�yA1AS�A�jA9XA�-A�+A�A�A&�A|�A\)AC�A33A��A��A�!Az�A�A|�A��A�AC�A�DA�A�yA��AZAJAƨAS�A��A
��A
�RA
z�A
VA	A�A�A�!A|�A��AJA��A/A ȴA ��A jA VA I�A (�A @�dZ@��R@�X@���@�5?@��7@��@�j@�%@��@��@@���@�@�`B@�\)@�^5@�O�@�C�@�v�@۾w@�hs@׶F@�S�@պ^@���@�X@�%@Гu@��m@��@���@ͩ�@͉7@�A�@�&�@�Q�@�9X@� �@��@��m@��;@���@Ǿw@�|�@��@��@���@�ff@�-@�@�x�@�b@�v�@��@��@��@�&�@��/@���@��@��9@�(�@��
@���@�S�@�V@���@�Q�@�t�@��@�^5@���@���@��@�j@�  @�+@��@��\@�=q@�@��@��@�1'@�-@�z�@�ƨ@�|�@��H@��@�/@���@�A�@��m@��P@�C�@�@���@���@�?}@���@�9X@��F@���@�1'@��
@���@�dZ@��@��H@��R@�~�@�@��-@�Ĝ@���@�{@���@���@�x�@�p�@�hs@�?}@���@��D@�S�@��\@�E�@�x�@�hs@�/@�%@��@�r�@�r�@�z�@�z�@�r�@�z�@�j@�z�@�z�@��@���@���@���@��9@��j@��j@��j@��j@�Ĝ@��j@��@�(�@� �@��
@���@�|�@�S�@��@�ȴ@�v�@�M�@�{@�X@�  @�
=@��y@��!@�M�@�{@���@�`B@�/@��@��9@��@�9X@��@���@��m@��w@���@�|�@�|�@�\)@�C�@�+@��@��!@�~�@�n�@�{@��-@�hs@�V@���@��`@��`@���@���@��@�bN@�1@���@��@��@�t�@�S�@�K�@�"�@��@���@��\@�n�@�V@�$�@���@�p�@��@��9@�1'@;d@|j@z��@zJ@y��@y�@y�#@y��@y��@y7L@x��@x��@x��@x�@x1'@x  @w��@w�@w�w@wl�@v�y@v�y@v��@v�+@v$�@v$�@v{@u@u/@s�
@k�m@_|�@W;d@N5?@F��@=�@;t�@8bN@-p�@*n�@$�@��@"�@�@��@��@�j@t�@ ��?�dZ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBǮBǮBǮBǮBǮBɺB��B��B��B��B��B��B��B��BɺBȴBȴBŢBB��B��B�qB�^B�XB�XB�LB�'B��B��B�JB�Bo�BbNBS�BK�BN�BQ�B\)B^5BW
Bk�Bx�Bw�Bo�BhsB_;BP�B8RB\B�B�HBȴB�wB�XB�-B�B��B��B��B�VBy�BaHBR�BD�B8RB)�B�B
�B
��B
�XB
�B
��B
w�B
m�B
l�B
k�B
k�B
jB
iyB
hsB
ffB
dZB
cTB
aHB
]/B
[#B
XB
N�B
A�B
>wB
7LB
)�B
oB	�B	ȴB	�9B	�B	��B	��B	��B	��B	��B	�bB	�PB	�=B	�=B	�1B	�+B	�B	�B	}�B	{�B	w�B	o�B	ffB	_;B	[#B	R�B	F�B	?}B	9XB	49B	0!B	-B	+B	'�B	#�B	�B	�B	�B	�B	�B	�B	�B	{B	uB	hB	\B	JB	%B	B	B	B	B	B	  B��B��B��B�B�B�mB�/B�
B��B��B��BɺBǮBĜB�}B�^B�LB�FB�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B�oB�PB�DB�1B�B�B}�B{�Bz�Bx�Bv�Br�Bo�Bk�BdZBcTBcTBdZBdZBdZBdZBcTBcTBaHB`BB]/B[#BW
BR�BP�BN�BM�BM�BM�BM�BK�BH�BE�BC�BB�B@�B=qB8RB49B1'B/B-B-B,B+B+B+B+B+B+B)�B(�B'�B&�B$�B"�B!�B �B�B�B�B�B�B�B�BuBuBbB\BVBPBVBVBVBVBPBDBVBVBVBPBPBVBVBVBPBJBhBuBuBuBuBuBuBuBuBuB{B{B{B�B�B�B{B�B�B�B!�B"�B"�B#�B#�B#�B#�B#�B$�B$�B#�B%�B%�B)�B,B-B.B0!B2-B1'B33B33B6FB6FB7LB8RB9XB9XB:^B<jBC�BJ�BM�BN�BO�BR�BVBXBZBZB[#B[#B[#B\)B^5B`BBbNBe`BffBo�Bv�Bx�Bz�B{�B}�B~�B� B�B�B�B�1B�{B��B��B��B��B��B��B��B��B��B��B�B�B�?B�FB�LB�RB�jB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�jB�jB�jB�dB�dB�dB�dB�dB�dB�dB�qB�}B�}BBÖBĜBŢBǮBɺB��B��B��B��B�BB�B�B�B�B�B��B��B��B��B	B	B	%B	1B		7B		7B	DB	JB	VB	VB	\B	bB	hB	uB	�B	�B	�B	�B	 �B	#�B	(�B	)�B	+B	+B	,B	.B	0!B	1'B	6FB	:^B	;dB	=qB	?}B	@�B	A�B	C�B	F�B	I�B	L�B	M�B	N�B	Q�B	VB	[#B	_;B	cTB	iyB	n�B	z�B	�B	�B	�B	�B	�B	�+B	�DB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ÖB	�TB	��B
	7B
�B
&�B
+B
/B
?}B
B�B
J�B
Q�B
W
B
XB
bNB
ffB
jB
v�B
y�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BǆBǊBǈBǈBǊBɓBʙBˢBˢB˝B˝BˣB˝BʙBɓBȏBȏB�zB�iB�[B�\B�IB�5B�/B�1B�#B�B��B�ZB�%B��BosBb$BS�BK�BN�BQ�B\B^BV�Bk_Bx�Bw�BorBhJB_BP�B8)B/B�zB�BȊB�KB�+B�B��B��B��B��B�/By�BaBR�BDsB8)B)�BlB
�B
ͫB
�0B
��B
�}B
w�B
mkB
lfB
k\B
k\B
jVB
iQB
hMB
f@B
d1B
c1B
a B
]B
Z�B
W�B
N�B
AbB
>OB
7(B
)�B
IB	�ZB	ȑB	�B	��B	��B	��B	�xB	�pB	�_B	�@B	�-B	�B	�B	�B	�B	��B	��B	}�B	{�B	w�B	o|B	fFB	_B	[B	R�B	F�B	?^B	98B	4B	0B	,�B	*�B	'�B	#�B	�B	�B	�B	�B	tB	lB	hB	_B	UB	JB	<B	+B	B	�B	�B	�B	�B	 �B��B��B��B��B�B��B�PB�B��B��BλBʦBɞBǐBāB�aB�CB�2B�*B�B�B��B��B��B��B��B��B��B��B�}B�}B�vB�iB�TB�4B�)B�B��B��B}�B{�Bz�Bx�Bv�Br�Bo�BkmBd>Bc9Bc8Bd=Bd>Bd?Bd<Bc7Bc:Ba/B`)B]B[BV�BR�BP�BN�BM�BM�BM�BM�BK�BH�BE�BC|BBtB@iB==B8:B4B1B/B,�B,�B+�B*�B*�B*�B*�B*�B*�B)�B(�B'�B&�B$�B"�B!�B �B�B~BgByBzBRBKBXBZB-B%B$BB#B=B"B"B6B(B!BB"BBB<B B B4BB2BXB[BXBWBYBXBZBZBXBaB`BbBgBcBfBbBgBKBuB!�B"�B"�B#�B#�B#�B#�B#�B$�B$�B#�B%�B%�B)�B+�B,�B-�B0B2B1B3B3B6(B6'B7,B82B99B9<B:>B<LBCxBJ�BM�BN�BO�BR�BU�BW�BY�BY�B[B[B[ B\B^B`#Bb-Be@BfBBo}Bv�Bx�Bz�B{�B}�B~�B�B��B��B��B�B�VB�oB�pB�vB��B��B��B��B��B��B��B��B��B�B� B�&B�-B�CB�KB�IB�MB�KB�MB�KB�NB�KB�MB�IB�CB�DB�DB�?B�?B�?B�@B�?B�@B�?B�KB�WB�TB�jB�oB�wB�}BǉBɔB̥BͭBαB��B�B�WB�]B�iB�B�B��B��B��B��B	 �B	�B	�B	B		B		B	B	!B	-B	-B	2B	:B	@B	JB	`B	gB	jB	�B	 �B	#�B	(�B	)�B	*�B	*�B	+�B	-�B	/�B	0�B	6B	:2B	;9B	=DB	?QB	@ZB	A_B	ClB	F~B	I�B	L�B	M�B	N�B	Q�B	U�B	Z�B	_B	c'B	iKB	njB	z�B	��B	��B	��B	��B	��B	��B	�B	�-B	�:B	�FB	�NB	�SB	�eB	�kB	�qB	�wB	�qB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�eB	�$B	��B
	B
^B
&�B
*�B
.�B
?JB
B^B
J�B
Q�B
V�B
W�B
bB
f4B
jMB
v�B
y�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708232016053117082320160531170823  AO  ARCAADJP                                                                    20150224202017    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150224202017  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150224202017  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170823  IP                  G�O�G�O�G�O�                
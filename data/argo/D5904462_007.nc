CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:05Z AOML 3.0 creation; 2016-08-07T21:51:10Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221405  20160807145110  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_007                   2C  D   APEX                            6529                            072314                          846 @��|e��1   @����@2������cț��S�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C�fC
  C  C  C  C  C  C  C�C�C�C�fC�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�fD�P D���D���D��D�FfD�s3D��3D��D�6fD�s3Dǹ�D��D�I�Dڠ D��fD��D�<�D�` D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
B��
B�
=B��
B��
B��
B��
B��
B���B��
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
C k�Ck�C�Ck�CQ�C
k�Ck�Ck�Ck�Ck�Ck�Ck�C�C�C�CQ�C Q�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�B�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA�{DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtz�Dy��D��D�]qD��D��>D�'D�S�D���D��D�'D�C�D���D��D�*>D�WDڭqD���D��D�J>D�mqD��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aݣ�A�;dA��#Aܛ�A܃A�x�A�t�A�jA�`BA�\)A�XA�O�A�I�A�C�A�A�A�?}A�9XA�5?A�1'A�"�A���AۅA�7LA�
=A�ƨA���Aڛ�A�r�A���A��A���A�JA�S�A���A�33Aҥ�A�ĜA�=qA�ZA���A�VA��;A��Aʥ�A��HAȏ\A�G�A��;A��#A���Aŉ7A�%A��;A+A���A�I�A���A���A�$�A��yA���A�r�A�ƨA�(�A�S�A� �A�33A��#A�=qA�/A�jA�ȴA�K�A�ĜA�M�A��A���A��A��hA���A��FA�5?A�M�A�dZA�M�A�1A��`A��/A��;A\)A{oAwG�At��As�hAo�Aj^5Ag�AeG�Ac��Abr�A_"�A]�;A\�AY/AXbAT�AO��AL�yAI�
AC��A@��A=�
A<�`A<Q�A9��A7S�A5�A4�\A1|�A0M�A.I�A-\)A-
=A+�7A(ĜA'?}A%�PA$��A$  A"�A �/A z�A VA 5?A�mAdZA�#A��A��A$�A|�A�A�A��At�A%A��A�An�AffAM�A�A/A��A��An�A^5AE�Ap�A��A��AS�AJA`BA�AM�A�wA	ƨAv�A�hA/A��A�A%A$�A��AdZA"�A^5A ��AjA=qA�AbA��A+A��A~�AZA5?A�mAt�AG�A;dAx�Ap�AoA ��@�l�@�E�@��@��;@��!@��!@�~�@�@���@�x�@���@��;@�ƨ@���@�ȴ@�x�@���@��u@���@�A�@��@��
@�K�@�!@�-@�^5@�5?@���@���@�p�@�&�@��@�1@�33@���@��-@�  @�K�@�"�@���@�ȴ@�{@�z�@睲@���@��`@��;@��@�\@�^@�7L@�V@��/@�Q�@�"�@�V@�J@�V@ܼj@�I�@�1@ۮ@��@�M�@�@�x�@�bN@�  @ו�@�~�@��#@�&�@��/@���@�Ĝ@�z�@Ӯ@�S�@��@�o@�ȴ@�-@�p�@�b@�@·+@�n�@�M�@�5?@���@��@�(�@ˮ@���@ʟ�@�-@�J@ɑh@�O�@�%@��`@ȴ9@ț�@ȋD@��@��
@�ƨ@Ǖ�@�l�@�o@�=q@�p�@��@�V@�V@�bN@�t�@�+@�ȴ@�=q@���@���@�
=@���@��y@�ȴ@��\@��@�&�@�j@�1'@�1@�ƨ@�ƨ@��;@�dZ@��@���@�E�@�5?@�5?@��@��T@�p�@�?}@�V@���@�Q�@��w@�t�@�\)@��@��@���@���@�j@�bN@�Q�@��;@��@�=q@��@��@�?}@��@���@�;d@�"�@��@��@�
=@�ȴ@��+@�^5@�=q@��@���@�p�@�?}@�%@���@���@�Q�@�9X@�(�@��@��;@��@�|�@�33@��!@�v�@�V@�5?@��@��-@�7L@��@�A�@�C�@���@��H@���@�ff@���@���@��/@���@�Z@�1@�l�@��@���@�M�@��T@���@�hs@�/@�V@���@�1'@�\)@��@���@�^5@�M�@�5?@��T@��7@�`B@�?}@���@��@���@���@�-@��T@���@��-@��7@�`B@�X@�hs@�hs@��`@���@�bN@� �@�1@�  @��m@���@�dZ@��@��R@��+@�J@��-@���@��@�x�@�`B@�7L@��@�Z@�9X@�b@��@��w@��@�;d@�o@��\@���@�@��-@���@���@���@��@�hs@�7L@��@��@�9X@��m@��P@�t�@�
=@��H@���@�33@�K�@z�H@s��@kdZ@b~�@\1@V�+@G
=@>�R@9G�@1��@)hs@#��@@�#@O�@�@7L@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aݣ�A�;dA��#Aܛ�A܃A�x�A�t�A�jA�`BA�\)A�XA�O�A�I�A�C�A�A�A�?}A�9XA�5?A�1'A�"�A���AۅA�7LA�
=A�ƨA���Aڛ�A�r�A���A��A���A�JA�S�A���A�33Aҥ�A�ĜA�=qA�ZA���A�VA��;A��Aʥ�A��HAȏ\A�G�A��;A��#A���Aŉ7A�%A��;A+A���A�I�A���A���A�$�A��yA���A�r�A�ƨA�(�A�S�A� �A�33A��#A�=qA�/A�jA�ȴA�K�A�ĜA�M�A��A���A��A��hA���A��FA�5?A�M�A�dZA�M�A�1A��`A��/A��;A\)A{oAwG�At��As�hAo�Aj^5Ag�AeG�Ac��Abr�A_"�A]�;A\�AY/AXbAT�AO��AL�yAI�
AC��A@��A=�
A<�`A<Q�A9��A7S�A5�A4�\A1|�A0M�A.I�A-\)A-
=A+�7A(ĜA'?}A%�PA$��A$  A"�A �/A z�A VA 5?A�mAdZA�#A��A��A$�A|�A�A�A��At�A%A��A�An�AffAM�A�A/A��A��An�A^5AE�Ap�A��A��AS�AJA`BA�AM�A�wA	ƨAv�A�hA/A��A�A%A$�A��AdZA"�A^5A ��AjA=qA�AbA��A+A��A~�AZA5?A�mAt�AG�A;dAx�Ap�AoA ��@�l�@�E�@��@��;@��!@��!@�~�@�@���@�x�@���@��;@�ƨ@���@�ȴ@�x�@���@��u@���@�A�@��@��
@�K�@�!@�-@�^5@�5?@���@���@�p�@�&�@��@�1@�33@���@��-@�  @�K�@�"�@���@�ȴ@�{@�z�@睲@���@��`@��;@��@�\@�^@�7L@�V@��/@�Q�@�"�@�V@�J@�V@ܼj@�I�@�1@ۮ@��@�M�@�@�x�@�bN@�  @ו�@�~�@��#@�&�@��/@���@�Ĝ@�z�@Ӯ@�S�@��@�o@�ȴ@�-@�p�@�b@�@·+@�n�@�M�@�5?@���@��@�(�@ˮ@���@ʟ�@�-@�J@ɑh@�O�@�%@��`@ȴ9@ț�@ȋD@��@��
@�ƨ@Ǖ�@�l�@�o@�=q@�p�@��@�V@�V@�bN@�t�@�+@�ȴ@�=q@���@���@�
=@���@��y@�ȴ@��\@��@�&�@�j@�1'@�1@�ƨ@�ƨ@��;@�dZ@��@���@�E�@�5?@�5?@��@��T@�p�@�?}@�V@���@�Q�@��w@�t�@�\)@��@��@���@���@�j@�bN@�Q�@��;@��@�=q@��@��@�?}@��@���@�;d@�"�@��@��@�
=@�ȴ@��+@�^5@�=q@��@���@�p�@�?}@�%@���@���@�Q�@�9X@�(�@��@��;@��@�|�@�33@��!@�v�@�V@�5?@��@��-@�7L@��@�A�@�C�@���@��H@���@�ff@���@���@��/@���@�Z@�1@�l�@��@���@�M�@��T@���@�hs@�/@�V@���@�1'@�\)@��@���@�^5@�M�@�5?@��T@��7@�`B@�?}@���@��@���@���@�-@��T@���@��-@��7@�`B@�X@�hs@�hs@��`@���@�bN@� �@�1@�  @��m@���@�dZ@��@��R@��+@�J@��-@���@��@�x�@�`B@�7L@��@�Z@�9X@�b@��@��w@��@�;d@�o@��\@���@�@��-@���@���@���@��@�hs@�7L@��@��@�9X@��m@��P@�t�@�
=G�O�@���@�33@�K�@z�H@s��@kdZ@b~�@\1@V�+@G
=@>�R@9G�@1��@)hs@#��@@�#@O�@�@7L@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB(�B(�B(�B(�B(�B(�B(�B(�B'�B'�B&�B%�B%�B%�B%�B%�B%�B$�B%�B%�B&�B+B8RB8RB2-B5?B49B2-B�BDBB
��B
��B
�B
�fB
�ZB
�TB
�NB
�BB
�yBBBBB&�BI�BL�BP�B\)Bl�Bq�By�B�hB��B�XB�}BǮB��B�TB�B��B�B�B��Bs�B&�B
��B
�yB
��B
��B
��B
�`B
�)B
ŢB
�XB
��B
B
��B
N�B
YB
iyB
�bB
��B
��B
�+B
ZB
C�B
"�B	��B	��B	�B	��B	�PB	�B	hsB	H�B	:^B	33B	-B	%�B	�B	hB		7B	  B��B�B�fB�BB�
B��B��B��B�
B�ZB�BB�
B��B�B��B��B��B��B��BɺBB�wB��BǮBƨBŢBƨBǮBǮBƨBƨBƨB��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB��B�
B�
B�B�5B�;B�5B�;B�#B�B��B��B��B��BǮBĜBÖBBBBBBƨBȴBɺB��B��B�
B��B�B	B	+B	{B	+B	6FB	49B	2-B	2-B	49B	9XB	=qB	>wB	B�B	K�B	N�B	P�B	P�B	R�B	T�B	YB	ZB	W
B	XB	W
B	VB	VB	T�B	T�B	T�B	W
B	XB	\)B	]/B	aHB	cTB	gmB	jB	m�B	n�B	r�B	r�B	s�B	x�B	y�B	y�B	{�B	}�B	}�B	}�B	� B	�B	� B	�B	�B	�B	�B	�B	�B	�+B	�=B	�1B	�7B	�+B	�%B	�7B	�\B	�bB	�bB	�\B	�VB	�VB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�9B	�?B	�FB	�LB	�LB	�LB	�^B	�^B	�^B	�dB	�dB	�jB	�wB	�}B	�}B	�}B	�}B	��B	B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�/B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�`B	�`B	�fB	�mB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
	7B
bB
�B
�B
(�B
33B
7LB
<jB
?}B
C�B
K�B
O�B
T�B
\)B
bNB
gmB
k�B
o�B
r�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B(�B(�B(�B(�B(�B(�B(�B(�B'�B'�B&�B%�B%�B%�B%�B%�B%�B$�B%�B%�B&�B*�B85B80B2B5 B4B2B�B&B�B
��B
��B
�B
�FB
�:B
�6B
�/B
�%B
�\B B�B �B B&�BI�BL�BP�B\BljBq�By�B�GB��B�7B�ZBǌB��B�0B�ZB��B�B��B�gBs�B&�B
��B
�WB
��B
��B
��B
�<B
�B
�B
�6B
ͱB
�jB
�|B
N�B
X�B
iSB
�=B
��B
��B
�B
Y�B
CsB
"�B	��B	˦B	��B	��B	�4B	��B	hWB	H�B	:?B	3B	,�B	%�B	pB	MB		B��B��B�qB�KB�'B��B��B��B��B��B�=B�#B��B��B��B��BͳB̱B͵B̯BɛB�rB�YB�kBǏBƉBŃBƊBǏBǎBƆBƈBƇB˩BϿB��B��BͳBͲBϿB��B��B��BιB˧BʡBʥBɜB˧B��B��B��B�B�B�B�B�B��B��BϿB̭BʠBǋB�|B�sB�qB�pB�pB�oB�oBƆBȓBəBʠB��B��B��B�B	 �B	B	XB	*�B	6 B	4B	2	B	2B	4B	91B	=LB	>PB	BgB	K�B	N�B	P�B	P�B	R�B	T�B	X�B	Y�B	V�B	W�B	V�B	U�B	U�B	T�B	T�B	T�B	V�B	W�B	\B	]B	a B	c+B	gDB	jWB	mkB	npB	r�B	r�B	s�B	x�B	y�B	y�B	{�B	}�B	}�B	}�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	��B	�B	�0B	�9B	�6B	�1B	�-B	�+B	�+B	�.B	�>B	�WB	�]B	�ZB	�YB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	� B	� B	�3B	�2B	�2B	�9B	�:B	�=B	�JB	�NB	�OB	�QB	�PB	�\B	�aB	�bB	�lB	�pB	�mB	�pB	�uB	�sB	�uB	�tB	�wB	�{B	ǂB	ǀB	ȆB	ɍB	ʓB	̞B	ѾB	��B	��B	зB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�B	�B	�B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	� B	�B	� B	�%B	�$B	�&B	�+B	�1B	�0B	�1B	�1B	�2B	�1B	�3B	�0B	�2B	�7B	�8B	�6B	�/B	�0B	�9B	�?B	�6B	�=B	�>B	�?B	�<B	�>B	�=B	�?B	�>B	�EB	�EB	�IB	�JB	�IB	�XB	�iB	�jB	�pB	�oB	�vB	�{B	�B	�B	�B	�~B	�{B	�|B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
	B
2B
\B
�B
(�B
3B
7B
<5B
?IB
CcB
K�B
O�B
T�B
[�B
bB
g;B
kRB
oiB
r~B
u�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451102016080714511020160807145110  AO  ARCAADJP                                                                    20150226221405    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221405  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221405  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145110  IP                  G�O�G�O�G�O�                
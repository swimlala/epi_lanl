CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:03Z AOML 3.0 creation; 2016-08-07T21:51:09Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221403  20160807145110  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_005                   2C  D   APEX                            6529                            072314                          846 @����1   @�	hK�@2$�/��cÍO�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  C   C  C  C  CffC	��C  C�fC  C  C  C�C  C  C  C  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��D�Y�D�� D�ٚD�fD�L�D��3D��fD�3D�0 D�y�D��fD�fD�C3Dډ�D��3D�fD�,�D�c3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
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
B�
=B�
=BУ�B��
B��
B��
B��B��
B��
B�
=B��
B��
B��
B��
C k�Ck�Ck�Ck�C��C
8RCk�CQ�Ck�Ck�Ck�C�Ck�Ck�Ck�Ck�C Q�C"Q�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��DuDyǮD�*>D�gD��qD��D��D�Z>D���D���D� �D�=qD��D���D��D�P�DڗD��D��D�:>D�p�D�Ф111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aܴ9Aܗ�A܏\A܃A܁A�z�A�t�A�r�A�n�A�l�A�hsA�dZA�`BA�`BA�^5A�\)A�ZA�XA�XA�VA�VA�Q�A�M�A�G�A�{A۝�A�/AھwA�`BA� �A�1A���A��HA��#A���A�A٬A١�AكA�M�A�oA���A�33A��A�1A��Aԥ�A��A�z�A��A�C�AжFAϴ9A�A�A���A�ȴAŝ�A���A��A�r�A�=qA���A�1A��RA�ZA���A�v�A�9XA�O�A��`A�S�A�Q�A�M�A��A�bA�bNA�r�A��HA�(�A��uA��7A��A��!A�ȴA�$�A��/A�"�A��A}�;A{�PAz��Av��As��Aq��Ap �Ao�AmAk"�Ah�+Ad�A_�wA]S�AYO�AW��AV5?AS��AP�uAO�AOdZAM�AM�7AL�AJ�`AH��AG|�AF�/ACoA@$�A?�A>��A>n�A<�A8��A5��A5�PA5O�A4�yA2�RA1A17LA/�mA-�-A-�A-hsA-O�A,�A*A�A(��A&��A%/A$�A!��A��A�9A��A��A~�A�#AS�A��Al�AĜAhsA�Az�A\)A9XAhsAZA�^A%AbNA�AI�A��A
�A��AdZA�AoAVA��AbNA��A�\A�!A��A�Al�AE�A�/A��A��A��A�hA�Al�AG�A?}A33AG�A&�Av�AQ�A1A�wA��At�A"�A bN@���@�G�@��@��@�=q@�J@��T@��h@��@��D@�A�@��;@�t�@�C�@�\)@�o@�J@�ff@���@�1'@�"�@�n�@�$�@���@�?}@��@�@�^5@�5?@@��@�E�@���@�%@�@�|�@�M�@陚@�G�@�/@�w@�E�@��@�j@�+@�G�@��/@��@�@�J@۶F@�"�@�`B@أ�@��@�t�@���@��y@���@�=q@Ցh@���@Ԭ@�z�@��
@�
=@�@�X@�V@�A�@�"�@�M�@͉7@���@̓u@�r�@��@�t�@�o@���@�5?@�{@��@��#@ɡ�@��@�(�@��;@Ǯ@�33@���@���@�x�@�X@�7L@��@Ĵ9@ēu@ă@�Z@��m@�\)@�o@§�@�v�@�M�@�J@��#@�p�@�%@���@��@���@��@�bN@�9X@�dZ@���@�$�@���@��-@���@�p�@��@��/@��@���@��@�Ĝ@�Ĝ@��j@��j@���@�I�@���@���@�V@�$�@���@���@��7@�O�@�7L@���@���@��j@���@��@�j@��@���@�v�@�-@��^@�&�@���@��@��P@�l�@�S�@��@��@�ȴ@���@��!@���@�ff@�V@��#@�O�@��9@�bN@�1@���@�l�@��@��@���@���@�ff@��T@���@��7@�?}@���@��@�j@���@���@��P@�K�@�33@���@�-@���@��h@�`B@�7L@��j@��m@��@��R@�@�`B@���@���@�Ĝ@�Ĝ@��
@��y@���@��\@��\@�~�@�ff@�M�@�@���@�`B@�&�@�V@���@���@���@��@���@��u@�ƨ@�C�@��@���@�ff@�V@�5?@���@�/@�Ĝ@�bN@�b@��
@���@���@��@�@�p�@�%@�z�@��@��@�C�@�+@�"�@�@��@�5?@���@��7@�x�@�7L@�7L@�&�@��j@�Z@�I�@��w@�C�@�"�@�"�@��@���@��\@�M�@�@���@�p�@�G�@�7L@�V@��@�Q�@� �@��;@��
@���@��F@��@�33@�ȴ@�=q@��D@�|�@�t�@|z�@q��@i��@`A�@XQ�@O�P@F�y@?
=@6V@0bN@,�@%V@ r�@��@��@x�@�@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aܴ9Aܗ�A܏\A܃A܁A�z�A�t�A�r�A�n�A�l�A�hsA�dZA�`BA�`BA�^5A�\)A�ZA�XA�XA�VA�VA�Q�A�M�A�G�A�{A۝�A�/AھwA�`BA� �A�1A���A��HA��#A���A�A٬A١�AكA�M�A�oA���A�33A��A�1A��Aԥ�A��A�z�A��A�C�AжFAϴ9A�A�A���A�ȴAŝ�A���A��A�r�A�=qA���A�1A��RA�ZA���A�v�A�9XA�O�A��`A�S�A�Q�A�M�A��A�bA�bNA�r�A��HA�(�A��uA��7A��A��!A�ȴA�$�A��/A�"�A��A}�;A{�PAz��Av��As��Aq��Ap �Ao�AmAk"�Ah�+Ad�A_�wA]S�AYO�AW��AV5?AS��AP�uAO�AOdZAM�AM�7AL�AJ�`AH��AG|�AF�/ACoA@$�A?�A>��A>n�A<�A8��A5��A5�PA5O�A4�yA2�RA1A17LA/�mA-�-A-�A-hsA-O�A,�A*A�A(��A&��A%/A$�A!��A��A�9A��A��A~�A�#AS�A��Al�AĜAhsA�Az�A\)A9XAhsAZA�^A%AbNA�AI�A��A
�A��AdZA�AoAVA��AbNA��A�\A�!A��A�Al�AE�A�/A��A��A��A�hA�Al�AG�A?}A33AG�A&�Av�AQ�A1A�wA��At�A"�A bN@���@�G�@��@��@�=q@�J@��T@��h@��@��D@�A�@��;@�t�@�C�@�\)@�o@�J@�ff@���@�1'@�"�@�n�@�$�@���@�?}@��@�@�^5@�5?@@��@�E�@���@�%@�@�|�@�M�@陚@�G�@�/@�w@�E�@��@�j@�+@�G�@��/@��@�@�J@۶F@�"�@�`B@أ�@��@�t�@���@��y@���@�=q@Ցh@���@Ԭ@�z�@��
@�
=@�@�X@�V@�A�@�"�@�M�@͉7@���@̓u@�r�@��@�t�@�o@���@�5?@�{@��@��#@ɡ�@��@�(�@��;@Ǯ@�33@���@���@�x�@�X@�7L@��@Ĵ9@ēu@ă@�Z@��m@�\)@�o@§�@�v�@�M�@�J@��#@�p�@�%@���@��@���@��@�bN@�9X@�dZ@���@�$�@���@��-@���@�p�@��@��/@��@���@��@�Ĝ@�Ĝ@��j@��j@���@�I�@���@���@�V@�$�@���@���@��7@�O�@�7L@���@���@��j@���@��@�j@��@���@�v�@�-@��^@�&�@���@��@��P@�l�@�S�@��@��@�ȴ@���@��!@���@�ff@�V@��#@�O�@��9@�bN@�1@���@�l�@��@��@���@���@�ff@��T@���@��7@�?}@���@��@�j@���@���@��P@�K�@�33@���@�-@���@��h@�`B@�7L@��j@��m@��@��R@�@�`B@���@���@�Ĝ@�Ĝ@��
@��y@���@��\@��\@�~�@�ff@�M�@�@���@�`B@�&�@�V@���@���@���@��@���@��u@�ƨ@�C�@��@���@�ff@�V@�5?@���@�/@�Ĝ@�bN@�b@��
@���@���@��@�@�p�@�%@�z�@��@��@�C�@�+@�"�@�@��@�5?@���@��7@�x�@�7L@�7L@�&�@��j@�Z@�I�@��w@�C�@�"�@�"�@��@���@��\@�M�@�@���@�p�@�G�@�7L@�V@��@�Q�@� �@��;@��
@���@��F@��@�33@�ȴG�O�@��D@�|�@�t�@|z�@q��@i��@`A�@XQ�@O�P@F�y@?
=@6V@0bN@,�@%V@ r�@��@��@x�@�@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B!�B!�B �B �B �B �B �B �B!�B!�B!�B�B�B�B�B�B\BbBbBJBhB�BoB�BH�BD�B<jB�B
��B
��B
��B
��B
�B
�B
��B
��B
B
�9B
�B
��B
��B
�+B
{�B
jB
ZB
W
B
O�B
L�B
J�B
C�B
T�B
e`B
L�B
>wB
49B
)�B
"�B
uB
+B	��B	�`B	��B	�jB	�3B	��B	�+B	}�B	s�B	k�B	bNB	R�B	C�B	/B	�B	JB��B��B�B�B�B�B�yB�B�yB�sB�mB�B�B�yB�B��B��B��B��B��B��B��B��B��B��B�B�B�mB�BB�
B�B�ZB�ZB�BB�B�BB�5B�)B�B�B��B��B��B��B��B��B��B�
B�B�B�)B�B�B�yB�fB�TB�;B�B��B��B��B��B��BǮBÖBƨBȴBȴBȴBȴB��BɺB��B�B�B�B�5B�B	%B	\B	�B	"�B	#�B	#�B	$�B	%�B	&�B	)�B	1'B	6FB	;dB	?}B	?}B	A�B	A�B	@�B	?}B	=qB	:^B	;dB	9XB	:^B	<jB	<jB	<jB	=qB	B�B	F�B	H�B	I�B	K�B	L�B	M�B	M�B	P�B	]/B	p�B	t�B	t�B	u�B	u�B	t�B	r�B	r�B	t�B	v�B	z�B	� B	�B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�?B	�FB	�?B	�?B	�FB	�LB	�LB	�LB	�RB	�RB	�XB	�XB	�^B	�^B	�jB	�wB	�}B	��B	��B	B	B	ÖB	ŢB	ƨB	ƨB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�sB	�mB	�mB	�mB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
%B
%B
B
B
%B
%B
B
%B
%B
%B
B
B
%B
%B
%B
+B
\B
�B
!�B
(�B
1'B
9XB
=qB
A�B
H�B
N�B
S�B
W
B
[#B
`BB
e`B
iyB
m�B
s�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BXBZBhBjBpBhBlBnBtBnBuBsBsBuB{B{ByB{B{ByByB�B�B�B�B"�B"�B!�B!�B �B �B �B �B �B �B!�B!�B!�B�B�B�BxBaB<BBBFB,BEBqBOB_BH�BD|B<HB�B
��B
ϾB
��B
��B
��B
��B
��B
ͯB
�oB
�B
��B
��B
��B
�
B
{�B
jaB
Y�B
V�B
O�B
L�B
J�B
CuB
T�B
e>B
L�B
>VB
4B
)�B
"�B
UB
B	��B	�@B	̬B	�HB	�B	��B	�B	}�B	s�B	kiB	b/B	R�B	CxB	.�B	xB	.B��B��B�B�}B�gB�kB�^B�cB�^B�VB�QB�bB�gB�\B�B��B��B��B��B��B��B��B��B��B��B�B�gB�PB�'B��B��B�:B�=B�$B��B�#B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�	B�`B�|B�XB�BB�5B�B��B��BϽBͳB̫BʡBǍB�vBƉBȓBȕBȓBȔBˤBɘB��B��B��B��B�B�B	 B	:B	{B	"�B	#�B	#�B	$�B	%�B	&�B	)�B	0�B	6B	;<B	?WB	?YB	A`B	AaB	@\B	?UB	=LB	:7B	;>B	91B	:8B	<BB	<BB	<DB	=JB	BgB	F�B	H�B	I�B	K�B	L�B	M�B	M�B	P�B	]B	p|B	t�B	t�B	u�B	u�B	t�B	r�B	r�B	t�B	v�B	z�B	�B	��B	�!B	�GB	�VB	�_B	�hB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�B	� B	�%B	�%B	�+B	�*B	�1B	�4B	�=B	�MB	�QB	�VB	�\B	�bB	�aB	�iB	�tB	�}B	�|B	ȆB	ȇB	ɎB	ɍB	ʕB	˚B	˛B	̟B	̝B	̞B	˙B	˚B	˚B	ͤB	̞B	ͥB	ͤB	ͥB	ͥB	ͧB	έB	кB	ѽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�	B	�
B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�&B	�%B	�)B	�+B	�+B	�+B	�+B	�1B	�1B	�7B	�:B	�6B	�6B	�7B	�9B	�9B	�<B	�=B	�FB	�EB	�DB	�FB	�MB	�CB	�BB	�CB	�>B	�>B	�>B	�7B	�6B	�7B	�>B	�DB	�CB	�JB	�JB	�JB	�KB	�LB	�KB	�JB	�WB	�`B	�iB	�jB	�mB	�pB	�nB	�uB	�tB	�sB	�sB	�tB	�uB	�zB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
+B
PB
!�B
(�B
0�B
9$B
=>B
AXB
H�B
N�B
S�B
V�B
Z�B
`B
e.B
iHB
m_B
s�B
w�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451102016080714511020160807145110  AO  ARCAADJP                                                                    20150226221403    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221403  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221403  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145110  IP                  G�O�G�O�G�O�                
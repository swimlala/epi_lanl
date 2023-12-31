CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:24Z AOML 3.0 creation; 2016-05-31T19:14:30Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230524  20160531121430  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               "A   AO  4051_7090_034                   2C  D   APEX                            5368                            041511                          846 @ֈ�p���1   @ֈ��&��@3�z�H�d~��"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    "A   A   A   @9��@�  @�  A   A!��AA��A`  A�  A���A�  A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds� Dy� D�3D�9�D�vfD���D�	�D�C3D�s3D��fD��fD�C3D��3D�p D�fD�9�Dډ�D��3D�3D�<�D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @$z�@j�H@�p�@�p�AQ�A<Q�AZ�RAz�RA�(�A�\)A��\A��\A�\)A�\)A�\)A�\)B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C��C��C��C��C	��C��C��C�C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPqGDP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dy��D��D�/D�k�D��>D��D�8�D�h�D���D���D�8�D�x�D�eqD���D�/D�DฤD��D�2>D�eqD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�VA�VA�JA�JA�VA�bA�oA��A��A��A��A��A��A��A��A��A��A�{A�A��HAŸRAŮAţ�Aś�AŃA�l�A�S�A�G�A�;dA�/A�1A���A���A��mA���A�ĜA���Aİ!AčPA�I�A�1A�ĜAã�AÕ�AÃA�z�A�v�A�p�A�p�A�n�A�n�A�n�A�n�A�n�A�p�A�p�A�hsA�ffA�ffA�bNA�VA�M�A�I�A�G�A�E�A�A�A�;dA�5?A�-A��A���A£�A��RA���A��
A�C�A��`A��\A��9A��!A�O�A�t�A��A�t�A�1A�(�A�7LA��^A�n�A��A���A���A��TA��A�A���A�JA��A���A�ƨA���A�  A�^5A�+A���A���A�5?A��A��7A��jA��wA�$�A�hsA��`A�-A�1A�1A�E�A�XA�%A���A��;A��A}��A{�TAyG�Ar�HAm��Ak\)AjZAi�#Ah��Af�HAeS�AbȴAa33A_��A]��A[G�AY|�AX$�AUx�AR��APM�ANĜAJjAIVAH�uAF�9AE�AB�A@�A> �A<�A:��A8~�A7��A7C�A5�mA3��A1�;A/��A/��A/t�A/33A.�A.9XA-;dA+�PA)��A)�A(r�A'K�A&9XA%�hA$��A"ffA ��A�A��A �A&�AVAK�A�TA��AdZA
=A�7A�A1'A�7A$�A�A?}A1A�A�/AAĜA{A
(�AA��A`BA��A�jA�AVA��A�
A 1@�~�@�V@��R@��D@�"�@��9@��
@��@�^@�9@��@�-@���@�p�@���@���@��@�@�^@��
@�^@�(�@�M�@��
@�1@�^5@��@ԓu@ӥ�@�l�@�l�@Ӿw@� �@�b@��m@��m@ӥ�@���@ѩ�@�p�@�/@�C�@���@�r�@�(�@�$�@ȼj@�K�@�ff@�%@��m@ǥ�@���@�A�@�V@�b@�5?@���@�x�@�7L@�V@�&�@��`@�+@�l�@�
=@+@��@��7@�hs@���@��\@�;d@�33@��y@�@���@��u@��
@�t�@�M�@��7@�/@��@��u@�r�@�  @�
=@��R@���@�n�@���@���@��7@�p�@�O�@�7L@�%@�Ĝ@�A�@���@���@���@�dZ@�
=@��@�v�@��@���@�x�@�/@��@��`@��u@���@��m@���@��w@���@�33@���@�n�@�{@��@��@���@�/@���@��@�1'@��
@��F@��P@�|�@��@��\@�v�@�V@���@��T@�@�p�@�/@�%@��@�j@�I�@� �@���@�dZ@�+@���@��H@��@��\@�E�@�@��^@���@��@�x�@�hs@�7L@��9@�r�@�b@�l�@�"�@�@���@��R@���@��@���@�?}@���@�I�@��@�  @�  @���@���@��@��m@�l�@��y@��!@���@�~�@�$�@��@���@��^@�x�@�V@��@���@�Ĝ@� �@�bN@��D@��w@���@�@���@�5?@�p�@�?}@�?}@�7L@�?}@���@�bN@���@��;@�ƨ@�l�@�;d@��@�
=@���@���@�ff@��@��-@�O�@��@�%@��@���@��@�j@�A�@�1'@�(�@� �@�1@���@��;@��F@�t�@�S�@�;d@��!@�5?@��#@��@��@��h@���@��-@�G�@��@��@��u@��D@��@�bN@�bN@�I�@�A�@�A�@�9X@�9X@�(�@���@��@�t�@�l�@�\)@��@��\@�~�@�M�@���@�@�J@z~�@o�@g
=@_�;@V�R@P��@H��@@Ĝ@9hs@1�@*M�@#�@�@-@�R@�@�+@
M�@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�
=A�VA�VA�JA�JA�VA�bA�oA��A��A��A��A��A��A��A��A��A��A�{A�A��HAŸRAŮAţ�Aś�AŃA�l�A�S�A�G�A�;dA�/A�1A���A���A��mA���A�ĜA���Aİ!AčPA�I�A�1A�ĜAã�AÕ�AÃA�z�A�v�A�p�A�p�A�n�A�n�A�n�A�n�A�n�A�p�A�p�A�hsA�ffA�ffA�bNA�VA�M�A�I�A�G�A�E�A�A�A�;dA�5?A�-A��A���A£�A��RA���A��
A�C�A��`A��\A��9A��!A�O�A�t�A��A�t�A�1A�(�A�7LA��^A�n�A��A���A���A��TA��A�A���A�JA��A���A�ƨA���A�  A�^5A�+A���A���A�5?A��A��7A��jA��wA�$�A�hsA��`A�-A�1A�1A�E�A�XA�%A���A��;A��A}��A{�TAyG�Ar�HAm��Ak\)AjZAi�#Ah��Af�HAeS�AbȴAa33A_��A]��A[G�AY|�AX$�AUx�AR��APM�ANĜAJjAIVAH�uAF�9AE�AB�A@�A> �A<�A:��A8~�A7��A7C�A5�mA3��A1�;A/��A/��A/t�A/33A.�A.9XA-;dA+�PA)��A)�A(r�A'K�A&9XA%�hA$��A"ffA ��A�A��A �A&�AVAK�A�TA��AdZA
=A�7A�A1'A�7A$�A�A?}A1A�A�/AAĜA{A
(�AA��A`BA��A�jA�AVA��A�
A 1@�~�@�V@��R@��D@�"�@��9@��
@��@�^@�9@��@�-@���@�p�@���@���@��@�@�^@��
@�^@�(�@�M�@��
@�1@�^5@��@ԓu@ӥ�@�l�@�l�@Ӿw@� �@�b@��m@��m@ӥ�@���@ѩ�@�p�@�/@�C�@���@�r�@�(�@�$�@ȼj@�K�@�ff@�%@��m@ǥ�@���@�A�@�V@�b@�5?@���@�x�@�7L@�V@�&�@��`@�+@�l�@�
=@+@��@��7@�hs@���@��\@�;d@�33@��y@�@���@��u@��
@�t�@�M�@��7@�/@��@��u@�r�@�  @�
=@��R@���@�n�@���@���@��7@�p�@�O�@�7L@�%@�Ĝ@�A�@���@���@���@�dZ@�
=@��@�v�@��@���@�x�@�/@��@��`@��u@���@��m@���@��w@���@�33@���@�n�@�{@��@��@���@�/@���@��@�1'@��
@��F@��P@�|�@��@��\@�v�@�V@���@��T@�@�p�@�/@�%@��@�j@�I�@� �@���@�dZ@�+@���@��H@��@��\@�E�@�@��^@���@��@�x�@�hs@�7L@��9@�r�@�b@�l�@�"�@�@���@��R@���@��@���@�?}@���@�I�@��@�  @�  @���@���@��@��m@�l�@��y@��!@���@�~�@�$�@��@���@��^@�x�@�V@��@���@�Ĝ@� �@�bN@��D@��w@���@�@���@�5?@�p�@�?}@�?}@�7L@�?}@���@�bN@���@��;@�ƨ@�l�@�;d@��@�
=@���@���@�ff@��@��-@�O�@��@�%@��@���@��@�j@�A�@�1'@�(�@� �@�1@���@��;@��F@�t�@�S�@�;d@��!@�5?@��#@��@��@��h@���@��-@�G�@��@��@��u@��D@��@�bN@�bN@�I�@�A�@�A�@�9X@�9X@�(�@���@��@�t�@�l�@�\)@��@��\@�~�@�M�@���@�@�J@z~�@o�@g
=@_�;@V�R@P��@H��@@Ĝ@9hs@1�@*M�@#�@�@-@�R@�@�+@
M�@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB7LB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB6FB5?B5?B5?B6FB7LB7LB:^B;dB<jB=qB=qB=qB>wB?}B?}B@�BA�BA�BA�BB�BD�BI�BO�BW
BZB\)B^5B`BBaHBbNBcTBcTBdZBdZBdZBe`Be`Be`BffBgmBgmBgmBiyBiyBjBjBjBk�Bk�Bl�Bl�Bm�Bo�BjB(�BuBVBJBDB1BB  B  B��B��B��B�B�#BȴB�3B�B�B��B�oB�1By�BffBN�BB�B+BoBB�mB�)B�
B��BÖB�?B�B��B�\B�Bx�Bm�BT�B=qB�BB
�mB
��B
�}B
��B
�7B
ffB
VB
J�B
;dB
%�B	��B	�HB	��B	ɺB	ŢB	�wB	�!B	��B	�7B	x�B	l�B	]/B	L�B	@�B	8RB	+B	�B	�B	oB		7B	B	B��B��B�B�;B��B��B��BɺBȴB��BƨB�wB�?B�-B�'B�!B�B�!B�3B�!B�B�B��B��B��B��B��B��B�{B�hB�\B�VB�JB�7B�+B�B�B�B�B~�B}�B{�Bz�Bx�Bu�Br�Bq�Bo�Bn�Bm�Bk�BjBgmBdZBcTBaHB`BB`BB`BB`BB]/B\)B\)B_;BcTBe`BffBgmBffBhsBjBjBjBk�Bk�Bl�Bl�Bl�Bk�Bk�Bl�Bl�Bn�Bn�Bp�Bq�Bm�Bk�B`BBYBXB]/B^5BbNBjBq�Bv�B|�B~�B�B�%B�+B�B�B�B|�Bv�Bt�BjBl�B�B��B��B��B�oB�bB�JB�Bw�Bv�B�%B��B��B��B��B��B��B��B�!B�-B�3B�B��B��B�B�FBȴB��B��B��B�
B�BB�ZB�fB�B�B�B��B��B��B��B	B	B	%B	+B	JB	PB	PB	PB	JB	JB	PB	\B	hB	oB	{B	{B	�B	�B	�B	�B	�B	�B	#�B	$�B	&�B	'�B	,B	0!B	1'B	1'B	2-B	33B	9XB	@�B	E�B	H�B	H�B	H�B	G�B	G�B	H�B	I�B	K�B	K�B	K�B	K�B	K�B	P�B	S�B	VB	XB	[#B	[#B	\)B	_;B	aHB	cTB	cTB	hsB	jB	k�B	l�B	o�B	r�B	t�B	u�B	u�B	w�B	y�B	{�B	|�B	}�B	~�B	~�B	~�B	� B	�B	�%B	�7B	�JB	�PB	�VB	�bB	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�9B	�?B	�jB	B	B	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�B	�B	�B	�#B	�/B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
PB
oB
�B
"�B
)�B
1'B
8RB
?}B
E�B
L�B
R�B
[#B
`BB
cTB
k�B
n�B
q�B
u�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B7mB6hB6hB6hB6hB6hB6hB6hB6iB6fB6iB6iB6iB6fB6fB6iB7jB7jB6iB5]B5aB5`B6dB7mB7kB:B;�B<�B=�B=�B=�B>�B?�B?�B@�BA�BA�BA�BB�BD�BI�BO�BW*BZ?B\IB^TB`eBahBbnBcuBcuBd{Bd{Bd{BeBe�Be�Bf�Bg�Bg�Bg�Bi�Bi�Bj�Bj�Bj�Bk�Bk�Bl�Bl�Bm�Bo�Bj�B)B�BsBjB`BMB<B  B B�	B�B��B�B�@B��B�RB�9B�'B��B��B�PBy�Bf�BN�BB�B+"B�B$B�B�GB�)B�BôB�_B�/B��B�|B�@Bx�Bm�BUB=�B�B4B
�B
��B
��B
�"B
�]B
f�B
V*B
J�B
;�B
&B	�B	�tB	�
B	��B	��B	��B	�OB	��B	�eB	yB	l�B	]_B	L�B	@�B	8�B	+3B	�B	�B	�B		jB	UB	FB�.B�	B��B�rB�7B�B�B��B��B��B��B��B�yB�fB�`B�\B�SB�ZB�kB�]B�UB�<B�,B�!B�
B� B��B��B��B��B��B��B��B�uB�iB�ZB�PB�IB�CB8B~1B|(B{ByBv Br�Bq�Bo�Bn�Bm�Bk�Bj�Bg�Bd�Bc�Ba�B`�B`�B`�B`�B]kB\iB\jB_{Bc�Be�Bf�Bg�Bf�Bh�Bj�Bj�Bj�Bk�Bk�Bl�Bl�Bl�Bk�Bk�Bl�Bl�Bn�Bn�Bp�Bq�Bm�Bk�B`�BYXBXPB]oB^sBb�Bj�Bq�Bw	B}-B:B�RB�cB�jB�UB�RB�KB}-BwBt�Bj�Bl�B�WB��B��B��B��B��B��B�MBxBwB�dB��B�B�B�%B�.B�5B�&B�]B�jB�qB�GB�B�+B�SB��B��B�B�B�-B�EB�}B�B�B��B��B��B��B��B��B�B	LB	WB	]B	dB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$B	%B	'!B	('B	,?B	0[B	1`B	1^B	2eB	3jB	9�B	@�B	E�B	H�B	H�B	H�B	G�B	G�B	H�B	I�B	LB	K�B	K�B	K�B	K�B	QB	T2B	V:B	XFB	[YB	[XB	\_B	_pB	a�B	c�B	c�B	h�B	j�B	k�B	l�B	o�B	r�B	t�B	u�B	u�B	xB	zB	|B	}%B	~*B	1B	.B	1B	�6B	�OB	�XB	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�0B	�<B	�@B	�@B	�OB	�VB	�TB	�VB	�aB	�kB	�rB	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	��B	��B	��B	�B	� B	�'B	�+B	�1B	�7B	�3B	�7B	�=B	�BB	�BB	�KB	�WB	�cB	�mB	�vB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�$B
IB
�B
�B
�B
#B
*,B
1XB
8�B
?�B
E�B
L�B
S!B
[RB
`sB
c�B
k�B
n�B
q�B
u�B
z	B
}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.33 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214302016053112143020160531121430  AO  ARCAADJP                                                                    20140721230524    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230524  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230524  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121430  IP                  G�O�G�O�G�O�                
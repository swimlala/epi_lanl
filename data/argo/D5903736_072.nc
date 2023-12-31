CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:44Z AOML 3.0 creation; 2016-05-31T19:14:36Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230544  20160531121436  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               HA   AO  4051_7090_072                   2C  D   APEX                            5368                            041511                          846 @����	1   @���� 
@5�33333�e'\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    HA   B   B   @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� DyS3D�fD�6fD�� D�ٚD���D�,�D�� D�ٚD�	�D�@ D��3D�ٚD�� D�@ Dڠ D�ٚD���D�0 D�|�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�ff@�ffA33A?33A`��A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB۳3B߳3B��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C`�Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'�3D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��DyP D��D�4�D�~fD�� D��3D�+3D��fD�� D� D�>fD���D�� D��fD�>fDڞfD�� D��3D�.fD�{3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A��A���A���A�~�A�l�A��A��!A�M�A�1'A��A�%A�  A��`A��uA�O�A�&�A��A���A��A�l�A�ZA�Q�A�"�A���A���A�S�A��A��jA��9A���A�n�A�?}A�33A��A��DA�z�A�\)A�&�A��HA�&�A��;A��jA��A�1'A��A�|�A�;dA�r�A��`A��7A�E�A�%A�\)A�33A�  A�ZA�~�A��
A���A��A�n�A��A��#A��DA�t�A�ZA��A�M�A�K�A���A�?}A�K�A��^A�"�A�ffA��yA��A�S�A�;dA��A��PA�ffA�1'A�Q�A�r�A�{A�|�A��/A�ĜA�S�A�l�A�oA�+A��FA�XA�bA���A�A��A��hA�x�A�Q�A���A�dZA��
A��A�t�A�  A�jA��9A��A�dZA�33A�&�A�hA~�A}S�A{��A{�PA{�Az�`Ay�Ax��Aw��Au�7Asp�ArffAp�Aox�An-Amt�Aj��AhQ�Ag7LAfbNAe�Ad�HA`~�A]��A];dA\9XAYƨAW�AU\)ARA�AP�AO7LAN�DAN�AM��AMG�AJJAH��AFȴAD�+AC&�ABn�A@�A>E�A=��A<bA:ffA9O�A8=qA8$�A8JA7�A7|�A6v�A6I�A6  A5A3�7A2��A2n�A1�mA0�A/G�A.��A-S�A+�A)
=A'��A'�A'\)A%O�A"z�A!�PA��AȴA��A��Ar�A�-A�A��A|�A=qA��A�DA�A�A�AS�AZA
��A
5?A	A	A^5A�A�PA&�A�\A�A�HA9XAO�AVAhsA -@�ƨ@��@��^@�@��;@�^@�9@���@�K�@�@�F@��@��`@�(�@�F@��@旍@�J@��@��@�M�@��@�|�@�%@۾w@ڏ\@�7L@��y@֗�@�~�@�E�@���@�G�@���@ԋD@�n�@�O�@�1@Η�@�5?@�@��`@�1@��@��
@˥�@�t�@�n�@ɉ7@�A�@�33@��@�ff@�%@ă@�A�@�9X@å�@�33@��y@�@�-@���@�`B@�hs@�X@���@�r�@�A�@�  @�dZ@�
=@�ff@���@�b@�$�@��@�&�@�1@�|�@��@�@���@�ff@�E�@�E�@�=q@�hs@�(�@���@�|�@�o@�@��y@�~�@��@���@�%@���@��u@�j@���@�t�@��!@�=q@��@�7L@���@���@�K�@��y@�V@�$�@�{@��^@��h@�`B@��@��@�j@�b@��@��;@���@��@��H@���@�~�@�^5@�=q@�J@��h@�hs@�G�@��/@��@��@��
@���@�l�@�;d@�
=@��y@�ȴ@��!@���@�ff@�M�@���@���@���@���@��+@�=q@�-@��T@��h@�?}@��@��@��@�Z@��m@�ƨ@��@���@���@�@���@���@�^5@��@���@���@��h@�x�@�hs@�`B@�X@�7L@���@��@��@��u@�Q�@���@�t�@�;d@�
=@���@�n�@�^5@�E�@�@��^@�G�@��/@�A�@�1@��@�+@���@�~�@�v�@�^5@�E�@�$�@�{@��T@�G�@�/@�V@��j@��@�bN@�9X@��@�b@�b@��@�|�@�S�@��@���@�M�@��@�?}@�j@� �@��@��@� �@�1@��;@���@�C�@���@��\@�@���@��h@�&�@���@��j@��9@��@���@�bN@�A�@�(�@� �@� �@��@�b@��
@��w@�;d@��F@�Z@y�^@v��@mp�@g�@]�-@S��@N5?@E?}@9�@2�@-�@%��@�w@�@&�@��@1'@��@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A��A���A���A�~�A�l�A��A��!A�M�A�1'A��A�%A�  A��`A��uA�O�A�&�A��A���A��A�l�A�ZA�Q�A�"�A���A���A�S�A��A��jA��9A���A�n�A�?}A�33A��A��DA�z�A�\)A�&�A��HA�&�A��;A��jA��A�1'A��A�|�A�;dA�r�A��`A��7A�E�A�%A�\)A�33A�  A�ZA�~�A��
A���A��A�n�A��A��#A��DA�t�A�ZA��A�M�A�K�A���A�?}A�K�A��^A�"�A�ffA��yA��A�S�A�;dA��A��PA�ffA�1'A�Q�A�r�A�{A�|�A��/A�ĜA�S�A�l�A�oA�+A��FA�XA�bA���A�A��A��hA�x�A�Q�A���A�dZA��
A��A�t�A�  A�jA��9A��A�dZA�33A�&�A�hA~�A}S�A{��A{�PA{�Az�`Ay�Ax��Aw��Au�7Asp�ArffAp�Aox�An-Amt�Aj��AhQ�Ag7LAfbNAe�Ad�HA`~�A]��A];dA\9XAYƨAW�AU\)ARA�AP�AO7LAN�DAN�AM��AMG�AJJAH��AFȴAD�+AC&�ABn�A@�A>E�A=��A<bA:ffA9O�A8=qA8$�A8JA7�A7|�A6v�A6I�A6  A5A3�7A2��A2n�A1�mA0�A/G�A.��A-S�A+�A)
=A'��A'�A'\)A%O�A"z�A!�PA��AȴA��A��Ar�A�-A�A��A|�A=qA��A�DA�A�A�AS�AZA
��A
5?A	A	A^5A�A�PA&�A�\A�A�HA9XAO�AVAhsA -@�ƨ@��@��^@�@��;@�^@�9@���@�K�@�@�F@��@��`@�(�@�F@��@旍@�J@��@��@�M�@��@�|�@�%@۾w@ڏ\@�7L@��y@֗�@�~�@�E�@���@�G�@���@ԋD@�n�@�O�@�1@Η�@�5?@�@��`@�1@��@��
@˥�@�t�@�n�@ɉ7@�A�@�33@��@�ff@�%@ă@�A�@�9X@å�@�33@��y@�@�-@���@�`B@�hs@�X@���@�r�@�A�@�  @�dZ@�
=@�ff@���@�b@�$�@��@�&�@�1@�|�@��@�@���@�ff@�E�@�E�@�=q@�hs@�(�@���@�|�@�o@�@��y@�~�@��@���@�%@���@��u@�j@���@�t�@��!@�=q@��@�7L@���@���@�K�@��y@�V@�$�@�{@��^@��h@�`B@��@��@�j@�b@��@��;@���@��@��H@���@�~�@�^5@�=q@�J@��h@�hs@�G�@��/@��@��@��
@���@�l�@�;d@�
=@��y@�ȴ@��!@���@�ff@�M�@���@���@���@���@��+@�=q@�-@��T@��h@�?}@��@��@��@�Z@��m@�ƨ@��@���@���@�@���@���@�^5@��@���@���@��h@�x�@�hs@�`B@�X@�7L@���@��@��@��u@�Q�@���@�t�@�;d@�
=@���@�n�@�^5@�E�@�@��^@�G�@��/@�A�@�1@��@�+@���@�~�@�v�@�^5@�E�@�$�@�{@��T@�G�@�/@�V@��j@��@�bN@�9X@��@�b@�b@��@�|�@�S�@��@���@�M�@��@�?}@�j@� �@��@��@� �@�1@��;@���@�C�@���@��\@�@���@��h@�&�@���@��j@��9@��@���@�bN@�A�@�(�@� �@� �@��@�b@��
@��wG�O�@��F@�Z@y�^@v��@mp�@g�@]�-@S��@N5?@E?}@9�@2�@-�@%��@�w@�@&�@��@1'@��@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBhBbBhBoBoBoBhB\BDB1B+B+B1B1B	7B	7BDB	7BB��B��B�B�B�B�B�fB�NB�BB�#B�
B�B�B��B��B��B��B��BǮBB�}B�RB�FB�B��B��B��B��B�oB��B��B�bBq�B`BB=qB�BDBB��B��B�B�B�B�BƨB��B�-B��B�BhsB[#BO�B9XB�B��B�B��B�RB�3B��B��B�hB�+B|�Bx�Be`BVBA�B49B-B&�B"�B�BoB%BB  B
��B
�B
��B
�}B
�?B
�B
��B
��B
�hB
�%B
|�B
n�B
aHB
\)B
T�B
M�B
F�B
D�B
A�B
?}B
9XB
2-B
)�B
�B
oB
DB
B	��B	�B	�B	�5B	��B	��B	ǮB	B	�dB	��B	��B	��B	�bB	�B	v�B	n�B	`BB	XB	T�B	Q�B	O�B	M�B	H�B	<jB	5?B	.B	%�B	 �B	�B	�B	PB		7B	B��B��B��B�B�B�B�B�B�B�B�fB�HB�;B�/B�B�B��B��BǮB��B�^B�LB�?B�-B�B��B��B��B��B�oB�bB�PB�7B�B�B~�B}�B{�By�Bw�Bu�Bt�Br�Bp�Bo�Bn�Bm�Bl�Bk�BjBjBiyBhsBffBe`BcTBaHBaHB`BB`BB_;B^5B\)B[#B]/B_;B_;B`BBbNBbNB_;B^5B^5B_;B_;B`BB`BB`BB`BB`BBe`BffBe`BhsBiyBk�Bl�Br�Bs�Bs�Bs�Bs�Bt�Bt�Bt�Bx�By�B{�B� B�B�B�+B�DB�JB�PB�\B�hB��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�^B�wB�}B�}B�}B�}B�}B��B��BÖBǮB��B��B�
B�#B�;B�NB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	
=B	PB	hB	�B	�B	�B	�B	 �B	$�B	'�B	(�B	)�B	+B	.B	0!B	0!B	0!B	0!B	1'B	5?B	6FB	7LB	8RB	8RB	9XB	;dB	=qB	=qB	A�B	E�B	E�B	F�B	G�B	I�B	J�B	L�B	M�B	M�B	N�B	O�B	P�B	T�B	YB	_;B	bNB	ffB	gmB	hsB	iyB	k�B	n�B	q�B	s�B	v�B	x�B	{�B	�B	�B	�B	�B	�B	�=B	�JB	�JB	�VB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�LB	�XB	�qB	�}B	��B	ĜB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�;B	�;B	�HB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
\B
�B
�B
 �B
%�B
/B
8RB
;dB
@�B
K�B
R�B
VB
[#B
aHB
dZB
hsB
m�B
q�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BBuBoBtB{BzB|ByBgBTB@B7B:B?B>B	FB	FBQB	BBB� B��B�B�B�B�B�pB�\B�MB�4B�B�B�B��B��B��B��B��BǸBB��B�]B�RB�B��B�B��B��B�xB��B��B�kBq�B`KB=xB�BLBB��B��B�B�B�B�!BƭB��B�4B��B�BhyB[+BO�B9^B�B��B�	B��B�SB�8B��B��B�pB�/B|�Bx�BehBVBA�B4=B-B&�B"�B�BuB-BB B
��B
�B
��B
��B
�EB
�B
��B
��B
�rB
�.B
|�B
n�B
aSB
\6B
UB
M�B
F�B
D�B
A�B
?�B
9cB
27B
*B
�B
yB
PB
B	��B	�B	�B	�DB	��B	��B	ǿB	B	�tB	��B	��B	��B	�uB	�*B	v�B	n�B	`SB	X"B	UB	Q�B	O�B	M�B	H�B	<~B	5SB	.*B	%�B	 �B	�B	�B	gB		LB	$B��B��B��B��B��B��B�B��B�B�B�|B�aB�UB�EB�6B�!B�B��B��B��B�vB�dB�WB�FB�$B��B��B��B��B��B�}B�jB�SB�:B�#BB~B|By�Bw�Bu�Bt�Br�Bp�Bo�Bn�Bm�Bl�Bk�Bj�Bj�Bi�Bh�Bf�Be~BcnBagBaeB`^B`_B_ZB^RB\EB[BB]JB_YB_YB`^BbjBbmB_ZB^PB^RB_YB_YB`bB`_B``B`aB``Be{Bf�Be~Bh�Bi�Bk�Bl�Br�Bs�Bs�Bs�Bs�Bt�Bt�Bt�Bx�By�B|B�B� B�&B�HB�_B�eB�kB�xB��B��B��B��B��B��B��B��B�B�B�B�"B�&B�KB�_B�vB��B��B��B��B��B��B��B��BïB��B��B��B�"B�9B�RB�dB�B�B�B�B�B�B��B��B�B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B�B	B	'B	4B	
SB	fB	{B	�B	�B	�B	�B	 �B	$�B	(B	)
B	*B	+B	.'B	05B	08B	04B	06B	1;B	5SB	6XB	7_B	8fB	8dB	9kB	;zB	=�B	=�B	A�B	E�B	E�B	F�B	G�B	I�B	J�B	L�B	M�B	M�B	N�B	O�B	P�B	UB	Y,B	_LB	b`B	fvB	g�B	h�B	i�B	k�B	n�B	q�B	s�B	v�B	x�B	{�B	�B	�B	�#B	�'B	�*B	�PB	�[B	�]B	�fB	�sB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�%B	�*B	�2B	�5B	�>B	�FB	�\B	�eB	�B	��B	��B	ĪB	ǾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�"B	�*B	�2B	�<B	�HB	�HB	�WB	�cB	�hB	�hB	�lB	�tB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
 B
jB
�B
�B
 �B
%�B
/&B
8^B
;qB
@�B
K�B
R�B
VB
[.B
aSB
deB
h{B
m�B
q�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214362016053112143620160531121436  AO  ARCAADJP                                                                    20140721230544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230544  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230544  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121436  IP                  G�O�G�O�G�O�                
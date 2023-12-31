CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:40Z AOML 3.0 creation; 2016-06-01T00:08:16Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230840  20160531170816  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               DA   AO  4055_7112_068                   2C  D   APEX                            5374                            041511                          846 @����1   @��	N�@;K��Q��d(�\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    DA   A   A   @�ff@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�3D� D�C3D��fD�� D�fD�<�D�s3D��fD��D�<�D�l�D��3D�3D�9�D�|�D�� D�3D�9�D�s3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�ffA33A%��AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Br33By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN�gDO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�3Do#3Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du Dy� D�fD�Q�D���D��fD��D�K3D���D���D�+3D�K3D�{3D��D��D�H Dڋ3D��fD�!�D�H D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�5?A�33A�7LA�7LA�9XA�=qA�=qA�?}A�A�A�?}A�=qA�7LA�5?A�/A�/A�1'A�33A�/A�-A�/A�-A�33A�/A�/A�/A�1'A�1'A�1'A�/A�/A�-A�+A�$�A�$�A� �A� �A��A� �A�(�A�+A�+A�+A�&�A��A��A�bA�VA�bA�oA�VA�VA�VA�JA�
=A�1A��A��#A���A��A�A�A��`A�VA��-A�hsA�=qA��FA���A��A�
=A��PA��A�%A�M�A��;A�\)A�Q�A�$�A��HA��A��A�t�A���A���A��RA�ZA�|�A��A��PA��TA��wA�z�A�oA��\A��A�JA���A�JA��;A��A�JA�ƨA~bNAzjAy�hAy�Ax$�Avv�AtA�An�jAkl�AhĜAg�7Ag%Afv�Ae��Ad�Ab�HA^��A\VAZbAY�wAX�yAWAVn�AU�AU�PAU\)AU�ATAS33AR�RARVAQ��AQ��AQx�AP�AN�uAJ�AF�DAEVADA�AC��AC�#AC�7AB��AA�TA@�A=�A<v�A;S�A:^5A9�#A9O�A8�A7�#A7�A7A6��A6ffA6  A5�#A5��A5p�A5�A4�!A49XA3�-A3A2~�A2�A1p�A0ffA/7LA.�RA.{A-K�A,VA+�A*�9A*ZA)S�A(bNA'�A'�A&�A%��A$�HA#hsA"�A!�^A 1'A|�A�AAVA9XA�FAS�A�PA;dA�wA��AO�A��A�!A��Az�A�AI�A�mA�#AA��AdZA�+A/A+A	��A	7LA{A��A^5AdZAI�A�PA �+A J@���@���@��j@��y@���@�I�@�1@���@���@�M�@��9@�@��@�l�@��#@���@���@�?}@�D@�@���@�@�F@�"�@��@旍@�J@�7L@� �@�F@�|�@�K�@��y@⟾@�{@��/@�t�@�K�@�33@�-@��
@�z�@և+@�x�@�&�@�1'@��;@ӶF@ӍP@�t�@�@�ȴ@җ�@�x�@���@�9X@�t�@��@�~�@̬@˕�@���@���@�dZ@��T@�`B@��@���@���@���@ļj@Ĵ9@Ĵ9@ēu@Õ�@���@�n�@�5?@�O�@��9@��@��@�5?@��#@��-@��h@��D@�A�@��P@��R@�X@�t�@�@��R@��\@�-@�?}@�9X@�dZ@�C�@��@�X@��@�Ĝ@�;d@��9@�A�@�  @��@��@�@���@��u@�M�@�%@��D@�ƨ@�"�@���@��^@���@�1'@�C�@��!@�n�@��@���@��@�O�@�7L@��@�r�@��
@�
=@�n�@�=q@���@�/@��@��P@�33@��y@���@���@���@�~�@�V@�J@��T@�@��@���@�t�@���@�ȴ@�$�@�x�@��`@�z�@�I�@� �@��@��@�ƨ@���@�l�@�S�@�K�@�;d@���@�~�@��@��@��7@�7L@�/@��@��j@��9@��9@��@�j@�(�@��@���@�l�@�K�@�;d@�+@���@�^5@�E�@�V@��@��#@�@���@�p�@�/@�%@��j@�z�@�bN@��@��w@��P@�l�@�S�@�33@�
=@�ȴ@���@�~�@�=q@�{@�@���@���@�X@�V@��`@���@���@���@��9@�A�@���@��w@���@�K�@��!@�n�@�ff@�^5@�M�@�E�@�5?@�-@�$�@��@�{@�@��@��#@��^@���@�x�@�7L@��@��u@�Z@�A�@�b@��@~�@~$�@}�T@}`B@|�@|�/@|�/@|��@y�@n��@dj@^{@Z=q@Q��@K�@Fff@B=q@:�\@6v�@0 �@*~�@&@"��@��@ �@��@j@	hs@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�33A�7LA�7LA�9XA�=qA�=qA�?}A�A�A�?}A�=qA�7LA�5?A�/A�/A�1'A�33A�/A�-A�/A�-A�33A�/A�/A�/A�1'A�1'A�1'A�/A�/A�-A�+A�$�A�$�A� �A� �A��A� �A�(�A�+A�+A�+A�&�A��A��A�bA�VA�bA�oA�VA�VA�VA�JA�
=A�1A��A��#A���A��A�A�A��`A�VA��-A�hsA�=qA��FA���A��A�
=A��PA��A�%A�M�A��;A�\)A�Q�A�$�A��HA��A��A�t�A���A���A��RA�ZA�|�A��A��PA��TA��wA�z�A�oA��\A��A�JA���A�JA��;A��A�JA�ƨA~bNAzjAy�hAy�Ax$�Avv�AtA�An�jAkl�AhĜAg�7Ag%Afv�Ae��Ad�Ab�HA^��A\VAZbAY�wAX�yAWAVn�AU�AU�PAU\)AU�ATAS33AR�RARVAQ��AQ��AQx�AP�AN�uAJ�AF�DAEVADA�AC��AC�#AC�7AB��AA�TA@�A=�A<v�A;S�A:^5A9�#A9O�A8�A7�#A7�A7A6��A6ffA6  A5�#A5��A5p�A5�A4�!A49XA3�-A3A2~�A2�A1p�A0ffA/7LA.�RA.{A-K�A,VA+�A*�9A*ZA)S�A(bNA'�A'�A&�A%��A$�HA#hsA"�A!�^A 1'A|�A�AAVA9XA�FAS�A�PA;dA�wA��AO�A��A�!A��Az�A�AI�A�mA�#AA��AdZA�+A/A+A	��A	7LA{A��A^5AdZAI�A�PA �+A J@���@���@��j@��y@���@�I�@�1@���@���@�M�@��9@�@��@�l�@��#@���@���@�?}@�D@�@���@�@�F@�"�@��@旍@�J@�7L@� �@�F@�|�@�K�@��y@⟾@�{@��/@�t�@�K�@�33@�-@��
@�z�@և+@�x�@�&�@�1'@��;@ӶF@ӍP@�t�@�@�ȴ@җ�@�x�@���@�9X@�t�@��@�~�@̬@˕�@���@���@�dZ@��T@�`B@��@���@���@���@ļj@Ĵ9@Ĵ9@ēu@Õ�@���@�n�@�5?@�O�@��9@��@��@�5?@��#@��-@��h@��D@�A�@��P@��R@�X@�t�@�@��R@��\@�-@�?}@�9X@�dZ@�C�@��@�X@��@�Ĝ@�;d@��9@�A�@�  @��@��@�@���@��u@�M�@�%@��D@�ƨ@�"�@���@��^@���@�1'@�C�@��!@�n�@��@���@��@�O�@�7L@��@�r�@��
@�
=@�n�@�=q@���@�/@��@��P@�33@��y@���@���@���@�~�@�V@�J@��T@�@��@���@�t�@���@�ȴ@�$�@�x�@��`@�z�@�I�@� �@��@��@�ƨ@���@�l�@�S�@�K�@�;d@���@�~�@��@��@��7@�7L@�/@��@��j@��9@��9@��@�j@�(�@��@���@�l�@�K�@�;d@�+@���@�^5@�E�@�V@��@��#@�@���@�p�@�/@�%@��j@�z�@�bN@��@��w@��P@�l�@�S�@�33@�
=@�ȴ@���@�~�@�=q@�{@�@���@���@�X@�V@��`@���@���@���@��9@�A�@���@��w@���@�K�@��!@�n�@�ff@�^5@�M�@�E�@�5?@�-@�$�@��@�{@�@��@��#@��^@���@�x�@�7L@��@��u@�Z@�A�@�b@��@~�@~$�@}�T@}`B@|�@|�/@|�/@|��@y�@n��@dj@^{@Z=q@Q��@K�@Fff@B=q@:�\@6v�@0 �@*~�@&@"��@��@ �@��@j@	hs@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�BJ�B�B��B�9B�B��B��B��B��B��B�B^5B�B��B�LB��B��B�hB�hB��B�uB�Bu�BffB`BBVB>wB%�B�BuBJB%BB
��B
��B
�B
�TB
��B
��B
�-B
��B
�PB
m�B
J�B
(�B
oB
JB
+B	��B	�B	�BB	ǮB	�!B	��B	��B	��B	��B	��B	��B	�1B	r�B	gmB	]/B	ZB	VB	O�B	I�B	G�B	F�B	E�B	C�B	?}B	<jB	:^B	8RB	6FB	5?B	33B	-B	!�B	uB	B��B��B��B�B�B�B�yB�TB�#B�
B��B��B��B��BƨBĜBÖB��B��B��B�wB�qB�qB�jB�^B�XB�LB�?B�9B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�JB�7B�%B�B� B|�Bz�Bw�Bu�Bs�Bp�Bn�Bk�Be`B`BB^5B[#BYBXBW
BVBT�BQ�BO�BO�BN�BN�BM�BK�BH�BC�B@�B>wB;dB9XB6FB49B1'B0!B/B.B-B,B+B(�B&�B&�B%�B$�B$�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBuBhB\BPBVB\B\B\BbBbBbBbBbBbBbBbBhBhBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B!�B"�B#�B%�B'�B'�B'�B'�B)�B)�B+B,B.B33B49B49B49B49B6FB8RB:^B9XB<jB=qB=qB=qB?}BG�BH�BI�BK�BL�BP�BQ�BT�B`BBgmBiyBm�Bp�Br�Bv�By�B}�B�B�B�B�%B�1B�7B�=B�=B�DB�VB�hB��B��B��B��B��B��B�B�B�B�'B�-B�-B�-B�9B�FB�LB�RB�XB�dBƨBɺB��B��B��B�B�/B�5B�;B�HB�HB�NB�TB�ZB�`B�`B�`B�mB�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	1B	
=B	DB	
=B	JB	VB	VB	\B	hB	uB	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	&�B	)�B	,B	-B	/B	1'B	1'B	33B	5?B	7LB	:^B	<jB	<jB	<jB	<jB	=qB	A�B	D�B	F�B	G�B	J�B	O�B	R�B	R�B	S�B	S�B	S�B	T�B	T�B	VB	VB	VB	W
B	XB	XB	YB	ZB	\)B	^5B	aHB	dZB	ffB	gmB	hsB	k�B	n�B	q�B	r�B	u�B	x�B	x�B	x�B	y�B	�DB	�B	��B	�5B	�B
  B
VB
�B
�B
+B
1'B
9XB
A�B
E�B
J�B
Q�B
W
B
_;B
hsB
l�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B��B��B��B��B��B��B��B��B��B��B��B��B��B�fB��BJ�B�VB̠B�B��B��B��B��B��B�uB��B^BcB��B�$B��B��B�<B�<B�cB�IB��Bu�Bf<B`BU�B>QB%�BgBKBB�B�B
��B
��B
�nB
�,B
��B
�[B
�B
��B
�(B
miB
J�B
(�B
EB
"B
B	��B	�B	�B	ǊB	��B	��B	��B	��B	��B	��B	�]B	�B	r�B	gIB	]B	Y�B	U�B	O�B	I�B	G�B	F�B	E�B	CqB	?^B	<HB	:<B	80B	6$B	5B	3B	,�B	!�B	RB	�B��B��B��B�B��B�pB�ZB�4B�B��B��B��BͳBʣBƈB�~B�vB�mB�cB�fB�YB�PB�RB�MB�=B�9B�+B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�mB�dB�UB�DB�:B�*B�B�B��B�B|�Bz�Bw�Bu�Bs�Bp�BnzBkgBeAB`%B^B[BX�BW�BV�BU�BT�BQ�BO�BO�BN�BN�BM�BK�BH�BCzB@fB>XB;GB9<B6)B4B1B0B.�B-�B,�B+�B*�B(�B&�B&�B%�B$�B$�B#�B"�B!�B �B�B�B�B�B�B�B�BtB�B�B_B{B}BuBYBoBmBnBpBqBOBdB@BZBXBVB0B$B2BB=B$B<B(B(BABDB)B'B)B)B1B/B5BABhBNB`B|BgBfB�B�B�B�B�B�B�B�B�B�B�B|B�B �B�B!�B"�B#�B%�B'�B'�B'�B'�B)�B)�B*�B+�B-�B3B4B4B4B4B6$B8.B::B99B<IB=QB=OB=QB?YBG�BH�BI�BK�BL�BP�BQ�BT�B`BgIBiUBmkBp�Br�Bv�By�B}�B��B��B��B��B�
B�B�B�B�B�-B�BB�VB�uB�xB��B��B��B��B��B��B��B�B�B�B�B�B�$B�&B�1B�:B�BɐBʘBϵB��B��B�B�
B�B�B�B�#B�'B�/B�5B�3B�4B�CB�UB�gB�lB�}B�B��B��B��B��B��B��B��B��B��B��B	 �B	�B	�B	�B	B	
B	B	
B	B	,B	)B	0B	9B	GB	WB	YB	lB	qB	~B	 �B	"�B	#�B	$�B	%�B	&�B	)�B	+�B	,�B	.�B	0�B	0�B	3B	5B	7B	:/B	<:B	<9B	<;B	<:B	=BB	AZB	DlB	FxB	G~B	J�B	O�B	R�B	R�B	S�B	S�B	S�B	T�B	T�B	U�B	U�B	U�B	V�B	W�B	W�B	X�B	Y�B	[�B	^B	aB	d,B	f3B	g;B	hCB	kVB	nhB	qxB	rB	u�B	x�B	x�B	x�B	y�B	�B	��B	ΦB	�B	�QB	��B
!B
XB
�B
*�B
0�B
9"B
ASB
EkB
J�B
Q�B
V�B
_B
h<B
lUB
qu111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708162016053117081620160531170816  AO  ARCAADJP                                                                    20140721230840    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230840  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230840  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170816  IP                  G�O�G�O�G�O�                
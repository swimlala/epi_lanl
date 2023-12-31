CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-09T07:21:05Z AOML 3.0 creation; 2016-08-09T21:43:21Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160409072105  20160809144321  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               DA   AO  5287_9017_068                   2C  D   APEX                            6529                            072314                          846 @�e�����1   @�e��5~@1�$�/�d�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    DA   B   B   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B��B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD�fD�S3D�� D��fD�3D�9�D�|�D�� D�3D�L�D�|�D���D�	�D�S3Dڌ�D�ٚD��D�33D� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�33A33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bj33Bq��By��B��3B��fB��fB��B��B��fB��fB��fB��fB��fB��fB��fB��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cd��Cfs3ChY�Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D#3D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'#3D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt� Dy�3D�$�D�a�D��fD���D�!�D�H D��3D��fD�!�D�[3D��3D��3D� D�a�Dڛ3D�� D�3D�A�D�fD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�7A��A�A��A�DA�r�A�l�A�`BA�XA�I�A�7LA�/A�1'A�(�A�"�A� �A��A��A��A�oA�VA�1A���A�r�A���A���A��
A�E�A�=qAֶFAԩ�A��A��#AЋDAμjA�t�A�VA�7LA�ƨA�^5A�"�A�-AȶFA�v�A�A�A��A��A���A�5?A�ĜA�(�A��`A�G�A�K�A�
=A�O�A��HA��FA�bA��A��A��A�VA�
=A�(�A�ƨA�XA�"�A��/A��A�"�A���A���A���A��`A��wA�1'A�t�A�K�A���A�z�A�ffA�VA���A��TA�jA�ZA�ZA�33A�  A�v�A�I�A�z�A�n�A���A�A��mA��A��/A��TA��!A���A�hsA�$�A��/A~�/Ax��Au|�At��Aq�
Ar��Ap^5Anz�Al�9Ah�\AfĜAe?}Ab��A_`BA]�A\�9AZ��AXn�AV�RAS?}AQ��AN9XAK"�AI�AHr�AB��A@ĜA?��A=�mA;p�A:�A8M�A6�A4n�A2��A1�mA1`BA0��A/A-�A,n�A*�A)?}A(�A&��A%��A$ffA"��A"bA!�FA ��A 1'A
=A�hAffA�A�!A�uAA�A��A�+AE�A�AĜA�9A�+AA�A�TAƨA�hAhsA;dA�A�A"�A�A9XA��A"�A9XAJAA�#AK�A��A�7AA��A��A��A
�DA	XA	�A�jAM�A�A�^A|�AC�AVAn�A�;A\)A�Ar�AM�A(�A�;A��AZAVA  A�A r�A E�A (�A   @�|�@�33@�"�@���@���@�n�@���@��h@��@��;@���@�E�@��@�`B@�A�@��@�v�@�\)@�l�@���@��\@�-@�$�@�{@�J@��-@�V@��m@�\@�j@�9X@�Q�@��@��y@�M�@�v�@@�@�%@�D@�w@�"�@��@�-@���@��@�@�@�j@��@�!@�w@�S�@㕁@�@�P@�R@�E�@�$�@�"�@���@�@�@�r�@�dZ@�o@���@޸R@�ȴ@���@ް!@�ff@ݲ-@�7L@� �@�;d@�o@�@�^5@٩�@��@���@ؼj@ؓu@�1@�;d@և+@��@պ^@�x�@ӝ�@�C�@���@�=q@Ѳ-@�Ĝ@�bN@ϕ�@��@�O�@̓u@���@�K�@��H@ʸR@�$�@�`B@ȓu@��@ȓu@���@���@�M�@�-@�x�@��/@��@��@��#@�hs@�G�@�7L@��`@��u@�Z@�Q�@�I�@��@��;@���@��@��
@�ƨ@���@���@�t�@�K�@�t�@���@���@���@��-@��@��@�Z@�(�@��@�S�@�33@���@���@�v�@��@��@��/@��@�Z@��F@���@�t�@�+@�-@��@��9@�A�@���@��w@�ƨ@�\)@�@���@�ȴ@�E�@�p�@��@�Z@�1'@�  @�S�@��\@��#@���@�(�@��F@�l�@�"�@��y@���@�V@�5?@�{@�J@�J@���@��@�1@��@���@�|�@��@�p�@��9@��9@���@��u@��@�Q�@�b@���@��
@�dZ@�+@��H@���@�E�@��@��7@�x�@�p�@�p�@�hs@�7L@��@��`@���@��@�9X@��;@�t�@��@�ff@�{@�@�7L@��/@���@��9@��u@��D@�bN@��@�t�@�+@�
=@��@�=q@��#@�hs@��9@�bN@�I�@�(�@�  @���@��;@���@���@�v�@��^@��/@�Ĝ@��9@���@�z�@���@��@�z�@��@y��@n�y@f5?@\Z@Tz�@L�j@D�D@>@8��@3�@+�
@$z�@
=@=q@/@%@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A�7A��A�A��A�DA�r�A�l�A�`BA�XA�I�A�7LA�/A�1'A�(�A�"�A� �A��A��A��A�oA�VA�1A���A�r�A���A���A��
A�E�A�=qAֶFAԩ�A��A��#AЋDAμjA�t�A�VA�7LA�ƨA�^5A�"�A�-AȶFA�v�A�A�A��A��A���A�5?A�ĜA�(�A��`A�G�A�K�A�
=A�O�A��HA��FA�bA��A��A��A�VA�
=A�(�A�ƨA�XA�"�A��/A��A�"�A���A���A���A��`A��wA�1'A�t�A�K�A���A�z�A�ffA�VA���A��TA�jA�ZA�ZA�33A�  A�v�A�I�A�z�A�n�A���A�A��mA��A��/A��TA��!A���A�hsA�$�A��/A~�/Ax��Au|�At��Aq�
Ar��Ap^5Anz�Al�9Ah�\AfĜAe?}Ab��A_`BA]�A\�9AZ��AXn�AV�RAS?}AQ��AN9XAK"�AI�AHr�AB��A@ĜA?��A=�mA;p�A:�A8M�A6�A4n�A2��A1�mA1`BA0��A/A-�A,n�A*�A)?}A(�A&��A%��A$ffA"��A"bA!�FA ��A 1'A
=A�hAffA�A�!A�uAA�A��A�+AE�A�AĜA�9A�+AA�A�TAƨA�hAhsA;dA�A�A"�A�A9XA��A"�A9XAJAA�#AK�A��A�7AA��A��A��A
�DA	XA	�A�jAM�A�A�^A|�AC�AVAn�A�;A\)A�Ar�AM�A(�A�;A��AZAVA  A�A r�A E�A (�A   @�|�@�33@�"�@���@���@�n�@���@��h@��@��;@���@�E�@��@�`B@�A�@��@�v�@�\)@�l�@���@��\@�-@�$�@�{@�J@��-@�V@��m@�\@�j@�9X@�Q�@��@��y@�M�@�v�@@�@�%@�D@�w@�"�@��@�-@���@��@�@�@�j@��@�!@�w@�S�@㕁@�@�P@�R@�E�@�$�@�"�@���@�@�@�r�@�dZ@�o@���@޸R@�ȴ@���@ް!@�ff@ݲ-@�7L@� �@�;d@�o@�@�^5@٩�@��@���@ؼj@ؓu@�1@�;d@և+@��@պ^@�x�@ӝ�@�C�@���@�=q@Ѳ-@�Ĝ@�bN@ϕ�@��@�O�@̓u@���@�K�@��H@ʸR@�$�@�`B@ȓu@��@ȓu@���@���@�M�@�-@�x�@��/@��@��@��#@�hs@�G�@�7L@��`@��u@�Z@�Q�@�I�@��@��;@���@��@��
@�ƨ@���@���@�t�@�K�@�t�@���@���@���@��-@��@��@�Z@�(�@��@�S�@�33@���@���@�v�@��@��@��/@��@�Z@��F@���@�t�@�+@�-@��@��9@�A�@���@��w@�ƨ@�\)@�@���@�ȴ@�E�@�p�@��@�Z@�1'@�  @�S�@��\@��#@���@�(�@��F@�l�@�"�@��y@���@�V@�5?@�{@�J@�J@���@��@�1@��@���@�|�@��@�p�@��9@��9@���@��u@��@�Q�@�b@���@��
@�dZ@�+@��H@���@�E�@��@��7@�x�@�p�@�p�@�hs@�7L@��@��`@���@��@�9X@��;@�t�@��@�ff@�{@�@�7L@��/@���@��9@��u@��D@�bN@��@�t�@�+@�
=@��@�=q@��#@�hs@��9@�bN@�I�@�(�@�  @���@��;@���@���@�v�@��^@��/@�Ĝ@��9@���G�O�@���@��@�z�@��@y��@n�y@f5?@\Z@Tz�@L�j@D�D@>@8��@3�@+�
@$z�@
=@=q@/@%@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
"�B
>wB
J�B
L�B
H�B
M�B
K�B
J�B
J�B
J�B
J�B
I�B
G�B
I�B
L�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
A�B
'�B
�B
�B
�B
(�B
:^B
s�B
�'BO�BM�BN�BcTB��BJB#�B�B7LBYB��B��B�+B�7B~�B�=B�}B��Bx�BT�B`BBffBS�BQ�BC�B;dBA�BR�BM�B>wB49B)�B �B �B/B;dBK�Bx�B��BB�`BĜB�RB��B�hB�B� Bs�Be`BZBXBYBS�B6FB%�B�BPBVB%�B{BB�B��B�LB��Bk�BM�B6FB�B
�B
ɺB
�DB
B�B
hB	�BB	��B	�RB	��B	��B	�TB	��B	ǮB	�-B	��B	��B	� B	e`B	N�B	E�B	9XB	2-B	.B	 �B	�B		7B��B�B�BB��B��B��BȴBÖB�wB�qB��BĜB��B��B��B��B�B�B�
B�B�#B�/B�HB�HB�;B�)B�NB�sB�yB�sB�fB�NB�HB�B�B�B�B�B�B��B��B�B��B��B��B��B	B	+B		7B		7B	DB	VB	bB	bB	bB	�B	�B	�B	 �B	 �B	 �B	)�B	,B	7LB	>wB	E�B	F�B	D�B	B�B	C�B	W
B	`BB	bNB	cTB	dZB	ffB	hsB	gmB	ffB	e`B	gmB	gmB	e`B	dZB	dZB	cTB	iyB	m�B	n�B	o�B	t�B	v�B	v�B	v�B	w�B	{�B	�B	�B	�B	�%B	�+B	�7B	�DB	�=B	�JB	�VB	�VB	�PB	�JB	�=B	�1B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�LB	�LB	�LB	�RB	�jB	�qB	�dB	�}B	��B	B	ÖB	ÖB	��B	�}B	�dB	�!B	�B	�-B	�LB	�RB	�XB	�^B	�^B	ŢB	ƨB	ĜB	ǮB	ŢB	B	ÖB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�B	�B	�/B	�5B	�5B	�5B	�)B	�B	�B	�B	�B	�/B	�)B	�#B	�)B	�5B	�BB	�;B	�;B	�/B	�)B	�#B	�#B	�#B	�#B	�)B	�5B	�5B	�5B	�/B	�5B	�5B	�;B	�HB	�NB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
1B
+B
+B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
	7B

=B
VB
�B
&�B
,B
1'B
8RB
?}B
D�B
I�B
O�B
T�B
YB
]/B
`BB
gmB
k�B
p�B
u�B
y�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
"�B
>[B
J�B
L�B
H�B
M�B
K�B
J�B
J�B
J�B
J�B
I�B
G�B
I�B
L�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
AnB
'�B
�B
xB
lB
(�B
:BB
s�B
�BO�BM�BN�Bc2BδB B#�B�B7+BX�B��B��B�B�B~�B�B�UB��Bx�BT�B`BfCBS�BQ�BCnB;<BAcBR�BM�B>OB4B)�B �B �B.�B;<BK�Bx�B��B�B�7B�sB�*B��B�>B��B�Bs�Be3BY�BW�BX�BS�B6B%�BvB(B+B%�BNB�B�_BʔB�!B�WBk]BM�B6B}B
�B
ɑB
�B
BkB
DB	�B	�gB	�.B	��B	��B	�,B	��B	ǌB	�B	��B	�dB	�B	e;B	N�B	E�B	97B	2B	-�B	 �B	{B		B��B�fB�#B��B˧B˧BȖB�vB�WB�PB�cB�}B̭BϿB��B��B��B��B��B��B�B�B�$B�"B�B�B�+B�NB�TB�PB�DB�,B�&B�bB�[B�mB�B�B�B��B��B�B��B��B��B��B	�B	B		B		B	B	/B	<B	=B	=B	YB	wB	�B	 �B	 �B	 �B	)�B	+�B	7#B	>NB	ExB	F�B	DtB	BhB	CkB	V�B	`B	b#B	c)B	d3B	f;B	hIB	gBB	f:B	e7B	gBB	gBB	e3B	d/B	d0B	c(B	iMB	meB	nmB	otB	t�B	v�B	v�B	v�B	w�B	{�B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�*B	�+B	�#B	�B	�B	�B	�"B	�TB	�jB	�tB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�?B	�EB	�6B	�NB	�[B	�`B	�fB	�eB	�YB	�NB	�4B	��B	��B	��B	�B	�"B	�&B	�.B	�/B	�qB	�uB	�mB	�B	�tB	�_B	�fB	ɋB	˔B	ͣB	ϭB	ѽB	жB	гB	иB	ϭB	ѼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�	B	�B	�B	�#B	�)B	�+B	�*B	�3B	�4B	�6B	�2B	�6B	�-B	�-B	�/B	�/B	�.B	�6B	�4B	�=B	�?B	�@B	�GB	�HB	�LB	�RB	�YB	�`B	�_B	�]B	�`B	�ZB	�XB	�YB	�PB	�SB	�RB	�XB	�_B	�fB	�rB	�B	�B	�wB	�qB	�qB	�jB	�jB	�cB	�`B	�YB	�ZB	�^B	�eB	�lB	�lB	�nB	�qB	�xB	�~B	�}B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B

	B
"B
�B
&�B
+�B
0�B
8B
?FB
DdB
I�B
O�B
T�B
X�B
\�B
`B
g7B
kMB
ppB
u�B
y�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.45 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608091443212016080914432120160809144321  AO  ARCAADJP                                                                    20160409072105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160409072105  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160409072105  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160809144321  IP                  G�O�G�O�G�O�                